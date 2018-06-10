functor MiniMLLexFun(structure Tokens: MiniML_TOKENS)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
STRING | COMMENT | INITIAL
    structure UserDeclarations = 
      struct


structure Tokens = Tokens

type pos           = int
type svalue        = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult     = (svalue, pos) token


fun mkKW (kw, tk) = (kw, fn (p1:pos, p2:pos) => tk (p1, p2))


val keywords = map mkKW
[
  ("and",     Tokens.KW_and),
  ("andalso",Tokens.KW_andalso),
  ("bool",   Tokens.KW_bool),
  ("div",    Tokens.KW_div),
  ("else",   Tokens.KW_else),
  ("end",    Tokens.KW_end),
  ("fn",     Tokens.KW_fn),
  ("fun",    Tokens.KW_fun),
  ("handle", Tokens.KW_handle),
  ("if",     Tokens.KW_if),
  ("in",     Tokens.KW_in),
  ("int",    Tokens.KW_int),
  ("let",    Tokens.KW_let),
  ("mod",    Tokens.KW_mod),
  ("not",    Tokens.KW_not),
  ("of",     Tokens.KW_of),
  ("orelse", Tokens.KW_orelse),
  ("true",   Tokens.KW_true),
  ("false",  Tokens.KW_false),
  ("raise",  Tokens.KW_raise),
  ("string", Tokens.KW_string),
  ("then",   Tokens.KW_then),
  ("val",    Tokens.KW_val)
]


fun findKeywords (str: string, pos1: pos, pos2: pos) =
  case List.find (fn (s, _) => s = str) keywords of
    NONE          => Tokens.ID(str, pos1, pos2)
  | SOME((_, tk)) => tk(pos1, pos2)


(* Line number counter. *)
val pos = ref 1

fun error (e,l : int,_) =
  raise Errors.LexicalError (String.concat["line ", (Int.toString l), ": ", e])

fun eof ()          = Tokens.EOF(!pos,!pos)

val charlist        = ref ([] : string list)

val commentNesting  = ref (0)

fun addString (s)   = (charlist :=  s::(!charlist))

fun makeString ()   = (concat(rev(!charlist)) before charlist:=[])




      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as ()) = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;
      (pos := (!pos) + 1; continue()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction2 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (findKeywords (yytext, !pos, !pos))
      end
fun yyAction3 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        ((Tokens.INT(valOf (Int.fromString (yytext)), !pos, !pos))
     handle Overflow => error ("integer constant too large",!pos,!pos))
      end
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.HASH(!pos,!pos)))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COMMA(!pos,!pos)))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COLON(!pos,!pos)))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN COMMENT; commentNesting := 0; continue ()))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LPAREN(!pos,!pos)))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RPAREN(!pos,!pos)))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TIMES(!pos,!pos)))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PLUS(!pos,!pos)))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ARROW(!pos,!pos)))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MINUS(!pos,!pos)))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NEQ(!pos, !pos)))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DARROW(!pos,!pos)))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.EQSIGN(!pos,!pos)))
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GREATER(!pos,!pos)))
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GREATEREQ(!pos, !pos)))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LESS(!pos,!pos)))
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LESSEQ(!pos, !pos)))
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NEG(!pos,!pos)))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.CARET(!pos,!pos)))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN STRING; charlist := [""]; continue ()))
fun yyAction24 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (error ("bad character(s) " ^ yytext,!pos,!pos);
                            continue())
      end
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL;
                             error ("unterminated string constant",!pos,!pos);
                                    pos := (!pos)+1; (* charlist := "";*)
                                    continue() ))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL;
                            Tokens.STRING (makeString(), !pos,!pos)))
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString "\""; continue()))
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString "\n"; continue()))
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString "\t"; continue()))
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString "\r"; continue()))
fun yyAction31 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addString (yytext); continue ())
      end
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
      (if (!commentNesting)=0
                             then (YYBEGIN INITIAL;
                                   continue ())
                             else (commentNesting := (!commentNesting)-1;
                                   continue ())))
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;
      (commentNesting := (!commentNesting)+1;
                            continue ()))
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ37(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < #"0"
              then yyAction3(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ37(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ37(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp < #"0"
              then yyAction21(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ37(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
              else yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"."
                  then yyQ38(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < #"."
                  then if inp = #"'"
                      then yyQ38(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                else if inp = #"/"
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = #"_"
              then yyQ38(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #"A"
                  then yyQ38(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction2(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ38(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ38(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"a"
              then yyAction2(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"."
                  then yyQ38(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < #"."
                  then if inp = #"'"
                      then yyQ38(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                else if inp = #"/"
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = #"_"
              then yyQ38(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #"A"
                  then yyQ38(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction2(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ38(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ38(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"a"
              then yyAction2(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ39(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ40(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
              else yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ42(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #">"
              then if inp = #"="
                  then yyQ41(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ37(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < #"0"
              then yyAction3(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ37(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ43(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
              else yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ44(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ45(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyAction1(strm, yyNO_MATCH)
            else if inp < #"\v"
              then if inp <= #"\b"
                  then yyAction1(strm, yyNO_MATCH)
                  else yyQ46(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = #" "
              then yyQ46(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\v"
              then if inp <= #"\b"
                  then yyAction0(strm, yyNO_MATCH)
                  else yyQ46(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #" "
              then yyQ46(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyAction1(strm, yyNO_MATCH)
            else if inp < #"\v"
              then if inp <= #"\b"
                  then yyAction1(strm, yyNO_MATCH)
                  else yyQ46(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = #" "
              then yyQ46(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"-"
              then yyQ28(strm', lastMatch)
            else if inp < #"-"
              then if inp = #"\""
                  then yyQ21(strm', lastMatch)
                else if inp < #"\""
                  then if inp = #"\r"
                      then yyQ20(strm', lastMatch)
                    else if inp < #"\r"
                      then if inp = #"\n"
                          then yyQ19(strm', lastMatch)
                        else if inp < #"\n"
                          then if inp = #"\t"
                              then yyQ18(strm', lastMatch)
                              else yyQ17(strm', lastMatch)
                          else yyQ17(strm', lastMatch)
                    else if inp = #" "
                      then yyQ18(strm', lastMatch)
                      else yyQ17(strm', lastMatch)
                else if inp = #")"
                  then yyQ24(strm', lastMatch)
                else if inp < #")"
                  then if inp = #"$"
                      then yyQ17(strm', lastMatch)
                    else if inp < #"$"
                      then yyQ22(strm', lastMatch)
                    else if inp = #"("
                      then yyQ23(strm', lastMatch)
                      else yyQ17(strm', lastMatch)
                else if inp = #"+"
                  then yyQ26(strm', lastMatch)
                else if inp = #"*"
                  then yyQ25(strm', lastMatch)
                  else yyQ27(strm', lastMatch)
            else if inp = #"A"
              then yyQ34(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"<"
                  then yyQ31(strm', lastMatch)
                else if inp < #"<"
                  then if inp = #":"
                      then yyQ30(strm', lastMatch)
                    else if inp < #":"
                      then if inp <= #"/"
                          then yyQ17(strm', lastMatch)
                          else yyQ29(strm', lastMatch)
                      else yyQ17(strm', lastMatch)
                else if inp = #">"
                  then yyQ33(strm', lastMatch)
                else if inp = #"="
                  then yyQ32(strm', lastMatch)
                  else yyQ17(strm', lastMatch)
            else if inp = #"a"
              then yyQ34(strm', lastMatch)
            else if inp < #"a"
              then if inp = #"^"
                  then yyQ35(strm', lastMatch)
                else if inp < #"^"
                  then if inp <= #"Z"
                      then yyQ34(strm', lastMatch)
                      else yyQ17(strm', lastMatch)
                  else yyQ17(strm', lastMatch)
            else if inp = #"~"
              then yyQ36(strm', lastMatch)
            else if inp < #"~"
              then if inp <= #"z"
                  then yyQ34(strm', lastMatch)
                  else yyQ17(strm', lastMatch)
              else yyQ17(strm', lastMatch)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #")"
              then yyQ15(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ16(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"("
              then yyQ13(strm', lastMatch)
            else if inp < #"("
              then if inp = #"\n"
                  then if yyInput.eof(!(yystrm))
                      then UserDeclarations.eof(yyarg)
                      else yystuck(lastMatch)
                  else yyQ12(strm', lastMatch)
            else if inp = #"*"
              then yyQ14(strm', lastMatch)
              else yyQ12(strm', lastMatch)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"o"
              then yyAction31(strm, yyNO_MATCH)
            else if inp < #"o"
              then if inp = #"#"
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < #"#"
                  then if inp = #"\""
                      then yyQ8(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                      else yyAction31(strm, yyNO_MATCH)
                else if inp = #"n"
                  then yyQ9(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyAction31(strm, yyNO_MATCH)
            else if inp < #"s"
              then if inp = #"r"
                  then yyQ10(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ11(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ4(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
              else yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ3(strm', lastMatch)
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yyQ3(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyQ4(strm', lastMatch)
                      else yyQ3(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ5(strm', lastMatch)
                  else yyQ3(strm', lastMatch)
            else if inp = #"#"
              then yyQ3(strm', lastMatch)
            else if inp < #"#"
              then if inp = #"\""
                  then yyQ6(strm', lastMatch)
                  else yyQ3(strm', lastMatch)
            else if inp = #"\\"
              then yyQ7(strm', lastMatch)
              else yyQ3(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of STRING => yyQ0(!(yystrm), yyNO_MATCH)
    | COMMENT => yyQ1(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ2(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    end

  end
