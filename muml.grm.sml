structure MumlTokens =
  struct
    datatype token
      = KW_let
      | KW_in
      | KW_end
      | KW_fn
      | KW_fun
      | KW_val
      | KW_if
      | KW_then
      | KW_else
      | KW_andalso
      | KW_orelse
      | CON_int of Int.int
      | CON_string of string
      | ID of string
      | ARI_OP of string
      | LOG_OP of string
      | REL_OP of string		  
      | LP
      | RP
      | COMMA
      | SEMI
      | COLON
      | LB
      | RB
      | ARROW
      | TARROW
      | EOF
    val allToks = [
            KW_let, KW_in, KW_end, KW_fn, KW_fun, KW_val, KW_if, KW_then, KW_else, KW_andalso, KW_orelse, LP, RP, COMMA, SEMI, COLON, LB, RB, ARROW, TARROW, EOF
           ]
    fun toString tok =
(case (tok)
 of (KW_let) => "let"
  | (KW_in) => "in"
  | (KW_end) => "end"
  | (KW_fn) => "fn"
  | (KW_fun) => "fun"
  | (KW_val) => "val"
  | (KW_if) => "if"
  | (KW_then) => "then"
  | (KW_else) => "else"
  | (KW_andalso) => "andalso"
  | (KW_orelse) => "orelse"
  | (CON_int(_)) => "CON_int"
  | (CON_string(_)) => "CON_string"
  | (ID(_)) => "ID"
  | (ARI_OP(_)) => "ARI_OP"
  | (LOG_OP(_)) => "LOG_OP"
  | (REL_OP(_)) => "REL_OP"		   
  | (LP) => "("
  | (RP) => ")"
  | (COMMA) => ","
  | (SEMI) => ";"
  | (COLON) => ":"
  | (LB) => "["
  | (RB) => "]"
  | (ARROW) => "=>"
  | (TARROW) => "->"
  | (EOF) => "EOF"
(* end case *))
    fun isKW tok =
(case (tok)
 of (KW_let) => false
  | (KW_in) => false
  | (KW_end) => false
  | (KW_fn) => false
  | (KW_fun) => false
  | (KW_val) => false
  | (KW_if) => false
  | (KW_then) => false
  | (KW_else) => false
  | (KW_andalso) => false
  | (KW_orelse) => false
  | (CON_int(_)) => false
  | (CON_string(_)) => false
  | (ID(_)) => false
  | (ARI_OP(_)) => false
  | (LOG_OP(_)) => false
  | (REL_OP(_)) => false
  | (LP) => false
  | (RP) => false
  | (COMMA) => false
  | (SEMI) => false
  | (COLON) => false
  | (LB) => false
  | (RB) => false
  | (ARROW) => false
  | (TARROW) => false
  | (EOF) => false
(* end case *))
    fun isEOF EOF = true
      | isEOF _ = false
  end (* MumlTokens *)

functor MumlParseFn (Lex : ANTLR_LEXER) = struct

  local
    structure Tok =
MumlTokens
    structure UserCode =
      struct

  structure A = Ast

fun exp_PROD_1_ACT (SEMI, blockExp, expsequence, SEMI_SPAN : (Lex.pos * Lex.pos), blockExp_SPAN : (Lex.pos * Lex.pos), expsequence_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.Sequence(blockExp :: expsequence) )
fun expsequence_PROD_1_ACT (SEMI, blockExp, expsequence, SEMI_SPAN : (Lex.pos * Lex.pos), blockExp_SPAN : (Lex.pos * Lex.pos), expsequence_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( blockExp :: expsequence )
fun expsequence_PROD_2_ACT (blockExp, blockExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( [blockExp] )
fun blockExp_PROD_1_ACT (blockExp, ARROW, fnArgs, KW_fn, blockExp_SPAN : (Lex.pos * Lex.pos), ARROW_SPAN : (Lex.pos * Lex.pos), fnArgs_SPAN : (Lex.pos * Lex.pos), KW_fn_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.Fn(fnArgs, blockExp) )
fun blockExp_PROD_2_ACT (KW_else, KW_then, KW_if, opExp1, opExp2, opExp3, KW_else_SPAN : (Lex.pos * Lex.pos), KW_then_SPAN : (Lex.pos * Lex.pos), KW_if_SPAN : (Lex.pos * Lex.pos), opExp1_SPAN : (Lex.pos * Lex.pos), opExp2_SPAN : (Lex.pos * Lex.pos), opExp3_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.IfThenElse(opExp1, opExp2, opExp3) )
fun opExp_PROD_1_ACT (OP, opExp, appExps, OP_SPAN : (Lex.pos * Lex.pos), opExp_SPAN : (Lex.pos * Lex.pos), appExps_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.InfixApp(appExps, OP, opExp) )
fun appExps_PROD_1_ACT (simpleExp, simpleExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (
      let
        fun apps(result, nil) = result
          | apps(result, hd :: tl) = A.App(apps(hd, tl), result)

        val simpleExpRev = List.rev simpleExp
      in
        apps(List.hd simpleExpRev, List.tl simpleExpRev)
      end
    )
fun simpleExp_PROD_1_ACT (CON_int, CON_int_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.IntConstant(CON_int) )
fun simpleExp_PROD_2_ACT (CON_string, CON_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.StringConstant(CON_string) )
fun simpleExp_PROD_3_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.Variable(ref (A.create_symbol ID)) )
fun simpleExp_PROD_4_ACT (exp, KW_in, declist, KW_end, KW_let, exp_SPAN : (Lex.pos * Lex.pos), KW_in_SPAN : (Lex.pos * Lex.pos), declist_SPAN : (Lex.pos * Lex.pos), KW_end_SPAN : (Lex.pos * Lex.pos), KW_let_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.LetIn(declist, exp) )
fun simpleExp_PROD_5_ACT (LP, RP, exp, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( exp )
fun simpleExp_PROD_6_ACT (LP, RP, exp, COMMA, explist, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), explist_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.Tuple(exp :: explist) )
fun simpleExp_PROD_7_ACT (LP, RP, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.Unit )
fun explist_PROD_1_ACT (exp, COMMA, explist, exp_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), explist_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( exp :: explist )
fun explist_PROD_2_ACT (exp, exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( [exp] )
fun fnArgs_PROD_1_ACT (LP, RP, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( [] )
fun fnArgs_PROD_2_ACT (LP, RP, arglist, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), arglist_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( arglist )
fun fnArgs_PROD_3_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( [A.Name(ref (A.create_symbol ID))] )
fun arglist_PROD_1_ACT (ID, arglist, COMMA, ID_SPAN : (Lex.pos * Lex.pos), arglist_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.Name(ref (A.create_symbol ID)) :: arglist )
fun arglist_PROD_2_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( [A.Name(ref (A.create_symbol ID))] )
fun declist_PROD_1_ACT (dec, SEMI, declist, dec_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), declist_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( dec :: declist )
fun declist_PROD_2_ACT (dec, dec_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( [dec] )
fun valdec_PROD_1_ACT (ID, OP, exp, ID_SPAN : (Lex.pos * Lex.pos), OP_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.Valdec(A.Name(ref (A.create_symbol ID)), false, exp) )
fun fundec_PROD_1_ACT (ID, OP, exp, fnArgs, ID_SPAN : (Lex.pos * Lex.pos), OP_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), fnArgs_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.Valdec(A.Name(ref (A.create_symbol ID)), true, A.Fn(fnArgs, exp)) )
fun valdec_PROD_1_PRED (ID, OP, exp) = 
  ( OP = "=" )
fun fundec_PROD_1_PRED (ID, OP, exp, fnArgs) = 
  ( OP = "=" )
      end (* UserCode *)

    structure Err = AntlrErrHandler(
      structure Tok = Tok
      structure Lex = Lex)

(* replace functor with inline structure for better optimization
    structure EBNF = AntlrEBNF(
      struct
	type strm = Err.wstream
	val getSpan = Err.getSpan
      end)
*)
    structure EBNF =
      struct
	fun optional (pred, parse, strm) =
	      if pred strm
		then let
		  val (y, span, strm') = parse strm
		  in
		    (SOME y, span, strm')
		  end
		else (NONE, Err.getSpan strm, strm)

	fun closure (pred, parse, strm) = let
	      fun iter (strm, (left, right), ys) =
		    if pred strm
		      then let
			val (y, (_, right'), strm') = parse strm
			in iter (strm', (left, right'), y::ys)
			end
		      else (List.rev ys, (left, right), strm)
	      in
		iter (strm, Err.getSpan strm, [])
	      end

	fun posclos (pred, parse, strm) = let
	      val (y, (left, _), strm') = parse strm
	      val (ys, (_, right), strm'') = closure (pred, parse, strm')
	      in
		(y::ys, (left, right), strm'')
	      end
      end

    fun mk lexFn = let
fun getS() = {}
fun putS{} = ()
fun unwrap (ret, strm, repairs) = (ret, strm, repairs)
        val (eh, lex) = Err.mkErrHandler {get = getS, put = putS}
	fun fail() = Err.failure eh
	fun tryProds (strm, prods) = let
	  fun try [] = fail()
	    | try (prod :: prods) =
	        (Err.whileDisabled eh (fn() => prod strm))
		handle Err.ParseError => try (prods)
          in try prods end
fun matchKW_let strm = (case (lex(strm))
 of (Tok.KW_let, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_in strm = (case (lex(strm))
 of (Tok.KW_in, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_end strm = (case (lex(strm))
 of (Tok.KW_end, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_fn strm = (case (lex(strm))
 of (Tok.KW_fn, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_fun strm = (case (lex(strm))
 of (Tok.KW_fun, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_val strm = (case (lex(strm))
 of (Tok.KW_val, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_if strm = (case (lex(strm))
 of (Tok.KW_if, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_then strm = (case (lex(strm))
 of (Tok.KW_then, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_else strm = (case (lex(strm))
 of (Tok.KW_else, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_andalso strm = (case (lex(strm))
 of (Tok.KW_andalso, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_orelse strm = (case (lex(strm))
 of (Tok.KW_orelse, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCON_int strm = (case (lex(strm))
 of (Tok.CON_int(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchCON_string strm = (case (lex(strm))
 of (Tok.CON_string(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchID strm = (case (lex(strm))
 of (Tok.ID(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchOP strm = (case (lex(strm))
 of (Tok.OP(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchLP strm = (case (lex(strm))
 of (Tok.LP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRP strm = (case (lex(strm))
 of (Tok.RP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCOMMA strm = (case (lex(strm))
 of (Tok.COMMA, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSEMI strm = (case (lex(strm))
 of (Tok.SEMI, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCOLON strm = (case (lex(strm))
 of (Tok.COLON, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLB strm = (case (lex(strm))
 of (Tok.LB, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRB strm = (case (lex(strm))
 of (Tok.RB, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchARROW strm = (case (lex(strm))
 of (Tok.ARROW, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchTARROW strm = (case (lex(strm))
 of (Tok.TARROW, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchEOF strm = (case (lex(strm))
 of (Tok.EOF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))

val (exp_NT) = 
let
fun arglist_NT (strm) = let
      fun arglist_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm')
            val (arglist_RES, arglist_SPAN, strm') = arglist_NT(strm')
            val FULL_SPAN = (#1(ID_SPAN), #2(arglist_SPAN))
            in
              (UserCode.arglist_PROD_1_ACT (ID_RES, arglist_RES, COMMA_RES, ID_SPAN : (Lex.pos * Lex.pos), arglist_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun arglist_PROD_2 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.arglist_PROD_2_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') =>
              tryProds(strm, [arglist_PROD_1, arglist_PROD_2])
          | _ => fail()
        (* end case *))
      end
fun fnArgs_NT (strm) = let
      fun fnArgs_PROD_1 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.fnArgs_PROD_1_ACT (LP_RES, RP_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun fnArgs_PROD_2 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (arglist_RES, arglist_SPAN, strm') = arglist_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.fnArgs_PROD_2_ACT (LP_RES, RP_RES, arglist_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), arglist_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun fnArgs_PROD_3 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.fnArgs_PROD_3_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') => fnArgs_PROD_3(strm)
          | (Tok.LP, _, strm') =>
              (case (lex(strm'))
               of (Tok.RP, _, strm') => fnArgs_PROD_1(strm)
                | (Tok.ID(_), _, strm') => fnArgs_PROD_2(strm)
                | _ => fail()
              (* end case *))
          | _ => fail()
        (* end case *))
      end
fun exp_NT (strm) = let
      fun exp_PROD_1 (strm) = let
            val (blockExp_RES, blockExp_SPAN, strm') = blockExp_NT(strm)
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val (expsequence_RES, expsequence_SPAN, strm') = expsequence_NT(strm')
            val FULL_SPAN = (#1(blockExp_SPAN), #2(expsequence_SPAN))
            in
              (UserCode.exp_PROD_1_ACT (SEMI_RES, blockExp_RES, expsequence_RES, SEMI_SPAN : (Lex.pos * Lex.pos), blockExp_SPAN : (Lex.pos * Lex.pos), expsequence_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun exp_PROD_2 (strm) = let
            val (blockExp_RES, blockExp_SPAN, strm') = blockExp_NT(strm)
            val FULL_SPAN = (#1(blockExp_SPAN), #2(blockExp_SPAN))
            in
              ((blockExp_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_let, _, strm') => tryProds(strm, [exp_PROD_1, exp_PROD_2])
          | (Tok.KW_fn, _, strm') => tryProds(strm, [exp_PROD_1, exp_PROD_2])
          | (Tok.KW_if, _, strm') => tryProds(strm, [exp_PROD_1, exp_PROD_2])
          | (Tok.CON_int(_), _, strm') =>
              tryProds(strm, [exp_PROD_1, exp_PROD_2])
          | (Tok.CON_string(_), _, strm') =>
              tryProds(strm, [exp_PROD_1, exp_PROD_2])
          | (Tok.ID(_), _, strm') => tryProds(strm, [exp_PROD_1, exp_PROD_2])
          | (Tok.LP, _, strm') => tryProds(strm, [exp_PROD_1, exp_PROD_2])
          | _ => fail()
        (* end case *))
      end
and blockExp_NT (strm) = let
      fun blockExp_PROD_1 (strm) = let
            val (KW_fn_RES, KW_fn_SPAN, strm') = matchKW_fn(strm)
            val (fnArgs_RES, fnArgs_SPAN, strm') = fnArgs_NT(strm')
            val (ARROW_RES, ARROW_SPAN, strm') = matchARROW(strm')
            val (blockExp_RES, blockExp_SPAN, strm') = blockExp_NT(strm')
            val FULL_SPAN = (#1(KW_fn_SPAN), #2(blockExp_SPAN))
            in
              (UserCode.blockExp_PROD_1_ACT (blockExp_RES, ARROW_RES, fnArgs_RES, KW_fn_RES, blockExp_SPAN : (Lex.pos * Lex.pos), ARROW_SPAN : (Lex.pos * Lex.pos), fnArgs_SPAN : (Lex.pos * Lex.pos), KW_fn_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun blockExp_PROD_2 (strm) = let
            val (KW_if_RES, KW_if_SPAN, strm') = matchKW_if(strm)
            val (opExp1_RES, opExp1_SPAN, strm') = opExp_NT(strm')
            val (KW_then_RES, KW_then_SPAN, strm') = matchKW_then(strm')
            val (opExp2_RES, opExp2_SPAN, strm') = opExp_NT(strm')
            val (KW_else_RES, KW_else_SPAN, strm') = matchKW_else(strm')
            val (opExp3_RES, opExp3_SPAN, strm') = opExp_NT(strm')
            val FULL_SPAN = (#1(KW_if_SPAN), #2(opExp3_SPAN))
            in
              (UserCode.blockExp_PROD_2_ACT (KW_else_RES, KW_then_RES, KW_if_RES, opExp1_RES, opExp2_RES, opExp3_RES, KW_else_SPAN : (Lex.pos * Lex.pos), KW_then_SPAN : (Lex.pos * Lex.pos), KW_if_SPAN : (Lex.pos * Lex.pos), opExp1_SPAN : (Lex.pos * Lex.pos), opExp2_SPAN : (Lex.pos * Lex.pos), opExp3_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun blockExp_PROD_3 (strm) = let
            val (opExp_RES, opExp_SPAN, strm') = opExp_NT(strm)
            val FULL_SPAN = (#1(opExp_SPAN), #2(opExp_SPAN))
            in
              ((opExp_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_let, _, strm') => blockExp_PROD_3(strm)
          | (Tok.CON_int(_), _, strm') => blockExp_PROD_3(strm)
          | (Tok.CON_string(_), _, strm') => blockExp_PROD_3(strm)
          | (Tok.ID(_), _, strm') => blockExp_PROD_3(strm)
          | (Tok.LP, _, strm') => blockExp_PROD_3(strm)
          | (Tok.KW_fn, _, strm') => blockExp_PROD_1(strm)
          | (Tok.KW_if, _, strm') => blockExp_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
and opExp_NT (strm) = let
      fun opExp_PROD_1 (strm) = let
            val (appExps_RES, appExps_SPAN, strm') = appExps_NT(strm)
            val (OP_RES, OP_SPAN, strm') = matchOP(strm')
            val (opExp_RES, opExp_SPAN, strm') = opExp_NT(strm')
            val FULL_SPAN = (#1(appExps_SPAN), #2(opExp_SPAN))
            in
              (UserCode.opExp_PROD_1_ACT (OP_RES, opExp_RES, appExps_RES, OP_SPAN : (Lex.pos * Lex.pos), opExp_SPAN : (Lex.pos * Lex.pos), appExps_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun opExp_PROD_2 (strm) = let
            val (appExps_RES, appExps_SPAN, strm') = appExps_NT(strm)
            val FULL_SPAN = (#1(appExps_SPAN), #2(appExps_SPAN))
            in
              ((appExps_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_let, _, strm') =>
              tryProds(strm, [opExp_PROD_1, opExp_PROD_2])
          | (Tok.CON_int(_), _, strm') =>
              tryProds(strm, [opExp_PROD_1, opExp_PROD_2])
          | (Tok.CON_string(_), _, strm') =>
              tryProds(strm, [opExp_PROD_1, opExp_PROD_2])
          | (Tok.ID(_), _, strm') =>
              tryProds(strm, [opExp_PROD_1, opExp_PROD_2])
          | (Tok.LP, _, strm') => tryProds(strm, [opExp_PROD_1, opExp_PROD_2])
          | _ => fail()
        (* end case *))
      end
and appExps_NT (strm) = let
      fun appExps_PROD_1_SUBRULE_1_NT (strm) = let
            val (simpleExp_RES, simpleExp_SPAN, strm') = simpleExp_NT(strm)
            val FULL_SPAN = (#1(simpleExp_SPAN), #2(simpleExp_SPAN))
            in
              ((simpleExp_RES), FULL_SPAN, strm')
            end
      fun appExps_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_let, _, strm') => true
              | (Tok.CON_int(_), _, strm') => true
              | (Tok.CON_string(_), _, strm') => true
              | (Tok.ID(_), _, strm') => true
              | (Tok.LP, _, strm') => true
              | _ => false
            (* end case *))
      val (simpleExp_RES, simpleExp_SPAN, strm') = EBNF.posclos(appExps_PROD_1_SUBRULE_1_PRED, appExps_PROD_1_SUBRULE_1_NT, strm)
      val FULL_SPAN = (#1(simpleExp_SPAN), #2(simpleExp_SPAN))
      in
        (UserCode.appExps_PROD_1_ACT (simpleExp_RES, simpleExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and simpleExp_NT (strm) = let
      fun simpleExp_PROD_1 (strm) = let
            val (CON_int_RES, CON_int_SPAN, strm') = matchCON_int(strm)
            val FULL_SPAN = (#1(CON_int_SPAN), #2(CON_int_SPAN))
            in
              (UserCode.simpleExp_PROD_1_ACT (CON_int_RES, CON_int_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun simpleExp_PROD_2 (strm) = let
            val (CON_string_RES, CON_string_SPAN, strm') = matchCON_string(strm)
            val FULL_SPAN = (#1(CON_string_SPAN), #2(CON_string_SPAN))
            in
              (UserCode.simpleExp_PROD_2_ACT (CON_string_RES, CON_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun simpleExp_PROD_3 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.simpleExp_PROD_3_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun simpleExp_PROD_4 (strm) = let
            val (KW_let_RES, KW_let_SPAN, strm') = matchKW_let(strm)
            val (declist_RES, declist_SPAN, strm') = declist_NT(strm')
            val (KW_in_RES, KW_in_SPAN, strm') = matchKW_in(strm')
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
            val (KW_end_RES, KW_end_SPAN, strm') = matchKW_end(strm')
            val FULL_SPAN = (#1(KW_let_SPAN), #2(KW_end_SPAN))
            in
              (UserCode.simpleExp_PROD_4_ACT (exp_RES, KW_in_RES, declist_RES, KW_end_RES, KW_let_RES, exp_SPAN : (Lex.pos * Lex.pos), KW_in_SPAN : (Lex.pos * Lex.pos), declist_SPAN : (Lex.pos * Lex.pos), KW_end_SPAN : (Lex.pos * Lex.pos), KW_let_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun simpleExp_PROD_5 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.simpleExp_PROD_5_ACT (LP_RES, RP_RES, exp_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun simpleExp_PROD_6 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm')
            val (explist_RES, explist_SPAN, strm') = explist_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.simpleExp_PROD_6_ACT (LP_RES, RP_RES, exp_RES, COMMA_RES, explist_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), explist_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun simpleExp_PROD_7 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.simpleExp_PROD_7_ACT (LP_RES, RP_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LP, _, strm') =>
              (case (lex(strm'))
               of (Tok.RP, _, strm') => simpleExp_PROD_7(strm)
                | (Tok.KW_let, _, strm') =>
                    tryProds(strm, [simpleExp_PROD_5, simpleExp_PROD_6])
                | (Tok.KW_fn, _, strm') =>
                    tryProds(strm, [simpleExp_PROD_5, simpleExp_PROD_6])
                | (Tok.KW_if, _, strm') =>
                    tryProds(strm, [simpleExp_PROD_5, simpleExp_PROD_6])
                | (Tok.CON_int(_), _, strm') =>
                    tryProds(strm, [simpleExp_PROD_5, simpleExp_PROD_6])
                | (Tok.CON_string(_), _, strm') =>
                    tryProds(strm, [simpleExp_PROD_5, simpleExp_PROD_6])
                | (Tok.ID(_), _, strm') =>
                    tryProds(strm, [simpleExp_PROD_5, simpleExp_PROD_6])
                | (Tok.LP, _, strm') =>
                    tryProds(strm, [simpleExp_PROD_5, simpleExp_PROD_6])
                | _ => fail()
              (* end case *))
          | (Tok.ID(_), _, strm') => simpleExp_PROD_3(strm)
          | (Tok.CON_int(_), _, strm') => simpleExp_PROD_1(strm)
          | (Tok.CON_string(_), _, strm') => simpleExp_PROD_2(strm)
          | (Tok.KW_let, _, strm') => simpleExp_PROD_4(strm)
          | _ => fail()
        (* end case *))
      end
and explist_NT (strm) = let
      fun explist_PROD_1 (strm) = let
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm)
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm')
            val (explist_RES, explist_SPAN, strm') = explist_NT(strm')
            val FULL_SPAN = (#1(exp_SPAN), #2(explist_SPAN))
            in
              (UserCode.explist_PROD_1_ACT (exp_RES, COMMA_RES, explist_RES, exp_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), explist_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun explist_PROD_2 (strm) = let
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm)
            val FULL_SPAN = (#1(exp_SPAN), #2(exp_SPAN))
            in
              (UserCode.explist_PROD_2_ACT (exp_RES, exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_let, _, strm') =>
              tryProds(strm, [explist_PROD_1, explist_PROD_2])
          | (Tok.KW_fn, _, strm') =>
              tryProds(strm, [explist_PROD_1, explist_PROD_2])
          | (Tok.KW_if, _, strm') =>
              tryProds(strm, [explist_PROD_1, explist_PROD_2])
          | (Tok.CON_int(_), _, strm') =>
              tryProds(strm, [explist_PROD_1, explist_PROD_2])
          | (Tok.CON_string(_), _, strm') =>
              tryProds(strm, [explist_PROD_1, explist_PROD_2])
          | (Tok.ID(_), _, strm') =>
              tryProds(strm, [explist_PROD_1, explist_PROD_2])
          | (Tok.LP, _, strm') =>
              tryProds(strm, [explist_PROD_1, explist_PROD_2])
          | _ => fail()
        (* end case *))
      end
and declist_NT (strm) = let
      fun declist_PROD_1 (strm) = let
            val (dec_RES, dec_SPAN, strm') = dec_NT(strm)
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val (declist_RES, declist_SPAN, strm') = declist_NT(strm')
            val FULL_SPAN = (#1(dec_SPAN), #2(declist_SPAN))
            in
              (UserCode.declist_PROD_1_ACT (dec_RES, SEMI_RES, declist_RES, dec_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), declist_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun declist_PROD_2 (strm) = let
            val (dec_RES, dec_SPAN, strm') = dec_NT(strm)
            val FULL_SPAN = (#1(dec_SPAN), #2(dec_SPAN))
            in
              (UserCode.declist_PROD_2_ACT (dec_RES, dec_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_fun, _, strm') =>
              tryProds(strm, [declist_PROD_1, declist_PROD_2])
          | (Tok.KW_val, _, strm') =>
              tryProds(strm, [declist_PROD_1, declist_PROD_2])
          | _ => fail()
        (* end case *))
      end
and dec_NT (strm) = let
      fun dec_PROD_1 (strm) = let
            val (KW_val_RES, KW_val_SPAN, strm') = matchKW_val(strm)
            val (valdec_RES, valdec_SPAN, strm') = valdec_NT(strm')
            val FULL_SPAN = (#1(KW_val_SPAN), #2(valdec_SPAN))
            in
              ((valdec_RES), FULL_SPAN, strm')
            end
      fun dec_PROD_2 (strm) = let
            val (KW_fun_RES, KW_fun_SPAN, strm') = matchKW_fun(strm)
            val (fundec_RES, fundec_SPAN, strm') = fundec_NT(strm')
            val FULL_SPAN = (#1(KW_fun_SPAN), #2(fundec_SPAN))
            in
              ((fundec_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_fun, _, strm') => dec_PROD_2(strm)
          | (Tok.KW_val, _, strm') => dec_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and fundec_NT (strm) = let
      val (ID_RES, ID_SPAN, strm') = matchID(strm)
      val (fnArgs_RES, fnArgs_SPAN, strm') = fnArgs_NT(strm')
      val (OP_RES, OP_SPAN, strm') = matchOP(strm')
      val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
      in
        if (UserCode.fundec_PROD_1_PRED (ID_RES, OP_RES, exp_RES, fnArgs_RES))
          then let
            val FULL_SPAN = (#1(ID_SPAN), #2(exp_SPAN))
            in
              (UserCode.fundec_PROD_1_ACT (ID_RES, OP_RES, exp_RES, fnArgs_RES, ID_SPAN : (Lex.pos * Lex.pos), OP_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), fnArgs_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
          else fail()
      end
and valdec_NT (strm) = let
      val (ID_RES, ID_SPAN, strm') = matchID(strm)
      val (OP_RES, OP_SPAN, strm') = matchOP(strm')
      val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
      in
        if (UserCode.valdec_PROD_1_PRED (ID_RES, OP_RES, exp_RES))
          then let
            val FULL_SPAN = (#1(ID_SPAN), #2(exp_SPAN))
            in
              (UserCode.valdec_PROD_1_ACT (ID_RES, OP_RES, exp_RES, ID_SPAN : (Lex.pos * Lex.pos), OP_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
          else fail()
      end
and expsequence_NT (strm) = let
      fun expsequence_PROD_1 (strm) = let
            val (blockExp_RES, blockExp_SPAN, strm') = blockExp_NT(strm)
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val (expsequence_RES, expsequence_SPAN, strm') = expsequence_NT(strm')
            val FULL_SPAN = (#1(blockExp_SPAN), #2(expsequence_SPAN))
            in
              (UserCode.expsequence_PROD_1_ACT (SEMI_RES, blockExp_RES, expsequence_RES, SEMI_SPAN : (Lex.pos * Lex.pos), blockExp_SPAN : (Lex.pos * Lex.pos), expsequence_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun expsequence_PROD_2 (strm) = let
            val (blockExp_RES, blockExp_SPAN, strm') = blockExp_NT(strm)
            val FULL_SPAN = (#1(blockExp_SPAN), #2(blockExp_SPAN))
            in
              (UserCode.expsequence_PROD_2_ACT (blockExp_RES, blockExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_let, _, strm') =>
              tryProds(strm, [expsequence_PROD_1, expsequence_PROD_2])
          | (Tok.KW_fn, _, strm') =>
              tryProds(strm, [expsequence_PROD_1, expsequence_PROD_2])
          | (Tok.KW_if, _, strm') =>
              tryProds(strm, [expsequence_PROD_1, expsequence_PROD_2])
          | (Tok.CON_int(_), _, strm') =>
              tryProds(strm, [expsequence_PROD_1, expsequence_PROD_2])
          | (Tok.CON_string(_), _, strm') =>
              tryProds(strm, [expsequence_PROD_1, expsequence_PROD_2])
          | (Tok.ID(_), _, strm') =>
              tryProds(strm, [expsequence_PROD_1, expsequence_PROD_2])
          | (Tok.LP, _, strm') =>
              tryProds(strm, [expsequence_PROD_1, expsequence_PROD_2])
          | _ => fail()
        (* end case *))
      end
in
  (exp_NT)
end
val exp_NT =  fn s => unwrap (Err.launch (eh, lexFn, exp_NT , true) s)

in (exp_NT) end
  in
fun parse lexFn  s = let val (exp_NT) = mk lexFn in exp_NT s end

  end

end
