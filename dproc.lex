
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


%%

%s STRING COMMENT;

%header (functor MiniMLLexFun(structure Tokens: MiniML_TOKENS));


eol        = ("\n"|"\013\n"|"\013");
alpha      = [A-Za-z];
caps       = [A-Z];
digit      = [0-9];
ws         = [\ \t \n];
allchar    = ({alpha}|{digit}|"_"|"'"|".");
identifier = {alpha}{allchar}*;
integer    = "~"?{digit}+;


%%

<INITIAL>{eol}          => (pos := (!pos) + 1; continue());
<INITIAL>{ws}+          => (continue());

<INITIAL>{identifier}   => (findKeywords (yytext, !pos, !pos));
<INITIAL>{integer}      => 
    ((Tokens.INT(valOf (Int.fromString (yytext)), !pos, !pos))
     handle Overflow => error ("integer constant too large",!pos,!pos));

<INITIAL>"#"            => (Tokens.HASH(!pos,!pos));
<INITIAL>","            => (Tokens.COMMA(!pos,!pos));
<INITIAL>":"            => (Tokens.COLON(!pos,!pos));
<INITIAL>"(*"           => (YYBEGIN COMMENT; commentNesting := 0; continue ());
<INITIAL>"("            => (Tokens.LPAREN(!pos,!pos));
<INITIAL>")"            => (Tokens.RPAREN(!pos,!pos));
<INITIAL>"*"            => (Tokens.TIMES(!pos,!pos));
<INITIAL>"+"            => (Tokens.PLUS(!pos,!pos));
<INITIAL>"->"           => (Tokens.ARROW(!pos,!pos));
<INITIAL>"-"            => (Tokens.MINUS(!pos,!pos));
<INITIAL>"<>"           => (Tokens.NEQ(!pos, !pos));
<INITIAL>"=>"           => (Tokens.DARROW(!pos,!pos));
<INITIAL>"="            => (Tokens.EQSIGN(!pos,!pos));
<INITIAL>">"            => (Tokens.GREATER(!pos,!pos));
<INITIAL>">="           => (Tokens.GREATEREQ(!pos, !pos));
<INITIAL>"<"            => (Tokens.LESS(!pos,!pos));
<INITIAL>"<="           => (Tokens.LESSEQ(!pos, !pos));
<INITIAL>"~"            => (Tokens.NEG(!pos,!pos));
<INITIAL>"^"            => (Tokens.CARET(!pos,!pos));
<INITIAL>\"             => (YYBEGIN STRING; charlist := [""]; continue ());

<INITIAL>.              => (error ("bad character(s) " ^ yytext,!pos,!pos);
                            continue());

<STRING>{eol}           => (YYBEGIN INITIAL;
                             error ("unterminated string constant",!pos,!pos);
                                    pos := (!pos)+1; (* charlist := "";*)
                                    continue() );
<STRING>\"              => (YYBEGIN INITIAL;
                            Tokens.STRING (makeString(), !pos,!pos));
<STRING>"\\\""          => (addString "\""; continue());
<STRING>"\\n"           => (addString "\n"; continue());
<STRING>"\\t"           => (addString "\t"; continue());
<STRING>"\\r"           => (addString "\r"; continue());
<STRING>.               => (addString (yytext); continue ());
                                                                                                                                                          
                                                                                                                                                          
<COMMENT>"*)"           => (if (!commentNesting)=0
                             then (YYBEGIN INITIAL;
                                   continue ())
                             else (commentNesting := (!commentNesting)-1;
                                   continue ()));
<COMMENT>"(*"           => (commentNesting := (!commentNesting)+1;
                            continue ());
<COMMENT>.              => (continue());

