%name DprocLexer;

%let digit = [0-9];
%let int = {digit}+;
%let letter = [a-zA-Z];
%let id = {letter}({letter} | {digit} | "'" | "_")*;
%let op = ("+" | "-" | "*" | "/" | "**");

%let float = {int}"."{int};
%let decl = ("int" | "float" | "bool" | "column" | "table");
%let func = ("max" | "min" | "load" | "media" | "soma" | "subtracao" | "multiplicacao" | "divisao" | "insert" | "logic_comp");
%let op_rel = ("<" | ">" | ">=" | "<=" | "==" | "!=");
%let op_log = ("and" | "or");
%let bool = ("true" | "false");



%states CON_STRING;

%defs (
  structure T = DprocTokens
  type lex_result = T.token
  fun eof() = T.EOF
  val stringbuf = ref "";
);

<INITIAL> {float} => ( T.CON_float (valOf (Real.fromString yytext)) );
<INITIAL> {decl} => ( T.KW_decl yytext );
<INITIAL> {func} => ( T.KW_func yytext );
<INITIAL> teste => ( T.KW_teste );
<INITIAL> "=" => ( T.ASSIGN );
<INITIAL> {op_rel} => ( T.OP_rel yytext);
<INITIAL> {op_log} => ( T.OP_log yytext);
<INITIAL> {bool} => (T.CON_bool (valOf (Bool.fromString yytext)) );

<INITIAL> let => ( T.KW_let );
<INITIAL> in => ( T.KW_in );
<INITIAL> end => ( T.KW_end );
<INITIAL> fn => ( T.KW_fn );
<INITIAL> fun => ( T.KW_fun );
<INITIAL> val => ( T.KW_val );
<INITIAL> if => ( T.KW_if );
<INITIAL> then => ( T.KW_then );
<INITIAL> else => ( T.KW_else );
<INITIAL> andalso => ( T.KW_andalso );
<INITIAL> orelse => ( T.KW_orelse );
<INITIAL> "=>" => ( T.ARROW );
<INITIAL> "->" => ( T.TARROW );
<INITIAL> {id} => ( T.ID yytext );
<INITIAL> {op} => ( T.OP yytext );
<INITIAL> {int} => ( T.CON_int (valOf (Int.fromString yytext)) );
<INITIAL> "\"" => ( YYBEGIN(CON_STRING); stringbuf := ""; continue() );
<INITIAL> "(" => ( T.LP );
<INITIAL> ")" => ( T.RP );
<INITIAL> "[" => ( T.LB );
<INITIAL> "]" => ( T.RB );
<INITIAL> "," => ( T.COMMA );
<INITIAL> ";" => ( T.SEMI );
<INITIAL> ":" => ( T.COLON );
<INITIAL> " " | \n | \t => ( continue() );

<CON_STRING> "\"" => ( YYBEGIN(INITIAL); T.CON_string(!stringbuf) );
<CON_STRING> [^"]* => ( stringbuf := (!stringbuf ^ yytext); continue() );
