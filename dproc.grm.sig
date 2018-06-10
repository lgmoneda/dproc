signature MiniML_TOKENS =
sig
type ('a,'b) token
type svalue
val STRING: (string) *  'a * 'a -> (svalue,'a) token
val INT: (int) *  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
val TTIMES:  'a * 'a -> (svalue,'a) token
val TIMES:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val NEG:  'a * 'a -> (svalue,'a) token
val MINUS:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val LESSEQ:  'a * 'a -> (svalue,'a) token
val LESS:  'a * 'a -> (svalue,'a) token
val HASH:  'a * 'a -> (svalue,'a) token
val GREATEREQ:  'a * 'a -> (svalue,'a) token
val GREATER:  'a * 'a -> (svalue,'a) token
val NEQ:  'a * 'a -> (svalue,'a) token
val EQSIGN:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val DARROW:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val CARET:  'a * 'a -> (svalue,'a) token
val ARROW:  'a * 'a -> (svalue,'a) token
val KW_and:  'a * 'a -> (svalue,'a) token
val KW_val:  'a * 'a -> (svalue,'a) token
val KW_true:  'a * 'a -> (svalue,'a) token
val KW_then:  'a * 'a -> (svalue,'a) token
val KW_string:  'a * 'a -> (svalue,'a) token
val KW_raise:  'a * 'a -> (svalue,'a) token
val KW_orelse:  'a * 'a -> (svalue,'a) token
val KW_of:  'a * 'a -> (svalue,'a) token
val KW_not:  'a * 'a -> (svalue,'a) token
val KW_mod:  'a * 'a -> (svalue,'a) token
val KW_let:  'a * 'a -> (svalue,'a) token
val KW_int:  'a * 'a -> (svalue,'a) token
val KW_in:  'a * 'a -> (svalue,'a) token
val KW_if:  'a * 'a -> (svalue,'a) token
val KW_handle:  'a * 'a -> (svalue,'a) token
val KW_fun:  'a * 'a -> (svalue,'a) token
val KW_fn:  'a * 'a -> (svalue,'a) token
val KW_false:  'a * 'a -> (svalue,'a) token
val KW_end:  'a * 'a -> (svalue,'a) token
val KW_else:  'a * 'a -> (svalue,'a) token
val KW_div:  'a * 'a -> (svalue,'a) token
val KW_bool:  'a * 'a -> (svalue,'a) token
val KW_andalso:  'a * 'a -> (svalue,'a) token
end
signature MiniML_LRVALS=
sig
structure Tokens : MiniML_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
