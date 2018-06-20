(*fun makeTest(input: string): MumlTokens.token list = Helpers.string_to_tokens(input);*)

fun readlist (infile : string) = let 
  val ins = TextIO.openIn infile 
  fun loop ins = 
   case TextIO.inputLine ins of 
      SOME line => line :: loop ins 
    | NONE      => [] 
in 
  loop ins before TextIO.closeIn ins 
end;


val input = concat(readlist("test.dproc"));

fun makeTest(input: string): Ast.Exp = Helpers.string_to_ast(input);

val arvore_sintatica = makeTest(input);