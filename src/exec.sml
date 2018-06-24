fun getArvoreSintatica(input: string): Ast.Exp = Helpers.string_to_ast(input);

val args = CommandLine.arguments()
val _ = print ("Running " ^ hd args ^ "\n")
app processCmd (extractList(getArvoreSintatica(concat(readlist(hd args)))))	      
val _ = OS.Process.exit(OS.Process.success)
				
