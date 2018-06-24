use "src/interpreter_core.sml";

fun expToString(e:Ast.Exp): string =
    case e of
	Ast.IntConstant i => "Ast.IntConstant " ^ Int.toString(i) |
	Ast.VarDec (ID, exp) => "Declared var " ^ ID |
	Ast.Assign (ID, exp) => ID ^ " assigned to " ^ expToString(exp) |
	(* Ast.Int_v(i1+i2) (ID, exp) => ID ^ " assigned to " ^ expToString(eval(exp)) | *)
	_ => " "

fun transform [] = []
  | transform (x::xs) = (expToString(x)) :: (transform xs)
						     		 
fun allToStr (seq:Ast.Exp list):string list =
    transform(seq)
					 
					 
val args = CommandLine.arguments();
val _ = print ("Running " ^ hd args ^ "\n");
app processCmd (extractList(getArvoreSintatica(concat(readlist(hd args)))));

val _ = write_file(hd args ^ ".log", (allToStr(extractList(getArvoreSintatica(concat(readlist(hd args)))))));
val _ = OS.Process.exit(OS.Process.success);
