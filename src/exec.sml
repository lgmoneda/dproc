use "src/interpreter.sml";

fun run_program(input:string) = let val _ = reset_record() in
	app processCmd (extractList(getArvoreSintatica(concat(readlist(input)))))
end;

fun expToString(e:Ast.Exp): string =
    case e of
	Ast.IntConstant i => "Ast.IntConstant " ^ Int.toString(i) |
	Ast.VarDec (ID, exp) => "Declared var " ^ ID |
	Ast.Assign (ID, exp) => ID ^ " assigned to " ^ expToString(exp) |
	(* Ast.Int_v(i1+i2) (ID, exp) => ID ^ " assigned to " ^ expToString(eval(exp)) | *)
	_ => " "

(* fun printStoredVar(e: Ast.Exp): Ast.Value = *)
(*     case e of *)
(* 	Ast.VarDec (ID, exp) =>  look_up(ID, !record) *)
(*       | _ => look_up("", !record) *)

fun printStoredVar(e: Ast.Exp): string  =
    case e of
	Ast.VarDec (ID, exp) =>  ID
      | _ => ""

fun isDeclVar(e: Ast.Exp): bool  =
    case e of
	Ast.VarDec (ID, exp) =>  true
      | _ => false
		 
fun transform [] = []
  | transform (x::xs) = (expToString(x)) :: (transform xs)

fun allToStr (seq:Ast.Exp list):string list =
    transform(seq)

fun allToId (seq:Ast.Exp list):string list =
    map printStoredVar seq

fun showStoredVars (seq:Ast.Exp list): string list =
    map printStoredVar seq
						   
fun keepVarDecl (seq:Ast.Exp list): Ast.Exp list =
    List.filter isDeclVar seq
	   
fun looks (var_name: string) =
(* val _ = print(var_name); *)
    look_up(var_name, !record)
					 
val args = CommandLine.arguments();
val _ = print ("\n\n\n\nRunning " ^ hd args ^ "\n");
run_program(hd args);


val _ = print ("Varibles: \n");
(allToId(keepVarDecl(extractList(getArvoreSintatica(concat(readlist(hd args)))))));

val _ = print ("Their values: \n");
map looks (allToId(keepVarDecl(extractList(getArvoreSintatica(concat(readlist(hd args)))))));

val _ = write_file(hd args ^ ".log", (allToStr(extractList(getArvoreSintatica(concat(readlist(hd args)))))));

val _ = OS.Process.exit(OS.Process.success);
