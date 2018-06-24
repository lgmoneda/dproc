(* Record functions *)
val record = ref [ ref ("", Ast.EndList)];

fun reset_record() = record := [ ref ("", Ast.EndList)];

fun isEndList(Ast.EndList) = true
      | isEndList(_) = false

fun isNil(x:(string * Ast.Value) ref list):bool =
	let val (_,value) = !(hd(x)) in
		if isEndList(value) then true else false
	end

fun insert(label, exp) = (record := ([ ref (label, exp)] @ !record));

fun look_up(label:string, rcd: (string * Ast.Value) ref list):Ast.Value = 
	let val (var_label, var_record) = !(hd(rcd))
	in 
		if var_label = label then var_record else 
			if isNil(tl(rcd)) then raise Exceptions.VarLookupError else look_up(label, tl(rcd))
	end

fun look_up_ref(label:string, rcd: (string * Ast.Value) ref list):(string * Ast.Value) ref = 
	let val (var_label, var_record) = !(hd(rcd)); val possible_match = hd(rcd)
	in 
		if var_label = label then possible_match else 
			if isNil(tl(rcd)) then raise Exceptions.VarLookupError else look_up_ref(label, tl(rcd))
	end

fun update(label:string, v:Ast.Value) = 
	let val match = look_up_ref(label, !record) in
		match := (label, v)
	end;



(* Read file function*)
fun readlist (infile : string) = let 
  val ins = TextIO.openIn infile 
  fun loop ins = 
   case TextIO.inputLine ins of 
      SOME line => line :: loop ins 
    | NONE      => [] 
in 
  loop ins before TextIO.closeIn ins 
end;


(*Semantic functions*)
fun extractList (seq:Ast.Exp):Ast.Exp list =
	case seq of 
	Ast.Sequence i => i;

fun extractListVal(vallist:Ast.Value) =
	case vallist of
		Ast.List l => l

fun eval(e:Ast.Exp):Ast.Value =
      case e of
        Ast.IntConstant i => Ast.Int_v i  |
        Ast.StringConstant s => Ast.String_v s |
        Ast.FloatConstant f => Ast.Float_v f |
        Ast.BoolConstant b => Ast.Bool_v b |
        Ast.Tuple explist => Ast.List(map eval explist) |
        Ast.InfixApp(e1, s, e2) => eval_binop(eval(e1), s, eval(e2)) |
        Ast.RelApp(e1, s, e2) => eval_relapp(eval(e1), s, eval(e2)) |
        Ast.VarRef(s) => look_up(s, !record) |
        Ast.FuncExp(f, args) => apply_func(f, args) |
        _ => raise Exceptions.OperationError

    and eval_binop(v1:Ast.Value, s:string, v2:Ast.Value):Ast.Value =
      case (v1, s, v2) of
        (Ast.Int_v i1, "+", Ast.Int_v i2) => Ast.Int_v(i1+i2) |
        (Ast.Int_v i1, "-", Ast.Int_v i2) => Ast.Int_v(i1-i2) |
        (Ast.Int_v i1, "*", Ast.Int_v i2) => Ast.Int_v(i1*i2) |
        (Ast.Int_v i1, "/", Ast.Int_v i2) => Ast.Int_v(i1 div i2) |

        (Ast.Float_v f1, "+", Ast.Float_v f2) => Ast.Float_v(f1+f2) |
        (Ast.Float_v f1, "-", Ast.Float_v f2) => Ast.Float_v(f1-f2) |
        (Ast.Float_v f1, "*", Ast.Float_v f2) => Ast.Float_v(f1*f2) |
        (Ast.Float_v f1, "/", Ast.Float_v f2) => Ast.Float_v(f1 / f2) |
        
        (Ast.String_v s1, "+", Ast.String_v s2) => Ast.String_v(s1 ^ s2) |
        _ => raise Exceptions.OperationError

    and eval_relapp(v1:Ast.Value, s:string, v2:Ast.Value):Ast.Value = 
      case (v1, s, v2) of
      	(Ast.Int_v i1, ">", Ast.Int_v i2) => Ast.Bool_v(i1 > i2) |
        (Ast.Int_v i1, ">=", Ast.Int_v i2) => Ast.Bool_v(i1 >= i2) |
        (Ast.Int_v i1, "<", Ast.Int_v i2) => Ast.Bool_v(i1 < i2) |
        (Ast.Int_v i1, "<=", Ast.Int_v i2) => Ast.Bool_v(i1 <= i2) |
        (Ast.Int_v i1, "==", Ast.Int_v i2) => Ast.Bool_v(i1 = i2) |
        (Ast.Int_v i1, "!=", Ast.Int_v i2) => Ast.Bool_v(i1 <> i2) |

        (Ast.Float_v f1, ">", Ast.Float_v f2) => Ast.Bool_v(f1 > f2) |
        (Ast.Float_v f1, ">=", Ast.Float_v f2) => Ast.Bool_v(f1 >= f2) |
        (Ast.Float_v f1, "<", Ast.Float_v f2) => Ast.Bool_v(f1 < f2) |
        (Ast.Float_v f1, "<=", Ast.Float_v f2) => Ast.Bool_v(f1 <= f2) |
        (Ast.Float_v f1, "==", Ast.Float_v f2) => Ast.Bool_v(Real.==(f1, f2)) |
        (Ast.Float_v f1, "!=", Ast.Float_v f2) => Ast.Bool_v(Real.!=(f1, f2)) |

        (Ast.Bool_v true, "and", Ast.Bool_v true) => Ast.Bool_v(true) |
        (Ast.Bool_v true, "and", Ast.Bool_v false) => Ast.Bool_v(false) |
        (Ast.Bool_v false, "and", Ast.Bool_v true) => Ast.Bool_v(false) |
        (Ast.Bool_v false, "and", Ast.Bool_v false) => Ast.Bool_v(false) |
        (Ast.Bool_v true, "or", Ast.Bool_v true) => Ast.Bool_v(true) |
        (Ast.Bool_v true, "or", Ast.Bool_v false) => Ast.Bool_v(true) |
        (Ast.Bool_v false, "or", Ast.Bool_v true) => Ast.Bool_v(true) |
        (Ast.Bool_v false, "or", Ast.Bool_v false) => Ast.Bool_v(false) |
        _ => raise Exceptions.OperationError


    and apply_func(f, args) =
    	case f of
    		"soma" => soma(eval(List.nth(args,0)), eval(List.nth(args,1))) |
    		"subtracao" => subtracao(eval(List.nth(args,0)), eval(List.nth(args,1))) |
    		"multiplicacao" => multiplicacao(eval(List.nth(args,0)), eval(List.nth(args,1))) |
    		"divisao" => divisao(eval(List.nth(args,0)), eval(List.nth(args,1))) |
    		"max" => maximo(extractListVal(eval(List.nth(args,0)))) |
    		"min" => minimo(extractListVal(eval(List.nth(args,0)))) |
    		"media" => media(extractListVal(eval(List.nth(args,0)))) |
    		"logic_comp" => logic_comp(eval(List.nth(args,0)), eval(List.nth(args,1)), eval(List.nth(args,2)), eval(List.nth(args,3)), eval(List.nth(args,4))) |
    		_ => raise Exceptions.OperationError

	
	and soma(c1:Ast.Value, c2:Ast.Value):Ast.Value =
		case c2 of
			Ast.List c2 => Ast.List(ListPair.map (fn (x, y) => eval_binop(x, "+", y) ) (extractListVal(c1), c2)) |
			Ast.Int_v i => Ast.List(map (fn x => eval_binop(x, "+", Ast.Int_v i) ) (extractListVal(c1))) |
			Ast.Float_v f => Ast.List(map (fn x => eval_binop(x, "+", Ast.Float_v f) ) (extractListVal(c1))) |
			Ast.String_v s => Ast.List(map (fn x => eval_binop(x, "+", Ast.String_v s) ) (extractListVal(c1))) |
			_ => raise Exceptions.OperationError

	and subtracao(c1:Ast.Value, c2:Ast.Value):Ast.Value =
		case c2 of
			Ast.List c2 => Ast.List(ListPair.map (fn (x, y) => eval_binop(x, "-", y) ) (extractListVal(c1), c2)) |
			Ast.Int_v i => Ast.List(map (fn x => eval_binop(x, "-", Ast.Int_v i) ) (extractListVal(c1))) |
			Ast.Float_v f => Ast.List(map (fn x => eval_binop(x, "-", Ast.Float_v f) ) (extractListVal(c1))) |
			_ => raise Exceptions.OperationError

	and multiplicacao(c1:Ast.Value, c2:Ast.Value):Ast.Value =
		case c2 of
			Ast.List c2 => Ast.List(ListPair.map (fn (x, y) => eval_binop(x, "*", y) ) (extractListVal(c1), c2)) |
			Ast.Int_v i => Ast.List(map (fn x => eval_binop(x, "*", Ast.Int_v i) ) (extractListVal(c1))) |
			Ast.Float_v f => Ast.List(map (fn x => eval_binop(x, "*", Ast.Float_v f) ) (extractListVal(c1))) |
			_ => raise Exceptions.OperationError

	and divisao(c1:Ast.Value, c2:Ast.Value):Ast.Value =
		case c2 of
			Ast.List c2 => Ast.List(ListPair.map (fn (x, y) => eval_binop(x, "/", y) ) (extractListVal(c1), c2)) |
			Ast.Int_v i => Ast.List(map (fn x => eval_binop(x, "/", Ast.Int_v i) ) (extractListVal(c1))) |
			Ast.Float_v f => Ast.List(map (fn x => eval_binop(x, "/", Ast.Float_v f) ) (extractListVal(c1))) |
			_ => raise Exceptions.OperationError

	and maximo [] = raise Empty 
 		| maximo [x:Ast.Value] = x
 		| maximo (x::xs : Ast.Value list) =
		    let 
		        val y = maximo xs
		      in
		        if isBoolTrue(eval_relapp(x, ">", y)) then x else y
		      end

	and minimo [] = raise Empty 
 		| minimo [x:Ast.Value] = x
 		| minimo (x::xs : Ast.Value list) =
		    let 
		        val y = minimo xs
		      in
		        if isBoolTrue(eval_relapp(x, "<", y)) then x else y
		      end

	and media(l: Ast.Value list) = eval_binop(sum(l), "/", Ast.Float_v(Real.fromInt(length(l)))) 

	and sum (h::t : Ast.Value list) = eval_binop(intToFloat(h), "+", sum(t)) |
		sum(nil) = Ast.Float_v(0.0)

	and logic_comp(v1:Ast.Value, s:Ast.Value, v2:Ast.Value, v3:Ast.Value, v4:Ast.Value) = 
		case s of
			Ast.String_v p =>
				Ast.List(ListPair.map (fn (x, y) => eval_logic_comp(x, p, y, v3, v4) ) (extractListVal(v1), extractListVal(v2))) |
			_ => raise Exceptions.OperationError

	and eval_logic_comp(v1, s, v2, v3, v4) = if isBoolTrue(eval_relapp(v1, s, v2)) then v3 else v4

    and processCmd (cmd:Ast.Exp) = 
		case cmd of
			Ast.VarDec(ID, exp) => insert(ID, eval(exp)) |
			Ast.Assign(ID, exp) => update(ID, eval(exp)) |
			Ast.IfThenElse(e, cmd1, cmd2) => if isBoolTrue(eval(e)) = true then processCmd(cmd1) else processCmd(cmd2) |
			_ => raise Exceptions.OperationError

	and isBoolTrue(b) = 
		case b of
			Ast.Bool_v true => true |
			Ast.Bool_v false => false

	and intToFloat(i) =
		case i of
			Ast.Int_v f => Ast.Float_v(Real.fromInt(f)) |
			Ast.Float_v f => Ast.Float_v f

(*val input = concat(readlist("test.dproc"));*)

fun getArvoreSintatica(input: string): Ast.Exp = Helpers.string_to_ast(input);

(*
val arvore_sintatica = getArvoreSintatica(input);
val cmds = extractList(arvore_sintatica);
app processCmd cmds;*)

fun makeTest(input:string) = let val _ = reset_record() in
	app processCmd (extractList(getArvoreSintatica(concat(readlist(input)))))
end;

val _ = print("\nRodando bateria de testes...\n\n\n");

val _ = print("\n Teste 1 - test_ifelse.dproc\n\n");
val _ = makeTest("testes/test_ifelse.dproc");
val _ = print("O valor da variável avg deve ser (Float_v 255.0), e foi calculada como: \n");
look_up("avg", !record);
val _ = print("O valor da variável flag deve ser (Bool_v true), e foi calculada como: \n");
look_up("flag", !record);

val _ = print("\n\n\n Teste 2 - test_log_comp.dproc\n\n");
val _ = makeTest("testes/test_log_comp.dproc");
val _ = print("O valor da variável c deve ser [Int_v 4, Int_v 4, Int_v 2, Int_v 2, Int_v 4], e foi calculada como: \n");
look_up("c", !record);
val _ = print("O valor da variável d deve ser [Int_v 1, Int_v 0, Int_v 0, Int_v 1, Int_v 0], e foi calculada como: \n");
look_up("d", !record);

val _ = print("\n\n\n Teste 3 - test_oper.dproc\n\n");
val _ = makeTest("testes/test_oper.dproc");
val _ = print("O valor da variável a deve ser [Float_v 0.6, Float_v 1.04, Float_v 0.54, Float_v 0.64], e foi calculada como: \n");
look_up("a", !record);

val _ = print("\n\n\n Teste 4 - test_strings.dproc\n\n");
val _ = makeTest("testes/test_strings.dproc");
val _ = print("O valor da variável result deve ser [String_v \"Ola, esse \", String_v \"e um \", String_v \"teste da \", String_v \"linguagem DPROC!\"], e foi calculada como: \n");
look_up("result", !record);
val _ = print("O valor da variável result2 deve ser [String_v \"oi!\", String_v \"tchau!\"], e foi calculada como: \n");
look_up("result2", !record);