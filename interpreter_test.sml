(*fun makeTest(input: string): MumlTokens.token list = Helpers.string_to_tokens(input);*)

(* Record functions *)
val record = ref [("", Ast.EndList)];

fun isEndList(Ast.EndList) = true
      | isEndList(_) = false

fun isNil(x:(string * Ast.Value) list):bool =
	let val (_,value) = hd(x) in
		if isEndList(value) then true else false
	end

fun insert(label, exp) = (record := [(label, exp)] @ !record);

fun look_up(label:string, rcd:(string * Ast.Value) list):Ast.Value = 
	let val (var_label, var_record) = hd(rcd)
	in 
		if var_label = label then var_record else 
			if isNil(tl(rcd)) then  Ast.LookupError else look_up(label, tl(rcd))
	end



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
        Ast.FuncExp(f, args) => apply_func(f, args)

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
        
        (Ast.String_v s1, "+", Ast.String_v s2) => Ast.String_v(s1 ^ s2)

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
        (Ast.Bool_v false, "or", Ast.Bool_v false) => Ast.Bool_v(false)


    and apply_func(f, args) =
    	case f of
    		"soma" => soma(eval(List.nth(args,0)), eval(List.nth(args,1))) |
    		"subtracao" => subtracao(eval(List.nth(args,0)), eval(List.nth(args,1))) |
    		"multiplicacao" => multiplicacao(eval(List.nth(args,0)), eval(List.nth(args,1))) |
    		"divisao" => divisao(eval(List.nth(args,0)), eval(List.nth(args,1))) |
    		"max" => maximo(extractListVal(eval(List.nth(args,0)))) |
    		"min" => minimo(extractListVal(eval(List.nth(args,0))))

	
	and soma(c1:Ast.Value, c2:Ast.Value):Ast.Value =
		case c2 of
			Ast.List c2 => Ast.List(ListPair.map (fn (x, y) => eval_binop(x, "+", y) ) (extractListVal(c1), c2)) |
			Ast.Int_v i => Ast.List(map (fn x => eval_binop(x, "+", Ast.Int_v i) ) (extractListVal(c1))) |
			Ast.Float_v f => Ast.List(map (fn x => eval_binop(x, "+", Ast.Float_v f) ) (extractListVal(c1)))

	and subtracao(c1:Ast.Value, c2:Ast.Value):Ast.Value =
		case c2 of
			Ast.List c2 => Ast.List(ListPair.map (fn (x, y) => eval_binop(x, "-", y) ) (extractListVal(c1), c2)) |
			Ast.Int_v i => Ast.List(map (fn x => eval_binop(x, "-", Ast.Int_v i) ) (extractListVal(c1))) |
			Ast.Float_v f => Ast.List(map (fn x => eval_binop(x, "-", Ast.Float_v f) ) (extractListVal(c1)))

	and multiplicacao(c1:Ast.Value, c2:Ast.Value):Ast.Value =
		case c2 of
			Ast.List c2 => Ast.List(ListPair.map (fn (x, y) => eval_binop(x, "*", y) ) (extractListVal(c1), c2)) |
			Ast.Int_v i => Ast.List(map (fn x => eval_binop(x, "*", Ast.Int_v i) ) (extractListVal(c1))) |
			Ast.Float_v f => Ast.List(map (fn x => eval_binop(x, "*", Ast.Float_v f) ) (extractListVal(c1)))

	and divisao(c1:Ast.Value, c2:Ast.Value):Ast.Value =
		case c2 of
			Ast.List c2 => Ast.List(ListPair.map (fn (x, y) => eval_binop(x, "/", y) ) (extractListVal(c1), c2)) |
			Ast.Int_v i => Ast.List(map (fn x => eval_binop(x, "/", Ast.Int_v i) ) (extractListVal(c1))) |
			Ast.Float_v f => Ast.List(map (fn x => eval_binop(x, "/", Ast.Float_v f) ) (extractListVal(c1)))

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

    and processCmd (cmd:Ast.Exp) = 
		case cmd of
			Ast.VarDec(ID, exp) => insert(ID, eval(exp)) |
			Ast.Assign(ID, exp) => insert(ID, eval(exp)) |
			Ast.IfThenElse(e, cmd1, cmd2) => if isBoolTrue(eval(e)) = true then processCmd(cmd1) else processCmd(cmd2)

	and isBoolTrue(b) = 
		case b of
			Ast.Bool_v true => true |
			Ast.Bool_v false => false

	and check_list_type(vallist:Ast.Value) =
		case hd(extractListVal(vallist)) of
			Ast.Int_v i => "int" |
			Ast.Float_v f => "float" |
			Ast.String_v s => "string" |
			Ast.Bool_v b => "bool";

val input = concat(readlist("test.dproc"));

fun makeTest(input: string): Ast.Exp = Helpers.string_to_ast(input);

val arvore_sintatica = makeTest(input);

val cmds = extractList(arvore_sintatica);

val results = map processCmd cmds; 