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

fun eval(e:Ast.Exp):Ast.Value =
      case e of
        Ast.IntConstant i => Ast.Int_v i  |
        Ast.StringConstant s => Ast.String_v s |
        Ast.FloatConstant f => Ast.Float_v f |
        Ast.BoolConstant b => Ast.Bool_v b |
        Ast.Tuple explist => Ast.List(map eval explist) |
        Ast.InfixApp(e1, s, e2) => eval_binop(eval(e1), s, eval(e2)) |
        Ast.VarRef(s) => look_up(s, !record)

    and eval_binop(v1:Ast.Value, s:string, v2:Ast.Value):Ast.Value =
      case (v1, s, v2) of
        (Ast.Int_v i1, "+", Ast.Int_v i2) => Ast.Int_v(i1+i2) |
        (Ast.Int_v i1, "-", Ast.Int_v i2) => Ast.Int_v(i1-i2) |
        (Ast.Int_v i1, "*", Ast.Int_v i2) => Ast.Int_v(i1*i2) |
        (Ast.Int_v i1, "/", Ast.Int_v i2) => Ast.Int_v(i1 div i2) |
        (Ast.Int_v i1, ">", Ast.Int_v i2) => Ast.Bool_v(i1 > i2) |
        (Ast.Int_v i1, ">=", Ast.Int_v i2) => Ast.Bool_v(i1 >= i2) |
        (Ast.Int_v i1, "<", Ast.Int_v i2) => Ast.Bool_v(i1 < i2) |
        (Ast.Int_v i1, "<=", Ast.Int_v i2) => Ast.Bool_v(i1 <= i2) |
        (Ast.Int_v i1, "==", Ast.Int_v i2) => Ast.Bool_v(i1 = i2) |
        (Ast.Int_v i1, "!=", Ast.Int_v i2) => Ast.Bool_v(i1 <> i2) |
        (Ast.String_v s1, "+", Ast.String_v s2) => Ast.String_v(s1 ^ s2)

    and processCmd (cmd:Ast.Exp) = 
		case cmd of
			Ast.VarDec(ID, exp) => insert(ID, eval(exp)) |
			Ast.Assign(ID, exp) => insert(ID, eval(exp));



val input = concat(readlist("test.dproc"));

fun makeTest(input: string): Ast.Exp = Helpers.string_to_ast(input);

val arvore_sintatica = makeTest(input);

val cmds = extractList(arvore_sintatica);

val results = map processCmd cmds;
