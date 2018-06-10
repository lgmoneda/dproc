structure PrettyPrinter = struct

open AbstractSyntaxTree

fun tuple2str(f:'a -> string) (l:'a list):string = let
  fun inner(l:'a list):string =
    case l of
      [] => ""
    | [x] => f x
    | x::xs => (f x) ^ ", " ^ (inner xs)
in
  "(" ^ (inner l) ^ ")"
end


fun value2str(v:value):string =
  case v of
    INT_V i => Int.toString i
  | STRING_V s => "\"" ^ s  ^ "\""
  | BOOL_V b => Bool.toString b
  | EXCEPTION_V(id, s) => "exception "^ id ^ " with message  \"" ^ s ^ "\""
  | TUPLE_V(vlist) => tuple2str value2str vlist
  | FN_V(name, _, _) => "closure (arg name = " ^ name ^ ")"

fun b2str(b:binop):string =
  case b of
    PLUS_B => "+"
  | MINUS_B => "-"
  | TIMES_B => "*"
  | DIV_B => "div"
  | MOD_B => "mod"
  | EQ_B => "="
  | NEQ_B => "<>"
  | GT_B => ">"
  | GEQ_B => ">="
  | LT_B => "<"
  | LEQ_B => "<="
  | CARET_B => "^"

fun u2str(u:unop):string =
  case u of
    NOT_U => "not"
  | NEG_U => "~"


fun type2str(t:typ):string = let
  fun typetuple2str(tlist:typ list):string =
    case tlist of
      [] => ""
    | [x] => type2str x
    | x::xs => (type2str x) ^ " * " ^ (typetuple2str xs)
in
  case t of
    INT_T => "int"
  | STRING_T => "string"
  | BOOL_T => "bool"
  | TUPLE_T tlist =>typetuple2str tlist
  | FN_T(t1, t2) => (type2str t1) ^ "->" ^  (type2str t2)
  | EXCEPTION_T => raise Fail ("This type only used interally by typechecker." ^ 
                               " Shouldn't be printed!")
end

fun exp2str(e:exp):string =
  case e of
    INT_E i => Int.toString i
  | STRING_E s => "\"" ^ s ^ "\""
  | TRUE_E => "true"
  | FALSE_E => "false"
  | VAR_E v => v
  | TUPLE_E elist => tuple2str exp2str elist
  | APP_E(e1, e2) => "(" ^ (exp2str e1) ^ ") (" ^ (exp2str e2) ^ ")"
  | PROJ_E(i, e1) => "(#" ^ (Int.toString i) ^ " (" ^ (exp2str e1) ^"))"
  | FN_E(name, _, e1) => "(fn " ^ name ^ " => (" ^ (exp2str e1) ^ "))"
  | IF_E(e1, e2, e3) => "(if " ^ (exp2str e1) ^ " then " ^ (exp2str e2) ^ 
                                                " else " ^ (exp2str e3)
  | BINOP_E(e1, b, e2) => "(" ^ (exp2str e1) ^ ")" ^ (b2str b)
                                                   ^ "(" ^ (exp2str e2) ^ ")"
  | UNOP_E(u, e1) => "(" ^ (u2str u) ^ "(" ^ (exp2str e1) ^ "))"
  | ANDALSO_E(e1, e2) => "(" ^ (exp2str e1) ^ " andalso " ^ (exp2str e2) ^ ")"
  | ORELSE_E(e1, e2) => "(" ^ (exp2str e1) ^ " orelse " ^ (exp2str e2) ^ ")"
  | LET_E(dec, e1) => "let " ^ (decl2str dec) ^ " in " ^ (exp2str e1) ^ " end"
  | HANDLE_E(e1, ename, binding, e2) => "(" ^ (exp2str e1) ^ " handle " ^ ename 
                                            ^ " " ^ binding ^ " => " ^ (exp2str e2)
  | RAISE_E(ename, e1) => "raise " ^ ename ^ "(" ^ (exp2str e1) ^ ")"

and decl2str(d:decl):string =
  case d of
    FUN_D(fname, aname, at, rt, body) => 
      "fun " ^ fname 
             ^ "(" ^ aname ^ ":" ^ (type2str at) ^ "):" 
             ^ (type2str rt) ^ " = " ^ (exp2str body)
  | AND_D(fname1, aname1, at1, rt1, body1, fname2, aname2, at2, rt2, body2) =>
      "fun " ^ fname1 
             ^ "(" ^ aname1 ^ ":" ^ (type2str at1) ^ "):" 
             ^ (type2str rt1) ^ " = " ^ (exp2str body1)
             ^ "and "
             ^ fname2
             ^ "(" ^ aname2 ^ ":" ^ (type2str at2) ^ "):"
             ^ (type2str rt2) ^ " = " ^ (exp2str body2)
  | VAL_D(name, e1) => "val " ^ name ^ " = " ^ (exp2str e1)

end