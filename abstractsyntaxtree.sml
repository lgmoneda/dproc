structure AbstractSyntaxTree = struct

datatype binop = PLUS_B     (* + *)
               | MINUS_B    (* - *)
               | TIMES_B    (* * *)
               | DIV_B      (* div *)
               | MOD_B      (* mod *)
               | EQ_B       (* = *)
               | NEQ_B      (* <> *)
               | GT_B       (* > *)
               | GEQ_B      (* >= *)
               | LT_B       (* < *)
               | LEQ_B      (* <= *)
               | CARET_B    (* ^ *)

datatype unop = NOT_U       (* not *)
              | NEG_U       (* ~ *)

type id = string

type exception_name = string

datatype typ = INT_T
             | STRING_T
             | BOOL_T
             | TUPLE_T of typ list
             | FN_T of typ * typ
             | EXCEPTION_T   (* only used interally by typechecker *)

datatype decl = (* fn_name, arg_name, arg_type, ret_type, body *)
                FUN_D of id * id * typ * typ * exp 
              | AND_D of id * id * typ * typ * exp * 
                         id * id * typ * typ * exp
              | VAL_D of id * exp

and exp = INT_E of int
  | STRING_E of string
  | TRUE_E
  | FALSE_E
  | VAR_E of id
  | TUPLE_E of exp list
  | APP_E of exp * exp
  | PROJ_E of int * exp
  | FN_E of id * typ * exp
  | IF_E of exp * exp * exp
  | BINOP_E of exp * binop * exp
  | ANDALSO_E of exp * exp
  | ORELSE_E of exp * exp
  | UNOP_E of unop * exp
  | LET_E of decl * exp
    (* e1 handle Fail s => e2 *)
  | HANDLE_E of exp * exception_name * id * exp
    (* raise Fail "oops!" *)
  | RAISE_E of exception_name * exp

datatype value = INT_V of int
               | STRING_V of string
               | BOOL_V of bool
               | EXCEPTION_V of id * string
               | TUPLE_V of value list
                 (* pointer to environment fn declared in *)
               | FN_V of id * exp * (environment ref)

withtype environment = (id * value) list

(* implementation of environements *)

fun env_lookup(var:id, env:environment):value =
  case List.find (fn(x, _) => x = var) env of
    SOME (x, v) => v
  | NONE => raise Fail "Type-checker was implemented incorrectly."

fun env_add(var:id, v:value, env:environment):environment =
  (var, v)::env

(* the empty environment, for starting out *)
val empty_env = []

end
