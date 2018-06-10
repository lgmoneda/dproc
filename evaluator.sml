structure Evaluator : EVALUATOR = struct

open AbstractSyntaxTree
open SpecialForms

(* Creates the seed for the random number generator. *)
val rndSeed = Random.rand (10, 20)


val brokenTypes = Fail "Type-checker implemented incorrectly if this happened."

fun eval_binop(e1:exp, b:binop, e2:exp, env:environment):value =
  case (eval(e1, env), b, eval(e2, env)) of

    (EXCEPTION_V x, _, _) => EXCEPTION_V x
  | (_, _, EXCEPTION_V x) => EXCEPTION_V x

  | (STRING_V s1, CARET_B, STRING_V s2) => STRING_V(s1 ^ s2)
                                           
  | (INT_V i1, PLUS_B, INT_V i2) => INT_V(i1 + i2)
  | (INT_V i1, MINUS_B, INT_V i2) => INT_V(i1 - i2)
  | (INT_V i1, TIMES_B, INT_V i2) => INT_V(i1 * i2)
  | (INT_V i1, DIV_B, INT_V i2) => INT_V(i1 div i2)
  | (INT_V i1, MOD_B, INT_V i2) => INT_V(i1 mod i2)

  | (INT_V i1, EQ_B, INT_V i2) => BOOL_V(i1 = i2)
  | (BOOL_V b1, EQ_B, BOOL_V b2) => BOOL_V(b1 = b2)
  | (TUPLE_V t1, EQ_B, TUPLE_V t2) => BOOL_V(t1 = t2)
  | (STRING_V t1, EQ_B, STRING_V t2) => BOOL_V(t1 = t2)

  | (INT_V i1, NEQ_B, INT_V i2) => BOOL_V(i1 <> i2)
  | (BOOL_V b1, NEQ_B, BOOL_V b2) => BOOL_V(b1 <> b2)
  | (TUPLE_V t1, NEQ_B, TUPLE_V t2) => BOOL_V(t1 <> t2)
  | (STRING_V t1, NEQ_B, STRING_V t2) => BOOL_V(t1 = t2)

  | (INT_V i1, GT_B, INT_V i2) => BOOL_V(i1 > i2)
  | (INT_V i1, GEQ_B, INT_V i2) => BOOL_V(i1 >= i2)
  | (INT_V i1, LT_B, INT_V i2) => BOOL_V(i1 < i2)
  | (INT_V i1, LEQ_B, INT_V i2) => BOOL_V(i1 <= i2)

  | (STRING_V t1, GT_B, STRING_V t2) => BOOL_V(t1 > t2)
  | (STRING_V t1, GEQ_B, STRING_V t2) => BOOL_V(t1 >= t2)
  | (STRING_V t1, LT_B, STRING_V t2) => BOOL_V(t1 < t2)
  | (STRING_V t1, LEQ_B, STRING_V t2) => BOOL_V(t1 <= t2)

  | _ => raise brokenTypes

and eval_unop(u:unop, e:exp, env:environment):value =
  case (u, eval(e, env)) of
    (_, EXCEPTION_V(x, s)) => EXCEPTION_V(x,s)
  | (NOT_U, BOOL_V b) => BOOL_V(not b)
  | (NEG_U, INT_V i) => INT_V(~ i)
  | _ => raise brokenTypes

and eval(e:exp, env:environment):value =
  case e of
    INT_E i => INT_V i
  | STRING_E s => STRING_V s
  | TRUE_E => BOOL_V true
  | VAR_E x => env_lookup(x, env)
  | FALSE_E => BOOL_V false
  | TUPLE_E(elist) =>
    let
      val inner = List.map (fn(e) => eval(e, env)) elist
      fun findException(vlist:value list):value option =
        List.find (fn v => (case v of
                              EXCEPTION_V _ => true
                            | _ => false)) vlist
    in
      case findException inner of
        SOME v => v
      | NONE => TUPLE_V inner
    end

(*
    (* version of APP_E that evaluates both the function and the arguments,
       irrespective of any exception that might have been raised *)

  | APP_E(e1, e2) =>
    (case expToSpecialFormName e1 of
         SOME s => evaluateSpecialForm(s, e2, env)
       | NONE => 
           (case (eval(e1, env), eval(e2, env)) of
              (EXCEPTION_V(x, s), _) => EXCEPTION_V(x, s)
            | (_, EXCEPTION_V(x, s)) => EXCEPTION_V(x, s)
            | (FN_V(x, body, renv), v2) => eval(body, env_add(x, v2, !renv))))
*)

  | APP_E(e1, e2) =>
    (* call the special form, if it is one *)
    (case expToSpecialFormName e1 of
         SOME s => evaluateSpecialForm(s, e2, env)
       | NONE => 
         (case eval(e1, env) of
	    EXCEPTION_V(x, s) => EXCEPTION_V(x, s)
	  | FN_V(x, body, closure_env) =>
	    (case eval(e2, env) of
	       EXCEPTION_V(x, s) => EXCEPTION_V(x, s)
	     | v2 => eval(body, env_add(x, v2, !closure_env)))
	  | _ => raise brokenTypes))

  | PROJ_E(i, e1) =>
    (case eval(e1, env) of
      EXCEPTION_V(x,s) => EXCEPTION_V(x,s)
    | TUPLE_V(vlist) => (List.nth(vlist, i - 1)
                         handle Subscript => raise brokenTypes)
    | _ => raise brokenTypes)
  | FN_E(x, _, e1) => FN_V(x, e1, ref env)
  | IF_E(e1, e2, e3) =>
    (case eval(e1, env) of
      EXCEPTION_V(x,s) => EXCEPTION_V(x,s)
    | BOOL_V b => if b then eval(e2, env) else eval(e3, env)
    | _ => raise brokenTypes)
  | BINOP_E(e1, b, e2) => eval_binop(e1, b, e2, env)
  | ANDALSO_E(e1, e2) =>
    (case eval(e1, env) of
       EXCEPTION_V(x,s)  => EXCEPTION_V(x,s)
     | BOOL_V b => if b then eval(e2, env) else BOOL_V false
     | _ => raise brokenTypes)
  | ORELSE_E(e1, e2) =>
    (case eval(e1, env) of
      EXCEPTION_V(x,s) => EXCEPTION_V(x,s)
    | BOOL_V b => if b then BOOL_V true else eval(e2, env)
    | _ => raise brokenTypes)
  | UNOP_E(u, e1) => eval_unop(u, e1, env)
  | LET_E(FUN_D(fnname, x, _, _, e1), e2) =>
      let
        val hole = ref []
        val env_with_fn_def = env_add(fnname, FN_V(x, e1, hole), env)
        val () = hole := env_with_fn_def
      in
        eval(e2, env_with_fn_def)
      end
  | LET_E(AND_D(fname1, x1, _, _, e1, fname2, x2, _, _, e2), e3) =>
      let
        val hole = ref []
        val env_with_fn_defs = env_add(fname2, FN_V(x2, e2, hole), 
                                       env_add(fname1, FN_V(x1, e1, hole), env))
        val () = hole := env_with_fn_defs
      in
        eval(e3, env_with_fn_defs)
      end
  | LET_E(VAL_D(x, e1), e2) =>
    let
      val v1 = eval(e1, env)
    in
      case v1 of
        EXCEPTION_V _ => v1
      | _ => eval(e2, env_add(x, v1, env))
    end
  | HANDLE_E(e1, ex_name, binding, e2) =>
    (case eval(e1, env) of
       EXCEPTION_V(x,s) => if x = ex_name
                           then eval(e2, env_add(binding, STRING_V s, env))
                           else EXCEPTION_V(x,s)
     | x => x)
  | RAISE_E(x, e1) =>
    (case eval(e1, env) of
       EXCEPTION_V(x2, s2) => EXCEPTION_V(x2, s2)
     | STRING_V s => EXCEPTION_V(x, s)
     | _ => raise brokenTypes)

and expToSpecialFormName(e:exp):specialFormName option = 
  case e of
    VAR_E x => (case nameToSpecialFormName x of
                  SOME t => SOME t
                | NONE => NONE)
  | _ => NONE

(* evaluates a special form *)
and evaluateSpecialForm(fname:specialFormName, expr:exp, env:environment):value = 
  case fname of

    IF_MAYBE => (case expr of
                   TUPLE_E [e1, e2] =>
                     eval(if (Random.randRange (0, 1) rndSeed) = 1
                            then e1
                            else e2,
                          env)
                 | _ => EXCEPTION_V("Runtime", "IF_MAYBE needs two arguments"))

  | NTH_EVAL => raise Fail "Not implemented"
  | HANDLE_EVAL => raise Fail "Not implemented"

end
