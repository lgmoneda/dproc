structure TypeChecker : TYPECHECKER = struct

open AbstractSyntaxTree
open SpecialForms

(* to be thrown if a type error is found *)
exception StaticTypeError of string

(* the environment used for looking up types *)
type typenv = (id * typ) list

(* The empty type environment *)
(* was: val empty_typenv = [] *)
val empty_typenv = [("fact", FN_T(INT_T, INT_T))]


fun tenv_lookup(var:id, env:typenv):typ =
  case List.find (fn(x, _) => x = var) env of
    SOME (x, v) => v
  | NONE => 
     raise StaticTypeError ("Identifier " ^ var ^ " does not have a defined type")

fun tenv_add(var:id, t:typ, env:typenv):typenv =
    (var, t)::env

fun type_unification(t1:typ, t2:typ, err_str:string):typ =
  case (t1, t2) of
    (EXCEPTION_T, _) => t2
  | (_, EXCEPTION_T) => t1

(*
    (* An alternative  type unification algorithm for tuples.
       Which is better? Does is make any difference? See suggested problems! *)
  | (TUPLE_T lt1, TUPLE_T lt2) =>
      if not(length lt1 = length lt2)
      then raise StaticTypeError err_str
      else TUPLE_T (map (fn (t1, t2) => type_unification(t1, t2, err_str))
                        (ListPair.zip(lt1, lt2)))
*)

  | (TUPLE_T lt1, TUPLE_T lt2) =>
      if not(length lt1 = length lt2)
      then raise StaticTypeError err_str
      else let
        val tr = map (fn (t1, t2) => type_unification(t1, t2, err_str))
                     (ListPair.zip(lt1, lt2))
        val exc = List.exists (fn t => t = EXCEPTION_T) tr
      in
        if exc
        then (print "aaaaa"; EXCEPTION_T)
        else (print "bbbbb"; TUPLE_T tr)
      end
  | _ => if t1 = t2
         then t1
         else raise StaticTypeError err_str

(* for unifying lists of types *)
fun tunif(tlist:typ list, err_str:string):typ =
  case tlist of
    [] => raise Fail "tunif called incorrectly."
  | [x] => x
  | x::xs => type_unification(x, tunif(xs, err_str), err_str)

fun get_type(e:exp, tenv:typenv):typ =
  case e of
    INT_E _ => INT_T
  | STRING_E _ => STRING_T
  | TRUE_E => BOOL_T
  | FALSE_E => BOOL_T
  | VAR_E x => tenv_lookup(x, tenv)
  | TUPLE_E elist => TUPLE_T (List.map (fn x => get_type(x, tenv)) elist)

(*
    (* Initial implementation of APP_E - note that it does not deal
       with any special form, thus they can not be typechecked. *)

  | APP_E(e1, e2) =>
    (case (get_type(e1, tenv), get_type(e2, tenv)) of
      (EXCEPTION_T, _) => EXCEPTION_T
    | (FN_T(ta, tb), EXCEPTION_T) => tb
    | (FN_T(ta, tb), t2) =>
        (type_unification(ta, t2, "Arg to application has wrong type");
         tb)
    | (_, _) => raise StaticTypeError "Function expected.")

*)

  | APP_E(e1, e2) =>
    let
      val specForm = case e1 of
                       VAR_E x => (case SpecialForms.nameToSpecialFormName x of
                                     SOME t => SOME t
                                   | NONE => NONE)
                     | _ => NONE
    in
      case specForm of
        SOME s => typeCheckSpecialForm(s, e2, tenv)
      | NONE => 
          (case (get_type(e1, tenv), get_type(e2, tenv)) of
	     (EXCEPTION_T, _) => EXCEPTION_T
          
(*
            (* Initial, worse implementation. See lecture notes for lecture 25.
                for a detailed discussion on the treatment of FN_T. *)
	   | (_, EXCEPTION_T) => EXCEPTION_T
*)
             (* Better solution for for FN_T. See lecture notes for lecture 25. *)
           |  (FN_T(ta, tb), EXCEPTION_T) => tb
	   | (FN_T(ta, tb), t2) =>
	       (type_unification(ta, t2, "Arg to application has wrong type");
		tb)
	   | (_, _) => raise StaticTypeError "Function expected.")
     end 

  | PROJ_E(i, e1) =>
    (case get_type(e1, tenv) of
       TUPLE_T tlist =>
         if i < 1 orelse i > (List.length tlist)
         then raise StaticTypeError ("Index "^(Int.toString i) ^ " out of bounds")
         else List.nth(tlist, i - 1)
     | EXCEPTION_T => EXCEPTION_T
     | _ => raise StaticTypeError "Tuple projection on a non-tuple")
  | FN_E(x, t, e) => FN_T(t, get_type(e, tenv_add(x, t, tenv)))
  | IF_E(e1, e2, e3) =>
    (type_unification(get_type(e1, tenv), BOOL_T,
       "Condition of IF does not evaluate to boolean.");
     type_unification(get_type(e2, tenv), get_type(e3, tenv),
       "Branches of IF not same type"))
  | BINOP_E(e1, b, e2) => get_type_binop(e1, b, e2, tenv)
  | UNOP_E(u, e1) => get_type_unop(u, e1, tenv)
  | ANDALSO_E(e1, e2) =>
      tunif([BOOL_T, get_type(e1, tenv), get_type(e2, tenv)],
        "andalso requires two bool exps")
  | ORELSE_E(e1, e2) =>
      tunif([BOOL_T, get_type(e1, tenv), get_type(e2, tenv)],
        "orelse requires two bool exps")
  | LET_E(FUN_D(fname, x, at, rt, e1), e2) =>
    let
      val declaredFnType = FN_T(at, rt)
      val foundFnType = get_type(e1, tenv_add(fname, declaredFnType, 
                                       tenv_add(x, at, tenv)))
      val _ = type_unification(rt, foundFnType, 
                "Declared function type diffrent from actual")
    in
      get_type(e2, tenv_add(fname, declaredFnType, tenv))
    end
  | LET_E(AND_D(fname1, x1, at1, rt1, e1, fname2, x2, at2, rt2, e2), e3) =>
    let
      val declaredFnType1 = FN_T(at1, rt1)
      val declaredFnType2 = FN_T(at2, rt2)
      val foundFnType1 = get_type(e1, tenv_add(fname2, declaredFnType2,
                                        tenv_add(fname1, declaredFnType1,
                                          tenv_add(x1, at1, tenv))))
      val foundFnType2 = get_type(e2, tenv_add(fname2, declaredFnType2,
                                        tenv_add(fname1, declaredFnType1,
                                          tenv_add(x2, at2, tenv))))
      val _ = type_unification(rt1, foundFnType1,
                "Declared function type diffrent from actual")
      val _ = type_unification(rt2, foundFnType2,
                "Declared function type diffrent from actual")
    in
      get_type(e3, tenv_add(fname2, declaredFnType2,
                     tenv_add(fname1, declaredFnType1, tenv)))
    end
  | LET_E(VAL_D(x, e1), e2) => get_type(e2, tenv_add(x, get_type(e1, tenv), tenv))
  | HANDLE_E(e1, ex_name, binding, e2) =>
      type_unification(get_type(e1, tenv),
                         get_type(e2, tenv_add(binding, STRING_T, tenv)),
                           "Handle expression must have same type as inner exp")
  | RAISE_E(ex_name, e1) =>
      (case get_type(e1, tenv) of
         STRING_T => EXCEPTION_T
       | EXCEPTION_T => EXCEPTION_T
       | _ => raise StaticTypeError "Raise expression must evaluate to string.")

and get_type_unop(u:unop, e:exp, tenv:typenv):typ =
  case (u, get_type(e, tenv)) of
    (NOT_U, t) =>
       type_unification(BOOL_T, t, "NOT applied to non-bool expression")
  | (NEG_U, t) =>
       type_unification(INT_T, t, "NEG applied to non-int expression")

and get_type_binop(e1:exp, b:binop, e2:exp, tenv:typenv):typ =
  case (get_type(e1, tenv), b, get_type(e2, tenv)) of
    (t1, PLUS_B, t2) => 
      tunif([INT_T, t1, t2], "+ called on non-int arguments")
  | (t1, MINUS_B, t2) =>
      tunif([INT_T, t1, t2], "- called on non-int arguments")
  | (t1, TIMES_B, t2) =>
      tunif([INT_T, t1, t2], "* called on non-int arguments")
  | (t1, DIV_B, t2) =>
      tunif([INT_T, t1, t2], "div called on non int arguments")
  | (t1, MOD_B, t2) =>
      tunif([INT_T, t1, t2], "mod called on non int arguments")
  | (t1, EQ_B, t2) =>
      (case tunif([t1, t2], "= called on different typed expressions") of
         FN_T(_, _) => raise StaticTypeError "= called on FN types"
       | _ => BOOL_T)
  | (t1, NEQ_B, t2) =>
      (case tunif([t1, t2], "<> called on different typed expressions") of
         FN_T(_, _) => raise StaticTypeError "<> called on FN types"
       | _ => BOOL_T)
  | (t1, GT_B, t2) =>
      (tunif([INT_T, t1, t2], "> called on non int args"); BOOL_T)
  | (t1, GEQ_B, t2) =>
      (tunif([INT_T, t1, t2], ">= called on non-int args"); BOOL_T)
  | (t1, LT_B, t2) =>
      (tunif([INT_T, t1, t2], "< called on non-int args"); BOOL_T)
  | (t1, LEQ_B, t2) =>
      (tunif([INT_T, t1, t2], "<= called on non-int args"); BOOL_T)
  | (t1, CARET_B, t2) =>
      tunif([STRING_T, t1, t2], "^ called on non-string arguments")

and typeCheckSpecialForm(fname:specialFormName, expr:exp, tenv:typenv):typ =
  case fname of

    IF_MAYBE => (case expr of
                   TUPLE_E [e1, e2] =>
                     type_unification(get_type(e1, tenv),
                                      get_type(e2, tenv),
                                      "args must be of the same type")
                 | _ => raise StaticTypeError "ifmaybe takes exactly two args")
 
  | (NTH_EVAL | HANDLE_EVAL) => raise Fail "not implemented" 

end