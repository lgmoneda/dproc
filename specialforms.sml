structure SpecialForms = struct

(* All the special forms in the evaluator. *)
datatype specialFormName =  (* takes a tuple of 2 elements and randomly evaluates one *)
                            IF_MAYBE
                          | NTH_EVAL
                          | HANDLE_EVAL

(* Maps a string to a special form. 
   Return NONE if given string is not a special form. *)
fun nameToSpecialFormName(fname:string):specialFormName option = 
  case fname of 
    "ifmaybe" => SOME IF_MAYBE
  | "nth_eval" => SOME NTH_EVAL
  | "handle_eval" => SOME HANDLE_EVAL
  | _ => NONE

end