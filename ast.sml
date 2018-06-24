structure
Ast = struct

  datatype Exp =
      IntConstant of int
    | FloatConstant of real
    | StringConstant of string
    | BoolConstant of bool
    | App of Exp * Exp
    | InfixApp of Exp * string * Exp
    | Tuple of Exp list
    | Sequence of Exp list
    | IfThenElse of Exp * Exp * Exp
    | VarDec of string * Exp
    | VarRef of string
    | FuncExp of string * Exp list
    | Assign of string * Exp
    | LogApp of Exp * string * Exp
    | RelApp of Exp * string * Exp
  ;

  datatype Value = Int_v of int
    | String_v of string
    | Float_v of real
    | Bool_v of bool
    | List of Value list
    | EndList
    ;

end