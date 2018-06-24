structure DprocTokens =
  struct
    datatype token
      = KW_if
      | KW_then
      | KW_else
      | CON_int of Int.int
      | CON_string of string
      | ID of string
      | OP of string
      | LP
      | RP
      | COMMA
      | SEMI
      | LB
      | RB
      | CON_float of real
      | KW_decl of string
      | KW_func of string
      | KW_teste
      | ASSIGN
      | OP_rel of string
      | OP_log of string
      | CON_bool of bool
      | EOF
    val allToks = [
            KW_if, KW_then, KW_else, LP, RP, COMMA, SEMI, LB, RB, KW_teste, ASSIGN, EOF
           ]
    fun toString tok =
(case (tok)
 of (KW_if) => "if"
  | (KW_then) => "then"
  | (KW_else) => "else"
  | (CON_int(_)) => "CON_int"
  | (CON_string(_)) => "CON_string"
  | (ID(_)) => "ID"
  | (OP(_)) => "OP"
  | (LP) => "("
  | (RP) => ")"
  | (COMMA) => ","
  | (SEMI) => ";"
  | (LB) => "["
  | (RB) => "]"
  | (CON_float(_)) => "CON_float"
  | (KW_decl(_)) => "KW_decl"
  | (KW_func(_)) => "KW_func"
  | (KW_teste) => "teste"
  | (ASSIGN) => "="
  | (OP_rel(_)) => "OP_rel"
  | (OP_log(_)) => "OP_log"
  | (CON_bool(_)) => "CON_bool"
  | (EOF) => "EOF"
(* end case *))
    fun isKW tok =
(case (tok)
 of (KW_if) => false
  | (KW_then) => false
  | (KW_else) => false
  | (CON_int(_)) => false
  | (CON_string(_)) => false
  | (ID(_)) => false
  | (OP(_)) => false
  | (LP) => false
  | (RP) => false
  | (COMMA) => false
  | (SEMI) => false
  | (LB) => false
  | (RB) => false
  | (CON_float(_)) => false
  | (KW_decl(_)) => false
  | (KW_func(_)) => false
  | (KW_teste) => false
  | (ASSIGN) => false
  | (OP_rel(_)) => false
  | (OP_log(_)) => false
  | (CON_bool(_)) => false
  | (EOF) => false
(* end case *))
    fun isEOF EOF = true
      | isEOF _ = false
  end (* DprocTokens *)

functor DprocParseFn (Lex : ANTLR_LEXER) = struct

  local
    structure Tok =
DprocTokens
    structure UserCode =
      struct

  structure A = Ast

fun program_PROD_1_ACT (SEMI, cmdSequence, declSequence, SEMI_SPAN : (Lex.pos * Lex.pos), cmdSequence_SPAN : (Lex.pos * Lex.pos), declSequence_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.Sequence(declSequence @ cmdSequence ))
fun program_PROD_2_ACT (cmdSequence, blockCmd, SEMI1, SEMI2, cmdSequence_SPAN : (Lex.pos * Lex.pos), blockCmd_SPAN : (Lex.pos * Lex.pos), SEMI1_SPAN : (Lex.pos * Lex.pos), SEMI2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.Sequence(blockCmd :: cmdSequence) )
fun program_PROD_4_ACT (blockDecl, declSequence, blockDecl_SPAN : (Lex.pos * Lex.pos), declSequence_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.Sequence(blockDecl :: declSequence) )
fun program_PROD_6_ACT (blockDecl, blockDecl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( blockDecl )
fun expsequence_PROD_1_ACT (SEMI, blockExp, expsequence, SEMI_SPAN : (Lex.pos * Lex.pos), blockExp_SPAN : (Lex.pos * Lex.pos), expsequence_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( blockExp :: expsequence )
fun expsequence_PROD_2_ACT (blockExp, blockExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( [blockExp] )
fun blockCmd_PROD_1_ACT (ID, opExp, ASSIGN, ID_SPAN : (Lex.pos * Lex.pos), opExp_SPAN : (Lex.pos * Lex.pos), ASSIGN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.Assign(ID, opExp) )
fun blockCmd_PROD_2_ACT (KW_else, KW_then, KW_if, opExp, blockCmd1, blockCmd2, KW_else_SPAN : (Lex.pos * Lex.pos), KW_then_SPAN : (Lex.pos * Lex.pos), KW_if_SPAN : (Lex.pos * Lex.pos), opExp_SPAN : (Lex.pos * Lex.pos), blockCmd1_SPAN : (Lex.pos * Lex.pos), blockCmd2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.IfThenElse(opExp, blockCmd1, blockCmd2) )
fun cmdSequence_PROD_1_ACT (SEMI, cmdSequence, blockCmd, SEMI_SPAN : (Lex.pos * Lex.pos), cmdSequence_SPAN : (Lex.pos * Lex.pos), blockCmd_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( blockCmd :: cmdSequence)
fun cmdSequence_PROD_2_ACT (blockCmd, blockCmd_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( [blockCmd] )
fun declSequence_PROD_1_ACT (blockDecl, declSequence, blockDecl_SPAN : (Lex.pos * Lex.pos), declSequence_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( blockDecl :: declSequence )
fun declSequence_PROD_2_ACT (blockDecl, blockDecl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( [blockDecl] )
fun blockDecl_PROD_1_ACT (ID, SEMI, KW_decl, ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_decl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.VarDec(ID, A.IntConstant(0)) )
fun blockDecl_PROD_2_ACT (ID, SEMI, KW_decl, ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_decl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.VarDec(ID, A.FloatConstant(0.0)) )
fun blockDecl_PROD_3_ACT (ID, SEMI, KW_decl, ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_decl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.VarDec(ID, A.BoolConstant(false)) )
fun blockDecl_PROD_4_ACT (ID, SEMI, KW_decl, ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_decl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.VarDec(ID, A.StringConstant("")) )
fun blockDecl_PROD_5_ACT (ID, SEMI, KW_decl, ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_decl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.VarDec(ID, A.IntConstant(0)) )
fun blockDecl_PROD_6_ACT (ID, SEMI, KW_decl, ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_decl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.VarDec(ID, A.IntConstant(0)) )
fun opExp_PROD_1_ACT (OP, opExp, appExps, OP_SPAN : (Lex.pos * Lex.pos), opExp_SPAN : (Lex.pos * Lex.pos), appExps_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.InfixApp(appExps, OP, opExp) )
fun opExp_PROD_2_ACT (OP_rel, opExp, appExps, OP_rel_SPAN : (Lex.pos * Lex.pos), opExp_SPAN : (Lex.pos * Lex.pos), appExps_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.RelApp(appExps, OP_rel, opExp) )
fun opExp_PROD_3_ACT (OP_log, opExp, appExps, OP_log_SPAN : (Lex.pos * Lex.pos), opExp_SPAN : (Lex.pos * Lex.pos), appExps_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.RelApp(appExps, OP_log, opExp) )
fun appExps_PROD_1_ACT (simpleExp, simpleExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (
      let
        fun apps(result, nil) = result
          | apps(result, hd :: tl) = A.App(apps(hd, tl), result)

        val simpleExpRev = List.rev simpleExp
      in
        apps(List.hd simpleExpRev, List.tl simpleExpRev)
      end
    )
fun simpleExp_PROD_1_ACT (CON_int, CON_int_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.IntConstant(CON_int) )
fun simpleExp_PROD_2_ACT (CON_float, CON_float_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.FloatConstant(CON_float) )
fun simpleExp_PROD_3_ACT (CON_bool, CON_bool_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.BoolConstant(CON_bool) )
fun simpleExp_PROD_4_ACT (CON_string, CON_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.StringConstant(CON_string) )
fun simpleExp_PROD_5_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.VarRef(ID) )
fun simpleExp_PROD_6_ACT (LP, RP, simpleExp, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), simpleExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( simpleExp )
fun simpleExp_PROD_7_ACT (LB, RB, blockExp, COMMA, explist, LB_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), blockExp_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), explist_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.Tuple(blockExp :: explist) )
fun explist_PROD_1_ACT (blockExp, COMMA, explist, blockExp_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), explist_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( blockExp :: explist )
fun explist_PROD_2_ACT (blockExp, blockExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( [blockExp] )
fun func_apply_PROD_1_ACT (KW_func, func_args, KW_func_SPAN : (Lex.pos * Lex.pos), func_args_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( A.FuncExp(KW_func, func_args) )
fun func_args_PROD_1_ACT (LP, RP, blockExp, COMMA, explist, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), blockExp_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), explist_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( blockExp :: explist )
fun func_args_PROD_2_ACT (LP, RP, blockExp, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), blockExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( [blockExp] )
fun blockDecl_PROD_1_PRED (ID, SEMI, KW_decl) = 
  ( KW_decl = "int" )
fun blockDecl_PROD_2_PRED (ID, SEMI, KW_decl) = 
  ( KW_decl = "float" )
fun blockDecl_PROD_3_PRED (ID, SEMI, KW_decl) = 
  ( KW_decl = "bool" )
fun blockDecl_PROD_4_PRED (ID, SEMI, KW_decl) = 
  ( KW_decl = "string" )
fun blockDecl_PROD_5_PRED (ID, SEMI, KW_decl) = 
  ( KW_decl = "column" )
fun blockDecl_PROD_6_PRED (ID, SEMI, KW_decl) = 
  ( KW_decl = "table" )
      end (* UserCode *)

    structure Err = AntlrErrHandler(
      structure Tok = Tok
      structure Lex = Lex)

(* replace functor with inline structure for better optimization
    structure EBNF = AntlrEBNF(
      struct
	type strm = Err.wstream
	val getSpan = Err.getSpan
      end)
*)
    structure EBNF =
      struct
	fun optional (pred, parse, strm) =
	      if pred strm
		then let
		  val (y, span, strm') = parse strm
		  in
		    (SOME y, span, strm')
		  end
		else (NONE, Err.getSpan strm, strm)

	fun closure (pred, parse, strm) = let
	      fun iter (strm, (left, right), ys) =
		    if pred strm
		      then let
			val (y, (_, right'), strm') = parse strm
			in iter (strm', (left, right'), y::ys)
			end
		      else (List.rev ys, (left, right), strm)
	      in
		iter (strm, Err.getSpan strm, [])
	      end

	fun posclos (pred, parse, strm) = let
	      val (y, (left, _), strm') = parse strm
	      val (ys, (_, right), strm'') = closure (pred, parse, strm')
	      in
		(y::ys, (left, right), strm'')
	      end
      end

    fun mk lexFn = let
fun getS() = {}
fun putS{} = ()
fun unwrap (ret, strm, repairs) = (ret, strm, repairs)
        val (eh, lex) = Err.mkErrHandler {get = getS, put = putS}
	fun fail() = Err.failure eh
	fun tryProds (strm, prods) = let
	  fun try [] = fail()
	    | try (prod :: prods) =
	        (Err.whileDisabled eh (fn() => prod strm))
		handle Err.ParseError => try (prods)
          in try prods end
fun matchKW_if strm = (case (lex(strm))
 of (Tok.KW_if, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_then strm = (case (lex(strm))
 of (Tok.KW_then, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_else strm = (case (lex(strm))
 of (Tok.KW_else, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCON_int strm = (case (lex(strm))
 of (Tok.CON_int(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchCON_string strm = (case (lex(strm))
 of (Tok.CON_string(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchID strm = (case (lex(strm))
 of (Tok.ID(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchOP strm = (case (lex(strm))
 of (Tok.OP(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchLP strm = (case (lex(strm))
 of (Tok.LP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRP strm = (case (lex(strm))
 of (Tok.RP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCOMMA strm = (case (lex(strm))
 of (Tok.COMMA, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSEMI strm = (case (lex(strm))
 of (Tok.SEMI, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLB strm = (case (lex(strm))
 of (Tok.LB, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRB strm = (case (lex(strm))
 of (Tok.RB, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCON_float strm = (case (lex(strm))
 of (Tok.CON_float(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchKW_decl strm = (case (lex(strm))
 of (Tok.KW_decl(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchKW_func strm = (case (lex(strm))
 of (Tok.KW_func(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchKW_teste strm = (case (lex(strm))
 of (Tok.KW_teste, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchASSIGN strm = (case (lex(strm))
 of (Tok.ASSIGN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOP_rel strm = (case (lex(strm))
 of (Tok.OP_rel(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchOP_log strm = (case (lex(strm))
 of (Tok.OP_log(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchCON_bool strm = (case (lex(strm))
 of (Tok.CON_bool(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchEOF strm = (case (lex(strm))
 of (Tok.EOF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))

val (program_NT) = 
let
fun blockDecl_NT (strm) = let
      fun blockDecl_PROD_1 (strm) = let
            val (KW_decl_RES, KW_decl_SPAN, strm') = matchKW_decl(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            in
              if (UserCode.blockDecl_PROD_1_PRED (ID_RES, SEMI_RES, KW_decl_RES))
                then let
                  val FULL_SPAN = (#1(KW_decl_SPAN), #2(SEMI_SPAN))
                  in
                    (UserCode.blockDecl_PROD_1_ACT (ID_RES, SEMI_RES, KW_decl_RES, ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_decl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
                else fail()
            end
      fun blockDecl_PROD_2 (strm) = let
            val (KW_decl_RES, KW_decl_SPAN, strm') = matchKW_decl(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            in
              if (UserCode.blockDecl_PROD_2_PRED (ID_RES, SEMI_RES, KW_decl_RES))
                then let
                  val FULL_SPAN = (#1(KW_decl_SPAN), #2(SEMI_SPAN))
                  in
                    (UserCode.blockDecl_PROD_2_ACT (ID_RES, SEMI_RES, KW_decl_RES, ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_decl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
                else fail()
            end
      fun blockDecl_PROD_3 (strm) = let
            val (KW_decl_RES, KW_decl_SPAN, strm') = matchKW_decl(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            in
              if (UserCode.blockDecl_PROD_3_PRED (ID_RES, SEMI_RES, KW_decl_RES))
                then let
                  val FULL_SPAN = (#1(KW_decl_SPAN), #2(SEMI_SPAN))
                  in
                    (UserCode.blockDecl_PROD_3_ACT (ID_RES, SEMI_RES, KW_decl_RES, ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_decl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
                else fail()
            end
      fun blockDecl_PROD_4 (strm) = let
            val (KW_decl_RES, KW_decl_SPAN, strm') = matchKW_decl(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            in
              if (UserCode.blockDecl_PROD_4_PRED (ID_RES, SEMI_RES, KW_decl_RES))
                then let
                  val FULL_SPAN = (#1(KW_decl_SPAN), #2(SEMI_SPAN))
                  in
                    (UserCode.blockDecl_PROD_4_ACT (ID_RES, SEMI_RES, KW_decl_RES, ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_decl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
                else fail()
            end
      fun blockDecl_PROD_5 (strm) = let
            val (KW_decl_RES, KW_decl_SPAN, strm') = matchKW_decl(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            in
              if (UserCode.blockDecl_PROD_5_PRED (ID_RES, SEMI_RES, KW_decl_RES))
                then let
                  val FULL_SPAN = (#1(KW_decl_SPAN), #2(SEMI_SPAN))
                  in
                    (UserCode.blockDecl_PROD_5_ACT (ID_RES, SEMI_RES, KW_decl_RES, ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_decl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
                else fail()
            end
      fun blockDecl_PROD_6 (strm) = let
            val (KW_decl_RES, KW_decl_SPAN, strm') = matchKW_decl(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            in
              if (UserCode.blockDecl_PROD_6_PRED (ID_RES, SEMI_RES, KW_decl_RES))
                then let
                  val FULL_SPAN = (#1(KW_decl_SPAN), #2(SEMI_SPAN))
                  in
                    (UserCode.blockDecl_PROD_6_ACT (ID_RES, SEMI_RES, KW_decl_RES, ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_decl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
                else fail()
            end
      in
        (case (lex(strm))
         of (Tok.KW_decl(_), _, strm') =>
              tryProds(strm, [blockDecl_PROD_1, blockDecl_PROD_2,
                blockDecl_PROD_3, blockDecl_PROD_4, blockDecl_PROD_5,
                blockDecl_PROD_6])
          | _ => fail()
        (* end case *))
      end
fun opExp_NT (strm) = let
      fun opExp_PROD_1 (strm) = let
            val (appExps_RES, appExps_SPAN, strm') = appExps_NT(strm)
            val (OP_RES, OP_SPAN, strm') = matchOP(strm')
            val (opExp_RES, opExp_SPAN, strm') = opExp_NT(strm')
            val FULL_SPAN = (#1(appExps_SPAN), #2(opExp_SPAN))
            in
              (UserCode.opExp_PROD_1_ACT (OP_RES, opExp_RES, appExps_RES, OP_SPAN : (Lex.pos * Lex.pos), opExp_SPAN : (Lex.pos * Lex.pos), appExps_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun opExp_PROD_2 (strm) = let
            val (appExps_RES, appExps_SPAN, strm') = appExps_NT(strm)
            val (OP_rel_RES, OP_rel_SPAN, strm') = matchOP_rel(strm')
            val (opExp_RES, opExp_SPAN, strm') = opExp_NT(strm')
            val FULL_SPAN = (#1(appExps_SPAN), #2(opExp_SPAN))
            in
              (UserCode.opExp_PROD_2_ACT (OP_rel_RES, opExp_RES, appExps_RES, OP_rel_SPAN : (Lex.pos * Lex.pos), opExp_SPAN : (Lex.pos * Lex.pos), appExps_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun opExp_PROD_3 (strm) = let
            val (appExps_RES, appExps_SPAN, strm') = appExps_NT(strm)
            val (OP_log_RES, OP_log_SPAN, strm') = matchOP_log(strm')
            val (opExp_RES, opExp_SPAN, strm') = opExp_NT(strm')
            val FULL_SPAN = (#1(appExps_SPAN), #2(opExp_SPAN))
            in
              (UserCode.opExp_PROD_3_ACT (OP_log_RES, opExp_RES, appExps_RES, OP_log_SPAN : (Lex.pos * Lex.pos), opExp_SPAN : (Lex.pos * Lex.pos), appExps_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun opExp_PROD_4 (strm) = let
            val (appExps_RES, appExps_SPAN, strm') = appExps_NT(strm)
            val FULL_SPAN = (#1(appExps_SPAN), #2(appExps_SPAN))
            in
              ((appExps_RES), FULL_SPAN, strm')
            end
      fun opExp_PROD_5 (strm) = let
            val (func_apply_RES, func_apply_SPAN, strm') = func_apply_NT(strm)
            val FULL_SPAN = (#1(func_apply_SPAN), #2(func_apply_SPAN))
            in
              ((func_apply_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_func(_), _, strm') => opExp_PROD_5(strm)
          | (Tok.CON_int(_), _, strm') =>
              tryProds(strm, [opExp_PROD_1, opExp_PROD_2, opExp_PROD_3,
                opExp_PROD_4])
          | (Tok.CON_string(_), _, strm') =>
              tryProds(strm, [opExp_PROD_1, opExp_PROD_2, opExp_PROD_3,
                opExp_PROD_4])
          | (Tok.ID(_), _, strm') =>
              tryProds(strm, [opExp_PROD_1, opExp_PROD_2, opExp_PROD_3,
                opExp_PROD_4])
          | (Tok.LP, _, strm') =>
              tryProds(strm, [opExp_PROD_1, opExp_PROD_2, opExp_PROD_3,
                opExp_PROD_4])
          | (Tok.LB, _, strm') =>
              tryProds(strm, [opExp_PROD_1, opExp_PROD_2, opExp_PROD_3,
                opExp_PROD_4])
          | (Tok.CON_float(_), _, strm') =>
              tryProds(strm, [opExp_PROD_1, opExp_PROD_2, opExp_PROD_3,
                opExp_PROD_4])
          | (Tok.CON_bool(_), _, strm') =>
              tryProds(strm, [opExp_PROD_1, opExp_PROD_2, opExp_PROD_3,
                opExp_PROD_4])
          | _ => fail()
        (* end case *))
      end
and func_apply_NT (strm) = let
      val (KW_func_RES, KW_func_SPAN, strm') = matchKW_func(strm)
      val (func_args_RES, func_args_SPAN, strm') = func_args_NT(strm')
      val FULL_SPAN = (#1(KW_func_SPAN), #2(func_args_SPAN))
      in
        (UserCode.func_apply_PROD_1_ACT (KW_func_RES, func_args_RES, KW_func_SPAN : (Lex.pos * Lex.pos), func_args_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and func_args_NT (strm) = let
      fun func_args_PROD_1 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (blockExp_RES, blockExp_SPAN, strm') = blockExp_NT(strm')
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm')
            val (explist_RES, explist_SPAN, strm') = explist_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.func_args_PROD_1_ACT (LP_RES, RP_RES, blockExp_RES, COMMA_RES, explist_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), blockExp_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), explist_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun func_args_PROD_2 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (blockExp_RES, blockExp_SPAN, strm') = blockExp_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.func_args_PROD_2_ACT (LP_RES, RP_RES, blockExp_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), blockExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LP, _, strm') =>
              tryProds(strm, [func_args_PROD_1, func_args_PROD_2])
          | _ => fail()
        (* end case *))
      end
and blockExp_NT (strm) = let
      val (opExp_RES, opExp_SPAN, strm') = opExp_NT(strm)
      val FULL_SPAN = (#1(opExp_SPAN), #2(opExp_SPAN))
      in
        ((opExp_RES), FULL_SPAN, strm')
      end
and explist_NT (strm) = let
      fun explist_PROD_1 (strm) = let
            val (blockExp_RES, blockExp_SPAN, strm') = blockExp_NT(strm)
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm')
            val (explist_RES, explist_SPAN, strm') = explist_NT(strm')
            val FULL_SPAN = (#1(blockExp_SPAN), #2(explist_SPAN))
            in
              (UserCode.explist_PROD_1_ACT (blockExp_RES, COMMA_RES, explist_RES, blockExp_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), explist_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun explist_PROD_2 (strm) = let
            val (blockExp_RES, blockExp_SPAN, strm') = blockExp_NT(strm)
            val FULL_SPAN = (#1(blockExp_SPAN), #2(blockExp_SPAN))
            in
              (UserCode.explist_PROD_2_ACT (blockExp_RES, blockExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.CON_int(_), _, strm') =>
              tryProds(strm, [explist_PROD_1, explist_PROD_2])
          | (Tok.CON_string(_), _, strm') =>
              tryProds(strm, [explist_PROD_1, explist_PROD_2])
          | (Tok.ID(_), _, strm') =>
              tryProds(strm, [explist_PROD_1, explist_PROD_2])
          | (Tok.LP, _, strm') =>
              tryProds(strm, [explist_PROD_1, explist_PROD_2])
          | (Tok.LB, _, strm') =>
              tryProds(strm, [explist_PROD_1, explist_PROD_2])
          | (Tok.CON_float(_), _, strm') =>
              tryProds(strm, [explist_PROD_1, explist_PROD_2])
          | (Tok.KW_func(_), _, strm') =>
              tryProds(strm, [explist_PROD_1, explist_PROD_2])
          | (Tok.CON_bool(_), _, strm') =>
              tryProds(strm, [explist_PROD_1, explist_PROD_2])
          | _ => fail()
        (* end case *))
      end
and appExps_NT (strm) = let
      fun appExps_PROD_1_SUBRULE_1_NT (strm) = let
            val (simpleExp_RES, simpleExp_SPAN, strm') = simpleExp_NT(strm)
            val FULL_SPAN = (#1(simpleExp_SPAN), #2(simpleExp_SPAN))
            in
              ((simpleExp_RES), FULL_SPAN, strm')
            end
      fun appExps_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.CON_int(_), _, strm') => true
              | (Tok.CON_string(_), _, strm') => true
              | (Tok.ID(_), _, strm') => true
              | (Tok.LP, _, strm') => true
              | (Tok.LB, _, strm') => true
              | (Tok.CON_float(_), _, strm') => true
              | (Tok.CON_bool(_), _, strm') => true
              | _ => false
            (* end case *))
      val (simpleExp_RES, simpleExp_SPAN, strm') = EBNF.posclos(appExps_PROD_1_SUBRULE_1_PRED, appExps_PROD_1_SUBRULE_1_NT, strm)
      val FULL_SPAN = (#1(simpleExp_SPAN), #2(simpleExp_SPAN))
      in
        (UserCode.appExps_PROD_1_ACT (simpleExp_RES, simpleExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and simpleExp_NT (strm) = let
      fun simpleExp_PROD_1 (strm) = let
            val (CON_int_RES, CON_int_SPAN, strm') = matchCON_int(strm)
            val FULL_SPAN = (#1(CON_int_SPAN), #2(CON_int_SPAN))
            in
              (UserCode.simpleExp_PROD_1_ACT (CON_int_RES, CON_int_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun simpleExp_PROD_2 (strm) = let
            val (CON_float_RES, CON_float_SPAN, strm') = matchCON_float(strm)
            val FULL_SPAN = (#1(CON_float_SPAN), #2(CON_float_SPAN))
            in
              (UserCode.simpleExp_PROD_2_ACT (CON_float_RES, CON_float_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun simpleExp_PROD_3 (strm) = let
            val (CON_bool_RES, CON_bool_SPAN, strm') = matchCON_bool(strm)
            val FULL_SPAN = (#1(CON_bool_SPAN), #2(CON_bool_SPAN))
            in
              (UserCode.simpleExp_PROD_3_ACT (CON_bool_RES, CON_bool_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun simpleExp_PROD_4 (strm) = let
            val (CON_string_RES, CON_string_SPAN, strm') = matchCON_string(strm)
            val FULL_SPAN = (#1(CON_string_SPAN), #2(CON_string_SPAN))
            in
              (UserCode.simpleExp_PROD_4_ACT (CON_string_RES, CON_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun simpleExp_PROD_5 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.simpleExp_PROD_5_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun simpleExp_PROD_6 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (simpleExp_RES, simpleExp_SPAN, strm') = simpleExp_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.simpleExp_PROD_6_ACT (LP_RES, RP_RES, simpleExp_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), simpleExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun simpleExp_PROD_7 (strm) = let
            val (LB_RES, LB_SPAN, strm') = matchLB(strm)
            val (blockExp_RES, blockExp_SPAN, strm') = blockExp_NT(strm')
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm')
            val (explist_RES, explist_SPAN, strm') = explist_NT(strm')
            val (RB_RES, RB_SPAN, strm') = matchRB(strm')
            val FULL_SPAN = (#1(LB_SPAN), #2(RB_SPAN))
            in
              (UserCode.simpleExp_PROD_7_ACT (LB_RES, RB_RES, blockExp_RES, COMMA_RES, explist_RES, LB_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), blockExp_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), explist_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LB, _, strm') => simpleExp_PROD_7(strm)
          | (Tok.ID(_), _, strm') => simpleExp_PROD_5(strm)
          | (Tok.CON_bool(_), _, strm') => simpleExp_PROD_3(strm)
          | (Tok.CON_int(_), _, strm') => simpleExp_PROD_1(strm)
          | (Tok.CON_float(_), _, strm') => simpleExp_PROD_2(strm)
          | (Tok.CON_string(_), _, strm') => simpleExp_PROD_4(strm)
          | (Tok.LP, _, strm') => simpleExp_PROD_6(strm)
          | _ => fail()
        (* end case *))
      end
fun blockCmd_NT (strm) = let
      fun blockCmd_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val (ASSIGN_RES, ASSIGN_SPAN, strm') = matchASSIGN(strm')
            val (opExp_RES, opExp_SPAN, strm') = opExp_NT(strm')
            val FULL_SPAN = (#1(ID_SPAN), #2(opExp_SPAN))
            in
              (UserCode.blockCmd_PROD_1_ACT (ID_RES, opExp_RES, ASSIGN_RES, ID_SPAN : (Lex.pos * Lex.pos), opExp_SPAN : (Lex.pos * Lex.pos), ASSIGN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun blockCmd_PROD_2 (strm) = let
            val (KW_if_RES, KW_if_SPAN, strm') = matchKW_if(strm)
            val (opExp_RES, opExp_SPAN, strm') = opExp_NT(strm')
            val (KW_then_RES, KW_then_SPAN, strm') = matchKW_then(strm')
            val (blockCmd1_RES, blockCmd1_SPAN, strm') = blockCmd_NT(strm')
            val (KW_else_RES, KW_else_SPAN, strm') = matchKW_else(strm')
            val (blockCmd2_RES, blockCmd2_SPAN, strm') = blockCmd_NT(strm')
            val FULL_SPAN = (#1(KW_if_SPAN), #2(blockCmd2_SPAN))
            in
              (UserCode.blockCmd_PROD_2_ACT (KW_else_RES, KW_then_RES, KW_if_RES, opExp_RES, blockCmd1_RES, blockCmd2_RES, KW_else_SPAN : (Lex.pos * Lex.pos), KW_then_SPAN : (Lex.pos * Lex.pos), KW_if_SPAN : (Lex.pos * Lex.pos), opExp_SPAN : (Lex.pos * Lex.pos), blockCmd1_SPAN : (Lex.pos * Lex.pos), blockCmd2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_if, _, strm') => blockCmd_PROD_2(strm)
          | (Tok.ID(_), _, strm') => blockCmd_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun declSequence_NT (strm) = let
      fun declSequence_PROD_1 (strm) = let
            val (blockDecl_RES, blockDecl_SPAN, strm') = blockDecl_NT(strm)
            val (declSequence_RES, declSequence_SPAN, strm') = declSequence_NT(strm')
            val FULL_SPAN = (#1(blockDecl_SPAN), #2(declSequence_SPAN))
            in
              (UserCode.declSequence_PROD_1_ACT (blockDecl_RES, declSequence_RES, blockDecl_SPAN : (Lex.pos * Lex.pos), declSequence_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun declSequence_PROD_2 (strm) = let
            val (blockDecl_RES, blockDecl_SPAN, strm') = blockDecl_NT(strm)
            val FULL_SPAN = (#1(blockDecl_SPAN), #2(blockDecl_SPAN))
            in
              (UserCode.declSequence_PROD_2_ACT (blockDecl_RES, blockDecl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_decl(_), _, strm') =>
              tryProds(strm, [declSequence_PROD_1, declSequence_PROD_2])
          | _ => fail()
        (* end case *))
      end
fun cmdSequence_NT (strm) = let
      fun cmdSequence_PROD_1 (strm) = let
            val (blockCmd_RES, blockCmd_SPAN, strm') = blockCmd_NT(strm)
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val (cmdSequence_RES, cmdSequence_SPAN, strm') = cmdSequence_NT(strm')
            val FULL_SPAN = (#1(blockCmd_SPAN), #2(cmdSequence_SPAN))
            in
              (UserCode.cmdSequence_PROD_1_ACT (SEMI_RES, cmdSequence_RES, blockCmd_RES, SEMI_SPAN : (Lex.pos * Lex.pos), cmdSequence_SPAN : (Lex.pos * Lex.pos), blockCmd_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun cmdSequence_PROD_2 (strm) = let
            val (blockCmd_RES, blockCmd_SPAN, strm') = blockCmd_NT(strm)
            val FULL_SPAN = (#1(blockCmd_SPAN), #2(blockCmd_SPAN))
            in
              (UserCode.cmdSequence_PROD_2_ACT (blockCmd_RES, blockCmd_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_if, _, strm') =>
              tryProds(strm, [cmdSequence_PROD_1, cmdSequence_PROD_2])
          | (Tok.ID(_), _, strm') =>
              tryProds(strm, [cmdSequence_PROD_1, cmdSequence_PROD_2])
          | _ => fail()
        (* end case *))
      end
fun program_NT (strm) = let
      fun program_PROD_1 (strm) = let
            val (declSequence_RES, declSequence_SPAN, strm') = declSequence_NT(strm)
            val (cmdSequence_RES, cmdSequence_SPAN, strm') = cmdSequence_NT(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(declSequence_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.program_PROD_1_ACT (SEMI_RES, cmdSequence_RES, declSequence_RES, SEMI_SPAN : (Lex.pos * Lex.pos), cmdSequence_SPAN : (Lex.pos * Lex.pos), declSequence_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun program_PROD_2 (strm) = let
            val (blockCmd_RES, blockCmd_SPAN, strm') = blockCmd_NT(strm)
            val (SEMI1_RES, SEMI1_SPAN, strm') = matchSEMI(strm')
            val (cmdSequence_RES, cmdSequence_SPAN, strm') = cmdSequence_NT(strm')
            val (SEMI2_RES, SEMI2_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(blockCmd_SPAN), #2(SEMI2_SPAN))
            in
              (UserCode.program_PROD_2_ACT (cmdSequence_RES, blockCmd_RES, SEMI1_RES, SEMI2_RES, cmdSequence_SPAN : (Lex.pos * Lex.pos), blockCmd_SPAN : (Lex.pos * Lex.pos), SEMI1_SPAN : (Lex.pos * Lex.pos), SEMI2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun program_PROD_3 (strm) = let
            val (blockExp_RES, blockExp_SPAN, strm') = blockExp_NT(strm)
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(blockExp_SPAN), #2(SEMI_SPAN))
            in
              ((blockExp_RES), FULL_SPAN, strm')
            end
      fun program_PROD_4 (strm) = let
            val (blockDecl_RES, blockDecl_SPAN, strm') = blockDecl_NT(strm)
            val (declSequence_RES, declSequence_SPAN, strm') = declSequence_NT(strm')
            val FULL_SPAN = (#1(blockDecl_SPAN), #2(declSequence_SPAN))
            in
              (UserCode.program_PROD_4_ACT (blockDecl_RES, declSequence_RES, blockDecl_SPAN : (Lex.pos * Lex.pos), declSequence_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun program_PROD_5 (strm) = let
            val (blockCmd_RES, blockCmd_SPAN, strm') = blockCmd_NT(strm)
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(blockCmd_SPAN), #2(SEMI_SPAN))
            in
              ((blockCmd_RES), FULL_SPAN, strm')
            end
      fun program_PROD_6 (strm) = let
            val (blockDecl_RES, blockDecl_SPAN, strm') = blockDecl_NT(strm)
            val FULL_SPAN = (#1(blockDecl_SPAN), #2(blockDecl_SPAN))
            in
              (UserCode.program_PROD_6_ACT (blockDecl_RES, blockDecl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_if, _, strm') =>
              tryProds(strm, [program_PROD_2, program_PROD_5])
          | (Tok.ID(_), _, strm') =>
              tryProds(strm, [program_PROD_2, program_PROD_3, program_PROD_5])
          | (Tok.KW_decl(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.ID(_), _, strm') =>
                    (case (lex(strm'))
                     of (Tok.SEMI, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_if, _, strm') => program_PROD_1(strm)
                            | (Tok.ID(_), _, strm') => program_PROD_1(strm)
                            | (Tok.EOF, _, strm') => program_PROD_6(strm)
                            | (Tok.KW_decl(_), _, strm') =>
                                tryProds(strm, [program_PROD_1, program_PROD_4])
                            | _ => fail()
                          (* end case *))
                      | _ => fail()
                    (* end case *))
                | _ => fail()
              (* end case *))
          | (Tok.CON_int(_), _, strm') => program_PROD_3(strm)
          | (Tok.CON_string(_), _, strm') => program_PROD_3(strm)
          | (Tok.LP, _, strm') => program_PROD_3(strm)
          | (Tok.LB, _, strm') => program_PROD_3(strm)
          | (Tok.CON_float(_), _, strm') => program_PROD_3(strm)
          | (Tok.KW_func(_), _, strm') => program_PROD_3(strm)
          | (Tok.CON_bool(_), _, strm') => program_PROD_3(strm)
          | _ => fail()
        (* end case *))
      end
in
  (program_NT)
end
val program_NT =  fn s => unwrap (Err.launch (eh, lexFn, program_NT , true) s)

in (program_NT) end
  in
fun parse lexFn  s = let val (program_NT) = mk lexFn in program_NT s end

  end

end
