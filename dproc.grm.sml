functor MiniMLLrValsFun (structure Token : TOKEN) : MiniML_LRVALS = 
struct
structure ParserData=
struct
structure Header = 
struct

open AbstractSyntaxTree


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\036\000\003\000\035\000\004\000\155\000\005\000\155\000\
\\009\000\155\000\011\000\155\000\014\000\033\000\017\000\032\000\
\\020\000\155\000\023\000\155\000\025\000\031\000\027\000\155\000\
\\029\000\155\000\038\000\024\000\040\000\023\000\041\000\155\000\
\\042\000\022\000\000\000\
\\001\000\001\000\036\000\003\000\035\000\004\000\156\000\005\000\156\000\
\\009\000\156\000\011\000\156\000\014\000\033\000\017\000\032\000\
\\020\000\156\000\023\000\156\000\025\000\031\000\027\000\156\000\
\\029\000\156\000\038\000\024\000\040\000\023\000\041\000\156\000\
\\042\000\022\000\000\000\
\\001\000\001\000\036\000\003\000\035\000\004\000\157\000\005\000\157\000\
\\009\000\157\000\011\000\157\000\014\000\033\000\017\000\032\000\
\\020\000\157\000\023\000\157\000\025\000\031\000\027\000\157\000\
\\029\000\157\000\038\000\024\000\040\000\023\000\041\000\157\000\
\\042\000\022\000\000\000\
\\001\000\001\000\036\000\003\000\035\000\004\000\158\000\005\000\158\000\
\\009\000\158\000\011\000\158\000\014\000\033\000\017\000\032\000\
\\020\000\158\000\023\000\158\000\025\000\031\000\027\000\158\000\
\\029\000\158\000\038\000\024\000\040\000\023\000\041\000\158\000\
\\042\000\022\000\000\000\
\\001\000\001\000\036\000\003\000\035\000\004\000\159\000\005\000\159\000\
\\009\000\159\000\011\000\159\000\014\000\033\000\017\000\032\000\
\\020\000\159\000\023\000\159\000\025\000\031\000\027\000\159\000\
\\029\000\159\000\038\000\024\000\040\000\023\000\041\000\159\000\
\\042\000\022\000\000\000\
\\001\000\001\000\036\000\003\000\035\000\004\000\160\000\005\000\160\000\
\\009\000\160\000\011\000\160\000\014\000\033\000\017\000\032\000\
\\020\000\160\000\023\000\160\000\025\000\031\000\027\000\160\000\
\\029\000\160\000\038\000\024\000\040\000\023\000\041\000\160\000\
\\042\000\022\000\000\000\
\\001\000\002\000\087\000\012\000\086\000\019\000\085\000\037\000\084\000\000\000\
\\001\000\004\000\093\000\000\000\
\\001\000\005\000\090\000\000\000\
\\001\000\006\000\020\000\007\000\019\000\010\000\018\000\013\000\017\000\
\\015\000\016\000\018\000\015\000\021\000\014\000\034\000\013\000\
\\037\000\012\000\039\000\011\000\044\000\010\000\045\000\009\000\
\\046\000\008\000\000\000\
\\001\000\006\000\020\000\015\000\016\000\021\000\014\000\037\000\012\000\
\\039\000\011\000\044\000\010\000\045\000\009\000\046\000\008\000\000\000\
\\001\000\008\000\045\000\022\000\044\000\000\000\
\\001\000\011\000\068\000\000\000\
\\001\000\020\000\071\000\000\000\
\\001\000\026\000\072\000\000\000\
\\001\000\026\000\088\000\000\000\
\\001\000\026\000\100\000\000\000\
\\001\000\026\000\111\000\000\000\
\\001\000\026\000\119\000\000\000\
\\001\000\026\000\122\000\000\000\
\\001\000\028\000\089\000\000\000\
\\001\000\028\000\096\000\000\000\
\\001\000\028\000\108\000\000\000\
\\001\000\029\000\000\000\000\000\
\\001\000\030\000\077\000\000\000\
\\001\000\030\000\113\000\000\000\
\\001\000\030\000\124\000\000\000\
\\001\000\037\000\048\000\044\000\047\000\000\000\
\\001\000\037\000\078\000\000\000\
\\001\000\037\000\117\000\000\000\
\\001\000\041\000\064\000\000\000\
\\001\000\041\000\105\000\000\000\
\\001\000\041\000\106\000\000\000\
\\001\000\041\000\109\000\000\000\
\\001\000\041\000\121\000\000\000\
\\001\000\044\000\041\000\000\000\
\\001\000\044\000\061\000\000\000\
\\001\000\044\000\069\000\000\000\
\\001\000\044\000\070\000\000\000\
\\001\000\044\000\073\000\000\000\
\\001\000\044\000\074\000\000\000\
\\001\000\044\000\092\000\000\000\
\\001\000\044\000\116\000\000\000\
\\001\000\044\000\118\000\000\000\
\\001\000\045\000\040\000\000\000\
\\127\000\000\000\
\\128\000\023\000\115\000\000\000\
\\129\000\000\000\
\\130\000\000\000\
\\131\000\000\000\
\\132\000\024\000\095\000\000\000\
\\133\000\000\000\
\\134\000\042\000\094\000\000\000\
\\135\000\000\000\
\\136\000\000\000\
\\137\000\000\000\
\\138\000\000\000\
\\139\000\000\000\
\\140\000\001\000\036\000\003\000\035\000\009\000\034\000\014\000\033\000\
\\017\000\032\000\025\000\031\000\030\000\030\000\031\000\029\000\
\\032\000\028\000\033\000\027\000\035\000\026\000\036\000\025\000\
\\038\000\024\000\040\000\023\000\042\000\022\000\000\000\
\\141\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\000\000\
\\145\000\000\000\
\\146\000\000\000\
\\147\000\000\000\
\\148\000\000\000\
\\149\000\006\000\020\000\015\000\016\000\021\000\014\000\037\000\012\000\
\\039\000\011\000\044\000\010\000\045\000\009\000\046\000\008\000\000\000\
\\150\000\003\000\035\000\014\000\033\000\042\000\022\000\000\000\
\\151\000\003\000\035\000\014\000\033\000\042\000\022\000\000\000\
\\152\000\000\000\
\\153\000\000\000\
\\154\000\000\000\
\\161\000\003\000\035\000\014\000\033\000\025\000\031\000\038\000\024\000\
\\040\000\023\000\042\000\022\000\000\000\
\\162\000\001\000\036\000\003\000\035\000\014\000\033\000\025\000\031\000\
\\038\000\024\000\040\000\023\000\042\000\022\000\000\000\
\\163\000\003\000\035\000\014\000\033\000\038\000\024\000\040\000\023\000\
\\042\000\022\000\000\000\
\\164\000\000\000\
\\165\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\000\000\
\\172\000\000\000\
\\173\000\000\000\
\\174\000\000\000\
\\175\000\027\000\065\000\000\000\
\"
val actionRowNumbers =
"\009\000\067\000\077\000\066\000\
\\058\000\045\000\082\000\079\000\
\\078\000\010\000\009\000\044\000\
\\080\000\035\000\010\000\011\000\
\\009\000\027\000\081\000\076\000\
\\010\000\010\000\010\000\010\000\
\\010\000\010\000\010\000\010\000\
\\010\000\010\000\010\000\010\000\
\\036\000\010\000\010\000\085\000\
\\030\000\087\000\009\000\009\000\
\\084\000\012\000\037\000\038\000\
\\013\000\014\000\039\000\070\000\
\\068\000\069\000\003\000\002\000\
\\005\000\004\000\000\000\001\000\
\\075\000\074\000\071\000\040\000\
\\072\000\073\000\083\000\009\000\
\\061\000\059\000\009\000\024\000\
\\028\000\009\000\006\000\015\000\
\\020\000\086\000\008\000\009\000\
\\041\000\007\000\052\000\050\000\
\\051\000\021\000\006\000\056\000\
\\054\000\055\000\006\000\009\000\
\\065\000\048\000\016\000\009\000\
\\006\000\006\000\009\000\031\000\
\\032\000\060\000\006\000\064\000\
\\053\000\049\000\062\000\057\000\
\\022\000\033\000\009\000\017\000\
\\063\000\006\000\025\000\009\000\
\\046\000\042\000\029\000\043\000\
\\018\000\006\000\034\000\019\000\
\\006\000\026\000\009\000\047\000\
\\023\000"
val gotoT =
"\
\\001\000\124\000\009\000\005\000\010\000\004\000\011\000\003\000\
\\012\000\002\000\013\000\001\000\000\000\
\\012\000\019\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\035\000\000\000\
\\009\000\037\000\010\000\004\000\011\000\003\000\012\000\002\000\
\\013\000\001\000\014\000\036\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\040\000\000\000\
\\002\000\041\000\000\000\
\\009\000\044\000\010\000\004\000\011\000\003\000\012\000\002\000\
\\013\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\047\000\011\000\003\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\048\000\011\000\003\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\049\000\011\000\003\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\050\000\011\000\003\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\051\000\011\000\003\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\052\000\011\000\003\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\053\000\011\000\003\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\054\000\011\000\003\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\055\000\011\000\003\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\056\000\011\000\003\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\057\000\011\000\003\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\058\000\011\000\003\000\012\000\002\000\013\000\001\000\000\000\
\\000\000\
\\010\000\060\000\011\000\003\000\012\000\002\000\013\000\001\000\000\000\
\\010\000\061\000\011\000\003\000\012\000\002\000\013\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\064\000\010\000\004\000\011\000\003\000\012\000\002\000\
\\013\000\001\000\000\000\
\\009\000\065\000\010\000\004\000\011\000\003\000\012\000\002\000\
\\013\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\037\000\010\000\004\000\011\000\003\000\012\000\002\000\
\\013\000\001\000\014\000\073\000\000\000\
\\000\000\
\\000\000\
\\009\000\074\000\010\000\004\000\011\000\003\000\012\000\002\000\
\\013\000\001\000\000\000\
\\000\000\
\\000\000\
\\009\000\077\000\010\000\004\000\011\000\003\000\012\000\002\000\
\\013\000\001\000\000\000\
\\005\000\081\000\006\000\080\000\007\000\079\000\008\000\078\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\089\000\010\000\004\000\011\000\003\000\012\000\002\000\
\\013\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\095\000\006\000\080\000\007\000\079\000\008\000\078\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\096\000\006\000\080\000\007\000\079\000\008\000\078\000\000\000\
\\009\000\097\000\010\000\004\000\011\000\003\000\012\000\002\000\
\\013\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\099\000\010\000\004\000\011\000\003\000\012\000\002\000\
\\013\000\001\000\000\000\
\\006\000\100\000\008\000\078\000\000\000\
\\005\000\101\000\006\000\080\000\007\000\079\000\008\000\078\000\000\000\
\\009\000\102\000\010\000\004\000\011\000\003\000\012\000\002\000\
\\013\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\105\000\006\000\080\000\007\000\079\000\008\000\078\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\108\000\010\000\004\000\011\000\003\000\012\000\002\000\
\\013\000\001\000\000\000\
\\000\000\
\\000\000\
\\005\000\110\000\006\000\080\000\007\000\079\000\008\000\078\000\000\000\
\\000\000\
\\009\000\112\000\010\000\004\000\011\000\003\000\012\000\002\000\
\\013\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\118\000\006\000\080\000\007\000\079\000\008\000\078\000\000\000\
\\000\000\
\\000\000\
\\005\000\121\000\006\000\080\000\007\000\079\000\008\000\078\000\000\000\
\\000\000\
\\009\000\123\000\010\000\004\000\011\000\003\000\012\000\002\000\
\\013\000\001\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 125
val numrules = 49
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | STRING of  (string)
 | INT of  (int) | ID of  (string) | ExpSeq of  (exp list)
 | AtExpSeq of  (exp) | AtExp of  (exp) | Binops of  (exp)
 | InfixExp of  (exp) | Exp of  (exp) | TypeAtom of  (typ)
 | TypeTuple of  (typ) | TypeTuple2 of  (typ list) | Type of  (typ)
 | ArgListDeclSeq of  ( ( id * typ option )  list)
 | ArgListDecl of  ( ( id * typ option )  list) | Decl of  (decl)
 | Start of  (exp)
end
type svalue = MlyValue.svalue
type result = exp
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 28) => true | _ => false
val showTerminal =
fn (T 0) => "KW_andalso"
  | (T 1) => "KW_bool"
  | (T 2) => "KW_div"
  | (T 3) => "KW_else"
  | (T 4) => "KW_end"
  | (T 5) => "KW_false"
  | (T 6) => "KW_fn"
  | (T 7) => "KW_fun"
  | (T 8) => "KW_handle"
  | (T 9) => "KW_if"
  | (T 10) => "KW_in"
  | (T 11) => "KW_int"
  | (T 12) => "KW_let"
  | (T 13) => "KW_mod"
  | (T 14) => "KW_not"
  | (T 15) => "KW_of"
  | (T 16) => "KW_orelse"
  | (T 17) => "KW_raise"
  | (T 18) => "KW_string"
  | (T 19) => "KW_then"
  | (T 20) => "KW_true"
  | (T 21) => "KW_val"
  | (T 22) => "KW_and"
  | (T 23) => "ARROW"
  | (T 24) => "CARET"
  | (T 25) => "COLON"
  | (T 26) => "COMMA"
  | (T 27) => "DARROW"
  | (T 28) => "EOF"
  | (T 29) => "EQSIGN"
  | (T 30) => "NEQ"
  | (T 31) => "GREATER"
  | (T 32) => "GREATEREQ"
  | (T 33) => "HASH"
  | (T 34) => "LESS"
  | (T 35) => "LESSEQ"
  | (T 36) => "LPAREN"
  | (T 37) => "MINUS"
  | (T 38) => "NEG"
  | (T 39) => "PLUS"
  | (T 40) => "RPAREN"
  | (T 41) => "TIMES"
  | (T 42) => "TTIMES"
  | (T 43) => "ID"
  | (T 44) => "INT"
  | (T 45) => "STRING"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36)
 $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29)
 $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22)
 $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15)
 $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8)
 $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 
0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.Exp Exp, Exp1left, Exp1right)) :: rest671))
 => let val  result = MlyValue.Start (Exp)
 in ( LrTable.NT 0, ( result, Exp1left, Exp1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.Exp Exp, _, Exp1right)) :: _ :: ( _, ( 
MlyValue.Type Type2, _, _)) :: _ :: _ :: ( _, ( MlyValue.Type Type1, _
, _)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, KW_fun1left, _)) :: rest671)) =>
 let val  result = MlyValue.Decl (FUN_D(ID1, ID2, Type1, Type2, Exp))
 in ( LrTable.NT 1, ( result, KW_fun1left, Exp1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.Exp Exp2, _, Exp2right)) :: _ :: ( _, ( 
MlyValue.Type Type4, _, _)) :: _ :: _ :: ( _, ( MlyValue.Type Type3, _
, _)) :: _ :: ( _, ( MlyValue.ID ID4, _, _)) :: _ :: ( _, ( 
MlyValue.ID ID3, _, _)) :: _ :: ( _, ( MlyValue.Exp Exp1, _, _)) :: _
 :: ( _, ( MlyValue.Type Type2, _, _)) :: _ :: _ :: ( _, ( 
MlyValue.Type Type1, _, _)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) ::
 _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, KW_fun1left, _)) :: 
rest671)) => let val  result = MlyValue.Decl (
AND_D(ID1, ID2, Type1, Type2, Exp1,
                                                 ID3, ID4, Type3, Type4, Exp2)
)
 in ( LrTable.NT 1, ( result, KW_fun1left, Exp2right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.Exp Exp, _, Exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID, _, _)) :: ( _, ( _, KW_val1left, _)) :: rest671)) =>
 let val  result = MlyValue.Decl (VAL_D(ID, Exp))
 in ( LrTable.NT 1, ( result, KW_val1left, Exp1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.Type Type, _, Type1right)) :: _ :: ( _, ( 
MlyValue.TypeTuple TypeTuple1, TypeTuple1left, _)) :: rest671)) => let
 val  result = MlyValue.Type (FN_T(TypeTuple1, Type))
 in ( LrTable.NT 4, ( result, TypeTuple1left, Type1right), rest671)

end
|  ( 5, ( ( _, ( MlyValue.TypeTuple TypeTuple, TypeTuple1left, 
TypeTuple1right)) :: rest671)) => let val  result = MlyValue.Type (
TypeTuple)
 in ( LrTable.NT 4, ( result, TypeTuple1left, TypeTuple1right), 
rest671)
end
|  ( 6, ( ( _, ( MlyValue.TypeTuple2 TypeTuple2, TypeTuple21left, 
TypeTuple21right)) :: rest671)) => let val  result = 
MlyValue.TypeTuple (
case TypeTuple2 of
                                             [] => raise Fail "This shouldn't happen"
                                           | [x] => x
                                           | x::xs => TUPLE_T TypeTuple2
)
 in ( LrTable.NT 6, ( result, TypeTuple21left, TypeTuple21right), 
rest671)
end
|  ( 7, ( ( _, ( MlyValue.TypeAtom TypeAtom, TypeAtom1left, 
TypeAtom1right)) :: rest671)) => let val  result = MlyValue.TypeTuple2
 ([TypeAtom])
 in ( LrTable.NT 5, ( result, TypeAtom1left, TypeAtom1right), rest671)

end
|  ( 8, ( ( _, ( MlyValue.TypeTuple2 TypeTuple2, _, TypeTuple21right))
 :: _ :: ( _, ( MlyValue.TypeAtom TypeAtom, TypeAtom1left, _)) :: 
rest671)) => let val  result = MlyValue.TypeTuple2 (
TypeAtom::TypeTuple2)
 in ( LrTable.NT 5, ( result, TypeAtom1left, TypeTuple21right), 
rest671)
end
|  ( 9, ( ( _, ( _, KW_int1left, KW_int1right)) :: rest671)) => let
 val  result = MlyValue.TypeAtom (INT_T)
 in ( LrTable.NT 7, ( result, KW_int1left, KW_int1right), rest671)
end
|  ( 10, ( ( _, ( _, KW_bool1left, KW_bool1right)) :: rest671)) => let
 val  result = MlyValue.TypeAtom (BOOL_T)
 in ( LrTable.NT 7, ( result, KW_bool1left, KW_bool1right), rest671)

end
|  ( 11, ( ( _, ( _, KW_string1left, KW_string1right)) :: rest671)) =>
 let val  result = MlyValue.TypeAtom (STRING_T)
 in ( LrTable.NT 7, ( result, KW_string1left, KW_string1right), 
rest671)
end
|  ( 12, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.Type Type, _
, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.TypeAtom (Type)
 in ( LrTable.NT 7, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.InfixExp InfixExp, InfixExp1left, 
InfixExp1right)) :: rest671)) => let val  result = MlyValue.Exp (
InfixExp)
 in ( LrTable.NT 8, ( result, InfixExp1left, InfixExp1right), rest671)

end
|  ( 14, ( ( _, ( MlyValue.Exp Exp, _, Exp1right)) :: ( _, ( 
MlyValue.ID ID, _, _)) :: ( _, ( _, KW_raise1left, _)) :: rest671)) =>
 let val  result = MlyValue.Exp (RAISE_E(ID, Exp))
 in ( LrTable.NT 8, ( result, KW_raise1left, Exp1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.Exp Exp, _, Exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, _, _)) :: ( _, ( MlyValue.ID ID1, _, _)) :: _ :: ( _,
 ( MlyValue.InfixExp InfixExp, InfixExp1left, _)) :: rest671)) => let
 val  result = MlyValue.Exp (HANDLE_E(InfixExp, ID1, ID2, Exp))
 in ( LrTable.NT 8, ( result, InfixExp1left, Exp1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.Exp Exp, _, Exp1right)) :: ( _, ( 
MlyValue.INT INT, _, _)) :: ( _, ( _, HASH1left, _)) :: rest671)) =>
 let val  result = MlyValue.Exp (PROJ_E (INT, Exp))
 in ( LrTable.NT 8, ( result, HASH1left, Exp1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.Exp Exp, _, Exp1right)) :: _ :: ( _, ( 
MlyValue.Type Type, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: (
 _, ( _, KW_fn1left, _)) :: rest671)) => let val  result = 
MlyValue.Exp (FN_E (ID, Type, Exp))
 in ( LrTable.NT 8, ( result, KW_fn1left, Exp1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.Exp Exp, _, Exp1right)) :: _ :: _ :: ( _, (
 MlyValue.Type Type, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: _
 :: ( _, ( _, KW_fn1left, _)) :: rest671)) => let val  result = 
MlyValue.Exp (FN_E (ID, Type, Exp))
 in ( LrTable.NT 8, ( result, KW_fn1left, Exp1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.Exp Exp3, _, Exp3right)) :: _ :: ( _, ( 
MlyValue.Exp Exp2, _, _)) :: _ :: ( _, ( MlyValue.Exp Exp1, _, _)) :: 
( _, ( _, KW_if1left, _)) :: rest671)) => let val  result = 
MlyValue.Exp (IF_E (Exp1, Exp2, Exp3))
 in ( LrTable.NT 8, ( result, KW_if1left, Exp3right), rest671)
end
|  ( 20, ( ( _, ( _, _, KW_end1right)) :: ( _, ( MlyValue.Exp Exp, _,
 _)) :: _ :: ( _, ( MlyValue.Decl Decl, _, _)) :: ( _, ( _, 
KW_let1left, _)) :: rest671)) => let val  result = MlyValue.Exp (
LET_E (Decl, Exp))
 in ( LrTable.NT 8, ( result, KW_let1left, KW_end1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.Binops Binops, Binops1left, Binops1right))
 :: rest671)) => let val  result = MlyValue.InfixExp (Binops)
 in ( LrTable.NT 9, ( result, Binops1left, Binops1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.AtExpSeq AtExpSeq, AtExpSeq1left, 
AtExpSeq1right)) :: rest671)) => let val  result = MlyValue.InfixExp (
AtExpSeq)
 in ( LrTable.NT 9, ( result, AtExpSeq1left, AtExpSeq1right), rest671)

end
|  ( 23, ( ( _, ( MlyValue.InfixExp InfixExp2, _, InfixExp2right)) ::
 _ :: ( _, ( MlyValue.InfixExp InfixExp1, InfixExp1left, _)) :: 
rest671)) => let val  result = MlyValue.Binops (
BINOP_E(InfixExp1, PLUS_B,  InfixExp2))
 in ( LrTable.NT 10, ( result, InfixExp1left, InfixExp2right), rest671
)
end
|  ( 24, ( ( _, ( MlyValue.InfixExp InfixExp2, _, InfixExp2right)) ::
 _ :: ( _, ( MlyValue.InfixExp InfixExp1, InfixExp1left, _)) :: 
rest671)) => let val  result = MlyValue.Binops (
BINOP_E(InfixExp1, MINUS_B, InfixExp2))
 in ( LrTable.NT 10, ( result, InfixExp1left, InfixExp2right), rest671
)
end
|  ( 25, ( ( _, ( MlyValue.InfixExp InfixExp2, _, InfixExp2right)) ::
 _ :: ( _, ( MlyValue.InfixExp InfixExp1, InfixExp1left, _)) :: 
rest671)) => let val  result = MlyValue.Binops (
BINOP_E(InfixExp1, TIMES_B, InfixExp2))
 in ( LrTable.NT 10, ( result, InfixExp1left, InfixExp2right), rest671
)
end
|  ( 26, ( ( _, ( MlyValue.InfixExp InfixExp2, _, InfixExp2right)) ::
 _ :: ( _, ( MlyValue.InfixExp InfixExp1, InfixExp1left, _)) :: 
rest671)) => let val  result = MlyValue.Binops (
BINOP_E(InfixExp1, MOD_B,   InfixExp2))
 in ( LrTable.NT 10, ( result, InfixExp1left, InfixExp2right), rest671
)
end
|  ( 27, ( ( _, ( MlyValue.InfixExp InfixExp2, _, InfixExp2right)) ::
 _ :: ( _, ( MlyValue.InfixExp InfixExp1, InfixExp1left, _)) :: 
rest671)) => let val  result = MlyValue.Binops (
BINOP_E(InfixExp1, DIV_B,   InfixExp2))
 in ( LrTable.NT 10, ( result, InfixExp1left, InfixExp2right), rest671
)
end
|  ( 28, ( ( _, ( MlyValue.InfixExp InfixExp2, _, InfixExp2right)) ::
 _ :: ( _, ( MlyValue.InfixExp InfixExp1, InfixExp1left, _)) :: 
rest671)) => let val  result = MlyValue.Binops (
BINOP_E(InfixExp1, NEQ_B,   InfixExp2))
 in ( LrTable.NT 10, ( result, InfixExp1left, InfixExp2right), rest671
)
end
|  ( 29, ( ( _, ( MlyValue.InfixExp InfixExp2, _, InfixExp2right)) ::
 _ :: ( _, ( MlyValue.InfixExp InfixExp1, InfixExp1left, _)) :: 
rest671)) => let val  result = MlyValue.Binops (
BINOP_E(InfixExp1, EQ_B,    InfixExp2))
 in ( LrTable.NT 10, ( result, InfixExp1left, InfixExp2right), rest671
)
end
|  ( 30, ( ( _, ( MlyValue.InfixExp InfixExp2, _, InfixExp2right)) ::
 _ :: ( _, ( MlyValue.InfixExp InfixExp1, InfixExp1left, _)) :: 
rest671)) => let val  result = MlyValue.Binops (
BINOP_E(InfixExp1, LT_B,    InfixExp2))
 in ( LrTable.NT 10, ( result, InfixExp1left, InfixExp2right), rest671
)
end
|  ( 31, ( ( _, ( MlyValue.InfixExp InfixExp2, _, InfixExp2right)) ::
 _ :: ( _, ( MlyValue.InfixExp InfixExp1, InfixExp1left, _)) :: 
rest671)) => let val  result = MlyValue.Binops (
BINOP_E(InfixExp1, LEQ_B,   InfixExp2))
 in ( LrTable.NT 10, ( result, InfixExp1left, InfixExp2right), rest671
)
end
|  ( 32, ( ( _, ( MlyValue.InfixExp InfixExp2, _, InfixExp2right)) ::
 _ :: ( _, ( MlyValue.InfixExp InfixExp1, InfixExp1left, _)) :: 
rest671)) => let val  result = MlyValue.Binops (
BINOP_E(InfixExp1, GT_B,    InfixExp2))
 in ( LrTable.NT 10, ( result, InfixExp1left, InfixExp2right), rest671
)
end
|  ( 33, ( ( _, ( MlyValue.InfixExp InfixExp2, _, InfixExp2right)) ::
 _ :: ( _, ( MlyValue.InfixExp InfixExp1, InfixExp1left, _)) :: 
rest671)) => let val  result = MlyValue.Binops (
BINOP_E(InfixExp1, GEQ_B,   InfixExp2))
 in ( LrTable.NT 10, ( result, InfixExp1left, InfixExp2right), rest671
)
end
|  ( 34, ( ( _, ( MlyValue.InfixExp InfixExp2, _, InfixExp2right)) ::
 _ :: ( _, ( MlyValue.InfixExp InfixExp1, InfixExp1left, _)) :: 
rest671)) => let val  result = MlyValue.Binops (
ANDALSO_E(InfixExp1, InfixExp2))
 in ( LrTable.NT 10, ( result, InfixExp1left, InfixExp2right), rest671
)
end
|  ( 35, ( ( _, ( MlyValue.InfixExp InfixExp2, _, InfixExp2right)) ::
 _ :: ( _, ( MlyValue.InfixExp InfixExp1, InfixExp1left, _)) :: 
rest671)) => let val  result = MlyValue.Binops (
ORELSE_E(InfixExp1, InfixExp2))
 in ( LrTable.NT 10, ( result, InfixExp1left, InfixExp2right), rest671
)
end
|  ( 36, ( ( _, ( MlyValue.InfixExp InfixExp2, _, InfixExp2right)) ::
 _ :: ( _, ( MlyValue.InfixExp InfixExp1, InfixExp1left, _)) :: 
rest671)) => let val  result = MlyValue.Binops (
BINOP_E(InfixExp1, CARET_B, InfixExp2))
 in ( LrTable.NT 10, ( result, InfixExp1left, InfixExp2right), rest671
)
end
|  ( 37, ( ( _, ( MlyValue.AtExp AtExp, _, AtExp1right)) :: ( _, ( 
MlyValue.AtExpSeq AtExpSeq, AtExpSeq1left, _)) :: rest671)) => let
 val  result = MlyValue.AtExpSeq (APP_E(AtExpSeq, AtExp))
 in ( LrTable.NT 12, ( result, AtExpSeq1left, AtExp1right), rest671)

end
|  ( 38, ( ( _, ( MlyValue.AtExp AtExp, AtExp1left, AtExp1right)) :: 
rest671)) => let val  result = MlyValue.AtExpSeq (AtExp)
 in ( LrTable.NT 12, ( result, AtExp1left, AtExp1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.AtExp (VAR_E    ID)
 in ( LrTable.NT 11, ( result, ID1left, ID1right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.INT INT, INT1left, INT1right)) :: rest671))
 => let val  result = MlyValue.AtExp (INT_E    INT)
 in ( LrTable.NT 11, ( result, INT1left, INT1right), rest671)
end
|  ( 41, ( ( _, ( _, KW_true1left, KW_true1right)) :: rest671)) => let
 val  result = MlyValue.AtExp (TRUE_E)
 in ( LrTable.NT 11, ( result, KW_true1left, KW_true1right), rest671)

end
|  ( 42, ( ( _, ( _, KW_false1left, KW_false1right)) :: rest671)) =>
 let val  result = MlyValue.AtExp (FALSE_E)
 in ( LrTable.NT 11, ( result, KW_false1left, KW_false1right), rest671
)
end
|  ( 43, ( ( _, ( MlyValue.STRING STRING, STRING1left, STRING1right))
 :: rest671)) => let val  result = MlyValue.AtExp (STRING_E STRING)
 in ( LrTable.NT 11, ( result, STRING1left, STRING1right), rest671)

end
|  ( 44, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ExpSeq 
ExpSeq, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  
result = MlyValue.AtExp (
case ExpSeq of
                                             [] => raise Fail "This shouldn't happen"
                                           | [x] => x
                                           | t => TUPLE_E ExpSeq
)
 in ( LrTable.NT 11, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 45, ( ( _, ( MlyValue.AtExp AtExp, _, AtExp1right)) :: ( _, ( _, 
KW_not1left, _)) :: rest671)) => let val  result = MlyValue.AtExp (
UNOP_E   (NOT_U,   AtExp))
 in ( LrTable.NT 11, ( result, KW_not1left, AtExp1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.AtExp AtExp, _, AtExp1right)) :: ( _, ( _, 
NEG1left, _)) :: rest671)) => let val  result = MlyValue.AtExp (
UNOP_E   (NEG_U,   AtExp))
 in ( LrTable.NT 11, ( result, NEG1left, AtExp1right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.ExpSeq ExpSeq, _, ExpSeq1right)) :: _ :: (
 _, ( MlyValue.Exp Exp, Exp1left, _)) :: rest671)) => let val  result
 = MlyValue.ExpSeq (Exp::ExpSeq)
 in ( LrTable.NT 13, ( result, Exp1left, ExpSeq1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.Exp Exp, Exp1left, Exp1right)) :: rest671))
 => let val  result = MlyValue.ExpSeq ([Exp])
 in ( LrTable.NT 13, ( result, Exp1left, Exp1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : MiniML_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun KW_andalso (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun KW_bool (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun KW_div (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun KW_else (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun KW_end (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun KW_false (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun KW_fn (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun KW_fun (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun KW_handle (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun KW_if (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun KW_in (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun KW_int (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun KW_let (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun KW_mod (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun KW_not (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun KW_of (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun KW_orelse (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun KW_raise (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun KW_string (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun KW_then (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun KW_true (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun KW_val (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun KW_and (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun CARET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun DARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun EQSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATEREQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun HASH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun LESS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun TTIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.ID i,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.INT i,p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.STRING i,p1,p2))
end
end
