C:/Program Files (x86)/SMLNJ/bin/.run/run.x86-win32.exe: Fatal error -- in-core heap images not implemented


(* Need to load the library -- change this path to point to your* version of SML *)
CM.make' "/sml/lib/smlnj-lib.cm"; 
val print_expressions = ref true
val print_substitutions = ref true
val skip_values = ref true

(* Some utility functions *)

(* zip [a,b,c] [d,e,f] -> [(a,d), (b,e), (c,f) ] *)
exception Zip
fun zip (xs:'a list) (ys:'b list) : ('a * 'b) list = 
  (case (xs, ys) of
     ([],[]) => []
   | (x::xrest, y::yrest) => (x,y)::(zip xrest yrest)
   | _ => raise Zip)

(* pause for user input *)
fun pause() = (TextIO.inputLine TextIO.stdIn; ())

(* sort used for record expressions and patterns *)
fun sort r = ListMergeSort.sort (fn ((x:string,_),(y:string,_)) => x < y) r

(* error reporting *)
exception Error of string
fun error(s:string):'a = raise (Error s)

(*************************************************************************)

(* we represent identifiers (variables) as strings *)
type id = string

(* constants *)
datatype const = Int of int | Real of real | String of string | Char of char

(* binary operations *)
datatype binop = Plus | Times | Minus | Equal | Concat

(* expressions *)
datatype exp = 
  Const of const              (* 3, 2.178, "foo", #"c", etc. *)
| Id of id                    (* variables *)
| Fn of (id*exp)              (* anonymous functions: fn id => exp *)
| App of (exp*exp)            (* function application:  exp1(exp2) *)
| Binop of (exp*binop*exp)    (* binary operations:  eg., exp1 + exp2 *)
| Tuple of (exp list)         (* tuples:  (3,"foo",true) *)
| Ith of (int*exp)            (* tuple projection:  #i exp *)
| Record of ((id*exp)list)    (* records: {name="Greg", age=100} *)
| Field of (id*exp)           (* record projection:  #name exp *)
| DataCon of (id*(exp option))(* data constructors:  true, false, nil, 
                              * NONE, SOME(exp), NODE{left=e1,elt=x,right=e2}*)
| Case of (exp*(pat*exp)list)(* case:  case exp of pat1=>e1 | ... | patn=>en *)
| Let of (decl*exp)          (* let:  let decl in exp *)
| Fun of (id*id*exp)         (* recursive functions:  fun f(x)=exp *)
| List of (const list)
(* declarations *)
and decl = 
  Val_d of (pat*exp)         (* val pat = exp *)
| Fun_d of (id*id*exp)       (* fun f(x) = exp *)

(* patterns *)
and pat = 
  Wild_p                       (* wildcard:  _ *)
| Id_p of id                   (* variable:  x *)
| Const_p of int               (* constant:  3 *)
| DataCon_p of (id*pat option) (* data constructor:  true, false, nil, 
                             * NONE, SOME(pat), NODE{left=p1,elt=p2,right=p3}*)
| Tuple_p of pat list          (* tuple patterns:  (pat1,...,patn) *)
| Record_p of (id*pat) list (* record patterns: {field1=pat1,...,fieldn=patn}*)

(* returns true iff the expression is a value *)
fun is_value (e:exp):bool = 
  case e of
    Const _ => true
  | Fn _ => true
  | Tuple (es) => List.all is_value es
  | Record(ides) => List.all (fn (_,e) => is_value e) ides
  | DataCon(_,NONE) => true
  | DataCon(_,SOME(e)) => is_value e
  | _ => false

(***************************************)
(* Functions for printing out the code *)
(***************************************)

(* insert a separator between each element of a string list and 
 * concatenate the whole thing.  *)
fun sep (s:string) (lis:string list) : string = 
  let fun f lis = 
    case lis of
      [] => []
    | [x] => [x]
    | hd::tl => hd::s::(f tl)
  in 
    List.foldr (op ^) "" (f lis)
  end

(* convert a constant to a string *)
fun const2s (c:const):string = 
  case c of
    Int(i) => Int.toString i
  | Real(r) => Real.toString r
  | String(s) => "\"" ^ s ^ "\""
  | Char(c) => "#\"" ^ (Char.toString c) ^ "\""

(* convert a binary operation to a string *)
fun binop2s (b:binop):string = 
  case b of
    Plus => "+"
  | Minus => "-"
  | Times => "*"
  | Concat => "^"
  | Equal => "="


(* maximum precedence *)
val max_prec = 999;

(* precedence for binary operations *)
fun binop_prec(b) = 
  case b of
    Plus => 7
  | Times => 8
  | Minus => 7
  | Concat => 6
  | Equal => 5

(* precedence for expressions *)
fun prec(e:exp):int = 
  case e of
    Const(c) => max_prec
  | Id(x) => max_prec
  | Fn(id,e) => 1
  | App(e1,e2) => 2
  | Binop(e1,b,e2) => binop_prec(b)
  | Tuple(es) => max_prec
  | Ith(i,e) => 2
  | Record(ides) => max_prec
  | Field(x,e) => 2 
  | DataCon(x,NONE) => 2
  | DataCon(x,SOME(e)) => 2
  | Case(e,cases) => 1
  | Let(d,e) => max_prec
  | Fun(f,x,e) => 1

(* convert an expression to a string *)
fun exp2s (p:int) (e:exp):string = 
  let val p' = prec(e)
    val e2s = exp2s p'
    val s = 
      case e of
        Const(c) => const2s c
      | Id(x) => x
      | Fn(id,e) => "fn "^id^" => "^(e2s e)
      | App(e1,e2) => (e2s e1)^" "^(e2s e2)
      | Binop(e1,b,e2) => (e2s e1)^(binop2s b)^(e2s e2)
      | Tuple(es) => "("^(sep "," (List.map (exp2s 0) es))^")"
      | Ith(i,e) => "#"^(Int.toString i)^" "^(e2s e)
      | Record(ides) => 
          ("{"^(sep "," 
                (List.map (fn (x,e) => x^"="^(exp2s 0 e)) ides))^
           "}")
      | Field(x,e) => "#"^x^" "^(e2s e)
      | DataCon(x,NONE) => x
      | DataCon(x,SOME(e)) => 
          (case e of
             Tuple _ => (x ^ (e2s e))
           | _ => (x ^ " " ^ (e2s e)))
      | Case(e,cases) => 
             "case "^(exp2s 0 e)^" of "^
             (sep " | " (List.map case2s cases))
      | Let(d,e) => 
             "let "^(decl2s d)^" in "^(exp2s 0 e)^" end"
      | Fun(f,x,e) => 
             "fun "^f^"("^x^") = "^(exp2s 0 e)
  in 
    if (p' > p) then s else "("^s^")"
  end

(* convert a declaration to a string *)
and decl2s (d:decl):string = 
  case d of
    Val_d(p,e) => "val "^(pat2s p)^" = "^(exp2s 0 e)
  | Fun_d(f,x,e) => "fun "^f^"("^x^") = "^(exp2s 0 e)

(* convert a pattern to a string *)
and pat2s (p:pat):string = 
  case p of
    Wild_p => "_"
  | Id_p(x) => x
  | Const_p(i) => Int.toString i
  | DataCon_p(id,NONE) => id
  | DataCon_p(id,SOME(p)) => id^" "^(pat2s p)
  | Tuple_p(ps) => "("^(sep "," (List.map pat2s ps))^")"
  | Record_p(idps) => 
      "{"^(sep "," 
           (List.map (fn (x,p) => x^"="^(pat2s p)) idps))^"}"

(* convert a case expression to a string *)
and case2s (p:pat,e:exp):string = 
  (pat2s p)^" => "^(exp2s 0 e)

(* print out an expression *)
fun print_exp(e:exp):unit = 
  print(exp2s 0 e); print "\n";

(* example data constructors true and false *)
val True = DataCon("true",NONE);
val False = DataCon("false",NONE);

(* apply a binary operation to two constants *)
fun apply_binop(b:binop,c1:const,c2:const):exp =
  case (b,c1,c2) of
    (Plus,Int i,Int j) => Const(Int(i+j))
  | (Plus,Real i,Real j) => Const(Real(i+j))
  | (Times,Int i,Int j) => Const(Int(i*j))
  | (Times,Real i,Real j) => Const(Real(i*j))
  | (Minus,Int i,Int j) => Const(Int(i-j))
  | (Minus,Real i,Real j) => Const(Real(i-j))
  | (Concat,String s1,String s2) => Const(String(s2 ^ s2))
  | (Equal,Int i,Int j) => if i = j then True else False
  | (Equal,String i,String j) => if i = j then True else False
  | (Equal,Char i,Char j) => if i = j then True else False
  | (_,_,_) => error("bad binop application")

(* raised when a pattern match fails *)
exception MatchFail

(* a substitution is a list of variables and their associated expressions *)
type substitution = (id * exp) list

(* match the value v against the pattern p to get a substitution *)
fun match(v:exp,p:pat):substitution = 
  case (v,p) of
    (_, Wild_p) => []
  | (_, Id_p(x)) => [(x,v)]
  | (Const(Int i), Const_p j) => if (i = j) then [] else raise MatchFail
  | (DataCon(id,NONE), DataCon_p(id',NONE)) =>
      if (id = id') then [] else raise MatchFail
  | (DataCon(id,SOME(v')), DataCon_p(id',SOME(p'))) =>
        if (id = id') then match(v',p') else raise MatchFail
  | (Tuple(vs), Tuple_p(ps)) =>
          List.foldr (op @) [] (map match (zip vs ps))
  | (Record(idvs), Record_p(idps)) => 
          let val idps = sort idps
          in 
	    List.foldr (op @) [] (map (fn ((_,v),(_,p)) => match(v,p))
				  (zip idvs idps))
          end
  | (_, _) => raise MatchFail


(* return the list of variables that occur in a pattern *)
fun pat_vars(p:pat):id list = 
  case p of
    Wild_p => []
  | Id_p(x) => [x]
  | Const_p(_) => []
  | DataCon_p(_,NONE) => []
  | DataCon_p(_,SOME(p')) => pat_vars p'
  | Tuple_p(ps) => List.foldr (op @) [] (map pat_vars ps)
  | Record_p(idps) => 
      List.foldr (op @) [] (map (fn (_,p) => pat_vars p) idps)

(* substitute the value v for the variable x within the expression e *)
fun subst(s as (x:id,v:exp),e:exp):exp =
  case e of
    Const _ => e
  | Id(y) => if (x = y) then v else e
  | Fn(y,e) => if (x = y) then e else Fn(y,subst(s,e))
  | App(e1,e2) => App(subst(s,e1),subst(s,e2))
  | Binop(e1,b,e2) => Binop(subst(s,e1),b,subst(s,e2))
  | Tuple(es) => Tuple(List.map (fn e => subst(s,e)) es)
  | Ith(i,e) => Ith(i,subst(s,e))
  | Record(ides) => Record(List.map (fn (lab,e) => (lab,subst(s,e))) ides)
  | Field(id,e) => Field(id,subst(s,e))
  | DataCon(id,NONE) => e
  | DataCon(id,SOME(e)) => DataCon(id,SOME(subst(s,e)))
  | Case(e,cases) => Case(subst(s,e),List.map (subst_case s) cases)
  | Let(Val_d(p,e1),e2) => 
      let val pvs = pat_vars(p)
        val d = Val_d(p,subst(s,e1))
      in
        if List.exists (fn y => y = x) pvs then
          Let(d,e2)
        else
          Let(d,subst(s,e2))
      end
  | Let(Fun_d(f,y,e1),e2) => 
      if (x = f) then e
      else if (x = y) then Let(Fun_d(f,y,e1),subst(s,e2))
	   else Let(Fun_d(f,y,subst(s,e1)),subst(s,e2))
  | Fun(f,y,e') => 
             if (x = f) orelse (x = y) then e
             else Fun(f,y,subst(s,e'))
(* substitute v for x within a case *)
and subst_case (s as (x:id,v:exp)) (p:pat,e:exp) : pat*exp = 
  if List.exists (fn y => y = x) (pat_vars p) then (p,e)
  else (p,subst(s,e))


(* substitute a substitution (list of variables and associated values) within
 * an expression *)
fun substitute(S:substitution,e:exp):exp = 
  (* print out the substitution *)
  (if (!print_substitutions) then
     (print "\nsubstituting [";
      print (sep "," 
             (List.map (fn (x,e) => "("^x^","^(let val s = exp2s 0 e
                                               in 
                                                 if (size s) > 15 then
                                                   (String.substring(s,0,14)) ^ "..." else s
                                               end)^")") S));
      print "]\n within ";
      print_exp e;
      print "\n";
      pause())
   else ();
   List.foldr subst e S)

(* evaluate the expression e to get a value *)
fun eval'(e:exp):exp =
  case e of
    Const(c) => Const(c)
  | Fn(x,e') => Fn(x,e')
  | Id(x) => error("Id: unbound variable "^x)
  | App(e1,e2) =>
      let val v1 = eval e1
        val v2 = eval e2
      in
        case v1 of
          Fn(x,e') => eval(substitute([(x,v2)],e'))
        | _ => error("App: not a function")
      end
  | Binop(e1,b,e2) =>
      (case (eval e1,eval e2) of
         (Const c1, Const c2) => apply_binop(b,c1,c2)
       | _ => error("Binop: arguments not constants"))
  | Tuple es => Tuple (map eval es)
  | Ith(i,e) => 
         (case (eval e) of
            Tuple(vs) => List.nth(vs,i)
          | _ => error("Ith: not a tuple"))
  | Record lab_es => Record (sort (map (fn (lab,e) => (lab,eval e)) lab_es))
  | Field(id,e) =>
            (case (eval e) of
               Record(lab_vs) => 
                 (case List.find (fn (x,v) => x = id) lab_vs of
                    SOME(_,v) => v
                  | NONE => error("Field: record missing field "^id))
             | _ => error("Field: not a record"))
  | DataCon(id,NONE) => DataCon(id,NONE)
  | DataCon(id,SOME(e)) => DataCon(id,SOME(eval e))
  | Case(e,cases) => find_match(eval e,cases)
  | Let(Val_d(p,e1),e2) =>
               let val v = eval e1
                 val S = match(v,p)
               in
                 eval(substitute(S,e2))
               end
  | Let(Fun_d(f,x,e1),e2) => eval(substitute([(f,Fun(f,x,e1))],e2))
  | Fun(f,x,e) => eval (Fn(x,substitute([(f,Fun(f,x,e))],e)))

(* print out the expression and evaluate it *)
and eval(e:exp):exp = 
  (* don't bother to evaluate expressions that are already values *)
  if (!skip_values andalso is_value e) then e else
    let val _ = 
      if (!print_expressions) then
        (print("\nThe current expression being evaluated is: \n");
         print_exp e; print "\n"; pause(); ())
      else ()
	val r = eval' e
    in 
      (if (!print_expressions) then
         (print "the result of eval(";
          print_exp e;
          print ") is: \n";
          print_exp r;
          print "\n\n";
          pause())
       else ()); r
    end

(* given a list of cases from a case expression, try to match v 
 * against the associated pattern.  If this succeeds, then apply
 * the resulting substitution to the right-hand-side of the case
 * and evaluate it.  Otherwise, go on to the next case. *)
and find_match(v:exp,cases:(pat * exp) list):exp = 
  case cases of
    [] => error("total match failure")
  | ((p,e)::rest) =>
      (eval(substitute(match(v,p),e)) handle MatchFail => find_match(v,rest))

(**************************************************************************)
(* Some examples to evaluate:  swap_example, fact_example, append_example *)
(**************************************************************************)
val one = Const(Int(1))
val two = Const(Int(2))
val three = Const(Int(3))
val four = Const(Int(4))
fun plus(e1,e2) = Binop(e1,Plus,e2)

(*
let val swap = 
    fn (p) => 
      let val (x,y) = p
      in
	  (y,x) 
      end
in
   swap(3+1,4+2)
end
*)
val swap = Fn("p",Let(Val_d(Tuple_p[Id_p "x", Id_p "y"], Id "p"),
		      Tuple[Id "y", Id "x"]))
val swap_example = 
    Let(Val_d(Id_p "swap",swap), App(Id "swap", Tuple[plus(three,one),
						      plus(four,two)]))

(* 
let fun fact(n) = 
        if (n = 1) then 1 else n * fact(n-1)
in
    fact 4
end
*)

fun ife e1 e2 e3 = 
    Case(e1,[(DataCon_p("true",NONE),e2),(DataCon_p("false",NONE),e3)]);
val fact = 
    ("fact","n",
     ife (Binop(Id "n",Equal,one)) one
     (Binop(Id "n",Times,App(Id "fact",Binop(Id "n",Minus,one)))))
val fact_example = Let(Fun_d(fact),App(Id "fact",Const(Int 4)))

(*
datatype intlist = Nil | Cons of (int * intlist)

let fun append(p) = 
        case p of
	  (Nil, y) => y
        | (Cons(hd,tl), y) => Cons(hd,append(tl,y))
in
    append (Cons(1,Nil), Cons(2,Cons(3,Nil)))
end
*)
fun cons(e1:exp,e2:exp):exp = DataCon("Cons",SOME(Tuple[e1,e2]))
val nil_e:exp = DataCon("Nil",NONE)
val nil_p:pat = DataCon_p("Nil",NONE)
fun cons_p(p1:pat,p2:pat):pat = DataCon_p("Cons",SOME(Tuple_p[p1,p2]))
fun pair_p(p1:pat,p2:pat):pat = Tuple_p[p1,p2]
val append = 
    ("append","p",
     Case(Id("p"),[(pair_p(nil_p,Id_p("y")), Id("y")),
		   (pair_p(cons_p(Id_p("hd"),Id_p("tl")),Id_p("y")),
		    cons(Id("hd"),App(Id("append"),Tuple[Id("tl"),Id("y")])))
		   ]))
val append_example = Let(Fun_d(append),App(Id("append"),
					   Tuple[cons(one,nil_e),
						 cons(two,cons(three,nil_e))]))