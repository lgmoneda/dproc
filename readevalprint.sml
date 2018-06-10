(* REP loop: a simple loop which reads input, evaluates it, and prints the result. *)
structure ReadEvalPrint = struct

  open AbstractSyntaxTree

  (* Define global environment so that it contains recursive functions. *)
  fun makeGlobalEnv(): environment = 
    let
      val body = IF_E(BINOP_E (VAR_E "n",EQ_B,INT_E 0),
                      INT_E 1,
                      BINOP_E (VAR_E "n",
                               TIMES_B,
                               APP_E (VAR_E "fact",
                                      BINOP_E (VAR_E "n",
                                               MINUS_B,
                                               INT_E 1))))
      val hole = ref []
      val env = env_add("fact", FN_V("n", body, hole), empty_env)
      val () = hole := env
    in
      env
    end

  (* Takes a string, parses it, type checks it, then evaluates it.
   * Useful for testing your code directly. *)
  fun evaluateString(s:string):AbstractSyntaxTree.value = let
    val ast = Parser.parseString s
    val _ = TypeChecker.get_type(ast, TypeChecker.empty_typenv)
  in
    (* was: Evaluator.eval(ast, AbstractSyntaxTree.empty_env) *)
    Evaluator.eval(ast, makeGlobalEnv())
  end

  (* never exits. *)
  fun loop():unit = let
    val () = print("MiniML> ")
    val input = TextIO.inputLine TextIO.stdIn
  in
    if (String.size input) < 1 then ()   (* exit on EOF (use CTRL + Z) *)
    else if (String.size input) < 2
    then (* dont parse it if line was empty *)
         loop() 
    else 
        let 
          val outputValue = evaluateString input
        in
          (print ((PrettyPrinter.value2str outputValue) ^ "\n");
           loop())
        end
  end handle TypeChecker.StaticTypeError s => (print ("Type error: " ^ s ^"\n");
                                               loop())
           | Errors.LexicalError s => (print (s ^ "\n"); loop())
           | Errors.ParseError s => (print (s ^ "\n"); loop())

  (* The "main" function.  Call this to start the interactive interpreter. *)
  fun main():unit = 
    (print ("== CS312-2004FA MiniML interpreter ==\n\n" ^
           "Please input each expression on a single line.\n\n");
     loop())

end
