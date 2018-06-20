structure Helpers =
  struct

    structure DPROC = DprocParseFn(DprocLexer)

    fun isEOF(DprocTokens.EOF) = true
      | isEOF(_) = false

    fun string_to_tokens(inputString: string): DprocTokens.token list =
      let
        val initial_strm = DprocLexer.streamifyInstream (TextIO.openString inputString)
        val lexer = DprocLexer.lex (AntlrStreamPos.mkSourcemap())
        fun dowork(strm) =
          let
            val lex_result = lexer strm
            val next_token = #1 lex_result
          in
            if isEOF(next_token)
            then []
            else next_token :: dowork(#3 lex_result)
          end
      in
        dowork(initial_strm)
      end

    fun string_to_ast(inputString: string): Ast.Exp =
      let
        val strm = DprocLexer.streamifyInstream (TextIO.openString inputString)
        val lexer = DprocLexer.lex (AntlrStreamPos.mkSourcemap())
        val (r, strm', errs) = DPROC.parse lexer strm
      in
        (case r
          of SOME(exp) => exp
          |  _ => raise Exceptions.ParseError ("parse error on " ^ inputString))
      end

    fun string_to_infer(inputString: string): Ast.Type option =
      TypeInference.infer(SymbolAnalysis.resolve(string_to_ast(inputString)))

    fun infer_and_print(inputString: string): unit = (
      case string_to_infer(inputString) of
        SOME t => (
          print (inputString ^ ": ");
          print (Ast.toString_type_helper (Ast.niceTvarPrinter ()) t);
          print "\n";
          ()
          )
      | NONE => (
        print "type error\n";
        ()
        )
      )

  end
