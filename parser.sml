
structure Parser : sig
  val parse : TextIO.instream -> AbstractSyntaxTree.exp
  val parseString : string    -> AbstractSyntaxTree.exp
end = struct

  (*
    There is a lot of stuff here that is boilerplate and hard to
    explain. You don't need to worry about this code
  *)

  structure MiniMLLrVals = MiniMLLrValsFun(structure Token = LrParser.Token)

  structure MiniMLLex    = MiniMLLexFun(structure Tokens = MiniMLLrVals.Tokens)

  structure MiniMLParser = Join (structure LrParser   = LrParser
                                 structure ParserData = MiniMLLrVals.ParserData
                                 structure Lex        = MiniMLLex)


  fun invoke (lexstream) =
  let
    val print_error = fn (s, i: int, _) =>
      raise Errors.ParseError (concat [s," [line ",(Int.toString i),"]"])
  in
    MiniMLParser.parse(0, lexstream, print_error, ())
  end


  fun parse (instream:TextIO.instream):AbstractSyntaxTree.exp =
  let
    val lexer = MiniMLParser.makeLexer (fn _ => TextIO.inputLine instream)
    val dummyEOF = MiniMLLrVals.Tokens.EOF(0, 0)
    fun loop lexer =
    let
      val (result, lexer)   = invoke lexer
      val (nextToken,lexer) = MiniMLParser.Stream.get lexer
    in if MiniMLParser.sameToken(nextToken,dummyEOF) then result
       else loop lexer
    end
  in
    MiniMLLex.UserDeclarations.pos := 1;
    loop lexer
  end


  fun parseString (str:string):AbstractSyntaxTree.exp = let
    val s = TextIO.openString (str)
    val r = parse (s)
  in
    TextIO.closeIn (s);
    r
  end

end
