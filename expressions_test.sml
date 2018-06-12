let
  fun s(name) = ref (Ast.create_symbol name)

  fun makeTest(input: string, expected: Ast.Value) = (input,
      fn() => Assert.assertTrue(let
        val actuals = Helpers.string_to_val(input)
      in
        Ast.eq_v(actuals, expected)
      end));
in
  ConsoleTestRunner.runTestCase([
    makeTest("420",
      Ast.eval(Ast.IntConstant(420))
    )
    ,
    makeTest("2+3",
      Ast.eval(Ast.InfixApp(Ast.IntConstant(2),"+",Ast.IntConstant(3))
    )),
    makeTest("2-3", Ast.eval(Ast.IntConstant(~1))),
    makeTest("2*3", Ast.eval(Ast.IntConstant(6))),
    makeTest("16/4", Ast.eval(Ast.IntConstant(4)))

    ])

end