let
  fun makeTest(input: string, expected: DprocTokens.token list) = (input,
      fn() => Assert.assertTrue(let
        val actuals = Helpers.string_to_tokens(input)
      in
        actuals = expected
      end));
in
  ConsoleTestRunner.runTestCase([
    (* constants *)
    makeTest("0", [DprocTokens.CON_int(0)]),
    makeTest("99", [DprocTokens.CON_int(99)]),

    makeTest("\"\"", [DprocTokens.CON_string("")]),
    makeTest("\"a\"", [DprocTokens.CON_string("a")]),
    makeTest("\"hello world\"", [DprocTokens.CON_string("hello world")]),
    makeTest("\"\\t\"", [DprocTokens.CON_string("\t")])

    (* punctuation *)
    (* makeTest("()", [DprocTokens.LP, DprocTokens.RP]), *)
    (* makeTest("[]", [DprocTokens.LB, DprocTokens.RB]), *)
    (* makeTest(",;:", [DprocTokens.COMMA, DprocTokens.SEMI, DprocTokens.COLON]), *)
    (* makeTest("=>", [DprocTokens.ARROW]), *)
    (* makeTest("->", [DprocTokens.TARROW]), *)

    (* operators *)
    (* makeTest("=", [DprocTokens.OP("=")]), *)
    (* makeTest("*", [DprocTokens.OP("*")]), *)
    (* makeTest(">", [DprocTokens.OP(">")]), *)
    (* makeTest("<", [DprocTokens.OP("<")]), *)
    (* makeTest("+", [DprocTokens.OP("+")]), *)
    (* makeTest("-", [DprocTokens.OP("-")]), *)
    (* makeTest("^", [DprocTokens.OP("^")]), *)
    (* makeTest("==>", [DprocTokens.OP("==>")]), *)
    (* makeTest("><+-", [DprocTokens.OP("><+-")]), *)

    (* keywords *)
    (* makeTest("if", [DprocTokens.KW_if]), *)
    (* makeTest("then", [DprocTokens.KW_then]), *)
    (* makeTest("else", [DprocTokens.KW_else]), *)
    (* makeTest("andalso", [DprocTokens.KW_andalso]), *)
    (* makeTest("orelse", [DprocTokens.KW_orelse]), *)

    (* makeTest("fn", [DprocTokens.KW_fn]), *)
    (* makeTest("fun", [DprocTokens.KW_fun]), *)
    (* makeTest("val", [DprocTokens.KW_val]), *)

    (* makeTest("let", [DprocTokens.KW_let]), *)
    (* makeTest("in", [DprocTokens.KW_in]), *)
    (* makeTest("end", [DprocTokens.KW_end]), *)

    (* identifiers *)
    (* makeTest("a a1 a' a_", [DprocTokens.ID("a"), DprocTokens.ID("a1"), DprocTokens.ID("a'"), DprocTokens.ID("a_")]), *)
    (* makeTest("ab_ab_ab FooBar'Baz", [DprocTokens.ID("ab_ab_ab"), DprocTokens.ID("FooBar'Baz")]), *)

    (* programs *)
    (* makeTest("fun identity(x) = x", [ *)
    (*   DprocTokens.KW_fun, DprocTokens.ID("identity"), DprocTokens.LP, DprocTokens.ID("x"), DprocTokens.RP, *)
    (*   DprocTokens.OP("="), DprocTokens.ID("x") *)
    (* ]), *)
    (* makeTest("val identity = fn(x) => x", [ *)
    (*   DprocTokens.KW_val, DprocTokens.ID("identity"), DprocTokens.OP("="), DprocTokens.KW_fn, DprocTokens.LP, *)
    (*   DprocTokens.ID("x"), DprocTokens.RP, DprocTokens.ARROW, DprocTokens.ID("x") *)
    (* ]), *)
    (* makeTest("fun fib(n) = if n > 2 then fib(n-1) + fib(n-2) else 1", [ *)
    (*   DprocTokens.KW_fun, DprocTokens.ID("fib"), DprocTokens.LP, DprocTokens.ID("n"), DprocTokens.RP, *)
    (*   DprocTokens.OP("="), DprocTokens.KW_if, DprocTokens.ID("n"), DprocTokens.OP(">"), DprocTokens.CON_int(2), *)
    (*   DprocTokens.KW_then, DprocTokens.ID("fib"), DprocTokens.LP, DprocTokens.ID("n"), DprocTokens.OP("-"), *)
    (*   DprocTokens.CON_int(1), DprocTokens.RP, DprocTokens.OP("+"), DprocTokens.ID("fib"), DprocTokens.LP, *)
    (*   DprocTokens.ID("n"), DprocTokens.OP("-"), DprocTokens.CON_int(2), DprocTokens.RP, *)
    (*   DprocTokens.KW_else, DprocTokens.CON_int(1) *)
    (* ]) *)
  ])
end
