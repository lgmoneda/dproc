use "src/interpreter.sml";
val _ = print("\nRodando bateria de testes...\n\n\n");

val _ = print("\n Teste 1 - test_ifelse.dproc\n\n");
val _ = makeTest("tests/test_ifelse.dproc");
val _ = print("O valor da variável avg deve ser (Float_v 255.0), e foi calculada como: \n");
look_up("avg", !record);
val _ = print("O valor da variável flag deve ser (Bool_v true), e foi calculada como: \n");
look_up("flag", !record);

val _ = print("\n\n\n Teste 2 - test_log_comp.dproc\n\n");
val _ = makeTest("tests/test_log_comp.dproc");
val _ = print("O valor da variável c deve ser [Int_v 4, Int_v 4, Int_v 2, Int_v 2, Int_v 4], e foi calculada como: \n");
look_up("c", !record);
val _ = print("O valor da variável d deve ser [Int_v 1, Int_v 0, Int_v 0, Int_v 1, Int_v 0], e foi calculada como: \n");
look_up("d", !record);

val _ = print("\n\n\n Teste 3 - test_oper.dproc\n\n");
val _ = makeTest("tests/test_oper.dproc");
val _ = print("O valor da variável a deve ser [Float_v 0.6, Float_v 1.04, Float_v 0.54, Float_v 0.64], e foi calculada como: \n");
look_up("a", !record);

val _ = print("\n\n\n Teste 4 - test_strings.dproc\n\n");
val _ = makeTest("tests/test_strings.dproc");
val _ = print("O valor da variável result deve ser [String_v \"Ola, esse \", String_v \"e um \", String_v \"teste da \", String_v \"linguagem DPROC!\"], e foi calculada como: \n");
look_up("result", !record);
val _ = print("O valor da variável result2 deve ser [String_v \"oi!\", String_v \"tchau!\"], e foi calculada como: \n");
look_up("result2", !record);
