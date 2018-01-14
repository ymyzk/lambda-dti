open OUnit2

let suite = "test">:::[
    "test_examples">::: Test_examples.suite;
    "test_pp">::: Test_pp.suite;
    "test_typing">::: Test_typing.suite;
  ]

let () = run_test_tt_main suite
