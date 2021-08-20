open! Base
open! Stdio
open Test_utils

let%expect_test _ =
  let parsed_expr = Utils.parse_and_print "(+ 1 2 (* 2 2))" in
  [%expect
    {|
    (List
     ((Atom +) (Number 1) (Number 2) (List ((Atom *) (Number 2) (Number 2))))) |}];
  Utils.eval_and_print parsed_expr;
  [%expect {| (Number 7) |}]

let%expect_test _ =
  let parsed_expr = Utils.parse_and_print "(+ 1 2)" in
  [%expect {| (List ((Atom +) (Number 1) (Number 2))) |}];
  Utils.eval_and_print parsed_expr;
  [%expect {| (Number 3) |}]

let%expect_test _ =
  let parsed_expr = Utils.parse_and_print "(mod 1 (+ 1 2 3) 4 (-1 2) 5)" in
  [%expect
    {|
    (List
     ((Atom mod) (Number 1) (List ((Atom +) (Number 1) (Number 2) (Number 3)))
      (Number 4) (List ((Atom -1) (Number 2))) (Number 5))) |}];
  Utils.eval_and_print parsed_expr;
  [%expect {| (Number 1) |}]

let%expect_test _ =
  let parsed_expr = Utils.parse_and_print "#(1 2 3 4)" in
  [%expect {| (Vector ((Number 1) (Number 2) (Number 3) (Number 4))) |}];
  Utils.eval_and_print parsed_expr;
  [%expect {| (Vector ((Number 1) (Number 2) (Number 3) (Number 4))) |}]

let%expect_test _ =
  let parsed_expr = Utils.parse_and_print "'(1)" in
  [%expect {| (List ((Atom quote) (List ((Number 1))))) |}];
  Utils.eval_and_print parsed_expr;
  [%expect {| (List ((Number 1))) |}]

let%expect_test _ =
  let parsed_expr = Utils.parse_and_print "(+\"valid\")" in
  [%expect {| (List ((Atom +) (String valid))) |}];
  Utils.eval_and_print parsed_expr;
  [%expect {| (String valid) |}]

let%expect_test _ =
  let parsed_expr = Utils.parse_and_print "(+ 1 2 4 5 6)" in
  [%expect
    {| (List ((Atom +) (Number 1) (Number 2) (Number 4) (Number 5) (Number 6))) |}];
  Utils.eval_and_print parsed_expr;
  [%expect {| (Number 18) |}]

let%expect_test _ =
  let parsed_expr = Utils.parse_and_print "(#\\C)" in
  [%expect {| (List ((Char C))) |}];
  Utils.eval_and_print parsed_expr;
  [%expect {| (List ((Char C))) |}]

let%expect_test _ =
  let parsed_expr = Utils.parse_and_print "(+ 1 2 . (1 2))" in
  [%expect {| (DottedList ((Atom +) (Number 1) (Number 2)) (List ((Number 1) (Number 2)))) |}];
  Utils.eval_and_print parsed_expr;
  [%expect {| (Number 6) |}]

let%expect_test _ =
  let parsed_expr = Utils.parse_and_print "(1 2 . 3)" in
  [%expect {| (DottedList ((Number 1) (Number 2)) (Number 3)) |}];
  Utils.eval_and_print parsed_expr;
  [%expect {| (List ((Number 1) (Number 2) (Number 3))) |}]

let%expect_test _ =
  let parsed_expr = Utils.parse_and_print "(1 + 1)" in
  [%expect {| (List ((Number 1) (Atom +) (Number 1))) |}];
  Utils.eval_and_print parsed_expr;
  [%expect {| (List ((Number 1) (Atom +) (Number 1))) |}]