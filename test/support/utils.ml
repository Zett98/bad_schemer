open Base
open Scheme

let print to_print =
  ( match to_print with
  | Ok ast -> Ast.sexp_of_t ast
  | Error err -> String.sexp_of_t err )
  |> Sexp.to_string_hum |> Stdio.print_endline

let parse_and_print input =
  let parsed = Parser.parse_expr input in
  print parsed;
  parsed

let eval_and_print input = Result.map ~f:Interpret.eval input |> print
