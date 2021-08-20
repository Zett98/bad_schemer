open Base
open Stdio
open Scheme

let read_and_eval s =
  Parser.parse_expr s
  |> Result.map ~f:(fun line ->
         let ast = Ast.sexp_of_t line in
         print_endline [%string "ast:\n %{ast#Sexp}"];
         let interpreted = Interpret.eval line |> Ast.sexp_of_t in
         print_endline [%string "result:\n %{interpreted#Sexp}"] )
  |> Result.iter_error ~f:(fun err -> prerr_endline @@ "error " ^ err)

let () = In_channel.input_line stdin |> Option.iter ~f:read_and_eval
