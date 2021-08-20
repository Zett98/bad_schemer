open Base

val parse_and_print : string -> (Scheme.Ast.t, string) Result.t
val eval_and_print : (Scheme.Ast.t, string) Result.t -> unit
