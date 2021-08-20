open Base

(** parse_expr [str] parses multiline scheme expr and converts it to
    [Ast.t list]*)
val parse_expr : string -> (Ast.t, string) Result.t
