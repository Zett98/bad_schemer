open Base

type t =
  | Atom of string
  | List of t list
  | DottedList of t list * t
  | Number of int
  | String of string
  | Boolean of bool
  | Char of char
  | Vector of t array
[@@deriving sexp_of]