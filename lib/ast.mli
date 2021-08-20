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

val sexp_of_t : t -> Sexp.t
