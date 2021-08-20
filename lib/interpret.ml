open! Base

let rec unpack_num = function
  | Ast.Number n -> n
  | Ast.String s -> (
    try Int.of_string s with
    | _ -> 0 (* default to 0 if we failed to parse the string*) )
  | Ast.List [ n ] -> unpack_num n
  | Ast.Char c -> Char.to_int c
  | Ast.List _
  | Ast.Atom _
  | Ast.DottedList _
  | Ast.Boolean _
  | Vector _ ->
    0

let[@warning "-8"] numeric_binop op (init :: rest) =
  let init = init |> unpack_num in
  Ast.Number
    (List.fold_left ~f:(fun acc x -> op acc @@ unpack_num x) ~init rest)

let quot a b =
  let rec go ?(acc = 0) a b =
    if a < b then
      acc
    else
      go ~acc:(acc + 1) (a - b) b in
  go a b

let lookup_table =
  let empty = Hashtbl.create (module String) in
  let lst =
    [ ("+", numeric_binop ( + )); ("-", numeric_binop ( - ))
    ; ("*", numeric_binop ( * )); ("/", numeric_binop ( / ))
    ; ("mod", numeric_binop ( % )); ("quotient", numeric_binop quot)
    ; ("remainder", numeric_binop Int.rem) ] in
  lst |> List.iter ~f:(fun (key, data) -> Hashtbl.add_exn empty ~key ~data);
  empty

let apply func args =
  try
    let func = Hashtbl.find_exn lookup_table func in
    func args
  with
  | _ -> Ast.Boolean false

let eval_dotted eval (head, tail) =
  let[@warning "-4"] rec flatten = function
    | Ast.List x -> x
    | any -> flatten (Ast.List [ eval any ]) in
  eval (Ast.List (head @ flatten tail))

let rec eval = function
  | (Ast.Char _ | Ast.Number _ | Ast.String _ | Ast.Boolean _) as value -> value
  | Ast.List [ Ast.Atom _; x ] -> x
  | Ast.List (Ast.Atom func :: args) -> apply func @@ List.map ~f:eval args
  | Ast.DottedList (head, tail) -> eval_dotted eval (head, tail)
  | Ast.Vector x -> Ast.Vector (Array.map ~f:eval x)
  | Ast.List x -> Ast.List (List.map ~f:eval x)
  | Ast.Atom _ as x -> x
