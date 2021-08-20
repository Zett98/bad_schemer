open! Base
open Angstrom

let symbol = take_while1 (String.mem "!$%&|*+-/:<=>?@^_~")

let letter =
  take_while1 (function
    | 'A' .. 'Z'
    | 'a' .. 'z' ->
      true
    | _ -> false )

let parse_digit_1 =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false )

let parse_digit_2 =
  string "#d"
  *> take_while1 (function
       | '0' .. '9' -> true
       | _ -> false )

let parse_hex =
  string "#x"
  *> take_while1 (function
       | '0' .. '9'
       | 'a' .. 'f'
       | 'A' .. 'F' ->
         true
       | _ -> false )
  >>= fun x -> return @@ "0x" ^ x

let parse_number = parse_digit_1 <|> parse_digit_2 <|> parse_hex

let escaped =
  let* cc = char '\\' *> satisfy (String.mem "\\\"ntr") in
  match cc with
  | '\\' -> return @@ String.of_char cc
  | '"' -> return @@ String.of_char cc
  | 't' -> return "\t"
  | 'n' -> return "\n"
  | 'r' -> return "\r"
  | _ -> failwith "impossible match"

let parse_char_name =
  string "space" <|> string "newline"
  >>= function
  | "space" -> return ' '
  | "newline" -> return '\n'
  | _ -> failwith "impossible named char"

let p_char =
  let* res = string "#\\" *> (parse_char_name <|> any_char) in
  return @@ Ast.Char res

let p_false = string "#f" *> return (Ast.Boolean false)
let p_true = string "#t" *> return (Ast.Boolean true)
let p_bools = p_false <|> p_true

let p_atom =
  let* first = letter <|> symbol in
  let* rest = many (letter <|> parse_number <|> symbol) in
  let atom = first :: rest |> String.concat |> String.strip in
  return @@ Ast.Atom atom

let p_number =
  let* number = parse_number in
  return @@ Ast.Number (Int.of_string number)

let p_string =
  let* _ = char '"' in
  let* str =
    many (take_while1 (fun x -> not @@ String.mem "\"\\" x) <|> escaped)
  in
  let* _ = char '\"' in
  return (Ast.String (String.concat str))

let ws =
  skip_while (function
    | '\x20' (* not a valid digit, letter or punctuation*)
    | '\x0a' (* \n *)
    | '\x0d' (* \r *)
    | '\x09' (* \t *) ->
      true
    | _ -> false )

let parse_expr = p_bools <|> p_atom <|> p_string <|> p_number <|> p_char

let parse_list p =
  let* _ = char '(' *> ws in
  let* head = sep_by ws p <* ws in
  let dotted_tail =
    char '.' *> ws *> p <* ws <* char ')' >>| fun x -> Ast.DottedList (head, x)
  in
  let list_end = ws *> char ')' *> return (Ast.List head) in
  dotted_tail <|> list_end

let parse_array p =
  let* arr = string "#(" *> ws *> sep_by ws p <* ws <* string ")" in
  return @@ Ast.Vector (Array.of_list arr)

let parse_quoted p =
  char '\'' *> p >>| fun x -> Ast.List [ Ast.Atom "quote"; x ]

let parse_qquoted p =
  char '`' *> p >>= fun x -> return @@ Ast.List [ Ast.Atom "quasiquote"; x ]

let parse_unquote p =
  char ',' *> p >>= fun x -> return @@ Ast.List [ Ast.Atom "unquote"; x ]

let parse_unquote_splicing p =
  char ',' *> char '@' *> p
  >>= fun x -> return @@ Ast.List [ Ast.Atom "unquote-splicing"; x ]

let quotations p =
  choice
    [ parse_quoted p; parse_qquoted p; parse_unquote p; parse_unquote_splicing p
    ]

let pparser =
  fix (fun pparser ->
      parse_expr <|> quotations pparser <|> parse_array pparser
      <|> parse_list pparser )

let parse_expr s = parse_string ~consume:Consume.All (ws *> pparser) s
