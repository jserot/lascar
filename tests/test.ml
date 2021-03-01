let string_of_token t =
  let open Genlex in
  match t with
  | Int n -> "Int " ^ string_of_int n
  | Float n -> "Float " ^ string_of_float n
  | Char n -> "Char " ^ String.make 1 n
  | String n -> "String " ^ n
  | Ident n -> "Ident " ^ n
  | Kwd n -> "Kwd " ^ n

let test lexer parser show s =
  let r = match Utils.Parsing.run ~lexer ~parser s with
    | Ok e -> show e
    | Error None -> "syntax error "
    | Error (Some t) -> "syntax error at token " ^ string_of_token t in
  Printf.printf "%s => %s\n" s r
