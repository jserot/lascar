open Utils
open Lascar

let string_of_token t =
  let open Genlex in
  match t with
  | Int n -> "Int " ^ string_of_int n
  | Float n -> "Float " ^ string_of_float n
  | Char n -> "Char " ^ String.make 1 n
  | String n -> "String " ^ n
  | Ident n -> "Ident " ^ n
  | Kwd n -> "Kwd " ^ n

let test lexer parse show s =
  let r = match Parsing.run lexer parse s with
    | Ok e -> show e
    | Error None -> "syntax error "
    | Error (Some t) -> "syntax error at token " ^ string_of_token t in
  Printf.printf "%s => %s\n" s r

module IntExpr = Fsm_expr.Make(Fsm_value.Int)
               
let _ =
  List.iter
    (test IntExpr.lexer IntExpr.parse IntExpr.to_string) [
      "1";
      "-1";
      "1+2";
      "-1+2";
      "1+(-2)";
      "1+2*3";
      "1*2+3";
      "1*(2+3)";
      "k+1";
      "-k";
      "(-k)*2";
      "+"
      ]

module BoolExpr = Fsm_expr.Make(Fsm_value.Bool)
               
let _ =
  List.iter
    (test BoolExpr.lexer BoolExpr.parse BoolExpr.to_string) [
      "k";
      "0";
      "1";
      "k && 1";
      "k && u || v";
      "k && (u||v)";
      "u||v && k";
      "~u";
      "~(u&&v)";
      "~~"
      ]
