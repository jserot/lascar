(**********************************************************************)
(*                                                                    *)
(*                              LASCAr                                *)
(*                                                                    *)
(*  Copyright (c) 2017-present, Jocelyn SEROT.  All rights reserved.  *)
(*                                                                    *)
(*  This source code is licensed under the license found in the       *)
(*  LICENSE file in the root directory of this source tree.           *)
(*                                                                    *)
(**********************************************************************)

open Ppxlib
open Lascar

let string_of_token t =
  let open Genlex in
  match t with
  | Int n -> "Int<" ^ string_of_int n ^ ">"
  | Float n -> "Float<" ^ string_of_float n ^ ">"
  | Char n -> "Char<" ^ String.make 1 n ^ ">"
  | String n -> "String<" ^ n ^ ">"
  | Ident n -> "Ident<" ^ n ^ ">"
  | Kwd n -> "Kwd<" ^ n ^ ">"

exception Parse_error of string * string option

let expand parser_name parser_fn ~loc ~path:_ (s:_) =
  let _ =
    try
      parser_fn s
    with
    | Utils.Parsing.Parse_error (s, Some tok) -> raise (Parse_error (s, Some (string_of_token tok)))
    | Utils.Parsing.Parse_error (s, None) -> raise (Parse_error (s, None)) in
  let f = Ast_builder.Default.evar ~loc parser_name in
  let e = Ast_builder.Default.estring ~loc s in
  [%expr [%e f] [%e e]]

let mk_ext ext_name parser_name parser_fn =
  Extension.declare
    ext_name
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    (expand parser_name parser_fn)

let () = Ppxlib.Driver.register_transformation "fsm_action" ~extensions:[mk_ext "fsm_action" "Fsm_action.Int.of_string" Fsm_action.Int.of_string]
let () = Ppxlib.Driver.register_transformation "fsm_trans" ~extensions:[mk_ext "fsm_trans" "Fsm_transition.Int.of_string" Fsm_transition.Int.of_string]
