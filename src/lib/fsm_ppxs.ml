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

let expand parser_name parser_fn ~loc ~path:_ (s:_) =
  let _ =
    try parser_fn s
    with Parse.Error (line,_,tok,msg) ->
      if line = -1 then (* No location *)
        Location.raise_errorf ~loc "%s " msg
      else
        Location.raise_errorf ~loc "%s at line %d near token \"%s\"" msg (loc.loc_start.pos_lnum+line) tok in
  let f = Ast_builder.Default.evar ~loc parser_name in
  let e = Ast_builder.Default.estring ~loc s in
  [%expr [%e f] [%e e]]

let mk_ext ext_name parser_name parser_fn =
  Extension.declare
    ext_name
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    (expand parser_name parser_fn)

let () = Ppxlib.Driver.register_transformation "fsm_action" ~extensions:[mk_ext "fsm_action" "Fsm_action.of_string" Fsm_action.of_string]
let () = Ppxlib.Driver.register_transformation "fsm_trans" ~extensions:[mk_ext "fsm_trans" "Fsm_transition.of_string" Fsm_transition.of_string]
