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

open Utils

module type T = sig
  module Expr: Fsm_expr.T
  module Condition: Fsm_condition.T with module Expr = Expr
  module Action: Fsm_action.T with module Expr = Expr
  type t = Condition.t list * Action.t list
  val compare: t -> t -> int
  val to_string: t -> string
  val of_string: string -> t
  val parse: Genlex.token Stream.t -> t
end

module Make (Expr: Fsm_expr.T) = struct

  module Expr = Expr

  module Condition = Fsm_condition.Make(Expr)

  module Action = Fsm_action.Make(Expr)

  type t = Condition.t list * Action.t list

  let compare = Stdlib.compare

  let to_string (conds,acts) =
    let s1 = ListExt.to_string Condition.to_string ", " conds in
    let s2 = ListExt.to_string Action.to_string ", " acts in
    let l = String.make (Misc.max (String.length s1) (String.length s2)) '_' in
    if s2 <> ""
    then Printf.sprintf "%s\\n%s\\n%s" s1 l s2 
    else Printf.sprintf "%s" s1

  let keywords = Expr.keywords @ [","; "|"]

  let p_conditions = Parsing.separated_list "," ~stop_on:(Some "|") Condition.parse

  let p_actions = Parsing.separated_list "," Action.parse

  let lexer s = s |> Expr.mk_unaries |> Stream.of_string |> Genlex.make_lexer keywords 
               
  let p_transition s =
    let p_rest s =
      match Stream.peek s with
      | Some (Genlex.Kwd sep) when sep="|" -> Stream.junk s; p_actions s
      | Some _ -> raise Stream.Failure
      | None -> [] in
    match Stream.peek s with
    | Some (Genlex.Kwd sep) when sep="|" ->
       Stream.junk s;
       let acts = p_rest s in
       ([], acts)
    | Some _ ->
       let conds = p_conditions s in
       let acts = p_actions s in
       conds, acts
    | None ->
       [], []
      
  let parse = p_transition
  let of_string s = p_transition (lexer s)
end

module Trans (T1: T) (T2: T) =
struct
  module FC = Fsm_condition.Trans (T1.Condition) (T2.Condition)
  module FA = Fsm_action.Trans (T1.Action) (T2.Action)
  let map fv (conds,acts)  = List.map (FC.map fv) conds, List.map (FA.map fv) acts
end
