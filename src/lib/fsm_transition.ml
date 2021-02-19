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
  val of_string: string*string -> t
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

  let of_string (conds,acts) = Condition.list_of_string conds, Action.list_of_string acts
end

module Trans (T1: T) (T2: T) =
struct
  module FC = Fsm_condition.Trans (T1.Condition) (T2.Condition)
  module FA = Fsm_action.Trans (T1.Action) (T2.Action)
  let map fv (conds,acts)  = List.map (FC.map fv) conds, List.map (FA.map fv) acts
end
