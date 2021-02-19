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

(** {2 FSM transitions} *)

module type T = sig
  module Expr: Fsm_expr.T
  module Condition: Fsm_condition.T with module Expr = Expr
  module Action: Fsm_action.T with module Expr = Expr
  type t = Condition.t list * Action.t list
  val compare: t -> t -> int
  val to_string: t -> string
  val of_string: string*string -> t
end

module Make (Expr: Fsm_expr.T) : T with module Expr = Expr
                                    and module Condition.Expr = Expr
                                    and module Action.Expr = Expr
                                    (* and type Condition.Expr.value = Expr.value
                                     * and type Action.Expr.value = Expr.value *)

(** Functor for converting an FSM transition, with a given implementation of values
   into another one with a different implementation *)
module Trans (T1: T) (T2: T) :
sig
  val map: (T1.Expr.value -> T2.Expr.value) -> T1.t -> T2.t
end
