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

(** {2 Actions for FSM transitions} *)

module type T = sig
  module Expr : Fsm_expr.T
  type t = 
  | Assign of Expr.ident * Expr.t          (* var, value *)
      [@@deriving show {with_path=false}]
  val to_string: t -> string
  val of_string: ?lexer:(string->Genlex.token Stream.t) -> string -> t               
  val lexer: string -> Genlex.token Stream.t
  val parse: Genlex.token Stream.t -> t               
end

module Make (Expr: Fsm_expr.T) : T with module Expr = Expr

(** Functor for converting a FSM action, with a given implementation of values
   into another one with a different implementations *)
module Trans (A1: T) (A2: T) :
sig
  val map: (A1.Expr.value -> A2.Expr.value) -> A1.t -> A2.t
end
