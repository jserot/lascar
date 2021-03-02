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

(** {2 Conditions for FSM transitions} *)

module type T = sig
  module Expr : Fsm_expr.T
  type t = 
    | Test of Expr.ident * string * Expr.t (* var, op, expr *)
      [@@deriving show {with_path=false}]
  val to_string: t -> string
  val of_string: ?lexer:(string->Genlex.token Stream.t) -> string -> t               
  val lexer: string -> Genlex.token Stream.t
  val parse: Genlex.token Stream.t -> t               
  val keywords: string list
  val eval: Expr.env -> t -> bool
end

module Make (Expr: Fsm_expr.T) : T with module Expr = Expr

(** Functor for converting a FSM condition, with a given implementation of values
   into another one with a different implementations *)
module Trans (C1: T) (C2: T) :
sig
  val map: (C1.Expr.value -> C2.Expr.value) -> C1.t -> C2.t
end
