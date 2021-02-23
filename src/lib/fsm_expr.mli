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

(** {2 Simple expressions for FSMs} *)

module type T = sig

  type ident = string [@@deriving show]
  (** The type of identifiers occuring in expressions *)

  type value [@@deriving show]
  (** The type of expression values *)

  (** The type of expressions *)
  type t = 
    EConst of value            (** Constants *)   
  | EVar of ident              (** Input, output or local variable *)
  | EBinop of string * t * t   (** Binary operation *)
  | EUnop of char * t        (** Unary operation  *)
  [@@deriving show {with_path=false}]
           
  type env = (ident * value option) list
           
  exception Unknown of ident
  exception Unknown_op of string
  exception Unbound of ident
  exception Illegal_expr

  val test_ops: (string * (value -> value -> bool)) list (** name, fun *)

  val to_string: t -> string

  val of_string: string -> t

  val lookup: env -> ident -> value

  val eval: env -> t -> value

  val lexer: string -> Genlex.token Stream.t
  val parse: Genlex.token Stream.t -> t 

  val keywords: string list
  val mk_unaries: string -> string

end

(** Functor building an implementation of the Fsm_expr structure given an implementation of values *)
module Make (V: Fsm_value.T) : T with type value = V.t

(** Functor for converting a FSM expression, with a given implementation of values
   into another one with a different implementations *)
module Trans (E1: T) (E2: T) :
sig
  val map: (E1.value -> E2.value) -> E1.t -> E2.t
end
