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

(** The signature of values that may appear in FSM expressions *)
module type VALUE = sig
  type t
  val binary_ops: (string * ((t -> t -> t) * int)) list (** name, (fun, infix level); ex: +, -, *, ... *)
  val unary_ops: (char * (t -> t)) list (** name, fun *)
  val of_int: int -> t
  val of_string: string -> t
  val to_string: t -> string
end
  
(** Output signature of the functor {!Fsm_expr.Make}. *)
module type T = sig

  type ident = string 
  (** The type of identifiers occuring in expressions *)

  type value
  (** The type of expression values *)

  (** The type of expressions *)
  type t = 
    EConst of value            (** Constants *)   
  | EVar of ident              (** Input, output or local variable *)
  | EBinop of string * t * t   (** Binary operation *)
  | EUnop of char * t        (** Unary operation  *)

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

end

(** Functor building an implementation of the Fsm_expr structure given an implementation of values *)
module Make (V: VALUE) : T with type value = V.t

(** Some predefined instances *)
module Int : T with type value = int
module Bool : T with type value = bool
