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

(** {2 Values occuring in FSM expressions and FSMs} *)

module type T = sig
  type t [@@deriving show]
  val to_string: t -> string
  val binary_ops: (string * ((t -> t -> t) * int)) list (** name, (fun, infix level); ex: +, -, *, ... *)
  val unary_ops: (char * (t -> t)) list (** name, fun *)
  val of_int: int -> t
  val of_string: string -> t
end

(** Some predefined instances *)
module Int : T with type t = int

module Bool : T with type t = bool
