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

(** Extension to the {!Stdib.Set} module *)

(** Input signature of the {!SetExt.Make} functor. *)
(* module type OrderedType = sig *)
(*   include Set.OrderedType *)
(*   (\* type t *\) *)
(*   val to_string: t -> string *)
(*   (\* include Stringable with type t := t *\) *)
(* end *)

(** Output signature of the {!SetExt.Make} functor. *)
module type S = sig
  include Set.S
  module Elt : OrderedTypeExt.T with type t = elt
  val map: (elt -> elt) -> t -> t
      (** [map f s] applies function [f] to each element of set [s] *)
  val power: t -> t list
      (** [power_set s] computes the power set of [s].
          {b Note}: the result is returned as a {e list} of sets because sets of sets cannot be defined
          easily in a functorial way.. *)
  val extract: t -> elt * t
      (** Extract an element from a set, returning the element and the updated set.
          Raise [Not_found] if the set is empty *)
  val to_string: t -> string
      (** Returns a string representation *)
end

(** Functor building an implementation of the set structure given a totally ordered, stringable type. *)
module Make (E : OrderedTypeExt.T) : S with type elt = E.t

(** Functor for pairing structures describing elements for computing set products *)
(* module Pair (E1: OrderedType.T) (E2: OrderedType.T) : *)
(*     sig *)
(*       include OrderedType.T with type t = E1.t * E2.t *)
(*     end *)

(** Functor building an implementation of a set product given the implementation of two sets *)
module Product (S1: S) (S2: S) :
    sig
      include S with type elt = S1.elt * S2.elt
      val product: S1.t -> S2.t -> t
    end
