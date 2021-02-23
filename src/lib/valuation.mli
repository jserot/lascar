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

(** {2 A [valuation] is a collection of [(name,value)] associations} *)

module type T = sig

    type name = string
      (** The type of variable names *)

    type value 
      (** The type for variable values *)

    type t = (name * value) list 
    (** The type of valuation. Basic, public implementation here *)

    val compare: t -> t -> int

    val to_string: t -> string

    exception Invalid_valuation of t
                                 
    val check: name list -> t -> unit
      (** [check names vs] checks whether [vs] is a "complete" valuation wrt. to [names]. {i i.e.} whether
          each variable listed in [names] has a valuation in [vs] and each variable listed in [vs] occurs in 
          [names]. Raises {!Invalid_valuation} in case of failure. *)

    val empty: t

    exception Duplicate of name

    val add: name -> value -> t -> t

    val remove: name -> t -> t

    val mem: name -> t -> bool

    val assoc: name -> t -> value

    val string_of_value: value -> string
end

(** Functor building an implementation of the Valuation structure given an implementation of values *)
module Make (V: Fsm_value.T) : T with type value = V.t

(** Some predefined instances *)

module Bool : T with type value = bool
module Int : T with type value = int
     
