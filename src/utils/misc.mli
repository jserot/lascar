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

(** Several useful types and functions *)

type ('a, 'b) either =
    Fst of 'a
  | Snd of 'b
  | Both of 'a * 'b

val log2 : int -> int

val time_of_day : unit -> string

val flip: ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
    (** [flip f x y] is [f y x] *)

val id: 'a -> 'a
    (** [id x] is [x] *)

val max: 'a -> 'a -> 'a 
val min: 'a -> 'a -> 'a 

val iter_fix: ('a -> 'a -> bool) -> ('a -> 'a) -> 'a -> 'a
    (** [iter_fix eq f x] iterates [f], starting with [x], until idempotence, i.e. computes 
        [f (f (f ... f(x)...))] until the  condition [f(x)=x] is met.  
        Equality test is performed with the [eq] argument *)

val append_file: string -> string -> unit
    (** [append_file f s] appends [s] to file [f] *)
