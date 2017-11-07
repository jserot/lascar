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

(** Several functions operating on values of type ['a option] *)

val map: ('a -> 'b) -> 'a option -> 'b option
    (** [apply f v] is [Some (f x)] if [v] is [Some x], [None] if [v] is [None] *)
 
val iter: ('a -> unit) -> 'a option -> unit
    (** [iter f x] applies [f] to [v] if [x] is [Some v], does nothing if [x] is [None] *)

val to_string: ('a -> string) -> 'a option -> string
    (** [to_string f v] is [f x] if [v] is [Some x], [""] if [v] is [None] *)


