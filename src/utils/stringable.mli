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

(** String-convertible types *)

module type T = sig
  type t
  val to_string: t -> string
end

module Pair (M1: T) (M2: T) :
sig
  include T with type t = M1.t * M2.t
  val mk: M1.t -> M2.t -> t
end

module Triplet (M1: T) (M2: T) (M3: T) :
sig
  include T with type t = M1.t * M2.t * M3.t
  val mk: M1.t -> M2.t -> M3.t -> t
end
              
