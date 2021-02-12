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

(** {2 Extension to the [Stdlib.Set.OrderedType] module} *)

module type T = sig
  include Set.OrderedType
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

module Either (M1: T) (M2: T) :
sig
  include T with type t = M1.t option * M2.t option
  val mk: M1.t option -> M2.t option -> t
end

module Either3 (M1: T) (M2: T) (M3: T) :
sig
  include T with type t = M1.t option * M2.t option * M3.t option
  val mk: M1.t option -> M2.t option -> M3.t option -> t
end
