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

module type T = sig
  include Set.OrderedType
  val to_string: t -> string
end

module Pair (M1: T) (M2: T) =
struct 
  type t = M1.t * M2.t 
  let compare = compare 
  let to_string (e1,e2) = M1.to_string e1 ^ "," ^ M2.to_string e2
  let mk e1 e2 = e1,e2
end

module Triplet (M1: T) (M2: T) (M3: T) =
struct 
  type t = M1.t * M2.t * M3.t
  let compare = compare 
  let to_string (e1,e2,e3) = M1.to_string e1 ^ "," ^ M2.to_string e2 ^ "," ^ M3.to_string e3
  let mk e1 e2 e3 = e1,e2,e3
end

module Either (M1: T) (M2: T) =
struct 
  type t = M1.t option * M2.t option
  let compare = compare 
  let to_string = function
            (None, None) -> ""
          | (Some e1, None) -> M1.to_string e1
          | (None, Some e2) -> M2.to_string e2
          | (Some e1, Some e2) -> M1.to_string e1 ^ "," ^ M2.to_string e2
  let mk e1 e2 = e1,e2
end

module Either3 (M1: T) (M2: T) (M3: T) =
struct 
  type t = M1.t option * M2.t option * M3.t option
  let compare = compare 
  let to_string = function
            (None, None, None) -> ""
          | (Some e1, None, None) -> M1.to_string e1
          | (None, Some e2, None) -> M2.to_string e2
          | (None, None, Some e3) -> M3.to_string e3
          | (Some e1, Some e2, None) -> M1.to_string e1 ^ "," ^ M2.to_string e2
          | (Some e1, None, Some e3) -> M1.to_string e1 ^ "," ^ M3.to_string e3
          | (None, Some e2, Some e3) -> M2.to_string e2 ^ "," ^ M3.to_string e3
          | (Some e1, Some e2, Some e3) -> M1.to_string e1 ^ "," ^ M2.to_string e2 ^ "," ^ M3.to_string e3
  let mk e1 e2 e3 = e1,e2,e3
end
