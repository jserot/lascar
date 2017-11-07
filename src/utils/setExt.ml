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

(* module type OrderedType = sig *)
(*   include Set.OrderedType *)
(*   val to_string: t -> string *)
(*   (\* include Stringable with type t := t *\) *)
(* end *)

module type S = sig
  include Set.S
  module Elt: OrderedTypeExt.T with type t = elt
  val map: (elt -> elt) -> t -> t
  val power: t -> t list
  val extract: t -> elt * t
  val to_string: t -> string
end

module Make (E: OrderedTypeExt.T) = struct
  module S = Set.Make(E)
  module Elt = E
  include S
  let map f s = S.fold (fun e s -> S.add (f e) s) s S.empty
  let extract s = let e = S.choose s in e, S.remove e s
  let rec power s =
    if S.is_empty s
    then [S.empty]
    else
      let e, s' = extract s in
      let ss = power s' in ss @ (List.map (fun s -> add e s) ss)
  let to_string s = "{" ^ ListExt.to_string E.to_string "," (S.elements s) ^ "}"
end

(* module Pair (E1: OrderedType.T) (E2: OrderedType.T) = *)
(*   struct *)
(*     type t = E1.t * E2.t *)
(*     let compare = compare *)
(*     let to_string (x,y) = E1.to_string x ^ "," ^ E2.to_string y *)
(*   end *)

module Product (S1: S) (S2: S) =
  struct
    (* module R = Make(Pair(S1.Elt)(S2.Elt)) *)
    module R = Make(OrderedTypeExt.Pair(S1.Elt)(S2.Elt))
    include R
    let product s1 s2 =
      let f x y t = R.add (x,y) t in
      let g x t = S2.fold (f x) s2 t in
      S1.fold g s1 R.empty
  end

