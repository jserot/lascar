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

module type VALUE = Utils.OrderedTypeExt.T

module type T = sig
    type name = string
    type value
    type t = (name * value) list
    val compare: t -> t -> int
    val to_string: t -> string
    exception Invalid_valuation of t
    val check: name list -> t -> unit
    val empty: t
    exception Duplicate of name
    val add: name -> value -> t -> t
    val remove: name -> t -> t
    val mem: name -> t -> bool
    val assoc: name -> t -> value
    val string_of_value: value -> string
end

module Make (V: VALUE) =
struct
  type name = string

  type value = V.t

  type t = (name * value) list  (* A simple implementation using association list *)

  let empty = []

  exception Duplicate of name
                       
  let add n v vs = if List.mem_assoc n vs then raise (Duplicate n) else (n,v)::vs
 
  let remove n vs = List.remove_assoc n vs

  let mem n vs = List.mem_assoc n vs

  let assoc n vs = List.assoc n vs

  let compare vs1 vs2 =
    let module S = Set.Make (struct type t = name * value  let compare = Stdlib.compare end) in
    S.compare (S.of_list vs1) (S.of_list vs2)

  let of_list l = l
  let to_list l = l

  let to_string vs = Utils.ListExt.to_string (function (n,v) -> n ^ "=" ^ V.to_string v) "," vs

  exception Invalid_valuation of t

  let names_of v = List.map fst v

  let check names v =
    let module S = Set.Make (struct type t = string let compare = Stdlib.compare end) in
    if not (S.equal (S.of_list names) (S.of_list (names_of v))) then raise (Invalid_valuation v)
 
  let string_of_value v = V.to_string v
end

module Bool =
  Make(
      struct
        type t = bool
        let compare = Stdlib.compare
        let to_string = function true -> "1" | false -> "0"
      end)

module Int =
  Make(
      struct
        type t = int
        let compare = Stdlib.compare
        let to_string = string_of_int
      end)
