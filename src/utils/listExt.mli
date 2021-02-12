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

(** {2 Extension to the [Stdlib.List] module} *)

val fold_lefti : ('a -> int -> 'b -> 'a) -> 'a -> 'b list -> 'a
  (** [fold_lefti f a [b1; ...; bn]] is [f (... (f 1 (f 0 a b1) b2) ...) bn]. *)

val fold_righti : (int -> 'a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  (** [fold_righti f [a1; ...; an] b] is [f a1 (f (n-2) a2 (... (f (n-1) an b) ...))]. *)

val split3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
  (** Transform a list of triplets into a triplet of lists. *)

val combine3 : 'a list -> 'b list -> 'c list -> ('a * 'b * 'c) list
  (** Transform a triplet of lists into a list of triplets. *)

val to_string: ('a -> string) -> string -> 'a list -> string
    (** [to_string f sep [x1;...;xn]] returns [f x1 ^ sep ^ f x2 ^ sep ^ ... ^ sep ^ f xn]
        or the empty string if the list is empty *)

val range: (int -> 'a) -> int -> int -> 'a list
    (** [make f n1 n2] returns [[f n1; ..., f n2]] if [n2>=n1] or the empty list if [n2<n1] *)

val index_of: 'a -> 'a list -> int
    (** [index_of e l] returns the index of the first occurence of [e] in list [l], starting at 0.
        @raise Failure is [e] does not occur in [l] *)
    
val flat_map: ('a -> 'b list) -> 'a list -> 'b list
    (** [flat_map f [x1;...,xn]] is [f x1 @ f x2 @ ... @ f xn] *)

val inter: 'a list -> 'a list -> 'a list
    (** [inter l1 l2] returns the intersection of the lists [l1] and [l2] *)

val union: 'a list -> 'a list -> 'a list
    (** [union l1 l2] returns the union of the lists [l1] and [l2] *)

val merge: ?cmp:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
    (** [merge p l1 l2] merges lists [l1] and [l2] using comparison function [cmp] (default: Pervasives.compare).
        For example, [merge [1;3;8] [2;5;6;10] = [1;2;3;5;5;8;10]]. *)

val prod: ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
    (** [prod f l1 l2] computes the "product" of lists [l1] and [l2], i.e. the list obtained by applying [f] to each
        possible pair of elements of [l1] and [l2]. *)

val cart_prod2: 'a list -> 'b list -> ('a * 'b) list
    (** [cart_prod] is a specialized version of [prod] in which [f x y = (x,y)] *)

val prod3: ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list
val cart_prod3: 'a list -> 'b list -> 'c list -> ('a * 'b *'c) list
    (** [prod3] (resp. [cart_prod3]) are generalisations of {!prod} (resp. {!cart_prod2}) to three input lists *)

val cart_prodn: 'a list list -> 'a list list
    (** [gen_cart_prod] computes the n-ary cartesian product of lists. *)

val filter_map: ('a -> bool) -> ('a -> 'b) -> 'a list -> 'b list
    (** [filter_map p f l] is a short-cut for [List.map f (List.filter p l)] *)

val iter_sep: ('a -> unit) -> (unit -> unit) -> 'a list -> unit
    (** [iter_sep f g l] is like [List.iter f l] but calls function [g] between each call to function [f]. *)

val iter_fst: (bool -> 'a -> unit) -> 'a list -> unit
    (** [iter_fst f [x1;x2;...xn]] is [f true x1; f false x2; ...; f false xn] *)

val remove: 'a -> 'a list -> 'a list
    (** [remove e l] is a short-cut for [List.filter (fun e' -> e <> e') l)] *)

val power_set: 'a list -> 'a list list
    (** [power_set l] computes the "power set" of [l], i.e. the list of all lists composed of elements of [l].
        The length of the result is [2^n], where [n] is the length of [l].
        Example: [power [1;2;3]] is [[[];[1];[2];[3];[1;2];[1;3];[2;3];[1;2;3]]] *)

val power: int -> 'a list -> 'a list list
    (** [power n l] computes the list of all lists of length [n] made of elements of [l].
        The length of the result is [m^n], where [m] is the length of [l].
        Example : [power 2 [1;2;3]] is [[[1;1];[2;1];[3;1];[1;2];[2;2];[3;2];[1;3];[2;3];[3;3]]] *)

val assoc: ?cmp:('a -> 'a -> int) -> 'a -> ('a * 'c) list -> 'c
val mem_assoc: ?cmp:('a -> 'a -> int) -> 'a -> ('a * 'c) list -> bool
    (** [assoc] (resp. [mem_assoc]) is a variant of {!Stdlib.List.assoc} (resp. {!Stdlib.List.mem_assoc}) in which
        the comparison function used for comparing keys can be passed as an optional argument. By default,
        {!Pervasive.compare} is used. *)

val update_assoc: ?cmp:('k -> 'k -> int) -> ('v -> 'a -> 'v) -> 'k -> 'a -> ('k * 'v) list -> ('k * 'v) list
    (** [update_assoc f k v l] replaces the value [v'] associated to each occurrence of key [k] in
        the association list [l] by [f v' v]. If [k] does not occur, [l] is unchanged. As for {!assoc} and {!mem_assoc},
        the comparison function can be specified with the optional argument [cmp]. *)

val replace_assoc: ?cmp:('k -> 'k -> int) -> 'k -> 'v -> ('k * 'v) list -> ('k * 'v) list
    (** [replace_assoc k v l] is [update_assoc (fun _ v -> v) v k]. It replaces the value associated to each occurrence of
        key [k] by [v] in the association list [l]. If [k] does not occur, [l] is unchanged. As for {!assoc} and {!mem_assoc},
        the comparison function can be specified with the optional argument [cmp]. *)

val partition: ?cmp:('a -> 'a -> int) -> ('a * 'b) list -> ('a * 'b list) list
    (** [partition [(x1,y1);...;(xn;yn)]] returns the list [[k1,ys1; ...;km,ysm]] where [k1],...,[km] are
        all the distinct values occuring in [[x1;...;xn]] and [ysi] the list of values [yj] paired with [ki] in 
        the input list. For example [partition [(1,"a");(3,"b");(1,"c");(3,"d")]] is [[3,["d";"b"];1,["b";"a"]]]. *)

val scatter: ('a -> int) -> 'a list -> 'a list array
    (** [scatter h l] returns an array [a] in which [a.(i)] is the list of all elements [x] of [l] such that [h x = i].
        The length of the array is computed from the maximum value of [h] on [l].
        Example: [scatter String.length ["a";"bc";"de";"fghi"]] is [[|[];["a"];["bc;de"];[];["fghi"]|]].
        As for {!assoc} and {!mem_assoc}, the comparison function can be specified with the optional argument [cmp]. *)

val parse: string -> (Genlex.token Stream.t -> 'a) -> Genlex.token Stream.t -> 'a list
    (** Higher-order parser for lists *)
    
