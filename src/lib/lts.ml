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

open Utils

module type STATE = OrderedTypeExt.T

module type LABEL = OrderedTypeExt.T

module type ATTR =  Ltsa.ATTR

module type T = sig

  type state (** The type of state identifiers *)
  type label (** The type of transition labels *)

  module Repr: Ltsa.T with type state = state and type label = label and type attr = unit
     (** The underlying representation : a LTS for which state attributes have type [unit]. *)

  type t = Repr.t 

  val repr_of: t -> Repr.t
  val of_repr: Repr.t -> t

  type transition = state * label * state
        (** The type for transition. A transition is a triplet [(s1,l,s2)], where [s1] is the 
            source state, [s2] the destination state and [l] the transition label *)
  type itransition = label * state
        (** The type for initial transition. An initial transition is a pair [(l,s)], where
            [s] is the destination state and [l] the transition label *)

  module State : STATE with type t = state
  module Label : LABEL with type t = label
  module States : Set.S with type elt = state

  val states: t -> States.t
      (** Returns the set of states *)
  val states': t -> state list
      (** Returns the list of states *)
  val istates: t -> States.t
      (** Returns the set of initial states *)
  val istates': t -> state list
      (** Returns the list of initial states *)
  val transitions: t -> transition list
      (** Returns the list of transitions *)
  val itransitions: t -> itransition list
      (** Returns the list of initial transitions *)

  val string_of_state: state -> string
    (** A synonym of {!State.to_string} *)
  val string_of_label: label -> string
    (** A synonym of {!Label.to_string} *)
  val string_of_transition: transition -> string

  (** {6 Inspectors} *)

  val is_state: t -> state -> bool
      (** [is_state s q] returns true iff [q] is a state in [s] *)
  val is_init_state: t -> state -> bool
      (** [is_init s q] returns true iff [q] is an initial state in [s] *)
  val is_reachable: t -> state -> bool
      (** [is_reachable s q] returns true iff [q] is a reachable state in [s], {i i.e.} if it 
          can be reached from an initial state using the transitio relation. *)
  val is_transition: t -> transition -> bool
      (** [is_transition t q] returns true iff [t] is a transition in [s] *)

  val succs: t -> state -> States.t
    (** [succs s q] returns the set of immediate successors in [s], {i i.e.} the set of state [q'] such that 
        there exists a transition [(q,l,q')] in [R].
        Raise Invalid_argument if [q] is not in [s]. *)
  val succs': t -> state -> (state * label) list
    (** [succs' s q] returns the list of immediate successors, with the associated transition label, of state [q] in [s].
        Raise Invalid_argument if [q] is not in [s]. *)

  val preds: t -> state -> States.t
    (** [preds s q] returns the set of immediate predecessors of state [q] in [s], {i i.e.} the set of state [q'] such that 
        there exists a transition [(q',l,q)] in [R].
        Raise Invalid_argument if [q] is not in [s]. *)
  val preds': t -> state -> (state * label) list
    (** [preds' s q] returns the list of immediate predecessors, with the associated transition label, of state [q] in [s].
        Raise Invalid_argument if [q] is not in [s]. *)
  val succs_hat: t -> state -> States.t
    (** Transitive closure of [succs]. 
        [succs_hat s q] returns all the successors (immediate or not) of [q] in [s] *)
  val preds_hat: t -> state -> States.t
    (** Transitive closure of [preds]. 
        [preds_hat s q] returns all the predecessors (immediate or not) of [q] in [s] *)

  (** {6 Building functions} *)

  val empty: t
      (** The empty LTS (no state, no transition) *)

  val create: states:state list -> itrans:(label * state) list -> trans:(state * label * state) list -> t
  (** [create qs q0s ts] builds a LTS from
     - a list of states identifiers
     - a list of initial transitions, where each transition is given as [(label,dst_state)]
     - a list of transitions [ts], where each transition is given as [(src_state,label,dst_state)] *)

  val add_state: state -> t -> t
      (** [add_state q s] returns the LTS obtained by adding state [q] to [s].
         Returns [s] is [q] is already a state in [s] *)

  exception Invalid_state of state

  val add_transition: state * label * state -> t -> t
      (** [add_transition (q1,l,q2) s] returns the LTS obtained by adding transition from state [q1]
          to state [q2], with label [l] to LTS [s].
          Raises [Invalid_state] if [q1] or [q2] are not states of [s] *)
  
  val add_itransition: label * state -> t -> t
      (** [add_itransition (l,q) s] returns the LTS obtained by adding initial transition to state [q],
         with label [l] to LTS [s].
         Raises [Invalid_state] if [q] are is not a state of [s] *)

  val remove_state: state -> t -> t
      (** [remove_state q s] returns the LTS obtained by removing state [q], and all attached transitions, from [s].
         Raises [Invalid_state] is [q] is not a state in [s] *)
  
  (** {6 Global iterators} *)

  val iter_states: (state -> unit) -> t -> unit
    (** [iter_states f s] applies function [f] to all states (with associated attribute) of [s] *)
  val fold_states: (state -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold_states f s z] computes [f xN ... (f x2 (f x1 z))...], where [x1], ..., [xN] are all the states of [s] *)
  val iter_transitions: (transition -> unit) -> t -> unit
    (** [iter_transitions f s] applies function [f] to all transitions of [s] *)
  val fold_transitions: (transition -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold_transitions f s z] computes [f xN ... (f x2 (f x1 z))...], where [x1], ..., [xN] are all the transitions of [s] *)
  val iter_itransitions: (itransition -> unit) -> t -> unit
    (** [iter_itransitions f s] applies function [f] to all initial transitions of [s] *)
  val fold_itransitions: (itransition -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold_itransitions f s z] computes [f xN ... (f x2 (f x1 z))...], where [x1], ..., [xN] are all the initial transitions of [s] *)

  (** {6 State iterators} *)

  val fold_succs: t -> state -> (state -> label -> 'a -> 'a) -> 'a -> 'a
    (** [fold_succs s x f z] computes [f xN lN ... (f x2 (f x1 l1 z) l2)...], where [x1], ..., [xN] are all the
       successors of state [x] in LTS [s], and [l1], ..., [lN] the associated transitions labels *)
  val iter_succs: t -> state -> (state -> label -> unit) -> unit
    (** [iter_succs s x f z] computes [f x1 l1; ... ;f xN lN], where [x1], ..., [xN] are all the
       successors of state [x] in LTS [s], and [l1], ..., [lN] the associated transitions labels *)
  val fold_preds: t -> state -> (state -> label -> 'a -> 'a) -> 'a -> 'a
    (** [fold_preds s x f z] computes [f xN lN ... (f x2 (f x1 l1 z) l2)...], where [x1], ..., [xN] are all the
       predecessors of state [x] in LTS [s], and [l1], ..., [lN] the associated transitions labels *)
  val iter_preds: t -> state -> (state -> label -> unit) -> unit
    (** [iter_preds s x f z] computes [f x1 l1; ... ;f xN lN], where [x1], ..., [xN] are all the
       predecessors of state [x] in LTS [s], and [l1], ..., [lN] the associated transitions labels *)

  (** {6 Global transformations} *)

  val map_state: (state -> state) -> t -> t
  val map_label: (label -> label) -> t -> t
  val map_transition: (state option * label * state -> state option * label * state) -> t -> t
  val clean: t -> t

  (** {6 Output functions} *)

  val dot_output: string
               -> ?fname:string
               -> ?options:Dot.graph_style list
               -> ?marked_states:(state * Dot.node_style) list
               -> ?extra_nodes:(string * Dot.node_style) list
               -> ?implicit_transitions:transition list
               -> t
               -> unit

  val dot_output_oc: string
               -> out_channel
               -> ?options:Dot.graph_style list
               -> ?marked_states:(state * Dot.node_style) list
               -> ?extra_nodes:(string * Dot.node_style) list
               -> ?implicit_transitions:transition list
               -> t
               -> unit

  val dot_output_execs: string -> ?fname:string -> ?options:Dot.graph_style list -> int -> t -> unit

  val tex_output: string -> ?fname:string -> ?listed_transitions:label list option -> t -> unit
        
end

module Make (S: Ltsa.STATE) (L: Ltsa.LABEL) = 
struct
  
  module Attr = struct
    type t = unit
    let compare = compare
    let to_string _ = ""
  end

  module Repr = Ltsa.Make(S)(L)(Attr)

  type state = Repr.state (* = S.t *)
  type label = Repr.label (* = E.t *)
  type attr = Attr.t

  module State = Repr.State
  module Label = Repr.Label
  module States = Repr.States
  module Attrs = Map.Make(struct type t = state let compare = compare end) (* Not used *)
  
  type transition = Repr.transition
  type itransition = Repr.itransition

  type t = Repr.t 
     (* We keep the same internal repr. It's just that attr=unit here *)

  let repr_of s = s
  let of_repr s = s

  let empty = Repr.empty

  let add_state q s = Repr.add_state (q,()) s

  exception Invalid_state of state

  let remove_state q s =
    try Repr.remove_state q s
    with Repr.Invalid_state s -> raise (Invalid_state s)

  let add_transition ((q,l,q') as t) s =
    try Repr.add_transition t s
    with Repr.Invalid_state s -> raise (Invalid_state s)

  let add_itransition t s =
    try Repr.add_itransition t s
    with Repr.Invalid_state s -> raise (Invalid_state s)

  let attr_of s q = ()

  let create ~states:qs ~itrans:ts0 ~trans:ts =
    empty |> (function s -> List.fold_left (Fun.flip add_state) s qs)
          |> (function s -> List.fold_left (Fun.flip add_transition) s ts)
          |> (function s -> List.fold_left (Fun.flip add_itransition) s ts0)

  let states s = Repr.states s
  let istates s = Repr.istates s
  let states' s = List.map fst (Repr.states' s)
(*   let attrs s = Attrs.empty *)
  let istates' s = Repr.istates' s
  let transitions s =  Repr.transitions s
  let itransitions s =  Repr.itransitions s

  let string_of_state = Repr.string_of_state
  let string_of_label = Repr.string_of_label
  let string_of_attr _ = ""
  let string_of_transition (q,l,q') = "(" ^ string_of_state q ^ "," ^ string_of_label l ^ "," ^ string_of_state q' ^ ")"
 
  let is_state = Repr.is_state
  let is_init_state = Repr.is_init_state
  let is_transition = Repr.is_transition

  let iter_states f s = Repr.iter_states (fun q _ -> f q) s
  let fold_states f s z = Repr.fold_states (fun q _ acc -> f q acc) s z

  let iter_transitions f s = Repr.iter_transitions f s
  let fold_transitions f s z = Repr.fold_transitions f s z

  let iter_itransitions f s = Repr.iter_itransitions f s
  let fold_itransitions f s z = Repr.fold_itransitions f s z

  let fold_succs s q f z = Repr.fold_succs s q f z
  let fold_preds s q f z = Repr.fold_preds s q f z
  let iter_succs s q f = Repr.iter_succs s q f
  let iter_preds s q f = Repr.iter_preds s q f

  let succs s q = Repr.succs s q
  let preds s q = Repr.preds s q
  let succs' s q = Repr.succs' s q
  let preds' s q = Repr.preds' s q
  let succs_hat s q = Repr.succs_hat s q
  let preds_hat s q = Repr.preds_hat s q

  let map_state = Repr.map_state
  let map_label = Repr.map_label
  let map_transition = Repr.map_transition
  let clean = Repr.clean

  let is_reachable = Repr.is_reachable

  let dot_output = Repr.dot_output
  let dot_output_oc = Repr.dot_output_oc
    
  let dot_output_execs = Repr.dot_output_execs
    
  let tex_output = Repr.tex_output

end

module Trans (S1: T) (S2: T) =
struct
  module F = Ltsa.Trans (S1.Repr) (S2.Repr)
  let map fs fl s1 = S2.of_repr (F.map fs fl (fun _ -> ()) (S1.repr_of s1))
end

module Product
  (S1: T)
  (S2: T) =
struct
  module S = OrderedTypeExt.Pair (S1.State) (S2.State)
  module L = OrderedTypeExt.Either (S1.Label) (S2.Label)
  module R = Make(S)(L)
  include R
  module P = Ltsa.Product (S1.Repr) (S2.Repr)
  let synch_product sync s1 s2 = 
    let p = P.synch_product sync (S1.repr_of s1) (S2.repr_of s2) in
    let add_states s = P.fold_states (fun q _ acc -> R.add_state q acc) p s in
    let add_transitions s = P.fold_transitions (fun t acc -> R.add_transition t acc) p s in
    let add_itransitions s = P.fold_itransitions (fun t acc -> R.add_itransition t acc) p s in
    R.empty |> add_states |> add_transitions |> add_itransitions
  let synchronized_product sync_set = synch_product (function l -> List.mem l sync_set)
  let free_product = synch_product (function _ -> true)
  let asynchronous_product = synch_product (function (Some _, None) | (None, Some _)-> true | _ -> false)
  let synchronous_product = synch_product (function (Some _, Some _) -> true | _ -> false)
end

module Product3 (S1: T) (S2: T) (S3: T) =
struct
  module S = OrderedTypeExt.Triplet (S1.State) (S2.State) (S3.State)
  module L = OrderedTypeExt.Either3 (S1.Label) (S2.Label) (S3.Label)
  module R = Make(S)(L)
  include R
  module P = Ltsa.Product3 (S1.Repr) (S2.Repr) (S3.Repr)
  let synch_product sync s1 s2 s3 = 
    let p = P.synch_product sync (S1.repr_of s1) (S2.repr_of s2) (S3.repr_of s3) in
    let add_states s = P.fold_states (fun q _ acc -> R.add_state q acc) p s in
    let add_transitions s = P.fold_transitions (fun t acc -> R.add_transition t acc) p s in
    let add_itransitions s = P.fold_itransitions (fun t acc -> R.add_itransition t acc) p s in
    R.empty |> add_states |> add_transitions |> add_itransitions
  let synchronized_product sync_set = synch_product (function l -> List.mem l sync_set)
  let free_product = synch_product (function _ -> true)
  let asynchronous_product = synch_product (function (Some _, None, None) | (None, Some _, None) | (None, None, Some _) -> true | _ -> false)
  let synchronous_product = synch_product (function (Some _, Some _, Some _) -> true | _ -> false)
end

module type Merge = sig
  type state
  type label
  val merge_state: state * state -> state
  val merge_label: label option * label option -> label
end

module IProduct (S: T) (M: Merge with type state=S.state and type label=S.label) = struct
  include S
  module P = Product(S)(S) 
  module U = Trans(P)(S)
  let tr = U.map M.merge_state M.merge_label
  let free_product s1 s2 = tr (P.free_product s1 s2)
  let synch_product sync s1 s2 = tr (P.synch_product sync s1 s2)
  let synchronized_product sync s1 s2 = tr (P.synchronized_product sync s1 s2)
  let asynchronous_product s1 s2 = tr (P.asynchronous_product s1 s2)
  let synchronous_product s1 s2 = tr (P.synchronous_product s1 s2)
end

module type Merge3 = sig
  type state
  type label
  val merge_state: state * state * state -> state
  val merge_label: label option * label option * label option -> label
end
  
module IProduct3 (S: T) (M: Merge3 with type state=S.state and type label=S.label) = struct
  include S
  module P = Product3(S)(S)(S)
  module U = Trans(P)(S)
  let tr = U.map M.merge_state M.merge_label
  let free_product s1 s2 s3 = tr (P.free_product s1 s2 s3)
  let synch_product sync s1 s2 s3 = tr (P.synch_product sync s1 s2 s3)
  let synchronized_product sync s1 s2 s3 = tr (P.synchronized_product sync s1 s2 s3)
  let asynchronous_product s1 s2 s3 = tr (P.asynchronous_product s1 s2 s3)
  let synchronous_product s1 s2 s3 = tr (P.synchronous_product s1 s2 s3)
end
