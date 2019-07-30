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

(** Labeled Transition Systems with State Attributes

    A Labeled Transition System with Attributes on states (LTSA) is 6-uple [(Q,L,A,I,a,R)] where
    - [Q] is a set of state identifiers
    - [L] is a set of transition labels
    - [A] is a set of state attributes
    - [I] is a subset of [Q] giving the initial states
    - [a] is function from [Q] to [A] associating a state to its attribute
    - [R] is the transition relation, defined as a subset of [QxLxQ]

    *)

   
open Utils
   
(** Extension to the {!Dot.graph_style} type for drawing LTSAs *)
type Dot.graph_style +=
     | Circular   (** Circular layout (circo); default is layered *)
     | NoAttr     (** Do not draw state attributes *)
     | NoLabel    (** Do not draw transition label *)
   
(** Input signatures of the functor {!Ltsa.Make}. *)

module type STATE = OrderedTypeExt.T
      (** Signature of the module describing state identifiers*)

module type LABEL = OrderedTypeExt.T
      (** Signature of the module describing transition labels *)

module type ATTR = sig
  type t
  val to_string: t -> string
end
      (** Signature of the module describing state attributes *)

(** Output signature of the functor {!Ltsa.Make}. *)
module type T = sig

  type state (** The type of state identifiers *)
  type label (** The type of transition labels *)
  type attr  (** The type of state attributes *)

  type transition = state * label * state
        (** The type for transition. A transition is a triplet [(s1,l,s2)], where [s1] is the 
            source state, [s2] the destination state and [l] the transition label *)
  type itransition = label * state
        (** The type for initial transition. An initial transition is a pair [(l,s)], where
            [s] is the destination state and [l] the transition label *)

  type t (** The type of Labeled Transition Systems with state attributes *)

  module State : STATE with type t = state
  module Label : LABEL with type t = label
  module Attr : ATTR with type t = attr
  module States : SetExt.S with type elt = state
  module Attrs : Map.S with type key = state
  module Tree : Tree.S with type node = state and type edge = label

  val states: t -> States.t
      (** Returns the set of states *)
  val states': t -> (state * attr) list
      (** Returns the set of states, with attached attribute as a assocation list *)
  val istates: t -> States.t
      (** Returns the set of initial states *)
  val istates': t -> state list
      (** Returns the set of initial states as a list *)
  val transitions: t -> transition list
      (** Returns the list of transitions *)
  val itransitions: t -> itransition list
      (** Returns the list of initial transitions *)

  val string_of_state: state -> string
    (** A synonym of {!State.to_string} *)
  val string_of_label: label -> string
    (** A synonym of {!Label.to_string} *)
  val string_of_attr: attr -> string
    (** A synonym of {!Attr.to_string} *)

  (** {1 Inspectors} *)

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

  val attr_of: t -> state -> attr
      (** [attr_of s q] returns the attribute of state [q] in [s].
          Raise [Not_found] if there is no state [q] in [s] *)

  (** {1 Building functions} *)

  val empty: t
      (** The empty LTSA (no state, no transition) *)

  val create: states:(state * attr) list -> itrans:(label * state) list -> trans:(state * label * state) list -> t
  (** [create qs q0s ts] builds a LTSA from
     - a list of states identifiers, each with an attached attribute
     - a list of initial transitions, where each transition is given as [(label,dst_state)]
     - a list of transitions [ts], where each transition is given as [(src_state,label,dst_state)] *)

  val add_state: state * attr -> t -> t
      (** [add_state (q,a) s] returns the LTSA obtained by adding state [q], with attribute [a], to [s].
         Returns [s] is [q] is already a state in [s] *)

  exception Invalid_state of state

  val add_transition: state * label * state -> t -> t
      (** [add_transition (q1,l,q2) s] returns the LTSA obtained by adding transition from state [q1]
          to state [q2], with label [l] to LTSA [s].
          Raises [Invalid_state] if [q1] or [q2] are not states of [s] *)
  
  val add_itransition: label * state -> t -> t
      (** [add_itransition (l,q) s] returns the LTSA obtained by adding initial transition to state [q],
         with label [l] to LTSA [s].
         Raises [Invalid_state] if [q] are is not a state of [s] *)
  
  val remove_state: state -> t -> t
      (** [remove_state q s] returns the LTSA obtained by removing state [q], and all attached transitions, from [s].
         Raises [Invalid_state] is [q] is not a state in [s] *)
  
  (** {1 Global iterators} *)

  val iter_states: (state -> attr -> unit) -> t -> unit
    (** [iter_states f s] applies function [f] to all states (with associated attribute) of [s] *)
  val fold_states: (state-> attr -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold_states f s z] computes [f xN ... (f x2 (f x1 z))...], where [x1], ..., [xN] are all the states of [s] *)
  val iter_transitions: (transition -> unit) -> t -> unit
    (** [iter_transitions f s] applies function [f] to all transitions of [s] *)
  val fold_transitions: (transition -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold_transitions f s z] computes [f xN ... (f x2 (f x1 z))...], where [x1], ..., [xN] are all the transitions of [s] *)
  val iter_itransitions: (itransition -> unit) -> t -> unit
    (** [iter_itransitions f s] applies function [f] to all initial transitions of [s] *)
  val fold_itransitions: (itransition -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold_itransitions f s z] computes [f xN ... (f x2 (f x1 z))...], where [x1], ..., [xN] are all the initial transitions of [s] *)

  (** {1 State iterators} *)

  val fold_succs: t -> state -> (state -> label -> 'a -> 'a) -> 'a -> 'a
    (** [fold_succs s x f z] computes [f xN lN ... (f x2 (f x1 l1 z) l2)...], where [x1], ..., [xN] are all the
       successors of state [x] in LTSA [s], and [l1], ..., [lN] the associated transitions labels *)
  val iter_succs: t -> state -> (state -> label -> unit) -> unit
    (** [iter_succs s x f z] computes [f x1 l1; ... ;f xN lN], where [x1], ..., [xN] are all the
       successors of state [x] in LTSA [s], and [l1], ..., [lN] the associated transitions labels *)
  val fold_preds: t -> state -> (state -> label -> 'a -> 'a) -> 'a -> 'a
    (** [fold_preds s x f z] computes [f xN lN ... (f x2 (f x1 l1 z) l2)...], where [x1], ..., [xN] are all the
       predecessors of state [x] in LTSA [s], and [l1], ..., [lN] the associated transitions labels *)
  val iter_preds: t -> state -> (state -> label -> unit) -> unit
    (** [iter_preds s x f z] computes [f x1 l1; ... ;f xN lN], where [x1], ..., [xN] are all the
       predecessors of state [x] in LTSA [s], and [l1], ..., [lN] the associated transitions labels *)

  (** {1 Global transformations} *)

  val map_state: (state -> state) -> t -> t
      (** [map_state f s] returns the LTSA obtained by replacing each state [q] by [f q] in [s].
          Result is undefined if [f] is not injective. *)

  val map_attr: (attr -> attr) -> t -> t
      (** [map_attr f s] returns the LTSA obtained by replacing each state attribute [a] by [f a] in [s]. *)

  val map_label: (label -> label) -> t -> t
      (** [map_label f s] returns the LTSA obtained by replacing each transition label [l] by [f l] in [s]. *)

  val clean: t -> t
      (** Removes unreachable nodes and associated transitions *)

  val unwind: int -> t -> Tree.t list
      (** [unwind depth s] unwinds LTSA system [s] to produce a list of execution trees (rooted at
         initial states) up to the specified depth *)

  (** {1 Output functions} *)

  val dot_output: string
               -> ?fname:string
               -> ?options:Dot.graph_style list
               -> ?marked_states:(state * Dot.node_style) list
               -> ?extra_nodes:(string * Dot.node_style) list
               -> ?implicit_transitions:transition list
               -> t
               -> unit
    (** [dot_output name s] writes a .dot representation of [s] with name [name].
        The name of the output file is [name.dot] or specified with the [fname] optional argument.
        States listed with the optional argument [marked_states] will be drawn with the specified style.
        Extra nodes, to be added to the generated graph, can be specified with the [extra_nodes] optional argument.
        Transitions listed in the [implicit_transitions] optional argument will {e not} be drawn. *)

  val dot_output_oc: string
               -> out_channel
               -> ?options:Dot.graph_style list
               -> ?marked_states:(state * Dot.node_style) list
               -> ?extra_nodes:(string * Dot.node_style) list
               -> ?implicit_transitions:transition list
               -> t
               -> unit
    (** [dot_output_coc name oc s] is a variant of [dot_output] in which the description of [s]
        is written to the (previously opened) output channel [oc]. *)

  val dot_output_execs: string -> ?fname:string -> ?options:Dot.graph_style list -> int -> t -> unit
    (** [dot_output_execs name depth s] writes a .dot representation, with name [name]
        of the execution trees obtained by calling [unwind depth s].
        The name of the file is [name.dot] or specified with the [fname] optional argument.
        Drawing options can be specified with the [options] optional argument. *)

  val tex_output: string -> ?fname:string -> ?listed_transitions:label list option -> t -> unit
    (** [tex_output name fname s] writes a .tex representation of [s] with name [name].
        The name of the output file is [name.dot] or specified with the [fname] optional argument.
        When the optional argument [listed_transitions] is [Some l], only transitions listed in [l] are written, otherwise 
        all transitions of [s] are written. *)
end

(** Functor building an implementation of the LTSA structure given an implementation of
   state identifiers, transition labels and state attributes *)

module Make (S: STATE) (L: LABEL) (A: ATTR) : T with type state = S.t and type label = L.t and type attr = A.t

(** Functor for converting a LTSA, with a given implementation of state identifiers, transition labels and state attributes
   into another one with different respective implementations *)

module Trans (S1: T) (S2: T) :
sig
  val map: (S1.state -> S2.state) -> (S1.label -> S2.label) -> (S1.attr -> S2.attr) -> S1.t -> S2.t
end

(** Functor for computing the external product, in different flavors, of two LTSA *)
module Product (S1: T) (S2: T) :
sig
  include T with
        type state = S1.state * S2.state
    and type label = S1.label option * S2.label option
    and type attr = S1.attr * S2.attr
  val free_product: S1.t -> S2.t -> t
    (** [free_product s1 s2] computes the free product of the two LTSA [s1] and [s2].
        If [s1=<Q1,L1,A1,a1,I1,R1>] and [s2=<Q2,L2,A2,a2,I2,R2>], then the [free_product s1 s2] is [<Q,L,A,a,I,R>] where
        {ul {- [Q = Q1xQ2]}
        {- [L = L1 U L2 U L1xL2]}
        {- [A = A1xA2]}
        {- [a(q1,q2) = a1(q1),a2(q2)]}
        {- [I = I1xI2]}
        {- [R], subset of [QxLxQ], is
        [{((q1,q2),l1,(q1',q2)) | (q1,l1,q1') in R1, q2 in Q2} U {((q1,q2),l2,(q1,q2')) | q1 in Q1, (q2,l2,q2') in R2} U {((q1,q2),(l2,l2),(q1',q2')) | (q1,l1,q1') in R1, (q2,l2,q2') in R2}]}}. In the result, transition labels
        {ul {- [l1] are encoded as [(Some l1, None)]}
            {- [l2] as [(None, Some l2)]}
            {- [(l1,l2)] as [(Some l1, Some l2)]}}.
     *)

  val synch_product: (S1.label option * S2.label option -> bool) -> S1.t -> S2.t -> t
    (** [synch_product p s1 s2] computes the synchronized product of the two LTSA [s1] and [s2].
        The restricted product is the obtained by first computing the free product of [s1] and [s2] and then keeping
        only transitions [(q,l,q')] for which the transition label [l] satisfies the predicate [p]. *)

  val synchronized_product: (S1.label option * S2.label option) list -> S1.t -> S2.t -> t
    (** [synchronized_product sync s1 s2] is a variant of {!synch_product} in which the synchronisation function is given
        {e in extension} as a list of allowed transitions. *)

  val asynchronous_product: S1.t -> S2.t -> t
    (** [asynchronous_product s1 s2] computes the asynchronous product of the two LTSA [s1] and [s2].
        This is a [synchronized_product] in which the synchronisation list contains only labels [(Some l1,None)]
        or [(None,Some l2))]. *)

  val synchronous_product: S1.t -> S2.t -> t
    (** [synchronous_product p s1 s2] computes the synchronous product of the two LTSA [s1] and [s2].
        This is a [synchronized_product] in which the synchronisation list contains only labels [(Some l1,Some l2)]. *)
end

(** Functor for computing the external product, in different flavors, of three LTSA *)
module Product3 (S1: T) (S2: T) (S3: T):
sig
  include T with
        type state = S1.state * S2.state * S3.state
    and type label = S1.label option * S2.label option * S3.label option
    and type attr = S1.attr * S2.attr * S3.attr
  val free_product: S1.t -> S2.t -> S3.t -> t
    (** [free_product s1 s2] computes the free product of the three LTSA [s1], [s2] and [s3] *)
  val synch_product: (S1.label option * S2.label option * S3.label option -> bool) -> S1.t -> S2.t -> S3.t -> t
    (** [synch_product p s1 s2 s3] computes the synchronized product of the three LTSA [s1], [s2] and [s3] using the synchronisation
        predicate [p]. *)
  val synchronized_product: (S1.label option * S2.label option * S3.label option) list -> S1.t -> S2.t -> S3.t -> t
    (** [synchronized_product sync s1 s2] computes the synchronized product of the three LTSA [s1], [s2] and [s3] using the
        synchronization set [sync] *)
  val asynchronous_product: S1.t -> S2.t -> S3.t ->t
    (** [asynchronous_product s1 s2] computes the asynchronous product of the three LTSA [s1], [s2] and [s3] *)
  val synchronous_product: S1.t -> S2.t -> S3.t ->t
    (** [synchronous_product p s1 s2] computes the synchronous product of the three LTSA [s1], [s2] and [s3] *)
end

(** Signature of the second argument for the {!IProduct} functor *)
module type Merge = sig
  type state
  type label
  type attr
  val merge_state: state * state -> state
  val merge_label: label option * label option -> label
  val merge_attr: attr * attr -> attr
end
  
(** Functor for computing the internal product, in different flavors, of two LTSA *)
module IProduct (S: T) (M: Merge with type state=S.state and type label=S.label and type attr=S.attr) :
sig
  include T with
        type state = S.state
    and type label = S.label
    and type attr = S.attr
    and type t = S.t
  val free_product: t -> t -> t
    (** [free_product s1 s2] computes the free product of the two LTSA [s1] and [s2] *)
  val synch_product: (label option * label option -> bool) -> t -> t -> t
    (** [synch_product p s1 s2] computes the synchronized product of the two LTSA [s1] and [s2] using the synchronisation
        predicate [p]. *)
  val synchronized_product: (label option * label option) list -> t -> t -> t
    (** [synchronized_product sync s1 s2] computes the synchronized product of the two LTSA [s1] and [s2]using the
        synchronization set [sync] *)
  val asynchronous_product: t -> t -> t
    (** [asynchronous_product s1 s2] computes the asynchronous product of the two LTSA [s1] and [s2]*)
  val synchronous_product: t -> t -> t
    (** [synchronous_product p s1 s2] computes the synchronous product of the two LTSA [s1] and [s2]*)
end

(** Signature of the second argument for the {!IProduct3} functor *)
module type Merge3 = sig
  type state
  type label
  type attr
  val merge_state: state * state * state -> state
  val merge_label: label option * label option * label option -> label
  val merge_attr: attr * attr * attr -> attr
end
  
(** Functor for computing the "internal" product, in different flavors, of three LTSA *)
module IProduct3 (S: T) (M: Merge3 with type state=S.state and type label=S.label and type attr=S.attr) :
sig
  include T with
        type state = S.state
    and type label = S.label
    and type attr = S.attr
    and type t = S.t
  val free_product: t -> t -> t -> t
    (** [free_product s1 s2 s3] computes the free product of the three LTSA [s1], [s2] and [s3] *)
  val synch_product: (label option * label option * label option -> bool) -> t -> t -> t -> t
    (** [synch_product p s1 s2 s3] computes the synchronized product of the three LTSA [s1], [s2] and [s3] using the synchronisation
        predicate [p]. *)
  val synchronized_product: (label option * label option * label option) list -> t -> t -> t -> t
    (** [synchronized_product sync s1 s2] computes the synchronized product of the three LTSA [s1], [s2] and [s3] using the
        synchronization set [sync] *)
  val asynchronous_product: t -> t -> t ->t
    (** [asynchronous_product s1 s2] computes the asynchronous product of the three LTSA [s1], [s2] and [s3] *)
  val synchronous_product: t -> t -> t ->t
    (** [synchronous_product p s1 s2] computes the synchronous product of the three LTSA [s1], [s2] and [s3] *)
end

