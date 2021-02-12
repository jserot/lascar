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

(** {2 Non-deterministic Finite Automata (NFA)}

    A NFA is just a Labeled Transition System (LTSA) for which
           - transition labels are taken from an input {i alphabet}
           - the set of initial states is reduced to a singleton
           - state attribute is boolean indicating whether the state is accepting or not

    *)

open Utils

module type SYMBOL = sig
  include Ltsa.LABEL
  val epsilon: t  (** The empty symbol *)
end

module type T = sig
  
  type symbol
  type state

  include Ltsa.T with type label := symbol and type state := state and type attr := bool

  module LTSA : Ltsa.T with type state = state and type label = symbol and type attr = bool

  module Symbol : SYMBOL with type t = symbol

  module Symbols : Set.S with type elt = symbol

  val lts_of: t -> LTSA.t
      (** Return the underlying representation of the NFA as a LTS *)

  val empty: symbol list -> t
      (** Creates an empty NFA (no states, no transitions) with a given symbol set. *)

  val add_state: state * bool * bool -> t -> t
      (** [add_state (q,i,f) a] returns the NFA obtained by adding state [q] to [s].
          [i] and [f] resp. indicates whether [q] is an initial and/or an accepting state.
          Raises [!Ltsa.Invalid_state] if [i] is [true] and there's already an accepting state in [a].
          Returns [a] is [q] is already a state in [a] *)

  val create:
      states:state list ->
      symbols:symbol list ->
      istate:state ->
      trans:(state * symbol * state list) list ->
      astates:state list ->
      t
          (** [create qs ss q0 rel aqs] builds a NFA from
             - a list of states [qs]
             - a list of symbols (alphabet) [ss]
             - an initial state [q0] 
             - a transition relation [rel] given as a list of triplets [(src_state,symbol,dst_states)]
             - a list of accepting states [aqs]
              
           Raises [Failure] if [q0] does not appear in [qs].
           *)

  val symbols: t -> Symbols.t
      (** Returns the set of symbols *)

  val symbols': t -> symbol list
      (** Returns the set of symbols as a list *)

  val acc_states: t -> States.t
      (** Returns the set of accepting states *)

  val acc_states': t -> state list
      (** Returns the set of accepting states as a list *)

  val istate: t -> state option
      (** Returns the initial state, when specified *)

  val string_of_symbol: symbol -> string

  val is_acc_state: t -> state -> bool

  (** {b TODO} : [union] and [star] operations *)

  val trans: t -> state -> symbol -> States.t
      (** [trans a q s] returns the set of states [q'] such that [(q,s,q')]
         belongs to the transition relation of [a] *)

  val trans': t -> state -> symbol -> state list
      (** [trans'] is like [trans] but returns a list *)


  val trans_hat: t -> state -> symbol list -> States.t
      (** [trans_hat a q ss] is the transitive generalisation of [trans].
         It returns the set of states which can reached when a sequence of symbols [ss] is given to [a],
         starting at state [q] *)

  val trans_hat': t -> state -> symbol list -> state list
      (** [trans_hat'] is like [trans_hat] but returns a list *)

  val accept: t -> symbol list -> bool
      (** [accept a ss] returns true iff [ss] is accepted by [a].
          {i I.e.}, if [trans_hat a q ss] contains at least one accepting state.
          Raises [Failure] is no initial state has been specified in [a]. *)

  val is_in_cycle: t -> state  -> bool
      (** [cycles a q] tests whether state [q] in automata [a] belongs to a cycle *)

  val totalize: t -> state -> t
      (** [totalize a q] returns a "totalized" version of automaton [a] by adding an extra "sink" state [q].
          Raises [Invalid_arg] if [q] is already a state of [a]. *)

  val unwind: int -> t -> LTSA.Tree.t
      (** [unwind depth s] unwinds LTS system [s] to produce an execution tree (rooted at initial state)
          up to the specified depth. *)

  val dot_output: string
               -> ?fname:string
               -> ?options:Dot.graph_style list
               -> t
               -> unit
    (** [dot_output name fname s] writes a .dot representation of [s] with name [name] in file [fname].
        Global graph drawing options can be specified with the [options] optional argument. *)

  val dot_output_oc: string
               -> out_channel
               -> ?options:Dot.graph_style list
               -> t
               -> unit
    (** [dot_output_oc name oc s] is a variant of [dot_output] in which the description of [s]
        is written to the (previously opened) output channel [oc]. *)

end

(** Functor building an implementation of the NFA structure given an implementation of
    state identifiers and symbols *)
module Make (S : Ltsa.STATE) (L : SYMBOL) : T
  with type state = S.t
   and type symbol = L.t

(** Functor for converting a NFA, with a given implementation of state identifiers and symbols
   into another one with different respective implementations *)
module Trans (S1: T) (S2: T) :
sig
  val map: (S1.state -> S2.state) -> (S1.symbol -> S2.symbol) -> S1.t -> S2.t
end

(** Functor for computing the products of two NFAs sharing the same symbol set *)
module Product (S1: T) (S2: T with type symbol = S1.symbol and type Symbols.t = S1.Symbols.t) :
sig
  include T with type state = S1.state * S2.state and type symbol = S1.symbol
  val product: S1.t -> S2.t -> t
end
