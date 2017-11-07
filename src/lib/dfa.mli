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

(** Deterministic Finite Automata (DFA)
 
    A DFA is just a NFA in which there's at most one transition with a given symbol from any state.
    This condition will be enforced by the [create] and [add_transition] functions.
    *)

open Utils

module type T = sig

  include Nfa.T

  module NFA: Nfa.T with type state = state and type symbol = symbol

  exception Non_deterministic

  val create:
      states:state list ->
      symbols:symbol list ->
      istate:state ->
      trans:(state * symbol * state) list ->
      astates:state list ->
      t
          (** [create qs ss q0 rel aqs] builds a NFA from
             - a list of states [qs]
             - a list of symbols (alphabet) [ss]
             - an initial state [q0] 
             - a transition relation [rel] given as a list of triplets [(src_state,symbol,dst_state)]
             - a list of accepting states [aqs]
              
           Raises [Failure] if [q0] does not appear in [qs].
           *)


  exception Stuck of state * symbol list

  val trans: t -> state -> symbol -> state
      (** [trans a q s] returns, it it exists, the state [q'] such that [(q,s,q')] occurs in the transition relation.
          Raises [Stuck] otherwise. *)

  val trans_hat: t -> state -> symbol list -> state
      (** [trans_hat a q ss] is the transitive generalisation of [trans] : it returns the state which is reached
         when a sequence of symbols [ss] is given to [a], starting at state [q], or raises [Stuck] *)

  val nfa_of: t -> NFA.t  (** For internal use *)
  val of_nfa: NFA.t -> t  (** For internal use *)

end

(** Functor building an implementation of the DFA structure given an implementation of
    state identifiers and symbols *)
module Make (S : Ltsa.STATE) (L : Nfa.SYMBOL) : T
  with type state = S.t
   and type symbol = L.t

(** Functor for converting a DFA, with a given implementation of state identifiers and symbols
   into another one with different respective implementations *)
module Trans (S1: T) (S2: T) :
sig
  val map: (S1.state -> S2.state) -> (S1.symbol -> S2.symbol) -> S1.t -> S2.t
end

(** Functor for computing the products of two NFAs sharing the same symbol set *)
module Product (S1: T) (S2: T with type symbol = S1.symbol and type Symbols.t = S1.Symbols.t and type NFA.Symbols.t = S1.NFA.Symbols.t) :
sig
  include T with type state = S1.state * S2.state and type symbol = S1.symbol
  val product: S1.t -> S2.t -> t
end
