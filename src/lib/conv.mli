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

(** {2 Functors for converting various kinds of Labeled Transition Systems} *)

(** Functor for converting a {!Ltsa} into a {!Lts} (by removing state attributes) *)
module ToLts (M: Ltsa.T) : sig

  include Lts.T with type state = M.state and type label = M.label

  val conv: M.t -> t
end

(** Functor for converting a {!Lts} into a {!Ltsa} (by adding [unit] state attributes) *)
module FromLts (M: Lts.T) : sig

  include Ltsa.T with type state = M.state and type label = M.label and type attr = unit

  val conv: M.t -> t
end

(** Functor for converting a {!Nfa} to an equivalent {!Dfa} (determinisation) *)
module ToDfa (N : Nfa.T) : sig

  include Dfa.T with type state = N.States.t and type symbol = N.symbol
        (** Each state of a resulting DFA is a subset of states of the NFA *)

  val conv: N.t -> t
end

(** Functor for converting a {!Moore} machine into an equivalent {!Mealy} one *)
module ToMealy (MM: Moore.T) : sig

  include Mealy.T with type state = MM.state

  val conv: MM.t -> t
    (** Convert a Moore automata into a Mealy automata, by turning 
       - turning each state [(q,o)] into [q]
       - turning each transitions [((q,o)/i/(q',o')] into [q,(i/o'),q']

    *)
end

(** Functor for converting a {!Mealy} machine into an equivalent {!Moore} one *)
module ToMoore (ME: Mealy.T) : sig

  include Moore.T with type state = ME.state * Valuation.Bool.t

  val conv: ?init:state option -> ?clean:bool -> ME.t -> t
    (** Convert a Mealy automata into a Moore automata, by turning each transition
       [q,(i/o'),q'] into a set of transitions [(q,o),i,(q',o')] for each possible output valuation [o].
       If [init] is not specified, all states [(q,o)] where [q] is an init state of the Mealy structure and [o]
       an output configuration are marked as init states. Otherwise, the designated state is used.
       Unreachable states are removed from the resulting automata unlesse the optional argument [clean]
       is set to false *)
end

(** Functor for transforming FSMs *)
module Fsm(F: Fsm.T) : sig

  include Fsm.T with type state = F.state * F.Valuation.t

  val defactorize: ?init:(Transition.Action.t list * state) option -> ?clean:bool -> Valuation.name list -> F.t -> t
(** [defactorize vars m] returns an equivalent FSM [m'] obtained by
          - removing variable listed in [vars] from [m] (all variables if [vars=[]])
          - introducing new states.
          - the optional argument [init] can be used to designate the initial state and actions of the resulting
            automata (when the operation leads to several initial states)
          - unreachable states are removed from the resulting automata unlesse the optional
            argument [clean] is set to false
       *)
(*           Formally, if [m] is [(Q,I,O,V,R)] then [m'] is [(Q',I,O,V',R')] where *)
(*           - Q' = Q x Domain(v) *)
(*           - V' = V - {v} *)
(*           - R' = U_{(t \in R}{defact(t)} *)
(*           - defact(q,(conds,acts),q') = { (q,u),(conds\{v},acts\{v}),(q',u') *)
(*                                         | u \in \Khi_v(conds), u' \in \Phi_v(acts,\Khi_v(conds)} *)
(*           - \Khi_v(conds) is the restriction of Domain(v) to the values compatibles with conditions [conds] *)
(*             (ex: If domain(v) = {0,1,2}, \Khi_v("v<2") = {0,1}) *)
(*           - \Phi_v(acts, D) is the "image" of a domain D by the actions [acts] *)
(*             (ex: \Phi_v("v:=v+1",{0,1}) = {1,2} *\) *)

end
