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

(** Mealy machines.

    A Mealy machine M is a LTS for which transition labels are pairs of input and output variable boolean valuations.

   *)

module type TRANSITION = sig
  type t = Valuation.Bool.t * Valuation.Bool.t
  val compare: t -> t -> int
  val to_string: t -> string
end

module Transition : TRANSITION

module type T = sig 

  type state

  include Ltsa.T with type label := Transition.t and type state := state and type attr := unit

  module M : Ltsa.T with type state = state and type label = Transition.t and type attr = unit

  exception Invalid_transition of transition

  val create: 
      inps:Valuation.Bool.name list ->
      outps:Valuation.Bool.name list ->
      states:state list ->
      istate:state ->
      trans:(state * Transition.t * state) list ->
      t
           (** [create ivs ovs ss is ts] builds an Mealy structure from
              - a list of input and output variables
              - a list [ss] of states
              - an initial state [is]
              - a list of transitions [ts], where each transition is given as
                [(src_state,(input_valuation,output_valuation),dst_state)]

              Raises [!Valuation.Invalid_valuation] if any specified valuation/assignement is incorrect ({i i.e.} involves
              identifiers not listed in [ivs] or [ovs] or incomplete ({i i.e.} lacks a input or output identifier). 
             *)

                               
  val empty: inps:Valuation.Bool.name list -> outps:Valuation.Bool.name list -> t
    (** [empty ivs ovs] builds an empty Mealy structure from a list of input and output variables. *)

  val add_state: state -> t -> t
    (** [add_state q s] adds state [q] to the Mealy structure [s] *)

  val remove_state: state -> t -> t
    (** [remove_state q s] removes state [q] from the Mealy structure [s] *)

  val add_transition: state * Transition.t * state -> t -> t
    (** [add_transition (q,(iv,os),q') s] adds a transition from state [q] to state [q'], labeled with the IO
        valuation [(iv,ov)] to the Mealy structure [s] *)

  val add_itransition: state -> t -> t
    (** [add_itransition q s] adds an initial transition to state [q] to the Mealy structure [s] *)

  val lts_of: t -> M.t
      (** Return the underlying representation of the Moore Machine as a LTS *)

  val istate: t -> state option
      (** Returns the initial state, when specified *)
  val inps: t -> Valuation.Bool.name list
    (** Returns the list of inputs variables *)
  val outps: t -> Valuation.Bool.name list
    (** Returns the list of outputs variables *)

  val trans: t -> state -> Transition.t -> States.t
      (** [trans a q t] returns the set of states [q'] such that [(q,t,q')]
         belongs to the transition relation of [a] *)
  val trans': t -> state -> Transition.t -> state list
      (** [trans'] is like [trans] but returns a list *)

  val unwind: int -> t -> M.Tree.t
      (** [unwind depth s] unwind machine [s] to produce an execution tree (rooted at initial state)
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

module Make (S: Lts.STATE) : T with type state = S.t
