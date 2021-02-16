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
   
(** Moore machines.

    A Moore machine M is a LTSA for which
    - state attributes are output variable valuations
    - transition labels are input variable valuations

   *)

module type T = sig 

  type state

  include Ltsa.T with type label := Valuation.Bool.t and type state := state and type attr := Valuation.Bool.t

  module M : Ltsa.T with type state = state and type label = Valuation.Bool.t and type attr = Valuation.Bool.t

  exception Invalid_transition of transition

  val create: 
      inps: Valuation.Bool.name list ->
      outps: Valuation.Bool.name list ->
      states: (state * Valuation.Bool.t) list ->
      istate: state ->
      trans: (state * Valuation.Bool.t * state) list ->
      t
           (** [mk ivs ovs ss is ts] builds an Moore structure from
              - a list of input and output variables (each being described by a name and a domain)
              - a list [ss] of states, each state being assigned a valuation for outputs
              - an initial state [is]
              - a list of transitions [ts], where each transition is given as [(src_state,input_valuation,values,dst_state)]

              Raises [Invalid_valuation] when appropriate (TBD) *)

  val empty: inps:Valuation.Bool.name list -> outps:Valuation.Bool.name list -> t

  val add_state: state * Valuation.Bool.t -> t -> t
  val add_transition: state * Valuation.Bool.t * state -> t -> t
  val add_itransition: state -> t -> t

  val lts_of: t -> M.t
      (** Return the underlying representation of the Moore Machine as a LTS *)

  val istate: t -> state option
      (** Returns the initial state, when specified *)
  val inps: t -> Valuation.Bool.name list
      (** Returns the list of inputs variables *)
  val outps: t -> Valuation.Bool.name list
      (** Returns the list of outputs variables *)

  val trans: t -> state -> Valuation.Bool.t -> States.t
      (** [trans a q s] returns the set of states [q'] such that [(q,s,q')]
         belongs to the transition relation of [a] *)
  val trans': t -> state -> Valuation.Bool.t -> state list
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

module Make (S: Ltsa.STATE) : T with type state = S.t
