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

(** {2 Finite State Machines}

    A FSM is a LTS
    - with an added set of local variables
    - for which state attributes may include output valuations 
    - for which transition labels are pairs of conditions and actions on inputs, outputs and local variables.

   *)

module Expr: Fsm_expr.T with type value=int

module type CONDITION = sig
  type t = 
    | Test of Expr.ident * string * Expr.t (* var, op, expr *)
  val to_string: t -> string
  val of_string: string -> t
  val list_of_string: string -> t list
  val eval: Expr.env -> t -> bool
end

module type ACTION = sig
  type t = 
  | Assign of Expr.ident * Expr.t          (* var, value *)
  val to_string: t -> string
  val of_string: string -> t
  val list_of_string: string -> t list
end

module Condition : CONDITION
module Action : ACTION

module type TRANSITION = sig
  type t = Condition.t list * Action.t list
  val compare: t -> t -> int
  val to_string: t -> string
  val of_string: string*string -> t
end

module Transition : TRANSITION

module type T = sig 

  type state

  module Val : Valuation.T with type value = int (** for outputs and local variables *)

  type value = Val.value 

  type var_name = Val.name
  type var_domain = value list
                  
  type var_desc = var_name * var_domain

  include Ltsa.T with type state := state and type label := Transition.t and type attr := Val.t

  module M : Ltsa.T with type state = state and type label = Transition.t and type attr = Val.t

  val create: 
      inps:var_desc list ->
      outps:var_desc list ->
      vars:var_desc list ->
      states:(state * Val.t) list ->
      istate:string * state ->
      trans:(state * (string*string) * state) list ->
      t
           (** [create ivs ovs lvs qs q0 ts] builds an FSM structure from
              - a list of input, output and local variables (each being described by a name and a domain)
              - a list of input and output identifiers
              - a list [qs] of states, with possible valuations of outputs
              - an initial state [q0] with the list of initial actions
              - a list of transitions [ts], where each transition is given as [(src_state,(conditions,actions),dst_state)]

              Raises [Not_found] if any specified condition or action involves identifiers not listed in [ivs], [ovs] or [lvs].
             *)

  val empty: inps:var_desc list -> outps:var_desc list -> lvars:var_desc list -> t
           (** [empty ivs ovs lvs] builds an "empty" FSM structure from
                a list of input, output and local variables (each being described by a name and a domain).
                This empty structure can then be "filled" using the {!add_state}, {!add_transition} and
                {!add_itransition} functions. 
             *)

  val add_state: state * Val.t -> t -> t
      (** [add_state (s,v) m] returns the FSM obtained by adding state [s], with a valuation of
          outputs [v], to FSM [m] *)

  val add_transition: state * Transition.t * state -> t -> t
      (** [add_transition t m] returns the FSM obtained by adding transition [t] to FSM [m] *)

  val add_transition': state * (string*string) * state -> t -> t
      (** [add_transition] is a variant of [add_transition] for which the added transition is specified
          using concrete syntax as a pair of strings  *)

  val add_itransition: Action.t list * state -> t -> t
      (** [add_itransition (acts,s) m] returns the FSM obtained by adding the initial transition [(acts,s)]
          to FSM [m] *)

  val add_itransition': string * state -> t -> t
      (** [add_itransition'] is a variant of [add_itransition] for which the initial actions are specified
          using concrete syntax as a string  *)

  val lts_of: t -> M.t
      (** Return the underlying representation of the Moore Machine as a LTS *)

  val istate: t -> state option
      (** Returns the initial state, when specified *)

  val inps: t -> var_desc list
      (** Returns the list of inputs variables, with corresponding domains *)

  val outps: t -> var_desc list
      (** Returns the list of outputs variables, with corresponding domains *)

  val vars: t -> var_desc list
      (** Returns the list of outputs variables, with corresponding domains *)

  val unwind: int -> t -> Tree.t
      (** [unwind depth s] unwind machine [s] to produce an execution tree (rooted at initial state)
          up to the specified depth. *)

  val dot_output: string
               -> ?fname:string
               -> ?options:Utils.Dot.graph_style list
               -> t
               -> unit
    (** [dot_output name fname s] writes a .dot representation of [s] with name [name] in file [fname].
        Global graph drawing options can be specified with the [options] optional argument. *)

  val dot_output_oc: string
               -> out_channel
               -> ?options:Utils.Dot.graph_style list
               -> t
               -> unit
    (** [dot_output_oc name oc s] is a variant of [dot_output] in which the description of [s]
        is written to the (previously opened) output channel [oc]. *)

end

module Make (S: Ltsa.STATE) : T with type state = S.t
