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

module type T = sig 

  type state

  module Value: Fsm_value.T

  module Expr: Fsm_expr.T with type value = Value.t

  module Transition: Fsm_transition.T with module Expr = Expr and type Condition.Expr.value = Value.t and type Action.Expr.value = Value.t
       
  module Valuation : Valuation.T with type value = Value.t (** for outputs and local variables *)

  type var_name = Valuation.name
  type var_domain = Value.t list
                  
  type var_desc = var_name * var_domain

  include Ltsa.T with type state := state and type label := Transition.t and type attr := Valuation.t

  module M : Ltsa.T with type state = state and type label = Transition.t and type attr = Valuation.t

  val create: 
      inps:var_desc list ->
      outps:var_desc list ->
      vars:var_desc list ->
      states:(state * Valuation.t) list ->
      istate: Transition.Action.t list * state ->
      trans: (state * Transition.t * state) list ->
      t

  val empty: inps:var_desc list -> outps:var_desc list -> lvars:var_desc list -> t

  val add_state: state * Valuation.t -> t -> t
  val add_transition: state * Transition.t * state -> t -> t
  val add_itransition: Transition.Action.t list * state -> t -> t

  val lts_of: t -> M.t

  val istate: t -> state option
  val inps: t -> var_desc list
  val outps: t -> var_desc list
  val vars: t -> var_desc list

  val unwind: int -> t -> Tree.t

  val dot_output: string
               -> ?fname:string
               -> ?options:Utils.Dot.graph_style list
               -> t
               -> unit

  val dot_output_oc: string
               -> out_channel
               -> ?options:Utils.Dot.graph_style list
               -> t
               -> unit

  val mk_cond: string -> Transition.Condition.t
  val mk_conds: string -> Transition.Condition.t list
  val mk_act: string -> Transition.Action.t
  val mk_acts: string -> Transition.Action.t list
  val mk_trans: string -> Transition.t

end

module Make (S: Ltsa.STATE) (V: Fsm_value.T) = struct 

  module Value = V

  module Expr = Fsm_expr.Make(V)
              
  module Transition = Fsm_transition.Make(Expr)
                    
  module Valuation = Valuation.Make(V)

  module M = Ltsa.Make (S) (Transition) (Valuation)

  type state = M.state
  type label = M.label
  type attr = M.attr

  module State = M.State
  module Label = Transition
  module States = M.States
  module Attr = Valuation
  module Attrs = Map.Make(struct type t = state let compare = compare end)

  module Tree =
    Tree.Make(
        struct
          type node = S.t
          type edge = Transition.t
          let string_of_node = S.to_string 
          let string_of_edge = Transition.to_string 
        end)
  
  type transition = M.transition
  type itransition = M.itransition

  type var_name = Valuation.name
  type var_domain = Value.t list
                  
  type var_desc = var_name * var_domain

  type t = {
      lts: M.t;                                    (* Internal representation, as a LTS *)
      ivs: var_desc list;                            (* Input variables *)
      ovs: var_desc list;                            (* Output variables *)
      lvs: var_desc list;                            (* Local variables *)
    }

  let lts_of s = s.lts

  let empty ~inps:ivars ~outps:ovars ~lvars:lvars = { lts = M.empty; ivs = ivars; ovs = ovars; lvs = lvars }

  let is_state s q = M.is_state s.lts q
  let istates s = M.istates s.lts
  let istates' s = M.istates' s.lts
  let istate s =
    match istates' s with
      [] -> None
    | [q] -> Some q
    |  _ -> failwith "Mealy: more than one initial state"
  let inps s = s.ivs
  let outps s = s.ovs
  let vars s = s.lvs

  let add_state (q,ov) s =
    { s with lts = M.add_state (q, ov) s.lts }

  let remove_state q s =
    { s with lts = M.remove_state q s.lts }

  exception Invalid_state of state

  let add_transition (q,(conds,acts),q') s =
    { s with lts = M.add_transition (q, (conds,acts), q') s.lts }
  (* let add_transition' (q,(conds,acts),q') s =
   *   add_transition (q, Transition.of_string (conds,acts), q') s *)

  let add_itransition (acts,q) s =
      { s with lts = M.add_itransition (([],acts), q) s.lts }
  (* let add_itransition' (acts,q) s =
   *  add_itransition (Transition.Action.list_of_string acts, q) s *)

  let attr_of a q = M.attr_of a.lts q

  let create ~inps:ivs ~outps:ovs ~vars:lvs ~states:qs ~istate:(acts,q0) ~trans:ts =
    if not (List.mem q0 (List.map fst qs)) then failwith "Mealy.create: the initial state is not listed in the set of states";
    let add_states qs s = List.fold_left (fun s q -> add_state q s) s qs in
    let add_transitions ts s = List.fold_left (fun s (q,(conds,acts),q') -> add_transition (q,(conds,acts),q') s) s ts in
    empty ~inps:ivs ~outps:ovs ~lvars:lvs |> add_states qs |> add_transitions ts |> add_itransition (acts,q0)

  let states s = M.states s.lts
  let states' s = M.states' s.lts
  let transitions s =  M.transitions s.lts
  let itransitions s =  M.itransitions s.lts

  let string_of_state = M.string_of_state
  let string_of_label = M.string_of_label
  let string_of_attr = M.string_of_attr
 
  let is_init_state s q = M.is_init_state s.lts q
  let is_transition s t = M.is_transition s.lts t

  let iter_states f s = M.iter_states f s.lts
  let fold_states f s z = M.fold_states f s.lts z

  let iter_transitions f s = M.iter_transitions f s.lts
  let fold_transitions f s z = M.fold_transitions f s.lts z

  let iter_itransitions f s = M.iter_itransitions f s.lts
  let fold_itransitions f s z = M.fold_itransitions f s.lts z

  let fold_succs s q f z = M.fold_succs s.lts q f z
  let fold_preds s q f z = M.fold_preds s.lts q f z
  let iter_succs s q f = M.iter_succs s.lts q f
  let iter_preds s q f = M.iter_preds s.lts q f

  let succs s q = M.succs s.lts q
  let preds s q = M.preds s.lts q
  let succs' s q = M.succs' s.lts q
  let preds' s q = M.preds' s.lts q
  let succs_hat s q = M.succs_hat s.lts q
  let preds_hat s q = M.preds_hat s.lts q

  let map_state f s = { s with lts = M.map_state f s.lts }
  let map_attr f s = { s with lts = M.map_attr f s.lts }
  let map_state_attr f s = { s with lts = M.map_state_attr f s.lts }
  let map_label f s = { s with lts = M.map_label f s.lts }
  let map_transition f s = { s with lts = M.map_transition f s.lts }

  let clean s = { s with lts = M.clean s.lts }

  let unwind depth a = failwith "Fsm.unwind: not implemented"

  let is_reachable s q = M.is_reachable s.lts q

  let var_node a =
      let string_of_var pfx (n,d) = pfx ^ n ^ " : {" ^ ListExt.to_string Valuation.string_of_value "," d ^ "}" in
      let txt = StringExt.concat_sep "\\n"
        [ListExt.to_string (string_of_var "In: ") "\\n" a.ivs;
         ListExt.to_string (string_of_var "Out: ") "\\n" a.ovs;
         ListExt.to_string (string_of_var "Var: ") "\\n" a.lvs]  in
      txt, { Dot.node_shape = "rect"; Dot.node_style = "regular" }

  let dot_output name ?(fname="") ?(options=[]) a =
    M.dot_output name ~fname:fname ~options:options ~extra_nodes:([var_node a]) a.lts

  let dot_output_oc name oc ?(options=[]) a =
    M.dot_output_oc name oc ~options:options ~extra_nodes:([var_node a]) a.lts

  let tex_output name ?(fname="") ?(listed_transitions=None) a = 
    M.tex_output name ~fname:fname ~listed_transitions:listed_transitions a.lts

  let dot_output_execs name ?(fname="") ?(options=[]) depth a =
    M.dot_output_execs name ~fname:fname ~options:options depth a.lts

  let mk_lexer keywords s = s |> Expr.mk_unaries |> Stream.of_string |> Genlex.make_lexer keywords
  let mk_cond s = Transition.Condition.of_string s
  let mk_act s = Transition.Action.of_string s
  let mk_conds s =
    let lexer = mk_lexer (Transition.Condition.keywords @ [","]) in
    s |> lexer |> Parsing.separated_list "," Transition.Condition.parse
  let mk_acts s =
    let lexer = mk_lexer (Transition.Action.keywords @ [","]) in
    s |> lexer |> Parsing.separated_list "," Transition.Action.parse
  let mk_trans s = Transition.of_string s

end

module Trans (S1: T) (S2: T) =
struct
  let map fs fv s1 =
    let mk_iov (id,v) = id, List.map fv v in
    let mk_val vs = List.map (fun (n,v) -> n, fv v) vs in
    let add_states s = S1.fold_states (fun q ov acc -> S2.add_state (fs q, mk_val ov) acc) s1 s in
    let mk_trans t =
      let module F = Fsm_transition.Trans(S1.Transition)(S2.Transition) in
      F.map fv t in
    let add_transitions s = S1.fold_transitions (fun (q,t,q') acc -> S2.add_transition (fs q, mk_trans t, fs q') acc) s1 s in
    let add_itransitions s = S1.fold_itransitions (fun (t,q') acc -> S2.add_itransition (snd (mk_trans t), fs q') acc) s1 s in
    S2.empty
      ~inps:(s1 |> S1.inps |> List.map mk_iov)
      ~outps:(s1 |> S1.outps |> List.map mk_iov)
      ~lvars:(s1 |> S1.vars |> List.map mk_iov)
      |> add_states |> add_transitions |> add_itransitions
end
