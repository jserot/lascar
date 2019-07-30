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

module type TRANSITION = sig
  type t = Valuation.Bool.t * Valuation.Bool.t
  val compare: t -> t -> int
  val to_string: t -> string
end

module Transition = struct
  type t = Valuation.Bool.t * Valuation.Bool.t
  let compare (iv,ov) (iv',ov') =
    if Valuation.Bool.compare iv iv' = 0 && Valuation.Bool.compare ov ov' = 0 then 0 else -1
  let to_string (iv,ov) = Valuation.Bool.to_string iv ^ " / " ^ Valuation.Bool.to_string ov
end

open Utils
   
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

  val empty: inps:Valuation.Bool.name list -> outps:Valuation.Bool.name list -> t

  val add_state: state -> t -> t
  val remove_state: state -> t -> t
  val add_transition: state * Transition.t * state -> t -> t
  val add_itransition: state -> t -> t

  val lts_of: t -> M.t

  val istate: t -> state option
  val inps: t -> Valuation.Bool.name list
  val outps: t -> Valuation.Bool.name list

  val trans: t -> state -> Transition.t -> States.t
  val trans': t -> state -> Transition.t -> state list

  val unwind: int -> t -> M.Tree.t

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
end

module Make (S: Lts.STATE) = struct 

  module Attr = struct
    type t = unit
    let compare = compare
    let to_string _ = ""
  end

  module M = Ltsa.Make (S) (Transition) (Attr)

  module Tree =
    Tree.Make(
        struct
          type node = S.t
          type edge = Transition.t
          let string_of_node = S.to_string 
          let string_of_edge = Transition.to_string 
        end)


  type state = M.state
  type label = M.label
  type attr = Attr.t

  module State = S
  module Label = Transition
  module States = M.States
  module Attrs = Map.Make(struct type t = state let compare = compare end) (* Not used *)
  
  type transition = M.transition
  type itransition = M.itransition

  type t = {
      lts: M.t;                                    (* Internal representation, as a LTS *)
      ivs: Valuation.Bool.name list;                            (* Input variables *)
      ovs: Valuation.Bool.name list;                            (* Output variables *)
    }

  let lts_of s = s.lts

  let empty ~inps:ivars ~outps:ovars = { lts = M.empty; ivs = ivars; ovs = ovars }

  let is_state s q = M.is_state s.lts q
  let istates s = M.istates s.lts
  let istates' s = M.istates' s.lts
  let istate s =
    match istates' s with
      [] -> None
    | [q] -> Some q
    |  _ -> failwith "Mealy: more than one initial state"
  let is_transition s t = M.is_transition s.lts t
  let inps s = s.ivs
  let outps s = s.ovs

  exception Invalid_state of state
  exception Invalid_transition of transition

  let add_state q s =
    { s with lts = M.add_state (q,()) s.lts }

  let remove_state q s = 
    { s with lts = M.remove_state q s.lts }

  let add_transition ((q,(iv,ov),q') as t) s =
    try 
      Valuation.Bool.check s.ovs ov;
      Valuation.Bool.check s.ivs iv; 
      { s with lts = M.add_transition (q, (iv,ov), q') s.lts }
    with Valuation.Bool.Invalid_valuation _ ->
      raise (Invalid_transition t)

  let add_itransition q s =
      { s with lts = M.add_itransition (([],[]), q) s.lts }

  let attr_of a q = M.attr_of a.lts q

  (* exception Invalid_valuation of Valuation.Bool.t *)

  let create ~inps:ivs ~outps:ovs ~states:qs ~istate:q0 ~trans:ts =
    if not (List.mem q0 qs) then failwith "Mealy.create: the initial state is not listed in the set of states";
    let add_states qs s = List.fold_left (fun s q -> add_state q s) s qs in
    let add_transitions ts s = List.fold_left (fun s t -> add_transition t s) s ts in
    empty ~inps:ivs ~outps:ovs |> add_states qs |> add_transitions ts |> add_itransition q0

  let states s = M.states s.lts
  let states' s = M.states' s.lts
  let transitions s =  M.transitions s.lts
  let itransitions s =  M.itransitions s.lts

  let string_of_state = M.string_of_state
  let string_of_label = M.string_of_label
  let string_of_attr _ = ""
 
  let is_init_state s q = M.is_init_state s.lts q

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
  let map_label f s = { s with lts = M.map_label f s.lts }

  let clean s = { s with lts = M.clean s.lts }

  let unwind depth a =
    match M.unwind depth a.lts with
      [t] -> t
    | _ -> failwith "Mealy.unwind" (* should not happen *)

  let is_reachable s q = M.is_reachable s.lts q

  let trans a q t =
    M.fold_transitions
      (fun (q',t',q'') acc -> if S.compare q' q = 0 && Transition.compare t' t = 0 then States.add q'' acc else acc)
      a.lts
      States.empty

  let trans' a q s = States.elements (trans a q s)

  let trans_hat a q ss =
    let rec h qs ss = match ss with
      [] -> qs
    | s::ss' -> 
        let qs' = States.fold (fun q acc -> States.union (trans a q s) acc) qs qs in
        h qs' ss' in
    h (States.singleton q) ss

  let trans_hat' a q s = States.elements (trans_hat a q s)
      
  let var_node a =
      let string_of_var pfx n = pfx ^ n in
      let txt = 
        Utils.ListExt.to_string (string_of_var "In: ") "\\n" a.ivs ^ "\\n"
      ^ Utils.ListExt.to_string (string_of_var "Out: ") "\\n" a.ovs  in
      txt, { Dot.node_shape = "rect"; Dot.node_style = "regular" }

  let dot_output name ?(fname="") ?(options=[]) a =
    M.dot_output name ~fname:fname ~options:options ~extra_nodes:([var_node a]) a.lts

  let dot_output_oc name oc ?(options=[]) a =
    M.dot_output_oc name oc ~options:options ~extra_nodes:([var_node a]) a.lts

  let tex_output name ?(fname="") ?(listed_transitions=None) a = 
    M.tex_output name ~fname:fname ~listed_transitions:listed_transitions a.lts

  let dot_output_execs name ?(fname="") ?(options=[]) depth a =
    M.dot_output_execs name ~fname:fname ~options:options depth a.lts

  (* let dump a = M.dump a.lts *)
end

