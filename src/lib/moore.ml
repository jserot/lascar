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
     
module BVal =
  Valuation.Make(
      struct
        type t = bool
        let compare = Stdlib.compare
        let to_string = function true -> "1" | false -> "0"
      end)

module type T = sig 

  type state

  include Ltsa.T with type label := BVal.t and type state := state and type attr := BVal.t

  module M : Ltsa.T with type state = state and type label = BVal.t and type attr = BVal.t

  exception Invalid_transition of transition

  val create: 
      inps:BVal.name list ->
      outps:BVal.name list ->
      states:(state * BVal.t) list ->
      istate:state ->
      trans:(state * BVal.t * state) list ->
      t

  val empty: inps:BVal.name list -> outps:BVal.name list -> t

  val add_state: state * BVal.t -> t -> t
  val add_transition: state * BVal.t * state -> t -> t
  val add_itransition: state -> t -> t

  val lts_of: t -> M.t

  val istate: t -> state option
  val inps: t -> BVal.name list
  val outps: t -> BVal.name list

  val trans: t -> state -> BVal.t -> States.t
  val trans': t -> state -> BVal.t -> state list

  val unwind: int -> t -> M.Tree.t

  val dot_output: string
               -> ?fname:string
               -> ?options:Dot.graph_style list
               -> t
               -> unit

  val dot_output_oc: string
               -> out_channel
               -> ?options:Dot.graph_style list
               -> t
               -> unit

end

module Make (S: Ltsa.STATE) = struct 

  module M = Ltsa.Make (S) (BVal) (BVal)

  module Tree =
    Tree.Make(
        struct
          type node = S.t
          type edge = BVal.t
          let string_of_node = S.to_string 
          let string_of_edge = BVal.to_string 
        end)

  type state = M.state
  type label = M.label
  type attr = BVal.t

  module State = S
  module Label = BVal
  module States = M.States
  module Attr = BVal
  module Attrs = Map.Make(struct type t = state let compare = compare end)
  
  type transition = M.transition
  type itransition = M.itransition

  type t = {
      lts: M.t;                                    (* Internal representation, as a LTS *)
      ivs: BVal.name list;                          (* Input variables *)
      ovs: BVal.name list;                          (* Output variables *)
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
    |  _ -> failwith "Moore: more than one initial state"
  let is_transition s t = M.is_transition s.lts t
  let inps s = s.ivs
  let outps s = s.ovs

  exception Invalid_state of state
  exception Invalid_transition of transition

  let add_state (q,ov) s =
    BVal.check s.ovs ov;
    { s with lts = M.add_state (q, ov) s.lts }

  let remove_state q s = 
    { s with lts = M.remove_state q s.lts }

  let add_transition (q,iv,q') s =
    BVal.check s.ivs iv;
    { s with lts = M.add_transition (q, iv, q') s.lts }

  let add_itransition q s =
      { s with lts = M.add_itransition ([], q) s.lts }

  let attr_of a q = M.attr_of a.lts q

  let create ~inps:ivs ~outps:ovs ~states:qs ~istate:q0 ~trans:ts =
    if not (List.mem q0 (List.map fst qs)) then failwith "Moore.create: the initial state is not listed in the set of states";
    let add_states qs s = List.fold_left (fun s q -> add_state q s) s qs in
    let add_transitions ts s = List.fold_left (fun s t -> add_transition t s) s ts in
    empty ~inps:ivs ~outps:ovs |> add_states qs |> add_transitions ts |> add_itransition q0

  let states s = M.states s.lts
  let states' s = M.states' s.lts
  let transitions s =  M.transitions s.lts
  let itransitions s =  M.itransitions s.lts

  let string_of_state = M.string_of_state
  let string_of_label = M.string_of_label
  let string_of_attr = M.string_of_attr
 
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
    | _ -> failwith "Moore.unwind" (* should not happen *)

  let is_reachable s q = M.is_reachable s.lts q

  let trans a q s =
    M.fold_transitions
      (fun (q',s',q'') acc -> if S.compare q' q = 0 && BVal.compare s' s = 0 then States.add q'' acc else acc)
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
        ListExt.to_string (string_of_var "In: ") "\\n" a.ivs ^ "\\n"
      ^ ListExt.to_string (string_of_var "Out: ") "\\n" a.ovs  in
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
