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

module Expr = Fsm_expr.Int 

module type CONDITION = sig
  type t = 
    | Test of Expr.ident * string * Expr.t (* var, op, expr *)
  val to_string: t -> string
  val of_string: string -> t
  val list_of_string: string -> t list
  val eval: Expr.env -> t -> bool
end
   
module Condition = struct
  type t = 
    | Test of Expr.ident * string * Expr.t (* var, op, expr *)
  let to_string c = match c with
  | Test (v,op,e) -> v ^ op ^ Expr.to_string e
  exception Unknown_op of string
  let p_cond s = 
    let open Genlex in
    match Stream.next s with
    | Ident v ->
       begin match Stream.next s with
       | Kwd op when List.mem_assoc op Expr.test_ops -> let e = Expr.parse s in Test (v, op, e)
       | _ -> raise Stream.Failure 
       end
    | _ -> raise Stream.Failure
  let of_string s = p_cond (Expr.lexer s)                   (* BNF : <cond>   ::= ID <test_op> <exp> *)
  let list_of_string s = ListExt.parse ";" p_cond (Expr.lexer s)
  let lookup op =
    try List.assoc op Expr.test_ops
    with Not_found -> raise (Unknown_op op)
  let eval env texp = match texp with
  | Test (id, op, exp) -> (lookup op) (Expr.lookup env id) (Expr.eval env exp)
end

module type ACTION = sig
  type t =
  | Assign of Expr.ident * Expr.t        (* variable, value *)
  val to_string: t -> string
  val of_string: string -> t                   (* BNF : <act>   ::= ID ':=' <exp> *)
  val list_of_string: string -> t list
end

module Action = struct
  type t = 
    | Assign of Expr.ident * Expr.t        (* variable, value *)
  let to_string a = match a with
    | Assign (id, expr) -> id ^ ":=" ^ Expr.to_string expr
  let rec p_act s = match Stream.next s with
    | Genlex.Ident e1 -> p_act1 e1 s
    | _ -> raise Stream.Failure
  and p_act1 e1 s = match Stream.next s with
    | Genlex.Kwd ":=" -> let e2 = Expr.parse s in Assign (e1, e2)
    | _ -> raise Stream.Failure
  let of_string s = p_act (Expr.lexer s)
  let list_of_string s = ListExt.parse ";" p_act (Expr.lexer s)
end

module type TRANSITION = sig
  type t = Condition.t list * Action.t list
  val compare: t -> t -> int
  val to_string: t -> string
  val of_string: string*string -> t
end

module Transition = struct

  type t = Condition.t list * Action.t list

  let compare = Stdlib.compare

  let to_string (conds,acts) =
    let s1 = ListExt.to_string Condition.to_string ", " conds in
    let s2 = ListExt.to_string Action.to_string ", " acts in
    let l = String.make (Misc.max (String.length s1) (String.length s2)) '_' in
    if s2 <> ""
    then Printf.sprintf "%s\\n%s\\n%s" s1 l s2 
    else Printf.sprintf "%s" s1

  let of_string (conds,acts) = Condition.list_of_string conds, Action.list_of_string acts
end

module type T = sig 

  type state

  module Val : Valuation.T with type value = int (** for outputs and local variables *)

  type value = Val.value 

  type var_name = Val.name
  type var_domain = value list
  
  type var_desc = var_name * var_domain

  include Ltsa.T with type state := state and type label := Transition.t and type attr := Valuation.Int.t

  module M : Ltsa.T with type state = state and type label = Transition.t and type attr = Valuation.Int.t

  val create: 
      inps:var_desc list ->
      outps:var_desc list ->
      vars:var_desc list ->
      states:(state * Valuation.Int.t) list ->
      istate:string * state ->
      trans:(state * (string*string) * state) list ->
      t

  val empty: inps:var_desc list -> outps:var_desc list -> lvars:var_desc list -> t

  val add_state: state * Valuation.Int.t -> t -> t
  val add_transition: state * Transition.t * state -> t -> t
  val add_transition': state * (string*string) * state -> t -> t
  val add_itransition: Action.t list * state -> t -> t
  val add_itransition': string * state -> t -> t

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
end

module Make (S: Ltsa.STATE) = struct 

  module M = Ltsa.Make (S) (Transition) (Valuation.Int)

  type state = M.state
  type label = M.label
  type attr = M.attr

  module State = M.State
  module Label = Transition
  module States = M.States
  module Attr = Valuation.Int
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

  module Val = Valuation.Int

  type value = Val.value
  type var_name = Val.name
  type var_domain = value list
                  
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
  let add_transition' (q,(conds,acts),q') s =
    add_transition (q, Transition.of_string (conds,acts), q') s

  let add_itransition (acts,q) s =
      { s with lts = M.add_itransition (([],acts), q) s.lts }
  let add_itransition' (acts,q) s =
   add_itransition (Action.list_of_string acts, q) s

  let attr_of a q = M.attr_of a.lts q

  let create ~inps:ivs ~outps:ovs ~vars:lvs ~states:qs ~istate:(acts,q0) ~trans:ts =
    if not (List.mem q0 (List.map fst qs)) then failwith "Mealy.create: the initial state is not listed in the set of states";
    let add_states qs s = List.fold_left (fun s q -> add_state q s) s qs in
    let add_transitions ts s = List.fold_left (fun s (q,(conds,acts),q') -> add_transition' (q,(conds,acts),q') s) s ts in
    empty ~inps:ivs ~outps:ovs ~lvars:lvs |> add_states qs |> add_transitions ts |> add_itransition' (acts,q0)

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
      let string_of_var pfx (n,d) = pfx ^ n ^ " : {" ^ ListExt.to_string Val.string_of_value "," d ^ "}" in
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

  (* let dump a = M.dump a.lts *)
end
