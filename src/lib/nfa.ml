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

module type SYMBOL = sig
  include Ltsa.LABEL
  val epsilon: t
end

module type T = sig
  
  type symbol
  type state

  include Ltsa.T with type label := symbol and type state := state and type attr := bool

  module LTSA : Ltsa.T with type state = state and type label = symbol and type attr = bool

  module Symbol : SYMBOL with type t = symbol

  module Symbols : Set.S with type elt = symbol

  val lts_of: t -> LTSA.t

  val empty: symbol list -> t
  val add_state: state * bool * bool -> t -> t

  val create:
      states:state list ->
      symbols:symbol list ->
      istate:state ->
      trans:(state * symbol * state list) list ->
      astates:state list ->
      t

  val symbols: t -> Symbols.t
  val symbols': t -> symbol list
  val acc_states: t -> States.t
  val acc_states': t -> state list
  val istate: t -> state option

  val string_of_symbol: symbol -> string

  val is_acc_state: t -> state -> bool

  val trans: t -> state -> symbol -> States.t
  val trans': t -> state -> symbol -> state list

  val trans_hat: t -> state -> symbol list -> States.t
  val trans_hat': t -> state -> symbol list -> state list

  val accept: t -> symbol list -> bool

  val is_in_cycle: t -> state  -> bool

  val totalize: t -> state -> t

  val unwind: int -> t -> LTSA.Tree.t

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

module Bool = struct
  type t = bool
  let compare = compare
  let to_string = string_of_bool
end

module Make (S : Ltsa.STATE) (L : SYMBOL) = 
struct

  module LTSA = Ltsa.Make (S) (L) (Bool)

  type state = LTSA.state (* = S.t *)
  type label = LTSA.label (* = E.t *)
  type symbol = L.t
  type attr = bool

  module State = S
  module Symbol = L
  module Label = Symbol
  module Symbols = Set.Make(L)
  module States = LTSA.States
  module Attr = Bool
  module Attrs = Map.Make(struct type t = state let compare = compare end)
  
  module A = struct
    type node = state
    type edge = symbol
    let string_of_node = State.to_string 
    let string_of_edge = Symbol.to_string 
  end

  module Tree = Tree.Make(A)

  type transition = LTSA.transition
                  
  type itransition = LTSA.itransition

  type t = {
      symbols: Symbols.t;                          (* SymbolSet *)
      lts: LTSA.t;                                 (* Internal representation, as a LTS *)
      istate: state option;                        (* Initial state, cached here for convenience *)
      astates: States.t                            (* Accepting states, cached here for convenience *)
    }

  let lts_of s = s.lts
(*   let of_repr s = s *)

  let empty ss  =  { symbols = Symbols.of_list ss; lts = LTSA.empty; istate = None; astates = States.empty }

  let is_state s q = LTSA.is_state s.lts q
  let istate s = s.istate
  let istates s = match istate s with None -> States.empty | Some q -> States.singleton q
  let istates' s = States.elements (istates s)
  let is_transition s t = LTSA.is_transition s.lts t

  let add_state (q,i,a) s =
      if is_state s q then s
      else
        let lts' = LTSA.add_state (q,a) s.lts in
        let lts'' = if i then LTSA.add_itransition (L.epsilon,q) lts' else lts' in
        { s with lts = lts'';
                 istate = begin match i, s.istate with
                          | false, _ -> s.istate
                          | true, None -> Some q
                          | true, Some _ -> raise (LTSA.Invalid_state q)
                          end;
                 astates = if a then States.add q s.astates else s.astates }
          

  let remove_state q s = { s with lts = LTSA.remove_state q s.lts; astates = States.remove q s.astates }

  exception Invalid_state of state

  let add_transition t s = { s with lts = LTSA.add_transition t s.lts }
  let add_itransition _ s = failwith "Nfa.add_itransition: illegal operation"

  let attr_of a q = LTSA.attr_of a.lts q

  let split_transition (q,s,qs') = List.map (function q' -> (q,s,q')) qs'
      (* Turn a multi (non-determ) transition into a list of transitions *)

  let create ~states:qs ~symbols:ss ~istate:q0 ~trans:ts ~astates:fs = 
    if not (List.mem q0 qs) then failwith "Nfa.create : the initial state is not listed in the set of states";
    { symbols = Symbols.of_list ss;
      lts = LTSA.create
        ~states:(List.map (function q -> q, List.mem q fs) qs)
        ~itrans:[L.epsilon,q0]
        ~trans:(ListExt.flat_map split_transition ts);
      istate = Some q0;                    
      astates = States.of_list fs } 

  let states s = LTSA.states s.lts
  let states' s = LTSA.states' s.lts
  let transitions s =  LTSA.transitions s.lts
  let itransitions s =  LTSA.itransitions s.lts
  let symbols s = s.symbols
  let symbols' s = Symbols.elements s.symbols
  let acc_states a = a.astates
  let acc_states' a = States.elements a.astates

  let string_of_state = LTSA.string_of_state
  let string_of_label = LTSA.string_of_label
  let string_of_attr = LTSA.string_of_attr
  let string_of_symbol = L.to_string
 
  let is_init_state s q = LTSA.is_init_state s.lts q
  let is_acc_state a q = LTSA.attr_of a.lts q 

  let iter_states f s = LTSA.iter_states f s.lts
  let fold_states f s z = LTSA.fold_states f s.lts z

  let iter_transitions f s = LTSA.iter_transitions f s.lts
  let fold_transitions f s z = LTSA.fold_transitions f s.lts z

  let iter_itransitions f s = LTSA.iter_itransitions f s.lts
  let fold_itransitions f s z = LTSA.fold_itransitions f s.lts z

  let fold_succs s q f z = LTSA.fold_succs s.lts q f z
  let fold_preds s q f z = LTSA.fold_preds s.lts q f z
  let iter_succs s q f = LTSA.iter_succs s.lts q f
  let iter_preds s q f = LTSA.iter_preds s.lts q f

  let succs s q = LTSA.succs s.lts q
  let preds s q = LTSA.preds s.lts q
  let succs' s q = LTSA.succs' s.lts q
  let preds' s q = LTSA.preds' s.lts q
  let succs_hat s q = LTSA.succs_hat s.lts q
  let preds_hat s q = LTSA.preds_hat s.lts q

  let clean s =
    let lts' = LTSA.clean s.lts in
    { s with lts = lts';
             astates = States.filter (function q -> LTSA.is_state lts' q) s.astates }

  let unwind depth a =
    match LTSA.unwind depth a.lts with
      [t] -> t
    | _ -> failwith "Nfa.unwind" (* should not happen *)

  let is_reachable s q = LTSA.is_reachable s.lts q

  let trans a q s =
    LTSA.fold_transitions
      (fun (q',s',q'') acc -> if S.compare q' q = 0 && L.compare s' s = 0 then States.add q'' acc else acc)
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
      
  let accept a ss = match istate a with
    Some q0 -> not (States.is_empty (States.inter (trans_hat a q0 ss) a.astates))
  | None -> failwith "Nfa.accept: no initial state to start from"

  exception Cycle

  let rec dfs a src visited tosee = 
    if States.is_empty tosee
    then false
    else
      begin
        let v, vs = States.extract tosee in
        if States.mem v visited then dfs a src visited vs 
        else
          let vs' = succs a v in
          if States.mem src vs' then true
          else dfs a src (States.add v visited) (States.union vs vs)
      end

  let is_in_cycle a q = dfs a q States.empty (States.singleton q)

  let totalize a qz =
    if is_state a qz then invalid_arg "Nfa.totalize: the added state must not belong to the existing set of states";
    let a' = add_state (qz,false,true) a in
    let add_sink_transition q _ a = 
      (* Add a transition [(q,s,qz)] in [a] whenever [delta(q,s)] is empty *)
      Symbols.fold
        (fun s a ->
          if States.is_empty (trans a q s)
          then add_transition (q,s,qz) a
          else a)
        a.symbols
        a in
    fold_states add_sink_transition a a' 

  let dot_acc_states a =
    List.map (fun s -> s, {Dot.node_shape="doublecircle"; Dot.node_style="solid"}) (States.elements a.astates)

  let dot_output name ?(fname="") ?(options=[]) a =
    LTSA.dot_output name ~fname:fname ~options:(Ltsa.NoAttr::options) ~marked_states:(dot_acc_states a) a.lts

  let dot_output_oc name oc ?(options=[]) a =
    LTSA.dot_output_oc name oc ~options:(Ltsa.NoAttr::options) ~marked_states:(dot_acc_states a) a.lts

  let tex_output name ?(fname="") ?(listed_transitions=None) a = 
    LTSA.tex_output name ~fname:fname ~listed_transitions:listed_transitions a.lts

  let dot_output_execs name ?(fname="") ?(options=[]) depth a =
    LTSA.dot_output_execs name ~fname:fname ~options:options depth a.lts

end

module Trans (S1: T) (S2: T) =
struct
  module F = Ltsa.Trans (S1.LTSA) (S2.LTSA)
  let map fq fs s1 =
    let add_state q is_acc s2 = S2.add_state (fq q, S1.is_init_state s1 q, is_acc) s2 in
    let add_transition s2 (q,s,q') = S2.add_transition (fq q, fs s, fq q') s2 in
    (S2.empty (List.map fs (S1.symbols' s1)))
    |> (S1.fold_states add_state s1)
    |> (function s2 -> List.fold_left add_transition s2 (S1.transitions s1))
end

module Product (S1: T) (S2: T with type symbol = S1.symbol and type Symbols.t = S1.Symbols.t) =
struct
  (* The product of two NFAs is the synchronized product of the corresponding LTS in which the
     sync relation R is defined by $(s1,s2) \in R iff s1=s2$ *)
  module S = OrderedTypeExt.Pair (S1.State) (S2.State)
  module L = S1.Symbol
  module R = Make(S)(L)
  include R
  module P = Ltsa.Product (S1.LTSA) (S2.LTSA)
  let product s1 s2 =
    let sync =
      if S1.Symbols.equal (S1.symbols s1) (S2.symbols s2)
      then List.map (fun s -> Some s, Some s) (S1.symbols' s1)
      else invalid_arg "Nfa.product: the two automata must share the same alphabet" in
    let single = function (Some s,_) -> s | _ -> failwith "Nfa.product" in
    let p = P.synchronized_product sync (S1.lts_of s1) (S2.lts_of s2) in
    let add_state q (f1,f2) r = R.add_state (q,false,f1&&f2) r in
    let add_transition (q,s,q') r = R.add_transition (q, single s, q') r in
    R.empty (List.map single sync)
    |> (P.fold_states add_state p)
    |> (P.fold_transitions add_transition p)
end
