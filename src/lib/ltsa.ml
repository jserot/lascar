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
   
type Dot.graph_style +=
     | Circular   (** Circular layout (circo); default is layered *)
     | NoAttr     (** Do not draw state attributes *)
     | NoLabel    (** Do not draw transition label *)

   
module type STATE = OrderedTypeExt.T

module type LABEL = OrderedTypeExt.T

module type ATTR = sig
  type t
  val to_string: t -> string
end

module type T = sig

  type state (** The type of state identifiers *)
  type label (** The type of transition labels *)
  type attr  (** The type of state attributes *)

  type transition = state * label * state

  type itransition = label * state

  type t (** The type of Labeled Transition Systems *)

  module State : STATE with type t = state
  module Label : LABEL with type t = label
  module Attr : ATTR with type t = attr
  module States : SetExt.S with type elt = state
  module Attrs : Map.S with type key = state
  module Tree : Tree.S with type node = state and type edge = label

  val states: t -> States.t
  val states': t -> (state * attr) list
  val istates: t -> States.t
  val istates': t -> state list
(*   val attrs: t -> Attr.t Attrs.t *)
  val transitions: t -> transition list
  val itransitions: t -> itransition list

  val string_of_state: state -> string
  val string_of_label: label -> string
  val string_of_attr: attr -> string

  val is_state: t -> state -> bool
  val is_init_state: t -> state -> bool
  val is_reachable: t -> state -> bool
  val is_transition: t -> transition -> bool

  val succs: t -> state -> States.t
  val succs': t -> state -> (state * label) list
  val preds: t -> state -> States.t
  val preds': t -> state -> (state * label) list
  val succs_hat: t -> state -> States.t
  val preds_hat: t -> state -> States.t

  val attr_of: t -> state -> attr

  val empty: t

  val create: states:(state * attr) list -> itrans:(label * state) list -> trans:(state * label * state) list -> t

  val add_state: state * attr -> t -> t

  exception Invalid_state of state

  val add_transition: state * label * state -> t -> t
  
  val add_itransition: label * state -> t -> t

  val remove_state: state -> t -> t
  
  val iter_states: (state -> attr -> unit) -> t -> unit
  val fold_states: (state-> attr -> 'a -> 'a) -> t -> 'a -> 'a
  val iter_transitions: (transition -> unit) -> t -> unit
  val fold_transitions: (transition -> 'a -> 'a) -> t -> 'a -> 'a
  val iter_itransitions: (itransition -> unit) -> t -> unit
  val fold_itransitions: (itransition -> 'a -> 'a) -> t -> 'a -> 'a


  val fold_succs: t -> state -> (state -> label -> 'a -> 'a) -> 'a -> 'a
  val iter_succs: t -> state -> (state -> label -> unit) -> unit
  val fold_preds: t -> state -> (state -> label -> 'a -> 'a) -> 'a -> 'a
  val iter_preds: t -> state -> (state -> label -> unit) -> unit

  val clean: t -> t
  val unwind: int -> t -> Tree.t list

  val dot_output: string
               -> ?fname:string
               -> ?options:Dot.graph_style list
               -> ?marked_states:(state * Dot.node_style) list
               -> ?extra_nodes:(string * Dot.node_style) list
               -> ?implicit_transitions:transition list
               -> t
               -> unit

  val dot_output_oc: string
               -> out_channel
               -> ?options:Dot.graph_style list
               -> ?marked_states:(state * Dot.node_style) list
               -> ?extra_nodes:(string * Dot.node_style) list
               -> ?implicit_transitions:transition list
               -> t
               -> unit

  val dot_output_execs: string -> ?fname:string -> ?options:Dot.graph_style list -> int -> t -> unit

  val tex_output: string -> ?fname:string -> ?listed_transitions:label list option -> t -> unit
end


module Make (S: STATE) (L: LABEL) (A: ATTR) =
struct

  module State = S
  module Label = L
  module Attr = A

  type state = S.t
  type label = L.t
  type attr = A.t

  type transition = state * label * state
  type itransition = label * state

  module States = SetExt.Make (State)
  module Attrs = Map.Make(struct type t = state let compare = compare end)

  module Tree =
    Tree.Make(
        struct
          type node = state
          type edge = label
          let string_of_node = State.to_string 
          let string_of_edge = Label.to_string 
        end)


  module Transition = struct
    type t = transition
    let compare = compare
    let to_string (q1,l,q2) = "(" ^ State.to_string q1 ^ "," ^ Label.to_string l ^ "," ^ State.to_string q2 ^ ")"
  end

  module D = Set.Make(Transition)
  module Q = States
  module H = Attrs

  type t = {
      states: Q.t;
      attrs: attr H.t;
      irel: label H.t;
      rel: D.t
      }

  let string_of_state = State.to_string
  let string_of_label = Label.to_string
  let string_of_attr = Attr.to_string

  let empty = { states = Q.empty; attrs = H.empty; irel = H.empty; rel = D.empty }

  let add_state (q,a) s = { s with states = Q.add q s.states; attrs = H.add q a s.attrs }

  let remove_state q s = {
    states = Q.remove q s.states;
    attrs = H.remove q s.attrs;
    irel = H.filter (fun q' _ -> State.compare q q' <> 0) s.irel;
    rel = D.filter (function (q',_,q'') -> State.compare q q' <> 0 && State.compare q q'' <> 0) s.rel 
    }

  let attr_of s q = H.find q s.attrs

  exception Invalid_state of state

  let check_state q qs = if not (Q.mem q qs) then raise (Invalid_state q)

  let add_transition ((q1,l,q2) as t) s = 
    check_state q1 s.states;
    check_state q2 s.states;
    { s with rel = D.add t s.rel }

  let add_itransition (l,q) s = 
    check_state q s.states;
    { s with irel = H.add q l s.irel }

  let create ~states:qs ~itrans:ts0 ~trans:ts =
    empty |> (function s -> List.fold_left (Misc.flip add_state) s qs)
          |> (function s -> List.fold_left (Misc.flip add_transition) s ts)
          |> (function s -> List.fold_left (Misc.flip add_itransition) s ts0)

  let states s =  s.states 
  let states' s =  H.bindings s.attrs
(*   let attrs s =  s.attrs  *)
  let istates s =  H.fold (fun q _ acc -> Q.add q acc) s.irel Q.empty
  let istates' s =  Q.elements (istates s)
  let itransitions s =  H.fold (fun q l z -> (l,q)::z) s.irel []
  let transitions s = D.elements s.rel

  let iter_states f s = H.iter f s.attrs
  let fold_states f s z = H.fold f s.attrs z

  let iter_transitions f s = D.iter f s.rel
  let fold_transitions f s z = D.fold f s.rel z

  let iter_itransitions f s = H.iter (fun q l -> f (l,q)) s.irel
  let fold_itransitions f s z = H.fold (fun q l z -> f (l,q) z) s.irel z

  let is_state s q = Q.mem q s.states
  let is_init_state s q = H.mem q s.irel
  let is_transition s t = D.mem t s.rel

  let fold_succs s q f z = D.fold (fun (src,lbl,dst) acc -> if S.compare src q = 0 then f dst lbl acc else acc) s.rel z
  let fold_preds s q f z = D.fold (fun (src,lbl,dst) acc -> if S.compare dst q = 0 then f src lbl acc else acc) s.rel z
  let iter_succs s q f = D.iter (fun (src,lbl,dst) -> if S.compare src q = 0 then f dst lbl) s.rel
  let iter_preds s q f = D.iter (fun (src,lbl,dst) -> if S.compare dst q = 0 then f src lbl) s.rel

  let trav_acc s f q0s =
    let step qs = Q.union qs (Q.fold (fun q acc -> Q.union acc (f s q)) qs Q.empty) in
    Misc.iter_fix Q.equal step q0s

  let succs s q = fold_succs s q (fun q' _ acc -> Q.add q' acc) Q.empty
  let preds s q = fold_preds s q (fun q' _ acc -> Q.add q' acc) Q.empty
  let succs' s q = fold_succs s q (fun q' l acc -> (q',l)::acc) [] 
  let preds' s q = fold_preds s q (fun q' l acc -> (q',l)::acc) []

  let succs_hat s q = trav_acc s succs (Q.singleton q)
  let preds_hat s q = trav_acc s preds (Q.singleton q)

  let reachable_states s = trav_acc s succs (istates s)

  let is_reachable s q = Q.mem q (reachable_states s)

  let clean s =
    let rs = reachable_states s in
    { states = rs;
      attrs = H.filter (fun q _ -> Q.mem q rs) s.attrs;
      rel = D.filter (function (q,l,q') -> Q.mem q rs && Q.mem q' rs) s.rel;
      irel = H.filter (fun q _ -> Q.mem q rs) s.irel }

  let unwind depth s = 
    let rec unwind level q =
      if level < depth then
        Tree.Node(q, List.map (fun (q,l) -> unwind (level+1) q, l) (succs' s q))
      else 
        Tree.Empty
    in
    List.map (unwind 0) (Q.elements (istates s))

  let dot_output_oc name oc ?(options=[]) ?(marked_states=[]) ?(extra_nodes=[]) ?(implicit_transitions=[]) m = 
    let rankdir = if List.mem Dot.RankdirLR options then "LR" else "UD" in
    let module K = Map.Make (State) in
    let default_node_style = { Dot.node_shape = "circle"; Dot.node_style = "solid" } in
    let node_id i = name ^ "_" ^ string_of_int i in
    let ini_id = name ^ "_ini" in
    let extra_id j = name ^ "_extra" ^ string_of_int j in
    let extra_idxs = ListExt.range Misc.id 0 (List.length extra_nodes -1) in
    let ndescs, nbn = 
      fold_states
        (fun s a (acc,n) ->
          let style = try ListExt.assoc ~cmp:State.compare s marked_states with Not_found -> default_node_style in
          let lbl =
            if List.mem NoAttr options 
            then string_of_state s
            else string_of_state s  ^ "\\n" ^ string_of_attr a in
          K.add s (node_id n,lbl,style) acc, n+1)
        m
        (K.empty, 0) in 
    let ndesc q =
      try K.find q ndescs 
      with Not_found -> failwith ("Ltsa.dot_output: cannot find state " ^ (string_of_state q)) in
    let dump_extra_node i (lbl, {Dot.node_shape=sh; Dot.node_style=st}) =
      Printf.fprintf oc "%s [label = \"%s\", shape = %s, style = %s]\n" (extra_id i) lbl sh st in
    let dump_state q a =
      let id, lbl, style = ndesc q in
      Printf.fprintf oc "%s [label = \"%s\", shape = %s, style = %s]\n" id lbl (style.Dot.node_shape) (style.Dot.node_style) in
    let dump_itransition (l,q) =
      let id, _, _ = ndesc q in
      if List.mem NoLabel options then
        Printf.fprintf oc "%s -> %s;\n" ini_id id
      else
        Printf.fprintf oc "%s -> %s [label=\"%s\"];\n" ini_id id (Label.to_string l) in
    let dump_transition ((q,l,q') as t) =
      if not (List.mem t implicit_transitions) then 
        let id, _, _ = ndesc q in
        let id', _, _ = ndesc q' in
        if List.mem NoLabel options then
          Printf.fprintf oc "%s -> %s;\n" id id'
        else
          Printf.fprintf oc "%s -> %s [label = \"%s\"];\n" id id' (Label.to_string l) in
    let layout, mindist =
      if List.mem Circular options then "circo", 1.5 else "dot", 1.0 in
    if List.mem Dot.SubGraph options then 
      Printf.fprintf oc "subgraph cluster_%s {\nlabel = %s;\nrankdir = %s;\nsize = \"8.5,11\";\nlabel = \"%s\"\n center = 1;\n nodesep = \"0.350000\"\n ranksep = \"0.400000\"\n fontsize = 14;\nmindist=\"%1.1f\"\n" name name rankdir name mindist
    else
      Printf.fprintf oc "digraph %s {\nlayout = %s;\nrankdir = %s;\nsize = \"8.5,11\";\nlabel = \"\"\n center = 1;\n nodesep = \"0.350000\"\n ranksep = \"0.400000\"\n fontsize = 14;\nmindist=\"%1.1f\"\n" name layout rankdir mindist;
    Printf.fprintf oc "%s [shape=point; label=\"\"; style = invis]\n" ini_id;
    List.iteri dump_extra_node extra_nodes;
    if List.length extra_nodes > 0 then begin
      (* The followning lines force the corresponding nodes to lie on the same rank *)
        Printf.fprintf oc "%s -> %s [style=invis]\n" ini_id (ListExt.to_string extra_id " -> " extra_idxs);
        Printf.fprintf oc "{rank=same; %s, %s}\n" ini_id (ListExt.to_string extra_id ", " extra_idxs)
        end;
    iter_states dump_state m;
    iter_itransitions dump_itransition m;
    iter_transitions dump_transition m;
    Printf.fprintf oc "}\n"

  let dot_output name ?(fname="") ?(options=[]) ?(marked_states=[]) ?(extra_nodes=[]) ?(implicit_transitions=[]) m = 
    let fname = match fname with "" -> name ^ ".dot" | _ -> fname in
    let oc = open_out fname in
    dot_output_oc
      name oc ~options:options ~marked_states:marked_states ~extra_nodes:extra_nodes ~implicit_transitions:implicit_transitions m


  let dot_output_execs name ?(fname="") ?(options=[]) depth s = 
    let fname = match fname with "" -> name ^ ".dot" | _ -> fname in
    match unwind depth s with
    | [] -> Tree.dot_output name fname Tree.Empty 
    | [t] -> Tree.dot_output name fname ~options:options t
    | _ -> failwith "Ltsa.dot_output_execs: not [yet] supported: output of multi-rooted exec trees"

  let succs_l s q l =
    fold_succs s q (fun s' l' acc -> if L.compare l l' = 0 then (s',l')::acc else acc) []

  let tex_dump oc title s listed_transitions =
    let labels = match listed_transitions with
      None ->
        let module LL = Set.Make(L) in
        LL.elements (D.fold (fun (_,l,_) ls -> LL.add l ls) s.rel LL.empty)
    | Some lbls -> lbls in
    Printf.fprintf oc "%% Generated automatically by Lascar OCaml library\n" ;
    Printf.fprintf oc "$\\begin{array}[c]{|c|";
    List.iter (function l -> Printf.fprintf oc "c|") labels;
    Printf.fprintf oc "}\n";
    Printf.fprintf oc "\\hline\n";
    Printf.fprintf oc "\\text{%s} " title;
    List.iter (function l -> Printf.fprintf oc " & %s" (Label.to_string l)) labels;
    Printf.fprintf oc "\\\\ \\hline\n";
    let dest q l = 
      match succs_l s q l with
        [] -> "-"
      | xs -> ListExt.to_string (function (q,l) -> string_of_state q) "," xs in
    let dump_state q =
      Printf.fprintf oc "%s%s " (if is_init_state s q then "\\rightarrow " else "") (string_of_state q);
      List.iter (function l -> Printf.fprintf oc " & %s" (dest q l)) labels;
      Printf.fprintf oc "\\\\ \\hline\n" in
    Q.iter dump_state s.states;
    Printf.fprintf oc "\\end{array}$\n"

  let tex_output name ?(fname="") ?(listed_transitions=None) a =
    let fname = match fname with "" -> name ^ ".tex" | _ -> fname in
    let oc = open_out fname in
    let title = StringExt.escape_car '_' name in
    tex_dump oc title a listed_transitions;
    close_out oc;
    Printf.printf "Wrote file %s\n" fname

end

module Trans (S1: T) (S2: T) =
struct
  let map fs fl fa s1 =
    let add_states s = S1.fold_states (fun q a acc -> S2.add_state (fs q, fa a) acc) s1 s in
    let add_transitions s = S1.fold_transitions (fun (q,l,q') acc -> S2.add_transition (fs q, fl l, fs q') acc) s1 s in
    let add_itransitions s = S1.fold_itransitions (fun (l,q') acc -> S2.add_itransition (fl l, fs q') acc) s1 s in
    S2.empty |> add_states |> add_transitions |> add_itransitions
end

module Product (S1: T) (S2: T) =
struct
  module S = OrderedTypeExt.Pair (S1.State) (S2.State)
  module L = OrderedTypeExt.Either (S1.Label) (S2.Label)
  module A = Stringable.Pair (S1.Attr) (S2.Attr)
  module R = Make(S)(L)(A)
  include R
  let add_states s1 s2 s =
    let states' = ListExt.prod (fun (q1,a1) (q2,a2) -> (q1,q2),(a1,a2)) (S1.states' s1) (S2.states' s2) in
    List.fold_left (fun acc (q,a) -> R.add_state (q,a) acc) s states'
  let add_transitions sync s1 s2 s =
    let trans' =
      (* [trans' = [ ((q1,q2)--(l1,.)-->(q1',q2) | (q1,l1,q1') \in R1, q2 \in Q2 ]
                 @ [ ((q1,q2)--(.,l2)-->(q1,q2') | (q2,l2,q2') \in R2, q1 \in Q1 ]
                 @ [ ((q1,q2)--(l1,l2)-->(q1',q2') | (q1,l1,q1') \in R1, (q2,l2,q2') \in R2] *)
        List.map (function ((q1,l1,q1'),q2) -> ((q1,q2),(Some l1,None),(q1',q2)))
                 (ListExt.cart_prod2 (S1.transitions s1) (List.map fst (S2.states' s2)))
      @ List.map (function ((q2,l2,q2'),q1) -> ((q1,q2),(None,Some l2),(q1,q2')))
                 (ListExt.cart_prod2 (S2.transitions s2) (List.map fst (S1.states' s1)))
      @ List.map (function ((q1,l1,q1'),(q2,l2,q2')) -> ((q1,q2),(Some l1,Some l2),(q1',q2')))
                 (ListExt.cart_prod2 (S1.transitions s1) (S2.transitions s2)) in
    List.fold_left (fun acc ((q,l,q') as t) -> if sync l then R.add_transition t acc else acc) s trans'
  let add_itransitions s1 s2 s =
    let itrans' =
      (* [itrans' = [ --(l1,l2)-->(q1,q2) | (l1,q1) \in I1, (l2,q2) \in I2 ] *)
      List.map (function ((l1,q1),(l2,q2)) -> ((Some l1,Some l2),(q1,q2)))
                 (ListExt.cart_prod2 (S1.itransitions s1) (S2.itransitions s2)) in
    List.fold_left (fun acc ((l,q) as t) -> R.add_itransition t acc) s itrans'
  let synch_product sync s1 s2 =
    R.empty |> (add_states s1 s2)
            |> (add_transitions sync s1 s2)
            |> (add_itransitions s1 s2)
  let synchronized_product sync_set = synch_product (function l -> List.mem l sync_set)
  let free_product = synch_product (function _ -> true)
  let asynchronous_product = synch_product (function (Some _, None) | (None, Some _)-> true | _ -> false)
  let synchronous_product = synch_product (function (Some _, Some _) -> true | _ -> false)
end

module Product3 (S1: T) (S2: T) (S3: T) =
struct
  module S = OrderedTypeExt.Triplet (S1.State) (S2.State) (S3.State)
  module L = OrderedTypeExt.Either3 (S1.Label) (S2.Label) (S3.Label)
  module A = Stringable.Triplet (S1.Attr) (S2.Attr) (S3.Attr)
  module R = Make(S)(L)(A)
  include R
  let add_states s1 s2 s3 s =
    let states' = ListExt.prod3 (fun (q1,a1) (q2,a2) (q3,a3) -> (q1,q2,q3),(a1,a2,a3)) (S1.states' s1) (S2.states' s2) (S3.states' s3) in
    List.fold_left (fun acc (q,a) -> R.add_state (q,a) acc) s states'
  let add_transitions sync s1 s2 s3 s =
    let states1 = List.map fst (S1.states' s1) in
    let states2 = List.map fst (S2.states' s2) in
    let states3 = List.map fst (S3.states' s3) in
    let trans' =
      (* [trans' = [ ((q1,q2,q3)--(l1,.,.)-->(q1',q2,q3) | (q1,l1,q1') \in R1, q2 \in Q2, q3 \in Q3 ]
                 @ [ ((q1,q2,q3)--(.,l2,.)-->(q1,q2',q3) | (q2,l2,q2') \in R2, q1 \in Q1, q3 \in Q3 ]
                 @ [ ((q1,q2,q3)--(.,.,l3)-->(q1,q2,q3') | (q3,l3,q3') \in R3, q1 \in Q1, q2 \in Q2 ]
                 @ [ ((q1,q2,q3)--(l1,l2,.)-->(q1',q2',q3) | (q1,l1,q1') \in R1, (q2,l2,q2') \in R2, q3 \in Q3 ]
                 @ [ ((q1,q2,q3)--(l1,.,l3)-->(q1',q2,q3') | (q1,l1,q1') \in R1, (q3,l3,q3') \in R3, q2 \in Q2 ]
                 @ [ ((q1,q2,q3)--(.,l2,l3)-->(q1,q2',q3') | (q2,l2,q2') \in R2, (q3,l3,q3') \in R3, q1 \in Q1 ]
                 @ [ ((q1,q2,q3)--(l1,l2,l3)-->(q1',q2',q3') | (q1,l1,q1') \in R1, (q2,l2,q2') \in R2, (q3,l3,q3') ] *)
        List.map (function ((q1,l1,q1'),q2,q3) -> ((q1,q2,q3),(Some l1,None,None),(q1',q2,q3)))
                 (ListExt.cart_prod3 (S1.transitions s1) states2 states3)
      @ List.map (function (q1,(q2,l2,q2'),q3) -> ((q1,q2,q3),(None,Some l2,None),(q1,q2',q3)))
                 (ListExt.cart_prod3 states1 (S2.transitions s2) states3)
      @ List.map (function (q1,q2,(q3,l3,q3')) -> ((q1,q2,q3),(None,None,Some l3),(q1,q2,q3')))
                 (ListExt.cart_prod3 states1 states2 (S3.transitions s3))
      @ List.map (function ((q1,l1,q1'),(q2,l2,q2'),q3) -> ((q1,q2,q3),(Some l1,Some l2,None),(q1',q2',q3)))
                 (ListExt.cart_prod3 (S1.transitions s1) (S2.transitions s2) states3)
      @ List.map (function ((q1,l1,q1'),q2,(q3,l3,q3')) -> ((q1,q2,q3),(Some l1,None,Some l3),(q1',q2,q3')))
                 (ListExt.cart_prod3 (S1.transitions s1) states2 (S3.transitions s3))
      @ List.map (function (q1,(q2,l2,q2'),(q3,l3,q3')) -> ((q1,q2,q3),(None,Some l2,Some l3),(q1,q2',q3')))
                 (ListExt.cart_prod3 states1 (S2.transitions s2) (S3.transitions s3))
      @ List.map (function ((q1,l1,q1'),(q2,l2,q2'),(q3,l3,q3')) -> ((q1,q2,q3),(Some l1,Some l2,Some l3),(q1',q2',q3')))
                 (ListExt.cart_prod3 (S1.transitions s1) (S2.transitions s2) (S3.transitions s3)) in
    List.fold_left (fun acc ((q,l,q') as t) -> if sync l then R.add_transition t acc else acc) s trans'
  let add_itransitions s1 s2 s3 s =
    let itrans' =
      (* [itrans' = [ --(l1,l2,l3)-->(q1,q2,q3) | (l1,q1) \in I1, (l2,q2) \in I2, (l3,q3) \in I3 ] *)
      List.map (function ((l1,q1),(l2,q2),(l3,q3)) -> ((Some l1,Some l2,Some l3),(q1,q2,q3)))
                 (ListExt.cart_prod3 (S1.itransitions s1) (S2.itransitions s2) (S3.itransitions s3)) in
    List.fold_left (fun acc ((l,q) as t) -> R.add_itransition t acc) s itrans'
  let synch_product sync s1 s2 s3 =
    R.empty |> (add_states s1 s2 s3)
            |> (add_transitions sync s1 s2 s3)
            |> (add_itransitions s1 s2 s3)
  let synchronized_product sync_set = synch_product (function l -> List.mem l sync_set)
  let free_product = synch_product (function _ -> true)
  let asynchronous_product = synch_product (function (Some _, None, None) | (None, Some _, None) | (None, None, Some _) -> true | _ -> false)
  let synchronous_product = synch_product (function (Some _, Some _, Some _) -> true | _ -> false)
end


module type Merge = sig
  type state
  type label
  type attr
  val merge_state: state * state -> state
  val merge_label: label option * label option -> label
  val merge_attr: attr * attr -> attr
end
  
module IProduct (S: T) (M: Merge with type state=S.state and type label=S.label and type attr=S.attr) = struct
  include S
  module P = Product(S)(S)
  module U = Trans(P)(S)
  let tr = U.map M.merge_state M.merge_label M.merge_attr
  let free_product s1 s2 = tr (P.free_product s1 s2)
  let synch_product sync s1 s2 = tr (P.synch_product sync s1 s2)
  let synchronized_product sync s1 s2 = tr (P.synchronized_product sync s1 s2)
  let asynchronous_product s1 s2 = tr (P.asynchronous_product s1 s2)
  let synchronous_product s1 s2 = tr (P.synchronous_product s1 s2)
end

module type Merge3 = sig
  type state
  type label
  type attr
  val merge_state: state * state * state -> state
  val merge_label: label option * label option * label option -> label
  val merge_attr: attr * attr * attr -> attr
end
  
module IProduct3 (S: T) (M: Merge3 with type state=S.state and type label=S.label and type attr=S.attr) = struct
  include S
  module P = Product3(S)(S)(S)
  module U = Trans(P)(S)
  let tr = U.map M.merge_state M.merge_label M.merge_attr
  let free_product s1 s2 s3 = tr (P.free_product s1 s2 s3)
  let synch_product sync s1 s2 s3 = tr (P.synch_product sync s1 s2 s3)
  let synchronized_product sync s1 s2 s3 = tr (P.synchronized_product sync s1 s2 s3)
  let asynchronous_product s1 s2 s3 = tr (P.asynchronous_product s1 s2 s3)
  let synchronous_product s1 s2 s3 = tr (P.synchronous_product s1 s2 s3)
end

