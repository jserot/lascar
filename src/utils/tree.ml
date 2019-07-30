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

(** Input signature of the functor {!Tree.Make}. *)
module type Arg =
sig
    type node
    type edge
    val string_of_node: node -> string
    val string_of_edge: edge -> string
end

(** Output signature of the functor {!Tree.Make}. *)
module type S = 
sig
  type node (** The type of information attached to the nodes of the tree *)

  type edge (** The type of information attached to the edges of the tree *)

  type t =  (** The type of trees *)
    Empty
  | Node of node * (t * edge) list

  val fold: (node -> node -> node) -> node -> t -> node
    (** [fold f z t] is [f (... (f (f z n1) n2) ...) nn], where [n1, ..., nn] are the nodes of [t].
        The in which the nodes are presented is unspecified. *)

  (* TO BE COMPLETED ... *)

  val dot_output: string
                  -> string
                  -> ?options:Dot.graph_style list
                  -> t
                  -> unit

  val dot_view: string
               -> ?options:Dot.graph_style list
               -> ?dir:string
               -> ?cmd:string
               -> t
               -> int
end

(** Functor building an implementation of the tree structure *)
module Make (A : Arg) =
struct
  type node = A.node
  
  type edge = A.edge

  type t =
    Empty
  | Node of node * (t * edge) list

  let rec fold f z = function
      Empty -> z
    | Node (a, ts) -> List.fold_left (fun z (t,_) -> f z (fold f z t)) a ts

  let output name oc options t =
  let rankdir = if List.mem Dot.RankdirLR options then "LR" else "UD" in
  let cnt = ref 0 in
  let rec outp parent (t,l) = 
      match t with
        Empty ->
          let current = !cnt in
          Printf.fprintf oc "%d [label = \"%s\" shape = plaintext]\n" current "...";
          if parent >= 0 then Printf.fprintf oc "%d -> %d [label = \"%s\"]\n" parent current l;
          incr cnt
      | Node (n, ts) ->
          let current = !cnt in
          Printf.fprintf oc "%d [label = \"%s\" shape = plaintext]\n" current (A.string_of_node n);
          if parent >= 0 then Printf.fprintf oc "%d -> %d [label = \"%s\"]\n" parent current l;
          incr cnt;
          List.iteri (fun i (t,l) -> outp current (t, A.string_of_edge l)) ts in
    Printf.fprintf oc "digraph %s {\nrankdir = %s;\nsize = \"8.5,11\";\nlabel = \"\"\n center = 1;\n nodesep = \"0.350000\"\n ranksep = \"0.400000\"\n fontsize = 12\n orientation = Portrait\n" name rankdir;
    outp (-1) (t,"");
    Printf.fprintf oc "}\n"

  let dot_output name fname ?(options=[]) t =
    let oc = open_out fname in
    output name oc options t;
    close_out oc

  let dot_view name ?(options=[]) ?(dir="/tmp") ?(cmd="open -a Graphviz") t =
    let fname = dir ^ "/" ^ name ^ ".dot" in
    dot_output name fname ~options:options t;
    Printf.printf "Wrote file %s\n" fname;
    Sys.command (cmd ^ " " ^ fname)
end
