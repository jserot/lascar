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

(** {2 A basic implementation of n-ary trees} *)

(** Input signature of the functor {!Tree.Make}. *)
module type Arg =
sig
    type node  (** The type of information attached to the nodes of the tree *)
    type edge  (** The type of information attached to the edges of the tree *)
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
        The order in which the nodes are presented is unspecified. *)

  (* TO BE COMPLETED ... *)

  val dot_output: string
                  -> string
                  -> ?options:Dot.graph_style list
                  -> t
                  -> unit
    (** [dot_output name fname t] writes a .dot representation of tree [t] with name [name] in file [fname].
        The aspect of the tree can be adjusted with the [options] optional argument (see {!Dot.graph_style}). *)

  val dot_view: string
               -> ?options:Dot.graph_style list
               -> ?dir:string
               -> ?cmd:string
               -> t
               -> int
    (** [dot_view name t] writes a .dot representation of [t] in file [dir/name.dot] and opens it with Graphviz.
        The output directory [dir] is specified with the [dir] optional argument (default: "/tmp").
        The command for launching Graphviz is specified with the [cmd] optional argument (default: "open -a Graphviz").
        The [options] optional argument is the same than defined for {!dot_output}.
        The function returns the exit code of the corresponding process. *)
end

(** Functor building an implementation of the tree structure *)
module Make (A : Arg) : S with type node = A.node and type edge = A.edge
