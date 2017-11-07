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

type graph_style = ..

type graph_style +=
   | SubGraph   (** Draw graph as a subgraph (to be inserted in an enclosing graph) *) 
   | RankdirUD  (** Draw graph in the Up-down direction *)
   | RankdirLR  (** Draw graph in the Left-right direction *)

type node_style = { node_shape: string; node_style: string }
