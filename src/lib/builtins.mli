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

(** Pre-defined modules to be used as functor arguments *)

module Int : OrderedTypeExt.T with type t=int

module String : OrderedTypeExt.T with type t=string

module Bool : OrderedTypeExt.T with type t=bool
