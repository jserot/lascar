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

(** {2 Pre-defined modules to be used as functor arguments} *)

module Int : Utils.OrderedTypeExt.T with type t=int

module String : Utils.OrderedTypeExt.T with type t=string

module Bool : Utils.OrderedTypeExt.T with type t=bool
