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

let map f x = match x with None -> None | Some x -> Some (f x)

let iter f x = match x with None -> () | Some x -> f x

let to_string f x = match x with None -> "" | Some x -> f x
