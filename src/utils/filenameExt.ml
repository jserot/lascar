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

let split_suffix name =
  try
    let i = String.rindex name '.' in
    String.sub name 0 i, String.sub name i (String.length name - i)
  with
    Not_found -> name, ""

let add_before_suffix name e =
  let b, s = split_suffix name in
  b ^ e ^ s

let replace_suffix name s =
  let b, _ = split_suffix name in
  b ^ "." ^ s
  
