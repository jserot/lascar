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

type ('a, 'b) either =
    Fst of 'a
  | Snd of 'b
  | Both of 'a * 'b

let flip f x y = f y x

let id x = x

let max x y = if x > y then x else y
let min x y = if x < y then x else y

let log2 x = int_of_float (log (float_of_int x) /. log 2.)

let rec iter_fix eq f x =
  let y = f x in
  if eq y x then x else iter_fix eq f y

let time_of_day () =
  "today"
(*   let t = Unix.localtime (Unix.time ()) in *)
(*   Printf.sprintf "%04d-%02d-%02d at %02d:%02d:%02d" *)
(*     (t.Unix.tm_year+1900) (t.Unix.tm_mon+1) t.Unix.tm_mday t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec *)

let append_file f s =
  let oc = open_out_gen [Open_append] 0x644 f in
  Printf.fprintf oc "%s" s;
  close_out oc
