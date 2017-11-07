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

let is_uppercased s = match s.[0] with 'A' .. 'Z' -> true | _ -> false

let rec explode s =
  if s = "" then []
  else String.sub s 0 1 :: explode (String.sub s 1 (String.length s - 1))

let escape_car c s =
  let l = String.length s in
  let j = ref 0 in
  for i = 0 to l-1 do if s.[i] = c then incr j done;
  let s' = Bytes.create (l + !j) in
  j := 0;
  for i = 0 to l-1 do
    if s.[i] = c then begin Bytes.set s' !j '\\'; incr j end;
    Bytes.set s' !j s.[i];
    incr j;
  done;
  Bytes.to_string s'

let remove_car c s =
  let l = String.length s in
  let n = ref l in
  for i=0 to l-1 do
    if s.[i] = c then decr n
  done;
  let r = Bytes.create !n in
  let j = ref 0 in
  for i=0 to l-1 do
    if s.[i] <> c then begin Bytes.set r !j s.[i]; incr j end
  done;
  Bytes.to_string r

let concat_sep sep ss =
  let rec concat = function
      [] -> ""
    | s::rest ->
       begin match s, concat rest with
       | "", "" -> ""
       | "", s -> s
       | s, "" -> s
       | s, s' -> s ^ sep ^ s'
       end in
  concat ss
