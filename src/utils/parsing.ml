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

let separated_list sep ?(stop_on:string option=None) p s =
  let rec p_rest s =
  match Stream.peek s, stop_on with
  | Some (Genlex.Kwd sep'), _ when sep'=sep ->
     Stream.junk s;
     let e = p s in
     let es = p_rest s in
     e::es
  | Some (Genlex.Kwd c), Some c' when c=c' -> [] (* Special case: do not report error but return *)
  | Some _, _ -> raise Stream.Failure
  | None, _ -> [] in
  match Stream.peek s with
  | Some _ ->
     let e = p s in
     let es = p_rest s in
     e::es
  | None ->
     []

let run ~lexer ~parser s =
  let ss = lexer s in
  try
    let e  = parser ss in
    begin match Stream.peek ss with
    | None -> Ok e
    | t -> Error t
    end
  with
    Stream.Failure -> Error (Stream.peek ss)

exception Parse_error of string * Genlex.token  option

let try_run ~lexer ~parser s =
  let ss = lexer s in
  try
    let e  = parser ss in
    begin match Stream.peek ss with
    | None -> e
    | t -> raise (Parse_error (s, Stream.peek ss))
    end
  with
    Stream.Failure -> raise (Parse_error (s, Stream.peek ss))
