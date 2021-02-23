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

module type T = sig
  module Expr : Fsm_expr.T
  type t = 
    | Test of Expr.ident * string * Expr.t (* var, op, expr *)
      [@@deriving show {with_path=false}]
  val to_string: t -> string
  val of_string: ?lexer:(string->Genlex.token Stream.t) -> string -> t               
  val lexer: string -> Genlex.token Stream.t
  val parse: Genlex.token Stream.t -> t               
  val eval: Expr.env -> t -> bool
end

module Make (Expr: Fsm_expr.T) = struct
  module Expr = Expr
  type t = 
    | Test of Expr.ident * string * Expr.t (* var, op, expr *)
      [@@deriving show {with_path=false}]
  let to_string c = match c with
  | Test (v,op,e) -> v ^ op ^ Expr.to_string e
  exception Unknown_op of string
  let keywords = Expr.keywords
  let lexer s = s |> Expr.mk_unaries |> Stream.of_string |> Genlex.make_lexer keywords 
  let p_cond s = 
    let open Genlex in
    match Stream.next s with
    | Ident v ->
       begin match Stream.next s with
       | Kwd op when List.mem_assoc op Expr.test_ops -> let e = Expr.parse s in Test (v, op, e)
       | _ -> raise Stream.Failure 
       end
    | _ -> raise Stream.Failure
  let parse = p_cond
  let of_string ?(lexer=lexer) s = parse (lexer s)
  let lookup op =
    try List.assoc op Expr.test_ops
    with Not_found -> raise (Unknown_op op)
  let eval env texp = match texp with
  | Test (id, op, exp) -> (lookup op) (Expr.lookup env id) (Expr.eval env exp)
end

module Trans (C1: T) (C2: T) =
struct
  module F = Fsm_expr.Trans (C1.Expr) (C2.Expr)
  let map fv c1  = match c1 with
    | C1.Test (id, op, exp) -> C2.Test (id, op, F.map fv exp)
end
