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
  | Assign of Expr.ident * Expr.t          (* var, value *)
      [@@deriving show {with_path=false}]
  val to_string: t -> string
  val of_string: ?lexer:(string->Genlex.token Stream.t) -> string -> t               
  val lexer: string -> Genlex.token Stream.t
  val keywords: string list
  val parse: Genlex.token Stream.t -> t               
end

module Make (Expr: Fsm_expr.T) = struct
  module Expr = Expr
  type t = 
    | Assign of Expr.ident * Expr.t        (* variable, value *)
      [@@deriving show {with_path=false}]
  let to_string a = match a with
    | Assign (id, expr) -> id ^ ":=" ^ Expr.to_string expr
  let keywords = Expr.keywords @ [":="]
  let lexer s = s |> Expr.mk_unaries |> Stream.of_string |> Genlex.make_lexer keywords 
  let rec p_act s = match Stream.next s with
    | Genlex.Ident e1 -> p_act1 e1 s
    | _ -> raise Stream.Failure
  and p_act1 e1 s = match Stream.next s with
    | Genlex.Kwd ":=" -> let e2 = Expr.parse s in Assign (e1, e2)
    | _ -> raise Stream.Failure
  let parse = p_act
  let of_string ?(lexer=lexer) s = p_act (lexer s)
end

module Trans (A1: T) (A2: T) =
struct
  module F = Fsm_expr.Trans (A1.Expr) (A2.Expr)
  let map fv a1  = match a1 with
    | A1.Assign (id, exp) -> A2.Assign (id, F.map fv exp)
end

module Int = Make(Fsm_expr.Int)
module Bool = Make(Fsm_expr.Bool)
