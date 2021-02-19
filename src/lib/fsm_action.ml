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

open Utils

module type T = sig
  module Expr : Fsm_expr.T
  type t = 
  | Assign of Expr.ident * Expr.t          (* var, value *)
  val to_string: t -> string
  val of_string: string -> t               (* BNF : <act>   ::= ID ':=' <exp> *)
  val list_of_string: string -> t list
end

module Make (Expr: Fsm_expr.T) = struct
  module Expr = Expr
  type t = 
    | Assign of Expr.ident * Expr.t        (* variable, value *)
  let to_string a = match a with
    | Assign (id, expr) -> id ^ ":=" ^ Expr.to_string expr
  let rec p_act s = match Stream.next s with
    | Genlex.Ident e1 -> p_act1 e1 s
    | _ -> raise Stream.Failure
  and p_act1 e1 s = match Stream.next s with
    | Genlex.Kwd ":=" -> let e2 = Expr.parse s in Assign (e1, e2)
    | _ -> raise Stream.Failure
  let of_string s = p_act (Expr.lexer s)
  let list_of_string s = ListExt.parse ";" p_act (Expr.lexer s)
end

module Trans (A1: T) (A2: T) =
struct
  module F = Fsm_expr.Trans (A1.Expr) (A2.Expr)
  let map fv a1  = match a1 with
    | A1.Assign (id, exp) -> A2.Assign (id, F.map fv exp)
end
