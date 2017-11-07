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

type ident = string 

type value = int 

type t = 
  EConst of value            (** Constants *)   
| EVar of ident              (** Input, output or local variable *)
| EBinop of string * t * t   (** Binary operation *)

type env = (ident * value option) list

exception Unknown of ident
exception Unbound of ident
exception Illegal_expr

let lookup env id = 
  try
    match List.assoc id env with
      Some v -> v
    | None -> raise (Unbound id)
  with 
    Not_found -> raise (Unknown id)

let binary_ops = [
    "+", ( + );
    "-", ( - );
    "*", ( * );
    "/", ( / );
  ]

let test_ops = [  (* TO FIX : should only be in [Fsm], not here, but this complicates the lexer defns *)
    "=", ( = );
    "!=", ( <> );
    "<", ( < );
    ">", ( > );
    "<=", ( <= );
    ">=", ( >= )
  ]

let binary_op op = 
  try List.assoc op binary_ops
  with Not_found -> raise (Unknown op)

let rec eval env exp = 
  match exp with
    EConst v -> v
  | EVar id -> lookup env id 
  | EBinop (op, exp1, exp2) -> binary_op op (eval env exp1) (eval env exp2)

let subst_vars vars exp =
  let rec subst e = match e with
      EConst _ -> e
    | EVar v -> if List.mem_assoc v vars then EConst (List.assoc v vars) else e
    | EBinop (op, exp1, exp2) -> EBinop (op, subst exp1, subst exp2) in
  subst exp

(* Parsing *)

open Genlex

(* BNF :
   <exp>  ::= INT
   | ID
   | <exp> <op> <exp>
   | '(' <exp> ')' <int>
   <op>    ::= '+' | '-' | '*' | '/'
 *)

let keywords = List.map fst binary_ops @ List.map fst test_ops @ [":="; "("; ")"; ";"]

let lexer s =
  let rec aux = parser
              | [< 'Int n when n<0; t=aux >] -> [< 'Kwd "-"; 'Int (-n); t >]
              | [< 'h; t=aux >] -> [< 'h; t >]
              | [< >] -> [< >] in
  aux (Genlex.make_lexer keywords (Stream.of_string s))

let rec p_exp0  = parser
                | [< 'Int n >] -> EConst n
                | [< 'Ident i >] -> EVar i
                | [< 'Kwd "("; e=p_exp ; 'Kwd ")" >] -> e

and p_exp1  = parser
            | [< e1=p_exp0 ; rest >] -> p_exp2  e1 rest

and p_exp2  e1 = parser
               | [< 'Kwd "*"; e2=p_exp1  >] -> EBinop("*", e1, e2)
               | [< 'Kwd "/"; e2=p_exp1  >] -> EBinop("/", e1, e2)
               | [< >] -> e1

and p_exp  = parser
           | [< e1=p_exp1 ; rest >] -> p_exp3  e1 rest

and p_exp3  e1 = parser
               | [< 'Kwd "+"; e2=p_exp  >] -> EBinop("+", e1, e2)
               | [< 'Kwd "-"; e2=p_exp  >] -> EBinop("-", e1, e2)
               | [< >] -> e1

let parse = p_exp

let of_string s = p_exp (lexer s)

let rec to_string e = match e with
    EConst c -> string_of_int c
  | EVar n ->  n
  | EBinop (op,e1,e2) -> to_string e1 ^ op ^ to_string e2 (* TODO : add parens *)

