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

(* let subst_vars vars exp =
 *   let rec subst e = match e with
 *       EConst _ -> e
 *     | EVar v -> if List.mem_assoc v vars then EConst (List.assoc v vars) else e
 *     | EBinop (op, exp1, exp2) -> EBinop (op, subst exp1, subst exp2) in
 *   subst exp *)

(* Parsing *)

(* BNF :
   <exp>  ::= INT
   | ID
   | <exp> <op> <exp>
   | '(' <exp> ')' <int>
   <op>    ::= '+' | '-' | '*' | '/'
 *)

let keywords = List.map fst binary_ops @ List.map fst test_ops @ [":="; "("; ")"; ";"]

let mk_binary_minus s = s |> String.split_on_char '-' |> String.concat " - "
                      
let lexer s = s |> mk_binary_minus |> Stream.of_string |> Genlex.make_lexer keywords 

open Genlex
   
let rec p_exp0 s =
  match Stream.next s with
    | Int n -> EConst n
    | Ident i -> EVar i
    | Kwd "(" ->
       let e = p_exp s in
       begin match Stream.peek s with
       | Some (Kwd ")") -> Stream.junk s; e
       | _ -> raise Stream.Failure
       end
    | _ -> raise Stream.Failure

and p_exp1 s =
  let e1 = p_exp0 s in
  p_exp2 e1 s
  
and p_exp2 e1 s =
  match Stream.peek s with
  | Some (Kwd "*") -> Stream.junk s; let e2 = p_exp1 s in EBinop("*", e1, e2)
  | Some (Kwd "/") -> Stream.junk s; let e2 = p_exp1 s in EBinop("/", e1, e2)
  | _ -> e1
  
and p_exp s =
  let e1 = p_exp1 s in p_exp3 e1 s
                     
and p_exp3 e1 s =
  match Stream.peek s with
  | Some (Kwd "+") -> Stream.junk s; let e2 = p_exp s in EBinop("+", e1, e2)
  | Some (Kwd "-") -> Stream.junk s; let e2 = p_exp s in EBinop("-", e1, e2)
  | _ -> e1

let parse = p_exp

let of_string s = s |> lexer |> p_exp

let rec to_string e = match e with
    EConst c -> string_of_int c
  | EVar n ->  n
  | EBinop (op,e1,e2) -> to_string e1 ^ op ^ to_string e2 (* TODO : add parens *)
