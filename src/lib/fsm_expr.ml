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

  type ident = string [@@deriving show]

  type value [@@deriving show]

  type t = 
    EConst of value            (** Constants *)   
  | EVar of ident              (** Input, output or local variable *)
  | EBinop of string * t * t   (** Binary operation  *)
  | EUnop of char * t          (** Unary operation  *)
  [@@deriving show {with_path=false}]

  type env = (ident * value option) list
           
  exception Unknown of ident
  exception Unknown_op of string
  exception Unbound of ident
  exception Illegal_expr

  val test_ops: (string * (value -> value -> bool)) list (** name, fun *)

  val to_string: t -> string

  val of_string: string -> t

  val lookup: env -> ident -> value

  val eval: env -> t -> value

  val lexer: string -> Genlex.token Stream.t
  val parse: Genlex.token Stream.t -> t 

  val keywords: string list
  val mk_unaries: string -> string
end

module Make (V: Fsm_value.T) = struct

    type ident = string [@@deriving show]

    type value = V.t  [@@deriving show]

    type t = 
      EConst of value            (** Constants *)   
    | EVar of ident              (** Input, output or local variable *)
    | EBinop of string * t * t   (** Binary operation *)
    | EUnop of char * t          (** Unary operation *)
    [@@deriving show {with_path=false}]

    type env = (ident * value option) list

    exception Unknown of ident
    exception Unknown_op of string
    exception Unbound of ident
    exception Illegal_expr

    let lookup env id = 
      try
        match List.assoc id env with
          Some v -> v
        | None -> raise (Unbound id)
      with 
        Not_found -> raise (Unknown id)

    let lookup_binop op = 
        try List.assoc op V.binary_ops |> fst
        with Not_found -> raise (Unknown_op op)

    let infix_level op = 
        try List.assoc op V.binary_ops |> snd
        with Not_found -> raise (Unknown_op op)

    let lookup_unop op = 
      try List.assoc op V.unary_ops
      with Not_found -> raise (Unknown (String.make 1 op))

    let rec eval env exp = 
      match exp with
        EConst v -> v
      | EVar id -> lookup env id 
      | EBinop (op, exp1, exp2) -> lookup_binop op (eval env exp1) (eval env exp2)
      | EUnop (op, exp) -> lookup_unop op (eval env exp)

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
   | <exp> <binop>_0 <exp>
   | <exp> <binop>_1 <exp>
   | <unop> <exp>
   | '(' <exp> ')'
     *)

    let test_ops = [
        (* Though test  operators are not part _per se_ of Fsm_exprs, they must be defined here 
           because the parser of Fsm.Conditions, which use them, share the same lexer.
           TO BE FIXED *)
        "=", (=);
        "!=", (<>);
        "<", (<);
        ">", (>);
        "<=", (<=);
        ">=", (>=)
      ]
                 
    let keywords =
        List.map (fun (op,_) -> String.make 1 op) V.unary_ops
      @ List.map fst V.binary_ops
      @ List.map fst test_ops
      @ [":="; "("; ")"; ";"; "|"]

    let mk_unaries s = Utils.Misc.space_chars (List.map fst V.unary_ops) s
                          
    let lexer s = s |> mk_unaries |> Stream.of_string |> Genlex.make_lexer keywords 

    open Genlex
       
    let rec p_exp0 s =
      match Stream.next s with
      | Int n -> EConst (V.of_int n)
      | Ident i -> EVar i
      | Kwd "(" ->
         let e = p_exp s in
         begin match Stream.peek s with
         | Some (Kwd ")") -> Stream.junk s; e
         | _ -> raise Stream.Failure
         end
      | _ -> raise Stream.Failure

    and p_exp1 s =
      match Stream.peek s with
      | Some (Kwd op) when List.mem_assoc op.[0] V.unary_ops -> Stream.junk s; let e = p_exp0 s in EUnop(op.[0], e)
      | _ ->
         let e1 = p_exp0 s in
         p_exp2 e1 s
      
    and p_exp2 e1 s =
      match Stream.peek s with
      | Some (Kwd op) when List.mem_assoc op V.binary_ops && infix_level op = 0 -> Stream.junk s; let e2 = p_exp1 s in EBinop(op, e1, e2)
      | _ -> e1
           
    and p_exp3 e1 s =
      match Stream.peek s with
      | Some (Kwd op) when List.mem_assoc op V.binary_ops && infix_level op = 1 -> Stream.junk s; let e2 = p_exp s in EBinop(op, e1, e2)
      | _ -> e1

    and p_exp s =
      let e1 = p_exp1 s in p_exp3 e1 s
                         
    let parse = p_exp

    let of_string s = s |> lexer |> p_exp

    let rec to_string e = match e with
        EConst c -> V.to_string c
      | EVar n ->  n
      | EBinop (op,e1,e2) -> to_string e1 ^ op ^ to_string e2 (* TODO : add parens *)
      | EUnop (op,e) -> String.make 1 op ^ to_string e (* TODO : add parens *)

end

module Trans (E1: T) (E2: T) =
struct
  let rec map fv e1  = match e1 with
    | E1.EConst c -> E2.EConst (fv c)
    | E1.EVar n -> E2.EVar n
    | E1.EBinop (op,e1,e2) -> E2.EBinop (op, map fv e1, map fv e2)
    | E1.EUnop (op,e) -> E2.EUnop (op, map fv e)
end
