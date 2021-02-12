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

(** {2 Simple (int) expressions for FSMs} *)

type ident = string 
  (** The type of identifiers occuring in expressions *)

type value = int 
  (** The type of expression values *)

(** The type of expressions *)
type t = 
  EConst of value            (** Constants *)   
| EVar of ident              (** Input, output or local variable *)
| EBinop of string * t * t   (** Binary operation (["+"], ["-"], ["*"] or ["/"]) *)

type env = (ident * value option) list

  
exception Unknown of ident
exception Unbound of ident
exception Illegal_expr

val to_string: t -> string

val of_string: string -> t

val lookup: env -> ident -> value

val eval: env -> t -> value

val lexer: string -> Genlex.token Stream.t
val parse: Genlex.token Stream.t -> t 

