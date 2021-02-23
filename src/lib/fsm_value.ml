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
  type t [@@deriving show]
  val to_string: t -> string
  val binary_ops: (string * ((t -> t -> t) * int)) list (** name, (fun, infix level) *)
  val unary_ops: (char * (t -> t)) list (** name, fun *)
  val of_int: int -> t
  val of_string: string -> t
end

module Int = struct
  type t = int [@@deriving show]
  let compare = Stdlib.compare
  let binary_ops = [
      "+", (( + ), 1);
      "-", (( - ), 1);
      "*", (( * ), 0);
      "/", (( / ), 0);
    ]
  let unary_ops = [
      '-', ( ~-)
    ]
  let of_int x = x 
  let of_string = int_of_string
  let to_string = string_of_int
end

module Bool = struct
  type t = bool [@@deriving show] 
  let compare = Stdlib.compare
  let binary_ops = [
      "||", (( || ), 1);
      "&&", (( && ), 0);
    ]
  let unary_ops = [
      '~', ( not )
    ]
  let of_int x = x <> 0 
  let of_string = bool_of_string
  let to_string = string_of_bool
end
