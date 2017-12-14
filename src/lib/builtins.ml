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

(** Pre-defined functor arguments *)

module Int = struct
  type t = int 
  let compare = Pervasives.compare
  let to_string = string_of_int
end

module String = struct
  type t = string
  let compare = Pervasives.compare
  let to_string x = x
end

module Bool = struct
  type t = bool
  let compare = Pervasives.compare
  let to_string = string_of_bool
end
