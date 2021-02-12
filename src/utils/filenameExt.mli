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

(** {2 Extension to the [Stdlib.Filename] module} *)

val split_suffix: string -> string * string
    (** [split_suffix fname] removes any suffix from [fname] and returns both the resulting name and the removed suffix *)

val add_before_suffix: string -> string -> string
    (** if [split_suffix fname] gives [(base,suff)], [add_before_suffix fname s] returns [base^s^suff] *)

val replace_suffix: string -> string -> string
    (** if [split_suffix fname] gives [(base,suff)], [replace_suffix fname s] returns [base^"."^s] *)

