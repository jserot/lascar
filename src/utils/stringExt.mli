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

(** Extension to the {!Stdlib.String} module *)

val is_uppercased: string -> bool
    (** Tests whether the string starts with an uppercase letter *)

val explode: string -> string list
    (** Decomposes a string into characters.
        Example: [explode "abc" = ["a";"b";"c"]] *)

val escape_car: char -> string -> string
    (** [escape_car c s] replaces each occurence of character [c] in [s] by its escaped version.
        Example: [escape 'z' "azb" = "a\zb"].
        This function is useful for generating HTML-compatible strings *)

val remove_car: char -> string -> string
    (** [remove_car c s] removes all occurences of character [c] in [s] *)
 
val concat_sep: string -> string list -> string
    (** [concat_sep sep [s1;..;sN]] returns the concatenation of strings [s1],..,[sN] using [sep] as a separator {i when needed}.
        Examples: [concat "," ["hello";"world"] = "hello,world"]; [concat_sep ":" ["";"xy"] = "xy"]. *)
