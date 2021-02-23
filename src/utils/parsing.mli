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

(** {2 Some functions for building recursive descent parsers using the Stream and Genlex modules} *)

(** {3 Higher order parsers} *)

val separated_list :
  string
  -> ?stop_on:string option
  -> (Genlex.token Stream.t -> 'a)
  -> Genlex.token Stream.t
 -> 'a list
 (** [separated_list sep p ss] parses stream [ss] as a [sep]-separated list of elements parsed by [p]
     and returns the list of parsed element. Parsing fails if [p] fails or a token non equal to [sep] 
     occurs in [ss] unless the [stop_on] optional argument is set to [Some t]. In this case, parsing stops
     and returns the list parsed so far when the token equal to [c] is read. This mechanism is required for
     parsing grammars in which empty list can be used as optional parts (see for example the syntax of
     [!Fsm_transition]s. *)

(** {3 Useful wrappers} *)

val run : ('a -> 'b Stream.t) -> ('b Stream.t -> 'c) -> 'a -> ('c, 'b option) result
  (** [run lexer parser src] builds a stream of token [ss] by applying [lexer] to [src], applies [parser]
      to this stream and returns either [Ok r] if parsing succeeds (with result [r]) or [Error e] if 
      parsing fails (with [e=Some t] and [t] the current lookahead token or [e=None] if the remaining
      input stream is empty. *)
