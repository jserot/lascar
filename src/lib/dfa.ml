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

  include Nfa.T

  module NFA: Nfa.T with type state = state and type symbol = symbol

  exception Non_deterministic

  val create:
      states:state list ->
      symbols:symbol list ->
      istate:state ->
      trans:(state * symbol * state) list ->
      astates:state list ->
      t

  exception Stuck of state * symbol list

  val trans: t -> state -> symbol -> state

  val trans_hat: t -> state -> symbol list -> state

  val nfa_of: t -> NFA.t
  val of_nfa: NFA.t -> t

end

module Make (S : Ltsa.STATE) (L : Nfa.SYMBOL) = 
struct

  module NFA = Nfa.Make(S)(L)

  include NFA

  exception Stuck of state * symbol list

  let nfa_of a = a
  let of_nfa a = a

  exception Non_deterministic

  let create ~states:qs ~symbols:ss ~istate:q0 ~trans:ts ~astates:fs = 
    (* Check for non-deterministic transitions *)
    let ts' = List.fold_left
      (fun acc (q,l,q') -> if List.mem_assoc (q,l) acc then raise Non_deterministic else ((q,l),q')::acc)
      []
      ts in
    NFA.create ~states:qs ~symbols:ss ~istate:q0 ~trans:(List.map (fun ((q,l),q') -> (q,l,[q'])) ts') ~astates:fs

  let add_transition ((q1,s,q2) as t) a =
    let incompatible_trans (q1',s',q2') = State.compare q1 q1' = 0 && Symbol.compare s s' = 0 && State.compare q2 q2' <> 0 in
    if List.exists incompatible_trans (transitions a) then raise Non_deterministic
    else NFA.add_transition t a

  let trans a q s = match States.elements (NFA.trans a q s) with 
    [] -> raise (Stuck (q,[s]))
  | [q'] -> q'
  | _ -> failwith "Dfa.trans" (* should not happen *)


  let trans_hat a q s = match States.elements (NFA.trans_hat a q s) with
    [] -> raise (Stuck (q,s))
  | [q'] -> q'
  | _ -> failwith "Dfa.trans_hat" (* should not happen *)
end

module Trans (S1: T) (S2: T) =
struct
  module F = Nfa.Trans (S1.NFA) (S2.NFA)
  let map fq fs s1 =
    let p = F.map fq fs (S1.nfa_of s1) in
    S2.of_nfa p
end


module Product (S1: T) (S2: T with type symbol = S1.symbol and type Symbols.t = S1.Symbols.t and type NFA.Symbols.t = S1.NFA.Symbols.t) =
struct
  module S = OrderedTypeExt.Pair (S1.State) (S2.State)
  module L = S1.Symbol
  module R = Make(S)(L)
  include R
  module P = Nfa.Product (S1.NFA) (S2.NFA)
  let product s1 s2 =
    let p = P.product (S1.nfa_of s1) (S2.nfa_of s2) in
    let add_state q f r = R.add_state (q,P.is_init_state p q,f) r in
    let add_transition (q,s,q') r = R.add_transition (q, s, q') r in
    R.empty (P.symbols' p)
    |> (P.fold_states add_state p)
    |> (P.fold_transitions add_transition p)
end
