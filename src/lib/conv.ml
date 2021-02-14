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

module ToLts (M: Ltsa.T) = struct

  include Lts.Make(M.State)(M.Label)

  let conv m =
    let add_states1 r = M.fold_states (fun q _ r -> add_state q r) m r in
    let add_transitions r = M.fold_transitions (fun t r -> add_transition t r) m r in
    let add_itransitions r = M.fold_itransitions (fun t r -> add_itransition t r) m r in
    empty |> add_states1 |> add_transitions |> add_itransitions

end

module FromLts (M: Lts.T) = struct

  include Ltsa.Make(M.State)(M.Label)(struct type t = unit let compare = compare let to_string _ = "" end)

  let conv m =
    let add_states r = M.fold_states (fun q r -> add_state (q,()) r) m r in
    let add_transitions r = M.fold_transitions (fun t r -> add_transition t r) m r in
    let add_itransitions r = M.fold_itransitions (fun t r -> add_itransition t r) m r in
    empty |> add_states |> add_transitions |> add_itransitions

end

module ToDfa (N : Nfa.T) = struct

  module Q = N.States
  module D = Dfa.Make(N.States)(N.Symbol)

  include D

  let conv nfa =
    let states' = Q.power (N.states nfa) in
    let add_states dfa =
      let is_init_state q =
        match N.istate nfa with
        | None -> false
        | Some q0 -> Q.equal q (Q.singleton q0) in
      let is_acc_state q = not (Q.is_empty (Q.inter q (N.acc_states nfa))) in
      List.fold_left
        (fun m q ->
          D.add_state (q, is_init_state q, is_acc_state q) m)
        dfa
        states' in
    let delta qs s = (* [$(delta(qs,s) = \{ q' \in Q | \exists q \in qs, (q,s,q') \in R \}] *)
       Q.filter
         (fun q' -> Q.exists (fun q -> N.is_transition nfa (q,s,q')) qs)
         (N.states nfa) in
    let add_transition dfa (qs,s) =
       D.add_transition (qs, s, delta qs s) dfa in
    let add_transitions dfa =
      List.fold_left add_transition dfa (Utils.ListExt.cart_prod2 states' (N.symbols' nfa)) in
    D.empty (N.symbols' nfa) |> add_states |> add_transitions |> D.clean
end

module ToMealy (MM: Moore.T) = struct

  module ME = Mealy.Make(MM.State)

  include ME

  let conv mm =
    let add_states m =
      MM.fold_states (fun q _ m -> ME.add_state q m) mm m in
    let add_transitions m =
      MM.fold_transitions
        (fun (q,iv,q') m -> ME.add_transition (q, (iv, MM.attr_of mm q'), q') m)
        mm
        m in
    ME.empty ~inps:(MM.inps mm) ~outps:(MM.outps mm) |> add_states |> add_transitions

end

module ToMoore (ME: Mealy.T) = struct

  (* open Utils *)

  module S = struct
    type t = ME.state * Valuation.Bool.t
    let compare = compare
    let to_string (q,v) = ME.string_of_state q (* ^ " "  ^ Valuation.Bool.to_string v *)
  end

  module MM = Moore.Make(S)

  include MM

  let conv ?(init=None) ?(clean=true) me =
    let ovs = (* The set of all possible output valuations *)
      let names = ME.outps me in
      let vs = Utils.ListExt.cart_prodn (List.map (function _ -> [false;true]) names) in
      List.map (List.combine names) vs in
    let add_states mm =
      let add_sub_states q mm =
        List.fold_left
          (fun m ov -> MM.add_state ((q, ov),ov) m)
          mm
          ovs in
      ME.fold_states (fun q _ m -> add_sub_states q m) me mm in
    let add_transitions mm = 
      let add_sub_transitions (q,(i,o),q') mm =
        List.fold_left
          (fun m ov -> MM.add_transition ((q,ov),i,(q',o)) m)
          mm
          ovs in
      ME.fold_transitions (fun t m -> add_sub_transitions t m) me mm in
    let add_itransitions mm =
      match init with 
        Some q ->
          MM.add_itransition q mm
      | None ->
          let add_sub_itransitions ((i,o),q) mm =
            List.fold_left
              (fun m ov -> MM.add_itransition (q,ov) m)
              mm
              ovs in
          ME.fold_itransitions (fun t m -> add_sub_itransitions t m) me mm in
    let r = MM.empty ~inps:(ME.inps me) ~outps:(ME.outps me) |> add_states |> add_transitions |> add_itransitions in
    if clean then r |> MM.clean else r
end

(* open Utils *)
   
module Fsm (F: Fsm.T) = struct

  module S = struct
    type t = F.state * Valuation.Int.t
    (* In the transformed FSM, states identifiers are paired with a valuation of the defactorized variables *)
    let compare = compare
    let to_string (q,vs) =
      F.string_of_state q ^ Utils.ListExt.to_string (function (n,v) -> string_of_int v) "" vs
  end

  module FF = Fsm.Make(S) 

  include FF

  let copy m =
    (* Add an (empty) valuation to each state of [m] *)
    let add_states mm =
      F.fold_states (fun q ov m -> FF.add_state ((q,Valuation.Int.empty),ov) m)
        m
        mm in
    let add_transitions mm =
      F.fold_transitions
        (fun (q,(conds,acts),q') m -> FF.add_transition ((q,Valuation.Int.empty),(conds,acts),(q',Valuation.Int.empty)) m)
        m
        mm in
    let add_itransitions mm =
      F.fold_itransitions (fun ((conds,acts),q) m -> FF.add_itransition (acts,(q,Valuation.Int.empty)) m)
        m
        mm in
    FF.empty ~inps:(F.inps m) ~outps:(F.outps m) ~lvars:(F.vars m) |> add_states |> add_transitions |> add_itransitions

  let defact ?(init=None) ?(clean=true) var m =
    let dom_v = List.assoc var (FF.vars m) in
    let remove_cond conds = List.filter (function Fsm.Condition.Test(v,_,_) when v=var -> false | _ -> true) conds in
    let remove_act acts = List.filter (function Fsm.Action.Assign(v,_) when v=var -> false | _ -> true) acts in
    let filter_domain (conds,acts) (u,u') =
      (* Tells whether a pair of valuations [(u,u')] for variable [var] is compatible with the
         specified transition conditions and actions *)
      let test_cond u cond = match cond with
        | Fsm.Condition.Test (id,op,texp) as c when id = var ->
            Fsm.Condition.eval [var, Some u] c  (* TO FIX : enrich eval env here ? *)
        | _ -> true in
      let test_conds u = List.for_all (test_cond u) conds in
      let test_act u u' act = match act with
        | Fsm.Action.Assign (v,exp) when v=var ->
           Fsm_expr.eval [var, Some u] exp = u'  (* TO FIX : enrich eval env here ? *)
        | _ -> test_conds u' in
      let test_acts u u' =
          match List.find_opt (function Fsm.Action.Assign (v,_) when v=var -> true | _ -> false) acts with
          | Some a -> (* If the list of actions contains an assignment to [var] then it is used to restrict the domain ... *)
             test_act u u' a
          | None -> (* ... else, the domain is restricted by the list of conditions *)
             List.for_all (test_cond u') conds in
      test_conds u && test_acts u u' in
    let add_states mm =
      (* Each state [(q,vv)] in [Q] gives a set of states [{(q,(vv++(v=u)) | u in domain(v)}] in Q'.
         The state attributes are _not_ affected. *)
      let add_sub_states (q,vv) attr mm =
        List.fold_left
          (fun m u -> FF.add_state ((q, Valuation.Int.add var u vv), attr) m)
          mm
          dom_v in
      FF.fold_states
        (fun q attr mm -> add_sub_states q attr mm)
        m
        mm in
    let add_transitions mm =
      let add_sub_transitions ((q,vv),(conds,acts),(q',vv')) mm =
        let d2v = List.filter (filter_domain (conds,acts)) (Utils.ListExt.cart_prod2 dom_v dom_v) in
        let conds' = remove_cond conds in
        let acts' = remove_act acts in
        List.fold_left
          (fun m (u,u') ->
             let qq = q, Valuation.Int.add var u vv in
             let qq' = q', Valuation.Int.add var u' vv' in
             FF.add_transition (qq,(conds',acts'),qq') m)
          mm
          d2v in
      FF.fold_transitions (fun t m -> add_sub_transitions t m) m mm in
    let add_itransitions mm =
      match init with
        Some (acts,q) ->
          FF.add_itransition' (acts,q) mm
      | None ->
         let add_sub_itransitions ((conds,acts),(q,ov)) mm =
           let d2v = List.filter (filter_domain ([],acts)) (Utils.ListExt.cart_prod2 dom_v dom_v) in
           List.fold_left
             (fun m (_,u') -> 
                let ov' = Valuation.Int.add var u' ov in 
                try FF.add_itransition (remove_act acts,(q,ov')) m
                with FF.M.Invalid_state q -> failwith ("add_itrans: q=" ^ (FF.string_of_state q)) )
             mm
             d2v in
        FF.fold_itransitions (fun t m -> add_sub_itransitions t m) m mm in
    let r = 
      FF.empty ~inps:(FF.inps m) ~outps:(FF.outps m) ~lvars:(FF.vars m) |> add_states |> add_transitions |> add_itransitions in
    if clean then FF.clean r else r

  let defactorize ?(init=None) ?(clean=true) vars m =
    let dvars = match vars with
      [] -> List.map fst (F.vars m) 
    | vs -> vs in
    List.fold_left (fun m var -> defact ~init:init ~clean:clean var m) (copy m) dvars

end
