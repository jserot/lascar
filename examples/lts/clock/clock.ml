(* A clock described as a synchronized product of two automata :
   one for the hours, the other for the minutes *)

module H =
  Lts.Make (Builtins.Int) (Builtins.String)

module M = H

let mk_state i = i

let mk_label p i = p ^ string_of_int i

let mk_transition p i j = i, mk_label p i, j
let mk_transition' p i = i, mk_label p i, i+1

let create f p n =
  f
    ~states:(ListExt.range mk_state 0 (n-1))
    ~itrans:["",0]
    ~trans:((mk_transition p (n-1) 0) :: ListExt.range (mk_transition' p) 0 (n-2))

let hours_per_day = 4 
let minutes_per_hour = 4

let hr = create H.create "h" hours_per_day
let mn = create M.create "m" minutes_per_hour

let _ = H.dot_output "hours" ~options:[Dot.RankdirUD] hr
let _ = M.dot_output "minutes" ~options:[Dot.RankdirUD] mn

module Clock = Lts.Product (M) (H)

open Lts
open Misc

let clock =
  Clock.synchronized_product
    (ListExt.range (function i -> Some (mk_label "m" i), None) 0 (minutes_per_hour-2)
     @ ListExt.range (function i -> Some (mk_label "m" (minutes_per_hour-1)), Some (mk_label "h" i)) 0 (hours_per_day-1))
    mn
    hr

let _ = Clock.dot_output "clock" ~options:[Dot.RankdirLR] clock
