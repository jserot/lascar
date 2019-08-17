(* Mutual exclusion protocal modelized as a synchronized product of three automata *)

open Utils
open Lascar

module S = Lts.Make (Builtins.Int) (Builtins.String)

let proc_a = S.create
    ~states:[1;2;3]
    ~itrans:["",1]
    ~trans:[1,"QA",2; 2,"PA",3; 3,"RA",1]

(* Qx = Process X waits for resource 
   Px = Process X uses resource
   Rx = Process X releases resource *)
           
let proc_b = S.create
    ~states:[1;2;3]
    ~itrans:["",1]
    ~trans:[1,"QB",2; 2,"PB",3; 3,"RB",1]

let jeton = S.create
    ~states:[1;0]
    ~itrans:["",1]
    ~trans:[1,"a",0; 0,"r",1]

(* a = resource attributes, b = resource released *)
          
let _ = S.dot_output "A" ~fname:"proc_a.dot" ~options:[Dot.RankdirUD] proc_a
let _ = S.dot_output "B" ~fname:"proc_b.dot" ~options:[Dot.RankdirUD] proc_b
let _ = S.dot_output "J" ~fname:"jeton.dot" ~options:[Dot.RankdirUD] jeton

let sync_set = [
  Some "QA", None, None;
  None,None,Some "QB";
  Some "PA", Some "a", None;
  Some "RA", Some "r", None;
  None, Some "a", Some "PB";
  None, Some "r", Some "RB"
  ]

module P = Lts.Product3 (S)(S)(S)

let em1 = P.synchronized_product sync_set proc_a jeton proc_b

let _ = P.dot_output "em" ~fname:"em.dot" ~options:[Dot.RankdirUD] em1

let sync_set' = sync_set @ 
  [Some "PA", Some "a", Some "QB";
   Some "RA", Some "r", Some "QB";
   Some "QA", Some "a", Some "PB";
   Some "QA", Some "r", Some "RB";
   Some "QA", None, Some "QB"]

let em' = P.synchronized_product sync_set' proc_a jeton proc_b

let _ = P.dot_output "em_bis" ~fname:"em_bis.dot" ~options:[Dot.RankdirUD] em'

(* let _ = Lts.tex_output "A" "./em_a.tex" proc_a *)
(* let _ = Lts.tex_output "B" "./em_b.tex" proc_b *)
(* let _ = Lts.tex_output "J" "./em_jeton.tex" jeton *)
(* let _ = Lts.tex_output "A||J||B" "./em.tex" ~listed_transitions:(Misc.Only sync_set) em *)

