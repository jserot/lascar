(* Basic example, without state attributes *)

open Lascar
open Utils

module State = struct
  type t = S0 | S1 | S2 [@@deriving show {with_path=false}]
  let compare = Stdlib.compare
  let to_string = show
end

module Label = Builtins.String

module S = Lts.Make (State) (Label) 

open State

(* One-step building *)
   
let s = S.create
   ~states:[S0; S1; S2]
   ~itrans:["",S0]
   ~trans:[S0,"a",S1;
    S1,"b",S0;
    S1,"c",S2;
    S2,"d",S2;
    S0,"e",S2]
    
let qs, ts  = S.states' s, S.transitions s

let _ = S.dot_output "simple" ~options:[Dot.RankdirUD] s

let _ = S.tex_output "simple" s

let _ = S.dot_output_execs "simple_execs" 4 s

(* Incremental building *)

let s' = 
   S.empty
|> (S.add_state S0)
|> (S.add_state S1)
|> (S.add_state S2)
|> (S.add_transition (S0,"a",S1))
|> (S.add_transition (S1,"b",S0))
|> (S.add_transition (S1,"c",S2))
|> (S.add_transition (S2,"d",S2))
|> (S.add_transition (S0,"e",S2))
|> (S.add_itransition ("",S0))

let _ = S.dot_output "simple2" ~options:[Dot.RankdirUD] s'
