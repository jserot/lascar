(* Basic example, without state attributes *)

module State = struct
  type t = S0 | S1 | S2
  let compare = Pervasives.compare
  let to_string = function S0 -> "S0" | S1 -> "S1" | S2 -> "S2"
end

module Label = struct
  type t = string
  let compare = Pervasives.compare
  let to_string x = x
end

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
