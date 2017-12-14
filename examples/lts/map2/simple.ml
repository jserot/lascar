(* Using the [Trans] functor for converting btw LTS *)

module State = struct
  type t = S0 | S1 | S2
  let compare = Pervasives.compare
  let to_string = function S0 -> "S0" | S1 -> "S1" | S2 -> "S2"
end

module Label = Builtins.String

module S = Lts.Make (State) (Label) 

open State

let s = S.create
   ~states:[S0; S1; S2]
   ~itrans:["",S0]
   ~trans:[S0,"a",S1;
    S1,"b",S0;
    S1,"c",S2;
    S2,"d",S2;
    S0,"e",S2]
    
let _ = S.dot_output "simple" ~options:[Dot.RankdirUD] s

module State' = Builtins.Int

module S' = Lts.Make (State') (Label) 

module F = Lts.Trans (S) (S')

let s' = F.map (function S0 -> 0 | S1 -> 1 | S2 -> 2) (function s -> s ^ s) s

let _ = S'.dot_output "simple2" s'

