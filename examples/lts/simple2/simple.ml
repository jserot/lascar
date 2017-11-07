(* Basic example, with state attributes *)

module State = struct
  type t = S0 | S1 | S2
  let compare = Pervasives.compare
  let to_string = function
      S0 -> "S0"
    | S1 -> "S1"
    | S2 -> "S2"
end

module Attr = struct
  type t = bool
  let compare = Pervasives.compare
  let to_string = string_of_bool
end

module Label = struct
  type t = string
  let compare = Pervasives.compare
  let to_string x = x
end

module S = Ltsa.Make(State)(Label)(Attr)

open State

let s = S.create
   ~states:[S0,false; S1,true; S2,false]
   ~itrans:["",S0]
   ~trans:[S0,"a",S1;
    S1,"b",S0;
    S1,"c",S2;
    S2,"d",S2;
    S0,"e",S2]
    
let qs, ts  = S.states' s, S.transitions s

let _ = S.dot_output "simple" ~options:[Dot.RankdirLR; Ltsa.Circular] s

let _ = S.tex_output "simple" s

let _ = S.dot_output_execs "simple_execs" ~options:[Dot.RankdirUD] 3 s
