module State = struct
  type t = S0 | S1 | S2
  let compare = Pervasives.compare
  let to_string = function S0 -> "S0" | S1 -> "S1" | S2 -> "S2"
end

module Label = Builtins.String

module Attr = Builtins.String

module M1 = Ltsa.Make (State)(Label)(Attr)

open State

let s1 = M1.create
   ~states:[S0, "p"; S1, "q"; S2, "r"]
   ~itrans:["",S0]
   ~trans:[S0,"a",S1;
    S1,"b",S0;
    S1,"c",S2;
    S2,"d",S2;
    S0,"e",S2]
    
let _ = M1.dot_output "s1" ~options:[Dot.RankdirUD] s1

module M2 = Conv.ToLts(M1)

let s2 = M2.conv s1

let _ = M2.dot_output "s2" ~options:[Dot.RankdirUD] s2

module M3 = Conv.FromLts(M2)

let s3 = M3.conv s2

let _ = M3.dot_output "s3" ~options:[Dot.RankdirUD] s3
