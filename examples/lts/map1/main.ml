(* Using the [map_xxx] functions for transforming a LTS *)

open Utils
open Lascar

module S = Ltsa.Make (Builtins.Int) (Builtins.String) (Builtins.Bool)

let s = S.create
   ~states:[0,true; 1,false; 2,false]
   ~itrans:["i",0]
   ~trans:[0,"a",1;
    1,"b",0;
    1,"c",2;
    2,"d",2;
    0,"e",2]
    
let _ = S.dot_output "simple" ~options:[Dot.RankdirUD] s

let s1 = s
         |> S.map_label String.capitalize_ascii
         |> S.map_state (function q -> q*10)
         |> S.map_attr not
       
let _ = S.dot_output "simple1" s1

let s2 = 
  let relabel q q' l = match q with
  | Some q -> String.capitalize_ascii l ^ "[" ^ string_of_int q ^ "->" ^ string_of_int q' ^ "]"
  | None ->  String.capitalize_ascii l ^ "[->" ^ string_of_int q' ^ "]" in
  s |> S.map_state_attr (function (q,a) -> q*10, not a)
    |> S.map_transition (function (q,l,q') -> q, relabel q q' l, q')
       
let _ = S.dot_output "simple2" s2

