(* Using the [map_xxx] functions for transforming a LTS *)

module State = struct
  type t = int 
  let compare = Pervasives.compare
  let to_string = string_of_int
end

module Label = struct
  type t = string
  let compare = Pervasives.compare
  let to_string x = x
end

module Attr = struct
  type t = bool
  let compare = Pervasives.compare
  let to_string = string_of_bool
end

module S = Ltsa.Make (State) (Label) (Attr)

open State

let s = S.create
   ~states:[0,true; 1,false; 2,false]
   ~itrans:["i",0]
   ~trans:[0,"a",1;
    1,"b",0;
    1,"c",2;
    2,"d",2;
    0,"e",2]
    
let _ = S.dot_output "simple" ~options:[Dot.RankdirUD] s

let s' = s
         |> S.map_label String.capitalize_ascii
         |> S.map_state (function q -> q*10)
         |> S.map_attr not
       
let _ = S.dot_output "simple2" s'

