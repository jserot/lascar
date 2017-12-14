(* Using the [map_xxx] functions for transforming a LTS *)

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

let s' = s
         |> S.map_label String.capitalize_ascii
         |> S.map_state (function q -> q*10)
         |> S.map_attr not
       
let _ = S.dot_output "simple2" s'

