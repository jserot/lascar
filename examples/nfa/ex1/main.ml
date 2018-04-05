(* Basic building and manipulation of non-determinitic finite automata (NFA) *)

open Utils
open Lascar

module Int = struct include Builtins.Int let epsilon = 0 end
module String = struct include Builtins.String let epsilon = "" end

module A = Nfa.Make (Int) (String)

let a1 = A.create
    ~states:[0;1;2]
    ~symbols:["a";"b"]
    ~istate:0
    ~trans:[0,"a",[0;1];
             0,"b",[0];
             1,"b",[2]]
    ~astates:[2]
    
let _ = A.dot_output "a1" a1

let accept ss =
  Printf.printf "Symbol sequence [%s] is %s\n" 
    (ListExt.to_string String.to_string "" ss)
    (if A.accept a1 ss then "accepted" else "not accepted")
  
let _ = List.iter accept [["a";"b"]; ["b";"b"]]

let a2 = A.totalize a1 100

let _ = A.dot_output "a2" a2

let _ = A.unwind 4 a1

let _ = A.dot_output_execs ~options:[Dot.RankdirLR] "a1_execs" 2 a1

module A' = Nfa.Make (String) (Int)

module F = Nfa.Trans (A) (A')

let a3 = F.map (function x -> "S" ^ string_of_int x) (function "a" -> 1 | "b" -> 2 | _ -> 0) a1

let _ = A'.dot_output "a3" a3
