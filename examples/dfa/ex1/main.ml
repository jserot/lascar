(* Deterministic finite automata (DFA) *)

open Utils
open Lascar

module Int = struct include Builtins.Int let epsilon = 0 end
module String = struct include Builtins.String let epsilon = "" end

module A = Dfa.Make (Int) (String)

let a1 = A.create
    ~states:[0;1;2]
    ~symbols:["a";"b"]
    ~istate:0
    ~trans:[0,"a",0;
            0,"b",1;
            1,"a",0;
            1,"b",2]
    ~astates:[2]

let _ = A.symbols' a1
let _ = A.states' a1
let _ = A.acc_states' a1
let _ = A.transitions a1
    
let _ = A.dot_output "a1" a1

let a1b =
   A.empty ["a";"b"]
|> A.add_state (0,true,false)
|> A.add_state (1,false,false)
|> A.add_state (2,false,true)
|> A.add_transition (0,"a",0)
|> A.add_transition (0,"b",1)
|> A.add_transition (1,"a",0)
|> A.add_transition (1,"b",2)

let _ = A.dot_output "a1b"  a1b

let trans a q s =
  try A.trans a q s
  with A.Stuck (q,s) ->
    Printf.printf "** a1: no transition from %s with %s\n" (Builtins.Int.to_string q) (ListExt.to_string Fun.id "," s);
    0

let _  = trans a1 0 "a"
let _  = trans a1 0 "b"
let _  = trans a1 1 "a"
let _  = trans a1 2 "a"

let _ = List.map (A.succs' a1) [0;1;2]

let _ = List.map (A.is_in_cycle a1) [0;1;2]

let a2 = A.totalize a1 100

let _ = A.dot_output "a2" a2

let _ = A.unwind 4 a1

let _ = A.dot_output_execs ~options:[Dot.RankdirUD] "a1_execs" 4 a1
