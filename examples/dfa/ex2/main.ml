(* Conversion from NFA to DFA *)

open Utils
open Lascar

module Int = struct include Builtins.Int let epsilon = 0 end
module String = struct include Builtins.String let epsilon = "" end

module N1 = Nfa.Make (Int) (String)

let nfa1 = N1.create
    ~states:[0;1;2]
    ~symbols:["a";"b"]
    ~istate:0
    ~trans:[0,"a",[0;1];
     0,"b",[0];
     1,"b",[2]]
    ~astates:[2]
    
let _ = N1.dot_output "nfa1" ~options:[Dot.RankdirLR] nfa1

module D1 = Conv.ToDfa(N1)

let dfa1 = D1.conv nfa1

let _ = D1.dot_output "dfa1" dfa1

(* Exhaustive test of equivalence of NFA and DFA (up to a given word length) *)

let check_equiv nfa dfa l =
  let check nfa dfa w = N1.accept nfa w = D1.accept dfa w in
  let syms = N1.symbols' nfa in
  List.for_all (check nfa dfa) (ListExt.power l syms)

let check nfa dfa n = if check_equiv nfa dfa n then Printf.printf "Ok: NFA=DFA (upto %d)\n" n else Printf.printf "** NFA <> DFA\n"

let _ = check nfa1 dfa1 12

(* Ex NFA 2 *)

let nfa2 = N1.create
    ~states:[0;1;2]
    ~symbols:["a";"b"]
    ~istate:0
    ~trans:[0,"a",[0];
     0,"b",[0;1];
     1,"a",[2];
     1,"b",[2]]
    ~astates:[2]

let _ = N1.dot_output "nfa2" nfa2

let dfa2 = D1.conv nfa2

let _ = D1.dot_output "dfa2" dfa2

let _ = check nfa2 dfa2 12

let nfa3 = N1.create
    ~states:[0;1;2]
    ~symbols:["0";"1"]
    ~istate:0
    ~trans:[0,"0",[0;1];
     0,"1",[0];
     1,"0",[2]]
    ~astates:[2]
    
let _ = N1.dot_output "nfa3" nfa3
let dfa3 = D1.conv nfa3
let _ = D1.dot_output "dfa3" dfa3

let _ = check nfa3 dfa3 12


