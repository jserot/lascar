(* FSM model of a signal generator.
   Output [s] goes to 1 with input [start] and stays to 1 for [n] "cycles" *)
   
open Utils
open Lascar

module S =
    struct
      type t = E0 | E1
      let compare = compare
      let to_string = function E0 -> "E0" | E1 -> "E1"
    end

open S

module F = Fsm.Make(S)

let mk n = F.create
  ~inps:["start",[0;1]]
  ~outps:["s",[0;1]]
  ~vars:["k", ListExt.range Fun.id 0 n]
  ~states:[E0,[]; E1,[]]
  ~istate:("", E0)
  ~trans:[
    E0, ("start=1","k:=0;s:=1"), E1;
    E1, ("k<"^string_of_int n,"k:=k+1"), E1;
    E1, ("k="^string_of_int n,"s:=0"), E0
    ]

let m1 = mk 2
       
let _ = F.dot_output "m1"  m1

module FF = Conv.Fsm(F);;

let m2 = FF.defactorize ~init:(Some ("",(E0,["k",0]))) [] m1

let _ = FF.dot_output ~options:[Dot.RankdirLR] "m2" m2

let _ = FF.dot_output_execs "m2_execs" ~options:[Dot.RankdirLR] 8 m2


