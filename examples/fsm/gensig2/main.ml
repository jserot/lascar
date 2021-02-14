(* This is a reformulation of ../gensig1 in which the output [s] is assigned to states *)
   
open Utils
open Lascar

module S =
    struct
      type t = Off | On
      let compare = compare
      let to_string = function Off -> "Off" | On -> "On"
    end

open S

module F = Fsm.Make(S)

let mk n = F.create
  ~inps:["start",[0;1]]
  ~outps:["s",[0;1]]
  ~vars:["k", ListExt.range Fun.id 0 n]
  ~states:[Off,["s",0]; On,["s",1]]
  ~istate:("", Off)
  ~trans:[
    Off, ("start=1","k:=0"), On;
    On, ("k<"^string_of_int n,"k:=k+1"), On;
    On, ("k="^string_of_int n,""), Off
    ]

let m1 = mk 2
       
let _ = F.dot_output "m1"  m1

module FF = Conv.Fsm(F)

let m2 = FF.defactorize ~init:(Some ("",(Off,["k",0]))) [] m1

let _ = FF.dot_output ~options:[Dot.RankdirLR] "m2" m2

let _ = FF.dot_output_execs "m2_execs" ~options:[Dot.RankdirLR] 8 m2


