(* FSM model of a signal generator.
   Output [s] goes to 1 with input [start] and stays to 1 for [n] "cycles" *)
   
open Utils
open Lascar

module S =
    struct
      type t = Off | On [@@deriving show {with_path=false}]
      let compare = Stdlib.compare
      let to_string = show
    end

open S

module F = Fsm.Make(S)(Fsm_value.Int)

let mk n = F.create
  ~inps:["start",[0;1]]
  ~outps:["s",[0;1]]
  ~vars:["k", ListExt.range Fun.id 0 n]
  ~states:[Off,[]; On,[]]
  ~istate:("", Off)
  ~trans:[
    Off, ("start=1","k:=0;s:=1"), On;
    On, ("k<"^string_of_int n,"k:=k+1"), On;
    On, ("k="^string_of_int n,"s:=0"), Off
    ]

let m1 = mk 2
       
let _ = F.dot_output "m1"  m1

module FF = Conv.Fsm(F)

let m2 =
  FF.defactorize ~init:(Some ("",(Off,["k",0]))) [] m1

let _ = FF.dot_output ~options:[Dot.RankdirLR] "m2" m2

let _ = FF.dot_output_execs "m2_execs" ~options:[Dot.RankdirLR] 8 m2


