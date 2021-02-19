(* FSM model of a modulo-n counter *)
   
open Utils
open Lascar

module S =
    struct
      type t = E
      let compare = compare
      let to_string = function E -> "E"
    end

open S

module F = Fsm.Make(S)(Fsm_value.Int)

let mk n = F.create
  ~inps:[]
  ~outps:[]
  ~vars:["c", ListExt.range Fun.id 0 (n-1)]
  ~states:[E,[]]
  ~istate:("c:=0", E)
  ~trans:[
    E, ("c<"^string_of_int (n-1),"c:=c+1"), E;
    E, ("c="^string_of_int (n-1),"c:=0"), E
    ]

let m1 = mk 4
       
let _ = F.dot_output "m1"  m1

module FF = Conv.Fsm(F)

let m2 = FF.defactorize ~init:(Some ("",(E,["c",0]))) ["c"] m1

let _ = FF.dot_output ~options:[Dot.RankdirLR] "m2" m2
