(* This is a reformulation of ../gensig1 in which the output [s] is assigned to states *)
   
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

let mk n =
  let open F in
  create
    ~inps:["start",[0;1]]
    ~outps:["s",[0;1]]
    ~vars:["k", ListExt.range Fun.id 0 n]
    ~states:[Off,["s",0]; On,["s",1]]
    ~istate:([], Off)
    ~trans:[
      Off, mk_trans "start=1 | k:=0", On;
      On, ([Test ("k", "<", EConst n)], mk_acts "k:=k+1"), On; 
      On, ([Test ("k", "=", EConst n)], []), Off
    ]

let m1 = mk 2
       
let _ = F.dot_output "m1"  m1

module FF = Conv.Fsm(F)

let m2 = FF.defactorize ~init:(Some ([],(Off,["k",0]))) [] m1

let _ = FF.dot_output ~options:[Dot.RankdirLR] "m2" m2

let _ = FF.dot_output_execs "m2_execs" ~options:[Dot.RankdirLR] 8 m2


