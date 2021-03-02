(* FSM model of a modulo-n counter *)
   
open Utils
open Lascar

module S =
    struct
      type t = E [@@deriving show {with_path=false}]
      let compare = Stdlib.compare
      let to_string = show
    end

open S

module F = Fsm.Make(S)(Fsm_value.Int)

let mk n =
  let open F in
  create
    ~inps:[]
    ~outps:[]
    ~vars:["c", ListExt.range Fun.id 0 (n-1)]
    ~states:[E,[]]
    ~istate:(mk_acts "c:=0", E)
    ~trans:[
      E, ([Test ("c", "<", EBinop ("-", EConst n, EConst 1))], mk_acts "c:=c+1"), E;
      E, ([Test ("c", "=", EBinop ("-", EConst n, EConst 1))], mk_acts "c:=0"), E
    ]

let m1 = mk 4
       
let _ = F.dot_output "m1"  m1

module FF = Conv.Fsm(F)

let m2 = FF.defactorize ~init:(Some ([],(E,["c",0]))) ["c"] m1

let _ = FF.dot_output ~options:[Dot.RankdirLR] "m2" m2
