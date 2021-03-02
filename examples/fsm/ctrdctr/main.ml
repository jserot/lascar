(* Several FSM models of a modulo-n counter/decounter, all related by factorisation/defactorisation opns *)
   
(* The most "intuitive" model, with two states and one variable  *)
   
open Utils
open Lascar

module S1 =
    struct
      type t = Inc | Dec [@@deriving show {with_path=false}]
      let compare = Stdlib.compare
      let to_string = show
    end

open S1

module F1 = Fsm.Make(S1)(Fsm_value.Int)

let mk1 n =
  let open F1 in
  let open Transition.Condition in
  create
    ~inps:[]
    ~outps:[]
    ~vars:["c", ListExt.range Fun.id 0 (n-1)]
    ~states:[Inc,[];Dec,[]]
    ~istate:(F1.mk_acts "c:=0", Inc)
    ~trans:[
      Inc, ([Test ("c", "<", EBinop ("-", EConst n, EConst 1))], mk_acts "c:=c+1"), Inc;
      Inc, ([Test ("c", "=", EBinop ("-", EConst n, EConst 1))], mk_acts "c:=c-1"), Dec;
      Dec, mk_trans "c>0 | c:=c-1", Dec;
      Dec, mk_trans "c=0 | c:=c+1", Inc;
    ]

let m1 = mk1 4
       
let _ = F1.dot_output "m1"  m1

module FF1 = Conv.Fsm(F1)

(* By defactorizing [m1] wrt. the [c] variable, we get the classical, variable-less automata *)

let m2 = FF1.defactorize ~init:(Some ([],(Inc,["c",0]))) ["c"] m1

let _ = FF1.dot_output ~options:[Dot.RankdirLR] "m2" m2

(* We could also have started with a single state model, using a second variable [dir] to 
   distinguish btw the counting and decounting phases : *)
      
module S2 =
    struct
      type t = E [@@deriving show {with_path=false}]
      let compare = compare
      let to_string = function E -> "E"
    end

open S2

module F2 = Fsm.Make(S2)(Fsm_value.Int)

let mk2 n =
  let open F2 in
  create
    ~inps:[]
    ~outps:[]
    ~states:[E,[]]      
    ~vars:["dir", [0;1];  (* dir=0 => inc; dir=1 => dec *)
           "c", ListExt.range Fun.id 0 (n-1)] 
    ~istate:(mk_acts "c:=0,dir:=0", E)
    ~trans:[
      E, ([Test ("dir", "=", EConst 0); Test ("c", "<", EBinop ("-", EConst n, EConst 1))], mk_acts "c:=c+1"), E;
      E, ([Test ("dir", "=", EConst 0); Test ("c", "=", EBinop ("-", EConst n, EConst 1))], mk_acts "c:=c-1,dir:=1"), E;
      E, mk_trans "dir=1,c>0 | c:=c-1", E;
      E, mk_trans "dir=1,c=0 | c:=c+1,dir:=0", E;
    ]

let m3 = mk2 4
       
let _ = F2.dot_output "m3"  m3

module FF2 = Conv.Fsm(F2)

(* Defactorizing [m3] wrt. to the [dir] variable gives back [m1] : *)
          
let m4 = FF2.defactorize ["dir"] m3

let _ = FF2.dot_output ~options:[Dot.RankdirUD] "m4" m4

(* Defactorizing [m3]  wrt. to the [c] variable leads to a less-intuitive, but working automata : *)
          
let m5 = FF2.defactorize ["c"] m3

let _ = FF2.dot_output ~options:[Dot.RankdirLR] "m5" m5

(* Finally, defactorizing [m3]  wrt. to both [dir] and [c] variables gives back [m2] : *)
          
let m6 = FF2.defactorize ["dir";"c"] m3

let _ = FF2.dot_output ~options:[Dot.RankdirLR] "m6" m6
