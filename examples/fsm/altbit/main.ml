(* These FSM models check whether values on its input [e] alternates between 0 and 1
   and sets its output [s] whenever this condition fails *)

(* First model : three states and no variable *)

open Lascar

module S1 =
    struct
      type t = Init | E0 | E1 [@@deriving show {with_path=false}]
      let compare = Stdlib.compare
      let to_string = show
    end

open S1

module F1 = Fsm.Make(S1)(Fsm_value.Int)

let m1 =
  let open F1 in
  let open Transition.Condition in
  let open Transition.Action in
  create
    ~inps:["e",[0;1]]
    ~outps:["s",[0;1]]
    ~vars:[]
    ~states:[Init,[]; E0,[]; E1,[]]
    ~istate:([Assign ("s", EConst 0)], Init)
    ~trans:[
      Init, ([Test ("e", "=", EConst 0)],[]), E0;
      Init, ([Test ("e", "=", EConst 1)],[]), E1;
      E0, ([Test ("e", "=", EConst 1)],[Assign ("s", EConst 0)]), E1;
      E0, ([Test ("e", "=", EConst 0)],[Assign ("s", EConst 1)]), E0;
      E1, ([Test ("e", "=", EConst 0)],[Assign ("s", EConst 0)]), E0;
      E1, ([Test ("e", "=", EConst 1)],[Assign ("s", EConst 1)]), E1;
    ]

let _ = F1.dot_output "m1"  m1

(* Idem, but with concrete syntax for initial actions and transitions, thx to dedicated parsers *)
       
let m11 = 
  let open F1 in
  create
    ~inps:["e",[0;1]]
    ~outps:["s",[0;1]]
    ~vars:[]
    ~states:[Init,[]; E0,[]; E1,[]]
    ~istate:(mk_acts "s:=0", Init)
    ~trans:[
      Init, mk_trans "e=0", E0;
      Init, mk_trans "e=1", E1;
      E0, mk_trans "e=1 | s:=0", E1;
      E0, mk_trans "e=0 | s:=1", E0;
      E1, mk_trans "e=0 | s:=0", E0;
      E1, mk_trans "e=1 | s:=1", E1;
    ]

let _ = F1.dot_output "m11"  m11

(* Second model : using a variable to memorize the last read input value *)

module S2 =
    struct
      type t = Init | E [@@deriving show {with_path=false}]
      let compare = Stdlib.compare
      let to_string = show
    end

open S2

module F2 = Fsm.Make(S2)(Fsm_value.Int)

let m2 = 
  let open F2 in 
  create
    ~inps:["e",[0;1]]
    ~outps:["s",[0;1]]
    ~vars:["last",[0;1]]
    ~states:[Init,[]; E,[]]
    ~istate:(mk_acts "s:=0", Init)
    ~trans:[
      Init, (mk_trans "e=0 | last:=0"), E;
      Init, (mk_trans "e=1 | last:=1"), E;
      E, (mk_trans "e=1,last=0 | s:=0,last:=1"), E;
      E, (mk_trans "e=1,last=1 | s:=1"), E;
      E, (mk_trans "e=0,last=1 | s:=0,last:=0"), E;
      E, (mk_trans "e=0,last=0 | s:=1"), E;
    ]

let _ = F2.dot_output "m2"  m2

(* Let's check that, by defactorizing [m2] wrt. variable [last] we get back to [m1] : *)
      
module FF2 = Conv.Fsm(F2)

let m3 = FF2.defactorize ~init:(Some (FF2.mk_acts "s:=0",(Init,["last",0]))) ["last"] m2

let _ = FF2.dot_output "m3" m3
