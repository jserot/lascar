(* A sequential adder described as a Mealy automata *)

open Utils
open Lascar

module S =
    struct
      type t = C0 | C1 [@@deriving show {with_path=false}]
      let compare = Stdlib.compare
      let to_string = show
    end

open S 

module ME = Mealy.Make (S)

let me1 = ME.create
  ~inps:["x1"; "x2"]
  ~outps:["s"]
  ~states:[C0; C1]
  ~istate:C0
  ~trans:[C0, (["x1",false;"x2",false],["s",false]), C0;
          C0, (["x1",false;"x2",true],["s",true]), C0;
          C0, (["x1",true;"x2",false],["s",true]), C0;
          C0, (["x1",true;"x2",true],["s",false]), C1;
          C1, (["x1",false;"x2",false],["s",true]), C0;
          C1, (["x1",false;"x2",true],["s",false]), C1;
          C1, (["x1",true;"x2",false],["s",false]), C1;
          C1, (["x1",true;"x2",true],["s",true]), C1]

let _ = ME.dot_output "me1" ~options:[Dot.RankdirLR] me1

module MM = Conv.ToMoore(ME)

let mm1 = MM.conv ~init:(Some (C0,["s",false])) me1

let _ = MM.dot_output "mm1" ~options:[Dot.RankdirLR(*;Ltsa.NoAttr*)] mm1

module ME1 = Conv.ToMealy(MM)

let me2 = ME1.conv mm1

let _ = ME1.dot_output "me2" ~options:[Dot.RankdirLR] me2

