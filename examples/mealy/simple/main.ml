(* Building a Mealy automata and converting it to Moore automata *)

open Utils
open Lascar

module S =
    struct
      type t = A | B
      let compare = compare
      let to_string = function A -> "A" | B -> "B"
    end

module ME = Mealy.Make (S)

open S

let me1 = ME.create
  ~inps:["e"]
  ~outps:["s"]
  ~states:[A; B]
  ~istate:A
  ~trans:[A, (["e",false],["s",false]), A;
          A, (["e",true],["s",false]), B;
          B, (["e",true],["s",false]), B;
          B, (["e",false],["s",true]), A]

let _ = ME.dot_output "me1" ~options:[Dot.RankdirLR] me1

module MM = Conv.ToMoore(ME)

let mm1 = MM.conv ~init:(Some (A,["s",true])) me1

let _ = MM.dot_output "mm1" ~options:[Dot.RankdirLR] mm1
