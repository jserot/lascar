(* Building a Moore automata and converting it to Mealy automata *)

open Utils
open Lascar

module S =
    struct
      type t = Q0 | Q1 | Q2 | Q3
      let compare = compare
      let to_string = function Q0 -> "Q0" | Q1 -> "Q1" | Q2 -> "Q2" | Q3 -> "Q3" 
    end

open S 

module MM = Moore.Make (S)

let mm1 = MM.create
  ~inps:["c"]
  ~outps:["s0";"s1"]
  ~states:[Q0, ["s0",false; "s1",false];
           Q1, ["s0",false; "s1",true];
           Q2, ["s0",true; "s1",false];
           Q3, ["s0",true; "s1",true]]
  ~istate:Q0
  ~trans:[Q0, ["c",false], Q0;
          Q0, ["c",true], Q1;
          Q1, ["c",false], Q1;
          Q1, ["c",true], Q2;
          Q2, ["c",false], Q2;
          Q2, ["c",true], Q3;
          Q3, ["c",false], Q3;
          Q3, ["c",true], Q0]

let _ = MM.dot_output "mm1" mm1

let _ = MM.unwind 4 mm1

let _ = MM.dot_output_execs ~options:[Dot.RankdirUD] "mm1_execs" 4 mm1

module ME = Conv.ToMealy(MM)

let me1 = ME.conv mm1

let _ = ME.dot_output "me1" ~options:[Dot.RankdirUD] me1
