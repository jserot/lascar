(* A modulo-4 counter defined as the synchronized product of two modulo-2 counters *)

open Utils
open Lascar

module Bit = Lts.Make (Builtins.Int) (Builtins.String)

let a0 = Bit.create ~states:[0;1] ~itrans:["",0] ~trans:[0,"u",1; 1,"d",0]
let a1 = Bit.create ~states:[0;1] ~itrans:["",0] ~trans:[0,"u",1; 1,"d",0]

let _ = Bit.dot_output "a1" ~options:[Dot.RankdirUD] a1
let _ = Bit.dot_output "a0" ~options:[Dot.RankdirUD] a0
(* let _ = Bit.tex_output "a1" "a1.tex" a1 *)
(* let _ = Bit.tex_output "a0" "a0.tex" a0 *)

module Ctr = Lts.Product(Bit)(Bit)

let a = Ctr.synchronized_product [Some "u", None; Some "d", Some "u"; Some "d", Some "d"] a0 a1
let _ = Ctr.dot_output "a" ~options:[Dot.RankdirLR] a
let _ = Ctr.tex_output "a" a

