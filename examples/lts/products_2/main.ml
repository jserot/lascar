(* Internal and external product of LTSAs *)

open Utils
open Lascar

module S = Ltsa.Make(Builtins.String)(Builtins.Int)(Builtins.String)

let a1 = S.create ~states:["A","";"B",""] ~trans:["A",1,"B"; "B",2,"A"] ~itrans:[0,"A"]
let a2 = S.create ~states:["u","";"v",""] ~trans:["u",3,"v"; "v",4,"u"; "v",5,"v"] ~itrans:[0,"u"] 

let _ = S.succs a1 "A"

let _ = S.dot_output "A1" ~options:[Dot.RankdirUD] a1
let _ = S.dot_output "A2" ~options:[Dot.RankdirUD] a2
let _ = S.tex_output "A1" a1  
let _ = S.tex_output "A2" a2

module M = struct
  type state = string
  type label = int
  type attr = string
  let merge_state (s1,s2) = s1 ^ s2
  let merge_label = function None, None -> 0 | Some l1, None -> l1 | None, Some l2 -> l2 | Some l1, Some l2 -> 10*l1+l2
  let merge_attr (a1,a2) = a1 ^ a2
end

module P1 = Ltsa.IProduct (S) (M)

let p1 = P1.synchronous_product a1 a2
let _ = P1.dot_output "IntProd" p1


module P2 = Ltsa.Product (S) (S)

let p2 = P2.synchronous_product a1 a2
let _ = P2.dot_output "ExtProd" p2
