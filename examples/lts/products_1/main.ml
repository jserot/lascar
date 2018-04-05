(* Product, in different flavors, of LTSs *)

open Utils
open Lascar

module S = Lts.Make (Builtins.String) (Builtins.Int)

let a1 = S.create ~states:["A";"B"] ~trans:["A",1,"B"; "B",2,"A"] ~itrans:[0,"A"]
let a2 = S.create ~states:["u";"v"] ~trans:["u",3,"v"; "v",4,"u"; "v",5,"v"] ~itrans:[0,"u"] 

let _ = S.succs a1 "A"

let _ = S.dot_output "A1" ~fname:"a1.dot" ~options:[Dot.RankdirUD] a1
let _ = S.dot_output "A2" ~fname:"a2.dot" ~options:[Dot.RankdirUD] a2
let _ = S.tex_output "A1" ~fname:"./a1.tex" a1  
let _ = S.tex_output "A2" ~fname:"./a2.tex" a2

module P = Lts.Product (S) (S)

let a12s = P.synchronous_product a1 a2
let _ = P.dot_output "A12s" ~fname:"a12s.dot" a12s
let _ = P.tex_output "A12s" ~fname:"./a12s.tex" a12s

let a12a = P.asynchronous_product a1 a2
let _ = P.dot_output "A12a" ~fname:"a12a.dot" a12a
let _ = P.tex_output "A12a" ~fname:"./a12a.tex" a12a

let a12f = P.free_product a1 a2
let _ = P.dot_output "A12f" ~fname:"a12f.dot" a12f
let _ = P.tex_output "A12f" ~fname:"./a12f.tex" a12f

module S' =
  Lts.Make 
    (struct type t = string let compare = compare let to_string x = x end)
    (struct type t = string let compare = compare let to_string x = x end)

module F = Lts.Trans (S) (S')

let b1 = F.map (fun x -> "_" ^ x) (fun x -> string_of_int (x+1)) a1

let _ = S'.dot_output "B1" ~fname:"b1.dot" ~options:[Dot.RankdirUD] b1
