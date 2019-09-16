(**********************************************************************)
(*                                                                    *)
(*                              LASCAr                                *)
(*                                                                    *)
(*  Copyright (c) 2017-present, Jocelyn SEROT.  All rights reserved.  *)
(*                                                                    *)
(*  This source code is licensed under the license found in the       *)
(*  LICENSE file in the root directory of this source tree.           *)
(*                                                                    *)
(**********************************************************************)

let fold_lefti f z xs =
  fst (List.fold_left (fun (z,i) x -> f z i x, i+1) (z,0) xs)

let fold_righti f xs z =
  fst (List.fold_right (fun x (z,i) -> f i x z, i-1) xs (z,List.length xs-1))

let rec split3 = function
    [] -> ([], [], [])
  | (x,y,z)::l ->
      let (rx, ry, rz) = split3 l in (x::rx, y::ry, z::rz)

let rec combine3 l1 l2 l3 =
  match (l1, l2, l3) with
    ([], [], []) -> []
  | (a1::l1, a2::l2, a3::l3) -> (a1, a2, a3) :: combine3 l1 l2 l3
  | (_, _, _) -> invalid_arg "ListExt.combine3"

let to_string f sep l =
  let rec h = function
      [] -> ""
    | [x] -> f x
    | x::xs -> f x ^ sep ^ h xs in
  h l

let range f n1 n2 =
  let rec h i = if i <= n2 then f i :: h (i+1) else [] in
  h n1

let index_of e l =
  let rec h i = function
      [] -> failwith "index_of"
    | x::xs -> if x=e then i else h (i+1) xs in
  h 0 l

let flat_map f l = List.concat (List.map f l)

let inter l1 l2 = List.filter (function x -> List.mem x l2) l1

let rec union l1 l2 = match l1, l2 with
  [], _ -> l2
| _, [] -> l1
| x1::xs1, x2::xs2 ->
   let r = compare x1 x2 in
   if      r = 0 then x1 :: union xs1 xs2 (* x1 = x2 *)
   else if r < 0 then x1 :: union xs1 l2  (* x1 < x2 *)
   else               x2 :: union l1 xs2  (* x1 > x2 *)

let merge ?(cmp=Stdlib.compare) l1 l2  =
  let rec h l1 l2 = match l1, l2 with
    [], [] -> []
  | l1, [] -> l1
  | [], l2 -> l2
  | x1::xs1, x2::xs2 -> if cmp x1 x2 < 0 then x1 :: h xs1 l2 else x2 :: h l1 xs2 in
  h l1 l2

let prod p l1 l2 = flat_map (function e1 -> List.map (p e1) l2) l1

let cart_prod2 l1 l2 = prod (fun x y -> x,y) l1 l2

let cart_prod3 l1 l2 l3 = prod (fun (x,y) z -> x,y,z) (cart_prod2 l1 l2) l3

let prod3 p l1 l2 l3 = List.map (function x,y,z -> p x y z) (cart_prod3 l1 l2 l3)

let cart_prodn ls =
  let h l = List.map (function e -> [e]) l in
  let p l1 l2 = prod (fun x y -> x@[y]) l1 l2 in
  match ls with
    [] -> []
  | [l] -> h l
  | l::ls -> List.fold_left p (h l) ls

let filter_map f g l = List.map g (List.filter f l)

let iter_sep f sep l =
  let rec h = function
      [] -> ()
    | [x] -> f x
    | x::xs -> f x; sep (); h xs in
  h l

let iter_fst f l =
  ignore (List.fold_left (fun z x -> f z x; false) true l)

let remove e l = List.filter (function e' -> e <> e') l

let rec power_set = function
  | [] -> [[]]
  | (x::xs) -> let xss = power_set xs in xss @ (List.map (fun xs -> x::xs) xss)
                                                 
let rec power n l =
  if n = 0 then [[]]
  else flat_map (function l' -> List.map (function x -> x::l') l) (power (n-1) l)

let assoc ?(cmp=Stdlib.compare) k l =
  let rec h = function
    [] -> raise Not_found
  | (k',v)::l -> if cmp k k' = 0 then v else h l in
  h l

let mem_assoc ?(cmp=Stdlib.compare) k l = 
  let rec h = function
  | [] -> false
  | (k',_)::l -> cmp k k' = 0 || h l in
  h l

let update_assoc ?(cmp=Stdlib.compare) f k v l =
  let rec h = function 
  | [] -> []
  | (k',v')::rest -> let v'' = if cmp k k' = 0 then f v' v else v' in (k',v'') :: h rest  in
  h l

let replace_assoc ?(cmp=Stdlib.compare) k v l = update_assoc ~cmp:cmp (fun _ v -> v) k v l 

let partition ?(cmp=Stdlib.compare) l =
  List.fold_left
    (fun acc (x,y) ->
      if List.mem_assoc x acc
      then update_assoc ~cmp:cmp (fun vs v -> v::vs) x y acc
      else (x,[y])::acc)
    []
    l 

let scatter p l = 
  let m = List.fold_left (fun acc x -> max acc (p x)) 0 l in
  let t = Array.make (m+1) [] in 
  List.iter (function x -> let k = p x in t.(k) <- x::t.(k)) l;
  t

let rec parse sep p s =
  match Stream.peek s with
  | Some _ ->
     let e = p s in
     let es = parse_aux p sep s in
     e::es
  | None ->
     []
              
and parse_aux p sep s =
  match Stream.next s with
  | Genlex.Kwd sep' ->
     if sep=sep' then parse sep p s else raise Stream.Failure
  | _ ->
     raise Stream.Failure
  | exception Stream.Failure ->
     []
