let _ = Int.of_string "1"
open Utils
open Lascar
open Fsm_expr

let _ = Int.of_string "-1"
let _ = Int.of_string "1+2"
let _ = Int.of_string "-1+2"
let _ = Int.of_string "1+(-2)"
let _ = Int.of_string "1+2*3"
let _ = Int.of_string "1*2+3"
let _ = Int.of_string "1*(2+3)"
let _ = Int.of_string "k+1"
let _ = Int.of_string "-k"
let _ = Int.of_string "(-k)*2"

let _ = Bool.of_string "k"
let _ = Bool.of_string "0"
let _ = Bool.of_string "1"
let _ = Bool.of_string "k && 1"
let _ = Bool.of_string "k && u || v"
let _ = Bool.of_string "k && (u||v)"
let _ = Bool.of_string "u||v && k"
let _ = Bool.of_string "~u"
let _ = Bool.of_string "~(u&&v)"
