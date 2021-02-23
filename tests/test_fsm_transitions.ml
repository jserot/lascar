open Lascar

module IntExpr = Fsm_expr.Make(Fsm_value.Int)
module Transition = Fsm_transition.Make(IntExpr)
               
let _ =
  List.iter
    (Test.test Transition.lexer Transition.parse Transition.show) [
      "";
      "k>1";
      "k>1,c=0";
      "k>1 | k:=k+1";
      "|s:=0"
      ]
