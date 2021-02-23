open Lascar

module IntExpr = Fsm_expr.Make(Fsm_value.Int)
module IntAction = Fsm_action.Make(IntExpr)
               
let _ =
  List.iter
    (Test.test IntAction.lexer IntAction.parse IntAction.show) [
      "k:=1+2";
      "k:=k+1";
      "k=1"
      ]
