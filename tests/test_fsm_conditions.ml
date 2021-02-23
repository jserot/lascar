open Lascar

module IntExpr = Fsm_expr.Make(Fsm_value.Int)
module IntCondition = Fsm_condition.Make(IntExpr)
               
let _ =
  List.iter
    (Test.test IntCondition.lexer IntCondition.parse IntCondition.show) [
      "k=1";
      "k>1+2*3";
      "k="
      ]
