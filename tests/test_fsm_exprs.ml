open Lascar

module IntExpr = Fsm_expr.Make(Fsm_value.Int)
               
let _ =
  List.iter
    (Test.test IntExpr.lexer IntExpr.parse IntExpr.show) [
      "1";
      "-1";
      "1+2";
      "-1+2";
      "1+(-2)";
      "1+2*3";
      "1*2+3";
      "1*(2+3)";
      "k+1";
      "-k";
      "(-k)*2";
      "+"
      ]

module BoolExpr = Fsm_expr.Make(Fsm_value.Bool)
               
let _ =
  List.iter
    (Test.test BoolExpr.lexer BoolExpr.parse BoolExpr.show) [
      "k";
      "0";
      "1";
      "k && 1";
      "k && u || v";
      "k && (u||v)";
      "u||v && k";
      "~u";
      "~(u&&v)";
      "~~"
      ]
