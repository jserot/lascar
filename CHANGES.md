# Changes

## 0.7.0 (Feb XX, 2021)
    * fixed formatting bugs in generated doc 
    * Added functions `Ltsa.map_state_attr`, `Ltsa.map_transition`, `Lts.map_state`
      and `Lts.map_transition` and updated `examples/lts/map1` accordingly
    * fixed bug in defactorization algorithm for FSMs with output values attached to
      states (see for ex `examples/fsm/gensig2`)
    * revised (generalized) interface for the `Fsm_expr` and `Fsm` modules (now parametrized on
      the type of values)
    * `make` now also builds a custom toplevel (`lascar_top`) embedding the `utils` and `lascar`
      libraries. The examples provided in `examples/xx/yy` can be executed interactively with this
      toplevel. For this, each example directory has a link to an `.ocamlinit` file telling where to
      find the corresponding `.cmi` files.
    * rewrote examples to use `show` PPX extension to generate state names
    * switched to Dune 2.6 (prev was 1.11)

## 0.6.0 (Sep, 16, 2019)
    * minor rewritings for ocaml >= 4.08

## 0.6-alpha (Aug, 18, 2019)
    * removed dependency on camlp4 by rewriting the Fsm_expr, Fsm and ListExt parsers
    * dunified build and install process
    
## 0.5 (Apr, 9, 2018)
	* Library is now packed under Lascar module to prevent name clash
    
## 0.4 (Feb, 5, 2018)
	* OPAMification
	* Home page and documentation moved to separate web site
	* Added INSTALL
	* CHANGELOG now in .md format
	
## 0.3 (Dec, 14, 2017)
	* Added functions [map_state], [map_attr] and [map_label]
	* Added modules [Builtins.{Int,String,Bool}]. Rewrote some examples accordingly
	
## 0.2 (Nov 27, 2107)
    * Added [ListExt.update_assoc]
	
## 0.1 (Nov 6, 2107)
    * First public version
