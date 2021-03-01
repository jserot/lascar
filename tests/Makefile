byte:
	dune build ./test_fsm_exprs.bc
	dune build ./test_fsm_conditions.bc
	dune build ./test_fsm_actions.bc
	dune build ./test_fsm_transitions.bc

run:
	dune exec ./test_fsm_exprs.bc
	dune exec ./test_fsm_conditions.bc
	dune exec ./test_fsm_actions.bc
	dune exec ./test_fsm_transitions.bc

clean:
	dune clean

clobber: clean
	@rm -f *~
