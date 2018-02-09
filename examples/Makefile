test:
	@for i in [a-z]*/[a-z0-9]*; do (echo "** Testing in $$i"; cd $$i; make); done


clean:
	@for i in [a-z]*/[a-z0-9]*; do (cd $$i; make clean); done
