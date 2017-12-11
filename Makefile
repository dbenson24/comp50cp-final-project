all: ebin ebin/msgserver.beam ebin/userserver.beam ebin/nodemanager.beam ebin/clientserver.beam ebin/tictactoegame.beam ebin/stress_test.beam

ebin:
	mkdir ebin

ebin/%.beam: src/%.erl
	erlc -o ebin $<

clean:
	rm ebin/*.beam

