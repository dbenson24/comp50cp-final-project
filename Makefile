all: ebin ebin/chat.beam ebin/userserver.beam ebin/nodemanager.beam

ebin:
	mkdir ebin

ebin/%.beam: src/%.erl
	erlc -o ebin $<

clean:
	rm ebin/*.beam

