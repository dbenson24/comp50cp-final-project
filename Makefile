all: ebin/chat.beam ebin/userserver.beam
	
ebin/%.beam: src/%.erl
	erlc -o ebin $<

clean:
	rm ebin/*.beam

