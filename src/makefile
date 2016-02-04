.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

MODS = novice
all: compile move

compile: ${MODS:%=%.beam}

move:
	mv *.beam ../ebin
	cp *.app ../ebin

clean:	
	rm -rf *.beam erl_crash.dump
