# leave these lines alone
.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean 

MODS = novice

all: compile

compile: ${MODS:%=%.beam} subdirs	
## special compilation requirements are added here

special1.beam: special1.erl    
	${ERL} -Dflag1 -W0 special1.erl

## run an application from the makefile

application1: compile
	${ERL} -pa Dir1  -s application1 start Arg1 Arg2 

subdirs:
#	cd dir1; make
#	cd dir2; make
#	...

# remove all the code

clean:	
	rm -rf *.beam erl_crash.dump
#	cd dir1; make clean
#	cd dir2; make clean  
