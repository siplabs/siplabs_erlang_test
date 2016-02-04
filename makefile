compile: subdirs	

subdirs:
	cd src; make

clean:	
	rm -rf *.beam erl_crash.dump
	cd ebin; rm -rf *
