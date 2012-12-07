algorithm=twostage

## internal stuff
arch=$(shell uname -p)
host=$(shell hostname)
date=$(shell date +%s)

bindir=$(shell ./tools/execute.sh --path --root=`pwd`)

ROOT=

exec=$(bindir)/paticle

all: $(bindir)
	$(MAKE) -C./psim algorithm=$(algorithm) bindir=$(bindir)
	
$(bindir) :
	mkdir -p $(bindir)


## just clean local objects
clean :: 
	rm -rf $(exec)


## clean all artefacts including log files
distclean :: clean
	rm -rf $(bindir)