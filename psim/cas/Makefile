## test suite
exec=main
project=cas

## internal stuff
arch=$(shell uname -p)
host=$(shell hostname)
date=$(shell date +%s)

bindir=./bin-$(arch)
objdir=./obj-$(arch)

DEFS=-XBINDIR=$(bindir) -XARCH=$(arch) -XMAIN=$(exec)

## build executable $(exec)
all build :: dirs  
	gprbuild  $(DEFS) -P $(project).gpr -cargs -fomit-frame-pointer 

dirs :: $(bindir) $(objdir)	
	
$(bindir):
	mkdir -p $(bindir)
	 
$(objdir):
	mkdir -p $(objdir)
	
## run a test with a given configuration and collect all relevant configuration data
test ::  $(resultdir)
	$(MAKE) Build="test" all
	$(bindir)/$(exec)


## just clean local objects
clean :: 
	gnatclean -v $(DEFS) -P $(project).gpr 
	rm -rf *.o

## clean all artefacts including log files
distclean :: clean
	rm -rf ./bin-* ./obj-*
	rm -rf *.log *.result
