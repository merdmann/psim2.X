## test suite
exec=main
project=locktest

## internal stuff
arch=$(shell uname -p)
host=$(shell hostname)
date=$(shell date +%s)

bindir=./bin-$(arch)
objdir=./obj-$(arch)
resultdir=./result-$(CFG)
ROOT=.

DEFS=-XBINDIR=$(bindir) \
	-XARCH=$(arch) \
	-XMAIN=$(exec) 

LIBS += -lpthread
ifdef _NUMA_ 
LIBS += -lnuma 
CFLAGS += -D_NUMA_
endif

## build executable $(exec)
all build :: dirs machdep
	gprbuild -gnat2012 $(DEFS) -P $(project).gpr -cargs -fomit-frame-pointer -largs ../$(objdir)/spinlock.o $(LIBS)

dirs :: $(bindir) $(objdir)	
	
$(bindir):
	mkdir -p $(bindir)
	 
$(objdir):
	mkdir -p $(objdir)
	
## machine/os depenant parts
machdep ::
	$(MAKE) CFLAGS=$(CFLAGS) -C../machdep all
	
clean distclean ::
	$(MAKE) -C../machdep $@
	
## just clean local objects
clean :: 
	-gnatclean -v $(DEFS) -P $(project).gpr 
	rm -rf *.o

## clean all artefacts including log files
distclean :: clean
	rm -rf ./bin-* ./obj-*
