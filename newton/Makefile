## test suite
exec=particle
project=newton
CFG=brad
rundir=.
ROOT=.


## internal stuff
arch=$(shell uname -p)
host=$(shell hostname)
date=$(shell date +%s)

bindir=$(rundir)/bin-$(arch)
resultdir=./result-$(CFG)

log=$(CFG)-$(arch)-$(host)
tmp=/tmp/$(log)
loginfo=$(tmp).log
cpuinfo=$(tmp).cpu
timeinfo=$(tmp).time
stdout=$(tmp).stdout
result=$(tmp).result
objdir=./obj-$(arch)
state=state

DEFS=-XBINDIR=$(bindir) -XARCH=$(arch) -XMAIN=$(exec)

ifdef _NUMA_ 
LIBS += -lnuma
CFLAGS += -D_NUMA_
endif

## build executable $(exec)
all build :: dirs 
	gprbuild  $(DEFS) -P $(project).gpr -cargs -fomit-frame-pointer -largs $(LIBS)

dirs :: $(bindir) $(objdir)	
	
$(bindir):
	mkdir -p $(bindir)
	 
$(objdir):
	mkdir -p $(objdir)
	
## run a test with a given configuration and collect all relevant configuration data
test ::  $(resultdir)
	$(MAKE) BUILD="test" all
	/usr/bin/time -p -o $(timeinfo) $(bindir)/$(exec) $(ROOT)/$(CFG) $(loginfo) $(state) | tee $(stdout)	
	cat /proc/cpuinfo | grep "model name" > $(cpuinfo)
	cat /proc/cpuinfo | grep  MHz >> $(cpuinfo)
	echo "----------------- Processor Information: " > $(result)			
	cat $(cpuinfo) >> $(result) 
	echo "----------------- Excution Time:" >> $(result)			
	cat $(timeinfo) >> $(result) 		
	fgrep T_ $(loginfo) | grep Total: >> $(result)
	echo "----------------- Stdout:" >> $(result)
	cat $(stdout) >> $(result)
	echo "------------------ Program Logging:" >> $(result)
	cat $(loginfo) >> $(result)	
	mv $(result) $(resultdir)/$(log)-$(date).log
	cp $(ROOT)/$(CFG).cfg $(resultdir)/$(CFG)-$(date).cfg
	
nbody :: $(resultdir)
	$(MAKE) CFG=nbody test

$(resultdir):
	mkdir -p $(resultdir)

## just clean local objects
clean :: 
	-gnatclean -v $(DEFS) -P $(project).gpr 
	rm -f $(state).init
	rm -rf *.o

## clean all artefacts including log files
distclean :: clean
	rm -rf ./bin-* ./obj-*
	rm -rf *.init
	rm -rf *.log *.result
