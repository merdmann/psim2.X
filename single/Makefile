## test suite
project=threads
CFG=brad
## the CPU to be used
CPU=0
## the implementation under test
opt=simple
buildtype=production
## the place where the state and configuratuon is stred
ROOT=.
## the place where we execute the executable
rundir=$(shell pwd)

## internal stuff
project=threads

arch=$(shell uname -p)
host=$(shell hostname)
date=$(shell date +%s)

## the place where to test code is executed
bindir=$(rundir)/bin-$(arch)
exec=particle
resultdir=$(ROOT)/result-$(CFG)

log=$(CFG)-$(arch)-$(host)
tmp=/tmp/$(log)
loginfo=$(tmp).log
cpuinfo=$(tmp).cpu
timeinfo=$(tmp).time
build=$(tmp).build
stdout=$(tmp).stdout
result=$(tmp).result
trajectory=$(tmp).trj
objdir=./obj-$(arch)
state=state
scheduling=SCHED_BATCH

DEFS=-XBINDIR=$(bindir) \
	-XARCH=$(arch) \
	-XMAIN=$(exec)\
	-XOPT=$(opt)

ifdef _NUMA_
LIBS += -lnuma
CFLAGS += -D_NUMA_
endif

LIBS += -L$(rundir) -lmachdep

## build executable $(exec)
all build :: dirs $(rundir)/libmachdep.a
	gprbuild  $(DEFS) -P $(project).gpr \
		-cargs -fomit-frame-pointer -gnateDSCHED_POLICY=$(scheduling) \
		-largs $(LIBS)

$(rundir)/libmachdep.a :
	$(MAKE) -C../psim/machdep rundir=$(rundir)

clean distclean::
	$(MAKE) -C../psim/machdep rundir=$(rundir) $@


dirs :: $(bindir) $(objdir)

$(bindir):
	mkdir -p $(bindir)

$(objdir):
	mkdir -p $(objdir)


## run a test with a given configuration and collect all relevant configuration data
test ::  $(resultdir)
	@echo "Build Options: $(DEFS)" > $(build)
	$(MAKE) BUILD=$(buildtype) scheduling=$(scheduling)  all >> $(build)
	rm -rf $(ROOT)/$(state).init
	schedtool -F -p 20 -a $(CPU) -e /usr/bin/time -p -o $(timeinfo) $(bindir)/$(exec) $(ROOT)/$(CFG) $(loginfo) $(ROOT)/$(state) | tee $(stdout)
	egrep ^P $(stdout) > $(trajectory)
	cat /proc/cpuinfo | grep "model name" > $(cpuinfo)
	cat /proc/cpuinfo | grep  MHz >> $(cpuinfo)
	@echo "----------------- Processor Information: " > $(result)
	cat $(cpuinfo) >> $(result)
	echo "----------------- Excution Time:" >> $(result)
	cat $(timeinfo) >> $(result)
	fgrep T_ $(loginfo) | grep Total: >> $(result)
	@echo "----------------- Stdout:" >> $(result)
	cat $(stdout) >> $(result)
	@echo "------------------ Build Logging:" >> $(result)
	cat $(build) >> $(result)
	@echo "------------------ Program Logging:" >> $(result)
	cat $(loginfo) >> $(result)
	mv $(result) $(resultdir)/$(log)-$(date).log
	cp $(ROOT)/$(CFG).cfg $(resultdir)/$(CFG)-$(date).cfg

nbody :: $(resultdir)
	$(MAKE) buildtype=production opt=$(opt) CFG=nbody test

$(resultdir):
	mkdir -p $(resultdir)

## just clean local objects
clean ::
	-gnatclean -v $(DEFS) -P $(project).gpr
	rm -f $(state).init
	rm -rf *.o *.a

## clean all artefacts including log files
distclean :: clean
	rm -rf ./bin-* ./obj-*
	rm -rf *.init
	rm -rf *.log *.result
