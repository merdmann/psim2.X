rundir=../

## internal stuff
arch=$(shell uname -p)
host=$(shell hostname)
date=$(shell date +%s)

objdir=$(rundir)/obj-$(arch)
libdir=$(rundir)
lib=$(libdir)/libmachdep.a

%.o :  %.c
	gcc $(CFLAGS) -D_GNU_SOURCE -c $? -o $@

all :: $(lib)

$(lib) : bind_cpu.o spinlock.o set_scheduling.o semaphore.o
	ar crvf $(lib)	bind_cpu.o spinlock.o set_scheduling.o semaphore.o
	ranlib $(lib)

clean ::
	rm -rf *.o $(lib)

distclean ::  clean

