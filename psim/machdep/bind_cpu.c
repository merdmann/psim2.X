/*
 * bind.c
 *
 *  Created on: Jul 11, 2012
 *      Author: merdmann
 */
#include <stdio.h>
#include <sched.h>
#include <stdlib.h>
#include <unistd.h>
#include <sched.h>
#include <errno.h>

#if _NUMA_==1
#include <numa.h>


int bind_cpu(int cpu) {
	struct bitmask *nodemask =  numa_parse_nodestring("0");
	struct bitmask *cpumask = numa_allocate_cpumask();

	numa_bind(nodemask);
	cpumask = numa_bitmask_clearall(cpumask);
	return numa_sched_setaffinity( getpid(), numa_bitmask_setbit(cpumask, cpu-1) );
}

#else

int bind_cpu(int cpu)
{
   int RC;

   cpu_set_t mask;

   CPU_ZERO(&mask);
   CPU_SET(cpu,&mask);
   RC = sched_setaffinity(getpid(), sizeof(mask), &mask);
}
#endif

int nbr_of_cpus() {
	sysconf(_SC_NPROCESSORS_ONLN);
}

