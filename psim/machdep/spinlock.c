#include <stdio.h>
#include <pthread.h>
#include <errno.h>

#define MAX_LOCKS 16

static pthread_spinlock_t spinlock[MAX_LOCKS];

void initialize() {
	int i;

	for( i=0; i<MAX_LOCKS; ++i )
		pthread_spin_init(&spinlock[i], 0);
}

void lock( int client ) {
	pthread_spin_lock(&spinlock[client]);
}

void unlock( int client ) {
	pthread_spin_unlock(&spinlock[client]);
}

