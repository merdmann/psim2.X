/*
 * semaphores.c
 *
 *  Created on: Nov 27, 2012
 *      Author: merdmann
 */



#include <unistd.h>     /* Symbolic Constants */
#include <sys/types.h>  /* Primitive System Data Types */
#include <errno.h>      /* Errors */
#include <stdio.h>      /* Input/Output */
#include <stdlib.h>     /* General Utilities */
#include <pthread.h>    /* POSIX Threads */
#include <string.h>     /* String handling */
#include <semaphore.h>  /* Semaphore */


static sem_t *mySem[10];


void SEM_registerClient(int id ) {
    mySem[id] = malloc( sizeof( sem_t ) );
    sem_init( mySem[id], 1, 0);
}

void SEM_unregisterClient(int id) {
    sem_destroy( mySem[id] );
    free( mySem[id] );

    mySem[id] = NULL;
}


void SEM_wait(int id) {
    sem_wait(mySem[id]);
}

void SEM_wakeup(int id) {
    sem_post(mySem[id]);
}

