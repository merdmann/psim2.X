#!/bin/sh
pids=`ps -aLM| grep particle|awk '{ print $3 }'` 
for p in ${pids} ; do
   taskset -p $p
done
