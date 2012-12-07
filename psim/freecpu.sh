#!/bin/sh
pids=`ps -afel|awk '{ print $4 }'` 
for i in $pids ; do
   taskset -a -p 80 $i
done
