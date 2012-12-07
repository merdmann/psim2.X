#!/bin/sh
i=0
r=coretest
rm -rf $r
mkdir -p $r

bindir=bin-`uname -p`

while true ; do
   proc=`echo "$i % 8" | bc`
   taskset -c ${proc} /usr/bin/time -p -o $r/core.$i ${bindir}/particle cores /tmp/core.$i.log /tmp/state.$1 > /dev/null &
   i=`echo "$i+1" | bc`
   echo $i
   if [ $i -ge $1 ] ; then
      exit;
   fi
done


fgrep user $r/core.*




