#!/bin/sh
cpus=`fgrep processor /proc/cpuinfo| wc -l`
host=`hostname`
x=`taskset -p 1 | grep 1`
if [ "x$x" = "x" ] ; then
	echo "Please run setcpu.sh as root first!"
	exit 1
fi;

state=""
cmd=""

for i in $* ; do
   case "$1" in
    -*=*) optarg=`echo "$1" | sed 's/[-_a-zA-Z0-9]*=//'` ;;
       *) optarg= ;;
   esac

   case $1 in
   	   --initial-state=*)
   	       initialstate=$optarg
   	   	   ;;

	   --exec=*)
	   	   exec=$optarg
	   	   ;;

	   --config=*)
	   	   config=$optarg
	   	   ;;

	   --state=*)
	   	   state=$optarg
	   	   ;;

	   --logfile=*)
	   	   logfile=$optarg
	   	   ;;

       *)
       	  cmd="${cmd} $i"
          ;;
   esac
   shift
done

if [ "x${initialstate}" != "x" ] ; then
   echo "Copying state ${initialstate}"
   cp ${initialstate} ${state}.init
fi

cmd="${exec} ${config} ${logfile} ${state}"

case ${host} in
	thor*|thunder*)
		taskset -c 0,1 ${cmd}
		;;
	hal)
		schedtool -F -p 10 -a 0x7f -e ${cmd}
		;;
	cyclope)
		taskset -c 1,2,3 ${cmd}
		;;
	sl01)
		taskset -c 0x00fe ${cmd}
		;;
	askar)
		taskset -c 1,2,3 ${cmd}
		;;
esac;

exit 0
