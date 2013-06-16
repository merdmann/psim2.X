#!/bin/bash -x
#DISPLAY=askar:0.0; export DISPLAY 
m=`uname -m`
case $m in 
   i686)
      home=/opt/ParaView-3.98.1-Linux-32bit/lib/paraview-3.98
      ;;

   x86_64)
      home=/opt/ParaView-3.98.1-Linux-64bit/lib/paraview-3.98
      ;;
esac
PATH=${home}:$PATH
LD_LIBRARY_PATH=${home}; export LD_LIBRARY_PATH 

${home}/mpiexec -np $1 ${home}/pvserver --reverse-connection --server-port=22222 --client-host=localhost

