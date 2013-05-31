#!/bin/sh 
# ************************************************************************ --
# *****           P A R T I C L E  S I M U L A T O R  (PSim)         ***** --
# *****               FOR  CLASSICAL PARTICLES                       ***** --
# ************************************************************************ --
# $Id: sort.sh 27 2010-10-20 04:44:18Z  $
#                          
# This file is part of PSim. This script is used to sort the output files
# of the simulator according to particles-
#                                                                         
# Copyright (C) 2010 Michael Erdmann                                      
#
# PSim is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

## setup environment
tmp=/tmp/sort.$$
trap 'rm -f "${tmp}.*" >/dev/null 2>&1' 0
trap "exit 2" 1 2 3 15

output=result.jpg
target=""
source=""

for i in $* ; do
   case "$1" in
    -*=*) optarg=`echo "$1" | sed 's/[-_a-zA-Z0-9]*=//'` ;;
       *) optarg= ;;
   esac

   case $1 in   	
       --source=*)
           source=$optarg
           ;;
           
       --target=*)
           target=$optarg
           ;;
       *) 
           cmd="$cmd $1"
           ;;
   esac
   shift
done

if [ "x${target}" = "x" ] ; then
	echo "--target is mandatory";
	exit 1
fi

if [ ! -e ${source} ] ; then
   echo "Error: ${source} not existing"
   exit 1
fi

N=`head ${source} | grep N= | cut -d '=' -f 2` 
echo "${source} $N"

echo "Reorganizing files"
p=1
while [ $p -le $N ]; do
   f=${target}.$p.data
   echo "Particle $p ... $f" 
   echo "P,T,M,X,Y,Z,VX,VY,VZ,RR,V,E" > $f
   fgrep "P $p;" ${source} | tr ';' ',' >> $f
      
   p=`expr $p + 1` 
done 

rm -rf ${tmp}
exit 0
