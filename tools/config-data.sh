#!/bin/sh
# ************************************************************************ --
# *****           P A R T I C L E  S I M U L A T O R  (PSim)         ***** --
# *****               FOR  CLASSICAL PARTICLES                       ***** --
# ************************************************************************ --
# $Id: config-data.sh 17 2010-10-02 07:15:52Z  $
#                                                                         
# Copyright (C) 2010 Michael Erdmann                                      
#                                                                           
# PSIM is copyrighted by the persons and institutions enumerated in the   
# AUTHORS file. This file is located in the root directory of the          
# PSIM distribution.                                                      
#                                                                          
# PSIM is free software;  you can redistribute it  and/or modify it under 
# terms of the  GNU General Public License as published  by the Free Soft- 
# ware  Foundation;  either version 2,  or (at your option) any later version. 
# PSIM is distributed in the hope that it will be useful, but WITHOUT 
# ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY 
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
# for  more details.  
# You should have  received  a copy of the GNU General Public License  
# distributed with PSIM;  see file COPYING.  If not, write to the 
# Free Software Foundation,  59 Temple Place - Suite 330,  Boston, 
# MA 02111-1307, USA.                                                     
#
#
tmp=/tmp/config-data.$$
trap 'rm -f "${tmp}" >/dev/null 2>&1' 0
trap "exit 2" 1 2 3 15

## get arguments
root=../../
output=""
config=""
with=""
version=""

for i in $* ; do
   case "$1" in
    -*=*) optarg=`echo "$1" | sed 's/[-_a-zA-Z0-9]*=//'` ;;
       *) optarg= ;;
   esac

   case $1 in
       --config=* )
   		   config=$optarg
           ;;
       --root=* )
	   root=$optarg
	   ;;
           
       --version=*)
           version=$optarg
           ;;
           
       --parameter=*)
           parameter=$optarg
           ;;
           
       *) 
           if [ "x${with}" = "x" ] ; then
              with="$1"
           else
              with="${with} $1"
           fi
           ;;
   esac
   shift
done

if [ "x${version}" = "x" ] ; then
   jobpath=${root}/jobs/${config}
else
   jobpath=${root}/jobs/${config}-${version}	
fi

configpath=${jobpath}/${config}.cfg

if [ ! -e ${configpath} ] ; then
   echo "Error: ${configpath} does not exist"
   value=""
else
   value=`fgrep "${parameter}=" ${configpath} | cut -d "=" -f 2 | cut -d ";" -f 1`
fi 
echo $value
rm -rf ${tmp}
exit 0
