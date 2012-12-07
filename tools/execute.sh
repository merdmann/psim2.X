#!/bin/sh
# ************************************************************************ --
# *****           P A R T I C L E  S I M U L A T O R  (PSim)         ***** --
# *****               FOR  CLASSICAL PARTICLES                       ***** --
# ************************************************************************ --
# $Id: execute.sh 17 2010-10-02 07:15:52Z  $
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
cpu=`uname -p`
bindir=bin-$cpu
root=./
cmd=""
path=""

for i in $* ; do
   case "$1" in
    -*=*) optarg=`echo "$1" | sed 's/[-_a-zA-Z0-9]*=//'` ;;
       *) optarg= ;;
   esac

   case $1 in
       --root=* )
   		   root=$optarg
           ;;
       --path )
           path="y"
           ;;
   
       *) 
          if [ "x${cmd}" = "x" ] ; then
          	cmd=$1
          else
            cmd="${cmd} $1"
		  fi
          ;;
   esac
   shift
done

if [ "x${path}" = "x" ] ; then 
    time ${root}${bindir}/${cmd}
else
    echo ${root}/${bindir}
fi
