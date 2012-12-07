#!/bin/sh -x
# ************************************************************************ --
# *****           P A R T I C L E  S I M U L A T O R  (PSim)         ***** --
# *****               FOR  CLASSICAL PARTICLES                       ***** --
# ************************************************************************ --
# $Id: di.sh 17 2010-10-02 07:15:52Z  $
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
tmp=/tmp/display.$$
trap 'rm -f "${tmp}" >/dev/null 2>&1' 0
trap "exit 2" 1 2 3 15

. ../libexec/msg.sh 

## get arguments
output=""
config=""
with=""
version=""
scale=1000.0
## dx data files
dxinput="ekin.data"
## has to match the dx implementaion
dxcfg="config.data"

for i in $* ; do
   case "$1" in
    -*=*) optarg=`echo "$1" | sed 's/[-_a-zA-Z0-9]*=//'` ;;
       *) optarg= ;;
   esac

   case $1 in
       --config=* )
	   config=$optarg
           ;;
           
       --output=* )
           output=$optarg
           ;;
           
       --version=*)
           version=$optarg
           ;;
           
       --scale=*)
       	   scale=$optarg
       	   ;;

       --root=*)
	   root=$optarg
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

if [ ! -e di.cfg ] ; then
	Error "The script has to be called from the place where the dx script is located."
	exit 1:
fi

if [ "x${config}" = "x" ] ; then
	Error "--config=<config name> is mandatory"
	exit 1
fi

if [ "x${with}" = "x" ] ; then
    particle=1
else
    particle=${with}
fi

if [ "x${output}" = "x" ] ; then
   output=part.${particle}
fi

## figure out hte location of the data file
if [ "x${version}" = "x" ] ; then
   jobpath=${root}/jobs/${config}
else
   jobpath=${root}/jobs/${config}-${version}	
fi

configpath=${jobpath}/${config}.cfg

## get all data files and compress them
resultfiles=`find ${jobpath} -name \*.data| cut -d "." -f 3-5 |uniq`
echo ${resultfiles}
if [ "x${resultfiles}" = "x" ] ; then
	Error " No data available for particle ${particle} in ${config}"
	exit 1
fi

files=`echo ${resultfiles} | wc -w`
echo -n "processing ${files} files .."

for i in ${resultfiles} ; do
   if [ -e ../$i.data ] ; then
      egrep ^P ../$i.data >> ${tmp}.rawdata
   fi
done
echo 

lc=`wc -l ${tmp}.rawdata | cut -d" " -f 1`
echo "Processing $lc points"
echo "PN;T0;M;X1;X2;X3;V1;V2;V3;XR;V;Error" > ${dxinput}
../tools/sample.sh --input=${tmp}.rawdata --count=3000  >> ${dxinput} 

## create a config data file in order to allow dx easy access to display the data
N=`../tools/config-data.sh --root=../ --parameter=N --config=${config} --version=${version}` 
DT=`../tools/config-data.sh --root=../ --parameter=DT --config=${config} --version=${version}` 
RS=`../tools/config-data.sh --root=../ --parameter=RS --config=${config} --version=${version}` 
P1=`../tools/config-data.sh --root=../ --parameter=P1 --config=${config} --version=${version}` 
P2=`../tools/config-data.sh --root=../ --parameter=P2 --config=${config} --version=${version}` 

if [ "x$P1" = "x" ] ; then 
	P1="<undef>"
fi

if [ "x$P2" = "x" ] ; then 
	P2="<undef>"
fi


echo "DT;RS;N;P1;P2;SC;FN" > ${dxcfg}
echo "$DT;$RS;$N;$P1;$P2;${scale};${dxinput}" >> ${dxcfg}

dx -script di.net

tifftopnm trajectory.tiff | pnmtojpeg > ${output}.trajectory.jpg 
tifftopnm energy.tiff | pnmtojpeg > ${output}.energy.jpg

rm -f ${tmp}.*
