#!/bin/sh -x
# ************************************************************************ --
# *****           P A R T I C L E  S I M U L A T O R  (PSim)         ***** --
# *****               FOR  CLASSICAL PARTICLES                       ***** --
# ************************************************************************ --
# $Id: display.sh 24 2010-10-14 16:24:58Z  $
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

#################
## COMMON CODE ##
#################
. ../libexec/preamble.sh
. ../libexec/msg.sh 

# cleanup all artefacts which are not stored in the tmp directory 
function cleanup {
	rm -f  config.data energy.tiff trajectory.tiff
}

###########
## BEGIN ##
###########
preamble `basename $0` cleanup

## get arguments
root="../"
datafile=""
output=""
config=""
with=""
version=""
scale=1000.0
## dx data files
dxinput=${tmp}.dx
## has to match the dx implementaion
dxcfg="config.data"

for i in $* ; do
   case "$1" in
    -*=*) optarg=`echo "$1" | sed 's/[-_a-zA-Z0-9]*=//'` ;;
       *) optarg= ;;
   esac

   case $1 in
       --config=*)
   	   config=$optarg
           ;;
       --root=*)
           root=$optarg
	   ;;
           
       --output=*)
           output=$optarg
           ;;
           
       --version=*)
           version=$optarg
           ;;
       --keep=*)
           datafile=$optarg
   		   ;;
           
       --scale=*)
       	   scale=$optarg
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

if [ ! -e image.cfg ] ; then
	Eror "The script has to be called from the place where the dx script is located."
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

if [ "x${version}" = "x" ] ; then
   jobpath=${root}/jobs/${config}
else
   jobpath=${root}/jobs/${config}-${version}	
fi

configpath=${jobpath}/${config}.cfg

resultfiles=`find ${jobpath} -name "*.data" | sort`
if [ "x${resultfiles}" = "x" ] ; then
	Error " No data available for particle ${particle} in ${config}"
	exit 1
fi

## process the conputing result
files=`echo ${resultfiles} | wc -w`
echo "processing ${files} files .."
cat ${resultfiles} | egrep "^P ${particle};" | tee ${tmp}.rawdata
lc=`wc -l ${tmp}.rawdata | cut -d" " -f 1`
echo "Processing $lc points"
../tools/sample.sh --input=${tmp}.rawdata --count=500 | awk -F ";" -f data.awk > ${dxinput} 

if [ ! "x${datafile}" = "x" ] ; then
   cp ${dxinput} ${datafile}
fi

## create a config data file in order to allow dx easy access to display the data
N=`../tools/config-data.sh --root=${root} --parameter=N --config=${config} --version=${version}` 
DT=`../tools/config-data.sh --root=${root}  --parameter=DT --config=${config} --version=${version}` 
RS=`../tools/config-data.sh --root=${root} --parameter=RS --config=${config} --version=${version}` 
P1=`../tools/config-data.sh --root=${root} --parameter=P1 --config=${config} --version=${version}` 
P2=`../tools/config-data.sh --root=${root} --parameter=P2 --config=${config} --version=${version}` 

echo $P1
echo $P2
echo $RS, $DT, $N

echo "DT;RS;N;P1;P2;SC;FN" > ${dxcfg}
echo "$DT;$RS;$N;$P1;$P2;${scale};${dxinput}" >> ${dxcfg}

dx -script image.net

tifftopnm trajectory.tiff | pnmtojpeg > ${output}.trajectory.jpg 
tifftopnm energy.tiff | pnmtojpeg > ${output}.energy.jpg

postamble cleanup
