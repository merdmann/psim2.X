#!/bin/sh
# ************************************************************************ --
# *****           P A R T I C L E  S I M U L A T O R  (PSim)         ***** --
# *****               FOR  CLASSICAL PARTICLES                       ***** --
# ************************************************************************ --
# $Id: run.sh 27 2010-10-20 04:44:18Z  $
#
# This file is part of PSim. It is used to execute the particle simulator
# in the correct environment.
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

. ./libexec/msg.sh
. ./libexec/preamble.sh
. ./libexec/jobs.sh

## provide help information
function help {
cat <<EOF

usage:
   ./run.sh option(s)

description:
   Executes the simulator from the command line.

   The simulator is executed within a so called workspace which contains the
   executables, the configuration files which controls the execution and the
   results.

   If the config file <config>.cfg is not already existing in the workspace
   it will be copied from the $home/configs directory.

   The binaries are expected to be found under $home/<arch>-bin.

   If not otherwise specfied the followig defaults will be used:

   workspace = \$home/jobs/\$config-\$version
   logdir=\$workspace
   resultdir=\$workspace

option(s):
   --home=<dir>      The distribution directory
   --workdir=<dir>   Specifies the place where the program is executed.
   --resultdir=<dir> The place where the results are stored after
                     completion.
   --logdir=<dir>    Location where the log files are stored.
   --config=<config> Specifies the configuration to be used.
   --version=<vers>  Specifies the version of a configuration

EOF
}

###########
## BEGIN ##
###########

preamble `basename $0`

state=""
workspace=""
workdir=""
version=""
config=""
state=""
home=`pwd`
opt_nosort=""
opt_halt=""

for i in $* ; do
   case "$1" in
    -*=*) optarg=`echo "$1" | sed 's/[-_a-zA-Z0-9]*=//'` ;;
       *) optarg= ;;
   esac

   case $1 in
       --config=*)
           config=$optarg
           ;;

       --version=*)
       	   version=$optarg
       	   ;;

       --state=*)
           state=$optarg
           ;;

       --home=*)
       	   home=$optarg
       	   ;;

       --jobname=*)
           jobname=$optarg
           ;;

       --resultdir=*)
       	   resultdir=$optarg
       	   ;;

       --workdir=*)
           workdir=$optarg
           ;;

       --logdir=*)
       	   logdir=$optarg
       	   ;;

       --nosort)
           opt_nosort="y"
           ;;

       --halt)
       	   opt_halt="y"
           ;;

       --h|--help)
           help
           exit 1
           ;;
        *)
           additional="${additional} $1"
           ;;
   esac
   shift
done

## create the workspace
if [ "x${config}" = "x" ] ; then
	Error "--config=<configname> is mandatory"
	exit 1
fi

configdir=`jobpath ${config} ${version}`

if [ -z "${workdir}" ] ; then
   workspace=${home}/${configdir}
else
   workspace=${workdir}/${configdir}
fi

if [ -z "${jobname}" ] ; then
   jobname=`date +%F.%H%M%S.%N`
fi

if [ -z "${resultdir}" ] ; then
   resultdir=${workspace}
fi

if [ -z "${logdir}" ] ; then
   logdir=${workspace}
fi

## set the tool pathes
logfile=${logdir}/${jobname}-sim.log
resultfile=${resultdir}/${jobname}.data
execdir=`${home}/tools/execute.sh --path --root=${home}`
sort=${home}/tools/sort.sh

if [ -z "${state}"] ; then
   state=${config}
fi

# create the workspace contents
if [ ! -d ${workspace} ] ; then
   Comment "Creating workspace... ${workspace} "
   mkdir -p ${workspace}
   cp ${home}/configs/${config}.cfg ${workspace}
fi

if [ -z "${resultdir}" ] ; then
   resultdir=${workspace}
fi

if [ -z "${logdir}" ] ; then
   logdir=${workspace}
fi

## set the tool pathes
logfile=${logdir}/${jobname}-sim.log
resultfile=${resultdir}/${jobname}.data
execdir=`${home}/tools/execute.sh --path --root=${home}`
sort=${home}/tools/sort.sh

if [ -z "${state}" ] ; then
   state=${config}
fi

if [ ! -d ${resultdir} ] ; then
   mkdir ${resultdir}
else
   Comment "${resultdir} already existing not created"
fi

cd ${workspace}
if [ -e ${execdir}/particle ] ; then
   cp ${execdir}/particle ${workspace}
else
   Error "simulator has not been build!"
   exit 1
fi
if [ ! -e ${workspace}/particle ] ; then
	Error "simulation program not existing"
	exit 1;
fi

## run
time -p ${workspace}/particle ${config} ${logfile} ${state} > ${resultfile}

if [ -z "${opt_nosort}" ] ; then
   ${sort} --source=${resultfile} --target=${jobname}
fi

#### copy out the results
if [ "${workspace}"  !=  "${resultdir}" ] ; then
   Comment "moving ${workspace} to ${resultdir}"
   mv ${workspace}/* ${resultdir}
   rm -rf ${workspace}
fi

postamble

## used on google compting engine
if [ "x${opt_halt}" != "x" ] ; then
   sync
   sudo shutdown -h now
fi
exit 0
