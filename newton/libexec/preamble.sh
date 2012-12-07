#!/bin/bash
# ************************************************************************ --
# *****           P A R T I C L E  S I M U L A T O R  (PSim)         ***** --
# *****               FOR  CLASSICAL PARTICLES                       ***** --
# ************************************************************************ --
# $Id: preamble.sh 26 2010-10-17 10:53:56Z  $
#                                                                         
# This file is part of PSim. It provides a basic infrastructure for all
# evaluation scripts in the PSim project. 
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
_msg_cmd=""
_cleanup_handler=""

#
# This function should be called at the begin of the execution. It 
# installs an exit handler and provides a path for temporary files
# which may be created during the execution of the script.
#
# $1 - Name of the program
# $2 - cleanup code to be called in case of script termination.
#
function preamble {	
   _msg_cmd=$1 
   _cleanup_handler=$2
   	
   tmp=/tmp/${_msg_cmd}.$$
      
   if [ "x$2" = "x" ] ; then
      trap 'postamble >/dev/null 2>&1' 0
   else
      trap '$2; postamble >/dev/null 2>&1' 0   
   fi
   trap "exit 2" 1 2 3 15   
   
}

#
# This function is to be called at the end of the script in order
# to cleanup resources which are held by the preamble framework.
#
# $1 - cleanup code
#
function postamble {
   rm -f ${tmp}.*
   ${_cleanup_handler}
}
