#!/bin/bash
# ************************************************************************ --
# *****           P A R T I C L E  S I M U L A T O R  (PSim)         ***** --
# *****               FOR  CLASSICAL PARTICLES                       ***** --
# ************************************************************************ --
# $Id: msg.sh 17 2010-10-02 07:15:52Z  $
#                                                                         
# This file is part of PSim. 
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

#
#  $1 - configuration
#  $2 - version
#
function jobpath {
   if [ "x$2" = "x" ] ; then
      jobpath=jobs/$1
   else	
      jobpath=jobs/$1-$2
   fi
   
   echo "${jobpath}"	
}