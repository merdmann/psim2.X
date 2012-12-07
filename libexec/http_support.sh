#!/bin/bash
# ************************************************************************ --
# *****           P A R T I C L E  S I M U L A T O R  (PSim)         ***** --
# *****               FOR  CLASSICAL PARTICLES                       ***** --
# ************************************************************************ --
# $Id: http_support.sh 17 2010-10-02 07:15:52Z  $
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

http_file="./index.html"

## http support functions
function T { 
	echo $1  >> ${http_file}
}

function http_begin {
http_file=$1	
cat > ${http_file} <<EOF
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
<head>
   <meta content="text/html; charset=ISO-8859-1" http-equiv="content-type">
   <title>$2</title>
</head>
<body>
EOF
}

function http_end {
cat >> ${http_file} <<EOF
</body>
</html>
EOF
}

function http_header {
	T "<h$1>$2</h$1>" 
}

function http_file {
	T "<listing>" 
	cat $1 >> ${http_file}
	T "</listing>" 
}
	
function http_image {
	T "<a href=\"$1\">"
    T "<img style=\"width: 500px; height: 380px;\" alt=\"$1\" src=\"$1\">"
    T "</a>"
}

function http_table {
    T "<table width=\"90%\">" 
}

function http_table_end {
    T "</table>" 
}

function http_row {
   T "<tr>" 	
}

function http_row_end {
   T "</tr>" 
}

function http_column {
    T "<td>" 
}

function http_column_end {
    T "</td>" 
}

function http_link {
    T "<a href=\"$2\">$1</a>"	
}

function http_p {
    T "<p>"	
}

function http_p_end {
    T "</p>"	
}