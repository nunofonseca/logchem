#!/bin/bash
##################################################################################
#
# Copyright (c)  2008, 2009, 2010 Nuno A. Fonseca (nunofonseca at acm.org)
#
#    This file is part of LogCHEM.
#
#    LogCHEM is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    LogCHEM is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with LogCHEM; if not, write to the Free Software
#    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
#
##################################################################################
# $Id: install.sh,v 1.8 2010/06/30 14:53:47 nf Exp $
###########################################################################################/

if [ "$1" == "-h" ]; then
    echo "Usage: install.sh [logchem directory]"
    echo "LogCHEM is installed in the current directory if no directory is provided."
    exit 0
fi

if [ "$1-" == "-" ]; then
    LogChemDir=`pwd`
else
    if [ ! -e $1 ]; then
	echo "creating $1"
	mkdir $1
    fi
    if [ ! -d $1 ]; then
	echo "Error: $1 exists and is not a directory"
	exit 1
    fi
    LogChemDir=$1
fi

function check_cmd {
    cmd_name=$1
    cmd_exec=$2
    cmd_mandatory=$3
    # by default all cmds are mandatory
    # check if cmd_name is in the path
    P=`bash -c "which $cmd_exec | cut -f 2 -d\  2> /dev/null"`
    if [ "$P" == "" ] && [ "$cmd_mandatory" == "no" ]; then 
	echo "WARNING: $cmd_name not found" > /dev/stderr
    else
	if [ "$P" == "" ]; then 
	    echo "ERROR: $cmd_name not found" > /dev/stderr
	    exit 1
	else
	    echo "INFO: $cmd_name found: $P"   > /dev/stderr
	fi
    fi
    echo "$P"
}

####################################################

echo "INFO: Checking dependencies..."
##############################
# check if YAP is in the path
YAP_PATH=`check_cmd YAP yap`

if [ "$YAP_PATH" = "" ]; then exit 1; fi


##############################
# check if vmd is in the path
check_cmd VMD vmd > /dev/null

##############################
# check if openbabel is in the path
check_cmd OpenBabel babel > /dev/null

# perl
check_cmd perl perl > /dev/null

# JChem - optional
check_cmd JChem generatemd no > /dev/null

# gzip
check_cmd gzip gzip > /dev/null

check_cmd TCL tclsh > /dev/null
check_cmd TK wish > /dev/null

###################################################
# update path in scripts
mkdir -p $LogChemDir/bin
mkdir -p $LogChemDir/lib

# Executables
cd src/bin
echo "INFO: Installing executables in $LogChemDir/bin"
FILES="lc_sdf2plstruct lc_matchmol lc_runaleph lc_vmd  lc_sdf_list_props logchem lc_eval_pat lc_refine_pat lc_sdfconv.pl"
for f in $FILES; do
    echo -n "INFO: installing $f..."
    F=$LogChemDir/bin/$f
    cp $f $F
    chmod +x $F
    sed -i "s,^#..*yap ,\#\!$YAP_PATH ," $F
    sed -i "s,^yap ,$YAP_PATH ," $F
    sed -i "s,^#LOGCHEM_DIR=.*,LOGCHEM_DIR=$LogChemDir," $F
    sed -i "s,^set LOGCHEM_DIR.*,set LOGCHEM_DIR \"$LogChemDir\"," $F
    echo "done."
done
cd ../..
# Aux. files
cd src
cp -rf pl tcl $LogChemDir/lib
sed -i "s,^logchem_dir(.*,logchem_dir('$LogChemDir').," $LogChemDir/lib/pl/bk_template.b
sed -i "s,^logchem_dir(.*,logchem_dir('$LogChemDir').," $LogChemDir/lib/pl/logchem_matchmol.pl
cd ..
echo "***************************************************"
echo "Please add $LogChemDir/bin to your path."
echo "To start logchem run 'logchem'"
exit