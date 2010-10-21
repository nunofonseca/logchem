#!/usr/bin/perl -w 
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
# $Id$
#
# perl -MCPAN -e 'install PerlMol'
# sudo yum install perl-openbabel

use strict;

if ( scalar(@ARGV)<4 ) {
    print STDERR << "EOF";
Error: Usage: sdf2.pl type{csv,arff} file.sdf <property1> ... 
Last property is the name of the property to be used as target class  when converting for arff format.
EOF
	exit(1);
}

use Chemistry::OpenBabel;

my $otype=shift @ARGV;
my $ifile=shift @ARGV;
my $obconversion = new Chemistry::OpenBabel::OBConversion;
$obconversion->SetInFormat("sdf");
my $obmol = new Chemistry::OpenBabel::OBMol;
my $notatend = $obconversion->ReadFile($obmol, "$ifile") ||  die "Unable to open $ifile\n";
my $var;

#my @vars2print=("molwt","natoms","nbonds","nhvyatoms","nres","energy","mass","tcharge","formula");
my @numvars=("molwt","natoms","nbonds","nhvyatoms","nres","energy","mass","tcharge");
my @stringvars=("formula");

##############################################################################
sub arg2data  {
    my($var) = @_;
    $var=~/(.*)\/(.*)/; 
    my @data=($1,$2);
    return @data;
}
sub csv_header () {
    print "Title";
    foreach $var (@numvars) {
	print ",$var";
    }
    foreach $var (@stringvars) {
	print ",$var";
    }
    foreach $var (@ARGV) {
	my @v=arg2data($var);
	print ",".$v[0];
    }
    print "\n";    
}
sub arff_header () {
    print "\@relation lc_ds\n";
    print "\@attribute molname string\n";
    foreach $var (@numvars) {
	print "\@attribute $var numeric\n";
    }
    foreach $var (@stringvars) {
	print "\@attribute $var string\n";
    }
    foreach $var (@ARGV) {
	my @v=arg2data($var);
	print "\@attribute ".$v[0]." ".$v[1]."\n";
    }
    print "\@data\n";
}
########################################################
# Header
#print $otype."\n";
if ( $otype eq "csv" ) {
    csv_header();
} else {
    arff_header();
}


######################################
# Data
while ($notatend) {
    my $title=$obmol->GetTitle();
    my $molwt=$obmol->GetMolWt();
    my $natoms=$obmol->NumAtoms();
    my $nbonds=$obmol->NumBonds();
    my $nhvyatoms=$obmol->NumHvyAtoms();
    my $nres=$obmol->NumResidues();
    my $energy=$obmol->GetEnergy();
    my $mass=$obmol->GetExactMass();
    my $tcharge=$obmol->GetTotalCharge();
    my $formula=$obmol->GetFormula();

    ##################
    # Title
    print "\"$title\"";
    # numeric values
    foreach $var (@numvars) {
	my $t=eval("\$$var");
	print ",$t";
    }
    # string values
    foreach $var (@stringvars) {
	my $t=eval("\$$var");
	print ",\"$t\"";
    }
    # user defined values
    foreach $var (@ARGV) {
	my @v=arg2data($var);
	my $vname=$v[0];
	my $vtype="$v[1]";

	my $o=$obmol->GetData("$vname");
	my $val;
	my $type;
	if ( defined($o) ) {
	    $val=$o->GetValue();
	    $type=$o->GetDataType();	    	    
	} else {
	    $val="?";
	    $type="?";
	}
#	print ",$var=$val/$type";
	if ( $vtype eq "numeric" ) {
	    print ",$val";
	} else {
	    print ",\"$val\"";
	}
    }
    print "\n";
    $obmol->Clear();
    $notatend = $obconversion->Read($obmol);
}
