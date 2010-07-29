#!/usr/bin/perl -w
# perl -MCPAN -e 'install PerlMol'
# sudo install perl openbabel
use strict;
#use Chemistry::OpenBabel;

my $sdf_file=$ARGV[0];

if ( scalar(@ARGV)<1 ) {
    print STDERR << "EOF";
Error: Usage: gen_tanimoto_matrix.pl file.sdf
EOF
	exit(1);
}

# add the ids to the title
# babel fich.orig fichwtitles --append DSSTox_FileID

# How to get the Id? 
`babel $sdf_file $sdf_file.smi`;

open(FD,$sdf_file.smi)  || die "Unable to open $sdf_file.smi\n";

while(!eof(FD)) {
    $_=<FD>;
    babel -f 1 -l 1 $sdf_file 1.sdf
    `echo $_ > $sdf_file.tmp.smi`;
    print "babel $sdf_file $sdf_file.tmp.smi 
}
