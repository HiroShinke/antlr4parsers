

use strict;
use autodie qw(open);

my $fh;

mkdir("NIST");

while( my $l = <> ){
    if( $l =~ /^CCVS85/ ){
	next;
    }
    elsif( my ($p) = ( $l =~ /^\*HEADER,COBOL,(\w+)/ ) ){
	open($fh,">","NIST/$p.cbl");
    }
    else {
	print $fh $l
    }
}
