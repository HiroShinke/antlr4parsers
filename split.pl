

use strict;
use autodie qw(open);

my $fh;

-d "MIST" || mkdir("NIST");

while( my $l = <> ){
    if( $l =~ /^CCVS85/ ){
	;
    }
    elsif( $l =~ /^\*END-OF/ ){
	close($fh);
	# undef $fh;
    }
    elsif( my ($lib,$p) = ( $l =~ /^\*HEADER,(\w+),(\w+)/ ) ){
	-d "NIST/$lib" || mkdir("NIST/$lib");
	open($fh,">","NIST/$lib/$p.cbl");
    }
    else {
	print $fh $l
    }
}
