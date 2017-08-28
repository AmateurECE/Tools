#!/usr/bin/perl
################################################################################
# NAME:		    tree.pl
#
# AUTHOR:	    Ethan D. Twardy
#
# DESCRIPTION:	    This program analyzes a file and recursively builds a
#		    primitive call graph for it.
#
# CREATED:	    08/28/2017
#
# LAST EDITED:	    08/28/2017
###

################################################################################
# Includes
###

use strict;
use warnings;

################################################################################
# Main
###

die "Expected a filename\n" if $#ARGV < 0;
my $graph;
my %regexs = (
    'as' => "^[ \t]*blr?[ \t]*([^ ^\t^\n]*)",
    'c'  => undef # TODO: THIS.
    );
get_calls($ARGV[0], \$graph);
print $graph, "\n";

################################################################################
# Subroutines
###

sub get_calls {
    my $rgraph = $_[1];
    $$rgraph .= "";
    local @ARGV = $_[0];
    $_[0] =~ m/.*\.(.*)$/;
    my $switch;
    if ($1 =~ m/S|s/) {
	$switch = 'as';
    } elsif ($1 =~ m/c/) {
	$switch = 'c';
    }

    print $_[0], "\n";
    my $line = 1;
    while (<>) {
	if (/$regexs{$switch}/) {
	    print "\t$line: ", $1, "\n";
	}
	$line++;
    }
}
################################################################################
