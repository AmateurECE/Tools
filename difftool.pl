#!/usr/bin/perl -w

use strict;
use warnings;

my $file = $ARGV[0];
my $cd = $ENV{'PWD'};
my $git_dir = $ENV{'DIR_GIT'};
my $local_dir = $ENV{'DIR_LOC'};
open my $loc, "<$cd/$local_dir/$file" or die "Could not open $file in $local_dir: $!\n";
open my $git, "<$cd/$git_dir/$file" or die "Could not open $file in $git_dir: $!\n";

my $same = "same";
while (<$git>) {
    my $line = <$loc>;
    if ($_ ne $line) {
	chomp $_;
	print "In Git $file: $_\nIn Local $file: $line";
	$same = "";
	last;
    }
}

if ($same eq "same") {
    print "$file observed. No differences found.\n";
}
