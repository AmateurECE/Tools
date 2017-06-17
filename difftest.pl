#!/usr/bin/perl -w

use strict;
use warnings;

my $cd = $ENV{'PWD'};
my $git_dir = $ENV{'DIR_GIT'};
my $loc_dir = $ENV{'DIR_LOC'};
opendir my $dirh, "$cd/$git_dir" or die "Cannot open directory $git_dir: $!\n";
my @files = readdir $dirh;
closedir $dirh;

foreach my $file (@files) {
    if (!-f "$cd/$git_dir/$file" || !-e "$cd/$loc_dir/$file") {
	next;
    }
    $file = scriptosh($file);
    my $result = `perl difftool.pl $file`;
    print "$result";
}

# FUNCTION: scriptosh -- converts a file path name to a shell-friendly file path name;
#           delimits all special characters.
# PARAMETERS: file: String -- the file name to be manipulated
# RETURN: file: String -- the successfully mangled file path name.
sub scriptosh {
    my $file = $_[0];
    $file =~ s/(?<=[^\\])[\ \'\(\)&,:]/\\$&/g;
    return $file;
}
