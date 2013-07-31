#!/usr/bin/perl -w

use strict;

use LWP::Simple;

my $ip = get("http://yaxu.org/tmp/bcn-ip.txt");
if ($ip) {
    chomp($ip);
    if ($ip =~ /^\d+\.\d+\.\d+\.\d+$/) {
#	print $ip;
#	exit 0;
    }
}
print "127.0.0.1";

