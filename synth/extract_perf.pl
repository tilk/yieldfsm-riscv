#!/usr/bin/env perl
use strict;
use warnings;
use feature "say";

opendir my $dh, "." or die;
my @cores = grep { -d "$_" && ! /^\.{1,2}$/ } readdir($dh);
closedir $dh;

my %data;

foreach my $core (@cores) {
    open my $fh, "$core/result.log" or die;
    $data{$core} = {fmax => 0, lc => 0};
    while (<$fh>) {
        if (/Max frequency for clock.*: (\d+.\d+) MHz/) {
            $data{$core}{fmax} = $1;
        }
        if (/ICESTORM_LC: *(\d+)\//) {
            $data{$core}{lc} = $1;
        }
    }
    close $fh;
}

my @values = ("fmax", "lc");
my %units = (fmax => "MHz", lc => "LCs");

foreach my $value (@values) {
    open my $fh, '>', "$value.json" or die;
    say $fh "[";
    my $iter = 0;
    foreach my $core (@cores) {
        if ($iter++ != 0) {
            say $fh "\t,";
        }
        say $fh "\t{";
        say $fh "\t\t\"name\": \"$core - $value\",";
        say $fh "\t\t\"unit\": \"$units{$value}\",";
        say $fh "\t\t\"value\": $data{$core}{$value}";
        say $fh "\t}";
    }
    say $fh "]";
    close $fh;
}

