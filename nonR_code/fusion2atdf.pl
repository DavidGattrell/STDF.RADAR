#!/usr/bin/perl 
#!/usr/bin/perl -d
#
# fusion2atdf.pl
#    script to process an ascii datalog from a Fusion tester and create
#    an atdf version that can then be converted to stdf.
#
# $Id: fusion2atdf.pl,v 1.1 2016/07/25 00:03:12 david Exp $
#
#    Usage:
#       fusion2atdf.pl fusion.txt fusion.atdf 
#
# Copyright (C) 2015-2016 David Gattrell
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
#
#
# David Gattrell   david.gattrell@gmail.com  4dec2015
#              6dec2015 - TSR/SBR/HBR/PCR records added
#              3jan2016 - now works with old/new formats, 
#                         and multiple-site-per-line and line-per-site formats
#			   4jan2016 - got old/line-per-site testcase, update script to work with this too.
#
# **********************************************************************
#

use strict;
use warnings;

use constant TRUE	=> 1;
use constant FALSE	=> 0;


# file section tracking variables
#--------------------------------
my $in_final_summary = FALSE;	# flag used to disable parsing until we have reached final summary,
								# this should help make things run a bit faster.
my $in_sublot_summary = FALSE;	# flag used to disable parsing until we have reached sublot summary,
								# this should help make things run a bit faster.
my $in_per_site_count_summary = FALSE;	# flag used to disable parsing until we have reached 
								# "Device Count Summary" section
my $in_device = FALSE;			# use to insert PIR/PRR records...
my $BPS_printed = FALSE;		# kluge, hide first BPS/EPS to avoid look ahead parsing for PIR per site
my %PIR_generated = ();			# track if PIR has been printed yet for this site
my $in_binning = FALSE;			# gather binning info for PRR record for each site
my $need_test_cols = TRUE;		# confirm spacing, only need to do on first occurrence.
my $final_summary_exists = FALSE;	# if exists, don't use sublot summary for SBR/HBR/TSR's
									# check for existence in first pass of file,
									# do actual parsing for SBR/HBR/TSR's in second pass
my $in_test_result_summary = FALSE;	#
my $in_bin_result_summary = FALSE;
my $in_hbin_summary = FALSE;
my $in_device_count_summary = FALSE;	# REVISIT... need to add this / PCR records
my $in_header = FALSE;			# only exists in older format files
my $old_version = FALSE;		# older vs newer format of ascii file Cx vs Dragon?


# ATDF header related variables
#--------------------------------
my $lot_id = "";
my $sublot_id = "";
my $part_typ = "";
my $job_nam = "";
my $node_nam = "";
my $tstr_typ = "Fusion_PAX";	# assumption here that is is PAX!
my $setup_t = "";
my $start_t = "";
my $oper_nam = "";
my $mode_cod = "";
my $stat_num = "";
my $test_cod = "";
my $rtst_cod = "";
my $job_rev = "";
my $exec_typ = "";
my $exec_ver = "";
my $prot_cod = "";

my $hand_typ = "";

# other ATDF field variables
#----------------------------
my $bps_name = "";
my $part_id = "";
my $x_coord = "";
my $y_coord = "";
my $pf_flag = "";
my $sw_bin = "";
my $hw_bin = "";
my $num_test = 0;
my $finish_t = "";

my $tnum = 0;
#my $pf_flag = "";			# P or F
my $lo_lim = "";
my $result = -999.9;
my $units = "";
my $hi_lim = "";
my $tst_nam = "";
my $pin_nam = "";
my $type = "";		# P parametric, F functional
my $execs = 0;
my $fails = 0;
my $pct = 0;
my $count = 0;
my $bin_nam = "";

my @sites = ();
my $site = "";
my $sc = 0;					# number of sites, not index of last ..ie 4 for 4 sites
my $site_count = 0;	
my %test_ts = ();			# REVISIT.. goes in PRR	
my $i = 0;					# site count counter

my $line_no = 0;
my $line = "";
my $prev_line = "";			# copy of previous line, for parsing "---- ---" lines
my $subline = "";			# copy of line that gets parsed/modified
my $tmp = "";
my $pos = -1;			
my $offset = 0;
my @col_starts = ();
my @tcol_starts = ();		# test line columns
my @bcol_starts = ();		# bin summary columns

my $day = "";
my $mon = "";
my $date = "";
my $time = "";
my $year = "";

my $debug1 = TRUE;			# 1st pass / 2nd pass status lines
my $debug2 = FALSE;
my $debug3 = FALSE;
my $debug4 = FALSE;			# parsing the "----" line to determine per device binning columns
my $debug5 = FALSE;			# parsing the "----" line to determine test line columns
my $debug6 = FALSE;			# TSR/SBR/HBR parsing
my $debug7 = FALSE;			# adding old format support / multiple site per line 

#-------------------------------------
# start of program
#-------------------------------------
if (($#ARGV < 1) || ($#ARGV > 1)) {
	print "Expected 2 arguments, fusion_datalog.txt fusion_datalog.atdf\n";
	print "USAGE: fusion2atdf.pl fusion_datalog.txt fusion_datalog.atdf\n";

	die;
}
print "arguments are >>@ARGV<< \n" if $debug2;

open INPUT,"$ARGV[0]" or die "ERROR: unable to open input filename $ARGV[0]\n";
open OUTPUT, ">$ARGV[1]" or die "ERROR: unable to open output filename $ARGV[1]\n";


# We need to parse the file 2x,
# - the first time, we just want summary information to build the MIR record which NEEDS to be 
#   at the beginning in an ATDF file
# - the second time, we can look at the actual per-part data and generate those ATDF records
#--------------------------------------------------------------------------------------------
print "now starting first pass parsing to generate ATDF header records...\n" if $debug1;
$line_no = 0;
while ($line = <INPUT>) {
	chomp($line);
	$line_no++;

	if ($line =~ /^\s+enVision_AsciiDL,  Version/) {
		$old_version = TRUE;
		# NOTE: sometimes it can say "stlist,  Version 2.2"
	} elsif ($line =~ /^Start of Lot/) {
		# extract $lot_id and $part_typ, in case no summary info in file
		# -- only in new format files
		$line =~ /LotID: (\S)+ /;
		$lot_id = $1;
		$line =~ /Device Name: (\S)+ /;
		$part_typ = $1;
	} elsif ($line =~ /^Device Count Summary:/) {
		# parse this block to determine active sites for building SDR record
		# it seems it can occur multiple times, so reset list each time
		# (for old version site info has already been parsed and is not in
		#  this summary section, so don't wipe/reset variable!
		# )
		if (!$old_version) {
			@sites = ();
		}
		$in_per_site_count_summary = TRUE;
	} elsif ($line =~ /^\s+SUBLOT SUMMARY\s*$/) {
		$in_sublot_summary = TRUE;
	} elsif ($line =~ /^\s+FINAL SUMMARY\s*$/) {
		$final_summary_exists = TRUE;
		$in_final_summary = TRUE;
		$in_sublot_summary = FALSE;
	} elsif ($line =~ /^Program Name:\s+/) {
		# the header only exists in older format files
		$in_header = TRUE;
		$old_version = TRUE;
	}

	if ($in_header) {
		if ($line =~ /^Program Name:\s+(.*)$/) {
			$job_nam = $1;
		} elsif ($line =~ /^Start Time:\s+(.*)$/) {
			$tmp = $1;			# Tue Oct 20 17:44:44 2015
			($day,$mon,$date,$time,$year) = split(/ +/,$tmp);
			$start_t = "$time $date-$mon-$year";	# 01:05:51 11-MAR-2015
		} elsif ($line =~ /^Device Name:\s+(.*)$/) {
			$part_typ = $1;
		} elsif ($line =~ /^Lot Retest:\s+(.*)$/) {
			$rtst_cod = $1;
		} elsif ($line =~ /^Lot ID:\s+(.*)$/) {
			$lot_id = $1;
		} elsif ($line =~ /^Sublot ID:\s+(.*)$/) {
			$sublot_id = $1;
		} elsif ($line =~ /^Tester Node Name:\s+(.*)$/) {
			$node_nam = $1;
		} elsif ($line =~ /^Test Head:\s+(.*)$/) {
		} elsif ($line =~ /^Tester Type:\s+(.*)$/) {
			$tstr_typ = $1;
		} elsif ($line =~ /^Operator ID:\s+(.*)$/) {
			$oper_nam = $1;
		} elsif ($line =~ /^enVision Version:\s+(.*)$/) {
			$exec_ver = $1;
		} elsif ($line =~ /^FAB ID:\s+(.*)$/) {
		} elsif ($line =~ /^Active Flow:\s+(.*)$/) {
		} elsif ($line =~ /^\s+Site Numbers:\s+(.*)$/) {
			$tmp = $1;
			@sites = split(/ +/,$tmp);
			print "Sites... @sites \n" if $debug7;
		} elsif ($line =~ /^\s+Active Adapter Board:\s+(.*)$/) {
		} elsif ($line =~ /^\s+Test No\.\s+Minimum\s+Maximum/) {
			$in_header = FALSE;
			
			# old format data file...
			# we don't need to parse the rest of the file, since we already
			# have all the MIR/SDR information now
			# we can now drop down to the second parsing loop below
			last;
		}
	}
	
	if ($in_per_site_count_summary) {
		# will do this 2x, if sublot and final summary are present,
		# but that is ok, final summary will overwrite duplicate sublot data
		if ($line =~ /^\s+(\d+)\s+/) {
			push @sites,$1;
			print "added $1 to \@sites \n" if $debug3;
		} elsif ($line =~ /^\s*ALL\s+/) {
			$in_per_site_count_summary = FALSE;
		}
	}

	if ($in_final_summary || $in_sublot_summary) {
		# if final summary exists, it will overwrite sublot summary, which is what we want
		if ($line =~ /^FILE NAME\s+: (.*)$/) {
			$job_nam = $1;		# REVISIT, I'm clobbering it below
		} elsif ($line =~ /^PROGRAM NAME\s+: (.*)$/) {
			$job_nam = $1;
		} elsif ($line =~ /^USER NAME\s+: (.*)$/) {
		} elsif ($line =~ /^DEVICE NAME\s+: (.*)$/) {
			$part_typ = $1;
		} elsif ($line =~ /^LOT ID\s+: (.*)$/) {
			$lot_id = $1;
		} elsif ($line =~ /^SUBLOT ID\s+: (.*)$/) {
			$sublot_id = $1;
		} elsif ($line =~ /^LOT STATUS\s+: (.*)$/) {
			$subline = $1;
			if ($subline eq "New Test") {
				$rtst_cod = "N";
			} else {
				$rtst_cod = "Y";
			}
		} elsif ($line =~ /^LOT TYPE\s+: (.*)$/) {
		} elsif ($line =~ /^LOT DESCRIPTION\s+: (.*)$/) {
		} elsif ($line =~ /^PRODUCT ID\s+: (.*)$/) {
		} elsif ($line =~ /^WAFER ID\s+: (.*)$/) {
		} elsif ($line =~ /^FAB ID\s+: (.*)$/) {
		} elsif ($line =~ /^LOT START TIME\s+: (.*)$/) {
			$tmp = $1;			# Tue Oct 20 17:44:44 2015
			($day,$mon,$date,$time,$year) = split(/ +/,$tmp);
			$start_t = "$time $date-$mon-$year";	# 01:05:51 11-MAR-2015
		} elsif ($line =~ /^SHIFT\s+: (.*)$/) {
		} elsif ($line =~ /^OPERATOR ID\s+: (.*)$/) {
			$oper_nam = $1;
		} elsif ($line =~ /^TESTER NAME\s+: (.*)$/) {
			$node_nam = $1;
		} elsif ($line =~ /^ACTIVE FLOW\s+: (.*)$/) {
		} elsif ($line =~ /^ACTIVE LOADBOARD\s+: (.*)$/) {
		} elsif ($line =~ /^LOCAL TIME\s+: (.*)$/) {
			$tmp = $1;			# Tue Oct 20 17:44:44 2015
			($day,$mon,$date,$time,$year) = split(/ +/,$tmp);
			$finish_t = "$time $date-$mon-$year";	# 01:05:51 11-MAR-2015
		} elsif ($line =~ /^GM TIME\s+: (.*)$/) {
		} elsif ($line =~ /^PROBER\/HANDLER\s+: (.*)$/) {
			$hand_typ = $1;
		} elsif ($line =~ /^HEADER\s+: (.*)$/) {
		}
	}

}
$in_final_summary = FALSE;
$in_sublot_summary = FALSE;
for $site (@sites) {
	$PIR_generated{$site} = FALSE;
}

if ($old_version) {
	# if old format, do not need to worry about partial and final summary
	# so set this flag
	$final_summary_exists = TRUE;
}



# now create the header records of the ATDF file
#-------------------------------------------------
print OUTPUT "FAR:A|4|2\n";
print OUTPUT "MIR:$lot_id|$part_typ|$job_nam|$node_nam|$tstr_typ|$setup_t|$start_t|";
print OUTPUT "$oper_nam|$mode_cod|$stat_num|$sublot_id|$test_cod|$rtst_cod|$job_rev|";
print OUTPUT "$exec_typ|$exec_ver|$prot_cod";
print OUTPUT "\n";
print OUTPUT "SDR:1|1|" , join(',',@sites) , "|$hand_typ\n";	# REVISIT... can add loadboard type



# 2nd pass parsing...
# now generating PIR/PRR and summary portions of the ATDF file
#-----------------------------------------------------------------
print "now starting second pass parsing to generate rest of ATDF records...\n" if $debug1;
close(INPUT);
open INPUT,"$ARGV[0]" or die "ERROR: unable to open input filename $ARGV[0]\n";

$line_no = 0;
$prev_line = "";
while ($line = <INPUT>) {
	chomp($line);
	$line_no++;

	if ($in_binning) {
		if ($line =~ /\s+(\d+)\s+(\d+)/) {
			$site = $1;
			$part_id = $2;
			
			$x_coord =  substr($line, $col_starts[2],$col_starts[3]-$col_starts[2]);
			$x_coord =~ s/^\s+|\s+$//g;	# remove leading and trailing whitespace
			$y_coord =  substr($line, $col_starts[3],$col_starts[4]-$col_starts[3]);
			$y_coord =~ s/^\s+|\s+$//g;	# remove leading and trailing whitespace
			$pf_flag =  substr($line, $col_starts[4],$col_starts[5]-$col_starts[4]);
			$pf_flag =~ s/^\s+|\s+$//g;	# remove leading and trailing whitespace
			$sw_bin =   substr($line, $col_starts[5],$col_starts[6]-$col_starts[5]);
			$sw_bin =~  s/^\s+|\s+$//g;	# remove leading and trailing whitespace
			$hw_bin =   substr($line, $col_starts[6],$col_starts[7]-$col_starts[6]);
			$hw_bin =~  s/^\s+|\s+$//g;	# remove leading and trailing whitespace
			$num_test = substr($line, $col_starts[7],$col_starts[8]-$col_starts[7]);
			$num_test =~ s/^\s+|\s+$//g;	# remove leading and trailing whitespace
			
			print OUTPUT "PRR:1|$site|$part_id|$num_test|$pf_flag|$hw_bin|";
			#print OUTPUT "$sw_bin|$x_coord|$y_coord|||$test_ts{$site}\n"; # REVISIT when $test_ts valid
			print OUTPUT "$sw_bin|$x_coord|$y_coord|||\n";

			#reset PIR printing flag
			$PIR_generated{$site} = FALSE;

		} elsif ($line =~ /^\s*---+\s+---+/) {
			# use the "----" sections to determine field widths
			@col_starts = ();
			$offset = 0;
			$pos = index($line," -",$offset);
			print " pos set to $pos \n" if $debug4;
			while ($pos >= 0) {
			 	push(@col_starts,$pos);
				$offset = $pos + 2;
				$pos = index($line," -",$offset);
			}
			push(@col_starts,length($line));
			print "binning col_starts: @col_starts\n" if $debug4;
		} elsif ($line =~ /^\s*$/) {
			$in_binning = FALSE;
			$in_device = FALSE;
		}
	} elsif ( $in_final_summary || (! $final_summary_exists && $in_sublot_summary)) {
		if ($in_test_result_summary) {
			print "In TSR parsing section\n" if $debug6;
			#&parse_test_result_summary();
			parse_test_result_summary();
		} elsif ($in_bin_result_summary) {
			print "In SBR parsing section\n" if $debug6;
			#&parse_sbin_result_summary();
			parse_sbin_result_summary();
		} elsif ($in_hbin_summary) {
			print "In HBR parsing section\n" if $debug6;
			#&parse_hbin_result_summary();
			parse_hbin_result_summary();
			# REVISIT!!
		} elsif ($in_device_count_summary) {
			print "In PCR parsing section\n" if $debug6;
			#&parse_part_count_summary();
			parse_part_count_summary();
		} elsif ($line =~ /^Test Result Summary:/) {
			print "Turning on TSR flag\n" if $debug6;
			$in_test_result_summary = TRUE;
		} elsif ($line =~ /^Bin Summary:/) {
			print "Turning on SBR flag\n" if $debug6;
			$in_bin_result_summary = TRUE;
			$in_test_result_summary = FALSE;
		} elsif ($line =~ /^Hardware Bin Summary for all Sites:/) {
			# old format...
			#   HBR is before SBR info
			print "Turning on HBR flag\n" if $debug6;
			$in_hbin_summary = TRUE;
			$in_test_result_summary = FALSE;
		} elsif ($line =~ /^Lot Finish Time:\s+(.*)$/) {
			# old format... 
			$tmp = $1;			# Tue Oct  7 17:44:44 2015
			($day,$mon,$date,$time,$year) = split(/ +/,$tmp);
			$finish_t = "$time $date-$mon-$year";	# 01:05:51 11-MAR-2015
		}
	} elsif ($line =~ /^GROUP : (.*)$/) {
		$bps_name = $1;
		if (!$in_device) {
			$in_device = TRUE;
			#for $site (@sites) {	# REVISIT.. assumes all sites active
			#	print OUTPUT "PIR:1|$site\n";
			#}
		} else {
			if ($BPS_printed) {
				print OUTPUT "EPS:\n";
			}
			print OUTPUT "BPS:$bps_name\n";
			$BPS_printed = TRUE;
		}
	} elsif ($need_test_cols && ($line =~ /^\s?\s?---+\s+---+/)) {
		# use the first "---------  ---  ---- ----..." occurrence to 
		# determine test information column spacing
		@tcol_starts = ();
		push(@tcol_starts,0);	
		#$offset = 0;
		$offset = index($line,"-");	# start looking for 2nd column
		$pos = index($line," -",$offset);
		while ($pos >= 0) {
			if ($pos > 0) {
				push(@tcol_starts,$pos);
			}
			$offset = $pos + 2;
			$pos = index($line," -",$offset);
		}
		push(@tcol_starts,length($line));
		print "test col starts: @tcol_starts\n" if $debug5;
		$need_test_cols = FALSE;	# speed things up, only do this once.
	#} elsif ($line =~ /^\s+(\d+)\s+[*]?([PF])[*]?\s+(\d+)\s+/) {
	# for old format, P is implicit... does this work for both formats?
	} elsif ($line =~ /^\s+(\d+)\s+[*]?([PF]?)[*]?\s+(\d+)\s+/) {
		# if in single site per line mode datalogging...
		# a test.. parse and generate a PTR record
		$tnum = $1;
		$pf_flag = $2;
		$site = $3;

		$lo_lim =  substr($line, $tcol_starts[3],$tcol_starts[4]-$tcol_starts[3]);
		$lo_lim =~ s/^\s+|\s+$//g;	# remove leading and trailing whitespace
		$lo_lim =~ s/([^0-9]*)$//;	# remove units
		$result =  substr($line, $tcol_starts[4],$tcol_starts[5]-$tcol_starts[4]);
		$result =~ s/^\s+|\s+$//g;	# remove leading and trailing whitespace
		$result =~ s/([^0-9]*)$//;	# remove units
		$units = $1;
		$hi_lim =  substr($line, $tcol_starts[5],$tcol_starts[6]-$tcol_starts[5]);
		$hi_lim =~ s/^\s+|\s+$//g;	# remove leading and trailing whitespace
		$hi_lim =~ s/([^0-9]*)$//;	# remove units
		$pin_nam =  substr($line, $tcol_starts[6],$tcol_starts[7]-$tcol_starts[6]);
		$pin_nam =~ s/^\s+|\s+$//g;	# remove leading and trailing whitespace
		$tst_nam =  substr($line, $tcol_starts[7],$tcol_starts[8]-$tcol_starts[7]);
		$tst_nam =~ s/^\s+|\s+$//g;	# remove leading and trailing whitespace

		# do we need to insert a PIR record here?
		if (! $PIR_generated{$site}) {
			print OUTPUT "PIR:1|$site\n";
			$PIR_generated{$site} = TRUE;
		}
		if ($pin_nam =~ /\S+/) {
			# if $pin_nam exists, append to $tst_nam....
			print OUTPUT "PTR:$tnum|1|$site|$result|$pf_flag||$tst_nam/$pin_nam|||$units|";
		} else {
			print OUTPUT "PTR:$tnum|1|$site|$result|$pf_flag||$tst_nam|||$units|";
		}
		print OUTPUT "$lo_lim|$hi_lim\n";
	} elsif ($line =~ /^\s+(\d+)\s+(\S+) (\S+)\s+(\S+) \S+\s+/) {
		# if in multiple site per line mode datalogging...
		$tnum = $1;
		$lo_lim = $2;
		$units = $3;
		$hi_lim = $4;
		
		$sc = @sites;	# returns number of sites, not index of last ..ie 4 for 4 sites
		$pin_nam =  substr($line, $tcol_starts[3+$sc],$tcol_starts[4+$sc]-$tcol_starts[3+$sc]);
		$pin_nam =~ s/^\s+|\s+$//g;	# remove leading and trailing whitespace
		$tst_nam =  substr($line, $tcol_starts[4+$sc],$tcol_starts[5+$sc]-$tcol_starts[4+$sc]);
		$tst_nam =~ s/^\s+|\s+$//g;	# remove leading and trailing whitespace

		for ($i=0; $i<$sc; $i++) {
			$site = $sites[$i];	
			$tmp =  substr($line, $tcol_starts[3+$i],$tcol_starts[4+$i]-$tcol_starts[3+$i]);
			# ie "F     9.000 -     "  or "     49.680 mV    "
			#    OR  "                  " if serial loop test!
			if ($tmp =~ /^(F?)\s*(\S+) (\S+)/) {
				$pf_flag = $1;
				$result = $2;

				# do we need to insert a PIR record here?
				if (! $PIR_generated{$site}) {
					print OUTPUT "PIR:1|$site\n";
					$PIR_generated{$site} = TRUE;
				}
				# REVISIT  $pin_nam is not used
				if ($pin_nam =~ /\S+/) {
					# if $pin_nam exists, append to $tst_nam....
					print OUTPUT "PTR:$tnum|1|$site|$result|$pf_flag||$tst_nam/$pin_nam|||$units|";
				} else {
					print OUTPUT "PTR:$tnum|1|$site|$result|$pf_flag||$tst_nam|||$units|";
				}
				print OUTPUT "$lo_lim|$hi_lim\n";
			}
		}
		

	} elsif ($line =~ /^\s+SUBLOT SUMMARY\s*$/) {
		$in_sublot_summary = TRUE;
	} elsif ($line =~ /^\s+FINAL SUMMARY\s*$/) {
		$in_final_summary = TRUE;
		$in_sublot_summary = FALSE;
	} elsif ($line =~ /^Test Result Summary:\s*$/) {
		# old format.. use this to determine we are in the final summary section of the file
		$in_final_summary = TRUE;

		print "Turning on TSR flag\n" if $debug6;
		$in_test_result_summary = TRUE;

	} elsif ($line =~ /^\s+Site  Device ID /) {
		# we have finished testing, we now need to parse for binning  (ie. done PTR's, parse for PRR)
		if ($BPS_printed) {
			print OUTPUT "EPS:\n";
			$BPS_printed = FALSE;
		}
		$in_binning = TRUE;
	}

	$prev_line = $line;
}

print OUTPUT "MRR:$finish_t\n";

close(INPUT);
close(OUTPUT);


###########################################################
#### SUBROUTINES 
###########################################################
#    
#    
#    
sub parse_test_result_summary
{
			if ($line =~ /^\s+(\d+)\s+(\d+)\s+(\S+)\s+(\d+)\s+(\d+)\s+(\S+)\s+(.+)$/) {
				$tnum = $1;
				$site = $2;
				$type = $3;
				$execs = $4;
				$fails = $5;
				$pct = $6;
				$tst_nam = $7;
				print OUTPUT "TSR:1|$site|$tnum|$tst_nam|$type|$execs|$fails\n";
			} elsif ($line =~ /^\s+(\d+)\s+(\S+)\s+(\d+)\s+(\d+)\s+(\S+)\s+(.+)$/) {
				# for old format, multiple sites follow initial site, but with test no field
				# empty and test name field empty
				#$tnum = $1;
				$site = $1;
				$type = $2;
				$execs = $3;
				$fails = $4;
				$pct = $5;
				#$tst_nam = $7;
				print OUTPUT "TSR:1|$site|$tnum|$tst_nam|$type|$execs|$fails\n";
			} elsif ($line =~ /^\s+(\d+)\s+(All)\s+(\S+)\s+(\d+)\s+(\d+)\s+(\S+)\s+(.+)$/) {
				$tnum = $1;
				$site = "";		# All translates to "" in TSR
				$type = $3;
				$execs = $4;
				$fails = $5;
				$pct = $6;
				$tst_nam = $7;
				print OUTPUT "TSR:1|$site|$tnum|$tst_nam|$type|$execs|$fails\n";
			} elsif ($line =~ /^\s*$/) {
				$in_test_result_summary = FALSE;
			}

}
###########################################################
sub parse_sbin_result_summary
{
	if ($old_version) {
			if ($line =~ /^\s+(\d+)\s+([PF])\s+(\d+)\s+(\S+)\s+(.*)$/) {
				$site = "";
				$pf_flag = $2;
				$sw_bin = $1;
				$count = $3;
				$bin_nam = $5;
				print OUTPUT "SBR:|$site|$sw_bin|$count|$pf_flag|$bin_nam\n";
			} elsif ($line =~ /^Device Count Summary/) {
				$in_bin_result_summary = FALSE;
				$in_device_count_summary = TRUE;	
			}
	} else {
			if ($line =~ /^\s+(\d+)\s+([PF])\S+\s+(\d+)\s+(\d+)\s+/) {
				$site = $1;
				$pf_flag = $2;
				$sw_bin = $3;
				$hw_bin = $4;
				$bin_nam =  substr($line, $bcol_starts[4],$bcol_starts[5]-$bcol_starts[4]);
				$bin_nam =~ s/^\s+|\s+$//g;	# remove leading and trailing whitespace
				$count =  substr($line, $bcol_starts[5],$bcol_starts[6]-$bcol_starts[5]);
				$count =~ s/^\s+|\s+$//g;	# remove leading and trailing whitespace
				print OUTPUT "SBR:|$site|$sw_bin|$count|$pf_flag|$bin_nam\n";
			} elsif ($line =~ /^\s+(ALL)\s+([PF])\S+\s+(\d+)\s+(\d+)\s+/) {
				$site = "";
				$pf_flag = $2;
				$sw_bin = $3;
				$hw_bin = $4;
				$bin_nam =  substr($line, $bcol_starts[4],$bcol_starts[5]-$bcol_starts[4]);
				$bin_nam =~ s/^\s+|\s+$//g;	# remove leading and trailing whitespace
				$count =  substr($line, $bcol_starts[5],$bcol_starts[6]-$bcol_starts[5]);
				$count =~ s/^\s+|\s+$//g;	# remove leading and trailing whitespace
				print OUTPUT "SBR:|$site|$sw_bin|$count|$pf_flag|$bin_nam\n";
			} elsif ($line =~ /^----\s+--+/) {
				# use the "----" sections to determine field widths
				@bcol_starts = ();
				push(@bcol_starts,0);
				$offset = 0;
				$pos = index($line," -",$offset);
				print " pos set to $pos \n" if $debug4;
				while ($pos >= 0) {
					push(@bcol_starts,$pos);
					$offset = $pos + 2;
					$pos = index($line," -",$offset);
				}
				push(@bcol_starts,length($line));
				print "bin summary col_starts: @bcol_starts\n" if $debug4;
			} elsif ($line =~ /^Hardware Bin Summary:/) {
				$in_bin_result_summary = FALSE;
				$in_hbin_summary = TRUE;
			}
	}
}
###########################################################
sub parse_hbin_result_summary
{
	if ($old_version) {
			if ($line =~ /^\s+(\d+)\s+([PF])\s+(\d+)\s+(\S+)/) {
				$site = "";
				$pf_flag = $2;
				$hw_bin = $1;
				$count = $3;
				print OUTPUT "HBR:|$site|$hw_bin|$count|$pf_flag\n";
			} elsif ($line =~ /^Software Bin Summary for all Sites:/) {
				$in_hbin_summary = FALSE;
				$in_bin_result_summary = TRUE;	# aka sbin summary
			}
	} else {
			if ($line =~ /^\s+(\d+)\s+([PF])\S+\s+(\d+)\s+(\d+)/) {
				$site = $1;
				$pf_flag = $2;
				$hw_bin = $3;
				$count = $4;
				print OUTPUT "HBR:|$site|$hw_bin|$count|$pf_flag\n";
			} elsif ($line =~ /^\s+(ALL)\s+([PF])\S+\s+(\d+)\s+(\d+)/) {
				$site = "";
				$pf_flag = $2;
				$hw_bin = $3;
				$count = $4;
				print OUTPUT "HBR:|$site|$hw_bin|$count|$pf_flag\n";
			} elsif ($line =~ /Device Count Summary/) {
				$in_hbin_summary = FALSE;
				$in_device_count_summary = TRUE;
			}
	}
}
###########################################################
sub parse_part_count_summary
{
			if ($line =~ /\s+(\d+)\s+(\d+)\s+(\d+)\s+(\S+)/) {
				$site = $1;
				$execs = $2;
				$count = $3;
				print OUTPUT "PCR:|$site|$execs|||$count\n";
			} elsif ($line =~ /\s+(ALL)\s+(\d+)\s+(\d+)\s+(\S+)/) {
				$site = "";
				$execs = $2;
				$count = $3;
				print OUTPUT "PCR:|$site|$execs|||$count\n";
			} elsif ($line =~ /^\s*$/) {
				$in_device_count_summary = FALSE;
			}
}
###########################################################
