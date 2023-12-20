#! /usr/bin/perl -w
#
# stdf_change_lotid.pl
#
# $Id: stdf_change_lotid.pl,v 1.2 2023/12/20 02:12:03 david Exp $
#
# stdf_change_lotid.pl [-in] in_file [-out] out_file [-e b|l]
#       -in in_file
#		-out out_file
#		-e or -endian big|little  
#		    endian-ness of stdf file will usually be automatically detected.
# 			this is in case you want to explicitly force the read mode.
#       -v or -verbose
#           print out old/new fields as they are replaced
#   === fields you can overwrite ====
# 		-r or -rtst_cod x 
#			retest code, argument is a character
#		-l or -lot_id xxxxx
#			lot id
#		-s or -sblot_id xxxxx
#			sublot id 
#		-p or -part_typ xxxxxx
#			part type 
#		-d or -dib_id xxxxxxx
#			device interface board (dib) id
#		-c or -card_id xxxxx
#			probe card id
#
#
# Copyright (C) 2015 David Gattrell
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
#  David.Gattrell
#  @
#  gmail.com
# 
#--------------------------------------------------------------------


use strict;

use constant TRUE	=> 1;
use constant FALSE	=> 0;



#--------------------------
# initialize variables
#--------------------------
my $generate_stdf = FALSE;
my $change_MIR = FALSE;
my $change_SDR = FALSE;
my $big_endian = FALSE;
my $force_endian = FALSE;
my $do_lot_id = FALSE;
my $do_sblot_id = FALSE;
my $do_part_typ = FALSE;
my $do_rtst_cod = FALSE;
my $do_dib_id = FALSE;
my $do_card_id = FALSE;
my $verbose = FALSE;

my $FAR_found = FALSE;
my $ATR_written = FALSE;
my $work_to_do = FALSE;	# true if we haven't updated MIR or SDR records yet	
my $creating_stdf = FALSE;	# true if we are generating a changed STDF file this run

my $in_name = "";
my $out_name = "new.stdf";
my $new_lot_id = "";
my $new_sblot_id = "";
my $new_part_typ = "";
my $new_rtst_cod = "";
my $new_dib_id = "";
my $new_card_id = "";

my $debug0 = FALSE;		# dump command line parsing info
my $debug1 = FALSE; 	# dump input STDF record headers
my $debug2 = FALSE;		# print out ACR record text field


#---------------------------------
# process the command line options
#---------------------------------
my $my_command_line = join(' ',"stdf_change_lotid.pl",@ARGV);	# for ATR record

while (scalar @ARGV > 0) {
	my $tmp = shift @ARGV;

	if (($tmp eq '-i') || ($tmp eq '-in')) {
		# next argument should be input filename
		$in_name = shift @ARGV;
	}
	elsif (($tmp eq '-o') || ($tmp eq '-out')) {
		# next argument should be output filename
		$out_name = shift @ARGV;
	}
	elsif (($tmp eq '-v') || ($tmp eq '-verbose')) {
		$verbose = TRUE;
	}
	elsif (($tmp eq '-e') || ($tmp eq '-endian')) {
		# next argument should be "little" or "big", l/b should also work
		$tmp = shift @ARGV;
		if (substr($tmp,0,1) eq 'l') {
			$force_endian = TRUE;
			$big_endian = FALSE;
		}
		elsif (substr($tmp,0,1) eq 'b') {
			$force_endian = TRUE;
			$big_endian = TRUE;
		}
	}
	elsif (($tmp eq '-l') || ($tmp eq '-lot_id')) {
		# next argument should be new lotid
		$new_lot_id = shift @ARGV;
		$do_lot_id = TRUE;
		$change_MIR = TRUE;
		$generate_stdf = TRUE;
	}
	elsif (($tmp eq '-s') || ($tmp eq '-sblot_id')) {
		# next argument should be new lotid
		$new_sblot_id = shift @ARGV;
		$do_sblot_id = TRUE;
		$change_MIR = TRUE;
		$generate_stdf = TRUE;
	}
	elsif (($tmp eq '-p') || ($tmp eq '-part_typ')) {
		# next argument should be new lotid
		$new_part_typ = shift @ARGV;
		$do_part_typ = TRUE;
		$change_MIR = TRUE;
		$generate_stdf = TRUE;
	}
	elsif (($tmp eq '-r') || ($tmp eq '-rtst_cod')) {
		# next argument should be new lotid
		$new_rtst_cod = shift @ARGV;
		$do_rtst_cod = TRUE;
		$change_MIR = TRUE;
		$generate_stdf = TRUE;
	}
	elsif (($tmp eq '-d') || ($tmp eq '-dib_id')) {
		# next argument should be new dib id
		$new_dib_id = shift @ARGV;
		$do_dib_id = TRUE;
		$change_SDR = TRUE;
		$generate_stdf = TRUE;
	}
	elsif (($tmp eq '-c') || ($tmp eq '-card_id')) {
		# next argument should be new probe card id
		$new_card_id = shift @ARGV;
		$do_card_id = TRUE;
		$change_SDR = TRUE;
		$generate_stdf = TRUE;
	}
	elsif (($tmp eq '-h') || ($tmp eq '-help')) {
		# usage...
		print_usage();
		exit;
	}
	elsif ($tmp =~ m/^\-/) {
		print "Unrecognized input: $tmp \n";
		print_usage();
		exit;
	}
	else {
		# -i and -o are optional, so if we are here, this is
		# either the in or out filename ...
		if (length($in_name)<1) {
			$in_name = $tmp;
		} 
		else {
			$out_name = $tmp;
		}
	}
}
if ($debug0) {
	##### REVISIT
}


#------------------------------------
# now parse the input file
#------------------------------------
if (! open(IN_STDF,'<'.$in_name)) {
	die("Could not open file $in_name");
}
binmode(IN_STDF);

if ($generate_stdf) {
	if (! open(OUT_STDF,'>'.$out_name)) {
		die("Could not create file $out_name");
	}
	binmode(OUT_STDF);
}


my $header;
my $record;
while (read(IN_STDF,$header,4)==4) {

	my $rec_len = get_U2($header,0,$big_endian);
	my $rec_typ = get_U1($header,2);
	my $rec_sub = get_U1($header,3);

	# FAR record ...
	# should be first record, but not always with Teradyne Flex files!
	# these are little endian, so make that the initial guess
	#
	# size should always be 2, so if size is 512, we should
	# change our endian-ness
	if( ($rec_typ == 0) && ($rec_sub == 10) ) {
		$FAR_found = TRUE;
		if ( ($rec_len == 512) && !$force_endian ) {
			$rec_len = 2;
			$big_endian = !$big_endian;
		}
	}

	if ($debug1) {
		print ("rec_typ $rec_typ  rec_sum $rec_sub  length $rec_len\n");
	}

	# suck in the rest of the record into $record
	#--------------------------------------------
	if( read(IN_STDF,$record,$rec_len)!=$rec_len ) {
		if ($generate_stdf) { close(OUT_STDF); }
		close(IN_STDF);
		die("Stdf file corrupted or truncated: $in_name");
	}


	# decide if we want to over-write this record
	#---------------------------------------------
	if ($change_MIR && ($rec_typ == 1) && ($rec_sub == 10) ) {
		# ok, we want to update this MIR record
		#--------------------------------------
		my $orig_record = $record;	# just for debugging
		my $field;					# the field we may modify
		my $field_len;				# if the field is Cn, this is the n portion

		# take first 10 bytes from record:
		#   U4  setup_t
		#   U4  start_t
		#   U1  stat_num
		#   C1  mode_cod
		my $new_record = substr($record,0,10,"");

		# copy or replace rtst_cod
		$field = substr($record,0,1,"");
		if ($do_rtst_cod) {
			$new_record .= pack_C1($new_rtst_cod);
			if ($verbose) {
				print "changing rtst_cod from >$field< to >$new_rtst_cod< \n";
			}
		} 
		else {
			$new_record .= $field;
		}

		# take next 4 bytes from record:
		#   C1  prot_cod
		#   U2  burn_tim
		#   C1  cmod_cod
		$new_record .= substr($record,0,4,"");

		# copy or replace lot_id
		$field_len = unpack('C',substr($record,0,1,""));	# get the n of Cn field...
		$field = "";
		if($field_len > 0) {
			$field = substr($record,0,$field_len,"");
		}
		if ($do_lot_id) {
			$new_record .= pack_Cn($new_lot_id);
			if ($verbose) {
				print "changing lot_id from >$field< to >$new_lot_id< \n";
			}
		} 
		else {
			$new_record .= pack_Cn($field);
		}

		# copy or replace part_typ
		$field_len = unpack('C',substr($record,0,1,""));	# get the n of Cn field...
		$field = "";
		if($field_len > 0) {
			$field = substr($record,0,$field_len,"");
		}
		if ($do_part_typ) {
			$new_record .= pack_Cn($new_part_typ);
			if ($verbose) {
				print "changing part_typ from >$field< to >$new_part_typ< \n";
			}
		} 
		else {
			$new_record .= pack_Cn($field);
		}

		# take next bytes from record:
		#   Cn  node_nam
		#   Cn  tstr_typ
		#   Cn  job_nam
		#   Cn  job_rev
		for (my $i=0; $i<4; $i++) {
			$field_len = unpack('C',substr($record,0,1,""));	# get the n of Cn field...
			$field = "";
			if($field_len > 0) {
				$field = substr($record,0,$field_len,"");
			}
			$new_record .= pack_Cn($field);
		}

		# copy or replace sblot_id
		$field_len = unpack('C',substr($record,0,1,""));	# get the n of Cn field...
		$field = "";
		if($field_len > 0) {
			$field = substr($record,0,$field_len,"");
		}
		if ($do_sblot_id) {
			$new_record .= pack_Cn($new_sblot_id);
			if ($verbose) {
				print "changing sblot_id from >$field< to >$new_sblot_id< \n";
			}
		} 
		else {
			$new_record .= pack_Cn($field);
		}
		
		# remaining fields are ones we don't plan on modifying:
		#   Cn  oper_nam
		#   Cn  exec_typ
		#   Cn  exec_ver
		#   Cn  test_cod
		#   Cn  tst_temp
		#  ... etc.

		# done all the key fields, just append the remaining part of the record verbatim
		$new_record .= $record;

		# now overwrite the original record with the new record...
		$record = $new_record;

		$change_MIR = FALSE;
	}

	if ($change_SDR && ($rec_typ == 1) && ($rec_sub == 80) ) {
		# ok, we want to update this SDR record
		#--------------------------------------
		my $orig_record = $record;	# just for debugging
		my $field;					# the field we may modify
		my $field_len;				# if the field is Cn, this is the n portion

		# take first 2 bytes from record:
		#   U1  head_num
		#   U1  site_grp
		my $new_record = substr($record,0,2,"");

		#   U1  site_cnt  (k)
		#   k*U1  site_num
		my $k = unpack('C',substr($record,0,1,""));
		$new_record .= pack_U1($k) . substr($record,0,$k,"");

		# take next bytes from record:
		#   Cn  hand_typ
		#   Cn  hand_id
		#   Cn  card_typ
		for (my $i=0; $i<3; $i++) {
			if (length($record)<1) { $record = pack('x'); }	# pad out record if it is short
			$field_len = unpack('C',substr($record,0,1,""));	# get the n of Cn field...
			$field = "";
			if($field_len > 0) {
				$field = substr($record,0,$field_len,"");
			}
			$new_record .= pack_Cn($field);
		}
		
		# copy or replace card_id
		if (length($record)<1) { $record = pack('x'); }	# pad out record if it is short
		$field_len = unpack('C',substr($record,0,1,""));	# get the n of Cn field...
		$field = "";
		if($field_len > 0) {
			$field = substr($record,0,$field_len,"");
		}
		if ($do_card_id) {
			$new_record .= pack_Cn($new_card_id);
			if ($verbose) {
				print "changing card_id from >$field< to >$new_card_id< \n";
			}
		} 
		else {
			$new_record .= pack_Cn($field);
		}

		# take next bytes from record:
		#   Cn  load_typ
		#   Cn  load_id
		#   Cn  dib_typ
		for (my $i=0; $i<3; $i++) {
			if (length($record)<1) { $record = pack('x'); }	# pad out record if it is short
			$field_len = unpack('C',substr($record,0,1,""));	# get the n of Cn field...
			$field = "";
			if($field_len > 0) {
				$field = substr($record,0,$field_len,"");
			}
			$new_record .= pack_Cn($field);
		}

		# copy or replace dib_id
		if (length($record)<1) { $record = pack('x'); }	# pad out record if it is short
		$field_len = unpack('C',substr($record,0,1,""));	# get the n of Cn field...
		$field = "";
		if($field_len > 0) {
			$field = substr($record,0,$field_len,"");
		}
		if ($do_dib_id) {
			$new_record .= pack_Cn($new_dib_id);
			if ($verbose) {
				print "changing dib_id from >$field< to >$new_dib_id< \n";
			}
		} 
		else {
			$new_record .= pack_Cn($field);
		}

		# done all the key fields, just append the remaining part of the record verbatim
		$new_record .= $record;

		# now overwrite the original record with the new record...
		$record = $new_record;

		$change_SDR = FALSE;
	}


	if( $generate_stdf ) {
		# dump record to new stdf file 
		
		print OUT_STDF 
				pack_U2(length($record),$big_endian).
				pack_U1($rec_typ).
				pack_U1($rec_sub).
				$record
			;
	}


	# do we need to insert out ATR record now?
	#---------------------------------------------
	if( $FAR_found && ! $ATR_written && $generate_stdf ) {
		my $atr_rec_typ = 0;
		my $atr_rec_sub = 20;
		my $atr_mod_tim = time();
		my $atr_record;
		my $atr_rec_len;

		$atr_record = pack_U4($atr_mod_tim,$big_endian).
					  pack_Cn($my_command_line);

		if ($debug2) {
			print ("my_command_line .. $my_command_line\n");
			print ("atr_record .. $atr_record\n");
		}

		print OUT_STDF
				pack_U2(length($atr_record),$big_endian).
				pack_U1($atr_rec_typ).		
				pack_U1($atr_rec_sub).		
				$atr_record
			;

		$ATR_written = TRUE;
	}

	# if we've done all our changes, no need to do further STDF parsing,
	# just append remaining IN_STDF to OUT_STDF... should be quicker.

	#### REVISIT
}


if ($generate_stdf) { close(OUT_STDF); }
close(IN_STDF);




#=============================================================
#=============================================================
#  subroutines
#=============================================================

sub print_usage
{
	print "stdf_change_lotid.pl -i in_stdf -o out_stdf -l new_lot_id \n";
	print "  -i|-in in_stdf -- in_stdf is the name of the stdf file to change \n";
	print "  -o|-out out_stdf -- out_stdf is the name to use when creating the modified file \n";
	print "  -e|-endian b|l|big|little -- optional, if the script doesn't correctly \n";
	print "                               automatically detecti the endian-ness, this \n";
	print "                               might work \n";
	print "  -v|-verbose -- print out original and new field values when replacing them \n";
	print " MIR and SDR fields that can be overwritten: \n";
	print "  -l|-lot_id new_lot_id -- will replace the lot_id field of the MIR record \n";
	print "  -s|-sblot_id new_sblot_id -- will replace the sblot_id field of the MIR record \n";
	print "  -r|-rtst_cod x -- will replace the rtst_cod field of the MIR record \n";
	print "  -p|-part_typ new_part_typ -- will replace the part_typ field of the MIR record \n";
	print "  -d|-dib_id new_dib_id -- will replace the dib_id field of the SDR record \n";
	print "  -c|-card_id new_probe_card_id -- will replace the card_id field of the SDR record \n";
	print "\nexamples: \n";
	print " stdf_change_lotid.pl a595.stdf new_a595.stdf -l j97543.1 -r R \n";
	print " stdf_change_lotid.pl -in a595.stdf -out new_a595.stdf -sblot_id wafer_07 \n";
}

#--------------------------------------------

sub get_U2
{
	my $header = shift();
	my $index = shift();
	my $big_endian = shift();

	my $my_template;

	if ($big_endian) {
		$my_template = 'n';
	} else {
		$my_template = 'v';
	}
	my $value = unpack($my_template, substr($header,$index,2) );

	return( $value );
}

#--------------------------------------------

sub get_U1
{
	my $header = shift();
	my $index = shift();
	my $value = unpack('C', substr($header,$index,1) );
	
	return( $value );
}

#---------------------------------------------

sub pack_U1
{
	my $my_U1 = shift();

	return( pack('C',$my_U1) );
}

#---------------------------------------------

sub pack_U2
{
	my $my_U2 = shift();
	my $big_endian = shift();

	my $my_packed_U2;

	if ($big_endian) {
		$my_packed_U2 = pack('n',$my_U2);
	}
	else {
		$my_packed_U2 = pack('v',$my_U2);
	}

	return( $my_packed_U2 );
}

#---------------------------------------------

sub pack_U4
{
	my $my_U4 = shift();
	my $big_endian = shift();

	my $my_packed_U4;

	if ($big_endian) {
		$my_packed_U4 = pack('N',$my_U4);
	}
	else {
		$my_packed_U4 = pack('V',$my_U4);
	}

	return( $my_packed_U4 );
}

#---------------------------------------------

sub pack_C1
{
	my $my_C1 = shift();

	return( pack('a1',$my_C1) );
}

#---------------------------------------------

sub pack_Cn
{
	my $my_Cn = shift();

	my $length = length($my_Cn);
	if ($length>255) {
		$length = 255;
		print("WARNING: truncating char string to 255 chars\n");
	}

	my $my_packed_Cn;
	if ($length<1) {
		$my_packed_Cn = pack('x');	# A null byte
	}
	else {
		$my_packed_Cn = 
				pack('C',$length).
				pack("a$length",$my_Cn);
	}

	return( $my_packed_Cn );
}

#---------------------------------------------
