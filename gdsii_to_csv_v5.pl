#!/usr/bin/perl -w
#
# gdsii_to_csv.pl example.gds >pad_locations.csv
# gdsii_to_csv.pl -a gdsii.ascii example.gds >pad_locations.csv
#
# $Id: gdsii_to_csv_v5.pl,v 1.1 2022/02/13 15:52:08 david Exp $
# 
# parses gdsii file and generates csv file of the
# pad opening centers, used for probe card design.
#
# details:
#  "-a" option dumps a human readable ascii version of the gdsii file
#  looks for BOUNDARY shapes = pad openings
#  looks for TEXT at the same location or nearby to map as pad name (within 60um)
#
# started May 14, 2008
#
#  revisit, Jan/2016
#    a different design group/tools, so script needs to react differently
#  revisit, Aug/2019
#    - add layer and pad opening size to csv dump
#      so you can screen out bogus layers and so you can decide if you want
#      to add additional needles to oversized pad locations
#  revisit, Feb/2022
#    - added support for AREF (arrays of references)
#    - re-jigged to handle nested hierarchy
#    - track layers for both pad and padname in csv dump
#
# Copyright (C) 2008,2016,2019,2022 David Gattrell
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
# *******************************************************

$debug1 = 0;	# print each record type and length
$debug2 = 0;	# dump STRNAMES and reference counts to each structure
$debug4 = 0;	# dump structure pinnames arrays

$do_undef_sname_dump = 0;
$do_ascii_dump = 0;
$do_csv_dump = 1;
$layer = "na";

@indents = ('','    ','        ','            ',
		'                ');


@all_strnames = ();
@undef_snames = ();	 # if we get sname, with no previous strname definition

# store TEXT elements in an array with associated arrays of x,y, layer, strname
@str_pinnames = ();
@str_pinname_xs = ();
@str_pinname_ys = ();
@str_pinname_layers = ();
@str_pinname_strnames = ();
%idx_name_start = ();	# {sname} .. faster?, just search section for this sname
%idx_name_stop = ();	# {sname} .. faster?, just search section for this sname

# store BOUNDARY elements in an array with associated arrays of x,y, 
@pad_xs = ();
@pad_ys = ();
@pad_min_xs = ();
@pad_max_xs = ();
@pad_min_ys = ();
@pad_max_ys = ();
@pad_layers = ();
@pad_strnames = ();
%idx_pad_start = ();	# {sname} .. faster?, just search section for this sname
%idx_pad_stop = ();		# {sname} .. faster?, just search section for this sname


# process the command line
#***************************
while (@ARGV) {
$arg = shift @ARGV;
	if ($arg =~ /^-a/) {
		$arg = shift @ARGV;
		$do_ascii_dump = 1;
		$ascii_file = $arg;
	} else {
		$gdsii_filename = $arg;
	}
}

# open the GDSII file for parsing
#*********************************
open(GDSII,"<$gdsii_filename") || die "Couldn't read $gdsii_filename";
binmode(GDSII);		# in case your running in microsoft windows

#open the ascii file for writing
#********************************
if ($do_ascii_dump) {
	open(ASCII,">$ascii_file") || die "Couldn't write $ascii_file";
}

# the first record should have a length of <256
# if it is larger then we need to swap the endian of the reading
#**************************************************************** 
$bytes = read(GDSII,$raw,2);
$record_length = unpack('s',$raw);
if ($record_length>256) {
	$other_endian = 1;
	$raw = join('', reverse(split(//, $raw)));
	$record_length = unpack('v',$raw);
} else {
	$other_endian = 0;
}


# now parse the various records in the file
#*******************************************
$indenting = 0;
while (($bytes>0) && ($record_length>0)) {
	read(GDSII,$raw,1);
	$record_type = unpack('C',$raw);
	read(GDSII,$data_type,1);
	$bytes_left = $record_length - 4;

if ($record_type==0x00) {
	$rec_name = "HEADER";
	$gdsii_version = &read_I2(*GDSII,*bytes_left,$other_endian);
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name version $gdsii_version \n");
	}
} elsif ($record_type==0x01) {
	$rec_name = "BGNLIB";

	$start_date = &read_date(*GDSII,*bytes_left,$other_endian);
	$edit_date = &read_date(*GDSII,*bytes_left,$other_endian);
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name started $start_date  last edited $edit_date \n");
	}

} elsif ($record_type==0x02) {
	$rec_name = "LIBNAME";
	$libname = &read_Cn(*GDSII,$bytes_left,*bytes_left,$other_endian);
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name $libname \n");
	}
} elsif ($record_type==0x03) {
	$rec_name = "UNITS";
	$user_units = &read_R8(*GDSII,*bytes_left,$other_endian);
	$meters = &read_R8(*GDSII,*bytes_left,$other_endian);
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name $user_units $meters \n");
	}
} elsif ($record_type==0x04) {
	$rec_name = "ENDLIB";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x05) {
	$rec_name = "BGNSTR";

	$start_date = &read_date(*GDSII,*bytes_left,$other_endian);
	$edit_date = &read_date(*GDSII,*bytes_left,$other_endian);
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name started $start_date  last edited $edit_date \n");
	}
	$indenting++;

} elsif ($record_type==0x06) {
	$rec_name = "STRNAME";
	$strname = &read_Cn(*GDSII,$bytes_left,*bytes_left,$other_endian);

	# sanity check, do we have duplicate strnames happening?
	$sane = 1;
	for ($i=0; $i<= $#all_strnames; $i++) {
		if($strname eq $all_strnames[$i]) {
			$sane = 0;
			print("ERROR: duplicate STRNAME encountered: $strname\n");
		}
	}
	if($sane) {
		push(@all_strnames,$strname);
	}
	$refs{$strname}=0;
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name $strname \n");
	}
} elsif ($record_type==0x07) {
	$rec_name = "ENDSTR";
	$indenting--;
	$strname = "";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x08) {
	$rec_name = "BOUNDARY";
	$element = $rec_name;
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
	$indenting++;
} elsif ($record_type==0x09) {
	$rec_name = "PATH";
	$element = $rec_name;
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
	$indenting++;
} elsif ($record_type==0x0a) {
	$rec_name = "SREF";
	$element = $rec_name;
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
	$indenting++;
} elsif ($record_type==0x0b) {
	$rec_name = "AREF";
	$element = $rec_name;
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
	$indenting++;
} elsif ($record_type==0x0c) {
	$rec_name = "TEXT";
	$element = $rec_name;
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
	$indenting++;
} elsif ($record_type==0x0d) {
	$rec_name = "LAYER";
	$layer = &read_I2(*GDSII,*bytes_left,$other_endian);
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name $layer \n");
	}
} elsif ($record_type==0x0e) {
	$rec_name = "DATATYPE";
	$datatype = &read_I2(*GDSII,*bytes_left,$other_endian);
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name $datatype \n");
	}
} elsif ($record_type==0x0f) {
	$rec_name = "WIDTH";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x10) {
	$rec_name = "XY";
	$xy = "";
	@xs = ();
	@ys = ();
	while ($bytes_left>7) {
		$x = &read_I4(*GDSII,*bytes_left,$other_endian);
		$y = &read_I4(*GDSII,*bytes_left,$other_endian);
		$xy = $xy . "(".$x.",".$y.") ";
		push(@xs,$x);
		push(@ys,$y);
	}
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name $xy\n");
	}
	if ($element eq "BOUNDARY") {
		# if we are in a boundary element, then get 
		# center of the pad opening and remember the
	 	# structure name, and layer 

		@sorted_xs = sort numerically @xs;
		@sorted_ys = sort numerically @ys;
		$new_mid_x = ($sorted_xs[0] + $sorted_xs[$#sorted_xs])/2.0;
		$new_mid_y = ($sorted_ys[0] + $sorted_ys[$#sorted_ys])/2.0;
		$new_min_x = $sorted_xs[0];
		$new_max_x = $sorted_xs[$#sorted_xs];
		$new_min_y = $sorted_ys[0];
		$new_max_y = $sorted_ys[$#sorted_ys];

		if(!defined($idx_pad_start{$strname})) {
			$idx_pad_start{$strname} = 1 + $#pad_xs;
		}
		push(@pad_xs,$new_mid_x);
		push(@pad_ys,$new_mid_y);
		push(@pad_min_xs,$new_min_x);
		push(@pad_min_ys,$new_min_y);
		push(@pad_max_xs,$new_max_x);
		push(@pad_max_ys,$new_max_y);
		push(@pad_layers,$layer);
		push(@pad_strnames,$strname);
		$idx_pad_stop{$strname} = $#pad_xs;

	} elsif ($element eq "SREF") {
		# if sname is in all_strnames, then we can process this...
		$found_it = 0;
		for ($i=0; ($i<= $#all_strnames) and ($found_it<1); $i++) {
			if ($sname eq $all_strnames[$i]) {
				$found_it = 1;
			}
		}
		if ($found_it) {
			$refs{$sname}++;	# this indicates the structure is not a top level structure
					

			# translate shape xy coordinates (ideally pad openings)
			# -----------------------------------------------------
			if(defined($idx_pad_start{$sname})) {
				# speed things up by only looking at the correct section of array
				$i_start = $idx_pad_start{$sname};
				$i_stop = $idx_pad_stop{$sname};
			} else {
				$i_start = 0;
				$i_stop = $#pad_strnames;
			}
			#for ($i=0; $i<=$#pad_strnames; $i++) {
			for ($i=$i_start; $i<=$i_stop; $i++) {
				if($sname eq $pad_strnames[$i]) {
					# yes!  There are some pad openings we need to translate here!
					$sx = $pad_xs[$i];
					$sy = $pad_ys[$i];
					$min_sx = $pad_min_xs[$i];
					$min_sy = $pad_min_ys[$i];
					$max_sx = $pad_max_xs[$i];
					$max_sy = $pad_max_ys[$i];

					if ($refl>0) { # reflect on x axis...
						$sy = $sy * -1;
						$tmp = $min_sy;
						$min_sy = $max_sy * -1; 
						$max_sy = $tmp * -1; 
					} 
					# expect angle of 0, 90, 180, or 270.
					# no translation needed if angle is 0
					if ($angle>225.0) { # aka 270
						$tmp = $sx;
						$sx = $sy;
						$sy = -1 * $tmp;
						$tmp = $min_sx;
						$min_sx = $min_sy;
						$min_sy = -1 * $max_sx;
						$max_sx = $max_sy;
						$max_sy = -1 * $tmp;
					}elsif($angle>135.0){ # aka 180
						$sx = -1 * $sx;
						$sy = -1 * $sy;
						$tmp = $min_sx;
						$min_sx = -1 * $max_sx;
						$max_sx = -1 * $tmp;
						$tmp = $min_sy;
						$min_sy = -1 * $max_sy;
						$max_sy = -1 * $tmp;
					}elsif($angle>45.0){ # aka 90
						$tmp = $sx;
						$sx = -1 * $sy;
						$sy = $tmp;
						$tmp = $min_sx;
						$min_sx = -1 * $max_sy;
						$max_sy = $max_sx;
						$max_sx = -1 * $min_sy;
						$min_sy = $tmp;
					}
					$sx = $sx + $x;
					$sy = $sy + $y;
					$min_sx = $min_sx + $x;
					$max_sx = $max_sx + $x;
					$min_sy = $min_sy + $y;
					$max_sy = $max_sy + $y;

					# now add the translated pad to this structure level
					if(!defined($idx_pad_start{$strname})) {
						$idx_pad_start{$strname} = 1 + $#pad_xs;
					}
					push(@pad_xs,$sx);
					push(@pad_ys,$sy);
					push(@pad_min_xs,$min_sx);
					push(@pad_min_ys,$min_sy);
					push(@pad_max_xs,$max_sx);
					push(@pad_max_ys,$max_sy);
					push(@pad_layers,$pad_layers[$i]);
					push(@pad_strnames,$strname);
					$idx_pad_stop{$strname} = $#pad_xs;
				}
			}


			# padnames that are stored in STRUCTURE that need to be translated
			#-----------------------------------------------------------------
			if(defined($idx_name_start{$sname})) {
				# speed things up by only looking at the correct section of array
				$i_start = $idx_name_start{$sname};
				$i_stop = $idx_name_stop{$sname};
			} else {
				$i_start = 0;
				$i_stop = $#str_pinname_strnames;
			}
			#for ($i=0; $i<=$#str_pinname_strnames; $i++) {
			for ($i=$i_start; $i<=$i_stop; $i++) {
				if($sname eq $str_pinname_strnames[$i]) {
					# yes!  There are some pad names we need to translate here!
					$sx = $str_pinname_xs[$i];
					$sy = $str_pinname_ys[$i];
					if ($refl>0) { # reflect on x axis...
						$sy = $sy * -1;
					} 
					# expect angle of 0, 90, 180, or 270.
					# no translation needed if angle is 0
					if ($angle>225.0) { # aka 270
						$tmp = $sx;
						$sx = $sy;
						$sy = -1 * $tmp;
					}elsif($angle>135.0){ # aka 180
						$sx = -1 * $sx;
						$sy = -1 * $sy;
					}elsif($angle>45.0){ # aka 90
						$tmp = $sx;
						$sx = -1 * $sy;
						$sy = $tmp;
					}
					$sx = $sx + $x;
					$sy = $sy + $y;

					# now add the translated pinname to this structure level
					if(!defined($idx_name_start{$strname})) {
						$idx_name_start{$strname} = 1 + $#str_pinname_xs;
					}
					push(@str_pinnames,$str_pinnames[$i]);
					push(@str_pinname_xs,$sx);
					push(@str_pinname_ys,$sy);
					push(@str_pinname_layers,$str_pinname_layers[$i]);
					push(@str_pinname_strnames,$strname);
					$idx_name_stop{$strname} = $#str_pinname_xs;
				}
			}
		}
	} elsif ($element eq "AREF") { 
		# if sname is in all_strnames, then we can process this...
		$found_it = 0;
		for ($i=0; ($i<= $#all_strnames) and ($found_it<1); $i++) {
			if ($sname eq $all_strnames[$i]) {
				$found_it = 1;
			}
		}
		if ($found_it) {
			$refs{$sname}++;	# this indicates the structure is not a top level structure

			# now need to loop through array ref coordinates
			# $columns $rows
			# # XY1 is bottom left, XY2 is bottom right + deltaX, XY3 is top left + deltaY
			$base_x = $xs[0];
			$base_y = $ys[0];
			$delta_x = ($xs[1] - $xs[0])/$columns;
			$delta_y = ($ys[2] - $ys[0])/$rows;

			for ($col=0; $col<$columns; $col++) {
				for ($row=0; $row<$rows; $row++) {
					$x = $base_x + ($col * $delta_x);
					$y = $base_y + ($row * $delta_y);
					
					# translate shape xy coordinates (ideally pad openings)
					# -----------------------------------------------------
					if(defined($idx_pad_start{$sname})) {
						# speed things up by only looking at the correct section of array
						$i_start = $idx_pad_start{$sname};
						$i_stop = $idx_pad_stop{$sname};
					} else {
						$i_start = 0;
						$i_stop = $#pad_strnames;
					}
					for ($i=$i_start; $i<=$i_stop; $i++) {
					#for ($i=0; $i<=$#pad_strnames; $i++) {
						if($sname eq $pad_strnames[$i]) {
							# yes!  There are some pad openings we need to translate here!
							$sx = $pad_xs[$i];
							$sy = $pad_ys[$i];
							$min_sx = $pad_min_xs[$i];
							$min_sy = $pad_min_ys[$i];
							$max_sx = $pad_max_xs[$i];
							$max_sy = $pad_max_ys[$i];

							if ($refl>0) { # reflect on x axis...
								$sy = $sy * -1;
								$tmp = $min_sy;
								$min_sy = $max_sy * -1; 
								$max_sy = $tmp * -1; 
							} 
							# expect angle of 0, 90, 180, or 270.
							# no translation needed if angle is 0
							if ($angle>225.0) { # aka 270
								$tmp = $sx;
								$sx = $sy;
								$sy = -1 * $tmp;
								$tmp = $min_sx;
								$min_sx = $min_sy;
								$min_sy = -1 * $max_sx;
								$max_sx = $max_sy;
								$max_sy = -1 * $tmp;
							}elsif($angle>135.0){ # aka 180
								$sx = -1 * $sx;
								$sy = -1 * $sy;
								$tmp = $min_sx;
								$min_sx = -1 * $max_sx;
								$max_sx = -1 * $tmp;
								$tmp = $min_sy;
								$min_sy = -1 * $max_sy;
								$max_sy = -1 * $tmp;
							}elsif($angle>45.0){ # aka 90
								$tmp = $sx;
								$sx = -1 * $sy;
								$sy = $tmp;
								$tmp = $min_sx;
								$min_sx = -1 * $max_sy;
								$max_sy = $max_sx;
								$max_sx = -1 * $min_sy;
								$min_sy = $tmp;
							}
							$sx = $sx + $x;
							$sy = $sy + $y;
							$min_sx = $min_sx + $x;
							$max_sx = $max_sx + $x;
							$min_sy = $min_sy + $y;
							$max_sy = $max_sy + $y;

							# now add the translated pad to this structure level
							if(!defined($idx_pad_start{$strname})) {
								$idx_pad_start{$strname} = 1 + $#pad_xs;
							}
							push(@pad_xs,$sx);
							push(@pad_ys,$sy);
							push(@pad_min_xs,$min_sx);
							push(@pad_min_ys,$min_sy);
							push(@pad_max_xs,$max_sx);
							push(@pad_max_ys,$max_sy);
							push(@pad_layers,$pad_layers[$i]);
							push(@pad_strnames,$strname);
							$idx_pad_stop{$strname} = $#pad_xs;
						}
					}


					# padnames that are stored in STRUCTURE that need to be translated
					#-----------------------------------------------------------------
					if(defined($idx_name_start{$sname})) {
						# speed things up by only looking at the correct section of array
						$i_start = $idx_name_start{$sname};
						$i_stop = $idx_name_stop{$sname};
					} else {
						$i_start = 0;
						$i_stop = $#str_pinname_strnames;
					}
					#for ($i=0; $i<=$#str_pinname_strnames; $i++) {
					for ($i=$i_start; $i<=$i_stop; $i++) {
						if($sname eq $str_pinname_strnames[$i]) {
							# yes!  There are some pad names we need to translate here!
							$sx = $str_pinname_xs[$i];
							$sy = $str_pinname_ys[$i];
							if ($refl>0) { # reflect on x axis...
								$sy = $sy * -1;
							} 
							# expect angle of 0, 90, 180, or 270.
							# no translation needed if angle is 0
							if ($angle>225.0) { # aka 270
								$tmp = $sx;
								$sx = $sy;
								$sy = -1 * $tmp;
							}elsif($angle>135.0){ # aka 180
								$sx = -1 * $sx;
								$sy = -1 * $sy;
							}elsif($angle>45.0){ # aka 90
								$tmp = $sx;
								$sx = -1 * $sy;
								$sy = $tmp;
							}
							$sx = $sx + $x;
							$sy = $sy + $y;

							# now add the translated pinname to this structure level
							if(!defined($idx_name_start{$strname})) {
								$idx_name_start{$strname} = 1 + $#str_pinname_xs;
							}
							push(@str_pinnames,$str_pinnames[$i]);
							push(@str_pinname_xs,$sx);
							push(@str_pinname_ys,$sy);
							push(@str_pinname_layers,$str_pinname_layers[$i]);
							push(@str_pinname_strnames,$strname);
							$idx_name_stop{$strname} = $#str_pinname_xs;
						}
					}
				}
			}

		} else {
			push(@undef_snames,$sname); # something is wrong!
		}

	}
} elsif ($record_type==0x11) {
	$rec_name = "ENDEL";
	$indenting--;
	$element = "";
	$angle = 0.0;   #reset default values for next element
	$refl = 0;		#reset default values for next element
	$absmag = 0;    #reset default values for next element
	$absangle = 0;  #reset default values for next element
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x12) {
	$rec_name = "SNAME";
	$sname = &read_Cn(*GDSII,$bytes_left,*bytes_left,$other_endian);
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name $sname \n");
	}
} elsif ($record_type==0x13) {
	$rec_name = "COLROW";
	$columns = &read_I2(*GDSII,*bytes_left,$other_endian);
	$rows = &read_I2(*GDSII,*bytes_left,$other_endian);
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name $columns $rows \n");
	}
} elsif ($record_type==0x14) {
	$rec_name = "TEXTNODE";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x15) {
	$rec_name = "NODE";
	$element = $rec_name;
	if ($do_ascii_dump) {
		print(ASCII "$rec_name \n");
	}
	$indenting++;
} elsif ($record_type==0x16) {
	$rec_name = "TEXTTYPE";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x17) {
	$rec_name = "PRESENTATION";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x18) {
	$rec_name = "SPACING";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x19) {
	$rec_name = "STRING";
	$text_name = &read_Cn(*GDSII,$bytes_left,*bytes_left,$other_endian);
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name $text_name\n");
	}
	if ($element eq "TEXT") {
		# add this potential pinname to the pinname list along with x,y, layer and structure
		if(!defined($idx_name_start{$strname})) {
			$idx_name_start{$strname} = 1 + $#str_pinname_xs;
		}
		push(@str_pinnames, $text_name);
		push(@str_pinname_xs, $x);
		push(@str_pinname_ys, $y);
		push(@str_pinname_layers, $layer);
		push(@str_pinname_strnames, $strname);
		$idx_name_stop{$strname} = $#str_pinname_xs;
	}
} elsif ($record_type==0x1a) {
	$rec_name = "STRANS";
	($refl,$absmag,$absangle) = &read_translation(*GDSII,*bytes_left,$other_endian);
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name refl $refl absmag $absmag absangle $absangle \n");
	}
} elsif ($record_type==0x1b) {
	$rec_name = "MAG";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x1c) {
	$rec_name = "ANGLE";
	$angle = &read_R8(*GDSII,*bytes_left,$other_endian);
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name $angle \n");
	}
} elsif ($record_type==0x1d) {
	$rec_name = "UINTEGER";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x1e) {
	$rec_name = "USTRING";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x1f) {
	$rec_name = "REFLIBS";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x20) {
	$rec_name = "FONTS";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x21) {	
	$rec_name = "PATHTYPE";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x22) {
	$rec_name = "GENERATIONS";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x23) {
	$rec_name = "ATTRTABLE";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x24) {
	$rec_name = "STYPTABLE";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x25) {
	$rec_name = "STRTYPE";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x26) {
	$rec_name = "ELFLAGS";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x27) {
	$rec_name = "ELKEY";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x28) {
	$rec_name = "LINKTYPE";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x29) {	
	$rec_name = "LINKKEYS";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x2a) {
	$rec_name = "NODETYPE";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x2b) {
	$rec_name = "PROPATTR";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x2c) {
	$rec_name = "PROPVALUE";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x2d) {
	$rec_name = "BOX";
	$element = $rec_name;
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
	$indenting++;
} elsif ($record_type==0x2e) {
	$rec_name = "BOXTYPE";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x2f) {
	$rec_name = "PLEX";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x30) {
	$rec_name = "BGNEXTN";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x31) {	
	$rec_name = "ENDTEXTN";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x32) {
	$rec_name = "TAPENUM";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x33) {
	$rec_name = "TAPECODE";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x34) {
	$rec_name = "STRCLASS";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x35) {
	$rec_name = "RESERVED";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x36) {
	$rec_name = "FORMAT";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x37) {
	$rec_name = "MASK";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x38) {
	$rec_name = "ENDMASKS";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x39) {	
	$rec_name = "LIBDIRSIZE";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x3a) {
	$rec_name = "SRFNAME";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} elsif ($record_type==0x3b) {
	$rec_name = "LIBSECUR";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
} else {
	# OH OH...
	$rec_name = "UNKNOWN";
	if ($do_ascii_dump) {
		print(ASCII "$indents[$indenting]");
		print(ASCII "$rec_name \n");
	}
}

	if ($debug1>0) {
		print("$rec_name  length $record_length \n");
	}
	if ($bytes_left>0) {
		read(GDSII,$bit_bucket,$bytes_left);
	}


	# ok, now check to see if there is another record...
	#****************************************************
	$bytes = read(GDSII,$raw,2);
	$raw = join('', reverse(split(//, $raw))) if ($other_endian);
	$record_length = unpack('v',$raw);
}
close ASCII;


# what strnames did we trigger on?
# --------------------------------
if ($debug2) {
	@list = @all_strnames;
	for(@list) {
		print "strname: $_ ... refs $refs{$_} \n";
	}
}


# does our structure pinlist data look sane?
#-------------------------------------------
if ($debug4) {
	print "... start structure pinlist dump\n";
	for ($i=0; $i<=$#str_pinname_strnames; $i++) {
		print "$str_pinnames[$i] at $str_pinname_xs[$i] , $str_pinname_ys[$i] layer $str_pinname_layers[$i]";
		print " in structure $str_pinname_strnames[$i] \n";
	}
	print "... done structure pinlist dump\n";
	
}


# did we have any undefined snames?
#----------------------------------

if( $do_undef_sname_dump and ($#undef_snames>-1)) {
	print("WARNING: undefined snames encountered: \n");
	for ($i=0; $i<=$#undef_snames; $i++) {
		print("   $undef_snames[$i] \n");
	}
	print(" ... done ... \n");
}


# we're done parsing, now dump the csv file...
# ----------------------------------------------
if ($do_csv_dump) {
	print("PAD_LAYER , PAD_NAME_LAYER , PAD_NAME , X_CENTER , Y_CENTER , X_MIN , X_MAX, X_WIDTH , Y_MIN , Y_MAX , Y_WIDTH");
	print("\n");
	@list = @all_strnames;
	for(@list) {
		if ($refs{$_}<1) {
			# this is the top of the hierarchy, dump this...
			$sname = $_;
			# step through all the pad openings, looking for ones from this structure
			if(defined($idx_pad_start{$sname})) {
				# speed things up by only looking at the correct section of array
				$i_start = $idx_pad_start{$sname};
				$i_stop = $idx_pad_stop{$sname};
			} else {
				$i_start = 0;
				$i_stop = $#pad_strnames;
			}
			#for ($i=0; $i<=$#pad_strnames; $i++) {
			for ($i=$i_start; $i<=$i_stop; $i++) {
				if($sname eq $pad_strnames[$i]) {
					# yes!  There are some pad openings for this structure!
					
					$my_x = $pad_xs[$i];
					$my_y = $pad_ys[$i];
					# now can we find a padname for this pad opening?
					# - first search within this top level STRUCTURE .. top_match
					# - else search again, but for coordinates that are near rather than exact .. top_near
					$pin_name = $sname;	 # if can't match name, use structure nam
					$pin_layer = "na";
					$top_match = 0;
					$top_near = 0;
					if(defined($idx_name_start{$sname})) {
						# speed things up by only looking at the correct section of array
						$j_start = $idx_name_start{$sname};
						$j_stop = $idx_name_stop{$sname};
					} else {
						$j_start = 0;
						$j_stop = $#str_pinname_strnames;
					}
					#for ($j=0; ($j<=$#str_pinname_strnames) and ($top_match<1); $j++) {
					for ($j=$j_start; ($j<=$j_stop) and ($top_match<1); $j++) {
						if($_ eq $str_pinname_strnames[$j]) {
							if (($my_x == $str_pinname_xs[$j]) and ($my_y == $str_pinname_ys[$j]) ) {
								$top_match = 1;
								$pin_name = $str_pinnames[$j];
								$pin_layer = $str_pinname_layers[$j];
							}
						}
					}
					if ($top_match<1) {
						$near = 60000;  # was 10000, changed to 40000.. maybe could make optional input variable?
						if(defined($idx_name_start{$sname})) {
							# speed things up by only looking at the correct section of array
							$j_start = $idx_name_start{$sname};
							$j_stop = $idx_name_stop{$sname};
						} else {
							$j_start = 0;
							$j_stop = $#str_pinname_strnames;
						}
						#for ($j=0; ($j<=$#str_pinname_strnames) and ($top_near<1); $j++) {
						for ($j=$j_start; ($j<=$j_stop) and ($top_near<1); $j++) {
							if($_ eq $str_pinname_strnames[$j]) {
								$pin_x = $str_pinname_xs[$j];
								$pin_y = $str_pinname_ys[$j];
								if (($pin_x < ($my_x + $near)) and ($pin_x > ($my_x - $near)) and
										($pin_y < ($my_y + $near)) and ($pin_y > ($my_y - $near)) ) {
									$top_near = 1;
									$pin_name = $str_pinnames[$j];
									$pin_layer = $str_pinname_layers[$j];
								}
							}
						}
					}
					
					# ok, now dump out what we have found!
					# but first scale the coordinates...
					$my_delta_x = $pad_max_xs[$i] - $pad_min_xs[$i];
					$my_delta_y = $pad_max_ys[$i] - $pad_min_ys[$i];

					$my_x = $my_x * $meters / 1.0e-6;
					$my_y = $my_y * $meters / 1.0e-6;
					$my_min_x = $pad_min_xs[$i] * $meters / 1.0e-6;
					$my_min_y = $pad_min_ys[$i] * $meters / 1.0e-6;
					$my_max_x = $pad_max_xs[$i] * $meters / 1.0e-6;
					$my_max_y = $pad_max_ys[$i] * $meters / 1.0e-6;
					$my_delta_x = $my_max_x - $my_min_x;
					$my_delta_y = $my_max_y - $my_min_y;
					$my_delta_x = sprintf("%.4f",$my_delta_x);	# clear up some 59.999999998 stuff!
					$my_delta_y = sprintf("%.4f",$my_delta_y);

					print("$pad_layers[$i] , $pin_layer, $pin_name , $my_x , $my_y ");
					print(", $my_min_x , $my_max_x , $my_delta_x ");
					print(", $my_min_y , $my_max_y , $my_delta_y ");
					print("\n");
				}
			}
		}
	}
}



# hide some warnings for single use variables:
$ick = $bit_bucket;
$ick = $data_type;
$ick = $date_string;


#===================================================
# SUBROUTINES
#===================================================
sub numerically { $a <=> $b; }

#===================================================
sub read_I2 {
	local(*my_file,*bytes_left,$other_endian) = @_;

	if ($bytes_left>1) {
		read(my_file,$raw,2);
		$bytes_left = $bytes_left - 2;
		$raw = join('', reverse(split(//, $raw))) if $other_endian;
		$my_i2 = unpack('s',$raw);
	} else {
		$my_i2 = 0;
	}

	$my_i2;
}

#====================================================
sub read_Cn {
	local(*my_file,$bytes_to_read,*bytes_left,$other_endian) = @_;

	$bytes_to_read = $bytes_left if ($bytes_left < $bytes_to_read);
	if ($bytes_to_read>0) {
		read(my_file,$raw,$bytes_to_read);
		$bytes_left = $bytes_left - $bytes_to_read;
		$my_string = unpack("A*",$raw);
	} else {
		$my_string = "";
	}

	$my_string;
}

#====================================================
sub read_date {
	local(*my_file,*bytes_left,$other_endian) = @_;

	@mons = ('na','jan','feb','mar','apr','may','jun','jul',
		 'aug','sep','oct','nov','dec');

	# date is series of 6 U2's for year month date hour min sec
	$year = 1900 + &read_I2(*my_file,*bytes_left,$other_endian);
	$mon = &read_I2(*my_file,*bytes_left,$other_endian);
	$date = &read_I2(*my_file,*bytes_left,$other_endian);
	$hour = &read_I2(*my_file,*bytes_left,$other_endian);
	$min = &read_I2(*my_file,*bytes_left,$other_endian);
	$sec = &read_I2(*my_file,*bytes_left,$other_endian);
	$date_string = $year." ".$mons[$mon]." ".$date." ".$hour.":".$min.":".$sec;

	$date_string;
}

#====================================================
sub read_R8 {
	local(*my_file,*bytes_left,$other_endian) = @_;

	if ($bytes_left>7) {
		read(my_file,$raw,1);	# sign bit + exponent
		$my_exp = unpack('C',$raw);
		if ($my_exp>127) {
			$sign = -1.0;
			$my_exp = $my_exp - 128;
		} else {
			$sign = 1.0;
		}
		$my_exp = $my_exp - 64;
	
		read(my_file,$raw,1);	# MSB
		$my_u1 = unpack('C',$raw);
	
		read(my_file,$raw,2);	# next 2 bytes 
		$raw = join('', reverse(split(//, $raw))) if $other_endian;
		$my_u2 = unpack('v',$raw);
	
		read(my_file,$raw,4);	# least significant 4 bytes
		$bytes_left = $bytes_left - 8;
		$raw = join('', reverse(split(//, $raw))) if $other_endian;
		$my_u4 = unpack('V',$raw);
		$mantissa = $my_u1 * 2**48 + $my_u2 * 2**32 + $my_u4;
		$my_r8 = $sign * $mantissa/(2**56) * 16**$my_exp;
	
		#print(" sign $sign exp $my_exp bytes $my_u1 $my_u2 $my_u4 r8 $my_r8 \n");
	} else {
		$my_r8 = 0.0;
	}

	$my_r8;
}

#====================================================
sub read_translation {
	local(*my_file,*bytes_left,$other_endian) = @_;

	if ($bytes_left>1) {
		read(my_file,$raw,2);
		$bytes_left = $bytes_left - 2;
		$raw = join('', reverse(split(//, $raw))) if $other_endian;
		$my_u2 = unpack('v',$raw);
	} else {
		$my_u2 = 0;
	}
	$refl = 0;
	$absmag = 0;
	$absangle = 0;
	$refl = 1 if ($my_u2 & 0x8000)>0;
	$absmag = 1 if ($my_u2 & 0x0004)>0;
	$absangle = 1 if ($my_u2 & 0x0002)>0;

	($refl, $absmag, $absangle);
}

#====================================================
sub read_I4 {
	local(*my_file,*bytes_left,$other_endian) = @_;

	if ($bytes_left>3) {
		read(my_file,$raw,4);
		$bytes_left = $bytes_left - 4;
		$raw = join('', reverse(split(//, $raw))) if $other_endian;
		$my_i4 = unpack('l',$raw);
	} else {
		$my_i4 = 0;
	}

	$my_i4;
}

#====================================================
sub close_enough {
	local($pin_names,$my_x,$my_y,$near) = @_;

	$close = 0;
	$pinname_x = 0;
	$pinname_y = 0;

	foreach my $pin_x (keys %$pin_names) {
		if (($pin_x < ($my_x + $near)) && ($pin_x > ($my_x - $near)) ) {
			foreach my $pin_y (keys %{ $$pin_names{$pin_x} }) {
				if (($pin_y < ($my_y + $near)) && 
					($pin_y > ($my_y - $near)) ) {
					$close = 1;
					$pinname_x = $pin_x;
					$pinname_y = $pin_y;
				}
			}
		}
	}

	($close,$pinname_x,$pinname_y);
}

#====================================================
#====================================================
