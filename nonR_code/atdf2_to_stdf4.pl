# atdf2_to_stdf4.pl
# Copyright (C) 2004-2005 Michael Hackerott. All Rights Reserved
#
# modified 2014 David Gattrell - add option to generate big-endian binary...
#                              - shakedown stdf->atdf->stdf flow
# modified 2017 Paul Robins    - Correction to GDR record handling to write 0 pad byte 
#                                before the associated multibyte numeric data record.
# modified 2017 David Gattrell - revisit PTR and FTR, stdf->atdf->stdf->atdf with a595.stdf
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of either the GNU General Public License or the Artistic
# License as specified in the Perl README file.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
# OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# This module is documented using POD in-line with the perl code and
# is extracted using the pod2html utility:
#
#	pod2html --infile=atdf2_to_stdf4.pl --outfile=atdf2_to_stdf4.html

=pod

=head1 NAME

atdf2_to_stdf4.pl - Converts an ATDF version 2 file to an STDF version 4
file

=head1 SYNTAX

	perl atdf2_to_stdf4.pl [OPTIONS] ARGUMENTS

	OPTIONS

		-a, --about
			Displays information about this program.

		-d, --debug
			Enables debug mode which outputs debug information to
			standard output.

		-h, --help
			Displays help for this program.

		-l, --log
			Enables log mode which outputs log information to
			standard output.

		-i, --intel
			STDF is created in little endian format (intel / vax).  Default is big endian (motorola / sparc)
	
	
	ARGUMENTS

		AtdfFileSpec
			The input ATDF file specification.

		StdfFileSpec
			The output STDF file specification.

	RETURNS

		The program returns a value of unix true (0) if no errors
		occur; otherwise, a value of unix false (1) is returned.

	STANDARD INPUT

		Standard input is ignored.

	STANDARD OUTPUT

		There is no output to standard output unless the log (-l, --log)
		or debug (-d, --debug) options are specified.

	STANDARD ERROR

		If an error occurs then a message is output to standard error.

=head1 SYNOPSIS

	perl atdf2_to_stdf4.pl -a
	perl atdf2_to_stdf4.pl --about

		Displays information about the program.

	perl atdf2_to_stdf4.pl -h
	perl atdf2_to_stdf4.pl --help

		Displays the help for the program.

	perl atdf2_to_stdf4.pl AtdfFileSpec StdfFileSpec

		Converts the ATDF file specified by AtdfFileSpec to the STDF
		file specified by StdfFileSpec.

	perl atdf2_to_stdf4.pl -l AtdfFileSpec StdfFileSpec
	perl atdf2_to_stdf4.pl --log AtdfFileSpec StdfFileSpec

		Converts the ATDF file specified by AtdfFileSpec to the STDF
		file specified by StdfFileSpec and outputs log information
		to standard output.

	perl atdf2_to_stdf4.pl -d AtdfFileSpec StdfFileSpec
	perl atdf2_to_stdf4.pl --debug AtdfFileSpec StdfFileSpec

		Converts the ATDF file specified by AtdfFileSpec to the STDF
		file specified by StdfFileSpec and outputs debug information
		to standard output.

	perl atdf2_to_stdf4.pl -l -d AtdfFileSpec StdfFileSpec
	perl atdf2_to_stdf4.pl --log --debug AtdfFileSpec StdfFileSpec

		Converts the ATDF file specified by AtdfFileSpec to the STDF
		file specified by StdfFileSpec and outputs log and debug
		information to standard output.

	NOTE: Use command line redirection to capture standard
	output to a file.

=head1 DESCRIPTION

This program converts an ATDF Version 2 file to an STDF Version 4 file.

=head1 ASCII (ATDF) TO BINARY (STDF) CONVERSION

This program implements the ASCII to BINARY conversion as defined in the
Teradyne ATDF and STDF specifications.  However, the operating system
and the Perl that this program executes on determines the actual binary
values.

=cut

BEGIN
{
	unshift(
		@INC,
		'lib',
		'../lib'
	);
};

use TDF;

use strict;

######################################################################
# PROGRAM CONSTANT DECLARATIONS
######################################################################

# set the version number
my $VERSION = '2.0.0';

use constant TRUE	=> 1;
use constant FALSE	=> 0;

use constant EOL	=> "\n";
use constant Q1	=> "\'";
use constant Q2	=> "\"";
use constant SPC	=> "\ ";
use constant TAB	=> "\t";

my %ATDF_TO_STDF_RECORD_SUBREF = (
	'ATR' => \&ATR,
	'BPS' => \&BPS,
	'DTR' => \&DTR,
	'EPS' => \&EPS,
	'FAR' => \&FAR,
	'FTR' => \&FTR,
	'GDR' => \&GDR,
	'HBR' => \&HBR,
	'MIR' => \&MIR,
	'MPR' => \&MPR,
	'MRR' => \&MRR,
	'PCR' => \&PCR,
	'PGR' => \&PGR,
	'PIR' => \&PIR,
	'PLR' => \&PLR,
	'PMR' => \&PMR,
	'PRR' => \&PRR,
	'PTR' => \&PTR,
	'RDR' => \&RDR,
	'SBR' => \&SBR,
	'SDR' => \&SDR,
	'TSR' => \&TSR,
	'WCR' => \&WCR,
	'WIR' => \&WIR,
	'WRR' => \&WRR,
);

######################################################################
# PROGRAM VARIABLE DECLARATIONS
######################################################################

my @gTimer = (time());

my %gOptions = (
	'debug'	=> FALSE,
	'log'	=> FALSE,
);
my $gAtdfFileSpec = undef;
my $gStdfFileSpec = undef;

my $gAtdfFileLineCurr = undef;
my $gAtdfFileLineNext = undef;

my $gAtdfFieldDelim = undef;

my $gAtdfRecordType = undef;
my $gAtdfRecordData = undef;

my %gAtdfRD = ();	 # ATDF Record Data

my $gAtdfScalingFlag = undef;
my $gAtdfScaleFactor = undef;

my %gStdfRD = ();	 # STDF Record Data
my $gStdfRH = undef; # STDF Record Head
my $gStdfRD = undef; # STDF Record Data

######################################################################
# MAIN
######################################################################

# enable binary mode for standard output
binmode(STDOUT);

# enable autoflush
$| = TRUE;

# initialize the stdf binary file format as Vax / Sun format
stdfEndian(1);	# 1 = IBM / Motorola / HP / Sparc, 2 = Vax / Intel

# process the unix command line options
while ($ARGV[0] =~ m/^\-/)
{
	my $option = shift(@ARGV);
	if ($option =~ m/^\-(a|\-about)/)
	{
		about();
	}
	elsif ($option =~ m/^\-(d|\-debug)/)
	{
		$gOptions{'debug'} = TRUE;
	}
	elsif ($option =~ m/^\-(h|\-help)/)
	{
		help();
	}
	elsif ($option =~ m/^\-(l|\-log)/)
	{
		$gOptions{'log'} = TRUE;
	}
	elsif ($option =~ m/^\-(i|\-intel)/)
	{
		# set global endian flag $gEndian{'STDF'}
		stdfEndian(2);	# 2 = Intel / Vax, 1 = Sparc / Motorola / HP
	};
};
dbugData([\%gOptions], ['*gOptions']);

# get the ATDF file specification
$gAtdfFileSpec = shift(@ARGV);
if ($gAtdfFileSpec eq '')
{
	fatal('ATDF file specification is not defined!');
}
elsif (! -f $gAtdfFileSpec)
{
	fatal(
		'ATDF file does not exist:',
		$gAtdfFileSpec
	);
};
dbugData([$gAtdfFileSpec], ['*gAtdfFileSpec']);

# get the STDF file specification
$gStdfFileSpec = shift(@ARGV);
if ($gStdfFileSpec eq '')
{
	fatal('STDF file specification is not defined!');
};

# open the ATDF file for read
if (! open(FILE_ATDF, '<'.$gAtdfFileSpec))
{
	fatal(
		'Failed to open for read ATDF file:',
		$gAtdfFileSpec
	);
};
dbugData([$gStdfFileSpec], ['*gStdfFileSpec']);

# open the STDF file for write as binary
if (! open(FILE_STDF, '>'.$gStdfFileSpec))
{
	fatal(
		'Failed to open for write STDF file:',
		$gStdfFileSpec
	);
};
binmode(FILE_STDF);

#
# READ AND PROCESS THE FIRST LINE IN THE ATDF FILE
#
# The first line of the ATDF file must be an FAR record!
#	FAR:A|4|2|U

atdfFileRead();
if ($gAtdfRecordType ne 'FAR')
{
	fatal(
		'The first line in the ATDF file is NOT an FAR record:',
		$gAtdfFileLineCurr
	);
};

$gAtdfFieldDelim = "\\".substr($gAtdfFileLineCurr, 5, 1);
dbugData([$gAtdfFieldDelim], ['*gAtdfFieldDelim']);
if ($gAtdfFieldDelim eq "\\")
{
	fatal(
		'The ATDF field delimiter character',
		'is not defined!'
	);
};
atdfToStdf();

#
# READ AND PROCESS THE OTHER LINES IN THE ATDF FILE
#

# read the ATDF file
while (! eof(FILE_ATDF))
{
	atdfFileRead();
	atdfToStdf();
};
# process the last ATDF record
atdfFileRead();
atdfToStdf();

# close the STDF file
close(FILE_STDF);

# close the ATDF file
close(FILE_ATDF);

# set the timer
$gTimer[1] = time();

# logg conversion
my $atdfFileSize = (-s $gAtdfFileSpec);
my $stdfFileSize = (-s $gStdfFileSpec);
my $seconds = ($gTimer[1] - $gTimer[0]) || 1;
my $bytesPerSecond = $atdfFileSize / $seconds;
loggText(
	sprintf(
		'Converted ATDF file to STDF file in %d seconds (%.1f bytes/second).',
		$seconds,
		$bytesPerSecond
	),
	sprintf('ATDF file: %s (%d bytes).', $gAtdfFileSpec, $atdfFileSize),
	sprintf('STDF file: %s (%d bytes).', $gStdfFileSpec, $stdfFileSize),
);

# exit to unix without error
exit(0);

######################################################################
# SUBROUTINES
######################################################################

# about()

sub about
{
	TDF::about('atdf2_to_stdf4.pl', $VERSION)
};

# $text = atdfAtLineInFile()

sub atdfAtLineInFile
{
	return(join(SPC,
		'at line', $.,
		'in ATDF file:',
		$gAtdfFileSpec
	));
};

# $atdfFileLine = atdfFileLineRead()

sub atdfFileLineRead
{
	my $atdfFileLine = <FILE_ATDF>;
	chomp($atdfFileLine);
	dbugData([$atdfFileLine], ['*atdfFileLineRead::atdfFileLine']);
	return($atdfFileLine);
};

# atdfFileRead()

sub atdfFileRead
{
	# process the current ATDF file line
	if ($gAtdfFileLineCurr eq '')
	{
		# read an ATDF file line
		$gAtdfFileLineCurr = atdfFileLineRead();
	}
	else
	{
		# set the current ATDF line to the next ATDF line
		$gAtdfFileLineCurr = $gAtdfFileLineNext;
	};
	dbugDataPurdy([$gAtdfFileLineCurr], ['*gAtdfFileLineCurr']);

	# process the next ATDF file line (execute while loop once if
	# the next line is NOT a continuation line; otherwise, continue
	# until a non-continuation line is read from the ATDF file.
	my $isContinuationLine = TRUE;
	while ($isContinuationLine == TRUE)
	{
		# read an ATDF file line
		$gAtdfFileLineNext = atdfFileLineRead();
		dbugDataPurdy([$gAtdfFileLineNext], ['*gAtdfFileLineNext']);

		# parse the ATDF file line
		my @atdfLineChr = unpack('a1a*', $gAtdfFileLineNext);

		# set the ATDF continuation flag
		$isContinuationLine = ($atdfLineChr[0] eq SPC) || FALSE;
		dbugDataPurdy([$isContinuationLine], ['*gAtdfFileLineNextCont']);
		if ($isContinuationLine)
		{
			# special case where DTR text field with CR's
			# need to put the CR's back in the string
			if ( substr($gAtdfFileLineCurr,0,3) eq "DTR" ) 
			{
				$gAtdfFileLineCurr .= EOL;
			}
			$gAtdfFileLineCurr .= $atdfLineChr[1];
		};
	};

	# parse the ATDF record and set the ATDF record type and data global variables
	($gAtdfRecordType, $gAtdfRecordData) = unpack(
		'a3x1a*',
		$gAtdfFileLineCurr
	);
	dbugDataPurdy([$gAtdfRecordType], ['*gAtdfRecordType']);
	dbugDataPurdy([$gAtdfRecordData], ['*gAtdfRecordData']);
};

# atdfScale(\$real)

sub atdfScale
{
	my $rsReal = shift();
	if (! $gAtdfScalingFlag)
	{
		${$rsReal} *= (10 ** (-1 * $gAtdfScaleFactor));
	};
};

# atdfToStdf()
#
# Converts an ATDF record to an STDF record as follows:
#
# 1. Splits the ATDF record text to an array of data fields.
# 2. Reformats the ATDF text data for conversion to binary.
# 3. Converts ATDF fields containing data as text to STDF fields
#    containing data as binary where there is a simple one to one
#    correspondence between the STDF and the ATDF fields.
# 4. Calls a subroutine for each record type to perform any complex ATDF
#    to STDF data mappings.
# 5. Formats the STDF record.
#
# NOTE: The atdfFileLineRead() subroutine must be called prior to
# this subroutine to set the global variables correctly.

sub atdfToStdf
{
	# declare local variables
	my @atdfRecordData = ();
	my $stdfFieldName = undef;
	my $stdfDataType = undef;
	my %stdfFieldNameToDataTypeMap = ();
	my $atdfFieldName = undef;
	my $i = undef;
	my $r = undef;
	my $stdfRecordData = undef;
	my $stdfRecordHead = undef;

	# initialize globals
	%gAtdfRD = ();
	%gStdfRD = ();
	$gStdfRH = undef;
	$gStdfRD = undef;

	# test the ATDF to STDF subroutine reference
	if ($ATDF_TO_STDF_RECORD_SUBREF{$gAtdfRecordType} eq '')
	{
		fatal(
			'Unknown ATDF record type',
			q1($gAtdfRecordType),
			atdfAtLineInFile()
		);
	};

	# split the ATDF record data
	if ($gAtdfRecordType eq 'GDR')
	{
		@atdfRecordData = ($gAtdfRecordData);
	}
	else
	{
		@atdfRecordData = split(/$gAtdfFieldDelim/o, $gAtdfRecordData);
	};
	#dbugDataPurdy([\@atdfRecordData], ['*atdfRecordData']);

	# get the ATDF record field names
	my @atdfFieldNames = atdfRecordFieldNames($gAtdfRecordType);

	# get the STDF record field names
	my @stdfFieldNames = stdfRecordFieldNames($gAtdfRecordType);

	# set the ATDF record data
	for $i (0 .. $#atdfFieldNames)
	{
		# set the ATDF record data for this field
		$gAtdfRD{$atdfFieldNames[$i]} = $atdfRecordData[$i];
	};
	dbugAtdfRD(\@atdfFieldNames);

	# perform conversion and mapping of the ATDF to the STDF field data
	$ATDF_TO_STDF_RECORD_SUBREF{$gAtdfRecordType}->();
	dbugStdfRD(\@stdfFieldNames);

	# set the STDF record data
	for $i (0 .. $#stdfFieldNames)
	{
		$stdfRecordData .= $gStdfRD{$stdfFieldNames[$i]};
	};
	dbugDataBin('$stdfRecordData', $stdfRecordData);

	# set the STDF record head
	(my $stdfRecTyp, my $stdfRecSub) = stdfRecordNameToTypeCodes($gAtdfRecordType);
	dbugData([$stdfRecTyp], ['*stdfRecTyp']);
	dbugData([$stdfRecSub], ['*stdfRecSub']);
	$stdfRecordHead  = join('',
		A2S_U2(length($stdfRecordData)),
		A2S_U1($stdfRecTyp),
		A2S_U1($stdfRecSub)
	);
	dbugDataBin('$stdfRecordHead', $stdfRecordHead);

	# write the STDF record to the STDF file
	print FILE_STDF $stdfRecordHead.$stdfRecordData;
};

# atdfToStdfFieldData($fieldName, $dataType[, $invalid])

sub atdfToStdfFieldData
{
	my $fieldName = shift();
	my $dataType = shift();
	my $invalid = shift();
	my $r = \&{'A2S_'.$dataType};
	my $data = $gAtdfRD{$fieldName};	# debugging null vs '0' issue...
	#$gStdfRD{$fieldName} = $r->($gAtdfRD{$fieldName} || $invalid);

	#if ( ($dataType eq "Cn") && ($data eq '0') ) {
	#	$r = \&{'A2S_'.$dataType.'0'};	# special case
	#} 
	#$gStdfRD{$fieldName} = $r->($data || $invalid);
	
	my $dbg_dg = 0;
	if (length($data))
	{
		$dbg_dg = $r->($data);
	} 
	else 
	{
		if ($dataType eq 'Cn')
		{
			$dbg_dg = pack("x");
		}
		else
		{
			$dbg_dg = $r->($data || $invalid);
		}
	}

	# just broke it out to see variable in ddd...
	$gStdfRD{$fieldName} = $dbg_dg;
};

# $atdfTestExpr = atdfToStdfFlagExpr($atdfTestExpr, $stdfFlagName, $stdfFlagMask)
#
# IF the ATDF test expression is true THEN set the STDF bit flag field.

sub atdfToStdfFlagExpr
{
	my $atdfTestExpr = shift();
	my $stdfFlagName = shift();
	my $stdfFlagMask = shift();
	if ($atdfTestExpr)
	{
		$gStdfRD{$stdfFlagName} |= $stdfFlagMask;
	};
	return($atdfTestExpr);
};

# $trueIndex = atdfToStdfFlagExprIfElse(\@flagExprSets)
#
# Where \@flagExprSets is a reference to an array that contains one or
# more elements as follows:
#
#	$flagExprSets[]
#		[0]=An expression that evaluates $gAtdfRD{$atdfFlagName} for TRUE.
#		[1]=The name of the STDF flag field to set if the ATDF expression
#		    evaluates to TRUE.
#		[2]=The bit mask to be used to set the STDF flag field.
#
# For each element of @flagExprSets evaluate $flagExprSets[][0].  If
# $flagExprSets[][0] is FALSE then next else set $gStdfRD{$flagExprSets[][1]}
# using the bit mask $flagExprSets[][2].

sub atdfToStdfFlagExprIfElse
{
	my $raFlagExprIfElse = shift();
	my $i = undef;
	my $n = $#{$raFlagExprIfElse};
	my $trueIndex = -1;
	for ($i = 0; $i <= $n; $i += 1)
	{
		# If FALSE then next
		$raFlagExprIfElse->[$i][0] || next;
		# set the flag field bit
		$gStdfRD{$raFlagExprIfElse->[$i][1]} |= $raFlagExprIfElse->[$i][2];
		# set the TRUE index
		$trueIndex = $i;
	};
	# return the index of @flagExprSets where $raFlagExprIfElse->[$i][0] is TRUE
	# or -1 if never TRUE.
	return($trueIndex);
};

# atdfToStdfFlagsStr($atdfFlagName, \@flagInfo)
#
# Each character of the ATDF flags string is matched to the ATDF flag
# characters in the flag info array.  IF a match occurs THEN the related
# STDF flag is set to the value of the STDF bit flag.
#
# $flagInfo[]
#	[0]	ATDF flag character
#	[1]	STDF flag name
#	[2]	STDF bit flag

sub atdfToStdfFlagsStr
{
	my $atdfFlagName = shift();
	my $raFlagInfo = shift();
	for my $atdfFlag (split(//, uc($gAtdfRD{$atdfFlagName})))
	{
		for my $r (0 .. $#{$raFlagInfo})
		{
			if ($raFlagInfo->[$r][0] eq $atdfFlag)
			{
				$gStdfRD{$raFlagInfo->[$r][1]} |= $raFlagInfo->[$r][2];
				last;
			};
		};
	};
};

# atdfToStdfHeadNumber()

sub atdfToStdfHeadNumber
{
	#$gStdfRD{'HEAD_NUM'} = A2S_U1($gAtdfRD{'HEAD_NUM'} || 255);
	if( length($gAtdfRD{'HEAD_NUM'}) )
	{
		$gStdfRD{'HEAD_NUM'} = A2S_U1($gAtdfRD{'HEAD_NUM'});
	}
	else
	{
		$gStdfRD{'HEAD_NUM'} = A2S_U1(255);
	}
};

# atdfToStdfPassFailFlag()

sub atdfToStdfPassFailFlag
{
	my $atdfPassFailFlag = uc($gAtdfRD{'PassFailFlag'});
	atdfToStdfFlagExprIfElse([
		[($atdfPassFailFlag eq 'A'), 'PARM_FLG', 0x20],
		[($atdfPassFailFlag eq 'F'), 'TEST_FLG', 0x80],
		[($atdfPassFailFlag eq ''), 'TEST_FLG', 0x40],
	]);
};

# atdfToStdfReal($fieldName, $dataType)

sub atdfToStdfReal
{
	my $fieldName = shift();
	my $dataType = shift();
	my $real = $gAtdfRD{$fieldName};
	atdfScale(\$real);
	my $r = \&{'A2S_'.$dataType};
	$gStdfRD{$fieldName} = $r->($real);
};

# atdfToStdfSiteNumber()

sub atdfToStdfSiteNumber
{
	if (length($gAtdfRD{'SITE_NUM'}))
	{
		$gStdfRD{'SITE_NUM'} = A2S_U1($gAtdfRD{'SITE_NUM'});
	}
	else 
	{
		$gStdfRD{'SITE_NUM'} = A2S_U1(255);
	}
};

# atdfToStdfUnits()
#
# Sets the STDF UNITS field and the global scale factor variable
# from the ATDF UNITS field.

sub atdfToStdfUnits
{
	my $atdfUnits = $gAtdfRD{'UNITS'};
	# test the ATDF scale flag
	if ($gAtdfScalingFlag == 0) # unscaled
	{
		# set the ATDF scale factor
		$gAtdfScaleFactor = scaleFactor($atdfUnits);
		dbugData([$gAtdfScaleFactor], ['*atdfToStdfUnits::gAtdfScaleFactor']);
		# set the scaled ATDF units
		if ($gAtdfScaleFactor != 0)
		{
			$atdfUnits = substr($atdfUnits, 1);
		};
	};
	# set the STDF units
	$gStdfRD{'UNITS'} = A2S_Cn($atdfUnits);
};

# $status = dbugEnabled()

sub dbugEnabled
{
	return($gOptions{'debug'});
};

# dbugData(dbugData(\@varRefs, \@varNames))

sub dbugData
{
	dbugEnabled() && TDF::dbugData(@_);
};

# dbugDataBin($datnam, $bindat)

sub dbugDataBin
{
	dbugEnabled() && TDF::dbugDataBin(@_);
};

# dbugDataPurdy(\@varRefs, \@varNames)

sub dbugDataPurdy
{
	dbugEnabled() && TDF::dbugDataPurdy(@_);
};

# dbugText(@text)

sub dbugText
{
	dbugEnabled() && TDF::dbugText(@_);
};

# dbugAtdfRD(\@fieldNames)

sub dbugAtdfRD
{
	my $raFieldNames = shift();
	my @text = ('%gAtdfRD = (');
	my $i = undef;
	my $atdfFieldName = undef;
	for $i (0 .. $#{$raFieldNames})
	{
		$atdfFieldName = $raFieldNames->[$i];
		push(@text, '  '.join(SPC,
			q1($atdfFieldName),
			'=>',
			$gAtdfRD{$atdfFieldName},
			','
		));
	};
	push(@text, ');');
	dbugText(join(EOL, @text));
};

# dbugStdfRD(\@fieldNames)

sub dbugStdfRD
{
	my $raFieldNames = shift();
	my $i = undef;
	my $stdfFieldName = undef;
	my @text = ('%gStdfRD = (');
	for $i (0 .. $#{$raFieldNames})
	{
		$stdfFieldName = $raFieldNames->[$i];
		push(@text,
			(SPC x 2).q1($stdfFieldName).SPC.'=>',
			map((SPC x 2).$_, dbugFmtBin($gStdfRD{$stdfFieldName})),
		);
	};
	push(@text, ');');
	dbugText(join(EOL, @text));
};

# help()

sub help
{
	print <<"END_HELP";
NAME

	atdf2_to_stdf4.pl - converts an ATDF Version 2 file to an STDF
	Version 4 file.

SYNTAX

	perl atdf2_to_stdf4.pl [OPTIONS] ARGUMENTS

	OPTIONS

		-a, --about
			Displays information about this program.

		-d, --debug
			Enables debug mode which outputs debug information to the
			log file.

		-h, --help
			Displays help for this program.

		-l, --log=filespec
			Enables log mode which outputs log information to the
			specificied log file.

	ARGUMENTS

		atdfFileSpec
			The input ATDF file specification.

		stdfFileSpec
			The output STDF file specification.

	RETURNS

		The program returns a value of unix true (0) if no errors
		occur; otherwise, a value of unix false (1) is returned.

STANDARD INPUT

	Standard input is ignored.

STANDARD OUTPUT

	There is no output to standard output.

STANDARD ERROR

	If an error occurs then a message is output to standard error.

SEE ALSO

	The program documentation for additional information.
END_HELP

	# exit to unix without error
	exit(0);
};

#=====================================================================
# ATDF TO STDF RECORD SUBROUTINES
#=====================================================================

# ATR()
#
# ATR:0:03:00 3-SEP-1992|bin_filter 7,9-12

sub ATR
{
	$gStdfRD{'MOD_TIM'} = A2S_U4(dateTimeToSeconds($gAtdfRD{'MOD_TIM'}));
	atdfToStdfFieldData('CMD_LINE', 'Cn');
};

# BPS()
#
# BPS:DC_TESTS

sub BPS
{
	atdfToStdfFieldData('SEQ_NAME', 'Cn');
};

# DTR()
#
# DTR:Datalog sampling rate is now 1 in 10

sub DTR
{
	atdfToStdfFieldData('TEST_DAT', 'Cn');
};

# EPS()
#
# EPS:

sub EPS
{
};

# FAR()
#
# NOTE: The first line of the ATDF file must be an FAR record!
#
# FAR:A|4|2|U

sub FAR
{
	# test the ATDF "Data File Type" field
	if (uc($gAtdfRD{'DataFileType'}) ne 'A')
	{
		fatal(
			'Invalid ATDF.FAR.DataFileType:',
			$gAtdfRD{'DataFileType'}
		);
	};

	# test the ATDF "STDF Version" field
	if (uc($gAtdfRD{'STDF_VER'}) != 4)
	{
		fatal(
			'Invalid ATDF.FAR.STDF_VER:',
			$gAtdfRD{'STDF_VER'}
		);
	};

	# test the ATDF "ATDF Version" field
	if (uc($gAtdfRD{'AtdfVersion'}) != 2)
	{
		fatal(
			'Invalid ATDF.FAR.AtdfVersion:',
			$gAtdfRD{'AtdfVersion'}
		);
	};

	# test the ATDF "Scaling Flag" field
	if (uc($gAtdfRD{'ScalingFlag'}) eq 'U')
	{
		$gAtdfScalingFlag = 0;
	}
	elsif (($gAtdfRD{'ScalingFlag'} eq '') || (uc($gAtdfRD{'ScalingFlag'}) eq 'S'))
	{
		$gAtdfScalingFlag = 1;
	}
	else
	{
		fatal(
			'Invalid ATDF.FAR.ScalingFlag:',
			$gAtdfRD{'ScalingFlag'}
		);
	};

	#$gStdfRD{'CPU_TYPE'} = A2S_U1(osCpuType());
	if (isStdfEndianBig) 
	{
		$gStdfRD{'CPU_TYPE'} = A2S_U1(1);	
	} else {
		$gStdfRD{'CPU_TYPE'} = A2S_U1(2);	
	}

	$gStdfRD{'STDF_VER'} = A2S_U1(4);
};

# FTR()
#
# FTR:27|2|1|P||CHECKERBOARD|A1|5|16|2|3|6|3|0|
#  10,2,8,12|0,1,1,4|4,5,6,7|0,0,0,0|8|DRV
#  |Check Driver||||2|2,3,4,6

sub FTR
{
	atdfToStdfFieldData('TEST_NUM', 'U4');
	atdfToStdfHeadNumber();
	atdfToStdfSiteNumber();
	atdfToStdfFieldData('VECT_NAM', 'Cn');
	atdfToStdfFieldData('TIME_SET', 'Cn');
	atdfToStdfFieldData('CYCL_CNT', 'U4');
	atdfToStdfFieldData('REL_VADR', 'U4');
	atdfToStdfFieldData('REPT_CNT', 'U4');
	atdfToStdfFieldData('NUM_FAIL', 'U4');
	atdfToStdfFieldData('XFAIL_AD', 'I4');
	atdfToStdfFieldData('YFAIL_AD', 'I4');
	atdfToStdfFieldData('VECT_OFF', 'I2');

	my @array = ();

	@array = split(',', $gAtdfRD{'ReturnIndexes'});
	$gStdfRD{'RTN_INDX'} = A2S_xTYPE(\&A2S_U2, [@array]);
	@array = split(',', $gAtdfRD{'ReturnStates'});
	$gStdfRD{'RTN_STAT'} = A2S_xTYPE(\&A2S_N1, [@array]);
	$gStdfRD{'RTN_ICNT'} = A2S_U2(scalar(@array));

	@array = split(',', $gAtdfRD{'ProgIndexes'});
	$gStdfRD{'PGM_INDX'} = A2S_xTYPE(\&A2S_U2, [@array]);
	@array = split(',', $gAtdfRD{'ProgStates'});
	$gStdfRD{'PGM_STAT'} = A2S_xTYPE(\&A2S_N1, [@array]);
	$gStdfRD{'PGM_ICNT'} = A2S_U2(scalar(@array));

	@array = split(',', $gAtdfRD{'FailingPins'});
	$gStdfRD{'FAIL_PIN'} = A2S_Dn(@array);

	atdfToStdfFieldData('OP_CODE', 'Cn');
	atdfToStdfFieldData('TEST_TXT', 'Cn');
	atdfToStdfFieldData('ALARM_ID', 'Cn');
	atdfToStdfFieldData('PROG_TXT', 'Cn');
	atdfToStdfFieldData('RSLT_TXT', 'Cn');
	atdfToStdfFieldData('PATG_NUM', 'U1');

	@array = split(',', $gAtdfRD{'Comparators'});
	$gStdfRD{'SPIN_MAP'} = A2S_Dn(@array);

	$gStdfRD{'TEST_FLG'} = 0x00;
	atdfToStdfPassFailFlag();
	atdfToStdfFlagsStr(
		'AlarmFlags',
		[
			# Bit 0: Alarm detected during testing
			['A', 'TEST_FLG', 0x01],
			# Bit 2: Test result unreliable
			['U', 'TEST_FLG', 0x04],
			# Bit 3: Time out occured
			['T', 'TEST_FLG', 0x08],
			# Bit 4: Test was not executed
			['N', 'TEST_FLG', 0x10],
			# Bit 5: Test aborted
			['X', 'TEST_FLG', 0x20],
		]
	);
	$gStdfRD{'TEST_FLG'} = A2S_B1($gStdfRD{'TEST_FLG'});

	$gStdfRD{'OPT_FLAG'} = 0xC0;
	atdfToStdfFlagExpr(($gAtdfRD{'CYCL_CNT'} eq ''), 'OPT_FLAG', 0x01);
	atdfToStdfFlagExpr(($gAtdfRD{'REL_VADR'} eq ''), 'OPT_FLAG', 0x02);
	atdfToStdfFlagExpr(($gAtdfRD{'REPT_CNT'} eq ''), 'OPT_FLAG', 0x04);
	atdfToStdfFlagExpr((($gAtdfRD{'XFAIL_AD'} eq '') && ($gAtdfRD{'YFAIL_AD'} eq '')), 'OPT_FLAG', 0x10);
	atdfToStdfFlagExpr(($gAtdfRD{'VECT_OFF'} eq ''), 'OPT_FLAG', 0x20);
	$gStdfRD{'OPT_FLAG'} = A2S_B1($gStdfRD{'OPT_FLAG'});
};

# GDR()
#
# GDR:TThis is text|L-435|U255|F645.7110|XFFE0014C

sub GDR
{
	# define local variables
	my @atdfGenData = split(/\|/, $gAtdfRD{'GenericData'});
	my %atdfFmtToDataType = (
		'U' => 'U1',
		'M' => 'U2',
		'B' => 'U4',
		'I' => 'I1',
		'S' => 'I2',
		'L' => 'I4',
		'F' => 'R4',
		'D' => 'R8',
		'T' => 'Cn',
		'X' => 'Bn',
		'Y' => 'Dn',
		'N' => 'N1',
	);
	my %dataTypeToCode = (
		'U1' => 1,
		'U2' => 2,
		'U4' => 3,
		'I1' => 4,
		'I2' => 5,
		'I4' => 6,
		'R4' => 7,
		'R8' => 8,
		'Cn' => 10,
		'Bn' => 11,
		'Dn' => 12,
		'N1' => 13,
	);

	# initialize the STDF GEN_DATA
	$gStdfRD{'GEN_DATA'} = undef;

	# set the stdf record data
	$gStdfRD{'FLD_CNT'} = A2S_U2(scalar(@atdfGenData));
	my $fmt = undef;
	my $txt = undef;
	my $dataType = undef;
	my $bin = undef;
	my $r = undef;
	my $even_boundary = 1;	# the GDR is guaranteed to begin on an even byte boundary
	for my $i (0 .. $#atdfGenData)
	{
		$atdfGenData[$i] =~ m/^(.)(.+)$/;
		$fmt = $1;
		$bin = undef;
		dbugData([$fmt], ['*GDR::fmt']);
		$txt = $2;
		dbugData([$txt], ['*GDR::txt']);
		$dataType = $atdfFmtToDataType{$fmt};
		dbugData([$dataType], ['*GDR::dataType']);
		if ($dataType eq '')
		{
			fatal(
				'Invalid GDR format character code',
				q1($fmt),
				'in field:',
				$atdfGenData[$i]
			);
		};
		if ($fmt =~ m/^[MBSLFD]$/)
		{
			if ($even_boundary)
			{
				$bin = A2S_B0();
				# once this field is done, we will still be on even boundary
			}
			else
			{
				# once this field is done, we will be on an even boundary
				$even_boundary = 1;
			}
		}
		elsif ($fmt =~ m/^[TXY]$/)
		{
			if ( (length($2) % 2) == 0 )
			{
				# even + type code byte, so overall is odd, need to toggle boundary flag
				$even_boundary = ! $even_boundary;
			};
		};
		$bin .= A2S_U1($dataTypeToCode{$dataType});
        
		$r = \&{'A2S_'.$dataType};
		$bin .= $r->($txt);
		dbugDataBin('GDR::bin', $bin);
		$gStdfRD{'GEN_DATA'} .= $bin;
	};
};

# HBR()
#
# HBR:2|1|6|212|F|SHORT
# HBR:||1|1346|P|PASSED

sub HBR
{
	atdfToStdfHeadNumber();
	atdfToStdfSiteNumber();
	atdfToStdfFieldData('HBIN_NUM', 'U2');
	atdfToStdfFieldData('HBIN_CNT', 'U4');
	atdfToStdfFieldData('HBIN_PF',  'C1', SPC);
	atdfToStdfFieldData('HBIN_NAM', 'Cn');
};

# MIR()
#
# MIR:A3002B|80386|80386HOT|akbar|J971
#  |8:14:59 23-JUL-1992|8:23:02 23-JUL-1992|Sandy
#  |P|1|2B|HOT|N|3.1.2|IG900|2.4|||300|100||
#  386_data.txt|ceramic|386|wk23||MPU2||||||386HOT|
#  3S|||A42136S|JOAN_S

sub MIR
{
	atdfToStdfFieldData('LOT_ID', 'Cn');
	atdfToStdfFieldData('PART_TYP', 'Cn');
	atdfToStdfFieldData('JOB_NAM', 'Cn');
	atdfToStdfFieldData('NODE_NAM', 'Cn');
	atdfToStdfFieldData('TSTR_TYP', 'Cn');
	$gStdfRD{'SETUP_T'} = A2S_U4(dateTimeToSeconds($gAtdfRD{'SETUP_T'}));
	$gStdfRD{'START_T'} = A2S_U4(dateTimeToSeconds($gAtdfRD{'START_T'}));
	atdfToStdfFieldData('OPER_NAM', 'Cn');
	atdfToStdfFieldData('MODE_COD', 'C1', SPC);
	atdfToStdfFieldData('STAT_NUM', 'U1');
	atdfToStdfFieldData('SBLOT_ID', 'Cn');
	atdfToStdfFieldData('TEST_COD', 'Cn');
	atdfToStdfFieldData('RTST_COD', 'C1', SPC);
	atdfToStdfFieldData('JOB_REV', 'Cn');
	atdfToStdfFieldData('EXEC_TYP', 'Cn');
	atdfToStdfFieldData('EXEC_VER', 'Cn');
	atdfToStdfFieldData('PROT_COD', 'C1', SPC);
	atdfToStdfFieldData('CMOD_COD', 'C1', SPC);
	atdfToStdfFieldData('BURN_TIM', 'U2', 65535);
	atdfToStdfFieldData('TST_TEMP', 'Cn');
	atdfToStdfFieldData('USER_TXT', 'Cn');
	atdfToStdfFieldData('AUX_FILE', 'Cn');
	atdfToStdfFieldData('PKG_TYP', 'Cn');
	atdfToStdfFieldData('FAMLY_ID', 'Cn');
	atdfToStdfFieldData('DATE_COD', 'Cn');
	atdfToStdfFieldData('FACIL_ID', 'Cn');
	atdfToStdfFieldData('FLOOR_ID', 'Cn');
	atdfToStdfFieldData('PROC_ID', 'Cn');
	atdfToStdfFieldData('OPER_FRQ', 'Cn');
	atdfToStdfFieldData('SPEC_NAM', 'Cn');
	atdfToStdfFieldData('SPEC_VER', 'Cn');
	atdfToStdfFieldData('FLOW_ID', 'Cn');
	atdfToStdfFieldData('SETUP_ID', 'Cn');
	atdfToStdfFieldData('DSGN_REV', 'Cn');
	atdfToStdfFieldData('ENG_ID', 'Cn');
	atdfToStdfFieldData('ROM_COD', 'Cn');
	atdfToStdfFieldData('SERL_NUM', 'Cn');
	atdfToStdfFieldData('SUPR_NAM', 'Cn');
};

# MPR()
#
# MPR:143|2|4||001.3,0009.6,001.5|F|D|||LH|mA|001.0
#  |002.0| 4.5|.1|V|3,4,5|%6.1f|%6.1f|%6.1f|0009.75
#  |002.25

sub MPR
{
	atdfToStdfFieldData('TEST_NUM', 'U4');
	atdfToStdfHeadNumber();
	atdfToStdfSiteNumber();

	my @atdfStatesArray = split(',', $gAtdfRD{'StatesArray'});
	my $atdfStatesArrayCount = scalar(@atdfStatesArray);
	$gStdfRD{'RTN_ICNT'} = A2S_U2($atdfStatesArrayCount);
	$gStdfRD{'RTN_STAT'} = A2S_xTYPE(\&A2S_N1, \@atdfStatesArray);

	my @atdfResultsArray = split(',', $gAtdfRD{'ResultsArray'});
	$gStdfRD{'RSLT_CNT'} = A2S_U2(scalar(@atdfResultsArray));
	$gStdfRD{'RTN_RSLT'} = A2S_xTYPE(\&A2S_R4, \@atdfResultsArray);

	atdfToStdfFieldData('TEST_TXT', 'Cn');
	atdfToStdfFieldData('ALARM_ID', 'Cn');

	atdfToStdfUnits();

	atdfToStdfReal('LO_LIMIT', 'R4');
	atdfToStdfReal('HI_LIMIT', 'R4');
	atdfToStdfReal('START_IN', 'R4');
	atdfToStdfReal('INCR_IN', 'R4');
	atdfToStdfFieldData('UNITS_IN', 'Cn');

	my @atdfIndexArray = split(',', $gAtdfRD{'IndexArray'});
	my $atdfIndexArrayCount = scalar(@atdfIndexArray);
	if ($atdfStatesArrayCount != $atdfIndexArrayCount)
	{
		fatal(
			'MPR.StatesArray and MPR.IndexArray sizes are not equal!'
		)
	};
	$gStdfRD{'RTN_INDX'} = A2S_xTYPE(\&A2S_U2, \@atdfIndexArray);

	atdfToStdfFieldData('C_RESFMT', 'Cn');
	atdfToStdfFieldData('C_LLMFMT', 'Cn');
	atdfToStdfFieldData('C_HLMFMT', 'Cn');
	atdfToStdfReal('LO_SPEC', 'R4');
	atdfToStdfReal('HI_SPEC', 'R4');

	for my $fieldName ('RES_SCAL', 'LLM_SCAL', 'HLM_SCAL')
	{
		my $atdfScale = $gAtdfRD{$fieldName};
		if (! $gAtdfScalingFlag)
		{
			$atdfScale = $gAtdfScaleFactor;
		};
		$gStdfRD{$fieldName} = A2S_I1($atdfScale);
	};

	$gStdfRD{'TEST_FLG'} = 0x00;
	$gStdfRD{'PARM_FLG'} = 0x00;
	$gStdfRD{'OPT_FLAG'} = 0x02;

	atdfToStdfPassFailFlag();

	atdfToStdfFlagsStr(
		'AlarmFlags',
		[
			['A', 'TEST_FLG', 0x01],
			['U', 'TEST_FLG', 0x04],
			['T', 'TEST_FLG', 0x08],
			['N', 'TEST_FLG', 0x10],
			['X', 'TEST_FLG', 0x20],
			['S', 'PARM_FLG', 0x01],
			['D', 'PARM_FLG', 0x02],
			['O', 'PARM_FLG', 0x04],
			['H', 'PARM_FLG', 0x08],
			['L', 'PARM_FLG', 0x10],
		]
	);

	atdfToStdfFlagsStr(
		'LimitCompare',
		[
			['L', 'PARM_FLG', 0x40],
			['H', 'PARM_FLG', 0x80],
		]
	);

	# STDF TEST_FLG Bit 1: Test result is unreliable
	atdfToStdfFlagExpr(($gAtdfRD{'RESULT'} eq ''), 'TEST_FLG', 0x02);

	# STDF OPT_FLAG Bit 0: Invalid result scale factor
	atdfToStdfFlagExpr(($gAtdfRD{'RES_SCAL'} eq ''), 'OPT_FLAG', 0x01);
	# STDF OPT_FLAG Bit 2: No low specification
	atdfToStdfFlagExpr(($gAtdfRD{'LO_SPEC'} eq ''), 'OPT_FLAG', 0x04);
	# STDF OPT_FLAG Bit 3: No high specification
	atdfToStdfFlagExpr(($gAtdfRD{'HI_SPEC'} eq ''), 'OPT_FLAG', 0x08);
	# STDF OPT_FLAG Bit 4: Invalid low limit
	atdfToStdfFlagExpr(($gAtdfRD{'LO_LIMIT'} eq ''), 'OPT_FLAG', 0x10);
	# STDF OPT_FLAG Bit 5: Invalid high limit
	atdfToStdfFlagExpr(($gAtdfRD{'HI_LIMIT'} eq ''), 'OPT_FLAG', 0x20);

	$gStdfRD{'TEST_FLG'} = A2S_B1($gStdfRD{'TEST_FLG'});
	$gStdfRD{'PARM_FLG'} = A2S_B1($gStdfRD{'PARM_FLG'});
	$gStdfRD{'OPT_FLAG'} = A2S_B1($gStdfRD{'OPT_FLAG'});
};

# MRR()
#
# MRR:12:17:12 23-JUL-1992|H|Handler problems|Yield Alarm

sub MRR
{
	$gStdfRD{'FINISH_T'} = A2S_U4(dateTimeToSeconds($gAtdfRD{'FINISH_T'}));
	$gStdfRD{'DISP_COD'} = A2S_C1(substr($gAtdfRD{'DISP_COD'}, 0, 1) || SPC);
	atdfToStdfFieldData('USR_DESC', 'Cn');
	atdfToStdfFieldData('EXC_DESC', 'Cn');
};

# PCR()
#
# PCR:2|1|497|5|11|212|481
# PCR:||3976|54|76|2311|3809

sub PCR
{
	atdfToStdfHeadNumber();
	atdfToStdfSiteNumber();
	atdfToStdfFieldData('PART_CNT', 'U4');
	atdfToStdfFieldData('RTST_CNT', 'U4', 4294967295);
	atdfToStdfFieldData('ABRT_CNT', 'U4', 4294967295);
	atdfToStdfFieldData('GOOD_CNT', 'U4');
	atdfToStdfFieldData('FUNC_CNT', 'U4', 4294967295);
};

# PGR()
#
# PGR:12|Data Out|5,6,7,8,9,10,11,12

sub PGR
{
	atdfToStdfFieldData('GRP_INDX', 'U2');
	atdfToStdfFieldData('GRP_NAM',  'Cn');
	my @atdfIndexArray = split(',', $gAtdfRD{'IndexArray'});
	$gStdfRD{'INDX_CNT'} = A2S_U2(scalar(@atdfIndexArray));
	$gStdfRD{'PMR_INDX'} = A2S_xTYPE(\&A2S_U2, \@atdfIndexArray);
};

# PIR()
#
# PIR:2|1

sub PIR
{
	atdfToStdfHeadNumber();
	atdfToStdfSiteNumber();
};

# PLR()
#
# PLR:2,3,6|20,20,21|H,H,H|H,L,L/H,H,H/L,L,L
#  |1,0,M/1,0,H/M,L,H

sub PLR
{
	my @atdfArray = ();

	@atdfArray = split(',', $gAtdfRD{'IndexArray'});
	$gStdfRD{'GRP_CNT'}  = A2S_U2(scalar(@atdfArray));
	$gStdfRD{'GRP_INDX'} = A2S_xTYPE(\&A2S_U2, \@atdfArray);

	@atdfArray = split(',', $gAtdfRD{'ModeArray'});
	$gStdfRD{'GRP_MODE'} = A2S_xTYPE(\&A2S_U2, \@atdfArray);

	@atdfArray = split(',', $gAtdfRD{'RadixArray'});
	my %atdfToStdfRadix = (
		'B' => 2,
		'O' => 8,
		'D' => 10,
		'H' => 16,
		'S' => 20,
	);
	for my $i (0 .. $#atdfArray)
	{
		$atdfArray[$i] = $atdfToStdfRadix{$atdfArray[$i]} || 0;
	};
	$gStdfRD{'GRP_RADX'} = A2S_xTYPE(\&A2S_U1, \@atdfArray);

	# split the ATDF Program State Array
	#	...|H,L,L/H,H,H/L,L,L|...
	@atdfArray = split(/\//, $gAtdfRD{'ProgramState'});
	my @atdfChal = ();
	my @atdfChar = ();
	for my $stateList (@atdfArray)
	{
		my @stateList = split(',', $stateList);
		my @chal = ();
		my @char = ();
		for my $i (0 .. $#stateList)
		{
			my $n = length($stateList[$i]);
			if ($n == 1)
			{
				# set the STDF PGM_CHAL
				$chal[$i] = SPC;
				# set the STDF PGM_CHAR
				$char[$i] = $stateList[$i];
			}
			elsif ($n == 2)
			{
				# set the STDF PGM_CHAL
				$chal[$i] = substr($stateList[$i], 1, 1);
				# set the STDF PGM_CHAR
				$char[$i] = substr($stateList[$i], 2, 1);
			};
		};
		push(@atdfChal, join(',', @chal));
		push(@atdfChar, join(',', @char));
	};
	dbugData([\@atdfChal], ['*PLR::atdfChal']);
	$gStdfRD{'PGM_CHAL'} = A2S_xTYPE(\&A2S_Cn, \@atdfChal);
	dbugData([\@atdfChar], ['*PLR::atdfChar']);
	$gStdfRD{'PGM_CHAR'} = A2S_xTYPE(\&A2S_Cn, \@atdfChar);

	# split the ATDF Returned State Array
	#	...|1,0,M/1,0,H/M,L,H|...
	@atdfArray = split(/\//, $gAtdfRD{'ReturnedState'});
	my @atdfChal = ();
	my @atdfChar = ();
	for my $stateList (@atdfArray)
	{
		my @stateList = split(',', $stateList);
		my @chal = ();
		my @char = ();
		for my $i (0 .. $#stateList)
		{
			my $n = length($stateList[$i]);
			if ($n == 1)
			{
				# set the STDF PGM_CHAL
				$chal[$i] = SPC;
				# set the STDF PGM_CHAR
				$char[$i] = $stateList[$i];
			}
			elsif ($n == 2)
			{
				# set the STDF PGM_CHAL
				$chal[$i] = substr($stateList[$i], 1, 1);
				# set the STDF PGM_CHAR
				$char[$i] = substr($stateList[$i], 2, 1);
			};
		};
		push(@atdfChal, join(',', @chal));
		push(@atdfChar, join(',', @char));
	};
	dbugData([\@atdfChal], ['*PLR::atdfChal']);
	$gStdfRD{'RTN_CHAL'} = A2S_xTYPE(\&A2S_Cn, \@atdfChal);
	dbugData([\@atdfChar], ['*PLR::atdfChar']);
	$gStdfRD{'RTN_CHAR'} = A2S_xTYPE(\&A2S_Cn, \@atdfChar);
};

# PMR()
#
# PMR:2|A|1-7|GND|MAIN GROUND|2|1

sub PMR
{
	atdfToStdfFieldData('PMR_INDX', 'U2');
	atdfToStdfFieldData('CHAN_TYP', 'U2');
	atdfToStdfFieldData('CHAN_NAM', 'Cn');
	atdfToStdfFieldData('PHY_NAM',  'Cn');
	atdfToStdfFieldData('LOG_NAM',  'Cn');
	atdfToStdfHeadNumber();
	atdfToStdfSiteNumber();
};

# PRR()
#
# PRR:2|1|13|78|F|0|17|-2|7|||644|
#  Device at edge of wafer|F13C20

sub PRR
{
	atdfToStdfHeadNumber();
	atdfToStdfSiteNumber();
	atdfToStdfFieldData('PART_ID',  'Cn');
	atdfToStdfFieldData('NUM_TEST', 'U2');
	atdfToStdfFieldData('HARD_BIN', 'U2');
	atdfToStdfFieldData('SOFT_BIN', 'U2', 65535);
	atdfToStdfFieldData('X_COORD',  'I2', -32768);
	atdfToStdfFieldData('Y_COORD',  'I2', -32768);
	atdfToStdfFieldData('TEST_T',   'U4');
	atdfToStdfFieldData('PART_TXT', 'Cn');
	atdfToStdfFieldData('PART_FIX', 'Bn');

	$gStdfRD{'PART_FLG'} = 0x00;
	my $atdfPassFailFlag = uc($gAtdfRD{'PassFailCode'});
	atdfToStdfFlagExprIfElse([
		[($atdfPassFailFlag eq 'F'), 'PART_FLG', 0x08],
		[($atdfPassFailFlag eq 'P'), 'PART_FLG', 0x00],
		[($atdfPassFailFlag eq ''),  'PART_FLG', 0x10],
	]);

	my $atdfRetestCode = uc($gAtdfRD{'RetestCode'});
	atdfToStdfFlagExprIfElse([
		# PIR, PTR, MPR, FTR, and PRR for the same HEAD_NUM and
		# SITE_NUM supersede any sequence of records with the same
		# PART_ID
		[($atdfRetestCode eq 'I'), 'PART_FLG', 0x01],
		# PIR, PTR, MPR, FTR, and PRR for the same HEAD_NUM and
		# SITE_NUM supersede any sequence of records with the same
		# X_COORD and Y_COORD
		[($atdfRetestCode eq 'C'), 'PART_FLG', 0x02],
	]);
	atdfToStdfFlagExpr(($gAtdfRD{'AbortCode'} eq 'Y'), 'PART_FLG', 0x04);
	$gStdfRD{'PART_FLG'} = A2S_B1($gStdfRD{'PART_FLG'});
};

# PTR()
#
# PTR:23|2|1|997.3|F|AOH|Check 2nd layer|||
#  A|-1.7|45.2| %9.4f|%7.2f|%7.2f|-1.75|45.25|3|3|4

sub PTR
{
	atdfToStdfFieldData('TEST_NUM', 'U4');
	atdfToStdfHeadNumber();
	atdfToStdfSiteNumber();
	atdfToStdfFieldData('RESULT', 'R4');

	atdfToStdfFieldData('TEST_TXT', 'Cn');
	atdfToStdfFieldData('ALARM_ID', 'Cn');

	atdfToStdfUnits();

	atdfToStdfReal('RESULT', 'R4');
	atdfToStdfReal('LO_LIMIT', 'R4');
	atdfToStdfReal('HI_LIMIT', 'R4');

	atdfToStdfFieldData('C_RESFMT', 'Cn');
	atdfToStdfFieldData('C_LLMFMT', 'Cn');
	atdfToStdfFieldData('C_HLMFMT', 'Cn');
	atdfToStdfReal('LO_SPEC', 'R4');
	atdfToStdfReal('HI_SPEC', 'R4');

	for my $fieldName ('RES_SCAL', 'LLM_SCAL', 'HLM_SCAL')
	{
		my $atdfScale = $gAtdfRD{$fieldName};
		if (! $gAtdfScalingFlag)
		{
			$atdfScale = $gAtdfScaleFactor;
		};
		$gStdfRD{$fieldName} = A2S_I1($atdfScale);
	};

	$gStdfRD{'TEST_FLG'} = 0x00;
	$gStdfRD{'PARM_FLG'} = 0x00;
	$gStdfRD{'OPT_FLAG'} = 0x02;

	atdfToStdfPassFailFlag();

	atdfToStdfFlagsStr(
		'AlarmFlags',
		[
			['A', 'TEST_FLG', 0x01],
			['U', 'TEST_FLG', 0x04],
			['T', 'TEST_FLG', 0x08],
			['N', 'TEST_FLG', 0x10],
			['X', 'TEST_FLG', 0x20],
			['S', 'PARM_FLG', 0x01],
			['D', 'PARM_FLG', 0x02],
			['O', 'PARM_FLG', 0x04],
			['H', 'PARM_FLG', 0x08],
			['L', 'PARM_FLG', 0x10],
		]
	);

	atdfToStdfFlagsStr(
		'LimitCompare',
		[
			['L', 'PARM_FLG', 0x40],
			['H', 'PARM_FLG', 0x80],
		]
	);

	# STDF TEST_FLG Bit 1: Test result is unreliable
	#.. no, if Alarm flag is U, ..
	# like RES_SCAL, numeric, should be '', 0 is valid!  REVISIT
	atdfToStdfFlagExpr(($gAtdfRD{'RESULT'} eq ''), 'TEST_FLG', 0x02);

	# STDF OPT_FLAG Bit 0: Invalid result scale factor
	# dcg commented out below... 0 is valid, ie Volts have scale of 0
	# ... REVISIT...
	atdfToStdfFlagExpr(($gAtdfRD{'RES_SCAL'} eq ''), 'OPT_FLAG', 0x01);
	# STDF OPT_FLAG Bit 2: No low specification
	atdfToStdfFlagExpr(($gAtdfRD{'LO_SPEC'} eq ''), 'OPT_FLAG', 0x04);
	# STDF OPT_FLAG Bit 3: No high specification
	atdfToStdfFlagExpr(($gAtdfRD{'HI_SPEC'} eq ''), 'OPT_FLAG', 0x08);
	# STDF OPT_FLAG Bit 4: Invalid low limit
	atdfToStdfFlagExpr(($gAtdfRD{'LO_LIMIT'} eq ''), 'OPT_FLAG', 0x10);
	# STDF OPT_FLAG Bit 5: Invalid high limit
	atdfToStdfFlagExpr(($gAtdfRD{'HI_LIMIT'} eq ''), 'OPT_FLAG', 0x20);

	$gStdfRD{'TEST_FLG'} = A2S_B1($gStdfRD{'TEST_FLG'});
	$gStdfRD{'PARM_FLG'} = A2S_B1($gStdfRD{'PARM_FLG'});
	$gStdfRD{'OPT_FLAG'} = A2S_B1($gStdfRD{'OPT_FLAG'});
};

# RDR()
#
# RDR:4,5,7

sub RDR
{
	my @atdfRetestBins = split(',', $gAtdfRD{'RetestBins'});
	$gStdfRD{'NUM_BINS'} = A2S_U2(scalar(@atdfRetestBins));
	$gStdfRD{'RTST_BIN'} = A2S_xTYPE(\&A2S_U2, \@atdfRetestBins);
};

# SBR()
#
# SBR:2|1|6|212|F|SHORT
# SBR:||1|1346|P|PASSED

sub SBR
{
	atdfToStdfHeadNumber();
	atdfToStdfSiteNumber();
	atdfToStdfFieldData('SBIN_NUM', 'U2');
	atdfToStdfFieldData('SBIN_CNT', 'U4');
	atdfToStdfFieldData('SBIN_PF',  'C1', SPC);
	atdfToStdfFieldData('SBIN_NAM', 'Cn');
};

# SDR()
#
# SDR:2|4|5,6,7,8|Delta Flex|D511||B101|17

sub SDR
{
	atdfToStdfHeadNumber();
	atdfToStdfFieldData('SITE_GRP', 'U1');
	my @atdfSiteArray = split(',', $gAtdfRD{'SiteArray'});
	$gStdfRD{'SITE_CNT'} = A2S_U1(scalar(@atdfSiteArray));
	$gStdfRD{'SITE_NUM'} = A2S_xTYPE(\&A2S_U1, \@atdfSiteArray);
	for my $keyL ('HAND', 'CARD', 'LOAD', 'DIB', 'CABL', 'CONT', 'LASR', 'EXTR')
	{
		for my $keyR ('TYP', 'ID')
		{
			atdfToStdfFieldData(join('_', $keyL, $keyR), 'Cn');
		};
	};
};

# TSR()
#
# TSR:2|2|600|Leakage|P|413|92|3||DC_TESTS|0.005|0.1
#  |7.2|1280.3|4329.5

sub TSR
{
	atdfToStdfHeadNumber();
	atdfToStdfSiteNumber();
	atdfToStdfFieldData('TEST_NUM', 'U4');
	atdfToStdfFieldData('TEST_NAM', 'Cn');
	atdfToStdfFieldData('TEST_TYP', 'C1', SPC);
	atdfToStdfFieldData('EXEC_CNT', 'U4', 4294967295);
	atdfToStdfFieldData('FAIL_CNT', 'U4', 4294967295);
	atdfToStdfFieldData('ALRM_CNT', 'U4', 4294967295);
	atdfToStdfFieldData('SEQ_NAME', 'Cn');
	atdfToStdfFieldData('TEST_LBL', 'Cn');
	atdfToStdfFieldData('TEST_TIM', 'R4');
	atdfToStdfFieldData('TEST_MIN', 'R4');
	atdfToStdfFieldData('TEST_MAX', 'R4');
	atdfToStdfFieldData('TST_SUMS', 'R4');
	atdfToStdfFieldData('TST_SQRS', 'R4');

	$gStdfRD{'OPT_FLAG'} = 0xC8;
	# STDF OPT_FLAG Bit 0: Invalid test minimum
	atdfToStdfFlagExpr(($gAtdfRD{'TEST_MIN'} eq ''), 'OPT_FLAG', 0x01);
	# STDF OPT_FLAG Bit 1: Invalid test maximum
	atdfToStdfFlagExpr(($gAtdfRD{'TEST_MAX'} eq ''), 'OPT_FLAG', 0x02);
	# STDF OPT_FLAG Bit 2: Invalid test maximum
	atdfToStdfFlagExpr(($gAtdfRD{'TEST_TIM'} eq ''), 'OPT_FLAG', 0x04);
	# STDF OPT_FLAG Bit 4: Invalid Test sums
	atdfToStdfFlagExpr(($gAtdfRD{'TST_SUMS'} eq ''), 'OPT_FLAG', 0x10);
	# STDF OPT_FLAG Bit 5: Invalid Test squares
	atdfToStdfFlagExpr(($gAtdfRD{'TST_SQRS'} eq ''), 'OPT_FLAG', 0x20);
	$gStdfRD{'OPT_FLAG'} = A2S_B1($gStdfRD{'OPT_FLAG'});
};

# WCR()
#
# WCR:D|R|D|5|.3|.25|1|23|19

sub WCR
{
	my $atdfFieldData = undef;
	$atdfFieldData = $gAtdfRD{'WF_FLAT'};
	if ($atdfFieldData !~ m/^[UDLR]$/i)
	{
		$atdfFieldData = SPC;
	};
	$gStdfRD{'WF_FLAT'} = A2S_C1($atdfFieldData);
	$atdfFieldData = $gAtdfRD{'POS_X'};
	if ($atdfFieldData !~ m/^[LR]$/i)
	{
		$atdfFieldData = SPC;
	};
	$gStdfRD{'POS_X'} = A2S_C1($atdfFieldData);
	$atdfFieldData = $gAtdfRD{'POS_Y'};
	if ($atdfFieldData !~ m/^[UD]$/i)
	{
		$atdfFieldData = SPC;
	};
	$gStdfRD{'POS_Y'} = A2S_C1($atdfFieldData);
	atdfToStdfFieldData('WAFR_SIZ', 'R4');
	atdfToStdfFieldData('DIE_HT', 'R4');
	atdfToStdfFieldData('DIE_WID', 'R4');
	$atdfFieldData = $gAtdfRD{'WF_UNITS'};
	if ($atdfFieldData !~ m/^[1234]$/)
	{
		$atdfFieldData = 0;
	};
	$gStdfRD{'WF_UNITS'} = A2S_U1($atdfFieldData);
	atdfToStdfFieldData('CENTER_X', 'I2', -32768);
	atdfToStdfFieldData('CENTER_Y', 'I2', -32768);
};

# WIR()
#
# WIR:1|8:23:02 23-JUL-1992|2

sub WIR
{
	atdfToStdfHeadNumber();
	$gStdfRD{'START_T'} = A2S_U4(dateTimeToSeconds($gAtdfRD{'START_T'}));
	atdfToStdfFieldData('SITE_GRP', 'U1', 255);
	atdfToStdfFieldData('WAFER_ID', 'Cn');
};

# WRR()
#
# WRR:1|11:02:42 23-JUL-1992|492|W01|3|102|214|2
#  |131|MOS-4|F54|S3-1|Glass buildup on prober
#  |Yield alarm on wafer W01

sub WRR
{
	atdfToStdfHeadNumber();
	$gStdfRD{'FINISH_T'} = A2S_U4(dateTimeToSeconds($gAtdfRD{'FINISH_T'}));
	atdfToStdfFieldData('PART_CNT', 'U4');
	atdfToStdfFieldData('WAFER_ID', 'Cn');
	atdfToStdfFieldData('SITE_GRP', 'U1', 255);
	atdfToStdfFieldData('RTST_CNT', 'U4', 4294967295);
	atdfToStdfFieldData('ABRT_CNT', 'U4', 4294967295);
	atdfToStdfFieldData('GOOD_CNT', 'U4', 4294967295);
	atdfToStdfFieldData('FUNC_CNT', 'U4', 4294967295);
	atdfToStdfFieldData('FABWF_ID', 'Cn');
	atdfToStdfFieldData('FRAME_ID', 'Cn');
	atdfToStdfFieldData('MASK_ID',  'Cn');
	atdfToStdfFieldData('USR_DESC', 'Cn');
	atdfToStdfFieldData('EXC_DESC', 'Cn');
};

######################################################################

=pod

=head1 SUPPORTED PLATFORMS

E<bull> Unix

E<bull> Windows.

=head1 REQUIRES

E<bull> Perl 5.6 or newer.

E<bull> Perl core module strict.

E<bull> Perl custom module TDF.pm.

=head1 SEE ALSO

E<bull> perl core and module documentation.

E<bull> STDF Specification V4 published by Teradyne, Inc.

E<bull> ATDF Specification V2 published by Teradyne, Inc.

=head1 AUTHOR

Michael Hackerott, michael.hackerott@mrhackerott.org

=head1 COPYRIGHT

Copyright E<copy> 2004-2005 Michael Hackerott. All rights reserved.

This program is free software; you can redistribute it and/r modify it
under the terms of either the GNU General Public License or the Artistic
License as specified in the Perl README file.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=head1 ACKNOWLEDGEMENT

The Standard Test Data Format (STDF) and ASCII Test Data Format (ATDF)
specifications are the original works of Teradyne Inc.

=head1 KNOWN BUGS

E<bull> None.

=head1 HISTORY

1.0.0 (200501011408) Michael Hackerott

E<bull>  Created program.

1.0.1 (200402080725) Michael Hackerott

E<bull> Moved about() subroutine to TDF.pm module.

1.0.2 (200402131512) Michael Hackerott

E<bull> Changed ATDF read logic to improve performance.

1.0.3 (200512030625) Michael Hackerott

E<bull> Added BEGIN block to add lib and ../lib to @INC.

1.0.4 (200512030659) Michael Hackerott

E<bull> Updated documentation.

E<bull> Fixed dbugEnabled() logic.

=cut

__END__
