# stdf4_to_atdf2.pl
# Copyright (C) 2004-2005 Michael Hackerott. All Rights Reserved
#
# modified 2006 David Gattrell - cygwin and linux, also significant digit tweaks
# modified 2011 David Gattrell - update to support STDF V4.2007
# modified 2012 David Gattrell - update to tolerate non-compliant IntegraFlex stdf files
# modified 2013 David Gattrell - S2A_U8 missing for some STDF V4.2007 support...
#                                MPR N1 were being read as U1, fixed
#                                DTR with \n now has space as first char of next line
# modified 2014 David Gattrell - closing loop of stdf->atdf->stdf integrity
# modified 2017 Haino - fix bug in FTR processing TEST_FLG
# modified 2017 Paul Robins - GDR re-enabled. Corrected GDR B0 (pad byte handling)
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
#	pod2html --infile=stdf4_to_atdf2.pl --outfile=stdf4_to_atdf2.html

=pod

=head1 NAME

stdf4_to_atdf2.pl - Converts an STDF version 4 file to an ATDF version 2
file

=head1 SYNTAX

	perl stdf4_to_atdf2.pl [OPTIONS] ARGUMENTS

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

		-w, --wrap
			Enables wrap mode which outputs the ATDF records as a
			maximum of 80 characters per file line.  Continuation
			lines begin with a space character. For example:

			00000000011111111112222222222333333333344444444445555555555666666666677777777778
			12345678901234567890123456789012345678901234567890123456789012345678901234567890
			MIR:char_dig_highv_0C|   MATRIX_MODE|char_ad1843_a580|joe_t|A585|
			 10:09:41 12-Sep-1995|11:46:14 12-Sep-1995|kan|E|1|1||||IMAGE V5.7 D1 081795||||

	ARGUMENTS

		StdfFileSpec
			The input STDF file specification.

		AtdfFileSpec
			The output ATDF file specification.

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

	perl stdf4_to_atdf2.pl -a
	perl stdf4_to_atdf2.pl --about

		Displays information about the program.

	perl stdf4_to_atdf2.pl -h
	perl stdf4_to_atdf2.pl --help

		Displays the help for the program.

	perl stdf4_to_atdf2.pl StdfFileSpec AtdfFileSpec

		Converts the STDF file specified by StdfFileSpec to the ATDF
		file specified by AtdfFileSpec.

	perl stdf4_to_atdf2.pl -l StdfFileSpec AtdfFileSpec
	perl stdf4_to_atdf2.pl --log StdfFileSpec AtdfFileSpec

		Converts the STDF file specified by StdfFileSpec to the ATDF
		file specified by AtdfFileSpec and outputs log information
		to standard output.

	perl stdf4_to_atdf2.pl -d StdfFileSpec AtdfFileSpec
	perl stdf4_to_atdf2.pl --debug StdfFileSpec AtdfFileSpec

		Converts the STDF file specified by StdfFileSpec to the ATDF
		file specified by AtdfFileSpec and outputs debug information
		to standard output.

	perl stdf4_to_atdf2.pl -l -d StdfFileSpec AtdfFileSpec
	perl stdf4_to_atdf2.pl --log --debug StdfFileSpec AtdfFileSpec

		Converts the STDF file specified by StdfFileSpec to the ATDF
		file specified by AtdfFileSpec and outputs log and debug
		information to standard output.

	NOTE: Use command line redirection to capture standard
	output to a file.

=head1 DESCRIPTION

This program converts an STDF Version 4 file to an ATDF Version 2 file.

=head1 BINARY (STDF) TO ASCII (ATDF) CONVERSION

This program implements the BINARY to ASCII conversion as defined in the
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

use POSIX;	# for ceil() and floor() functions

use TDF;

use strict;

######################################################################
# PROGRAM CONSTANT DECLARATIONS
######################################################################

# set the version number
my $VERSION = '3.2.3';

use constant TRUE	=> 1;
use constant FALSE	=> 0;

use constant EOL	=> "\n";
use constant Q1	=> "\'";
use constant Q2	=> "\"";
use constant SPC	=> "\ ";
use constant TAB	=> "\t";

# @gStdfBR indexes
use constant REC_LEN => 0;
use constant REC_TYP => 1;
use constant REC_SUB => 2;
use constant REC_DAT => 3;

my %STDF_TO_ATDF_RECORD_SUBREF = (
	'ATR' => \&ATR,
	'BPS' => \&BPS,
	'CNR' => \&CNR,		# v4.2007
	'DTR' => \&DTR,
	'EPS' => \&EPS,
	'FAR' => \&FAR,
	'FTR' => \&FTR,
	'GDR' => \&GDR,
	'HBR' => \&HBR,
	'MIR' => \&MIR,
	'MPR' => \&MPR,
	'MRR' => \&MRR,
	'NMR' => \&NMR,		# v4.2007
	'PCR' => \&PCR,
	'PGR' => \&PGR,
	'PIR' => \&PIR,
	'PLR' => \&PLR,
	'PMR' => \&PMR,
	'PRR' => \&PRR,
	'PSR' => \&PSR,		# v4.2007
	'PTR' => \&PTR,
	'RDR' => \&RDR,
	'SBR' => \&SBR,
	'SCR' => \&SCR,		# v4.2007
	'SDR' => \&SDR,
	'SSR' => \&SSR,		# v4.2007
	'STR' => \&STR,		# v4.2007
	'TSR' => \&TSR,
	'VUR' => \&VUR,		# v4.2007
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
	'wrap'	=> FALSE,
);
my $gStdfFileSpec = undef;
my $gAtdfFileSpec = undef;

my $gStdfCpuType = undef;

# STDF Binary Record
# $gStdfBR
#	[0]=REC_LEN
#	[1]=REC_TYP
#	[2]=REC_SUB
#	[3]=REC_DAT
my @gStdfBR = ();

# STDF File Byte Offset
my $gStdfFBO = undef;

# STDF Data Byte Offset
my $gStdfDBO = undef;

# STDF Record Name
my $gStdfRecordName = undef;

# STDF Record Data
my $gStdfRD = undef;
my @gStdfRD = ();
my %gStdfRD = ();
my %gStdfArrayIndex = ();

# ATDF Record Data
my %gAtdfRD = ();
my @gAtdfRD = ();

my %gAtdfDefaults = ();

######################################################################
# MAIN
######################################################################

# enable autoflush
$| = TRUE;

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
	elsif ($option =~ m/^\-(w|\-wrap)/)
	{
		$gOptions{'wrap'} = TRUE;
	};
};
dbugData([\%gOptions], ['*gOptions']);

# get the ATDF file specification
$gStdfFileSpec = shift(@ARGV);
if ($gStdfFileSpec eq '')
{
	fatal('STDF filename missing!  Usage: perl -f stdf*pl in.stdf out.atdf');
}
elsif (! -f $gStdfFileSpec)
{
	fatal(
		'STDF file does not exist:',
		$gStdfFileSpec
	);
};
dbugData([$gStdfFileSpec], ['*gStdfFileSpec']);

# get the ATDF file specification
$gAtdfFileSpec = shift(@ARGV);
if ($gAtdfFileSpec eq '')
{
	fatal('ATDF output filename missing!  Usage: perl -f stdf*pl in.stdf out.atdf');
};
dbugData([$gAtdfFileSpec], ['*gAtdfFileSpec']);

# open the STDF file for read as binary
if (! open(FILE_STDF, '<'.$gStdfFileSpec))
{
	fatal(
		'Failed to open for read STDF file:',
		$gStdfFileSpec
	);
};
binmode(FILE_STDF);

# open the ATDF file for write as ASCII
if (! open(FILE_ATDF, '>'.$gAtdfFileSpec))
{
	fatal(
		'Failed to open for write ATDF file:',
		$gAtdfFileSpec
	);
};

#
# READ THE FIRST RECORD IN THE STDF FILE AND TEST TO
# VALIDATE THAT IT IS AN FAR RECORD
#

# dcg - barnacle, sep/2012
# do an initial guess of the same type as IntegraFlex.
# if wrong but first record is FAR, it should correctly overwrite the guess (?)
stdfEndian(2);

# read the first STDF record
#stdfRecordRead(); -- comment out, flex sometimes has leading DTRs

# process the first STDF record
#FAR();	-- comment out, flex sometimes has leading DTRs

# print the ATDF record data to the ATDF file
# atdfRecordPrint(); -- comment out, flex sometimes has leading DTRs

#
# OUTPUT AN ATDF ATR RECORD
#

#
# PROCESS THE REST OF THE STDF FILE RECORDS
#

# set the STDF endian type
#stdfEndian($gStdfCpuType);-- dcg, comment out, do inside FAR function due to flex behaviour

# read the STDF file
while (stdfRecordRead())
{
	# initialize the global variables
	%gStdfRD = ();
	%gStdfArrayIndex = ();
	%gAtdfRD = ();

	# convert the STDF record data fields to ATDF
	$STDF_TO_ATDF_RECORD_SUBREF{$gStdfRecordName}->();
	dbugDataPurdy([\%gAtdfRD], ['*gAtdfRD']);

	# print the ATDF record data to the ATDF file
	atdfRecordPrint();
};

# close the ATDF file
close(FILE_ATDF);

# close the STDF file
close(FILE_STDF);

# set the timer
$gTimer[1] = time();

# logg conversion
my $atdfFileSize = (-s $gAtdfFileSpec);
my $stdfFileSize = (-s $gStdfFileSpec);
my $seconds = ($gTimer[1] - $gTimer[0]) || 1;
my $bytesPerSecond = $stdfFileSize / $seconds;
loggText(
	sprintf(
		'Converted STDF file to ATDF file in %d seconds (%.1f bytes/second).',
		$seconds,
		$bytesPerSecond
	),
	sprintf('STDF file: %s (%d bytes).', $gStdfFileSpec, $stdfFileSize),
	sprintf('ATDF file: %s (%d bytes).', $gAtdfFileSpec, $atdfFileSize),
);

# exit to unix without error
exit(0);

######################################################################
# SUBROUTINES
######################################################################

# about()

sub about
{
	TDF::about('stdf4_to_atdf2.pl', $VERSION)
};

# $atdfArrayCsv = atdfArrayCsv(\@array)

sub atdfArrayCsv
{
	my $raArray = shift();
	return(join(',', @{$raArray}));
};

# $atdfArrayCsv = atdfArrayCsvSortNA(\@array)

sub atdfArrayCsvSortNA
{
	my $raArray = shift();
	return(join(',', sort {$a <=> $b} (@{$raArray})));
};

# atdfRecordPrint()

sub atdfRecordPrint
{
	my @atdfRD = ();
	my $fldName = undef;
	my @atdfFieldNames = atdfRecordFieldNames($gStdfRecordName);
	for $fldName (@atdfFieldNames)
	{
		dbugData([$gAtdfRD{$fldName}], ["*gAtdfRD{$fldName}"]);
		push(@atdfRD, $gAtdfRD{$fldName});
	};
	dbugData([\@atdfRD], ['*atdfRecordPrint::atdfRD']);

	# output the ATDF record
	while ((scalar(@atdfRD) > 0) && ($atdfRD[-1] eq ''))
	{
		pop(@atdfRD);
	};
	if (($gStdfRecordName eq 'EPS') || (scalar(@atdfRD) > 0))
	{
		my $atdfRecord = undef;
		if ($gOptions{'wrap'})
		{
			$atdfRecord = atdfRecordWrap($gStdfRecordName, \@atdfRD);
		}
		else
		{
			$atdfRecord = $gStdfRecordName.':'.join('|', @atdfRD).EOL;
		};
		print FILE_ATDF $atdfRecord;
	};
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

# dbugSub($subname, @_)

sub dbugSub
{
	dbugEnabled() && TDF::dbugSub(@_);
};

# dbugText(@text)

sub dbugText
{
	dbugEnabled() && TDF::dbugText(@_);
};

# help()

sub help
{
	print <<"END_HELP";
NAME

	stdf4_to_atdf2.pl - Converts an STDF version 4 file to an ATDF
	version 2 file

SYNTAX

	perl stdf4_to_atdf2.pl [OPTIONS] ARGUMENTS

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

		-w, --wrap
			Enables wrap mode which outputs the ATDF records as a
			maximum of 80 characters per file line.  Continuation
			lines begin with a space character.

	ARGUMENTS

		StdfFileSpec
			The input STDF file specification.

		AtdfFileSpec
			The output ATDF file specification.

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

SEE ALSO

	The program documentation for additional information.
END_HELP

	# exit to unix without error
	exit(0);
};

# stdfFieldRead($fieldName, $dataType, $dataRept)
#
# NOTE: Works for STDF *n data types too!

sub stdfFieldRead
{
	dbugSub('stdfFieldRead', @_);
	my $fieldName = shift();
	my $dataType = shift();
	my $dataRept = shift();

	my $bindat = undef;

	# test for Nibbles = 1/2 Bytes  (dcg, jun2013)
	my $datacode = substr($dataType, 0, 1);
	if ($datacode eq 'N')
	{
		$dataRept = ceil($dataRept/2.0)
	}

	# repeat the read
	while($dataRept > 0)
	{
		my $binlen = substr($dataType, 1, 1);

		# test for variable length fields
		if ($binlen eq 'n')
		{
			my $stdfDataTypeCode = stdfRecordTypeToDataTypeCode($dataType);
			$binlen = stdfRead(substr($stdfDataTypeCode, 1, 1));
			$bindat .= $binlen;
			# convert the STDF binary length to ASCII
			$binlen = S2A_do($stdfDataTypeCode, $binlen);
			# test for non-byte variable length fields
			if ($dataType eq 'Dn')
			{
				# convert bits to bytes
				$binlen = int($binlen / 8) + (($binlen % 8) > 0);
			};
		};

		# read the binary field data
		$bindat .= stdfRead($binlen);

		# decrement the repeat count
		$dataRept -= 1;
	};

	# set the STDF field binary data
	$gStdfRD{$fieldName} = $bindat;
};

# PRIVATE $bindat = stdfRead($binlen)

sub stdfRead
{
	my $binlen = shift() || return(undef);
	my $bindat = substr(
		$gStdfBR[REC_DAT],
		$gStdfDBO,
		$binlen
	);
	$gStdfDBO += $binlen;
	dbugDataBin('$stdfRead::bindat', $bindat);
	return($bindat);
};

# stdfRecordRead()

sub stdfRecordRead
{
	# declare local variables
	my $stdfRecLen = undef;
	my $stdfRecTyp = undef;
	my $stdfRecSub = undef;

	# initialize the STDF global variables
	@gStdfBR = ();		# binary record

	# get the STDF file byte offset
	$gStdfFBO = tell(FILE_STDF);
	dbugData([$gStdfFBO], ['*gStdfFBO']);

	# read REC_LEN U*2
	my $status = read(FILE_STDF, $gStdfBR[REC_LEN], 2);

	# test the read status
	# undef	error
	# 0		eof
	# >0		number of bytes read
	if ($status > 0)
	{
		# unpack REC_LEN U*2
		$stdfRecLen = S2A_U2($gStdfBR[REC_LEN]);

		# read REC_TYP U*1
		read(FILE_STDF, $gStdfBR[REC_TYP], 1);
		# unpack REC_TYP U*1
		$stdfRecTyp = S2A_U1($gStdfBR[REC_TYP]);

		# read REC_SUB U*1
		read(FILE_STDF, $gStdfBR[REC_SUB], 1);
		# unpack REC_SUB U*1
		$stdfRecSub = S2A_U1($gStdfBR[REC_SUB]);

		# set the STDF record name
		$gStdfRecordName = stdfRecordTypeCodesToName($stdfRecTyp, $stdfRecSub);
		dbugData([$gStdfRecordName], ['*gStdfRN']);

		# test the STDF record name
		if ($gStdfRecordName eq '')
		{
			fatal(
				'Unknown header',
				'record type', $stdfRecTyp,
				'and/or',
				'record sub-type', $stdfRecSub,
				'at byte offset', $gStdfFBO,
				'in STDF file', $gStdfFileSpec
			);
		}
		# test for STDF.FAR record
		elsif ($gStdfRecordName eq 'FAR')
		{
			$stdfRecLen = 2;
		};

		# read the STDF record data
		read(FILE_STDF, $gStdfBR[REC_DAT], $stdfRecLen);

		# set the STDF record data byte offset
		$gStdfDBO = 0;

		dbugDataBin('$gStdfBR[REC_LEN]', $gStdfBR[REC_LEN]);
		dbugDataBin('$gStdfBR[REC_TYP]', $gStdfBR[REC_TYP]);
		dbugDataBin('$gStdfBR[REC_SUB]', $gStdfBR[REC_SUB]);
		dbugDataBin('$gStdfBR[REC_DAT]', $gStdfBR[REC_DAT]);
	};

	# return the status
	return($status);
};

# stdfToAtdfField($fieldName, $dataType[, $invalid])

sub stdfToAtdfField
{
	dbugSub('stdfToAtdfField', @_);
	my $fieldName = shift();
	my $dataType = shift();
	my $invalid = shift();
	# read the STDF field
	stdfFieldRead($fieldName, $dataType, 1);
	# set the ATDF field
	my $r = \&{'S2A_'.$dataType};
	$gAtdfRD{$fieldName} = $r->($gStdfRD{$fieldName});
	dbugData([$gAtdfRD{$fieldName}], ['*gAtdfRD{'.$fieldName.'}']);
	# test for invalid value
	if ($gAtdfRD{$fieldName} eq $invalid)
	{
		$gAtdfRD{$fieldName} = '';
	};
};

# stdfToAtdfFieldArray($stdfFieldName, $stdfDataType, $atdfArrayName, $atdfArraySize)

sub stdfToAtdfFieldArray
{
	dbugSub('stdfToAtdfFieldArray', @_);
	my $stdfFieldName = shift();
	my $stdfDataType = shift();
	my $atdfArrayName = shift();
	my $atdfArraySize = shift();
	stdfFieldRead($stdfFieldName, $stdfDataType, $atdfArraySize);
	$gAtdfRD{$atdfArrayName} = '';
	if ($atdfArraySize > 0)
	{
		my @atdfArray = S2A_xTYPE(
			$atdfArraySize,
			$stdfDataType,
			$gStdfRD{$stdfFieldName}
		);
		$gAtdfRD{$atdfArrayName} = atdfArrayCsv(\@atdfArray);
	};
	dbugData([$gAtdfRD{$atdfArrayName}], ['*gAtdfRD{'.$atdfArrayName.'}']);
};

# stdfToAtdfFieldDateTime($fieldName)

sub stdfToAtdfFieldDateTime
{
	dbugSub('stdfToAtdfFieldDateTime', @_);
	my $fieldName = shift();
	# set the ATDF field
	stdfToAtdfField($fieldName, 'U4');
	# convert the ATDF field seconds to date and time
	$gAtdfRD{$fieldName} = secondsToDateTime($gAtdfRD{$fieldName});
	dbugData([$gAtdfRD{$fieldName}], ['*gAtdfRD{'.$fieldName.'}']);
};

# stdfToAtdfFieldReal($fieldName)

sub stdfToAtdfFieldReal
{
	dbugSub('stdfToAtdfFieldReal', @_);
	my $fieldName = shift();
	# set the ATDF field
	stdfToAtdfField($fieldName, 'R4');
	# format the ATDF field
	atdfFormatReal(\$gAtdfRD{$fieldName});
	dbugData([$gAtdfRD{$fieldName}], ['*gAtdfRD{'.$fieldName.'}']);
};

# stdfToAtdfHeadNumber()

sub stdfToAtdfHeadNumber
{
	dbugSub('stdfToAtdfHeadNumber', @_);
	stdfToAtdfField('HEAD_NUM', 'U1');
	if ($gAtdfRD{'HEAD_NUM'} == 255)
	{
		$gAtdfRD{'HEAD_NUM'} = '';
	};
};

# stdfToAtdfSiteNumber()

sub stdfToAtdfSiteNumber
{
	dbugSub('stdfToAtdfSiteNumber', @_);
	stdfToAtdfField('SITE_NUM', 'U1');
	if ($gAtdfRD{'SITE_NUM'} == 255)
	{
		$gAtdfRD{'SITE_NUM'} = '';
	};
};

#=====================================================================
# STDF TO ATDF RECORD SUBROUTINES
#=====================================================================

# ATR()

sub ATR
{
	stdfToAtdfFieldDateTime('MOD_TIM');
	stdfToAtdfField('CMD_LINE', 'Cn');
};

# BPS()

sub BPS
{
	stdfToAtdfField('SEQ_NAME', 'Cn');
};

# CNR()

sub CNR
{
	stdfToAtdfField('CHN_NUM', 'U2');
	stdfToAtdfField('BIT_POS', 'U2');
	stdfToAtdfField('CELL_NAM', 'Sn');
};

# DTR()

sub DTR
{
	stdfToAtdfField('TEST_DAT', 'Cn');
};

# EPS()

sub EPS
{
};

# FAR()

sub FAR
{
	# this this is an FAR record
	if ($gStdfRecordName ne 'FAR')
	{
		fatal('The first record in the STDF file is not an FAR record!');
	};

	# unpack the STDF record data into fields
	@gStdfRD = unpack('CC', $gStdfBR[REC_DAT]);
	dbugData([\@gStdfRD], ['*gStdfRD']);

	# set the STDF CPU_TYPE
	$gStdfCpuType = $gStdfRD[0];
	dbugData([$gStdfCpuType], ['*gStdfCpuType']);

	# test the STDF CPU TYPE
	if ($gStdfCpuType eq '')
	{
		fatal(
			'Unknown FAR.CPU_TYPE:',
			$gStdfRD[0]
		);
	};

	# test the STDF STDF_VER record data
	if ($gStdfRD[1] ne '4')
	{
		fatal(
			'Invalid STDF Version:',
			$gStdfRD[1]
		);
	};

	# set the ATDF record data
	%gAtdfRD = (
		'DataFileType' => 'A',
		'STDF_VER' => '4',
		'AtdfVersion' => '2',
		'ScalingFlag' => undef,
	);

	# dcg added.. will this work when stuck in here?
	stdfEndian($gStdfCpuType);
};

# FTR()

sub FTR
{
	stdfToAtdfField('TEST_NUM', 'U4');
	stdfToAtdfHeadNumber();
	stdfToAtdfSiteNumber();
	stdfToAtdfField('TEST_FLG', 'B1');
	stdfToAtdfField('OPT_FLG',  'B1');
	stdfToAtdfField('CYCL_CNT', 'U4');
	stdfToAtdfField('REL_VADR', 'U4');
	stdfToAtdfField('REPT_CNT', 'U4');
	stdfToAtdfField('NUM_FAIL', 'U4');
	stdfToAtdfField('XFAIL_AD', 'I4');
	stdfToAtdfField('YFAIL_AD', 'I4');
	stdfToAtdfField('VECT_OFF', 'I2');
	stdfToAtdfField('RTN_ICNT', 'U2'); # j
	stdfToAtdfField('PGM_ICNT', 'U2'); # k
	stdfToAtdfFieldArray('RTN_INDX', 'U2', 'ReturnIndexes', $gAtdfRD{'RTN_ICNT'});
	stdfToAtdfFieldArray('RTN_STAT', 'N1', 'ReturnStates', $gAtdfRD{'RTN_ICNT'});
	stdfToAtdfFieldArray('PGM_INDX', 'U2', 'ProgIndexes', $gAtdfRD{'PGM_ICNT'});
	stdfToAtdfFieldArray('PGM_STAT', 'N1', 'ProgStates', $gAtdfRD{'PGM_ICNT'});
	stdfFieldRead('FAIL_PIN', 'Dn', 1);
	my @atdfFailingPins = S2A_Dn($gStdfRD{'FAIL_PIN'});
	$gAtdfRD{'FailingPins'} = atdfArrayCsv(\@atdfFailingPins);
	stdfToAtdfField('VECT_NAM', 'Cn');
	stdfToAtdfField('TIME_SET', 'Cn');
	stdfToAtdfField('OP_CODE',  'Cn');
	stdfToAtdfField('TEST_TXT', 'Cn');
	stdfToAtdfField('ALARM_ID', 'Cn');
	stdfToAtdfField('PROG_TXT', 'Cn');
	stdfToAtdfField('RSLT_TXT', 'Cn');
	stdfToAtdfField('PATG_NUM', 'U1');
	stdfFieldRead('SPIN_MAP', 'Dn', 1);
	my @atdfComparators = S2A_Dn($gStdfRD{'SPIN_MAP'});
	$gAtdfRD{'Comparators'} = atdfArrayCsv(\@atdfComparators);

	# set the functional Pass/Fail Flag
	#
	#	STDF
	#
	#		TEST_FLG bit 6
	#			0=	Pass/Fail flag (bit 7) is valid
	#			1=	Test completed with no pass/fail indication
	#		TEST_FLG bit 7
	#			0=	Test passed
	#			1=	Test failed
	#
	#	ATDF
	#
	#		F= Test failed
	#		P= Test passed standard limits
	#		empty= Test completed without pass/fail
	#
	#	STDF to ATDF map
	#
	#		TEST_FLG
	#		b7	b6	Indicates
	#		0	0	P
	#		0	1	empty
	#		1	0	F
	#		1	1	empty
	$gAtdfRD{'PassFailFlag'} = 'P';
	if    (bitand($gAtdfRD{'TEST_FLG'}, 0x40))
	{
		$gAtdfRD{'PassFailFlag'} = '';
	}
	elsif (bitand($gAtdfRD{'TEST_FLG'}, 0x80))
	{
		$gAtdfRD{'PassFailFlag'} = 'F';
	}
	elsif (bitand($gAtdfRD{'TEST_FLG'}, 0xC0))
	{
		$gAtdfRD{'PassFailFlag'} = '';
	};

	# set the functional Alarm Flags
	#
	#	STDF
	#
	#		TEST_FLG bit 0
	#			0= No alarm
	#			1= Alarm detected
	#		TEST_FLG bit 2
	#			0= Result is reliable
	#			1= Result is not reliable
	#		TEST_FLG bit 3
	#			0= Timeout
	#			1= No timeout
	#		TEST_FLG bit 4
	#			0= Test executed
	#			1= Test not executed
	#		TEST_FLG bit 5
	#			0= Test not aborted
	#			1= Test aborted
	#
	#	ATDF
	#
	#		A= Alarm detected
	#		N= Test was not executed
	#		T= Time-out occurred
	#		U= Test result is unreliable
	#		X= Test aborted
	#
	#	STDF to ATDF map

	$gAtdfRD{'AlarmFlags'} = '';
	if    (bitand($gAtdfRD{'TEST_FLG'}, 0x01))
	{
		$gAtdfRD{'AlarmFlags'} .= 'A';
	}
	elsif (bitand($gAtdfRD{'TEST_FLG'}, 0x02))
	{
		$gAtdfRD{'AlarmFlags'} .= 'U';
	}
	elsif (bitand($gAtdfRD{'TEST_FLG'}, 0x04))
	{
		$gAtdfRD{'AlarmFlags'} .= 'T';
	}
	elsif (bitand($gAtdfRD{'TEST_FLG'}, 0x08))
	{
		$gAtdfRD{'AlarmFlags'} .= 'N';
	}
	elsif (bitand($gAtdfRD{'TEST_FLG'}, 0x10))
	{
		$gAtdfRD{'AlarmFlags'} .= 'X';
	};

	# Use OPT_FLAG to set some fields to empty...
	#
	# b0 = 1 -> CYCL_CNT not valid
	# b1 = 1 -> REL_VADR not valid
	# b2 = 1 -> REPT_CNT not valid
	# b3 = 1 -> NUM_FAIL not valid
	# b4 = 1 -> XFAIL_AD and YVALID_AD not valid
	# b5 = 1 -> VECT_OFF not valid
	#
	if (bitand($gAtdfRD{'OPT_FLG'}, 0x01))
	{
		# a5 still populates this with valid #, even when flag says invalid!
		# so keep this information
		#$gAtdfRD{'CYCL_CNT'} = '';
	};
	if (bitand($gAtdfRD{'OPT_FLG'}, 0x02))
	{
		$gAtdfRD{'REL_VADR'} = '';
	};
	if (bitand($gAtdfRD{'OPT_FLG'}, 0x04))
	{
		$gAtdfRD{'REPT_CNT'} = '';
	};
	if (bitand($gAtdfRD{'OPT_FLG'}, 0x08))
	{
		$gAtdfRD{'NUM_FAIL'} = '';
	};
	if (bitand($gAtdfRD{'OPT_FLG'}, 0x10))
	{
		$gAtdfRD{'XFAIL_AD'} = '';
		$gAtdfRD{'YFAIL_AD'} = '';
	};
	if (bitand($gAtdfRD{'OPT_FLG'}, 0x20))
	{
		$gAtdfRD{'VECT_OFF'} = '';
	};

};

# GDR()

sub GDR
{
	stdfFieldRead('FLD_CNT', 'U2', 1);
	my $atdfFldCnt = S2A_U2($gStdfRD{'FLD_CNT'});
	my $dataTypeCode = undef;
	my %dataTypeCodeMap = (
		0  => 'B0',
		1  => 'U1',
		2  => 'U2',
		3  => 'U4',
		4  => 'I1',
		5  => 'I2',
		6  => 'I4',
		7  => 'R4',
		8  => 'R8',
		10 => 'Cn',
		11 => 'Bn',
		12 => 'Dn',
		13 => 'N1',
	);
	my %dataTypeToFmt = (
		'U1' => 'U',
		'U2' => 'M',
		'U4' => 'B',
		'I1' => 'I',
		'I2' => 'S',
		'I4' => 'L',
		'R4' => 'F',
		'R8' => 'D',
		'Cn' => 'T',
		'Bn' => 'X',
		'Dn' => 'Y',
		'N1' => 'N',
	);
	my @atdfGenData = ();
	my $dataType = undef;
	my $r = undef;
	for (1 .. $atdfFldCnt)
	{
		$dataTypeCode = S2A_U1(stdfRead(1));
		dbugData([$dataTypeCode], ['*GDR::dataTypeCode']);
		$dataType = $dataTypeCodeMap{$dataTypeCode};
		dbugData([$dataType], ['*GDR::dataType']);
		if ($dataType eq '')
		{
			fatal(
				'GDR record contains invalid data type code:',
				$dataTypeCode
			);
		};
		if ($dataType eq 'B0')   # 0 pad byte,used for alignment of MBSLFD values
		{
		#	next;
			$dataTypeCode = S2A_U1(stdfRead(1));
			dbugData([$dataTypeCode], ['*GDR::dataTypeCode']);
			$dataType = $dataTypeCodeMap{$dataTypeCode};
			dbugData([$dataType], ['*GDR::dataType']);
			if ($dataType eq '')
			{
				fatal(
					'GDR record contains invalid data type code:',
					$dataTypeCode
				);
			};
		};
		#if ($dataType =~ m/^(U2|U4|I2|I4|R4|R8)$/) # MBSLFD
		#{
		#	stdfRead(1); # read B*0 byte
		#};
		stdfFieldRead('GEN_DATA', $dataType, 1);
		$r = \&{'S2A_'.$dataType};
		push(
			@atdfGenData,
			$dataTypeToFmt{$dataType}.$r->($gStdfRD{'GEN_DATA'})
		);
	};
	$gAtdfRD{'GenericData'} = join("\|", @atdfGenData);
};

# HBR()

sub HBR
{
	stdfToAtdfHeadNumber();
	stdfToAtdfSiteNumber();
	stdfToAtdfField('HBIN_NUM', 'U2');
	stdfToAtdfField('HBIN_CNT', 'U4');
	stdfToAtdfField('HBIN_PF',  'C1', SPC);
	stdfToAtdfField('HBIN_NAM', 'Cn');
};

# MIR()

sub MIR
{
	stdfToAtdfFieldDateTime('SETUP_T');
	stdfToAtdfFieldDateTime('START_T');
	stdfToAtdfField('STAT_NUM', 'U1');
	stdfToAtdfField('MODE_COD', 'C1');
	stdfToAtdfField('RTST_COD', 'C1');
	stdfToAtdfField('PROT_COD', 'C1');
	stdfToAtdfField('BURN_TIM', 'U2');
	stdfToAtdfField('CMOD_COD', 'C1');
	stdfToAtdfField('LOT_ID',   'Cn');
	stdfToAtdfField('PART_TYP', 'Cn');
	stdfToAtdfField('NODE_NAM', 'Cn');
	stdfToAtdfField('TSTR_TYP', 'Cn');
	stdfToAtdfField('JOB_NAM',  'Cn');
	stdfToAtdfField('JOB_REV',  'Cn');
	stdfToAtdfField('SBLOT_ID', 'Cn');
	stdfToAtdfField('OPER_NAM', 'Cn');
	stdfToAtdfField('EXEC_TYP', 'Cn');
	stdfToAtdfField('EXEC_VER', 'Cn');
	stdfToAtdfField('TEST_COD', 'Cn');
	stdfToAtdfField('TST_TEMP', 'Cn');
	stdfToAtdfField('USER_TXT', 'Cn');
	stdfToAtdfField('AUX_FILE', 'Cn');
	stdfToAtdfField('PKG_TYP',  'Cn');
	stdfToAtdfField('FAMLY_ID', 'Cn');
	stdfToAtdfField('DATE_COD', 'Cn');
	stdfToAtdfField('FACIL_ID', 'Cn');
	stdfToAtdfField('FLOOR_ID', 'Cn');
	stdfToAtdfField('PROC_ID',  'Cn');
	stdfToAtdfField('OPER_FRQ', 'Cn');
	stdfToAtdfField('SPEC_NAM', 'Cn');
	stdfToAtdfField('SPEC_VER', 'Cn');
	stdfToAtdfField('FLOW_ID',  'Cn');
	stdfToAtdfField('SETUP_ID', 'Cn');
	stdfToAtdfField('DSGN_REV', 'Cn');
	stdfToAtdfField('ENG_ID',   'Cn');
	stdfToAtdfField('ROM_COD',  'Cn');
	stdfToAtdfField('SERL_NUM', 'Cn');
	stdfToAtdfField('SUPR_NAM', 'Cn');
};

# MPR()

sub MPR
{
	stdfToAtdfField('TEST_NUM', 'U4');
	stdfToAtdfHeadNumber();
	stdfToAtdfSiteNumber();
	stdfToAtdfField('TEST_FLG', 'B1');
	stdfToAtdfField('PARM_FLG', 'B1');
	stdfToAtdfField('RTN_ICNT', 'U2');
	stdfToAtdfField('RSLT_CNT', 'U2');
	stdfToAtdfFieldArray('RTN_STAT', 'N1', 'StatesArray', $gAtdfRD{'RTN_ICNT'});
	stdfToAtdfFieldArray('RTN_RSLT', 'R4', 'ResultsArray', $gAtdfRD{'RSLT_CNT'});
	stdfToAtdfField('TEST_TXT', 'Cn');
	stdfToAtdfField('ALARM_ID', 'Cn');
	stdfToAtdfField('OPT_FLG', 'B1');
	stdfToAtdfField('RES_SCAL', 'I1');
	stdfToAtdfField('LLM_SCAL', 'I1');
	stdfToAtdfField('HLM_SCAL', 'I1');
	stdfToAtdfFieldReal('LO_LIMIT');
	stdfToAtdfFieldReal('HI_LIMIT');
	stdfToAtdfField('START_IN', 'R4');
	stdfToAtdfField('INCR_IN', 'R4');
	stdfToAtdfFieldArray('RTN_INDX', 'U2', 'IndexArray', $gAtdfRD{'RTN_ICNT'});
	stdfToAtdfField('UNITS', 'Cn');
	stdfToAtdfField('UNITS_IN', 'Cn');
	stdfToAtdfField('C_RESFMT', 'Cn');
	stdfToAtdfField('C_LLMFMT', 'Cn');
	stdfToAtdfField('C_HLMFMT', 'Cn');
	stdfToAtdfFieldReal('LO_SPEC');
	stdfToAtdfFieldReal('HI_SPEC');

	# set the parametric Pass/Fail Flag
	#
	#	STDF
	#
	#		TEST_FLG bit 6
	#			0=	Pass/Fail flag (bit 7) is valid
	#			1=	Test completed with no pass/fail indication
	#		TEST_FLG bit 7
	#			0=	Test passed
	#			1=	Test failed
	#		PARM_FLG bit 5
	#			0=	Test failed or test passed standard limits
	#			1=	Test passed alternate limits
	#
	#	ATDF
	#
	#		     A= Test passed alternate limits
	#		     F= Test failed
	#		     P= Test passed standard limits
	#		 empty= Test completed without pass/fail
	#
	#	STDF to ATDF map
	#
	#		PARM_FLG	TEST_FLG	PassFailFlag
	#	bit	76543210	76543210
	#		--------	--------	------------
	#		..0....	00....	P
	#		..0....	01....	empty
	#		..0....	10....	F
	#		..0....	11....	empty
	#		..1....	00....	A
	#		..1....	01....	empty
	#		..1....	10....	empty
	#		..1....	11....	empty

	dbugData([$gAtdfRD{'PARM_FLG'}], ["*PTR::gAtdfRD{'PARM_FLG'}"]);
	dbugData([$gAtdfRD{'TEST_FLG'}], ["*PTR::gAtdfRD{'TEST_FLG'}"]);

	my $testFlgBit6 = bitand($gAtdfRD{'TEST_FLG'}, 0x40);
	dbugData([$testFlgBit6], ['*PTR::testFlgBit6']);
	my $testFlgBit7 = bitand($gAtdfRD{'TEST_FLG'}, 0x80);
	dbugData([$testFlgBit7], ['*PTR::testFlgBit7']);

	$gAtdfRD{'PassFailFlag'} = '';
	if (bitand($gAtdfRD{'PARM_FLG'}, 0x20) == 0)
	{
		if    (($testFlgBit7 == 0) && ($testFlgBit6 == 0))
		{
			$gAtdfRD{'PassFailFlag'} = 'P';
		}
		elsif (($testFlgBit7 == 1) && ($testFlgBit6 == 0))
		{
			$gAtdfRD{'PassFailFlag'} = 'F';
		};
	}
	else
	{
		if    (($testFlgBit7 == 0) && ($testFlgBit6 == 0))
		{
			$gAtdfRD{'PassFailFlag'} = 'A';
		};
	};

	# set the parametric Alarm Flags
	#
	#	STDF
	#
	#		TEST_FLG bit 0
	#			0= No alarm
	#			1= Alarm detected
	#		TEST_FLG bit 2
	#			0= Result is reliable
	#			1= Result is NOT reliable
	#		TEST_FLG bit 3
	#			0= Timeout
	#			1= No timeout
	#		TEST_FLG bit 4
	#			0= Test executed
	#			1= Test not executed
	#		TEST_FLG bit 5
	#			0= Test not aborted
	#			1= Test aborted
	#
	#		PARM_FLG bit 0
	#			0= No scale error
	#			1= Scale error
	#		PARM_FLG bit 1
	#			0= No drift error
	#			1= Drift error
	#		PARM_FLG bit 2
	#			0= No oscillation
	#			1= Oscillation detected
	#		PARM_FLG bit 3
	#			0= Measured value not high
	#			1= Measured value higher than high test limit
	#		PARM_FLG bit 4
	#			0= Measured value not low
	#			1= Measured value lower than low test limit
	#
	#	ATDF
	#
	#		A= Alarm detected
	#		U= Test result is unreliable
	#		T= Time-out occurred
	#		N= Test was not executed
	#		X= Test aborted
	#		S= Scale error
	#		D= Drift error
	#		O= Oscillation detected
	#		H= Measured value higher than high test limit
	#		L= Measured value lower than low test limit
	#
	#	STDF to ATDF map
	#
	#		TEST_FLG	PARM_FLG
	#	Bit:	76543210	76543210
	#		..XNTU.A	...LHODS

	dbugData([$gAtdfRD{'TEST_FLG'}], ["*PTR::gAtdfRD{'TEST_FLG'}"]);
	dbugData([$gAtdfRD{'PARM_FLG'}], ["*PTR::gAtdfRD{'PARM_FLG'}"]);

	$gAtdfRD{'AlarmFlags'} = '';
	if (bitand($gAtdfRD{'TEST_FLG'}, 0x01))
	{
		$gAtdfRD{'AlarmFlags'} .= 'A';
	};
	if (bitand($gAtdfRD{'TEST_FLG'}, 0x04))
	{
		$gAtdfRD{'AlarmFlags'} .= 'U';
	};
	if (bitand($gAtdfRD{'TEST_FLG'}, 0x08))
	{
		$gAtdfRD{'AlarmFlags'} .= 'T';
	};
	if (bitand($gAtdfRD{'TEST_FLG'}, 0x10))
	{
		$gAtdfRD{'AlarmFlags'} .= 'N';
	};
	if (bitand($gAtdfRD{'TEST_FLG'}, 0x20))
	{
		$gAtdfRD{'AlarmFlags'} .= 'X';
	};
	if (bitand($gAtdfRD{'PARM_FLG'}, 0x01))
	{
		$gAtdfRD{'AlarmFlags'} .= 'S';
	};
	if (bitand($gAtdfRD{'PARM_FLG'}, 0x02))
	{
		$gAtdfRD{'AlarmFlags'} .= 'D';
	};
	if (bitand($gAtdfRD{'PARM_FLG'}, 0x04))
	{
		$gAtdfRD{'AlarmFlags'} .= 'O';
	};
	if (bitand($gAtdfRD{'PARM_FLG'}, 0x08))
	{
		$gAtdfRD{'AlarmFlags'} .= 'H';
	};
	if (bitand($gAtdfRD{'PARM_FLG'}, 0x10))
	{
		$gAtdfRD{'AlarmFlags'} .= 'L';
	};

	# set the parametric Limit Compare Flags
	#
	#	STDF
	#
	#		PARM_FLG bit 6
	#			0= if result equals low limit then result is "fail"
	#			1= if result equals low limit then result is "pass"
	#		PARM_FLG bit 7
	#			0= if result equals high limit then result is "fail"
	#			1= if result equals high limit then result is "pass"
	#
	#	ATDF
	#
	#		L= Low limit comparison was >=
	#		H= High limit comparison was <=
	#
	#	STDF to ATDF map
	#
	#		PARM_FLG
	#	Bit:	76543210
	#		HL......

	dbugData([$gAtdfRD{'PARM_FLG'}], ["*PTR::gAtdfRD{'PARM_FLG'}"]);

	$gAtdfRD{'LimitCompare'} = '';
	if (bitand($gAtdfRD{'PARM_FLG'}, 0x40))
	{
		$gAtdfRD{'LimitCompare'} .= 'L';
	};
	if (bitand($gAtdfRD{'PARM_FLG'}, 0x80))
	{
		$gAtdfRD{'LimitCompare'} .= 'H';
	};
};

# MRR()

sub MRR
{
	stdfToAtdfFieldDateTime('FINISH_T');
	stdfToAtdfField('DISP_COD',  'C1', SPC);
	stdfToAtdfField('USR_DESC',  'Cn');
	stdfToAtdfField('EXC_DESC',  'Cn');
};

# NMR()

sub NMR
{
	stdfToAtdfField('REC_INDX', 'U1');
	stdfToAtdfField('REC_TOT', 'U1');
	stdfToAtdfField('TOTM_CNT', 'U2');
	stdfToAtdfField('LOCM_CNT', 'U2');	

	stdfToAtdfFieldArray('PMR_INDX', 'U2', 'IndexesArray', $gAtdfRD{'LOCM_CNT'});
	stdfToAtdfFieldArray('ATPG_NAM', 'Cn', 'NamesArray', $gAtdfRD{'LOCM_CNT'});

};

# PCR()

sub PCR
{
	stdfToAtdfHeadNumber();
	stdfToAtdfSiteNumber();
	stdfToAtdfField('PART_CNT', 'U4');
	stdfToAtdfField('RTST_CNT', 'U4', 4294967295);
	stdfToAtdfField('ABRT_CNT', 'U4', 4294967295);
	stdfToAtdfField('GOOD_CNT', 'U4', 4294967295);
	stdfToAtdfField('FUNC_CNT', 'U4', 4294967295);
};

# PGR()

sub PGR
{
	stdfToAtdfField('GRP_INDX', 'U2');
	stdfToAtdfField('GRP_NAM',  'Cn');
	stdfToAtdfField('INDX_CNT', 'U2');
	stdfToAtdfFieldArray('PMR_INDX', 'U2', 'IndexArray', $gAtdfRD{'INDX_CNT'});
};

# PIR()

sub PIR
{
	stdfToAtdfHeadNumber();
	stdfToAtdfSiteNumber();
};

# PLR()
#
# PLR:2,3,6|20,20,21|H,H,H|H,L,L/H,H,H/L,L,L
#  |1,0,M/1,0,H/M,L,H

sub PLR
{
	return;

	stdfToAtdfField('GRP_CNT', 'U2');
	my $k = $gAtdfRD{'GRP_CNT'};
	stdfToAtdfFieldArray('GRP_INDX', 'U2', 'IndexArray', $k);
	stdfToAtdfFieldArray('GRP_MODE', 'U2', 'ModeArray',  $k);

	stdfToAtdfFieldArray('GRP_RADX', 'U1', 'RadixArray', $k);
	my @atdfRadixArray = split(',', $gAtdfRD{'RadixArray'});
	for my $i (0 .. $#atdfRadixArray)
	{
		if ($atdfRadixArray[$i] == 2)
		{
			$atdfRadixArray[$i] = 'B';
		}
		elsif ($atdfRadixArray[$i] == 8)
		{
			$atdfRadixArray[$i] = 'O';
		}
		elsif ($atdfRadixArray[$i] == 10)
		{
			$atdfRadixArray[$i] = 'D';
		}
		elsif ($atdfRadixArray[$i] == 16)
		{
			$atdfRadixArray[$i] = 'H';
		}
		elsif ($atdfRadixArray[$i] == 20)
		{
			$atdfRadixArray[$i] = 'S';
		}
		else
		{
			$atdfRadixArray[$i] = '';
		};
	};
	$gAtdfRD{'RadixArray'} = atdfArrayCsv(\@atdfRadixArray);

	stdfFieldRead('PGM_CHAR', 'Cn', $k);
	stdfFieldRead('RTN_CHAR', 'Cn', $k);
	stdfFieldRead('PGM_CHAL', 'Cn', $k);
	stdfFieldRead('RTN_CHAL', 'Cn', $k);

	setAtdfStateArray('PGM', 'ProgramState');
	setAtdfStateArray('RTN', 'ReturnedState');

	# setAtdfStateArray($stdfStateType, $atdfStateName)

	sub setAtdfStateArray
	{
		my $stdfStateType = shift();
		my $atdfStateName = shift();
		my @atdfChal = S2A_xCn($k, $gStdfRD{$stdfStateType.'_CHAL'});
		dbugData([\@atdfChal], ['*PLR::atdfChal']);
		my @atdfChar = S2A_xCn($k, $gStdfRD{$stdfStateType.'_CHAR'});
		dbugData([\@atdfChar], ['*PLR::atdfChar']);
		my $atdfStateArray = undef;
		for my $i (1 .. $k)
		{
			my $n = $i - 1;
			my @chal = split(/,/, $atdfChal[$n]);
			dbugData([\@chal], ['*PLR::chal']);
			my @char = split(/,/, $atdfChar[$n]);
			dbugData([\@char], ['*PLR::char']);
			for my $j (0 .. $#char)
			{
				if ($chal[$j] eq SPC)
				{
					$chal[$j] = '';
				};
				$atdfStateArray .= $chal[$j].$char[$j].',';
			};
			chop($atdfStateArray);
			$atdfStateArray .= '/';
		};
		chop($atdfStateArray);
		$gAtdfRD{$atdfStateName} = $atdfStateArray;
	};

};

# PMR()

sub PMR
{
	stdfToAtdfField('PMR_INDX', 'U2');
	stdfToAtdfField('CHAN_TYP', 'U2');
	stdfToAtdfField('CHAN_NAM', 'Cn');
	stdfToAtdfField('PHY_NAM',  'Cn');
	stdfToAtdfField('LOG_NAM',  'Cn');
	stdfToAtdfHeadNumber();
	stdfToAtdfSiteNumber();
};

# PRR()

sub PRR
{
	stdfToAtdfHeadNumber();
	stdfToAtdfSiteNumber();
	stdfToAtdfField('PART_FLG', 'B1');
	stdfToAtdfField('NUM_TEST', 'U2');
	stdfToAtdfField('HARD_BIN', 'U2');
	stdfToAtdfField('SOFT_BIN', 'U2', 65535);
	stdfToAtdfField('X_COORD',  'I2', -32768);
	stdfToAtdfField('Y_COORD',  'I2', -32768);
	stdfToAtdfField('TEST_T',   'U4', 0);
	stdfToAtdfField('PART_ID',  'Cn');
	stdfToAtdfField('PART_TXT', 'Cn');
	stdfToAtdfField('PART_FIX', 'Bn');

	$gAtdfRD{'PassFailCode'} = '';
	if (! bitand($gAtdfRD{'PART_FLG'}, 0x10))
	{
		if (bitand($gAtdfRD{'PART_FLG'}, 0x08))
		{
			$gAtdfRD{'PassFailCode'} = 'F';
		}
		else
		{
			$gAtdfRD{'PassFailCode'} = 'P';
		};
	};

	$gAtdfRD{'RetestCode'} = '';
	my $bit0 = bitand($gAtdfRD{'PART_FLG'}, 0x01);
	my $bit1 = bitand($gAtdfRD{'PART_FLG'}, 0x02);
	if    (($bit1 == 1) && ($bit0 == 1))
	{
		fatal('PRR.PART_FLG field bits 0 and 1 are invalid!');
	}
	elsif (($bit1 == 0) && ($bit0 == 1))
	{
		$gAtdfRD{'RetestCode'} = 'I';
	}
	elsif (($bit1 == 1) && ($bit0 == 0))
	{
		$gAtdfRD{'RetestCode'} = 'C';
	};

	$gAtdfRD{'AbortCode'} = '';
	if (bitand($gAtdfRD{'PART_FLG'}, 0x04))
	{
		$gAtdfRD{'AbortCode'} = 'Y';
	};
};

# PSR()

sub PSR
{
	stdfToAtdfField('REC_INDX', 'U1');
	stdfToAtdfField('REC_TOT', 'U1');
	stdfToAtdfField('PSR_INDX', 'U2');
	stdfToAtdfField('PSR_NAM', 'Cn');
	stdfToAtdfField('OPT_FLG', 'B1');
	stdfToAtdfField('TOTP_CNT', 'U2');	
	stdfToAtdfField('LOCP_CNT', 'U2');
	stdfToAtdfFieldArray('PAT_BGN', 'U8', 'BeginsArray', $gAtdfRD{'LOCP_CNT'});
	stdfToAtdfFieldArray('PAT_END', 'U8', 'EndsArray', $gAtdfRD{'LOCP_CNT'});
	stdfToAtdfFieldArray('PAT_FILE', 'Cn', 'PatsArray', $gAtdfRD{'LOCP_CNT'});
	stdfToAtdfFieldArray('PAT_LBL', 'Cn', 'LabelsArray', $gAtdfRD{'LOCP_CNT'});
	stdfToAtdfFieldArray('FILE_UID', 'Cn', 'UIDsArray', $gAtdfRD{'LOCP_CNT'});
	stdfToAtdfFieldArray('ATPG_DSC', 'Cn', 'AtpgsArray', $gAtdfRD{'LOCP_CNT'});
	stdfToAtdfFieldArray('SRC_ID', 'Cn', 'SrcsArray', $gAtdfRD{'LOCP_CNT'});

};

# PTR()

sub PTR
{
	stdfToAtdfField('TEST_NUM', 'U4');
	stdfToAtdfHeadNumber();
	stdfToAtdfSiteNumber();
	stdfToAtdfField('TEST_FLG', 'B1');
	stdfToAtdfField('PARM_FLG', 'B1');
	stdfToAtdfField('RESULT', 'R4');
	#atdfFormatReal(\$gAtdfRD{'RESULT'});
	$gAtdfRD{'RESULT'} = sprintf("%.9e",$gAtdfRD{'RESULT'});
	stdfToAtdfField('TEST_TXT', 'Cn');
	stdfToAtdfField('ALARM_ID', 'Cn');
	stdfToAtdfField('OPT_FLG', 'B1');
	stdfToAtdfField('RES_SCAL', 'I1');
	stdfToAtdfField('LLM_SCAL', 'I1');
	stdfToAtdfField('HLM_SCAL', 'I1');
	stdfToAtdfField('LO_LIMIT', 'R4');
	#atdfFormatReal(\$gAtdfRD{'LO_LIMIT'});
	#$gAtdfRD{'LO_LIMIT'} = sprintf("%.9e",$gAtdfRD{'LO_LIMIT'});
	stdfToAtdfField('HI_LIMIT', 'R4');
	#$gAtdfRD{'HI_LIMIT'} = sprintf("%.9e",$gAtdfRD{'HI_LIMIT'});
	#atdfFormatReal(\$gAtdfRD{'HI_LIMIT'});
	stdfToAtdfField('UNITS', 'Cn');
	stdfToAtdfField('C_RESFMT', 'Cn');
	stdfToAtdfField('C_LLMFMT', 'Cn');
	stdfToAtdfField('C_HLMFMT', 'Cn');
	stdfToAtdfField('LO_SPEC', 'R4');
	#atdfFormatReal(\$gAtdfRD{'LO_SPEC'});
	#$gAtdfRD{'LO_SPEC'} = sprintf("%.9e",$gAtdfRD{'LO_SPEC'});
	stdfToAtdfField('HI_SPEC', 'R4');
	#atdfFormatReal(\$gAtdfRD{'HI_SPEC'});
	#$gAtdfRD{'HI_SPEC'} = sprintf("%.9e",$gAtdfRD{'HI_SPEC'});

	# set the parametric Pass/Fail Flag
	#
	#	STDF
	#
	#		TEST_FLG bit 6
	#			0=	Pass/Fail flag (bit 7) is valid
	#			1=	Test completed with no pass/fail indication
	#		TEST_FLG bit 7
	#			0=	Test passed
	#			1=	Test failed
	#		PARM_FLG bit 5
	#			0=	Test failed or test passed standard limits
	#			1=	Test passed alternate limits
	#
	#	ATDF
	#
	#		     A= Test passed alternate limits
	#		     F= Test failed
	#		     P= Test passed standard limits
	#		 empty= Test completed without pass/fail
	#
	#	STDF to ATDF map
	#
	#		PARM_FLG	TEST_FLG	PassFailFlag
	#	bit	76543210	76543210
	#		--------	--------	------------
	#		..0....	00....	P
	#		..0....	01....	empty
	#		..0....	10....	F
	#		..0....	11....	empty
	#		..1....	00....	A
	#		..1....	01....	empty
	#		..1....	10....	empty
	#		..1....	11....	empty

	dbugData([$gAtdfRD{'PARM_FLG'}], ["*PTR::gAtdfRD{'PARM_FLG'}"]);
	dbugData([$gAtdfRD{'TEST_FLG'}], ["*PTR::gAtdfRD{'TEST_FLG'}"]);

	my $testFlgBit6 = bitand($gAtdfRD{'TEST_FLG'}, 0x40);
	dbugData([$testFlgBit6], ['*PTR::testFlgBit6']);
	my $testFlgBit7 = bitand($gAtdfRD{'TEST_FLG'}, 0x80);
	dbugData([$testFlgBit7], ['*PTR::testFlgBit7']);

	$gAtdfRD{'PassFailFlag'} = '';
	if (bitand($gAtdfRD{'PARM_FLG'}, 0x20) == 0)
	{
		if    (($testFlgBit7 == 0) && ($testFlgBit6 == 0))
		{
			$gAtdfRD{'PassFailFlag'} = 'P';
		}
		elsif (($testFlgBit7 == 1) && ($testFlgBit6 == 0))
		{
			$gAtdfRD{'PassFailFlag'} = 'F';
		};
	}
	else
	{
		if    (($testFlgBit7 == 0) && ($testFlgBit6 == 0))
		{
			$gAtdfRD{'PassFailFlag'} = 'A';
		};
	};

	# set the parametric Alarm Flags
	#
	#	STDF
	#
	#		TEST_FLG bit 0
	#			0= No alarm
	#			1= Alarm detected
	#		TEST_FLG bit 2
	#			0= Result is reliable
	#			1= Result is NOT reliable
	#		TEST_FLG bit 3
	#			0= Timeout
	#			1= No timeout
	#		TEST_FLG bit 4
	#			0= Test executed
	#			1= Test not executed
	#		TEST_FLG bit 5
	#			0= Test not aborted
	#			1= Test aborted
	#
	#		PARM_FLG bit 0
	#			0= No scale error
	#			1= Scale error
	#		PARM_FLG bit 1
	#			0= No drift error
	#			1= Drift error
	#		PARM_FLG bit 2
	#			0= No oscillation
	#			1= Oscillation detected
	#		PARM_FLG bit 3
	#			0= Measured value not high
	#			1= Measured value higher than high test limit
	#		PARM_FLG bit 4
	#			0= Measured value not low
	#			1= Measured value lower than low test limit
	#
	#	ATDF
	#
	#		A= Alarm detected
	#		U= Test result is unreliable
	#		T= Time-out occurred
	#		N= Test was not executed
	#		X= Test aborted
	#		S= Scale error
	#		D= Drift error
	#		O= Oscillation detected
	#		H= Measured value higher than high test limit
	#		L= Measured value lower than low test limit
	#
	#	STDF to ATDF map
	#
	#           TEST_FLG    PARM_FLG
	#	Bit:    76543210    76543210
	#	        ..XNTU.A	...LHODS

	dbugData([$gAtdfRD{'TEST_FLG'}], ["*PTR::gAtdfRD{'TEST_FLG'}"]);
	dbugData([$gAtdfRD{'PARM_FLG'}], ["*PTR::gAtdfRD{'PARM_FLG'}"]);

	$gAtdfRD{'AlarmFlags'} = '';
	if (bitand($gAtdfRD{'TEST_FLG'}, 0x01))
	{
		$gAtdfRD{'AlarmFlags'} .= 'A';
	};
	if (bitand($gAtdfRD{'TEST_FLG'}, 0x04))
	{
		$gAtdfRD{'AlarmFlags'} .= 'U';
	};
	if (bitand($gAtdfRD{'TEST_FLG'}, 0x08))
	{
		$gAtdfRD{'AlarmFlags'} .= 'T';
	};
	if (bitand($gAtdfRD{'TEST_FLG'}, 0x10))
	{
		$gAtdfRD{'AlarmFlags'} .= 'N';
	};
	if (bitand($gAtdfRD{'TEST_FLG'}, 0x20))
	{
		$gAtdfRD{'AlarmFlags'} .= 'X';
	};
	if (bitand($gAtdfRD{'PARM_FLG'}, 0x01))
	{
		$gAtdfRD{'AlarmFlags'} .= 'S';
	};
	if (bitand($gAtdfRD{'PARM_FLG'}, 0x02))
	{
		$gAtdfRD{'AlarmFlags'} .= 'D';
	};
	if (bitand($gAtdfRD{'PARM_FLG'}, 0x04))
	{
		$gAtdfRD{'AlarmFlags'} .= 'O';
	};
	if (bitand($gAtdfRD{'PARM_FLG'}, 0x08))
	{
		$gAtdfRD{'AlarmFlags'} .= 'H';
	};
	if (bitand($gAtdfRD{'PARM_FLG'}, 0x10))
	{
		$gAtdfRD{'AlarmFlags'} .= 'L';
	};


	# set the parametric Limit Compare Flags
	#
	#	STDF
	#
	#		PARM_FLG bit 6
	#			0= if result equals low limit then result is "fail"
	#			1= if result equals low limit then result is "pass"
	#		PARM_FLG bit 7
	#			0= if result equals high limit then result is "fail"
	#			1= if result equals high limit then result is "pass"
	#
	#	ATDF
	#
	#		L= Low limit comparison was >=
	#		H= High limit comparison was <=
	#
	#	STDF to ATDF map
	#
	#		PARM_FLG
	#	Bit:	76543210
	#		HL......

	dbugData([$gAtdfRD{'PARM_FLG'}], ["*PTR::gAtdfRD{'PARM_FLG'}"]);

	$gAtdfRD{'LimitCompare'} = '';
	if (bitand($gAtdfRD{'PARM_FLG'}, 0x40))
	{
		$gAtdfRD{'LimitCompare'} .= 'L';
	};
	if (bitand($gAtdfRD{'PARM_FLG'}, 0x80))
	{
		$gAtdfRD{'LimitCompare'} .= 'H';
	};

	# clear invalid fields based on OPT_FLG
	#
	# b0 = 1 -> RES_SCAL invalid
	# b1 = 1 -> reserved, should be 1
	# b2 = 1 -> LO_SPEC invalid
	# b3 = 1 -> HI_SPEC invalid
	# b4 = 1 -> LO_LIMIT and LLM_SCAL invalid, use first PTR
	# b5 = 1 -> HI_LIMIT and HLM_SCAL invalid, use first PTR
	# b6 = 1 -> LO_LIMIT and LLM_SCAL invalid
	# b7 = 1 -> HI_LIMIT and HLM_SCAL invalid
	if (bitand($gAtdfRD{'OPT_FLG'}, 0x01))
	{
		$gAtdfRD{'RES_SCAL'} = '';
	};
	if (bitand($gAtdfRD{'OPT_FLG'}, 0x04))
	{
		$gAtdfRD{'LO_SPEC'} = '';
	};
	if (bitand($gAtdfRD{'OPT_FLG'}, 0x08))
	{
		$gAtdfRD{'HI_SPEC'} = '';
	};
	if (bitand($gAtdfRD{'OPT_FLG'}, 0x10))
	{
		$gAtdfRD{'LO_LIMIT'} = '';
		$gAtdfRD{'LLM_SCAL'} = '';
	};
	if (bitand($gAtdfRD{'OPT_FLG'}, 0x20))
	{
		$gAtdfRD{'HI_LIMIT'} = '';
		$gAtdfRD{'HLM_SCAL'} = '';
	};
	if (bitand($gAtdfRD{'OPT_FLG'}, 0x40))
	{
		$gAtdfRD{'LO_LIMIT'} = '';
		$gAtdfRD{'LLM_SCAL'} = '';
	};
	if (bitand($gAtdfRD{'OPT_FLG'}, 0x70))
	{
		$gAtdfRD{'HI_LIMIT'} = '';
		$gAtdfRD{'HLM_SCAL'} = '';
	};
};

# RDR()

sub RDR
{
	stdfToAtdfField('NUM_BINS', 'U2');
	stdfToAtdfFieldArray('RTST_BIN', 'U2', 'RetestBins', $gAtdfRD{'NUM_BINS'});
};

# SBR()

sub SBR
{
	stdfToAtdfHeadNumber();
	stdfToAtdfSiteNumber();
	stdfToAtdfField('SBIN_NUM', 'U2');
	stdfToAtdfField('SBIN_CNT', 'U4');
	stdfToAtdfField('SBIN_PF',  'C1', SPC);
	stdfToAtdfField('SBIN_NAM', 'Cn');
};

# SCR()

sub SCR
{
	stdfToAtdfField('REC_INDX', 'U1');
	stdfToAtdfField('REC_TOT', 'U1');
	stdfToAtdfField('SCR_INDX', 'U2');
	stdfToAtdfField('CHN_NAM', 'Cn');
	stdfToAtdfField('TOTS_CNT', 'U2');
	stdfToAtdfField('LOCS_CNT', 'U2');	# k
	stdfToAtdfField('SIN_PIN', 'U2');
	stdfToAtdfField('SOUT_PIN', 'U2');
	stdfToAtdfField('MSTR_CNT', 'U1');	# m
	stdfToAtdfField('SLAV_CNT', 'U1');	# n
	stdfToAtdfFieldArray('M_CLKS', 'U2', 'MclksArray', $gAtdfRD{'MSTR_CNT'});
	stdfToAtdfFieldArray('S_CLKS', 'U2', 'SclksArray', $gAtdfRD{'SLAV_CNT'});	
	stdfToAtdfField('INV_VAL', 'U1');
	stdfToAtdfFieldArray('CELL_LST', 'Sn', 'CellsArray', $gAtdfRD{'LOCS_CNT'});

};

# SDR()

sub SDR
{
	stdfToAtdfHeadNumber();
	stdfToAtdfField('SITE_GRP', 'U1');
	stdfToAtdfField('SITE_CNT', 'U1');
	stdfToAtdfFieldArray('SITE_NUM', 'U1', 'SiteArray', $gAtdfRD{'SITE_CNT'});
	for my $keyL ('HAND', 'CARD', 'LOAD', 'DIB', 'CABL', 'CONT', 'LASR', 'EXTR')
	{
		for my $keyR ('TYP', 'ID')
		{
			stdfToAtdfField(join('_', $keyL, $keyR), 'Cn');
		};
	};
};

# SSR()

sub SSR
{
	stdfToAtdfField('SSR_NAM', 'Cn');
	stdfToAtdfField('CHN_CNT', 'U2');	# k	
	stdfToAtdfFieldArray('CHN_LIST', 'U2', 'ChainsArray', $gAtdfRD{'CHN_CNT'});
	
};

# STR()

sub STR
{
	stdfToAtdfField('REC_INDX', 'U1');
	stdfToAtdfField('REC_TOT', 'U1');
	stdfToAtdfField('TEST_NUM', 'U4');
	stdfToAtdfField('HEAD_NUM', 'U1');	
	stdfToAtdfField('SITE_NUM', 'U1');	
	stdfToAtdfField('PSR_REF', 'U2');	
	stdfToAtdfField('TEST_FLG', 'B1');	
	stdfToAtdfField('LOG_TYP', 'Cn');	
	stdfToAtdfField('TEST_TXT', 'Cn');	
	stdfToAtdfField('ALARM_ID', 'Cn');	
	stdfToAtdfField('PROG_TXT', 'Cn');	
	stdfToAtdfField('RSLT_TXT', 'Cn');	
	stdfToAtdfField('Z_VAL', 'U1');	
	stdfToAtdfField('FMU_FLG', 'B1');	
	stdfToAtdfField('MASK_MAP', 'Dn');	
	stdfToAtdfField('FAL_MAP', 'Dn');		# probably typo in doc.. FAIL_MAP?	
	stdfToAtdfField('CYC_CNT', 'U8');	
	stdfToAtdfField('TOTF_CNT', 'U4');	
	stdfToAtdfField('TOTL_CNT', 'U4');	
	stdfToAtdfField('CYC_BASE', 'U8');	
	stdfToAtdfField('BIT_BASE', 'U2');	
	stdfToAtdfField('DATA_FLG', 'B1');	
	stdfToAtdfField('COND_CNT', 'U2');		# g
	stdfToAtdfField('LOCL_CNT', 'U4');		# k
	stdfToAtdfField('LIM_CNT', 'U2');		# j
	stdfToAtdfField('DATA_BIT', 'U1');	
	stdfToAtdfField('DATA_CHR', 'Cn');	
	stdfToAtdfField('DATA_CNT', 'U2');		# m
	stdfToAtdfField('USR1_LEN', 'U1');		# f1
	stdfToAtdfField('USR2_LEN', 'U1');		# f2
	stdfToAtdfField('USR3_LEN', 'U1');		# f3
	stdfToAtdfField('TXT_LEN', 'U1');		# f4

	stdfToAtdfFieldArray('LIM_INDX', 'U2', 'PMRArray', $gAtdfRD{'LIM_CNT'});
	stdfToAtdfFieldArray('LIM_SPEC', 'U4', 'LimitsArray', $gAtdfRD{'LIM_CNT'});

	stdfToAtdfFieldArray('COND_NAM', 'Cn', 'CondNameArray', $gAtdfRD{'COND_CNT'});
	stdfToAtdfFieldArray('COND_VAL', 'Cn', 'CondValArray', $gAtdfRD{'COND_CNT'});

	stdfToAtdfFieldArray('CYCL_NUM', 'U4', 'CyclesArray', $gAtdfRD{'LOCL_CNT'});
	stdfToAtdfFieldArray('PMR_INDX', 'U2', 'IndicesArray', $gAtdfRD{'LOCL_CNT'});
	stdfToAtdfFieldArray('CHN_NUM', 'U2', 'ChainsArray', $gAtdfRD{'LOCL_CNT'});

	stdfToAtdfFieldArray('CAP_DATA', 'U1', 'CapturesArray', $gAtdfRD{'DATA_CNT'});
	stdfToAtdfFieldArray('EXP_DATA', 'U1', 'ExpectedArray', $gAtdfRD{'DATA_CNT'});
	stdfToAtdfFieldArray('NEW_DATA', 'U1', 'NewVectorArray', $gAtdfRD{'DATA_CNT'});

	stdfToAtdfFieldArray('PAT_NUM', 'U4', 'PatternsArray', $gAtdfRD{'LOCL_CNT'});
	stdfToAtdfFieldArray('BIT_POS', 'U4', 'BitsArray', $gAtdfRD{'LOCL_CNT'});

	if ($gAtdfRD{'USR1_LEN'} > 2) {
		stdfToAtdfFieldArray('USR1', 'U4', 'Usr1Array', $gAtdfRD{'LOCL_CNT'});
	} elsif ($gAtdfRD{'USR1_LEN'} > 1) {
		stdfToAtdfFieldArray('USR1', 'U2', 'Usr1Array', $gAtdfRD{'LOCL_CNT'});
	} elsif ($gAtdfRD{'USR1_LEN'} > 0) {
		stdfToAtdfFieldArray('USR1', 'U1', 'Usr1Array', $gAtdfRD{'LOCL_CNT'});
	} 

	if ($gAtdfRD{'USR2_LEN'} > 2) {
		stdfToAtdfFieldArray('USR2', 'U4', 'Usr2Array', $gAtdfRD{'LOCL_CNT'});
	} elsif ($gAtdfRD{'USR2_LEN'} > 1) {
		stdfToAtdfFieldArray('USR2', 'U2', 'Usr2Array', $gAtdfRD{'LOCL_CNT'});
	} elsif ($gAtdfRD{'USR2_LEN'} > 0) {
		stdfToAtdfFieldArray('USR2', 'U1', 'Usr2Array', $gAtdfRD{'LOCL_CNT'});
	} 

	if ($gAtdfRD{'USR3_LEN'} > 2) {
		stdfToAtdfFieldArray('USR3', 'U4', 'Usr3Array', $gAtdfRD{'LOCL_CNT'});
	} elsif ($gAtdfRD{'USR3_LEN'} > 1) {
		stdfToAtdfFieldArray('USR3', 'U2', 'Usr3Array', $gAtdfRD{'LOCL_CNT'});
	} elsif ($gAtdfRD{'USR3_LEN'} > 0) {
		stdfToAtdfFieldArray('USR3', 'U1', 'Usr3Array', $gAtdfRD{'LOCL_CNT'});
	} 

	if ($gAtdfRD{'TXT_LEN'} > 2) {
		stdfToAtdfFieldArray('USER_TXT', 'C4', 'UserTxtArray', $gAtdfRD{'LOCL_CNT'});
	} elsif ($gAtdfRD{'TXT_LEN'} > 1) {
		stdfToAtdfFieldArray('USER_TXT', 'C2', 'UserTxtArray', $gAtdfRD{'LOCL_CNT'});
	} elsif ($gAtdfRD{'TXT_LEN'} > 0) {
		stdfToAtdfFieldArray('USER_TXT', 'C1', 'UserTxtArray', $gAtdfRD{'LOCL_CNT'});
	} 

};

# TSR()

sub TSR
{
	stdfToAtdfHeadNumber();
	stdfToAtdfSiteNumber();
	stdfToAtdfField('TEST_TYP', 'C1', SPC);
	stdfToAtdfField('TEST_NUM', 'U4');
	stdfToAtdfField('EXEC_CNT', 'U4', 4294967295);
	stdfToAtdfField('FAIL_CNT', 'U4', 4294967295);
	stdfToAtdfField('ALRM_CNT', 'U4', 4294967295);
	stdfToAtdfField('TEST_NAM', 'Cn');
	stdfToAtdfField('SEQ_NAME', 'Cn');
	stdfToAtdfField('TEST_LBL', 'Cn');
	stdfToAtdfField('OPT_FLAG', 'B1');
	stdfToAtdfField('TEST_TIM', 'R4');
	stdfToAtdfField('TEST_MIN', 'R4');
	stdfToAtdfField('TEST_MAX', 'R4');
	stdfToAtdfField('TST_SUMS', 'R4');
	# place to put trap for TSR with MHz freq..
	#if ($gAtdfRD{'TEST_NUM'} == 700)
	#{
	#	# TRAP HERE!
	#	my $bogus = 1;
	#}
	stdfToAtdfField('TST_SQRS', 'R4');

	if (bitand($gAtdfRD{'OPT_FLAG'}, 0x01))
	{
		$gAtdfRD{'TEST_MIN'} = undef;
	};
	if (bitand($gAtdfRD{'OPT_FLAG'}, 0x02))
	{
		$gAtdfRD{'TEST_MAX'} = undef;
	};
	if (bitand($gAtdfRD{'OPT_FLAG'}, 0x04))
	{
		$gAtdfRD{'TEST_TIM'} = undef;
	};
	if (bitand($gAtdfRD{'OPT_FLAG'}, 0x10))
	{
		$gAtdfRD{'TST_SUMS'} = undef;
	};
	if (bitand($gAtdfRD{'OPT_FLAG'}, 0x20))
	{
		$gAtdfRD{'TST_SQRS'} = undef;
	};
};

# VUR()

sub VUR
{
	stdfToAtdfField('UPD_NAM',   'Cn');		# ie  "V4-2007"
};

# WCR()

sub WCR
{
	stdfToAtdfField('WAFR_SIZ', 'R4');
	stdfToAtdfField('DIE_HT',   'R4');
	stdfToAtdfField('DIE_WID',  'R4');
	stdfToAtdfField('WF_UNITS', 'U1');
	stdfToAtdfField('WF_FLAT',  'C1', SPC);
	stdfToAtdfField('CENTER_X', 'I2', -32768);
	stdfToAtdfField('CENTER_Y', 'I2', -32768);
	stdfToAtdfField('POS_X',    'C1', SPC);
	stdfToAtdfField('POS_Y',    'C1', SPC);
};

# WIR()

sub WIR
{
	stdfToAtdfHeadNumber();
	stdfToAtdfField('SITE_GRP', 'U1', 255);
	stdfToAtdfFieldDateTime('START_T');
	stdfToAtdfField('WAFER_ID', 'Cn');
};

# WRR()

sub WRR
{
	stdfToAtdfHeadNumber();
	stdfToAtdfField('SITE_GRP', 'U1', 255);
	stdfToAtdfFieldDateTime('FINISH_T');
	stdfToAtdfField('PART_CNT', 'U4');
	stdfToAtdfField('RTST_CNT', 'U4', 4294967295);
	stdfToAtdfField('ABRT_CNT', 'U4', 4294967295);
	stdfToAtdfField('GOOD_CNT', 'U4', 4294967295);
	stdfToAtdfField('FUNC_CNT', 'U4', 4294967295);
	stdfToAtdfField('WAFER_ID', 'Cn');
	stdfToAtdfField('FABWF_ID', 'Cn');
	stdfToAtdfField('FRAME_ID', 'Cn');
	stdfToAtdfField('MASK_ID',  'Cn');
	stdfToAtdfField('USR_DESC', 'Cn');
	stdfToAtdfField('EXC_DESC', 'Cn');
};

######################################################################

=pod

=head1 REQUIRES

E<bull> Perl 5.6 or newer.

E<bull> Perl core module strict.

E<bull> Perl custom module TDF.pm.

=head1 SEE ALSO

E<bull> perl core and module documentation.

E<bull> STDF Specification V4 published by Teradyne, Inc.

E<bull> ATDF Specification V2 published by Teradyne, Inc.

=head1 AUTHORS

E<bull> Michael Hackerott, michael.hackerott@mrhackerott.org

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

E<bull>  Does not write an ATR record to the ATDF file.

E<bull>  GDR handler is not working and has been disabled.

E<bull>  PLR handler is not working and has been disabled.

=head1 HISTORY

1.0.0 (200412240425) Michael Hackerott

E<bull>  Created program.

1.0.1 (200402080725) Michael Hackerott

E<bull> Moved about() subroutine to TDF.pm module.

E<bull> Added help() subroutine.

1.1.0 (200512012020) Michael Hackerott

E<bull> Fixed endian logic.

1.1.1 (200512030625) Michael Hackerott

E<bull> Added BEGIN block to add lib and ../lib to @INC.

1.1.2 (200512030659) Michael Hackerott

E<bull> Updated documentation.

E<bull> Fixed dbugEnabled() logic.

=cut

__END__
