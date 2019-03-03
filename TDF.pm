# Copyright (C) 2004-2005 Michael Hackerott. All Rights Reserved
#
# modified 2006 David Gattrell - cygwin and linux, also significant digit tweaks
# modified 2011 David Gattrell - update to support STDF V4.2007
# modified 2012 David Gattrell - U4, I4 bug fix, now SBR records proper!
# modified 2013 David Gattrell - S2A_U8 missing for some STDF V4.2007 support...
#                                MPR N1 were being read as U1, fixed
#                                DTR with \n now has space as first char of next line
# modified 2014 David Gattrell - support setting endianness for ATDF->STDF...
#                                tweak to improve stdf->atdf->stdf file integrity
# modified 2017 David Gattrell - revisit PTR and FTR, stdf->atdf->stdf->atdf with a595.stdf
# modified 2018 Haino Hiroyuki - A2S_Nn,S2A_Nn nibble order swapped, and S2A_Bn handles size 0
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
#	pod2html --infile=TDF.pm --outfile=TDF.html

package TDF;

=pod

=head1 NAME

TDF - A library of subroutines for STDF version 4 and ATDF version 2 files.

=head1 SYNOPSIS

	1. use TDF;

	2. Call subroutines.

=head1 DESCRIPTION

Contains standard subroutines for manipulating STDF version 4 and ATDF
version 2 files and their contents.

This module is required by the atdf2_to_stdf4.pl and stdf4_to_atdf2.pl
programs.

=head2 Time and Date Usage

(Quoted from Teradyne STDF specification)

The date and time field used in this specification is defined as a four
byte (32 bit) unsigned integer field measuring the number of seconds
since midnight on January 1st, 1970, in the local time zone. This is the
UNIX standard base time, adjusted to the local time zone.

=head2 Data Representation

(Quoted from Teradyne STDF specification)

When data is shared among systems with unlike central processors, the
problem arises that there is little or no standardization of data
representation (that is, the bit ordering of various data types) among
the various processors of the world. For example, the data
representations for DEC, Motorola, Intel, and IBM computers are all
different, even though at least two of them adhere to the IEEE floating
point standard. Moreover, different processors made by the same company
sometimes store data in incompatible ways. To address this problem, the
STDF specification uses a field called CPU_TYPE in the File Attributes
Record (FAR). This field indicates the type of processor that wrote the
data (for example, Sun series or DEC-11 series). The field is used as
follows:

E<bull> When writing an STDF file, a system uses its own native data
representation. The type of the writing processor is stored in the
CPU_TYPE field.

E<bull> When reading an STDF file, a system must convert the records to its
own native data representation as it reads them, if necessary. To do so,
it checks the value of the CPU_TYPE field in the FAR, which is the first
record in the file. Then, if the writing CPU data representation is
incompatible with its own, it uses a subroutine that reads the next (or
selected) record and converts the records to its own data representation
as it reads them.

=head2 Bit Rules

This module internally operates on all numeric and text bit encoded
values in left (MSB) to right (LSB) order.

	Numeric		MSB -> LSB
		Binary	0b00000000
		Hex		0x00
		Octal	0000

	Text			MSB -> LSB
		Binary	'00000000'
		Hex		'00'
		Octal	'0000'

=head2 Byte Rules

This module internally operates on all numeric and text byte encoded
values in left (MSB) to right (LSB) order.

=head2 Endian Rules

"Little Endian" means that the low-order byte of the number is stored in
memory at the lowest address, and the high-order byte at the highest
address. (The little end comes first.) For example, a 4 byte LongInt

    Byte3 Byte2 Byte1 Byte0

will be arranged in memory as follows:

    Base Address+0   Byte0
    Base Address+1   Byte1
    Base Address+2   Byte2
    Base Address+3   Byte3

Intel processors (those used in PC's) use "Little Endian" byte order.

"Big Endian" means that the high-order byte of the number is stored in
memory at the lowest address, and the low-order byte at the highest
address. (The big end comes first.) Our LongInt, would then be stored
as:

    Base Address+0   Byte3
    Base Address+1   Byte2
    Base Address+2   Byte1
    Base Address+3   Byte0

The order of the bytes is referred to as the "endian".  In a little
endian CPU the storage of bytes in memory is address 0 is LSB, address
+1, is next most significant, and address +n is the most significant
byte.  In a big endian CPU the storage of bytes in memory is address 0
is MSB, address +1 is the next most significant, and address +n is the
least significant byte.

VAX and x86 are little endian CPU and SUN SPARC, HP PARC, and PowerPC
are big endian CPU.

	         MSB LSB
	Example:  12  34 hex
	--------------------------
	| ADDR | LITTLE | BIG    |
	|======|========|========|
	| 0000 |     34 |     12 |
	| 0001 |     12 |     34 |
	--------------------------

Therefore, if the STDF file was written in big endian and the system
executing an STDF conversion program is a little endian then the bytes
in each multibyte field must be swapped so that conversion of the bytes
to numeric values will be correct.

=cut

# use Exporter for module interface
use Exporter;

@ISA = qw(Exporter);

@EXPORT = qw
(
	about
	alert
	bin2hex
	bitand
	dateTimeToSeconds
	dbugData
	dbugDataBin
	dbugDataPurdy
	dbugFmtBin
	dbugSub
	dbugText
	dumpData
	error
	fatal
	hex2bin
	isAtdfEndianBig
	isAtdfEndianLittle
	isOSEndianBig
	isOSEndianLittle
	isStdfEndianBig
	isStdfEndianLittle
	loggText
	osCpuType
	q1
	reverseEndian
	scaleFactor
	secondsToDateTime
	setBitFlag
	stderr
	stdout
	trim
	atdfFieldName
	atdfFieldNameIndexLast
	atdfFormatReal
	atdfRecordFieldNames
	atdfRecordWrap
	A2S_do
	A2S_B0
	A2S_B1
	A2S_Bn
	A2S_C1
	A2S_Cn
	A2S_Cn0
	A2S_Dn
	A2S_I1
	A2S_I2
	A2S_I4
	A2S_N1
	A2S_R4
	A2S_R8
	A2S_U1
	A2S_U2
	A2S_U4
	A2S_xTYPE
	stdfEndian
	stdfFieldName
	stdfFieldNameIndexLast
	stdfRecordFieldNames
	stdfRecordNameToTypeCodes
	stdfRecordTypeCodesToName
	stdfRecordTypeToDataTypeCode
	S2A_do
	S2A_B1
	S2A_Bn
	S2A_C1
	S2A_Cn
	S2A_Dn
	S2A_I1
	S2A_I2
	S2A_I4
	S2A_N1
	S2A_R4
	S2A_R8
	S2A_U1
	S2A_U2
	S2A_U4
	S2A_U8
	S2A_Vn
	S2A_xCn
	S2A_xTYPE
);

@EXPORT_OK =
(
);

%EXPORT_TAGS =
(
);

use Data::Dumper;
$Data::Dumper::Indent = 0;

use Time::Local;

use strict;

########################################################################
# PRIVATE MODULE CONSTANT DECLARATIONS
########################################################################

# set the version number
my $VERSION = '3.2.3';

use constant TRUE	=> 1;
use constant FALSE	=> 0;

use constant EOL	=> "\n";
use constant Q1	=> "\'";
use constant Q2	=> "\"";
use constant SPC	=> "\ ";
use constant TAB	=> "\t";

# %OS_NAME_TO_CPU_TYPE
#
# Maps the Perl operating system identifier to the STDF CPU_TYPE field
# code.

my %OS_NAME_TO_CPU_TYPE = (
	'MSWin32' => 2, # LITTLE ENDIAN
	'solaris' => 1, # BIG ENDIAN
	'cygwin'  => 2, # LITTLE ENDIAN
	'linux'   => 2, # LITTLE ENDIAN
	'hpux'    => 1  # BIG ENDIAN
);

# %OS_NAME_TO_ENDIAN
#
# Maps the Perl operating system identifier to the endian identifier.

my %OS_NAME_TO_ENDIAN = (
	'MSWin32' => 'ENDLIT',
	'solaris' => 'ENDBIG',
	'cygwin'  => 'ENDLIT',
	'linux'   => 'ENDLIT',
	'hpux'    => 'ENDBIG'
);

# %ATDF_RECORD_FIELDS
#
# A hash of arrays that contains the names of all of the ATDF records
# and fields:
#
#	$ATDF_RECORD_FIELDS{$recordName}[$fieldIndex] = ATDF_FIELD_NAME

my %ATDF_RECORD_FIELDS = (
	'ATR' => [qw(
		MOD_TIM
		CMD_LINE
	)],
	'BPS' => [qw(
		SEQ_NAME
	)],
	'CNR' => [qw(
		CHN_NUM
		BIT_POS
		CELL_NAM
	)],
	'DTR' => [qw(
		TEST_DAT
	)],
	'EPS' => [qw(
	)],
	'FAR' => [qw(
		DataFileType
		STDF_VER
		AtdfVersion
		ScalingFlag
	)],
	'FTR' => [qw(
		TEST_NUM
		HEAD_NUM
		SITE_NUM
		PassFailFlag
		AlarmFlags
		VECT_NAM
		TIME_SET
		CYCL_CNT
		REL_VADR
		REPT_CNT
		NUM_FAIL
		XFAIL_AD
		YFAIL_AD
		VECT_OFF
		ReturnIndexes
		ReturnStates
		ProgIndexes
		ProgStates
		FailingPins
		OP_CODE
		TEST_TXT
		ALARM_ID
		PROG_TXT
		RSLT_TXT
		PATG_NUM
		Comparators
	)],
	'GDR' => [qw(
		GenericData
	)],
	'HBR' => [qw(
		HEAD_NUM
		SITE_NUM
		HBIN_NUM
		HBIN_CNT
		HBIN_PF
		HBIN_NAM
	)],
	'MIR' => [qw(
		LOT_ID
		PART_TYP
		JOB_NAM
		NODE_NAM
		TSTR_TYP
		SETUP_T
		START_T
		OPER_NAM
		MODE_COD
		STAT_NUM
		SBLOT_ID
		TEST_COD
		RTST_COD
		JOB_REV
		EXEC_TYP
		EXEC_VER
		PROT_COD
		CMOD_COD
		BURN_TIM
		TST_TEMP
		USER_TXT
		AUX_FILE
		PKG_TYP
		FAMLY_ID
		DATE_COD
		FACIL_ID
		FLOOR_ID
		PROC_ID
		OPER_FRQ
		SPEC_NAM
		SPEC_VER
		FLOW_ID
		SETUP_ID
		DSGN_REV
		ENG_ID
		ROM_COD
		SERL_NUM
		SUPR_NAM
	)],
	'MPR' => [qw(
		TEST_NUM
		HEAD_NUM
		SITE_NUM
		StatesArray
		ResultsArray
		PassFailFlag
		AlarmFlags
		TEST_TXT
		ALARM_ID
		LimitCompare
		UNITS
		LO_LIMIT
		HI_LIMIT
		START_IN
		INCR_IN
		UNITS_IN
		IndexArray
		C_RESFMT
		C_LLMFMT
		C_HLMFMT
		LO_SPEC
		HI_SPEC
		RES_SCAL
		LLM_SCAL
		HLM_SCAL
	)],
	'MRR' => [qw(
		FINISH_T
		DISP_COD
		USR_DESC
		EXC_DESC
	)],
	'NMR' => [qw(
		REC_INDX
		REC_TOT
		TOTM_CNT
		LOCM_CNT
		IndexesArray
		NamesArray
	)],				
	'PCR' => [qw(
		HEAD_NUM
		SITE_NUM
		PART_CNT
		RTST_CNT
		ABRT_CNT
		GOOD_CNT
		FUNC_CNT
	)],
	'PGR' => [qw(
		GRP_INDX
		GRP_NAM
		IndexArray
	)],
	'PIR' => [qw(
		HEAD_NUM
		SITE_NUM
	)],
	'PLR' => [qw(
		IndexArray
		ModeArray
		RadixArray
		ProgramState
		ReturnedState
	)],
	'PMR' => [qw(
		PMR_INDX
		CHAN_TYP
		CHAN_NAM
		PHY_NAM
		LOG_NAM
		HEAD_NUM
		SITE_NUM
	)],
	'PRR' => [qw(
		HEAD_NUM
		SITE_NUM
		PART_ID
		NUM_TEST
		PassFailCode
		HARD_BIN
		SOFT_BIN
		X_COORD
		Y_COORD
		RetestCode
		AbortCode
		TEST_T
		PART_TXT
		PART_FIX
	)],
	'PSR' => [qw(
		REC_INDX
		REC_TOT
		PSR_INDX
		PSR_NAM
		OPT_FLG
		TOTP_CNT
		LOCP_CNT
		BeginsArray
		EndsArray
		PatsArray
		LabelsArray
		UIDsArray
		AtpgsArray
		SrcsArray
	)],
	'PTR' => [qw(
		TEST_NUM
		HEAD_NUM
		SITE_NUM
		RESULT
		PassFailFlag
		AlarmFlags
		TEST_TXT
		ALARM_ID
		LimitCompare
		UNITS
		LO_LIMIT
		HI_LIMIT
		C_RESFMT
		C_LLMFMT
		C_HLMFMT
		LO_SPEC
		HI_SPEC
		RES_SCAL
		LLM_SCAL
		HLM_SCAL
	)],
	'RDR' => [qw(
		RetestBins
	)],
	'SBR' => [qw(
		HEAD_NUM
		SITE_NUM
		SBIN_NUM
		SBIN_CNT
		SBIN_PF
		SBIN_NAM
	)],
	'SCR' => [qw(
		REC_INDX
		REC_TOT
		SCR_INDX
		CHN_NAM
		TOTS_CNT
		LOCS_CNT
		SIN_PIN
		SOUT_PIN
		MSTR_CNT
		SLAV_CNT
		MclksArray
		SclksArray
		INV_VAL
		CellsArray
	)],
	'SDR' => [qw(
		HEAD_NUM
		SITE_GRP
		SiteArray
		HAND_TYP
		HAND_ID
		CARD_TYP
		CARD_ID
		LOAD_TYP
		LOAD_ID
		DIB_TYP
		DIB_ID
		CABL_TYP
		CABL_ID
		CONT_TYP
		CONT_ID
		LASR_TYP
		LASR_ID
		EXTR_TYP
		EXTR_ID
	)],
	'SSR' => [qw(
		SSR_NAM
		CHN_CNT
		ChainsArray
	)],
	'STR' => [qw(
		REC_INDX
		REC_TOT
		TEST_NUM
		HEAD_NUM
		SITE_NUM
		PSR_REF
		TEST_FLG
		LOG_TYP
		TEST_TXT
		ALARM_ID
		PROG_TXT
		RSLT_TXT
		Z_VAL
		FMU_FLG
		MASK_MAP
		FAL_MAP
		CYC_CNT
		TOTF_CNT
		TOTL_CNT
		CYC_BASE
		BIT_BASE
		DATA_FLG
		COND_CNT
		LOCL_CNT
		LIM_CNT
		DATA_BIT
		DATA_CHR
		DATA_CNT
		USR1_LEN
		USR2_LEN
		USR3_LEN
		TXT_LEN
		PMRArray
		LimitsArray
		CondNameArray
		CondValArray
		CyclesArray
		IndicesArray
		ChainsArray
		CapturesArray
		ExpectedArray
		NewVectorArray
		PatternsArray
		BitsArray
		Usr1Array
		Usr2Array
		Usr3Array
		UserTxtArray
	)],		
	'TSR' => [qw(
		HEAD_NUM
		SITE_NUM
		TEST_NUM
		TEST_NAM
		TEST_TYP
		EXEC_CNT
		FAIL_CNT
		ALRM_CNT
		SEQ_NAME
		TEST_LBL
		TEST_TIM
		TEST_MIN
		TEST_MAX
		TST_SUMS
		TST_SQRS
	)],
	'VUR' => [qw(
		UPD_NAM
	)],
	'WCR' => [qw(
		WF_FLAT
		POS_X
		POS_Y
		WAFR_SIZ
		DIE_HT
		DIE_WID
		WF_UNITS
		CENTER_X
		CENTER_Y
	)],
	'WIR' => [qw(
		HEAD_NUM
		START_T
		SITE_GRP
		WAFER_ID
	)],
	'WRR' => [qw(
		HEAD_NUM
		FINISH_T
		PART_CNT
		WAFER_ID
		SITE_GRP
		RTST_CNT
		ABRT_CNT
		GOOD_CNT
		FUNC_CNT
		FABWF_ID
		FRAME_ID
		MASK_ID
		USR_DESC
		EXC_DESC
	)],
);

# %ATDF_TO_STDF_DATA_TYPE_CODE_TO_SUBREF

my %ATDF_TO_STDF_DATA_TYPE_CODE_TO_SUBREF = (
	'B1' => \&A2S_B1,
	'Bn' => \&A2S_Bn,
	'C1' => \&A2S_C1,
	'C2' => \&A2S_C2,
	'C4' => \&A2S_C4,
	'Cn' => \&A2S_Cn,
	'Dn' => \&A2S_Dn,
	'I1' => \&A2S_I1,
	'I2' => \&A2S_I2,
	'I4' => \&A2S_I4,
	'N1' => \&A2S_N1,
	'R4' => \&A2S_R4,
	'R8' => \&A2S_R8,
	'U1' => \&A2S_U1,
	'U2' => \&A2S_U2,
	'U4' => \&A2S_U4,
	'Vn' => \&A2S_Vn,
);

# %STDF_CPU_TYPE_TO_ENDIAN
#
# A hash of arrays that contains the names of all of the ATDF records
# and fields:
#
#	$STDF_CPU_TYPE_TO_ENDIAN{$cpu_type} = ENDIAN_KEYWORD
#
# Where ENDIAN_KEYWORD is 'ENDBIG' for big endian and 'ENDLIT' for
# little endian.

my %STDF_CPU_TYPE_TO_ENDIAN = (
	1 => 'ENDBIG', # SUN
	0 => 'ENDLIT', # VAX
	2 => 'ENDLIT', # WIN
);

# %STDF_RECORD_NAME_TO_TYPE_CODES
#
# A hash of arrays that contains the STDF record type and sub-type code for
# each record name:
#
#	$STDF_RECORD_NAME_TO_TYPE_CODES{$recordName}
#		[0] = STDF_RECORD_TYPE
#		[1] = STDF_RECORD_SUBTYPE
#
#	NAME      TYPE      SUBTYPE
#	========  ========  ========
#	ATR              0        20
#	BPS             20        10
#	CNR				 1		  92	# 2007
#	DTR             50        30
#	EPS             20        20
#	FAR              0        10
#	FTR             15        20
#	GDR             50        10
#	HBR              1        40
#	MIR              1        10
#	MPR             15        15
#	MRR              1        20
#	NMR				 1		  91	# 2007
#	PCR              1        30
#	PGR              1        62
#	PIR              5        10
#	PLR              1        63
#	PMR              1        60
#	PRR              5        20
#	PSR				 1		  90	# 2007
#	PTR             15        10
#	RDR              1        70
#	SBR              1        50
#	SCR				 1		  94	# 2007
#	SDR              1        80
#	SSR				 1		  93	# 2007
#	STR				15		  30	# 2007
#	TSR             10        30
#	VUR				 0		  30	# 2007
#	WCR              2        30
#	WIR              2        10
#	WRR              2        20

my %STDF_RECORD_NAME_TO_TYPE_CODES = (
	'ATR' => [ 0, 20],
	'BPS' => [20, 10],
	'CNR' => [ 1, 92],		# 2007
	'DTR' => [50, 30],
	'EPS' => [20, 20],
	'FAR' => [ 0, 10],
	'FTR' => [15, 20],
	'GDR' => [50, 10],
	'HBR' => [ 1, 40],
	'MIR' => [ 1, 10],
	'MPR' => [15, 15],
	'MRR' => [ 1, 20],
	'NMR' => [ 1, 91],		# 2007
	'PCR' => [ 1, 30],
	'PGR' => [ 1, 62],
	'PIR' => [ 5, 10],
	'PLR' => [ 1, 63],
	'PMR' => [ 1, 60],
	'PRR' => [ 5, 20],
	'PSR' => [ 1, 90],		# 2007
	'PTR' => [15, 10],
	'RDR' => [ 1, 70],
	'SBR' => [ 1, 50],
	'SCR' => [ 1, 94],		# 2007
	'SDR' => [ 1, 80],
	'SSR' => [ 1, 93],		# 2007
	'STR' => [15, 30],		# 2007
	'TSR' => [10, 30],
	'VUR' => [ 0, 30],		# 2007
	'WCR' => [ 2, 30],
	'WIR' => [ 2, 10],
	'WRR' => [ 2, 20],
);

# %STDF_RECORD_TYPE_CODES_TO_NAME
#
#	$STDF_RECORD_TYPE_CODES_TO_NAME{$recTyp}{$recSub}=STDF_RECORD_NAME

my %STDF_RECORD_TYPE_CODES_TO_NAME = (
	'0' => {
		'10' => 'FAR',
		'20' => 'ATR',
		'30' => 'VUR',
	},
	'1' => {
		'10' => 'MIR',
		'20' => 'MRR',
		'30' => 'PCR',
		'40' => 'HBR',
		'50' => 'SBR',
		'60' => 'PMR',
		'62' => 'PGR',
		'63' => 'PLR',
		'70' => 'RDR',
		'80' => 'SDR',
		'90' => 'PSR',
		'91' => 'NMR',
		'92' => 'CNR',
		'93' => 'SSR',
		'94' => 'SCR',
	},
	'2' => {
		'10' => 'WIR',
		'20' => 'WRR',
		'30' => 'WCR',
	},
	'5' => {
		'10' => 'PIR',
		'20' => 'PRR',
	},
	'10' => {
		'30' => 'TSR',
	},
	'15' => {
		'10' => 'PTR',
		'15' => 'MPR',
		'20' => 'FTR',
		'30' => 'STR',
	},
	'20' => {
		'10' => 'BPS',
		'20' => 'EPS',
	},
	'50' => {
		'10' => 'GDR',
		'30' => 'DTR',
	}
);

# %STDF_RECORD_TYPE_TO_DATA_TYPE_CODE

my %STDF_RECORD_TYPE_TO_DATA_TYPE_CODE = (
	'Bn' => 'U1',
	'Cn' => 'U1',
	'Dn' => 'U2',
	'Vn' => 'U1',
);

# %STDF_RECORD_FIELDS
#
# A hash of arrays that contains the STDF field name and data type
# for each record name:
#
#	$STDF_RECORD_FIELDS{$recordName}[$fieldIndex]
#		[0] = STDF_FIELD_TYPE
#		[1] = STDF_DATA_TYPE

my %STDF_RECORD_FIELDS = (
	'ATR' => [
		['MOD_TIM',	'U4'],
		['CMD_LINE',	'Cn'],
	],
	'BPS' => [
		['SEQ_NAME',	'Cn'],
	],
	'CNR' => [
		['CHN_NUM',		'U2'],
		['BIT_POS',		'U2'],
		['CELL_NAM',	'Sn'],
	],
	'DTR' => [
		['TEST_DAT',	'Cn'],
	],
	'EPS' => [
	],
	'FAR' => [
		['CPU_TYPE',	'U1'],
		['STDF_VER',	'U1'],
	],
	'FTR' => [
		['TEST_NUM',	'U4'],
		['HEAD_NUM',	'U1'],
		['SITE_NUM',	'U1'],
		['TEST_FLG',	'B1'],
		['OPT_FLAG',	'B1'],
		['CYCL_CNT',	'U4'],
		['REL_VADR',	'U4'],
		['REPT_CNT',	'U4'],
		['NUM_FAIL',	'U4'],
		['XFAIL_AD',	'I4'],
		['YFAIL_AD',	'I4'],
		['VECT_OFF',	'I2'],
		['RTN_ICNT',	'U2',	'j'],
		['PGM_ICNT',	'U2',	'k'],
		['RTN_INDX',	'U2', 	'jx'],
		['RTN_STAT',	'N1',	'jx'],
		['PGM_INDX',	'U2', 	'kx'],
		['PGM_STAT',	'N1',	'kx'],
		['FAIL_PIN',	'Dn'],
		['VECT_NAM',	'Cn'],
		['TIME_SET',	'Cn'],
		['OP_CODE',	'Cn'],
		['TEST_TXT',	'Cn'],
		['ALARM_ID',	'Cn'],
		['PROG_TXT',	'Cn'],
		['RSLT_TXT',	'Cn'],
		['PATG_NUM',	'U1'],
		['SPIN_MAP',	'Dn'],
	],
	'GDR' => [
		['FLD_CNT',	'U2'],
		['GEN_DATA',	'Vn'],
	],
	'HBR' => [
		['HEAD_NUM',	'U1'],
		['SITE_NUM',	'U1'],
		['HBIN_NUM',	'U2'],
		['HBIN_CNT',	'U4'],
		['HBIN_PF',	'C1'],
		['HBIN_NAM',	'Cn'],
	],
	'MIR' => [
		['SETUP_T',	'U4'],
		['START_T',	'U4'],
		['STAT_NUM',	'U1'],
		['MODE_COD',	'C1'],
		['RTST_COD',	'C1'],
		['PROT_COD',	'C1'],
		['BURN_TIM',	'U2'],
		['CMOD_COD',	'C1'],
		['LOT_ID',	'Cn'],
		['PART_TYP',	'Cn'],
		['NODE_NAM',	'Cn'],
		['TSTR_TYP',	'Cn'],
		['JOB_NAM',	'Cn'],
		['JOB_REV',	'Cn'],
		['SBLOT_ID',	'Cn'],
		['OPER_NAM',	'Cn'],
		['EXEC_TYP',	'Cn'],
		['EXEC_VER',	'Cn'],
		['TEST_COD',	'Cn'],
		['TST_TEMP',	'Cn'],
		['USER_TXT',	'Cn'],
		['AUX_FILE',	'Cn'],
		['PKG_TYP',	'Cn'],
		['FAMLY_ID',	'Cn'],
		['DATE_COD',	'Cn'],
		['FACIL_ID',	'Cn'],
		['FLOOR_ID',	'Cn'],
		['PROC_ID',	'Cn'],
		['OPER_FRQ',	'Cn'],
		['SPEC_NAM',	'Cn'],
		['SPEC_VER',	'Cn'],
		['FLOW_ID',	'Cn'],
		['SETUP_ID',	'Cn'],
		['DSGN_REV',	'Cn'],
		['ENG_ID',	'Cn'],
		['ROM_COD',	'Cn'],
		['SERL_NUM',	'Cn'],
		['SUPR_NAM',	'Cn'],
	],
	'MPR' => [
		['TEST_NUM',	'U4'],
		['HEAD_NUM',	'U1'],
		['SITE_NUM',	'U1'],
		['TEST_FLG',	'B1'],
		['PARM_FLG',	'B1'],
		['RTN_ICNT',	'U2',	'j'],
		['RSLT_CNT',	'U2',	'k'],
		['RTN_STAT',	'N1',	'jx'],
		['RTN_RSLT',	'R4',	'kx'],
		['TEST_TXT',	'Cn'],
		['ALARM_ID',	'Cn'],
		['OPT_FLAG',	'B1'],
		['RES_SCAL',	'I1'],
		['LLM_SCAL',	'I1'],
		['HLM_SCAL',	'I1'],
		['LO_LIMIT',	'R4'],
		['HI_LIMIT',	'R4'],
		['START_IN',	'R4'],
		['INCR_IN',	'R4'],
		['RTN_INDX',	'U2',	'jx'],
		['UNITS',		'Cn'],
		['UNITS_IN',	'Cn'],
		['C_RESFMT',	'Cn'],
		['C_LLMFMT',	'Cn'],
		['C_HLMFMT',	'Cn'],
		['LO_SPEC',	'R4'],
		['HI_SPEC',	'R4'],
	],
	'MRR' => [
		['FINISH_T',	'U4'],
		['DISP_COD',	'C1'],
		['USR_DESC',	'Cn'],
		['EXC_DESC',	'Cn'],
	],
	'NMR' => [
		['REC_INDX',	'U1'],
		['REC_TOT',		'U1'],
		['TOTM_CNT',	'U2'],
		['LOCM_CNT',	'U2',	'k'],
		['PMR_INDX',	'U2',	'kx'],
		['ATPG_NAM',	'Cn',	'kx'],
	],
	'PCR' => [
		['HEAD_NUM',	'U1'],
		['SITE_NUM',	'U1'],
		['PART_CNT',	'U4'],
		['RTST_CNT',	'U4'],
		['ABRT_CNT',	'U4'],
		['GOOD_CNT',	'U4'],
		['FUNC_CNT',	'U4'],
	],
	'PGR' => [
		['GRP_INDX',	'U2'],
		['GRP_NAM',	'Cn'],
		['INDX_CNT',	'U2',	'k'],
		['PMR_INDX',	'U2',	'kx'],
	],
	'PIR' => [
		['HEAD_NUM',	'U1'],
		['SITE_NUM',	'U1'],
	],
	'PLR' => [
		['GRP_CNT',	'U2',	'k'],
		['GRP_INDX',	'U2',	'kx'],
		['GRP_MODE',	'U2',	'kx'],
		['GRP_RADX',	'U1',	'kx'],
		['PGM_CHAR',	'Cn',	'kx'],
		['RTN_CHAR',	'Cn',	'kx'],
		['PGM_CHAL',	'Cn',	'kx'],
		['RTN_CHAL',	'Cn',	'kx'],
	],
	'PMR' => [
		['PMR_INDX',	'U2'],
		['CHAN_TYP',	'U2'],
		['CHAN_NAM',	'Cn'],
		['PHY_NAM',	'Cn'],
		['LOG_NAM',	'Cn'],
		['HEAD_NUM',	'U1'],
		['SITE_NUM',	'U1'],
	],
	'PRR' => [
		['HEAD_NUM',	'U1'],
		['SITE_NUM',	'U1'],
		['PART_FLG',	'B1'],
		['NUM_TEST',	'U2'],
		['HARD_BIN',	'U2'],
		['SOFT_BIN',	'U2'],
		['X_COORD',	'I2'],
		['Y_COORD',	'I2'],
		['TEST_T',	'U4'],
		['PART_ID',	'Cn'],
		['PART_TXT',	'Cn'],
		['PART_FIX',	'Bn'],
	],
	'PSR' => [
		['REC_INDX',	'U1'],
		['REC_TOT',		'U1'],
		['PSR_INDX',	'U2'],
		['PSR_NAM',		'Cn'],
		['OPT_FLG',		'B1'],
		['TOTP_CNT',	'U2'],
		['LOCP_CNT',	'U2',	'k'],
		['PAT_BGN',		'U8',	'kx'],
		['PAT_END',		'U8',	'kx'],
		['PAT_FILE',	'Cn',	'kx'],
		['PAT_LBL',		'Cn',	'kx'],
		['FILE_UID',	'Cn',	'kx'],
		['ATPG_DSC',	'Cn',	'kx'],
		['SRC_ID',		'Cn',	'kx'],
	],
	'PTR' => [
		['TEST_NUM',	'Cn'],
		['HEAD_NUM',	'U1'],
		['SITE_NUM',	'U1'],
		['TEST_FLG',	'B1'],
		['PARM_FLG',	'B1'],
		['RESULT',	'R4'],
		['TEST_TXT',	'Cn'],
		['ALARM_ID',	'Cn'],
		['OPT_FLAG',	'B1'],
		['RES_SCAL',	'I1'],
		['LLM_SCAL',	'I1'],
		['HLM_SCAL',	'I1'],
		['LO_LIMIT',	'R4'],
		['HI_LIMIT',	'R4'],
		['UNITS',		'Cn'],
		['C_RESFMT',	'Cn'],
		['C_LLMFMT',	'Cn'],
		['C_HLMFMT',	'Cn'],
		['LO_SPEC',	'R4'],
		['HI_SPEC',	'R4'],
	],
	'RDR' => [
		['NUM_BINS',	'U2',	'k'],
		['RTST_BIN',	'U2',	'kx'],
	],
	'SBR' => [
		['HEAD_NUM',	'U1'],
		['SITE_NUM',	'U1'],
		['SBIN_NUM',	'U2'],
		['SBIN_CNT',	'U4'],
		['SBIN_PF',		'C1'],
		['SBIN_NAM',	'Cn'],
	],
	'SCR' => [
		['REC_INDX',	'U1'],
		['REC_TOT',		'U1'],
		['SCR_INDX',	'U2'],
		['CHN_NAM',		'Cn'],
		['TOTS_CNT',	'U2'],
		['LOCS_CNT',	'U2',	'k'],
		['SIN_PIN',		'U2'],
		['SOUT_PIN',	'U2'],
		['MSTR_CNT',	'U1',	'm'],
		['SLAV_CNT',	'U1',	'n'],
		['M_CLKS',		'U2',	'mx'],
		['S_CLKS',		'U2',	'nx'],
		['INV_VAL',		'U1'],
		['CELL_LST',	'Sn',	'kx'],
	],
	'SDR' => [
		['HEAD_NUM',	'U1'],
		['SITE_GRP',	'U1'],
		['SITE_CNT',	'U1',	'k'],
		['SITE_NUM',	'U1',	'kx'],
		['HAND_TYP',	'Cn'],
		['HAND_ID',	'Cn'],
		['CARD_TYP',	'Cn'],
		['CARD_ID',	'Cn'],
		['LOAD_TYP',	'Cn'],
		['LOAD_ID',	'Cn'],
		['DIB_TYP',	'Cn'],
		['DIB_ID',	'Cn'],
		['CABL_TYP',	'Cn'],
		['CABL_ID',	'Cn'],
		['CONT_TYP',	'Cn'],
		['CONT_ID',	'Cn'],
		['LASR_TYP',	'Cn'],
		['LASR_ID',	'Cn'],
		['EXTR_TYP',	'Cn'],
		['EXTR_ID',	'Cn'],
	],
	'SSR' => [
		['SSR_NAM',		'Cn'],
		['CHN_CNT',		'U2',	'k'],
		['CHN_LIST',	'U2',	'kx'],
	],		
	'STR' => [
		['REC_INDX',	'U1'],
		['REC_TOT',		'U1'],
		['TEST_NUM',	'U4'],
		['HEAD_NUM',	'U1'],
		['SITE_NUM',	'U1'],
		['PSR_REF',		'U2'],
		['TEST_FLG',	'B1'],
		['LOG_TYP',		'Cn'],
		['TEST_TXT',	'Cn'],
		['ALARM_ID',	'Cn'],
		['PROG_TXT',	'Cn'],
		['RSLT_TXT',	'Cn'],
		['Z_VAL',		'U1'],
		['FMU_FLG',		'B1'],
		['MASK_MAP',	'Dn'],
		['FAL_MAP',		'Dn'],
		['CYC_CNT',		'U8'],
		['TOTF_CNT',	'U4'],
		['TOTL_CNT',	'U4'],
		['CYC_BASE',	'U8'],
		['BIT_BASE',	'U2'],
		['DATA_FLG',	'B1'],
		['COND_CNT',	'U2',	'g'],
		['LOCL_CNT',	'U4',	'k'],
		['LIM_CNT',		'U2',	'j'],
		['DATA_BIT',	'U1'],
		['DATA_CHR',	'Cn'],
		['DATA_CNT',	'U2',	'm'],
		['USR1_LEN',	'U1',	'f'],
		['USR2_LEN',	'U1',	'a'],
		['USR3_LEN',	'U1',	'b'],
		['TXT_LEN',		'U1',	'c'],
		['LIM_INDX',	'U2',	'jx'],
		['LIM_SPEC',	'U4',	'jx'],
		['COND_NAM',	'Cn',	'gx'],
		['COND_VAL',	'Cn',	'gx'],
		['CYCL_NUM',	'U4',	'kx'],
		['PMR_INDX',	'U2',	'kx'],
		['CHN_NUM',		'U2',	'kx'],
		['CAP_DATA',	'U1',	'mx'],
		['EXP_DATA',	'U1',	'mx'],
		['NEW_DATA',	'U1',	'mx'],
		['PAT_NUM',		'U4',	'kx'],
		['BIT_POS',		'U4',	'kx'],
		['USR1',		'U4',	'kx'],	# U1,U2 or U4..?
		['USR2',		'U4',	'kx'],
		['USR3',		'U4',	'kx'],
		['TXT_LEN',		'C4',	'kx'],
	],	
	'TSR' => [
		['HEAD_NUM',	'U1'],
		['SITE_NUM',	'U1'],
		['TEST_TYP',	'C1'],
		['TEST_NUM',	'U4'],
		['EXEC_CNT',	'U4'],
		['FAIL_CNT',	'U4'],
		['ALRM_CNT',	'U4'],
		['TEST_NAM',	'Cn'],
		['SEQ_NAME',	'Cn'],
		['TEST_LBL',	'Cn'],
		['OPT_FLAG',	'B1'],
		['TEST_TIM',	'R4'],
		['TEST_MIN',	'R4'],
		['TEST_MAX',	'R4'],
		['TST_SUMS',	'R4'],
		['TST_SQRS',	'R4'],
	],
	'VUR' => [
		['UPD_NAM',	'Cn'],
	],
	'WCR' => [
		['WAFR_SIZ',	'R4'],
		['DIE_HT',	'R4'],
		['DIE_WID',	'R4'],
		['WF_UNITS',	'U1'],
		['WF_FLAT',	'C1'],
		['CENTER_X',	'I2'],
		['CENTER_Y',	'I2'],
		['POS_X',		'C1'],
		['POS_Y',		'C1'],
	],
	'WIR' => [
		['HEAD_NUM',	'U1'],
		['SITE_GRP',	'U1'],
		['START_T',	'U4'],
		['WAFER_ID',	'Cn'],
	],
	'WRR' => [
		['HEAD_NUM',	'U1'],
		['SITE_GRP',	'U1'],
		['FINISH_T',	'U4'],
		['PART_CNT',	'U4'],
		['RTST_CNT',	'U4'],
		['ABRT_CNT',	'U4'],
		['GOOD_CNT',	'U4'],
		['FUNC_CNT',	'U4'],
		['WAFER_ID',	'Cn'],
		['FABWF_ID',	'Cn'],
		['FRAME_ID',	'Cn'],
		['MASK_ID',	'Cn'],
		['USR_DESC',	'Cn'],
		['EXC_DESC',	'Cn'],
	],
);

# %STDF_RECORD_TYPE_TO_DATA_TYPE_CODE

my %STDF_RECORD_TYPE_TO_DATA_TYPE_CODE = (
	'Bn' => 'U1',
	'Cn' => 'U1',
	'Dn' => 'U2',
	'Vn' => 'U1',
);

# %STDF_TO_ATDF_DATA_TYPE_CODE_TO_SUBREF

my %STDF_TO_ATDF_DATA_TYPE_CODE_TO_SUBREF = (
	'B1' => \&S2A_B1,
	'Bn' => \&S2A_Bn,
	'C1' => \&S2A_C1,
	'Cn' => \&S2A_Cn,
	'Dn' => \&S2A_Dn,
	'I1' => \&S2A_I1,
	'I2' => \&S2A_I2,
	'I4' => \&S2A_I4,
	'N1' => \&S2A_N1,
	'R4' => \&S2A_R4,
	'R8' => \&S2A_R8,
	'U1' => \&S2A_U1,
	'U2' => \&S2A_U2,
	'U4' => \&S2A_U4,
	'U8' => \&S2A_U8,
	'Vn' => \&S2A_Vn,
);

########################################################################
# PRIVATE MODULE VARIABLE DECLARATIONS
########################################################################

# %gOS
#
# Contains three elements:
#
#	$gOS{'NAME'}
#		The name of the operating system that Perl was compiled
#		on (the same as the Perl special variable $^O).
#
#	$gOS{'CPU_TYPE'}
#		The STDF code for the CPU type of the computer that is
#		executing this module.
#
#	$gOS{'ENDIAN'}
#		A literal indicating the endian value of the operating
#		system that is executing this module.  Allowed values are
#		'ENDBIG' for big endian and 'ENDLIT' for little endian.

my %gOS = (
	'NAME' => $^O,
	'CPU_TYPE' => undef,
	'ENDIAN' => undef,
);

$gOS{'CPU_TYPE'} = $OS_NAME_TO_CPU_TYPE{$gOS{'NAME'}};
if ($gOS{'CPU_TYPE'} eq '')
{
	fatal(
		'Cannot create STDF file',
		'for operating system:', $gOS{'NAME'}
	);
};

$gOS{'ENDIAN'} = $OS_NAME_TO_ENDIAN{$gOS{'NAME'}};
if ($gOS{'ENDIAN'} eq '')
{
	fatal(
		'Float conversion is not implemented',
		'for operating system:', $gOS{'NAME'}
	);
};

# %gEndian
#
# Contains two elements:
#
#	$gEndian{'ATDF'}
#		The endian value of the ATDF file.
#
#	$gEndian{'STDF'}
#		The endian value of the STDF file.

my %gEndian = (
	'ATDF' => $gOS{'ENDIAN'},
	'STDF' => $gOS{'ENDIAN'},
);

########################################################################
# PRIVATE UTILITY SUBROUTINES
########################################################################

########################################################################
# PUBLIC UTILITY SUBROUTINES
########################################################################

=pod

=head1 UTILITY SUBROUTINES

=cut

=pod

=head2 S<about($program, $version)>

Outputs an about message to standard output.

=cut

sub about
{
	my $program = shift();
	my $version = shift();
	print <<"END_ABOUT";
$program Version $version
Copyright (C) 2004-2005 Michael Hackerott. All rights reserved
Email: michael.hackerott\@mrhackerott.org

This program is free software; you can redistribute it and/or modify it
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
END_ABOUT

	# exit to unix without error
	exit(0);
};

=pod

=head2 S<alert($keyword, @text)>

Outputs an alert message to a log file and returns the message.  An
alert message is formatted as:

	PROGRAM_NAME *KEYWORD* TEXT

=cut

sub alert
{
	my $alert = sprintf(
		"%s *%s* %s",
		$0,
		shift(),
		join(SPC, @_)
	);
	loggText($alert);
	return($alert);
};

=pod

=head2 S<$boolean = bitand($int1, $int2)>

Performs a bitwise AND of two integers and returns true if the result
contains one or more bits that are set to 1.

=cut

sub bitand
{
	my $bitand = (($_[0] & $_[1]) > 0) || 0;
	#dbugText('bitand('.join(', ', @_).')', '=', $bitand);
	return($bitand);
};

=pod

=head2 S<$hex = binToHexStr($bin)>

Converts a binary string to a hexadecimal string.  For example, the
binary string "01001010" would be converted to the hexadecimal string
"4A".

=cut

sub binToHexStr
{
	my $bin = shift();
	my %bin2hex = (
		'0000' => '0',
		'0001' => '1',
		'0010' => '2',
		'0011' => '3',
		'0100' => '4',
		'0101' => '5',
		'0110' => '6',
		'0111' => '7',
		'1000' => '8',
		'1001' => '9',
		'1010' => 'A',
		'1011' => 'B',
		'1100' => 'C',
		'1101' => 'D',
		'1110' => 'E',
		'1111' => 'F',
	);
	my $hex = '';
	my $i = 0;
	while ($i < length($bin ))
	{
		$hex .= $bin2hex{substr($bin, $i, 4)};
		$i += 4;
	};
	return($hex);
};

=pod

=head2 S<$seconds = dateTimeToSeconds($datetime)>

Converts a string containing a date and time value formatted as
"[h]h:[m]m:[s]s [d]d-MMM-YYYY" to epoch seconds.

=cut

sub dateTimeToSeconds
{
	my $datetime = shift();
	#dbugData([$datetime], ['*dateTimeToSeconds::datetime']);
	($datetime =~ m/^(.?.):(.?.):(.?.) (.?.)-(...)-(....)$/) || return;
	my %month = (
		'JAN' => 0,
		'FEB' => 1,
		'MAR' => 2,
		'APR' => 3,
		'MAY' => 4,
		'JUN' => 5,
		'JUL' => 6,
		'AUG' => 7,
		'SEP' => 8,
		'OCT' => 9,
		'NOV' => 10,
		'DEC' => 11
	);
	#my $seconds = timegm(
	my $seconds = timelocal(
		$3,				# [s]s
		$2,				# [m]m
		$1,				# [h]h
		$4,				# [d]d
		$month{uc($5)},	# MMM
		($6 - 1900),		# YYYY
	);
	#dbugData([$seconds], ['*dateTimeToSeconds::seconds']);
	return($seconds);
};

=pod

=head2 S<error(@text)>

Concatenates the values of the elements of the list @text using spaces
into a single string formatted as a standard error message and outputs
the error message to standard error.  The format of the error message
is:

	ERROR MESSAGE

=cut

sub error
{
	stderr(alert('ERROR', @_));
};

=pod

=head2 S<fatal(@text)>

Concatenates the values of the elements of the list @text using spaces
into a single string formatted as a standard fatal message, outputs the
fatal message to standard error, closes the open input file, and exits
the executing program to the command line.  The format of the fatal
message is:

	FATAL MESSAGE

=cut

sub fatal
{
	stderr(alert('FATAL', @_));
	close(FILE_INP);
	exit(1);
};

=pod

=head2 S<$bin = hexToBinStr($hex)>

Converts a hexadecimal string to a binary string.  For example, the
hexadecimal string "4A" would be converted to the binary string
"01001010".

=cut

sub hexToBinStr
{
	my %hex2bin = (
		'0' => '0000',
		'1' => '0001',
		'2' => '0010',
		'3' => '0011',
		'4' => '0100',
		'5' => '0101',
		'6' => '0110',
		'7' => '0111',
		'8' => '1000',
		'9' => '1001',
		'A' => '1010',
		'B' => '1011',
		'C' => '1100',
		'D' => '1101',
		'E' => '1110',
		'F' => '1111',
	);
	return(join('', map($hex2bin{$_}, split(//, shift()))));
};

=pod

=head2 S<$boolean = isAtdfEndianBig()>

Tests if the value of $gEndian{'ATDF'} is 'ENDBIG' and returns the
boolean result of the test.

=cut

sub isAtdfEndianBig
{
	return($gEndian{'ATDF'} eq 'ENDBIG');
};

=pod

=head2 S<$boolean = isAtdfEndianLittle()>

Tests if the value of $gEndian{'ATDF'} is 'ENDLIT' and returns the
boolean result of the test.

=cut

sub isAtdfEndianLittle
{
	return($gEndian{'ATDF'} eq 'ENDLIT');
};

=pod

=head2 S<$boolean = isOSEndianBig()>

Tests if the value of $gOS{'ENDIAN'} is 'ENDBIG' and returns the
boolean result of the test.

=cut

sub isOSEndianBig
{
	return($gOS{'ENDIAN'} eq 'ENDBIG');
};

=pod

=head2 S<$boolean = isOSEndianLittle()>

Tests if the value of $gOS{'ENDIAN'} is 'ENDLIT' and returns the
boolean result of the test.

=cut

sub isOSEndianLittle
{
	return($gOS{'ENDIAN'} eq 'ENDLIT');
};

=pod

=head2 S<$boolean = isStdfEndianBig()>

Tests if the value of $gEndian{'STDF'} is 'ENDBIG' and returns the
boolean result of the test.

=cut

sub isStdfEndianBig
{
	return($gEndian{'STDF'} eq 'ENDBIG');
};

=pod

=head2 S<$boolean = isStdfEndianLittle()>

Tests if the value of $gEndian{'STDF'} is 'ENDLIT' and returns the
boolean result of the test.

=cut

sub isStdfEndianLittle
{
	return($gEndian{'STDF'} eq 'ENDLIT');
};

=pod

=head2 S<loggText(@text)>

Concatenates the values of the elements of the list @text using spaces
into a single string formatted as a standard log message and then
outputs the message to standard out.  The log message is formatted as:

	[PROGRAM_NAME] MESSAGE

=cut

sub loggText
{
	stdout(sprintf("[%s] %s", $0, join(SPC, @_)));
};

=pod

=head2 S<osCpuType()>

Returns the CPU type of the operating system as an STDF CPU_TYPE field
code.

=cut

sub osCpuType
{
	return($gOS{'CPU_TYPE'});
};

=pod

=head2 S<$singleQuotedText = q1(@text)>

Concatenates the values of the elements of the list @text using spaces
into a single string formatted and returns the string surrounded by
single quotation characters.

=cut

sub q1
{

	return(Q1.join(SPC, @_).Q1);
};

=pod

=head2 S<reverseEndian(\$x)>

Tests to determine if the endian of the STDF and the operating system
are not equal and reverses the byte sequence of the STDF value to match
the endian-ness of the operating system.

This is required to interpret floating point numbers correctly.

=cut

sub reverseEndian
{
	my $rsStdX = shift();
	if (isStdfEndianBig())
	{
		#dbugDataBin('$reverseEndian::binary.0', ${$rsStdX});
		${$rsStdX} = join('', reverse(split(//, ${$rsStdX})));
		#dbugDataBin('$reverseEndian::binary.1', ${$rsStdX});
	};
};

=pod

=head2 S<$scaleFactor = scaleFactor($units)>

=cut

sub scaleFactor
{
	my $units = shift();
	my %scalePrefixToFactor = (
		'f' => 15,
		'p' => 12,
		'n' => 9,
		'u' => 6,
		'm' => 3,
		'%' => 2,
		'K' => -3,
		'M' => -6,
		'G' => -9,
		'T' => -12,
	);
	return($scalePrefixToFactor{substr($units, 0, 1)} || 0);
};

=pod

=head2 S<$datetime = secondsToDateTime($seconds)>

Converts an epoch seconds value to a string containing a date and time
value formatted as "[h]h:[m]m:[s]s [d]d-MMM-YYYY".

=cut

sub secondsToDateTime
{
	my $seconds = shift();
	#dbugData([$seconds], ['*secondsToDateTime::seconds']);
	my $sec = undef;
	my $min = undef;
	my $hr = undef;
	my $mday = undef;
	my $mon = undef;
	my $year = undef;
	my $wday = undef;
	my $yday = undef;
	my $isdst = undef;
	my %mon = (
		0  => 'JAN',
		1  => 'FEB',
		2  => 'MAR',
		3  => 'APR',
		4  => 'MAY',
		5  => 'JUN',
		6  => 'JUL',
		7  => 'AUG',
		8  => 'SEP',
		9  => 'OCT',
		10 => 'NOV',
		11 => 'DEC',
	);
	(
		$sec,
		$min,
		$hr,
		$mday,
		$mon,
		$year,
		$wday,
		$yday,
		$isdst
	#) = gmtime($seconds);	# align with dateTimeToSeconds, uses timelocal !!
	) = localtime($seconds);
	my $datetime = sprintf(
		"%02d:%02d:%02d %02d-%s-%04d",
		$hr, $min, $sec,
		$mday, $mon{$mon}, 1900 + $year
	);
	#dbugData([$datetime], ['*secondsToDateTime::datetime']);
	return($datetime);
};

=pod

=head2 S<setBitFlag(\$str, \$map, \$flg)>

=cut

sub setBitFlag
{
	my $rsStr = shift();
	my $rhMap = shift();
	my $rsFlg = shift();
	my $i = 0;
	my $n = length(${$rsStr});
	($n > 0) || return;
	my $flg = undef;
	while ($i < $n)
	{
		if ($flg = $rhMap->{substr(${$rsStr}, $i, 1)})
		{
			${$rsFlg} |= $flg;
		};
		$i += 1;
	};
};

=pod

=head2 S<stderr(@text)>

Concatenates the values of the elements of the list @text using spaces
into a single string and prints the string to standard error.

=cut

sub stderr
{
	print STDERR join(SPC, @_).EOL;
};

=pod

=head2 S<stdout(@text)>

Concatenates the values of the elements of the list @text using spaces
into a single string and prints the string to standard output.

=cut

sub stdout
{
	print STDOUT join(SPC, @_).EOL;
};

=pod

=head2 S<trim(\$text)>

Deletes the leading and trailing white space characters from the text
string.

=cut

sub trim
{
	my $rsText  = shift();
	${$rsText} =~ s/^\s+//; # delete leading white space(s)
	${$rsText} =~ s/\s+$//; # delete trailing white space(s)
};

########################################################################
# PUBLIC ATDF UTILITY SUBROUTINES
########################################################################

=pod

=head1 ATDF UTILITY SUBROUTINES

=cut

=head2 S<$atdfFieldName = atdfFieldName($atdfRecordName, $atdfFieldIndex)>

Returns the ATDF field name for the specified ATDF record name and
field index.

=cut

sub atdfFieldName
{
	my $atdfRecordName = shift();
	my $atdfFieldIndex = shift();
	return($ATDF_RECORD_FIELDS{$atdfRecordName}[$atdfFieldIndex]);
};

=head2 S<$atdfFieldNameIndexLast = atdfFieldNameIndexLast($atdfRecordName)>

Returns a the index of the last field name in the ordered list of the
ATDF field names for the specified ATDF record name.

=cut

sub atdfFieldNameIndexLast
{
	my $atdfRecordName = shift();
	return($#{$ATDF_RECORD_FIELDS{$atdfRecordName}});
};

=pod

=head2 S<atdfFormatReal(\$real)>

=cut

sub atdfFormatReal
{
	# maybe this whole subroutine should be replaced with
	# ${$rsReal} = sprintf(".9e",$rsReal);
	
	my $rsReal = shift();
	#dbugData([${$rsReal}], ['*atdfFormatReal::real']);
	${$rsReal} =~ m/^(\-)?(\d*)?(\.)?(\d*)?[eE]?(\-)?(\d*)?$/; # add exponent stuff dcg
	my $sgn = $1;
	#dbugData([$sgn], ['*atdfFormatReal::sgn']);
	my $int = $2;
	#dbugData([$int], ['*atdfFormatReal::int']);
	my $dot = $3;
	#dbugData([$dot], ['*atdfFormatReal::dot']);
	my $dec = $4;
	#dbugData([$dec], ['*atdfFormatReal::dec']);
	my $exp_sgn = $5;	# added exp stuff -- dcg
	my $exp = $6;		# added exp stuff -- dcg
	#if ($int eq '0') -- like 0.1 instead of .1 dcg, 18aug06
	#{
	#	$int = '';
	#};
	# stdf -> atdf -> stdf some rounding error at 6 decimals, so try 9, may2014
	if (length($dec) > 9)
	{
		## want to round, not truncate!  dcg
		$dec = substr($dec, 0, 10);
		$dec = $dec + 5;
		$dec = $dec / 10000000000.0; 

		# also, bug when 1.99999 becomes 2.0000 wasn't handled correctly
		#   gave you 1.100000 instead
		if ($dec > 1.0) 
		{
			$int = $int + 1;
		}
		else
		{
			$dec = $dec + 1.0;	# to stop exponent changing
		}
		$dec =~ m/^(\-)?(\d*)?(\.)?(\d*)?[eE]?(\-)?(\d*)?$/;
		$dec = $4;
		if (length($dec) > 9)
		{
			$dec = substr($dec, 0, 9);
		}
	};
	my @dec = split(//, $dec);
	while ( (@dec[-1] eq '0') && ($#dec > 0) )
	{
		pop(@dec);
	};
	my $real = $sgn.$int.$dot.join('', @dec);
	if ($exp =~ m/\d+/)  # dcg added this loop
	{
		$real = $real."e".$exp_sgn.$exp;
	}
	#dbugData([$real], ['*atdfFormatReal::real']);
	${$rsReal} = $real;
};

=pod

=head2 S<@atdfFieldNames = atdfRecordFieldNames($atdfRecordName)>

Returns an ordered list of the ATDF field names for the specified ATDF
record name.

=cut

sub atdfRecordFieldNames
{
	my $atdfRecordName = shift();
	return(@{$ATDF_RECORD_FIELDS{$atdfRecordName}});
};

=pod

=head2 S<atdfRecordWrap($atdfRecordName, \@atdfRecord)>

Wraps the ATDF record to a maximum of 80 characters per line.

=cut

sub atdfRecordWrap
{
	my $atdfRecordName = shift();
	my $raAtdfRecord = shift();

	my @l = ($atdfRecordName.':');
	my $i = 0;
	my $n = $#{$raAtdfRecord};
	my $z = undef;
	my $w = undef;

	while ($i <= $n)
	{
		$z = $raAtdfRecord->[$i].('|' x ($i < $n));
		$w = length($l[-1]) + length($z);
		if ($w < 80)
		{
			$l[-1] .= $z;
		}
		elsif ($w == 80)
		{
			$l[-1] .= $z;
			push(@l, ' ');
		}
		else # $w > 80
		{
			push(@l, ' '.$z);
		};
		$i += 1;
	};
	return(join(EOL, @l).EOL);
};

########################################################################
# PRIVATE ATDF TO STDF DATA TYPE SUBROUTINES
########################################################################

# $binary = A2S_pack($template, $ascii)

sub A2S_pack
{
	#print 'caller = '.(caller(1))[3]."\n";
	my $template = shift();
	my $ascii = shift();
	my $binary = pack($template, $ascii);
	#dbugDataBin('$A2S_pack::binary', $binary);
	return($binary);
};

########################################################################
# PUBLIC ATDF TO STDF DATA TYPE SUBROUTINES
########################################################################

=pod

=head1 ATDF TO STDF DATA TYPE SUBROUTINES

STDF binary values are always written to the STDF file in little endian
format independent of the runtime or target operating systems!

=cut

=pod

=head2 S<$result = A2S_do($stdfDataTypeCode, @arguments)>

Executes the ATDF to STDF data type conversion subroutine for the
specified STDF data type code and arguments and returns the result.

=cut

sub A2S_do
{
	my $stdfDataTypeCode = shift();
	my $r = $ATDF_TO_STDF_DATA_TYPE_CODE_TO_SUBREF{$stdfDataTypeCode};
	return($r->(@_));
};

=pod

=head2 S<$binary = A2S_B0()>

Returns an STDF B*0 null binary byte.

=cut

sub A2S_B0
{
	return(pack('x'));
};

=pod

=head2 S<$binary = A2S_B1($bitNumber)>

Converts an ATDF numeric value between 0 and 255 to an STDF B*1 data type
which is a one byte bit encoded value:

	b7 b6 b5 b4 b3 b2 b1 b0

therefore the value can be packed directly without any conversion.

=cut

sub A2S_B1
{
	my $bitnum = shift();
	if ($bitnum eq '')
	{
		fatal(qw(
			B*1
			argument
			value
			is
			null
		));
	}
	elsif ($bitnum > 0xFF)
	{
		fatal(
			'B*1',
			'argument',
			'value',
			q1($bitnum),
			'is',
			'larger',
			'than',
			'one',
			'byte.'
		);
	};
	return(A2S_pack('C', $bitnum));
};

=pod

=head2 S<$binary = A2S_Bn($hexNumber)>

Converts an ATDF hexadecimal number to an STDF B*n binary value.

=cut

sub A2S_Bn
{
	my $hexnum = shift();
	trim(\$hexnum);
	$hexnum =~ m/^(0x|x)?([0-9A-F]+)$/i;
	$hexnum = $2;
	#dbugData([$hexnum], ['*A2S_Bn::hexnum']);
	my $n = length($hexnum);
	if ($n % 2)
	{
		$hexnum = '0'.$hexnum;
		$n += 1;
	};
	my $i = undef;
	my $binary = undef;
	for ($i = 0; $i < $n; $i += 2)
	{
		$binary .= pack('H2', substr($hexnum, $i, 2));
	};
	if (defined($n)) 
	{
		return(pack('C', $n/2).$binary);
	}
	else 
	{
		return(pack('C', 0));
	}
};

=pod

=head2 S<$binary = A2S_C1($ascii)>

Converts an ATDF character value to an STDF C*1 binary value.

=cut

sub A2S_C1
{
	return(A2S_pack('A', shift()));
};


=pod

=head2 S<$binary = A2S_C2($ascii)>

Converts an ATDF character value to an STDF C*2 binary value.

=cut

sub A2S_C2
{
	return(A2S_pack('A'.2, shift()));	# REVISIT...
};


=pod

=head2 S<$binary = A2S_C4($ascii)>

Converts an ATDF character value to an STDF C*4 binary value.

=cut

sub A2S_C4
{
	return(A2S_pack('A'.4, shift()));	# REVISIT... 
};


=pod

=head2 S<$binary = A2S_Cn($ascii)>

Converts an ATDF character string of one or more ASCII characters to an
STDF C*n binary value where the first byte is the number of bytes in the
string followed by one byte for each character in the string.

=cut

sub A2S_Cn
{
	my $ascii = shift();
	#trim(\$ascii);  # we actually want to drag along the whitespace?
	my $n = length($ascii);
	my $binary = pack('x');
	if (defined($n) && ($n > 0) ) 
	{
		$binary = A2S_pack('C', $n);
		$binary .= A2S_pack('a'.$n, $ascii);
	};
	return($binary);
};

=pod

=head2 S<$binary = A2S_Cn0($ascii)>

Converts an ATDF character string of one or more ASCII characters to an
STDF C*n binary value where the first byte is the number of bytes in the
string followed by one byte for each character in the string.

=cut

sub A2S_Cn0
{
	my $ascii = shift();
	trim(\$ascii);
	my $n = length($ascii);

	# special case, if string is length 1, and character is '0', 
	# $ascii is misinterpreted as NULL, not '0'
	# hard code this case as a special function Cn0...

	$n = 1;
	my $binary = A2S_pack('C', $n);

	$binary .= A2S_pack('C', 48);	# ascii 48 = '0'
	return($binary);
};

=pod

=head2 S<$binary = A2S_Dn(@positiveIntegerNumbers)>

Converts a list of ATDF positive non-zero integer numbers to an STDF D*n
binary bit field.  The bit field is organized as:

	Byte:		0       1        2         ...
	Bit:		7 ... 0 15 ... 8 23 ... 9  ...
	Integer:	8 ... 1 16 ... 9 24 ... 17 ...

Where each bit in a byte corresponds to a positive integer value. The
maximum positive integer value is 65535.  For example, a list containing
2, 8, 11, and 15 would be encoded into a two byte bit field as:

	Bit: 	10000010 01000100

Returns a binary string formatted as:

	[2:bit count][N:bit field]

Where N = bit count.

=cut

sub A2S_Dn
{
	my @intnum = sort {$a <=> $b} @_;
	if (grep($_ > 65535, @intnum) > 0)
	{
		fatal(
			'A2S_Dn integer number greater than 65535!'
		);

	};
	my %bitlut = (
		0 => 0x01,
		1 => 0x02,
		2 => 0x04,
		3 => 0x08,
		4 => 0x10,
		5 => 0x20,
		6 => 0x40,
		7 => 0x80,
	);
	my $bitcnt = 0;
	my @bitfld = ();
	for my $i (0 .. $#intnum)
	{
		#print "\$A2S_Dn::intnum[$i] \= \'".$intnum[$i]."\'\n";
		if ($intnum[$i] > 0)
		{
			if ($intnum[$i] > $bitcnt)
			{
				$bitcnt = $intnum[$i];
			};
			my $bitnum = $intnum[$i] - 1;
			#print "\$A2S_Dn::bitnum \= \'".$bitnum."\'\n";
			my $bytndx = int($bitnum / 8);
			#print "\$A2S_Dn::bytndx \= \'".$bytndx."\'\n";
			my $bitndx = ($bitnum % 8);
			#print "\$A2S_Dn::bitndx \= \'".$bitndx."\'\n";
			my $bitval = $bitlut{$bitndx};
			#print "\$A2S_Dn::bitfld \= \'".$bitfld."\'\n";
			$bitfld[$bytndx] |= $bitval;
		};
	};
	if (isStdfEndianBig())
	{
		return(pack('n', $bitcnt).pack('C'.scalar(@bitfld), @bitfld));
	}
	else
	{
		return(pack('v', $bitcnt).pack('C'.scalar(@bitfld), @bitfld));
	}
};

=pod

=head2 S<$binary = A2S_I1($signedByte)>

Converts an ATDF one byte signed integer to an STDF I*1 binary.

=cut

sub A2S_I1
{
	return(A2S_pack('c', shift()));
};

=pod

=head2 S<$binary = A2S_I2($signedShort)>

Converts an ATDF two byte signed integer (i.e., a short) to an STDF I*2
binary.

=cut

sub A2S_I2
{
	if (isStdfEndianBig())
	{
		return(A2S_pack('n', shift()));
	}
	else
	{
		return(A2S_pack('v', shift()));
	}

};

=pod

=head2 S<$binary = A2S_I4($signedLong)>

Converts an ATDF four byte signed integer (i.e., a long) to an STDF I*4
binary value.

=cut

sub A2S_I4
{
	if (isStdfEndianBig())
	{
		return(A2S_pack('N', shift()));
	}
	else
	{
		return(A2S_pack('V', shift()));
	}

};

=pod

=head2 S<$binary = A2S_N1($hexchr)>

Converts one or two ATDF hexadecimal digits to an STDF N*1 binary value.

The STDF N*1 data type represents an unsigned integer data stored as a
nibble (nibble = 4 bits of a byte).  N1 argument values are always one
or two ASCII hexadecimal digits.  The packed binary must be a full byte.
Therefore, if there is only one argument digit, then a '0' is prepended
to make two digits or one byte.  For example, if the argument is 'A'
then '0A' is packed into the byte.

=cut

sub A2S_N1
{
	# get subroutine arguments
	my $hexnum = shift();

	# define local variables
	my $n = length($hexnum);

	# test the HEX argument
	if ($n == 0)
	{
		fatal(
			'N*1 argument value',
			'is null.'
		);
	}
	elsif ($n > 2)
	{
		fatal(
			'N*1 argument value',
			'contains more than two hexadecimal digits:',
			$hexnum
		);
	};

	# return the binary
	return(A2S_Nn($hexnum));
};

=pod

=head2 S<$binary = A2S_Nn($hexnum)>

Converts a string of one or more hexadecimal digits to an binary value.
If the hexadecimal string contains an odd number of digits then a zero
is pre-pended to the right of the string to create a binary value with
an even number of bytes.

=cut

sub A2S_Nn
{
	my $hexnum = shift();
	my $n = length($hexnum);
	if ($n == 0)
	{
		fatal(
			'N*n argument value',
			'is null.'
		);
	}
	elsif ($hexnum !~ m/^[0-9ABCDEF]+$/i)
	{
		fatal(
			'N*n argument value',
			'is not a hexadecimal number:',
			$hexnum
		);
	};
	if ($n % 2)
	{
		$hexnum = '0'.$hexnum;
	};
#	return(pack('H*', $hexnum));
	return(pack('h*', $hexnum));
};

# $binary = A2S_real($packTemplate, $real)

sub A2S_real
{
	my $pktmpl = shift();
	my $real = shift();
	my $binary = A2S_pack($pktmpl, $real);
	reverseEndian(\$binary);
	return($binary);
};

=pod

=head2 S<$binary = A2S_R4($realSingle)>

Converts an ATDF four byte signed real (i.e., a single) to an STDF R*4
binary value.

=cut

sub A2S_R4
{
	return(A2S_real('f', shift()));	# calling A2S_real, endian dealt with there.
};

=pod

=head2 S<$binary = A2S_R8($realDouble)>

Converts an ATDF eight byte signed real (i.e., a double) to an STDF R*8
binary value.

=cut

sub A2S_R8
{
	return(A2S_real('d', shift()));	# calling A2S_real, endian dealt with there.
};

=pod

=head2 S<$binary = A2S_U1($unsignedByte)>

Converts an ATDF one byte unsigned integer value to an STDF U*1 binary
value.

=cut

sub A2S_U1
{
	return(A2S_pack('C', shift()));
};

=pod

=head2 S<$binary = A2S_U2($unsignedShort)>

Converts an ATDF two byte unsigned integer (i.e., a short) to an STDF
U*2 binary value.

=cut

sub A2S_U2
{
	if (isStdfEndianBig())
	{
		return(A2S_pack('n', shift()));
	}
	else
	{
		return(A2S_pack('v', shift()));
	}
};

=pod

=head2 S<$binary = A2S_U4($unsignedLong)>

Converts an ATDF four byte unsigned integer (i.e., a long) to an STDF
U*4 binary value.

=cut

sub A2S_U4
{
	if (isStdfEndianBig())
	{
		return(A2S_pack('N', shift()));
	}
	else
	{
		return(A2S_pack('V', shift()));
	}
};

=pod

=head2 S<$binary = A2S_xTYPE(\&type, \@data)>

Converts the value of each element of the array referenced by \@data to
binary using the ATDF to STDF data type conversion subroutine reference
by \&type and returns a string containing the concatenated binary
values.

=cut

sub A2S_xTYPE
{
	my $rType = shift();
	my $raData = shift();
	my $binary = undef;
	my $n = scalar(@{$raData});
	if ($n > 0)
	{
		for my $i (0 .. $#{$raData})
		{
			$binary .= $rType->($raData->[$i]);
		};
	};
	return($binary);
};

########################################################################
# PUBLIC STDF UTILITY SUBROUTINES
########################################################################

=pod

=head1 STDF UTILITY SUBROUTINES

=cut

=pod

=head2 S<$stdfEndian = stdfEndian([$stdfCpuType])>

Returns the STDF endian keyword and optionally sets the STDF endian
keyword if a valid STDF CPU_TYPE is specified.

=cut

sub stdfEndian
{
	my $stdfCpuType = shift();
	if (exists($STDF_CPU_TYPE_TO_ENDIAN{$stdfCpuType}))
	{
		$gEndian{'STDF'} = $STDF_CPU_TYPE_TO_ENDIAN{$stdfCpuType};
	};
	return($gEndian{'STDF'});
};

=pod

=head2 S<$stdfFieldName = stdfFieldName($stdfRecordName, $stdfFieldIndex)>

Returns the STDF field name for the specified STDF record name and
field index.

=cut

sub stdfFieldName
{
	my $stdfRecordName = shift();
	my $stdfFieldIndex = shift();
	return($STDF_RECORD_FIELDS{$stdfRecordName}[$stdfFieldIndex][0]);
};

=pod

=head2 S<$stdfFieldNameIndexLast = stdfFieldNameIndexLast($stdfRecordName)>

Returns a the index of the last field name in the ordered list of the
STDF field names for the specified STDF record name.

=cut

sub stdfFieldNameIndexLast
{
	my $stdfRecordName = shift();
	return($#{$STDF_RECORD_FIELDS{$stdfRecordName}});
};

=pod

=head2 S<@stdfFieldNames = stdfRecordFieldNames($stdfRecordName)>

Returns an ordered list of the STDF field names for the specified STDF
record name.

=cut

sub stdfRecordFieldNames
{
	my $stdfRecordName = shift();
	my $i = undef;
	my $n = stdfFieldNameIndexLast($stdfRecordName);
	my @stdfRecordFieldNames = ();
	for $i (0 .. $n)
	{
		push(
			@stdfRecordFieldNames,
			$STDF_RECORD_FIELDS{$stdfRecordName}[$i][0]
		);
	};
	return(@stdfRecordFieldNames);
};

=pod

=head2 S<($stdfRecTypeCode, $stdfRecSubTypeCode) = stdfRecordNameToTypeCodes($stdfRecName)>

=cut

sub stdfRecordNameToTypeCodes
{
	my $stdfRecName = shift();
	my $r = $STDF_RECORD_NAME_TO_TYPE_CODES{$stdfRecName};
	my $stdfRecTyp = $r->[0];
	my $stdfRecSub = $r->[1];
	return($stdfRecTyp, $stdfRecSub);
};

=pod

=head2 S<$stdfRecName = stdfRecordTypeCodesToName($stdfRecTyp, $stdfRecSub)>

=cut

sub stdfRecordTypeCodesToName
{
	my $stdfRecTyp = shift();
	my $stdfRecSub = shift();
	return($STDF_RECORD_TYPE_CODES_TO_NAME{$stdfRecTyp}{$stdfRecSub});
};

=pod

=head2 S<$stdfRecordTypeToDataTypeCode = stdfRecordTypeToDataTypeCode($stdfRecordType)>

Returns a the index of the last field name in the ordered list of the
STDF field names for the specified STDF record name.

=cut

sub stdfRecordTypeToDataTypeCode
{
	my $stdfRecordType = shift();
	return($STDF_RECORD_TYPE_TO_DATA_TYPE_CODE{$stdfRecordType});
};

########################################################################
# PRIVATE STDF TO ATDF DATA TYPE SUBROUTINES
########################################################################

# $ascii = S2A_unpack($template, $binary)

sub S2A_unpack
{
	#dbugSub('S2A_unpack', $_[0]);
	my $template = shift();
	my $binary = shift();
	my $ascii = unpack($template, $binary);
	#dbugData([$ascii], ['*S2A_unpack::ascii']);
	return($ascii);
};

########################################################################
# PUBLIC STDF TO ATDF DATA TYPE SUBROUTINES
########################################################################

=pod

=head1 STDF TO ATDF DATA TYPE SUBROUTINES

STDF binary values are always written to the STDF file in little endian
format independent of the runtime or target operating systems and are
byte swapped for compatibility with the runtime operating system.

=cut

=pod

=head2 S<$result = S2A_do($stdfDataTypeCode, @arguments)>

Executes the STDF to ATDF data type conversion subroutine for the
specified STDF data type code and arguments and returns the result.

=cut

sub S2A_do
{
	my $stdfDataTypeCode = shift();
	my $r = $STDF_TO_ATDF_DATA_TYPE_CODE_TO_SUBREF{$stdfDataTypeCode};
	return($r->(@_));
};

=pod

=head2 S<$hexnum = S2A_B1($binary)>

Converts an STDF B*1 binary value to an ATDF bit number value.

=cut

sub S2A_B1
{
	return(S2A_unpack('C', shift()));
};

=pod

=head2 S<$hexnum = S2A_Bn($binary)>

Converts an STDF B*n binary value to an ATDF bit number string value.

=cut

sub S2A_Bn
{
	my $binary = shift();
	my $n = 2 * S2A_unpack('C', substr($binary, 0, 1));	# byte = 2 char! 00 to FF
	my $ascii;

#	return(uc(S2A_unpack('H'.$n, substr($binary, 1))));
	if ($n > 0)
	{
		$ascii = uc(S2A_unpack('H'.$n, substr($binary, 1)));
	} else {
		$ascii = "";
	};
	return($ascii);
};

=pod

=head2 S<$char = S2A_C1($binary)>

Converts an STDF C*1 binary value to an ATDF character value.

=cut

sub S2A_C1
{
	my $ascii = S2A_unpack('A', shift());
	if ($ascii eq SPC)
	{
		$ascii = undef;
	};
	return($ascii);
};

=pod

=head2 S<$text = S2A_Cn($binary)>

Converts an STDF C*n binary value to an ATDF character string.

=cut

sub S2A_Cn
{
	my $binary = shift();
	my $n = S2A_U1(substr($binary, 0, 1));
	#dbugData([$n], ['*S2A_Cn::n']);
	my $text = undef;
	if ($n > 0)
	{
		#$text = S2A_unpack('A'.$n, substr($binary, 1));
		$text = S2A_unpack('a'.$n, substr($binary, 1));	# allow trailing whitespace!
	};

	# dcg -- june 2013
	# if string has "\n"s, DTR will wrap to next line in atdf
	# ... it needs to have a leading space to signify a continued record
	#$text =~ s/\r/\r /g;
	$text =~ s/\n/\n /g;

	return($text);
};

=pod

=head2 S<@intnum = S2A_Dn($binary)>

Converts an STDF binary bit field to a list of ATDF positive non-zero integer
numbers.  (See A2S_Dn for a detailed explanation.)

=cut

sub S2A_Dn
{
	my $binary = shift();
	my %bitfld = (
		0 => 0x01,
		1 => 0x02,
		2 => 0x04,
		3 => 0x08,
		4 => 0x10,
		5 => 0x20,
		6 => 0x40,
		7 => 0x80,
	);
	my $bitcnt = unpack('v', substr($binary, 0, 2));
	#dbugData([$bitcnt], ['*S2A_Dn::bitcnt']);
	my $bitfld = substr($binary, 2);
	#dbugText("\$S2A_Dn::bitfld =\n".join("\n", dbugFmtBin($bitfld)));
	my @bitfld = unpack('C*', $bitfld);
	my $intnum = 0;
	my @intnum = ();
	for my $i (0 .. $#bitfld)
	{
		#dbugData([$bitfld[$i]], ['*S2A_Dn::bitfld['.$i.']']);
		for my $bitndx (0 .. 7)
		{
			#dbugData([$bitfld{$bitndx}], ['*S2A_Dn::bitfld{'.$bitndx.'}']);
			if ($bitfld[$i] & $bitfld{$bitndx})
			{
				push(@intnum, ($intnum + $bitndx + 1));
			};
		};
		$intnum += 8;
	};
	return(@intnum);
};

=pod

=head2 S<$signedByte = S2A_I1($binary)>

Converts an STDF I*1 binary value to an ATDF signed one byte value.

=cut

sub S2A_I1
{
	return(S2A_unpack('c', shift()));
};

=pod

=head2 S<$signedShort = S2A_I2($binary)>

Converts an STDF I*2 binary value to an ATDF signed two byte (i.e.,
short) value.

=cut

sub S2A_I2
{
	my $binary = shift();
	reverseEndian(\$binary);
	return(S2A_unpack('s', $binary));
};

=pod

=head2 S<$sint = S2A_I4($binary)>

Converts an STDF I*4 binary value to an ATDF signed four byte (i.e.,
long) value.

=cut

sub S2A_I4
{
	my $binary = shift();
	reverseEndian(\$binary);
	#return(S2A_unpack('l', shift())); # orig
	return(S2A_unpack('l', $binary)); # dcg, 9sep2012
};

=pod

=head2 S<$hex = S2A_N1($binary)>

Converts an STDF N*1 binary value to an ATDF hexadecimal value.

=cut

sub S2A_N1
{
	return(S2A_Nn(shift()));
};

=pod

=head2 S<$hexnum = S2A_Nn($binary)>

=cut

sub S2A_Nn
{
	my $hexbin = shift();
#	my $hexnum = unpack('H*', $hexbin);
	my $hexnum = unpack('h*', $hexbin);
#	if (substr($hexnum, 0, 1) eq '0')
#	{
#		$hexnum = substr($hexnum, 1);
#	};
	return($hexnum);
};

# $real = S2A_real($packTemplate, $binary)

sub S2A_real
{
	my $pktmpl = shift();
	my $binary = shift();
	reverseEndian(\$binary);
	my $real = S2A_unpack($pktmpl, $binary);
	#atdfFormatReal(\$real); for large real's, Perl screws up passing value to subroutine?
	my $realish = $real;
	$realish =~ m/^(\-)?(\d*)?(\.)?(\d*)?[eE]?(\-)?(\+)?(\d*)?$/;
	my $sgn = $1;
	my $int = $2;
	my $dot = $3;
	my $dec = $4;
	my $exp_sgn = $5;	# added exp stuff -- dcg
	my $exp = $7;		# added exp stuff -- dcg
	my $ascii_real = $sgn.$int.$dot.$dec;
	if ($exp =~ m/\d+/)  # dcg added this loop
	{
		$ascii_real = $ascii_real."e".$exp_sgn.$exp;
	}

	return($ascii_real);
};

=pod

=head2 S<$real = S2A_R4($binary)>

Converts and STDF R*4 binary value to an ATDF real value.

=cut

sub S2A_R4
{
	return(S2A_real('f', @_));
};

=pod

=head2 S<$real = S2A_R8($binary)>

Converts and STDF R*8 binary value to an ATDF real value.

=cut

sub S2A_R8
{
	return(S2A_real('d', @_));
};

=pod

=head2 S<$unsignedByte = S2A_U1($binary)>

Converts an STDF U*1 binary value to an ATDF unsigned one byte value.

=cut

sub S2A_U1
{
	return(S2A_unpack('C', shift()));
};

=pod

=head2 S<$unsignedShort = S2A_U2($binary)>

Converts an STDF U*2 binary value to an ATDF unsigned two byte (i.e.,
short) value.

=cut

sub S2A_U2
{
	my $binary = shift();
	reverseEndian(\$binary);
	return(S2A_unpack('v', $binary));
	# v = unsigned 16bit, little endian
	# n = unsigned 16bit, big endian
};

=pod

=head2 S<$uint = S2A_U4($binary)>

Converts an STDF U*4 binary value to an ATDF unsigned four byte (i.e.,
long) value.

=cut

sub S2A_U4
{
	my $binary = shift();
	reverseEndian(\$binary);
	#my $uint = S2A_unpack('V', shift());  # orig
	my $uint = S2A_unpack('V', $binary);  # dcg 9sep2012
	# V is 32bit unsigned little endian
	# N is 32bit unsigned big endian
	if ($uint eq '4294967295')
	{
		$uint = undef;
	};
	return($uint);
};

=pod

=head2 S<S2A_Vn()>

=cut

sub S2A_U8
{
	my $binary = shift();
	reverseEndian(\$binary);
	#my $uint = S2A_unpack('V', shift());  # orig
	my $uint = S2A_unpack('Q<', $binary);  # 64bit, little endian  
	# Q is 64bit unsigned - endian is ?, support is ?
	# V is 32bit unsigned little endian
	# N is 32bit unsigned big endian
	# do as 2 32 bits and stick them together...
	#my $uint1 = S2A_unpack('V', $binary);  
	#my $uint2 = S2A_unpack('V', $binary);  
	#my$uint = $uint1 + $uint2;
	if ($uint eq '18446744073709551615')	# ie FFFF FFFF FFFF FFFF
	{
		$uint = undef;
	};
	return($uint);
};

=pod

=head2 S<S2A_Vn()>

=cut

sub S2A_Vn
{
};

=pod

=head2 S<@txtdat = S2A_xCn($x, $bindat)>

Converts an STDF binary value containing one or more STDF C*n data type
values to an array of ATDF values.

=cut

sub S2A_xCn
{
	my $x = shift();
	my $bindat = shift();
	my $binlen = undef;
	my @txtdat = ();
	my $i = 0;
	while ($x > 0)
	{
		$binlen = S2A_U1(substr($bindat, $i, 1)) + 1;
		push(
			@txtdat,
			S2A_Cn(substr($bindat, $i, $binlen))
		);
		$i += $binlen;
		$x -= 1;
	};
	return(@txtdat);
};

=pod

=head2 S<@value = S2A_xTYPE($x, $dataType, $bindat)>

Converts an STDF binary value containing one or more STDF data type
values to an array of ATDF values.

NOTE: DO NOT USE FOR *n DATA TYPES!

=cut

sub S2A_xTYPE
{
	#dbugSub('S2A_xTYPE', @_);
	my $x = shift();				# atdf array size
	my $dataType = shift();			# data type, ie "N1" or "U2"
	my $bindat = shift();			# array of bytes to pull from
	my $binlen = length($bindat);
	my @txtdat = ();
	my $i = 0;
	my $r = undef;
	my $b = undef;
	my $n = substr($dataType, 1, 1);
	my $type = substr($dataType, 0, 1);	# dcg, 2013, need to treat N1 nibbles different!
	my $nibbles = undef;

	while (($x > 0) && ($i < $binlen))
	{
		$r = \&{'S2A_'.$dataType};
		$b = substr($bindat, $i, $n);
		if ($type eq 'N')
		{
			$nibbles = $r->($b);
			push(@txtdat, substr($nibbles, 0, 1));
			$x -= 1;
			if ($x >0)
			{
				push(@txtdat, substr($nibbles, 1, 1));
				$x -= 1;
			}
			$i += $n;
		} else {
			$b && push(@txtdat, $r->($b));
			$i += $n;
			$x -= 1;
		}
	};
	return(@txtdat);
};

########################################################################
# PUBLIC DEBUG SUBROUTINES
########################################################################

=pod

=head1 DEBUG SUBROUTINES

=cut

=pod

=head2 S<dbugData(\@varRefs, \@varNames)>

=cut

sub dbugData
{
	dumpData(@_);
};

=pod

=head2 S<dbugDataBin($datnam, $bindat)>

Outputs a binary value to standard output formatted as:

	VARIABLE_NAME =
	INDEX BINARY HEX DECIMAL CHARACTER

For example:

	If the variable $fabName contains the binary then ...

	$fabName =
	0000 00000101 05 5   ¤
	0001 01001101 4D 77  M
	0002 01001111 4F 79  O
	0003 01010011 53 83  S
	0004 00101101 2D 45  -
	0005 00110100 34 52  4

=cut

sub dbugDataBin
{
	my $datnam = shift();
	my $bindat = shift();
	my $margin = EOL.(SPC x 2);
	dbugText(
		$datnam,
		'=',
		$margin.join($margin, dbugFmtBin($bindat))
	);
};

=pod

=head2 S<dbugDataPurdy(\@varRefs, \@varNames)>

Outputs one or more variables to standard output in "purdy" (i.e.,
pretty) format.

=cut

sub dbugDataPurdy
{
	$Data::Dumper::Indent = 1;
	dumpData(@_);
	$Data::Dumper::Indent = 0;
};

=pod

=head2 S<dbugSub($subnam, @_)>

Outputs a subroutine name and argument list.

=cut

sub dbugSub
{
	my $subnam = shift();
	dbugText($subnam.'('.join(', ', @_).')');
};

=pod

=head2 S<dbugText(@text)>

Outputs a list of text values to standard output.

=cut

sub dbugText
{
	stdout(@_);
};

=pod

=head2 S<@fmtbin = dbugFmtBin($bindat)>

Returns a list of formatted strings for each byte in the binary data
value.  The string values are formatted as:

	INDEX BINARY HEX DECIMAL CHARACTER

For example:

	0000 00000101 05 5   ¤
	0001 01001101 4D 77  M
	0002 01001111 4F 79  O
	0003 01010011 53 83  S
	0004 00101101 2D 45  -
	0005 00110100 34 52  4

=cut

sub dbugFmtBin
{
	my $bindat = shift();
	my $i = undef;
	my $byte = undef;
	my $binstr = undef;
	my $hexstr = undef;
	my @bindat = split(//, $bindat);
	my @fmtbin = ();
	for $i (0 .. $#bindat)
	{
		$byte = $bindat[$i];
		$binstr = unpack('B8', $byte);
		$hexstr = binToHexStr($binstr);
		$bindat[$i] =~ s/[^\ \.\-\+\,\w]/\xA4/g;
		$fmtbin[$i] = sprintf(
			"%04d %08d %s %-3s %s",
			$i,
			$binstr,
			$hexstr,
			unpack('C', $byte),
			$bindat[$i]
		);
	};
	return(@fmtbin);
};

=pod

=head2 S<dumpData(\@varRefs, \@varNames)>

Outputs one or more variables to standard output using the
Data::Dumper::Dump subroutine.

=cut

sub dumpData
{
	my $text = Data::Dumper->Dump(@_);
	chomp($text);
	stdout($text);
};

########################################################################
# END OF MODULE
########################################################################

1; # last executing statement

=pod

=head1 SUPPORTED PLATFORMS

E<bull> Unix

E<bull> Windows.

=head1 REQUIRES

E<bull> Perl 5.6 or newer.

E<bull> Perl core modules: Data::Dumper, strict, and Time::Local.

=head1 SEE ALSO

E<bull> Perl core and module documentation.

E<bull> STDF Specification V4 published by Teradyne, Inc.

E<bull> ATDF Specification V2 published by Teradyne, Inc.

=head1 AUTHOR

Michael Hackerott, michael.hackerott@mrhackerott.org

=head1 COPYRIGHT

Copyright E<copy> 2004-2005 Michael Hackerott. All rights reserved.

This program is free software; you can redistribute it and/or modify it
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

An Essay on Endian Order, Dr. William T. Verts, April 19, 1996.
http://www.cs.umass.edu/~verts/cs32/endian.html

=head1 KNOWN BUGS

E<bull> S2A_Bn and S2A_Vn is not implemented.

=head1 HISTORY

1.0.0 (200501221020) Michael Hackerott

E<bull> Created module.

1.0.1 (200402080725) Michael Hackerott

E<bull> Added about() subroutine.

1.1.0 (200512012020) Michael Hackerott

E<bull> Fixed endian logic.

1.1.1 (200512030659) Michael Hackerott

E<bull> Updated documentation.

=cut

__END__
