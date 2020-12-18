#  ConvertStdf.R
#
# $Id: ConvertStdf.R,v 1.55 2020/12/18 00:58:20 david Exp $
#
#  R script that reads in an STDF file and converts it into a
#  set of R data.frames/matrix:
#
# Copyright (C) 2006-2016 David Gattrell
#               2012 Chad Erven
#               2018 David Gattrell
#               2020 David Gattrell
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
#-------------------------------------------------------------------
#  LotInfoFrame
#      LotInfoFrame[[index,"lotid"]]
#                         ,"sublotid"]]
#                         ,"start_t"]]
#                         ,"program"]]
#                         ,"tester_type"]]
#                         ,"tester_id"]]
#                         ,"handler"]]
#  HbinInfoFrame  (optional)
#      HbinInfoFrame[[index,"hbin_num"]]
#                          ,"hbin_cnt"]]
#                          ,"hbin_pf"]]
#                          ,"hbin_nam"]]
#  SbinInfoFrame  (optional)
#      SbinInfoFrame[[index,"sbin_num"]]
#                          ,"sbin_cnt"]]
#                          ,"sbin_pf"]]
#                          ,"sbin_nam"]]
#  WaferInfoFrame  (optional)
#      WaferInfoFrame[[index,"wafr_siz"]]
#                           ,"die_ht"]]
#                           ,"die_wid"]]
#                           ,"wf_units"]]
#                           ,"wf_flat"]]
#                           ,"center_x"]]
#                           ,"center_y"]]
#                           ,"pos_x"]]
#                           ,"pos_y"]]
#  WafersFrame  (optional)
#      WafersFrame[[index,"wafer_id"]]
#                        ,"start_t"]]
#                        ,"finish_t"]]
#                        ,"part_cnt"]]
#                        ,"good_cnt"]]
#  DevicesFrame
#      DevicesFrame[[index,"part_id"]]
#                         ,"temp"]]
#                         ,"x_coord"]]
#                         ,"y_coord"]]
#                         ,"wafer_index"]]
#                         ,"soft_bin"]]
#                         ,"hard_bin"]]
#                         ,"testtime"]]
#                         ,"site"]]
#						  ... optionally, any conditions...
#						  ,"Vdda"]]	# from PTR/MPR with testname CONDITION=Vdda
#  ParametersFrame
#      ParametersFrame[[index,"testnum"]]
#                            ,"testname"]]
#                            ,"scaler"]]
#                            ,"units"]]
#                            ,"ll"]]
#                            ,"ul"]]
#                            ,plot_ll"]]
#                            ,plot_ul"]]
#  TSRFrame
#      TSRFrame[[index,"testnum"]]
#                     ,"testname"]]
#                     ,"test_typ"]]
#                     ,"exec_cnt"]]
#                     ,"fail_cnt"]]
#                     ,"fixed_exec_cnt"]]
#                     ,"fixed_fail_cnt"]]
#  ResultsMatrix[row,col]
#      rows = devices
#      cols = parameters
#
#  TestFlagMatrix[row,col]
#      rows = devices
#      cols = parameters
#
#--------------------------------------------------------------

# namespace for the function... main function Global variables and local functions
#-------------------------------------------------------
if(exists(".ConvertStdf.env")) rm(.ConvertStdf.env) 
.ConvertStdf.env <- new.env(hash=TRUE, size=120L)

assign("Verbose0",FALSE,envir=.ConvertStdf.env)    # print per device info (PIR/PRR)
assign("Verbose1",FALSE,envir=.ConvertStdf.env)    # 
assign("Verbose2",FALSE,envir=.ConvertStdf.env)    # print per record info

assign("Timestamp1",0.0,envir=.ConvertStdf.env)    # timer to suppress prints to screen to >5sec

assign("Debug1",FALSE,envir=.ConvertStdf.env)      # debugging readSTDFstring()
assign("Debug2",FALSE,envir=.ConvertStdf.env)      # debugging MPR records
assign("Debug2b",FALSE,envir=.ConvertStdf.env)     # debugging MPR records
assign("Debug3",FALSE,envir=.ConvertStdf.env)		# debugging PMR records
assign("Debug4",FALSE,envir=.ConvertStdf.env)		# debugging FTR records 
assign("Debug5",FALSE,envir=.ConvertStdf.env)		# debugging TSR records
assign("Debug6",FALSE,envir=.ConvertStdf.env)		# debugging long strings
assign("Debug7",FALSE,envir=.ConvertStdf.env)		# debugging Previous_param_i

assign("Stdf_Version",3,envir=.ConvertStdf.env)		# usually set to 4

assign("Debug_testname","",envir=.ConvertStdf.env)	# keep track of the testname we last processed

assign("Do_conditions",FALSE,envir=.ConvertStdf.env) # flag to treat tests that begin with CONDITION= as conditions
assign("Condition_Names",NA,envir=.ConvertStdf.env) # vector of strings
assign("ConditionsMatrix",array(NaN, dim=c(0,0)),envir=.ConvertStdf.env) # Conditions_count x Devices_count
assign("Conditions_count",0,envir=.ConvertStdf.env)	# number of different conditions
assign("Do_DTRs",FALSE,envir=.ConvertStdf.env)		# flag to parse DTRs looking for REPEATABILITY or DEVICEID
assign("DTR_Names",NA,envir=.ConvertStdf.env) 		# vector of DTR strings that we triggered on
assign("DTRsMatrix",array(NaN, dim=c(0,0)),envir=.ConvertStdf.env) # DTR_Names_count x Devices_count
assign("DTR_Names_count",0,envir=.ConvertStdf.env)	# number of different DevicesFrame fields to add from DTR info.
assign("Parse_PRR_part_txt",FALSE,envir=.ConvertStdf.env)		# flag to parse PRR part_txt looking for name=value[,] 
assign("PRRtxt_Names",NA,envir=.ConvertStdf.env) 		# vector of PRR part_txt strings that we triggered on
assign("PRRtxt_Matrix",array(NaN, dim=c(0,0)),envir=.ConvertStdf.env) # PRRtxt_Names_count x Devices_count
assign("PRRtxt_Names_count",0,envir=.ConvertStdf.env)	# number of different DevicesFrame fields to add from PRR part_txt info.
assign("PIR_count",0,envir=.ConvertStdf.env)		# number of different PIR records

assign("Do_FTR_fail_cycle",FALSE,envir=.ConvertStdf.env)		# flag to change FTR result from fail_flag to failing_vector 

assign("Duplicate_testnames",FALSE,envir=.ConvertStdf.env) # if T testname = testtxt_tnum else testname = testtxt
assign("Use_MPR_invalid_pf_data",FALSE,envir=.ConvertStdf.env) # Credence weirdness

assign("Auto_flex",TRUE,envir=.ConvertStdf.env) 	# remove channels from testnames, keeps multisite testnames consistent
assign("Keep_alarmed_values",FALSE,envir=.ConvertStdf.env) 	# if alarm detected, still update ResultsMatrix if TRUE

assign("Executive_type",'',envir=.ConvertStdf.env)	# "enVision" or "Image" or ...
													# use to make some PTR interpretation
													# decisions.
assign("Executive_version",'',envir=.ConvertStdf.env)	# from MIR EXEC_VER field.. for 93K bugs by version
assign("MIR_Tester_type",'',envir=.ConvertStdf.env)	# "Fusion_HFi" "Catalyst" "IntegraFlex" "93000-SOC" "CMT" ...
assign("Ignore_testname_objects",FALSE,envir=.ConvertStdf.env)	# for Ltx/enVision long testnames
assign("Endy","big",envir=.ConvertStdf.env)			# initial guess at Endian type of stdf file
													# Sun based testers are big, Intel based testers are little
assign("Stdf",raw(0),envir=.ConvertStdf.env)		# vector containing stdf file contents
assign("Stdf_size",0,envir=.ConvertStdf.env)		# number of bytes in stdf stream
assign("Stdf_size_true",0,envir=.ConvertStdf.env)	# true size =1, estimate if =0 
assign("Stdf_ptr",1,envir=.ConvertStdf.env)			# track our progress through Stdf file
assign("Ptr",1,envir=.ConvertStdf.env)				# pointer to next position in Stdf (vector of raw)

assign("Device_count",0,envir=.ConvertStdf.env)		# number of devices processed (PIR/PRR records)
assign("Open_site",rep(0,32),envir=.ConvertStdf.env) # which Device index is for which site
assign("Parameter_count",0,envir=.ConvertStdf.env)	# number of tests processed (PTR/FTR/MPR*pins records)
assign("Wafer_count",0,envir=.ConvertStdf.env)		# number of wafers processed (WIR/WRR records)
assign("TSR_count",0,envir=.ConvertStdf.env)		# number of TSR records processed 
assign("TSR_count_siteA",0,envir=.ConvertStdf.env)	# number of TSR records processed for first site
assign("TSR_siteA",NA,envir=.ConvertStdf.env)		# first TSR site
assign("Do_Raw_TSRs",0,envir=.ConvertStdf.env)		# 0= track TSRs with matching PTR/FTR/MPRs? (slower) 
assign("Hbin_count",0,envir=.ConvertStdf.env)		# number of Hard bins processed (HBR records)
assign("Sbin_count",0,envir=.ConvertStdf.env)		# number of Soft bins processed (SBR records)
assign("Previous_param_i",0,envir=.ConvertStdf.env) # the ParametersFrame index from the previous PTR/FTR/MPR
assign("Good_guesses",0,envir=.ConvertStdf.env)		# how often Previous_param_i+1 worked
assign("Another_guess",0,envir=.ConvertStdf.env)	# the last MPR/FTR/PTR, for interleaved multisite
assign("Good_guesses2",0,envir=.ConvertStdf.env)	# how often Previous_param_i+1 worked
assign("Max_parts",0,envir=.ConvertStdf.env)		# limit of parts to extract from file

assign("Pin_names",NA,envir=.ConvertStdf.env)		# array of strings extracted from PMRs, used by MPRs
assign("Valid_WCR",FALSE,envir=.ConvertStdf.env)
assign("Demangle",0,envir=.ConvertStdf.env)			# -1 = remove some 0x0d, 1 = add some 0x0d, 2 = mod
assign("Unknown_rec_count",0,envir=.ConvertStdf.env) # count of how many unknown records encountered.  0 is a good number
assign("LotInfoFrame",data.frame(rbind(
		list( lotid="", sublotid="",
                start_t=NaN, program="",
                tester_type="", tester_id="",
                handler="")
		)),envir=.ConvertStdf.env)

#assign("DevicesFrame",data.frame(rbind(
#		list(part_id="", temp=NaN,
#                x_coord=NaN, y_coord=NaN,
#                wafer_index=NaN,
#                soft_bin=NaN, hard_bin=NaN,
#                testtime=NaN, site=NaN)
#		)),envir=.ConvertStdf.env)

#assign("ParametersFrame",data.frame(rbind(
#		list(testnum=NaN, testname="",
#                scaler=NaN, units="",
#                ll=NaN, ul=NaN,
#                plot_ll=NaN, plot_ul=NaN)
#		)),envir=.ConvertStdf.env)

assign("ResultsMatrix",array(NaN, dim=c(0,0)),envir=.ConvertStdf.env)

assign("TestFlagMatrix",array(NaN, dim=c(0,0)),envir=.ConvertStdf.env)
assign("Do_testflag_matrix",FALSE,envir=.ConvertStdf.env) # flag to enable/disable extra object

assign("TestOrderMatrix",array(NaN, dim=c(0,0)),envir=.ConvertStdf.env)
assign("Do_test_order_matrix",FALSE,envir=.ConvertStdf.env) # flag to enable/disable extra object
assign("Use_test_order_matrix",FALSE,envir=.ConvertStdf.env) # flag to revisit ParametersFrame order
assign("Save_test_order_matrix",FALSE,envir=.ConvertStdf.env) # flag save TestOrderMatris in RTDF
assign("Test_order_counter",0,envir=.ConvertStdf.env)	# number of tests processed (PTR/FTR/MPR*pins records)

assign("Do_mult_limits",0,envir=.ConvertStdf.env) # flag to enable/disable Multi-limits, also max # of limits sets to allow.
assign("MultLimIndexMatrix",array(NaN, dim=c(0,0)),envir=.ConvertStdf.env)	# same size as ResultsMatrix, which limits set to use
assign("MultLim_ll_Matrix",array(NaN, dim=c(0,0)),envir=.ConvertStdf.env)	# [# of params,# of limits]
assign("MultLim_ul_Matrix",array(NaN, dim=c(0,0)),envir=.ConvertStdf.env)
assign("MultLim_ll_ge_Matrix",array(NaN, dim=c(0,0)),envir=.ConvertStdf.env)
assign("MultLim_ul_ge_Matrix",array(NaN, dim=c(0,0)),envir=.ConvertStdf.env)
assign("MultLim_idx",NA,envir=.ConvertStdf.env) 	# <<- as.integer(  -- how many sets of limits for this particular Parameter?

assign("HbinInfoFrame",data.frame(rbind(
		list(hbin_num=NaN, hbin_cnt=NaN, hbin_pf="",hbin_nam="")
		)),envir=.ConvertStdf.env)

assign("SbinInfoFrame",data.frame(rbind(
		list(sbin_num=NaN, sbin_cnt=NaN, sbin_pf="",sbin_nam="")
		)),envir=.ConvertStdf.env)

assign("TSRFrame",data.frame(rbind(
		list(testnum=NaN, testname="",
						test_typ="",
						exec_cnt=NaN,fail_cnt=NaN,
						fixed_exec_cnt=NaN,
						fixed_fail_cnt=NaN)
		)),envir=.ConvertStdf.env)

assign("WaferInfoFrame",data.frame(rbind(
		list(wafr_siz=0, die_ht=0,
                        die_wid=0, wf_units=0,
                        wf_flat="W", center_x=0,
                        center_y=0, pos_x="E",
                        pos_y="N")
		)),envir=.ConvertStdf.env)

assign("WafersFrame",data.frame(rbind(
		list(wafer_id='', start_t=NaN,
					finish_t=NaN, part_cnt=NaN,
					good_cnt=NaN)
		)),envir=.ConvertStdf.env)

# due to speed issues, it is better to deal with vectors than data frames,
# so create the following vectors that duplicate data frame information
assign("Parameters_Names",NA,envir=.ConvertStdf.env)   # <<- as.character(ParametersFrame$testname)
assign("Parameters_testnum",NA,envir=.ConvertStdf.env) # <<- as.integer(
assign("Parameters_scaler",NA,envir=.ConvertStdf.env)  # <<- as.integer(
assign("Parameters_units",NA,envir=.ConvertStdf.env)   # <<- as.character(
assign("Parameters_ll",NA,envir=.ConvertStdf.env)		# <<- as.numeric(
assign("Parameters_ul",NA,envir=.ConvertStdf.env)		# <<- as.numeric(
assign("Parameters_ll_ge",NA,envir=.ConvertStdf.env)	# <<- as.integer(  1 means = is pass
assign("Parameters_ul_ge",NA,envir=.ConvertStdf.env)	# <<- as.integer(  1 means = is pass
assign("Parameters_plot_ll",NA,envir=.ConvertStdf.env)	# <<- as.numeric(
assign("Parameters_plot_ul",NA,envir=.ConvertStdf.env)	# <<- as.numeric(

assign("Devices_part_id",NA,envir=.ConvertStdf.env)    # <<- as.character(DevicesFrame$part_id)
assign("Devices_temp",NA,envir=.ConvertStdf.env)       # <<- as.numeric(
assign("Devices_x_coord",NA,envir=.ConvertStdf.env)    # <<- as.integer(
assign("Devices_y_coord",NA,envir=.ConvertStdf.env)    # <<- as.integer(
assign("Devices_wafer_index",NA,envir=.ConvertStdf.env) # <<- as.integer(
assign("Devices_soft_bin",NA,envir=.ConvertStdf.env)	# <<- as.integer(
assign("Devices_hard_bin",NA,envir=.ConvertStdf.env)	# <<- as.integer(
assign("Devices_pass_flag",NA,envir=.ConvertStdf.env)	# <<- as.integer(
assign("Devices_testtime",NA,envir=.ConvertStdf.env)	# <<- as.numeric(
assign("Devices_site",NA,envir=.ConvertStdf.env)		# <<- as.numeric(
assign("Devices_PIR_count",NA,envir=.ConvertStdf.env)	# <<- as.numeric(

assign("TSRs_testnum",NA,envir=.ConvertStdf.env)		# <<- as.integer(TSRFrame$testnum)
assign("TSRs_testname",NA,envir=.ConvertStdf.env)
assign("TSRs_test_typ",NA,envir=.ConvertStdf.env)
assign("TSRs_exec_cnt",NA,envir=.ConvertStdf.env)
assign("TSRs_fail_cnt",NA,envir=.ConvertStdf.env)
assign("TSRs_fixed_exec_cnt",NA,envir=.ConvertStdf.env)
assign("TSRs_fixed_fail_cnt",NA,envir=.ConvertStdf.env)

assign("TSRs_siteA_testnum",NA,envir=.ConvertStdf.env)		# <<- as.integer(TSRFrame$testnum)
assign("TSRs_siteA_testname",NA,envir=.ConvertStdf.env)
assign("TSRs_siteA_test_typ",NA,envir=.ConvertStdf.env)
assign("TSRs_siteA_exec_cnt",NA,envir=.ConvertStdf.env)
assign("TSRs_siteA_fail_cnt",NA,envir=.ConvertStdf.env)
assign("TSRs_siteA_fixed_exec_cnt",NA,envir=.ConvertStdf.env)
assign("TSRs_siteA_fixed_fail_cnt",NA,envir=.ConvertStdf.env)

# for STDF files that only have pinlists and limits in the first occurence of an MPR,
# we need to track that here
assign("MPR_count",0,envir=.ConvertStdf.env)			# number of different MPRs processed 
assign("MPR_testnames",NA,envir=.ConvertStdf.env)		# 
assign("MPR_testnums",NA,envir=.ConvertStdf.env)		# 
assign("MPR_pins_length",NA,envir=.ConvertStdf.env)	# 
assign("MPR_pins_start",NA,envir=.ConvertStdf.env)	# 
assign("MPR_all_pins",NA,envir=.ConvertStdf.env)	# 


assign("SiteSbinInfoFrame",data.frame(rbind(
		list(sbin_num=NaN, sbin_pf="",sbin_nam="")
		)),envir=.ConvertStdf.env)
assign("SiteSbinSiteVector",NA,envir=.ConvertStdf.env)
assign("SiteSbinCountMatrix",NA,envir=.ConvertStdf.env)
assign("SiteSbinCount",0,envir=.ConvertStdf.env)

##########################################################################
ConvertStdf <- function(stdf_name="",rtdf_name="",auto_93k=TRUE,do_summary=TRUE,
                        just_fail_tests_summary=TRUE,endian="big",
						stdf_dir="",do_conditions=FALSE,duplicate_testnames=FALSE,
						use_MPR_invalid_pf_data=FALSE,ltx_ignore_testname_objects=TRUE,
						do_testflag_matrix=FALSE,do_DTRs=FALSE,max_parts=-1,
						auto_demangle=FALSE,auto_flex=TRUE,keep_alarmed_values=FALSE,
						skip_TSRs=FALSE,raw_TSRs=FALSE,parse_PRR_part_txt=TRUE,
						do_FTR_fail_cycle=TRUE,
						use_testorder_matrix=FALSE,save_testorder_matrix=FALSE,
						mult_limits=0) {
    # stdf_name - name of stdf formatted file to convert to rtdf format
    # rtdf_name - name to give to rtdf formatted file
    # auto_93k  - if stdf is from HP/Agilent/Verigy 93K, try to auto fix
    #             broken limits... ll = -1*ul, ul= -1*ll
    # do_summary - generates .summary text file from stdf
    # just_fail_tests_summary - if do_summary, when dumping per test information
    #             only report the tests that had failures 
    # endian - initial guess at stdf type, either big or little, if stdf
    #             has FAR record, the FAR record will determine endian value.
	# stdf_dir - if stdf file is in a different directory, this is the
	#			  absolute path to that directory
	# do_conditions - if TRUE, then treat tests that have names beginning
	#             with "CONDITION=" as conditions, not tests.  This means
	#             adding an extra condition field to the DevicesFrame rather
	#             than another parameter to the ParametersFrame.  Also, 
	#             if the condition changes within the same PIR/PRR, the
	#             RTDF file will divide the part into multiple devices
	#             ...maybe should also support "SWEEP=" for sweeps that
	#             haven't been implemented as MPRs.
	# duplicate_testnames - if TRUE, then append _testnum to end of testname
	#			  (prior to /pinname for MPR's) so that testnames become
	#			  unique
	# use_MPR_invalid_pf_data - for some Credence MPR tests, an extra, 
	#			  usually duplicate, test is also generated, but with an 
	#			  invalid pass/fail flag.  By default, this extra data is
	#			  ignored.  To view it, set this flag to TRUE.
	#			  The duplicate_testnames flag will also be set to TRUE if
	#			  you set this flag to TRUE
	# ltx_ignore_testname_objects - For LTX/enVision files, strip off object stuff
	#			  that gets appended to testnames...  ie.  anything between
	#             a pair of "/"s... ie testname/MyFlowObj/flow/ becomes testname
	# do_testflag_matrix - Extracts pass/fail flag similar to ATDF format and
	#			  adds it to generated .rtdf file as TestFlagMatrix. 
	# do_DTRs - if set, will parse DTRs looking for specific information to add
	#             to DevicesFrame (this is custom for a few specific products)
	# max_parts - stop processing PTR/FTR/MPR after this many parts.
	#             if -1, process all parts
	#             if 0, don't populate ParametersFrame of ResultsMatrix
	#             else extract just the first "max_parts" devices to the rtdf
	# auto_demangle - as it parses the stdf file, if it comes across a corrupted
	#             record, it will try to uncorrupt it and continue
	#             - for now, try inserting 0x0d before 0x0a in the offending record
	#               (pc based tester stdf's that had dos2unix run on them)
	# auto_flex - if the tester type is "IntegraFlex", 
	#             (should we also - set endian to little )  
	#             (should we also - set duplicate_testnames to TRUE...) 
	#             - remove channel from testnames so that testnames are consistent between sites.
	#               testname pinname channel <> comment; channel = d+.ad+ (ie 9.b5 or 12.a7..)
	# keep_alarmed_values - if an alarm was detected during a test, the resulting measurement will be
	#             ignored (ie NaN in ResultsMatrix) unless this flag is set to true.
	#             (impacts PTR records with test_flg bit 0 set to 1)
	# skip_TSRs - if true, skip the whole processing of TSR records (Test Synopsis Records)
	# raw_TSRs - if true, then don't check that there are matching FTR/PTR/MPRs for TSRs, just
	#        	  dump all the TSRs.  (faster than if checking)
	# parse_PRR_part_txt - if true, will parse the part_txt field based on comma separated list of
	#             name=value sets and create additional DeviceFrame fields for each name
	# do_FTR_fail_cycle - if true, datalog the failing vector number, rather than just the fail flag
	#             fail flag .. 0=PASS, >0=FAIL
	#             fail vector .. -1=PASS, >-1=FAIL
	#             (added option 1-Feb-2020)
	# use_testorder_matrix - if this is set, a TestOrderMatrix will be generated, and then used to
	#             try to more intelligently order the ParametersFrame rather than just by the order
	#             the tests appear in the STDF file.  (Useful for multi-variant products or programs
	#             that have sampling tests, ie every 3rd part does slower parametric test
	# save_testorder_matrix - as above, the will generate a TestOrderMatrix, but will also save this
	#             matrix to the RTDF file.  If you don't want your RTDF files to be too bloated,
	#             but you still want the ParametersFrame order 'optimized'
	# mult_limits -- a value <2 = original behaviour... 
	#             check limits as well as testname and track if limits change during run,
	#             if so, generate output files per sets of limits up to the value of 
	#             mult_limits, <20 is probably the limit of sanity
    #---------------------------------------------
	#attach(.ConvertStdf.env) #...environment() is better approach?

    timestamp0 = proc.time()
    timestamp0 = timestamp0[3]  # start time
    Timestamp1 <<- timestamp0   # time since last screen write

	#cat("Dave, you are running the debugging version of ConvertStdf \n")


    # initialize Global variables
    #------------------------------
	Stdf_Version <<- 3
    Executive_type <<- ''
    Executive_version <<- ''
    MIR_Tester_type <<- ''
    Device_count <<- 0
	Open_site <<- rep(0,32)
    Parameter_count <<- 0
	Parameters_Names <<- NA
	Parameters_testnum <<- NA
    Wafer_count <<- 0
	TSR_count <<- 0
	TSR_count_siteA <<- 0
	TSR_siteA <<- NA
	Do_Raw_TSRs <<- raw_TSRs
    Hbin_count <<- 0
    Sbin_count <<- 0
	ResultsMatrix <<- array(NaN, dim=c(0,0))
	TestFlagMatrix <<- array(NaN, dim=c(0,0))
	Do_testflag_matrix <<- do_testflag_matrix
	TestOrderMatrix <<- array(NaN, dim=c(0,0))
	Do_test_order_matrix <<- 0	
	Use_test_order_matrix <<- 0
	Save_test_order_matrix <<- 0
	if(use_testorder_matrix>0) {
		Do_test_order_matrix <<- 1	
		Use_test_order_matrix <<- 1
	}
	if(save_testorder_matrix>0) {
		Do_test_order_matrix <<- 1	
		Save_test_order_matrix <<- 1
	}
	Test_order_counter <<- 0
	Max_parts <<- max_parts
	Unknown_rec_count <<- 0

	Do_mult_limits <<- 0
	MultLimIndexMatrix <<- array(NaN, dim=c(0,0))
	MultLim_ll_Matrix <<- array(NaN, dim=c(0,0))
	MultLim_ul_Matrix <<- array(NaN, dim=c(0,0))
	MultLim_ll_ge_Matrix <<- array(NaN, dim=c(0,0))
	MultLim_ul_ge_Matrix <<- array(NaN, dim=c(0,0))
	MultLim_idx <<- NA

	Another_guess <<- 0
	Previous_param_i <<- 1
	Good_guesses <<- 0
	Good_guesses2 <<- 0
	Demangle <<- 0
	Auto_flex <<- auto_flex
	Keep_alarmed_values <<- keep_alarmed_values
	
	LotInfoFrame[["lotid"]] <<- ""
	LotInfoFrame[["sublotid"]] <<- ""
	LotInfoFrame[["start_t"]] <<- NaN
	LotInfoFrame[["program"]] <<- ""
	LotInfoFrame[["tester_type"]] <<- ""
	LotInfoFrame[["tester_id"]] <<- ""
	LotInfoFrame[["handler"]] <<- ""

    Pin_names <<- NA
    Valid_WCR <<- FALSE

	MPR_count <<- 0
	MPR_testnames <<- NA
	MPR_testnums <<- NA
	MPR_pins_length <<- NA
	MPR_pins_start <<- NA
	MPR_all_pins <<- NA

	SiteSbinInfoFrame[["sbin_num"]] <<- NaN
	SiteSbinInfoFrame[["sbin_nam"]] <<- NaN
	SiteSbinInfoFrame[["sbin_pf"]] <<- ""
    SiteSbinSiteVector <<- integer()
	SiteSbinCountMatrix <<- array(NaN, dim=c(0,0))
	SiteSbinCount <<- 0

	Ignore_testname_objects <<- ltx_ignore_testname_objects
	Do_conditions <<- do_conditions
	Condition_Names <<- NA
	Conditions_count <<- 0
	Do_DTRs <<- do_DTRs
	DTR_Names <<- NA
	DTR_Names_count <<- 0
	PIR_count <<- 0
	ConditionsMatrix <<- array(NaN, dim=c(0,0)) 
	DTRsMatrix <<- array(NaN, dim=c(0,0))
	
	Do_FTR_fail_cycle <<- do_FTR_fail_cycle

	Parse_PRR_part_txt <<- parse_PRR_part_txt
	PRRtxt_Names <<- NA
	PRRtxt_Names_count <<- 0
	PRRtxt_Matrix <<- array(NaN, dim=c(0,0))

	Use_MPR_invalid_pf_data <<- use_MPR_invalid_pf_data
	#Duplicate_testnames <<- duplicate_testnames || use_MPR_invalid_pf_data
	Duplicate_testnames <<- duplicate_testnames 

    my_list = list(hbin_num=NaN, hbin_cnt=NaN, hbin_pf="",hbin_nam="")
    HbinInfoFrame <<- data.frame(rbind(my_list))

    my_list = list(sbin_num=NaN, sbin_cnt=NaN, sbin_pf="",sbin_nam="")
    SbinInfoFrame <<- data.frame(rbind(my_list))
	
	my_list = list(testnum=NaN, testname="",test_typ="",exec_cnt=NaN,
					fail_cnt=NaN,fixed_exec_cnt=NaN,fixed_fail_cnt=NaN)
	TSRFrame <<- data.frame(rbind(my_list))

	my_list = list(wafr_siz=0, die_ht=0, die_wid=0, wf_units=0,
                        wf_flat="W", center_x=0, center_y=0,
						pos_x="E", pos_y="N")
	WaferInfoFrame <<- data.frame(rbind(my_list))

	my_list = list(wafer_id='', start_t=NaN,finish_t=NaN, part_cnt=NaN,
					good_cnt=NaN)
	WafersFrame <<- data.frame(rbind(my_list))

	#if(auto_flex)  endian = "little"	# force initial guess to little vs. big
	#if(auto_flex)  duplicate_testnames = TRUE	# necessary?

	if(mult_limits>1) {
		# need to create
		# extra matrix for lower, upper limits and their GT vs GE
		# extra matrix sized like ResultsMatrix that gives index for limits reference
		Do_mult_limits <<- mult_limits
	}


    # if filenames not defined, prompt for them
    #------------------------------------------
    if (stdf_name == "") {
        stdf_name <- readline("Enter the name of the STDF file to read: ")
    }

    if (rtdf_name == "") {
        rtdf_name <- readline("Enter the name of the Rdata file to write: ")
    }


    # guess at the endian-ness of the stdf file
    #-------------------------------------------
    Endy <<- endian     # initial guess.  The first record should be 
                        # FAR, which will tell us if we are big or
                        # little.  If there is no FAR record, then
                        # the initial guess will be used.
                        # STDF files from Sun boxes, etc are big
                        # STDF files from Intel boxes, etc are little


    # open stdf file for reading...
    # suck file into memory in 100Mbyte chunks
	# MS Windows seems to hit a 500Mb ceiling, 
	# in Linux you hit the 2e9 maximum integer size limit (2Gb)
    #--------------------------------------------------
	if (stdf_dir != "") {
		my_dir = getwd()
		setwd(stdf_dir)
	}
    STDF <- gzfile(stdf_name,open="rb")

    raw_bytes <- file.info(stdf_name)[["size"]]		# returns a double

	read_bytes = 100e6
    MoreStdf = readBin(STDF,raw(),n=read_bytes)
    in_bytes = length(MoreStdf)
	Stdf <<- raw(0)
	Ptr <<- 1

	# for tracking progress, how far through the file are we?
    if (regexpr("gz$",stdf_name)>0) {
		if(in_bytes < read_bytes) {
			Stdf_size <<- in_bytes
			Stdf_size_true <<- 1
		} else {
			Stdf_size <<- raw_bytes * 12	# estimate of gz compression
			Stdf_size_true <<- 0			# estimate, not true size
		}
	} else {
		if(in_bytes < read_bytes)  Stdf_size <<- in_bytes
		else					   Stdf_size <<- raw_bytes
		Stdf_size_true <<- 1
	}
	Stdf_ptr <<- 0

	# if mangled/trying to demangle, look at first 100Mb chunk
	# and see if we can determine if it is a dos->unix or unix->dos
	# corruption...
	# corruption #1
	#     0x0a -> 0x0d + 0x0a  (inserts bogus 0x0d's before 0x0a's)
	# corruption #2
	#     0x0d -> 0x0a  (replaces any 0x0d with 0x0a)
	# corruption #3
	#     0x0d 0x0a -> 0x0a (if removes 0x0d's if they are before 0x0a's)

	if(auto_demangle) {
		# how many occurrences of...
		# 0x0d
		# 0x0a
		# 0x0d + 0x0a
		indices_13 = which(MoreStdf==13)
		indices_next = indices_13+1
		indices_10 = which(MoreStdf==10)
		indices_13_10 = intersect(indices_10,indices_next)

		cr_count = length(indices_13)	# 0x0d, 13, ascii CR
		lf_count = length(indices_10)   # 0x0a, 10, ascii LF
		crlf_count = length(indices_13_10)

		cat(sprintf("For %d bytes of the STDF file,\n",length(MoreStdf)))
		cat(sprintf("byte 0x0d occurs: %d times\n",cr_count))
		cat(sprintf("byte 0x0a occurs: %d times\n",lf_count))
		cat(sprintf("bytes 0x0d 0x0a occur: %d times\n",crlf_count))
		
		if(lf_count == crlf_count) {
			Demangle <<- -1
			cat("Will try removing some of the 0x0d bytes...\n")
		} else if(crlf_count ==0) {
			Demangle <<- 1
			cat("Will try adding 0x0d bytes before some of the 0x0a bytes...\n")
		}
	}


	# parse through 100Mb chunks of the stdf file...
	#-----------------------------------------------
    while (in_bytes > 0) {
		Stdf_ptr <<- Stdf_ptr + Ptr - 1		# overall pointer, for progress tracking
		if(length(Stdf)>0) {
			#browser()
			if(Ptr>length(Stdf)) {
				Stdf <<- MoreStdf
			} else {
				Stdf <<- c(Stdf[Ptr:length(Stdf)],MoreStdf)
			}
		} else {
			Stdf <<- MoreStdf
		}
		in_bytes =  length(Stdf)
		Ptr <<- 1

		#browser()

		# parse STDF records...
		#------------------------
		# NOTE: real record length = length of header (4 bytes) + value of rec_len!
		rec_len = readBin(Stdf[Ptr:(Ptr+1)],integer(),n=1,size=2,endian=Endy,signed=FALSE)
		Ptr <<- Ptr+2
		while ((Ptr+1+rec_len) <= in_bytes) {
			rec_typ = readBin(Stdf[Ptr],integer(),n=1,size=1,signed=FALSE)
			rec_sub = readBin(Stdf[Ptr+1],integer(),n=1,size=1,signed=FALSE)
			Ptr <<- Ptr+2
			# for debugging
			#cat(sprintf("rec_len is %d , rec_typ is %d, rec_sub is %d", 
			#           rec_len, rec_typ, rec_sub))
			#cat(sprintf(", Endy is %s, Ptr is %d\n", 
			#           Endy, Ptr))

			#if( (Ptr+rec_len+10) > in_bytes) {
			#	cat("... at end of chunk ...\n")
			#	browser()
			#}

			if(Demangle != 0) {
				# check that the start of the next record is ok.
				# if not, need to add or delete a 0x0d byte before a 0x0a byte

				# REVISIT.. 
				# need to consider case where record ends on stdf chunk
				# boundary

				if((Ptr+rec_len+4) <= in_bytes) {
					next_rec_typ = readBin(Stdf[Ptr+rec_len+2],integer(),n=1,size=1,signed=FALSE)
					next_rec_sub = readBin(Stdf[Ptr+rec_len+3],integer(),n=1,size=1,signed=FALSE)
					if(!valid_record_type(next_rec_typ,next_rec_sub)) {
						if(Demangle == 1) {
							# need to add a 0x0d before a 0x0a somewhere...
							suspects = which(Stdf[Ptr:(Ptr+rec_len+1)]==0x0a)
							#browser()
							if(length(suspects)==1) {
								Stdf[(Ptr+suspects):(in_bytes+1)] <<- Stdf[(Ptr+suspects-1):(in_bytes)]
								Stdf[Ptr+suspects-1] <<- as.raw(0x0d)
								in_bytes = in_bytes+1
								cat(sprintf("...Demangle inserted 0x0d byte at location %d \n",
											Stdf_ptr + Ptr + suspects - 1))
							} else if(length(suspects)>1) {
								cat(sprintf("Confused, found %d 0x0a's after byte %d\n",
											length(suspects), Stdf_ptr + Ptr))	
							} else {
								cat(sprintf("Could not find 0x0a after byte %d \n",
											Stdf_ptr + Ptr ))
							}
						}
					}
				} else {
					# REVISIT.. need to put off parsing record until we've read in more of
					# the STDF file
					# .. or mabe be last record?
					# maybe need to add a STDF_end_reached flag?
				}

			}
			parse_stdf_record(rec_typ,rec_sub,skip_TSRs,rec_len,Endy) # also uses Stdf,Ptr
			
			if ((Ptr+1) <= in_bytes) {
				rec_len = readBin(Stdf[Ptr:(Ptr+1)],integer(),n=1,size=2,
											endian=Endy,signed=FALSE)
				if ((Ptr+3+rec_len) <= in_bytes) {
					# if the next record is completely contained in the portion of the stdf
					# file we have loaded into memory, then we'll be using this rec_len,
					# and we can advance the pointer accordingly
					Ptr <<- Ptr+2
				} else {
					rec_len = in_bytes + 1
				}
			} else {
				# the next stdf record goes off the end of the raw vector we have loaded,
				# exit the while() loop to get fetch another chunk of the stdf file
				rec_len = in_bytes + 1    
			}
		}
		#browser()

		MoreStdf = readBin(STDF,raw(),n=read_bytes)
		in_bytes = length(MoreStdf)
		if( (Stdf_size_true==0) && (in_bytes<read_bytes) ) {
			# when we get to end of .gz file, we can switch from
			# estimate to actual progress tracking
			Stdf_size = Stdf_ptr + Ptr + in_bytes
			Stdf_size_true==1
		}
    }
    close(STDF)
	if (stdf_dir != "")  setwd(my_dir)





    if (Debug3) {
        cat(sprintf("%d pin names extracted from PMRs \n",length(Pin_names)))
        cat("Printing Pin names extracted from PMRs... \n")
        cat(Pin_names,fill=TRUE)
        cat("...Done printing Pin names \n")
    }


	dbg_1 = 1

    # resize from allocated size to used size,
	# also pack into Frame 
    #----------------------------------------------
    ResultsMatrix <<- ResultsMatrix[1:Device_count,]
    if(Do_testflag_matrix)  TestFlagMatrix <<- TestFlagMatrix[1:Device_count,]
    if(Do_test_order_matrix)  TestOrderMatrix <<- TestOrderMatrix[1:Device_count,]
	if(Do_mult_limits>0)  MultLimIndexMatrix <<- MultLimIndexMatrix[1:Device_count,]

	# above creates a vector instead of a matrix if only 1 device, so below fixes this.
	if(!is.matrix(ResultsMatrix)) {
		ResultsMatrix <<- matrix(data=ResultsMatrix,nrow=Device_count,ncol=Parameter_count)
		if(Do_testflag_matrix)  TestFlagMatrix <<- matrix(data=TestFlagMatrix,
									nrow=Device_count,ncol=Parameter_count)
		if(Do_test_order_matrix)  TestOrderMatrix <<- matrix(data=TestOrderMatrix,
									nrow=Device_count,ncol=Parameter_count)
		if(Do_mult_limits>0)  MultLimIndexMatrix <<- matrix(data=MultLimIndexMatrix,
									nrow=Device_count,ncol=Parameter_count)
	}

	dbg_1 = 2

	if(Do_conditions || Do_DTRs || Parse_PRR_part_txt) {
		my_code =    "my_list = list(part_id=\"\", temp=NaN,"
		my_code[2] = "x_coord=NaN, y_coord=NaN,"
		my_code[3] = "wafer_index=NaN,"
		my_code[4] = "soft_bin=NaN, hard_bin=NaN,"
		my_code[5] = "testtime=NaN, site=NaN"
		if (Conditions_count>0) {
			for (i in 1:Conditions_count) {
				if (length(grep("^temp$",Condition_Names[i],ignore.case=TRUE))>0) {
					my_code[5+i] = ""
					Devices_temp[1:Device_count] <<- ConditionsMatrix[1:Device_count,i]
				} else {
					my_code[5+i] = sprintf(",%s=NaN",Condition_Names[i])
				}
			}
		} else {
			i = 0
		}
		if (DTR_Names_count>0) {
			for (j in 1:DTR_Names_count) {
				my_code[5+i+j] = sprintf(",%s=NaN",DTR_Names[j])
			}
		} else {
			j = 0
		}
		if (PRRtxt_Names_count>0) {
			for (k in 1:PRRtxt_Names_count) {
				my_code[5+i+j+k] = sprintf(",%s=NaN",PRRtxt_Names[k])
			}
		} else {
			k = 0
		}
		my_code[6+i+j+k] = ")"

		eval(parse(text=my_code))
	} else {
		my_list = list(part_id="", temp=NaN,
                x_coord=NaN, y_coord=NaN,
                wafer_index=NaN,
                soft_bin=NaN, hard_bin=NaN,
                testtime=NaN, site=NaN)
	}
    DevicesFrame <- data.frame(rbind(my_list))
    DevicesFrame[1:Device_count,"part_id"] <- Devices_part_id[1:Device_count]
    DevicesFrame[1:Device_count,"temp"] <- Devices_temp[1:Device_count]
    DevicesFrame[1:Device_count,"x_coord"] <- Devices_x_coord[1:Device_count]
    DevicesFrame[1:Device_count,"y_coord"] <- Devices_y_coord[1:Device_count]
    DevicesFrame[1:Device_count,"wafer_index"] <- Devices_wafer_index[1:Device_count]
    DevicesFrame[1:Device_count,"soft_bin"] <- Devices_soft_bin[1:Device_count]
    DevicesFrame[1:Device_count,"hard_bin"] <- Devices_hard_bin[1:Device_count]
    DevicesFrame[1:Device_count,"testtime"] <- Devices_testtime[1:Device_count]
    DevicesFrame[1:Device_count,"site"] <- Devices_site[1:Device_count]
	DevicesFrame[1:Device_count,"source_dataset"] <- gsub("\\.gz$","",stdf_name)
    if(Do_conditions && (Conditions_count>0)) {
		for (i in 1:Conditions_count) {
			if (length(grep("^temp$",Condition_Names[i],ignore.case=TRUE))<1) {
				DevicesFrame[1:Device_count,Condition_Names[i]] <- ConditionsMatrix[1:Device_count,i]
			}
		}
	}
    if(Do_DTRs && (DTR_Names_count>0)) {
		for (i in 1:DTR_Names_count) {
				DevicesFrame[1:Device_count,DTR_Names[i]] <- DTRsMatrix[1:Device_count,i]
			}
	}
    if(Parse_PRR_part_txt && (PRRtxt_Names_count>0)) {
		#browser()
		for (i in 1:PRRtxt_Names_count) {
				DevicesFrame[1:Device_count,PRRtxt_Names[i]] <- PRRtxt_Matrix[1:Device_count,i]
			}
	}

	dbg_1 = 3

	my_list = list(testnum=NaN, testname="",
	                scaler=NaN, units="",
	                ll=NaN, ul=NaN,
					ll_ge=NaN, ul_ge=NaN,
	                plot_ll=NaN, plot_ul=NaN)
	ParametersFrame <<- data.frame(rbind(my_list))
	ParametersFrame[1:Parameter_count,"testnum"] <- Parameters_testnum[1:Parameter_count]
	ParametersFrame[1:Parameter_count,"testname"] <- Parameters_Names[1:Parameter_count]
	ParametersFrame[1:Parameter_count,"scaler"] <- Parameters_scaler[1:Parameter_count]
	ParametersFrame[1:Parameter_count,"units"] <- Parameters_units[1:Parameter_count]
	ParametersFrame[1:Parameter_count,"ll"] <- Parameters_ll[1:Parameter_count]
	ParametersFrame[1:Parameter_count,"ul"] <- Parameters_ul[1:Parameter_count]
	ParametersFrame[1:Parameter_count,"ll_ge"] <- Parameters_ll_ge[1:Parameter_count]
	ParametersFrame[1:Parameter_count,"ul_ge"] <- Parameters_ul_ge[1:Parameter_count]
	ParametersFrame[1:Parameter_count,"plot_ll"] <- Parameters_plot_ll[1:Parameter_count]
	ParametersFrame[1:Parameter_count,"plot_ul"] <- Parameters_plot_ul[1:Parameter_count]

	dbg_1 = 4

	if(TSR_count>0) {
        my_list = list(testnum=NaN, testname="",
						test_typ="",
						exec_cnt=NaN,fail_cnt=NaN,
						fixed_exec_cnt=NaN,
						fixed_fail_cnt=NaN)
		TSRFrame <<- data.frame(rbind(my_list))
		TSRFrame[1:TSR_count,"testnum"] <<- TSRs_testnum[1:TSR_count]
		TSRFrame[1:TSR_count,"testname"] <<- TSRs_testname[1:TSR_count]
		TSRFrame[1:TSR_count,"test_typ"] <<- TSRs_test_typ[1:TSR_count]
		TSRFrame[1:TSR_count,"exec_cnt"] <<- TSRs_exec_cnt[1:TSR_count]
		TSRFrame[1:TSR_count,"fail_cnt"] <<- TSRs_fail_cnt[1:TSR_count]
		TSRFrame[1:TSR_count,"fixed_exec_cnt"] <<- TSRs_fixed_exec_cnt[1:TSR_count]
		TSRFrame[1:TSR_count,"fixed_fail_cnt"] <<- TSRs_fixed_fail_cnt[1:TSR_count]
	} else if(TSR_count_siteA>0) {
		cat(sprintf("No overall TSR records found, using site %d TSR records\n",TSR_siteA))
		# no overall TSR records, so assume single site, use TSR's from first encountered site
        my_list = list(testnum=NaN, testname="",
						test_typ="",
						exec_cnt=NaN,fail_cnt=NaN,
						fixed_exec_cnt=NaN,
						fixed_fail_cnt=NaN)
		TSRFrame <<- data.frame(rbind(my_list))
		TSRFrame[1:TSR_count_siteA,"testnum"] <<- TSRs_siteA_testnum[1:TSR_count_siteA]
		TSRFrame[1:TSR_count_siteA,"testname"] <<- TSRs_siteA_testname[1:TSR_count_siteA]
		TSRFrame[1:TSR_count_siteA,"test_typ"] <<- TSRs_siteA_test_typ[1:TSR_count_siteA]
		TSRFrame[1:TSR_count_siteA,"exec_cnt"] <<- TSRs_siteA_exec_cnt[1:TSR_count_siteA]
		TSRFrame[1:TSR_count_siteA,"fail_cnt"] <<- TSRs_siteA_fail_cnt[1:TSR_count_siteA]
		TSRFrame[1:TSR_count_siteA,"fixed_exec_cnt"] <<- TSRs_siteA_fixed_exec_cnt[1:TSR_count_siteA]
		TSRFrame[1:TSR_count_siteA,"fixed_fail_cnt"] <<- TSRs_siteA_fixed_fail_cnt[1:TSR_count_siteA]

		TSR_count <<- TSR_count_siteA
	}

	#elapsed_seconds = proc.time()[3] - timestamp0
	#cat(sprintf("... done building RTDF file at %.2f seconds... \n",
	#			elapsed_seconds))

	dbg_1 = 5

	# fix SMAR test stupidities...
    # SMAR = software monkeys aren't reliable  :)
    #-----------------------------------------
    if( (Executive_type=="93000" && auto_93k) && (dim(ResultsMatrix)[2] > 0) ) {
        max_params = dim(ParametersFrame)[1]
		for (j in 1:max_params) {
            ll = ParametersFrame$ll[[j]]
            ul = ParametersFrame$ul[[j]]

			dbg_1 = 5.1

			#browser()

			# find limits that are the wrong polarity and fix them
			#------------------------------------------------------
			# if we have TestFlagMatrix, use that as sanity check, else look for median result
			#... won't work
			#    93K MPR records - all tests as fail, not just the failing test so below
			#                      won't work ?
			if(Do_testflag_matrix) {
				fix_it = FALSE

				flags_vector = TestFlagMatrix[,j]
				pass_flag_indices = which(flags_vector==0)

				results_vector = ResultsMatrix[,j]
				pass_results_vector = results_vector[pass_flag_indices]

				# compare results vs. limits to flags with/without limit swap
				# which is closest?
				len_ll_inconc = 0
				len_ll_inconc2 = 0
				len_ul_inconc = 0
				len_ul_inconc2 = 0
				if(length(pass_results_vector) > 0) {
					if((length(ll)>0) && is.finite(ll)) {
						inconceivable = which(pass_results_vector<ll)		# should not have any pass parts <ll
						inconceivable2 = which(pass_results_vector>(-1*ll))	# what if limits are x -1?

						len_ll_inconc = length(inconceivable)
						len_ll_inconc2 = length(inconceivable2)
					}

					if((length(ul)>0) && is.finite(ul)) {
						inconceivable = which(pass_results_vector>ul)		# should not have any pass parts <ll
						inconceivable2 = which(pass_results_vector<(-1*ul))	# what if limits are x -1?

						len_ul_inconc = length(inconceivable)
						len_ul_inconc2 = length(inconceivable2)
					}

					if( (len_ll_inconc+len_ul_inconc) > (len_ll_inconc2+len_ul_inconc2)) {
						# if more good parts fail limits vs -1x limits, then -1x
						fix_it = TRUE
						#browser()
					}
				}
				# REVISIT ... not done here!

			} else {
				results_vector = ResultsMatrix[,j]
				results_vector = results_vector[is.finite(results_vector)]
				med = median(results_vector)
				fix_it = FALSE
				if(is.finite(med)) {
					if((length(ll)>0) && is.finite(ll)) {
						if( (med<ll) && (med<(-1.0*ll)) ) {
							if(is.finite(ul)) {
								if( med>(-1.0*ul) )  fix_it = TRUE
							} else {
								fix_it = TRUE
							}
						}
					} else if((length(ul)>0) && is.finite(ul)) {
						if( (med>ul) && (med<(-1.0*ul)) )  {
							fix_it=TRUE
						}
					}
				}
			}
            if(fix_it) {
				#browser()
                ParametersFrame$ll[[j]] = -1.0*ul
                ParametersFrame$ul[[j]] = -1.0*ll
            }

			# find bogus LL's or UL's and remove them...
			# ie. if UL = 0.0 and UL is <LL, then it is bogus!
			#     if LL = 0.0 and LL is >UL, then it is bogus!
			#--------------------------------------------------
			if( (length(ll)>0) && (length(ul)>0) && is.finite(ll) && is.finite(ul) ) {
				if(ul<ll) {
					if(ul==0.0) {
						ParametersFrame$ul[[j]] = NaN
					} else if(ll==0.0) {
						ParametersFrame$ll[[j]] = NaN
					}
				}
			}

        }

    }

	dbg_1 = 6

	
	# remove bogus Eagle device if present...
	#------------------------------------------
    if(Executive_type=="Eagle" && (dim(ResultsMatrix)[2] > 0)) {
		# "Test Plan Delimiter"
		my_partid = as.character(DevicesFrame[[1,"part_id"]])
		if(my_partid=="Test Plan Delimiter") {
			keepers = rep(TRUE,dim(DevicesFrame)[1])
			keepers[1] = FALSE

			cat(sprintf("...removing Test Plan Delimiter device...\n"))
			DevicesFrame = DevicesFrame[keepers,]
			ResultsMatrix = ResultsMatrix[keepers,]
		}
	}


	# try to fix TSR stupidities: shame on quartet, shame on 93k
    #------------------------------------------------------------
    #for (i in 1:dim(TSRFrame)[1]) {
    #   test_typ = TSRFrame[i,"test_typ"]
    #   if (test_typ == "M" && (i>1)) {
    #       if (Executive_type=="93000" || Executive_type=="Credence") {
    #           exec_cnt = TSRFrame[i,"exec_cnt"]
    #           last_fail_cnt = TSRFrame[i-1,"fail_cnt"]
    #           last_fixed_exec_cnt = TSRFrame[i-1,"fixed_exec_cnt"]
    #           if (exec_cnt>last_fixed_exec_cnt) {
    #               last_exec_cnt = TSRFrame[i-1,"exec_cnt"]
    #               if (exec_cnt == last_exec_cnt) {
    #                   TSRFrame[i,"fixed_exec_cnt"] <<- last_fixed_exec_cnt
    #               } else if (exec_cnt == (last_exec_cnt+last_fixed_exec_cnt)) {
    #                   TSRFrame[i,"fixed_exec_cnt"] <<- last_fixed_exec_cnt
    #               }
    #           }
    #       }
    #   }
    #}


	# print out the number of unknown record if there were any
	#----------------------------------------------------------
	if(Unknown_rec_count>0) {
		cat(sprintf("WARNING: %d Unknown records when parsing this file!\n",Unknown_rec_count))
	}


	# if stdf file just contains PIR/PRR records, then
	# add a bogus test with NaN values so various scripts don't choke
	#-----------------------------------------------------------------
	if(dim(ResultsMatrix)[2]<1) {
		ResultsMatrix <<- cbind(ResultsMatrix,NaN)	
	}


	# should we try to reorder ParametersFrame based on TestOrderMatrix?
	if( Use_test_order_matrix ) {
		cat(sprintf("Now reordering ParametersFrame based on TestOrderMatrix...\n"))

		# make cross-reference list to existing ParametersFrame order, we'll sort this,
		# then do all the mapping at the end.
		xref_ParametersFrame = c(1:dim(ParametersFrame)[1])	

		# first device will have its tests in order, so start from there.
		last_sorted = which(TestOrderMatrix[1,]==max(TestOrderMatrix[1,],na.rm=TRUE))
		if(last_sorted < dim(ParametersFrame)[1]) {
			for (next_to_sort in (last_sorted+1):dim(ParametersFrame)[1]) {
				# which devices actually have results for this test?  
				valid_results = which(is.finite(TestOrderMatrix[,next_to_sort]))
				valid_part = valid_results[1]	# there should always be at least one!
				curr_order = TestOrderMatrix[valid_part,next_to_sort]
				prev_order = curr_order - 1
				unsorted_prev_param = which(TestOrderMatrix[valid_part,]==prev_order)
				# but this is the presorted index
				prev_param = which(xref_ParametersFrame==unsorted_prev_param)

				# ok, we will want to add next_to_sort at least after prev_param,
				# so step through params until we've gone too far
				curr_param = prev_param + 1
				sorted = FALSE
				while(!sorted && (curr_param < next_to_sort)) {
					valid_indices1 = which(is.finite(TestOrderMatrix[,next_to_sort]))
					valid_indices2 = which(is.finite(TestOrderMatrix[,xref_ParametersFrame[curr_param]]))
					indices = intersect(valid_indices1,valid_indices2)
					if(length(indices)>0) {
						min_incr = min(TestOrderMatrix[indices,next_to_sort] - 
								TestOrderMatrix[indices,xref_ParametersFrame[curr_param]],na.rm=TRUE)
					}
					if ((length(indices)>0) && min_incr<0) {
						# place the next_to_sort param before the curr_param
						# really, just shuffle xref_ParametersFrame vector
						xref_ParametersFrame[(curr_param+1):next_to_sort] = xref_ParametersFrame[curr_param:(next_to_sort-1)]
						xref_ParametersFrame[curr_param] = next_to_sort
						sorted = TRUE
					} else if(length(indices)>0) {
						curr_param = curr_param + 1
					} else {
						# we don't actually know... 50/50 guess,
						# can we be smarter?  REVISIT
						curr_param = curr_param + 1
					}
				}
			}
		}

		# now re-sort all the Frames and Matrices
		old_ParametersFrame = ParametersFrame
		old_ResultsMatrix = ResultsMatrix
		old_TestFlagMatrix = TestFlagMatrix
		old_TestOrderMatrix = TestOrderMatrix

		ParametersFrame <- old_ParametersFrame[xref_ParametersFrame,]	# Frames were local, not global
		ResultsMatrix <<- old_ResultsMatrix[,xref_ParametersFrame]
		TestFlagMatrix <<- old_TestFlagMatrix[,xref_ParametersFrame]
		TestOrderMatrix <<- old_TestOrderMatrix[,xref_ParametersFrame]

		if(Do_mult_limits>0) {
			old_MultLimIndexMatrix = MultLimIndexMatrix
			MultLimIndexMatrix <<- old_MultLimIndexMatrix[,xref_ParametersFrame]
			if(sum(dim(MultLim_ll_Matrix))>0) {
				old_MultLim_ll_Matrix = MultLim_ll_Matrix
				old_MultLim_ul_Matrix = MultLim_ul_Matrix
				old_MultLim_ll_ge_Matrix = MultLim_ll_ge_Matrix
				old_MultLim_ul_ge_Matrix = MultLim_ul_ge_Matrix
				MultLim_ll_Matrix <<- old_MultLim_ll_Matrix[xref_ParametersFrame,]
				MultLim_ul_Matrix <<- old_MultLim_ul_Matrix[xref_ParametersFrame,]
				MultLim_ll_ge_Matrix <<- old_MultLim_ll_ge_Matrix[xref_ParametersFrame,]
				MultLim_ul_ge_Matrix <<- old_MultLim_ul_ge_Matrix[xref_ParametersFrame,]
			}
		}
		cat(sprintf("... Done reordering ParametersFrame based on TestOrderMatrix\n"))
	}


	# above creates a vector instead of a matrix if only 1 device, so below fixes this.
	if(!is.matrix(ResultsMatrix)) {
		ResultsMatrix <<- matrix(data=ResultsMatrix,nrow=Device_count,ncol=Parameter_count)
		if(Do_testflag_matrix)  TestFlagMatrix <<- matrix(data=TestFlagMatrix,
									nrow=Device_count,ncol=Parameter_count)
		if(Do_test_order_matrix)  TestOrderMatrix <<- matrix(data=TestOrderMatrix,
									nrow=Device_count,ncol=Parameter_count)
		if(Do_mult_limits>0)  MultLimIndexMatrix <<- matrix(data=MultLimIndexMatrix,
									nrow=Device_count,ncol=Parameter_count)
	}


    # save Rtdf file
    #-----------------
    my_list = c("LotInfoFrame","ParametersFrame","DevicesFrame","ResultsMatrix")

	if ( (Do_testflag_matrix) && (dim(TestFlagMatrix)[1]>0) && (dim(TestFlagMatrix)[2]>0) ) {
        my_list[length(my_list)+1] = "TestFlagMatrix"
	}
	if ( (Save_test_order_matrix) && (dim(TestOrderMatrix)[1]>0) && (dim(TestOrderMatrix)[2]>0) ) {
        my_list[length(my_list)+1] = "TestOrderMatrix"
	}
    if (Hbin_count>0) {
        my_list[length(my_list)+1] = "HbinInfoFrame"
    } else {
        cat(sprintf("End of file reached with no hardbin summary \n"))
    }
    if (Sbin_count>0) {
        my_list[length(my_list)+1] = "SbinInfoFrame"
    } else {
        cat(sprintf("End of file reached with no softbin summary \n"))
    }
    if (TSR_count>0) {
        my_list[length(my_list)+1] = "TSRFrame"
    } else {
        cat(sprintf("End of file reached with no test synopsis records \n"))
    }
    if (Wafer_count>0) {
        my_list[length(my_list)+1] = "WafersFrame"
        if (Valid_WCR)  my_list[length(my_list)+1] = "WaferInfoFrame"
    }
	if (SiteSbinCount>1) {
		my_list[length(my_list)+1] = "SiteSbinInfoFrame"
		my_list[length(my_list)+1] = "SiteSbinSiteVector"
		my_list[length(my_list)+1] = "SiteSbinCountMatrix"
	}

	if (Do_mult_limits>0) {
		cat(sprintf("Now Separating based on unique Limit Sets...\n"))
		# REVISIT: temporary debug dump while coding this feature!
		dump_MultLim_objects = FALSE
		if (dump_MultLim_objects) {
			my_list[length(my_list)+1] = "MultLimIndexMatrix"
			my_list[length(my_list)+1] = "MultLim_ll_Matrix"
			my_list[length(my_list)+1] = "MultLim_ul_Matrix"
			my_list[length(my_list)+1] = "MultLim_ll_ge_Matrix"
			my_list[length(my_list)+1] = "MultLim_ul_ge_Matrix"
			my_list[length(my_list)+1] = "MultLim_idx"
		}

		# we now need to step through devices to see how many unique limit sets we have,
		# which parts go with which limit set,
		# and dump to separate RTDF files based on limit set.
		LimSetIndex = rep(1,times=Device_count)
		for (i in 1:Device_count) {
			found_it = FALSE
			if (i==1) {
				LimSetMatrix = matrix(MultLimIndexMatrix[1,],nrow=1,ncol=Parameter_count)
				LimSets = 1
			} else {
				valid_tests = which(is.finite(MultLimIndexMatrix[i,]))
				lim_set = 1
				while(!found_it && (lim_set <= LimSets)) {
					# does this device's limits match an existing set?
					limset_valid_tests = which(is.finite(LimSetMatrix[lim_set,]))
					common_tests = intersect(valid_tests,limset_valid_tests)
					if(all(MultLimIndexMatrix[i,common_tests]==LimSetMatrix[lim_set,common_tests])) {
						found_it = TRUE
						LimSetIndex[i] = lim_set
						# do we have any new tests to add to this LimSet?
						new_tests = setdiff(valid_tests,limset_valid_tests)
						LimSetMatrix[lim_set,new_tests] = MultLimIndexMatrix[i,new_tests]
					}
					lim_set = lim_set + 1
				}
				if(!found_it) {
					if(LimSets<Do_mult_limits) {
						# add to LimSetMatrix
						LimSetIndex[i] = lim_set
						LimSetMatrix = rbind( LimSetMatrix, MultLimIndexMatrix[i,] )
						LimSets = LimSets + 1
					} else {
						# REVISIT...
						cat("Oh poop!  need to fix this!")
					}
				}
			}
		}

		if (dump_MultLim_objects) {
			my_list[length(my_list)+1] = "LimSetIndex"
			my_list[length(my_list)+1] = "LimSetMatrix"
		}

    	save(list=my_list, file=rtdf_name)

		# now cycle through the unique datasets and dump into separate RTDF files...
		if(LimSets>1) {
			orig_ParametersFrame = ParametersFrame	# we'll be overwriting the limits here
			orig_DevicesFrame = DevicesFrame
			orig_ResultsMatrix = ResultsMatrix
			# what other objects will get resized per limit set?
			if(Do_testflag_matrix)  orig_TestFlagMatrix = TestFlagMatrix
			if(Do_test_order_matrix)  orig_TestOrderMatrix = TestOrderMatrix
			orig_MultLimIndexMatrix = MultLimIndexMatrix

			base_rtdf_name = as.character(strsplit(rtdf_name,"[.]rtdf$"))

			for (i in 1:LimSets) {
				ParametersFrame = orig_ParametersFrame
				alt_limits = which(LimSetMatrix[i,]>0)
				if(length(alt_limits)>0) {
					#browser()
					#ParametersFrame[alt_limits,"ll"] = MultLim_ll_Matrix[alt_limits,LimSetMatrix[i,alt_limits]]
					my_vec = function(x) MultLim_ll_Matrix[x,LimSetMatrix[i,x]]
					ParametersFrame[alt_limits,"ll"] = mapply(my_vec,alt_limits)
					my_vec = function(x) MultLim_ul_Matrix[x,LimSetMatrix[i,x]]
					ParametersFrame[alt_limits,"ul"] = mapply(my_vec,alt_limits)
					my_vec = function(x) MultLim_ll_ge_Matrix[x,LimSetMatrix[i,x]]
					ParametersFrame[alt_limits,"ll_ge"] = mapply(my_vec,alt_limits)
					my_vec = function(x) MultLim_ul_ge_Matrix[x,LimSetMatrix[i,x]]
					ParametersFrame[alt_limits,"ul_ge"] = mapply(my_vec,alt_limits)
				}

				keepers = which(LimSetIndex==i)
				dev_count = length(keepers)

				DevicesFrame <- orig_DevicesFrame[keepers,]
				ResultsMatrix <<- orig_ResultsMatrix[keepers,]
				if(Do_testflag_matrix)  TestFlagMatrix <<- orig_TestFlagMatrix[keepers,]
				if(Do_test_order_matrix)  TestOrderMatrix <<- orig_TestOrderMatrix[keepers,]
				
				# above creates a vector instead of a matrix if only 1 device, so below fixes this.
				if(!is.matrix(ResultsMatrix)) {
					ResultsMatrix <<- matrix(data=ResultsMatrix,nrow=dev_count,ncol=Parameter_count)
					if(Do_testflag_matrix)  TestFlagMatrix <<- matrix(data=TestFlagMatrix,
												nrow=dev_count,ncol=Parameter_count)
					if(Do_test_order_matrix)  TestOrderMatrix <<- matrix(data=TestOrderMatrix,
												nrow=dev_count,ncol=Parameter_count)
					if(Do_mult_limits>0)  MultLimIndexMatrix <<- matrix(data=MultLimIndexMatrix,
												nrow=dev_count,ncol=Parameter_count)
				}
				lim_rtdf_name = paste(base_rtdf_name,"_lim",i,".rtdf",sep="")
				save(list=my_list, file=lim_rtdf_name)
			}
		}

		cat(sprintf("... Done separating into %d unique Limit Sets\n",LimSets))

	} else {
    	save(list=my_list, file=rtdf_name)
	}
    timestamp9 = proc.time()
    timestamp9 = timestamp9[3] - timestamp0
    if (timestamp9<200.0) {
        cat(sprintf("Conversion Finished! \n processed %d Devices x %d Parameters in %.2f seconds\n",
                Device_count,Parameter_count,timestamp9))
    } else {
        cat(sprintf("Conversion Finished! \n processed %d Devices x %d Parameters in %.2f minutes\n",
                Device_count,Parameter_count,timestamp9/60.0))
    }

    # generate summary file
    #-----------------------
    if (do_summary) {

        # build filename
        #---------------
        if (regexpr("[.]Rdata$",rtdf_name)>0) {
            out_file = as.character(strsplit(rtdf_name,"[.]Rdata$"))
        } else if (regexpr("[.]Rtdf$",rtdf_name)>0) {
            out_file = as.character(strsplit(rtdf_name,"[.]Rtdf$"))
        } else if (regexpr("[.]rtdf$",rtdf_name)>0) {
            out_file = as.character(strsplit(rtdf_name,"[.]rtdf$"))
        }
        out_file = paste(out_file,".summary",sep="")
        cat(sprintf("Writing summary file to %s...\n",out_file))

        print_summary(stdf_name,out_file,just_fail_tests_summary)

        cat(sprintf("FINISHED\n"))

    }

	if (Debug7) {
		cat(sprintf("Previous_param_i was used %d times \n",Good_guesses))
	}
	#cat(sprintf("Another_guess was used %d times \n",Good_guesses2))


	if(Do_mult_limits>0) {
		# if the multiple limits flag is set, then search through MultLimIndexMatrix to
		# find common groups of limits and split into associated RTDF files

	}
}


###############################################################################
valid_record_type <- function(rec_typ,rec_sub) {

	# This needs to be updated anytime there is a change to parse_stdf_record()

	
	valid = FALSE
	
	if (rec_typ == 0) {     # per file information
        if (rec_sub == 10) {
            valid = TRUE
        } else if (rec_sub == 20) {
            valid = TRUE
        } else if (rec_sub == 30) {
            valid = TRUE
        }
    } else if (rec_typ == 1) {  # per lot information
        if (rec_sub == 10) {
            valid = TRUE
        } else if (rec_sub == 20) {
            valid = TRUE
        } else if (rec_sub == 30) {
            valid = TRUE
        } else if (rec_sub == 40) {
            valid = TRUE
        } else if (rec_sub == 50) {
            valid = TRUE
        } else if (rec_sub == 60) {
            valid = TRUE
        } else if (rec_sub == 62) {
            valid = TRUE
        } else if (rec_sub == 63) {
            valid = TRUE
        } else if (rec_sub == 70) {
            valid = TRUE
        } else if (rec_sub == 80) {
            valid = TRUE
        } else if (rec_sub == 90) {
            valid = TRUE
        } else if (rec_sub == 91) {
            valid = TRUE
        } else if (rec_sub == 92) {
            valid = TRUE
        } else if (rec_sub == 93) {
            valid = TRUE
        } else if (rec_sub == 94) {
            valid = TRUE
        }
    } else if (rec_typ == 2) {  # per wafer information
        if (rec_sub == 10) {
            valid = TRUE
        } else if (rec_sub == 20) {
            valid = TRUE
        } else if (rec_sub == 30) {
            valid = TRUE
        }
    } else if (rec_typ == 5) {  # per part information
        if (rec_sub == 10) {
            valid = TRUE
        } else if (rec_sub == 20) {
            valid = TRUE
        }
    } else if (rec_typ == 10) { # per test information
        if (rec_sub == 10) {
            valid = TRUE
            #parse_PDR_record(...)	# stdf v3 only!
        } else if (rec_sub == 20) {
            valid = TRUE
            #parse_FDR_record(...)	# stdf v3 only!
        } else if (rec_sub == 30) {
            valid = TRUE
        }
    } else if (rec_typ == 15) { # per test execution information
		if (rec_sub == 10) {
            valid = TRUE
		} else if (rec_sub == 15) {
            valid = TRUE
		} else if (rec_sub == 20) {
            valid = TRUE
		} else if (rec_sub == 30) {
            valid = TRUE
		}
    } else if (rec_typ == 20) { # per program segment information
		if (rec_sub == 10) {
            valid = TRUE
		} else if (rec_sub == 20) {
            valid = TRUE
		}
    } else if (rec_typ == 25) { # per site information (stdf v3 only)
        if (rec_sub == 10) {
            valid = TRUE
        } else if (rec_sub == 20) {
            valid = TRUE
        } else if (rec_sub == 30) {
            valid = TRUE
        } else if (rec_sub == 40) {
            valid = TRUE
        }
    } else if (rec_typ == 50) { # generic data
        if (rec_sub == 10) {
            valid = TRUE
        } else if (rec_sub == 30) {
            valid = TRUE
		}
    } else if (rec_typ == 180) { # reserved for Image
            valid = TRUE
    } else if (rec_typ == 181) { # reserved for IG900
            valid = TRUE
    } else if (rec_typ == 220) { # stdf+
        if (rec_sub == 201) {
            valid = TRUE
        } else if (rec_sub == 202) {
            valid = TRUE
        } else if (rec_sub == 203) {
            valid = TRUE
        } else if (rec_sub == 204) {
            valid = TRUE
        } else if (rec_sub == 205) {
            valid = TRUE
        } else if (rec_sub == 206) {
            valid = TRUE
		}
    }

	if(!valid) {
		#location = Stdf_ptr + Ptr
		#cat(sprintf("Invalid record type %d %d at byte %d \n",
		#		rec_typ,rec_sub,location))
	}

	return(valid)
}


###############################################################################
print_summary <- function(stdf_name,out_file,just_fail_tests_summary) {

        # dump MIR information
        #-----------------------
        my_string = paste("SUMMARY for file: ",stdf_name,"\n",sep="")
        cat(my_string,file=out_file)
        my_string = "------------------------------------------------------------------------\n"
        cat(my_string,file=out_file,append=TRUE)
        my_string = paste("Lot ID:       ",as.character(LotInfoFrame[["lotid"]]),"\n",sep="")
        cat(my_string,file=out_file,append=TRUE)
        my_string = paste("Sublot ID:    ",as.character(LotInfoFrame[["sublotid"]]),"\n",sep="")
        cat(my_string,file=out_file,append=TRUE)
        my_string = paste("Part Type:    ",as.character(LotInfoFrame[["part_typ"]]),"\n",sep="")
        cat(my_string,file=out_file,append=TRUE)

		my_string = paste("Test Program: ",as.character(LotInfoFrame[["program"]]),"\n",sep="")
        cat(my_string,file=out_file,append=TRUE)
        my_string = paste("Job Revision: ",as.character(LotInfoFrame[["job_rev"]]),"\n",sep="")
        cat(my_string,file=out_file,append=TRUE)

		my_string = paste("Tester Type:  ",as.character(LotInfoFrame[["tester_type"]]),"\n",sep="")
        cat(my_string,file=out_file,append=TRUE)
		my_string = paste("Tester ID:    ",as.character(LotInfoFrame[["tester_id"]]),"\n",sep="")
        cat(my_string,file=out_file,append=TRUE)
        my_string = paste("Exec Type:    ",as.character(LotInfoFrame[["exec_typ"]]),"\n",sep="")
        cat(my_string,file=out_file,append=TRUE)
        my_string = paste("Exec Version: ",as.character(LotInfoFrame[["exec_ver"]]),"\n",sep="")
        cat(my_string,file=out_file,append=TRUE)
        my_string = paste("Handler ID:   ",as.character(LotInfoFrame[["handler"]]),"\n",sep="")
        cat(my_string,file=out_file,append=TRUE)
        my_string = paste("Operator:     ",as.character(LotInfoFrame[["oper_nam"]]),"\n",sep="")
        cat(my_string,file=out_file,append=TRUE)

		my_string = paste("Test Code:    ",as.character(LotInfoFrame[["test_cod"]]),"\n",sep="")
        cat(my_string,file=out_file,append=TRUE)
        my_string = paste("Mode Code:    ",as.character(LotInfoFrame[["mode_cod"]]),"\n",sep="")
        cat(my_string,file=out_file,append=TRUE)
        my_string = paste("Retest Code:  ",as.character(LotInfoFrame[["rtst_cod"]]),"\n",sep="")
        cat(my_string,file=out_file,append=TRUE)

		start_t = ISOdatetime(1970,1,1,0,0,0) + as.numeric(LotInfoFrame[["start_t"]])
        finish_t = ISOdatetime(1970,1,1,0,0,0) + as.numeric(LotInfoFrame[["finish_t"]])
        my_t = as.POSIXlt(start_t)
        my_tzs = attr(my_t,"tzone")
        if (length(my_tzs)==3) {
            my_tz = as.character(my_tzs[2+(my_t$isdst)])
        } else {
            my_tz = as.character(my_tzs)
        }
        #my_string = paste("Start Time:   ",start_t," ",my_tz,"\n",sep="")
        my_string = paste("Start Time:   ",start_t,"\n",sep="")		# timezone info is bogus!
        cat(my_string,file=out_file,append=TRUE)
        my_string = paste("Finish Time:  ",finish_t,"\n",sep="")
        cat(my_string,file=out_file,append=TRUE)
        my_string = "------------------------------------------------------------------------\n"
        cat(my_string,file=out_file,append=TRUE)
        
        if (Sbin_count>0) {
            # SBR stuff: sort SbinInfoFrame by sbin_cnt...
            #------------------------------------------
            sbin_cnts = as.integer(SbinInfoFrame[["sbin_cnt"]])
            sorted = sort(sbin_cnts,decreasing=TRUE,index.return=TRUE)
            xrefs = sorted$ix
            total = sum(sbin_cnts)

            my_string = paste("Soft Binning Summary: Part Count ",total,"\n\n",sep="")
			cat(my_string,file=out_file,append=TRUE)

            my_string = "_Count  _____% Bin_no  _  Soft_Bin_Name_____________\n"
            cat(my_string,file=out_file,append=TRUE)
            # dump sbin count, %, bin #, type, name
            for (i in 1:length(xrefs)) {
                my_string = sprintf("%6d  %6.1f %6d  %s  %s \n",
                            as.integer(SbinInfoFrame[xrefs[i],"sbin_cnt"]),
                            100.0*as.numeric(SbinInfoFrame[xrefs[i],"sbin_cnt"])/total,
                            as.integer(SbinInfoFrame[xrefs[i],"sbin_num"]),
                            as.character(SbinInfoFrame[xrefs[i],"sbin_pf"]),
                            as.character(SbinInfoFrame[xrefs[i],"sbin_nam"]) )
                cat(my_string,file=out_file,append=TRUE)
            }
            my_string = "------------------------------------------------------------------------\n"
            cat(my_string,file=out_file,append=TRUE)
        }

		if (length(SiteSbinSiteVector)>1) {
			sbin_cnts = colSums(SiteSbinCountMatrix,na.rm=TRUE)
			sorted = sort(sbin_cnts,decreasing=TRUE,index.return=TRUE)
			xrefs = sorted$ix
			total = sum(sbin_cnts)
			
			# per-site SBR stuff
			#---------------------------
			my_string = "Soft Binning By Site Summary:\n\n"
			cat(my_string,file=out_file,append=TRUE)

			for (i in 1:length(SiteSbinSiteVector)) {
				cat(sprintf(" site%2d",SiteSbinSiteVector[i]),file=out_file,append=TRUE)
			}
			my_string = " Bin_no  _  Soft_Bin_Name_____________\n"
			cat(my_string,file=out_file,append=TRUE)
			for (i in 1:length(xrefs)) {
				for (j in 1:length(SiteSbinSiteVector)) {
					sbin_cnt = SiteSbinCountMatrix[j,xrefs[i]]
					if(!is.finite(sbin_cnt))  sbin_cnt = 0
					cat(sprintf(" %6d",sbin_cnt),file=out_file,append=TRUE)
				}
				my_string = sprintf(" %6d  %s  %s \n",
						as.integer(SiteSbinInfoFrame[xrefs[i],"sbin_num"]),
						as.character(SiteSbinInfoFrame[xrefs[i],"sbin_pf"]),
						as.character(SiteSbinInfoFrame[xrefs[i],"sbin_nam"]) )
				cat(my_string,file=out_file,append=TRUE)
			}
			# SiteSbinInfoFrame
			# SiteSbinSiteVector
			# SiteSbinCountMatrix
			my_string = "------------------------------------------------------------------------\n"
			cat(my_string,file=out_file,append=TRUE)
		}

        if (Hbin_count>0) {
            # HBR stuff: sort HbinInfoFrame by hbin_cnt...
            #------------------------------------------
            hbin_cnts = as.integer(HbinInfoFrame[["hbin_cnt"]])
            sorted = sort(hbin_cnts,decreasing=TRUE,index.return=TRUE)
            xrefs = sorted$ix
            total = sum(hbin_cnts)

            my_string = paste("Hard Binning Summary: Part Count ",total,"\n\n",sep="")
			cat(my_string,file=out_file,append=TRUE)

            my_string = "_Count  _____% Bin_no  _  Hard_Bin_Name_____________\n"
            cat(my_string,file=out_file,append=TRUE)
            # dump sbin count, %, bin #, type, name
            for (i in 1:length(xrefs)) {
                my_string = sprintf("%6d  %6.1f %6d  %s  %s \n",
                            as.integer(HbinInfoFrame[xrefs[i],"hbin_cnt"]),
                            100.0*as.numeric(HbinInfoFrame[xrefs[i],"hbin_cnt"])/total,
                            as.integer(HbinInfoFrame[xrefs[i],"hbin_num"]),
                            as.character(HbinInfoFrame[xrefs[i],"hbin_pf"]),
                            as.character(HbinInfoFrame[xrefs[i],"hbin_nam"]) )
                cat(my_string,file=out_file,append=TRUE)
            }
            my_string = "------------------------------------------------------------------------\n"
            cat(my_string,file=out_file,append=TRUE)
        }


        # TSR stuff
        #------------------------------------------------------
        if (TSR_count>0) {       
            total = TSRFrame[[1,"exec_cnt"]]    # or should we grab largest number?
            my_string = paste("Test Summary: Part Count ",total,"\n\n",sep="")
            cat(my_string,file=out_file,append=TRUE)

            my_string = "%_of_all  %_of_exec  execs_  fails_  Test_Name_____________\n"
            cat(my_string,file=out_file,append=TRUE)

            #  use TSRFrame order
            for (i in 1:dim(TSRFrame)[1]) {
                exec_cnt = as.numeric(TSRFrame[i,"fixed_exec_cnt"])
                fail_cnt = as.numeric(TSRFrame[i,"fixed_fail_cnt"])
                testname = as.character(TSRFrame[i,"testname"])

                if (!just_fail_tests_summary || (fail_cnt>0)) {
                    pct = 100.0*fail_cnt/total
                    pct2 = 100.0*fail_cnt/exec_cnt

                    my_string = sprintf("  %6.1f     %6.1f  %6d  %6d  %s \n",
                                pct, pct2, exec_cnt, fail_cnt, testname )
                    cat(my_string,file=out_file,append=TRUE)
                }
            }
        }

        my_string = "------------------------------------------------------------------------\n"
        cat(my_string,file=out_file,append=TRUE)

        # PRR extracted summary information
		#----------------------------------------------------------------

		# sanity check bins are consistently pass or fail
		my_pass_bins = unique(Devices_soft_bin[which(Devices_pass_flag==1)])
		my_pass_bins = my_pass_bins[which(is.finite(my_pass_bins))]
		my_fail_bins = unique(Devices_soft_bin[which(Devices_pass_flag==0)])
		confused_bins = intersect(my_pass_bins,my_fail_bins)
		if(length(confused_bins)>0) {
			# nasty message needed here
			my_string = sprintf("WARNING: both pass and fail for softbins")
			for (i in 1:length(confused_bins)) {
				my_string = sprintf("%s %d",my_string,confused_bins[i])
			}
			my_string = sprintf("%s \n",my_string)
			cat(my_string,file=out_file,append=TRUE)
			cat(my_string)
		}

		sbin_summary = as.data.frame(table(Devices_soft_bin))
		sbins = as.numeric(levels(sbin_summary[,1]))
		counts = sbin_summary[,2]
		total_count = sum(counts)

		# separate out PASSes from list
		if(length(which(sbins %in% my_pass_bins))>0) {
			i_pass_bins = which(sbins %in% my_pass_bins)
			good_sbins = sbins[i_pass_bins]
			good_counts = counts[i_pass_bins]
			good_total = sum(good_counts)
			sbins = sbins[-i_pass_bins]
			counts = counts[-i_pass_bins]
		} else {
			good_sbins = numeric()
			good_counts = numeric()
			good_total = 0
		}
		# sort fail bins in decreasing dropout order
		sorted=sort(counts,decreasing=TRUE,index.return=TRUE)
		xrefs = sorted$ix

        my_string = paste("PRR Soft Binning Summary: Part Count ",total_count,"\n\n",sep="")
		cat(my_string,file=out_file,append=TRUE)

        my_string = "_Count  _____% SBin_no  _  \n"
        cat(my_string,file=out_file,append=TRUE)
 		if (length(good_sbins)>0) {
			for (i in 1:length(good_sbins)) {
				my_string = sprintf("%6d  %6.1f %7d  %s \n",
							as.integer(good_counts[i]),
							100.0*as.numeric(good_counts[i])/total_count,
							as.integer(good_sbins[i]),
							"P" )
				cat(my_string,file=out_file,append=TRUE)
			}
		}
		if (length(xrefs)>0) {
			for (i in 1:length(xrefs)) {
				my_string = sprintf("%6d  %6.1f %7d  %s \n",
							as.integer(counts[xrefs[i]]),
							100.0*as.numeric(counts[xrefs[i]])/total_count,
							as.integer(sbins[xrefs[i]]),
							"F" )
				cat(my_string,file=out_file,append=TRUE)
			}
		}

        my_string = "------------------------------------------------------------------------\n"
        cat(my_string,file=out_file,append=TRUE)

}


#############################################################################
str_eq <- function(str1,str2) {
	# experiment to see if this code can be faster than
	# substr(test_txt,1,6)=="SWEEP="
	#... no, much slower.

	v1 <- strsplit(str1,split="")[[1]]
	v2 <- strsplit(str2,split="")[[1]]
	n2 <- nchar(str2)
	i <- 1
	matching <- TRUE
	while ((i<=n2) && matching) {
		if (v1[i]!=v2[i]) {
			matching = FALSE
		}
		i <- i + 1
	}

	return(matching)
}


###############################################################################
readSTDFstring <- function(bytes) {
    
    str_len = readBin(Stdf[Ptr],integer(),n=1,size=1,signed=FALSE)
    bytes = bytes - 1
    Ptr <<- Ptr + 1
    if (str_len > bytes) {
        cat(sprintf("ERROR: String length %d is longer than record length %d, skipping... \n",
                    str_len,bytes))
        my_string = ''
        
        if (Debug6)  browser()

        #bit_bucket = readBin(STDF,integer(),n=bytes,size=1)
        Ptr <<- Ptr + bytes
        bytes = 0
    } else {
        if (str_len > 0) {
			# Earlier versions of R allowed embedded nul bytes within character strings, 
			# but not R >= 2.8.0. readChar was commonly used to read fixed-size 
			# zero-padded byte fields for which readBin was unsuitable. readChar can 
			# still be used for such fields if there are no embedded nuls: otherwise 
			# readBin(what="raw") provides an alternative.
			#
			# readChar(con, nchars, useBytes = FALSE)
			# 
            #chars = readChar(Stdf[Ptr:(Ptr+str_len-1)],str_len)
			chars = rawToChar(Stdf[Ptr:(Ptr+str_len-1)])
            Ptr <<- Ptr + str_len
            #if(nchar(chars)!=str_len) {
            #    cat(sprintf("ERROR: read %d chars instead of %d chars \n",
            #       nchar(chars),str_len))
            #}
            my_string = paste(chars, collapse=NULL)
            bytes = bytes - str_len
        } else {
            my_string = ""
        }
    }

    return( list(string=my_string, bytes_left=bytes) )
}

###############################################################################
readSTDFbits <- function(bytes,endy) {
    
    bits_len = readBin(Stdf[Ptr:(Ptr+1)],integer(),n=1,size=2,endian=endy,signed=FALSE)
    Ptr <<- Ptr + 2
    field_len = as.integer( (bits_len+7)/8 )
    bytes = bytes - 2
    if (field_len > bytes) {
        cat("ERROR: bits length is longer than record length, skipping... \n")
		#browser()
        my_raw = NaN
        #bit_bucket = readBin(STDF,integer(),n=bytes,size=1)
        Ptr <<- Ptr + bytes
        bytes = 0
    } else {
        my_raw = readBin(Stdf[Ptr:(Ptr+field_len)],integer(),n=field_len,size=1,signed=FALSE)
        Ptr <<- Ptr + field_len
        bytes = bytes - field_len
    }

    return( list(raw=my_raw, bytes_left=bytes) )
}


###############################################################################
parse_stdf_record <- function(rec_typ,rec_sub,skip_TSRs,...) {

	# NOTE: also update valid_record_type() any time there is a change to the 
	# record types

    if (rec_typ == 0) {     # per file information
        if (rec_sub == 10) {
            if (Verbose2) cat("processing FAR record... \n")
            parse_FAR_record(...)
        } else if (rec_sub == 20) {
            #if (Verbose2) cat("processing ATR record... \n")
            # parse_ATR_record(...)
            if (Verbose2) cat("skipping ATR record... \n")
            ignore_STDF_record(...)
        } else if (rec_sub == 30) {
            #if (Verbose2) cat("processing VUR record... \n")
            # parse_VUR_record(...)
            if (Verbose2) cat("skipping VUR record... \n")
            ignore_STDF_record(...)
         } else {
            if (Verbose2) cat(sprintf(
                "skipping unknown record: type %d subtype %d... \n",
                rec_typ, rec_sub))
            unknown_record(rec_typ,rec_sub,...)
			ignore_STDF_record(...)
        }
    } else if (rec_typ == 1) {  # per lot information
        if (rec_sub == 10) {
            if (Verbose2) cat("processing MIR record... \n")
            parse_MIR_record(...)
        } else if (rec_sub == 20) {
            if (Verbose2) cat("processing MRR record... \n")
            parse_MRR_record(...)
        } else if (rec_sub == 30) {
            #parse_PCR_record(...)
            if (Verbose2) cat("skipping PCR record... \n")
            ignore_STDF_record(...)
        } else if (rec_sub == 40) {
            parse_HBR_record(...)
            #if (Verbose2) cat("skipping HBR record... \n")
            #ignore_STDF_record(...)
        } else if (rec_sub == 50) {
            parse_SBR_record(...)
            #if (Verbose2) cat("skipping SBR record... \n")
            #ignore_STDF_record(...)
        } else if (rec_sub == 60) {
            if (Verbose2) cat("processing PMR record... \n")
			if (Stdf_Version==3) {
				ignore_STDF_record(...)
			} else {
				parse_PMR_record(...)
			}
        } else if (rec_sub == 62) {
            #parse_PGR_record(...)
            if (Verbose2) cat("skipping PGR record... \n")
            ignore_STDF_record(...)
        } else if (rec_sub == 63) {
            #parse_PLR_record(...)
            if (Verbose2) cat("skipping PLR record... \n")
            ignore_STDF_record(...)
        } else if (rec_sub == 70) {
            #parse_RDR_record(...)
            if (Verbose2) cat("skipping RDR record... \n")
            ignore_STDF_record(...)
        } else if (rec_sub == 80) {
            parse_SDR_record(...)
        } else if (rec_sub == 90) {
            #parse_PSR_record(...)
            if (Verbose2) cat("skipping PSR record... \n")
            ignore_STDF_record(...)
        } else if (rec_sub == 92) {
            #parse_NMR_record(...)
            if (Verbose2) cat("skipping NMR record... \n")
            ignore_STDF_record(...)
         } else if (rec_sub == 93) {
            #parse_SSR_record(...)
            if (Verbose2) cat("skipping SSR record... \n")
            ignore_STDF_record(...)
         } else if (rec_sub == 94) {
            #parse_CDR_record(...)
            if (Verbose2) cat("skipping CDR record... \n")
            ignore_STDF_record(...)
        } else {
            if (Verbose2) cat(sprintf(
                "skipping unknown record: type %d subtype %d... \n", 
                rec_typ, rec_sub))
            unknown_record(rec_typ,rec_sub,...)
            ignore_STDF_record(...)
        }
    } else if (rec_typ == 2) {  # per wafer information
        if (rec_sub == 10) {
            if (Verbose2) cat("processing WIR record... \n")
            parse_WIR_record(...)
            #if (Verbose2) cat("skipping WIR record... \n")
            #ignore_STDF_record(...)
        } else if (rec_sub == 20) {
            if (Verbose2) cat("processing WRR record... \n")
            parse_WRR_record(...)
            #if (Verbose2) cat("skipping WRR record... \n")
            #ignore_STDF_record(...)
        } else if (rec_sub == 30) {
            if (Verbose2) cat("processing WCR record... \n")
            parse_WCR_record(...)
            #if (Verbose2) cat("skipping WCR record... \n")
            #ignore_STDF_record(...)
        } else {
            if (Verbose2) cat(sprintf(
                "skipping unknown record: type %d subtype %d... \n",
                rec_typ, rec_sub))
            unknown_record(rec_typ,rec_sub,...)
            ignore_STDF_record(...)
        }
    } else if (rec_typ == 5) {  # per part information
        if (rec_sub == 10) {
            if (Verbose0 || Verbose2) {
                cat(sprintf("processing PIR record %d ... \n",Device_count+1))
            } else {
                timestamp2 = proc.time()
                timestamp2 = timestamp2[3]
                if (timestamp2>(Timestamp1+5.0)) {
                    Timestamp1 <<- timestamp2
                    pct = 100.0 * (Stdf_ptr + Ptr) / Stdf_size
                    cat(sprintf("processing PIR record %d ... ",Device_count+1))
                    if(Stdf_size_true==1) {
						cat(sprintf(" %.1f%% through file \n",pct))
					} else {
						cat(sprintf(" estimate %.1f%% through file \n",pct))
					}
                }
            }
            parse_PIR_record(...)
        } else if (rec_sub == 20) {
            if (Verbose2) cat("processing PRR record... \n")
            parse_PRR_record(...)
        } else {
            if (Verbose2) cat(sprintf(
                "skipping unknown record: type %d subtype %d... \n",
                rec_typ, rec_sub))
            unknown_record(rec_typ,rec_sub,...)
            ignore_STDF_record(...)
        }
    } else if (rec_typ == 10) { # per test information
        if (rec_sub == 10) {
            if (Verbose2) cat("processing PDR record... \n")
            #parse_PDR_record(...)	# stdf v3 only!
            ignore_STDF_record(...)
        } else if (rec_sub == 20) {
            if (Verbose2) cat("processing FDR record... \n")
            parse_FDR_record(...)	# stdf v3 only!
        } else if (rec_sub == 30) {
            if (Verbose2) cat("processing TSR record... \n")
            if (skip_TSRs) {
				ignore_STDF_record(...)
            } else {
				parse_TSR_record(...)
			}
        } else {
            if (Verbose2) cat(sprintf(
                "skipping unknown record: type %d subtype %d... \n",
                rec_typ, rec_sub))
            unknown_record(rec_typ,rec_sub,...)
            ignore_STDF_record(...)
        }
    } else if (rec_typ == 15) { # per test execution information
		if ( (Max_parts>=0) && (Device_count>Max_parts) ) {
			ignore_STDF_record(...)
		} else {
			# if >5K new tests, it take take a looong time..
			# so progress updates here are nice if per PIR is >10seconds
			timestamp2 = proc.time()
			timestamp2 = timestamp2[3]
			if (timestamp2>(Timestamp1+10.0)) {
				Timestamp1 <<- timestamp2
				pct = 100.0 * (Stdf_ptr + Ptr) / Stdf_size
				cat(sprintf("processing PIR record %d ... ",Device_count+1))
				cat(sprintf("at byte %d ... ",(Stdf_ptr + Ptr)))
				if(Stdf_size_true==1) {
					cat(sprintf(" %.1f%% through file \n",pct))
				} else {
					cat(sprintf(" estimate %.1f%% through file \n",pct))
				}
			}
			if (rec_sub == 10) {
				if (Verbose2) cat("processing PTR record... \n")
				parse_PTR_record(...)
			} else if (rec_sub == 15) {
				if (Verbose2) cat("processing MPR record... \n")
				parse_MPR_record(...)  # REVISIT!!!
				#ignore_STDF_record(...)
			} else if (rec_sub == 20) {
				if (Verbose2) cat("processing FTR record... \n")
				parse_FTR_record(...)
			} else if (rec_sub == 30) {
				if (Verbose2) cat("processing STR record... \n")
				#parse_STR_record(...)
				ignore_STDF_record(...)
			} else {
				if (Verbose2) cat(sprintf(
					"skipping unknown record: type %d subtype %d... \n",
					rec_typ, rec_sub))
            	unknown_record(rec_typ,rec_sub,...)
				ignore_STDF_record(...)
			}
		}
    } else if (rec_typ == 20) { # per program segment information
        ignore_STDF_record(...)
    } else if (rec_typ == 25) { # per site information (stdf v3 only)
        if (rec_sub == 10) {
            if (Verbose2) cat("processing SHB record... \n")
            #parse_SHB_record(...)
			ignore_STDF_record(...)
        } else if (rec_sub == 20) {
            if (Verbose2) cat("processing SSB record... \n")
            #parse_SSB_record(...)  
            ignore_STDF_record(...)
        } else if (rec_sub == 30) {
            if (Verbose2) cat("processing STS record... \n")
            #parse_STS_record(...)  
            ignore_STDF_record(...)
        } else if (rec_sub == 40) {
            if (Verbose2) cat("processing SCR record... \n")
            #parse_SCR_record(...)  
            ignore_STDF_record(...)
        } else {
            if (Verbose2) cat(sprintf(
                "skipping unknown record: type %d subtype %d... \n",
                rec_typ, rec_sub))
            unknown_record(rec_typ,rec_sub,...)
            ignore_STDF_record(...)
        }
    } else if (rec_typ == 50) { # generic data
        if (rec_sub == 30) {
            if(Do_DTRs) {
				if (Verbose2) cat("processing DTR record... \n")
            	parse_DTR_record(...)	# REVISIT...
			} else {
				ignore_STDF_record(...)
			}
        } else {
        	 ignore_STDF_record(...)
		}
    } else if (rec_typ == 220) { # stdf+
        if (rec_sub == 201) {
            if (Verbose2) cat("processing BRR record... \n")
            #parse_BRR_record(...)
			ignore_STDF_record(...)
        } else if (rec_sub == 202) {
            if (Verbose2) cat("processing WTR record... \n")
            #parse_WTR_record(...)  
            ignore_STDF_record(...)
        } else if (rec_sub == 203) {
            if (Verbose2) cat("processing ETSR record... \n")
            #parse_ETSR_record(...)  
            ignore_STDF_record(...)
        } else if (rec_sub == 204) {
            if (Verbose2) cat("processing GTR record... \n")
            #parse_GTR_record(...)  
            ignore_STDF_record(...)
        } else if (rec_sub == 205) {
            if (Verbose2) cat("processing ADR record... \n")
            #parse_ADR_record(...)  
            ignore_STDF_record(...)
        } else if (rec_sub == 206) {
            if (Verbose2) cat("processing EPDR record... \n")
            #parse_EPDR_record(...)  
            ignore_STDF_record(...)
		}
    } else {                    # unknown record type
        unknown_record(rec_typ,rec_sub,...)
        ignore_STDF_record(...)
    }

}


#############################################################################

parse_FAR_record <- function(rec_len,endy) {

    cpu_type = 0
    stdf_ver = 0

    # make sure we have the correct endian...
    #----------------------------------------
    if (rec_len == 512) {
        if (endy == "big")  Endy <<- "little"  else  Endy <<- "big"
        rec_len = 2
    }


    # parse the record
    #------------------
    if (rec_len > 0) {
        cpu_type = readBin(Stdf[Ptr],integer(),n=1,size=1,signed=FALSE)
        Ptr <<- Ptr + 1
    }
    if (rec_len > 1) {
        stdf_ver = readBin(Stdf[Ptr],integer(),n=1,size=1,signed=FALSE)
        Ptr <<- Ptr + 1
    }


    # process the record information...
    #------------------------------------
    if ( (stdf_ver != 4) && (stdf_ver != 3) ) {
		cat(sprintf("WARNING: Expecting STDF version 3 or 4 file, not version %d \n", stdf_ver))
	} else {
		Stdf_Version <<- stdf_ver
	}

    # throw away any remaining portion of the record we aren't interested in...
    #--------------------------------------------------------------------------
    if (rec_len > 2) {
        cat(sprintf("WARNING: FAR record longer than expected: %d bytes \n",
            rec_len))
        #bit_bucket = readBin(STDF,integer(),n=rec_len-2,size=1)
        Ptr <<- Ptr + rec_len - 2
        rec_len = 0
    } 
}


#############################################################################
unknown_record <- function(rec_typ, rec_sub,...) {
	# just a place to set traps, also to count up # of unknown records
	#browser()
	cat(sprintf("Unknown record type: %d  subtype: %d \n",rec_typ,rec_sub))
	Unknown_rec_count <<- Unknown_rec_count + 1
}


#############################################################################
ignore_STDF_record <- function(rec_len,endy) {


    # throw away any remaining portion of the record we aren't interested in...
    #--------------------------------------------------------------------------
    #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
    Ptr <<- Ptr + rec_len
    rec_len = 0
}


#############################################################################
parse_MIR_record <- function(rec_len,endy) {

    # initialize variables
    setup_t = 0
    start_t = 0
    stat_num = 0
    mode_cod = ' '
    rtst_cod = ' '
    prot_cod = ' '
    burn_tim = 0
    cmod_cod = ' '
    lot_id = ''
    part_typ = ''
    node_nam = ''
    tstr_typ = ''
    job_nam = ''
    job_rev = ''
    sblot_id = ''
    oper_nam = ''
    exec_typ = ''
    exec_ver = ''
    test_cod = ''
    tst_temp = ''
    user_txt = ''
    aux_file = ''
    pkg_typ = ''
    famly_id = ''
    date_cod = ''
    facil_id = ''
    floor_id = ''
    proc_id = ''
    oper_frq = ''
    spec_nam = ''
    spec_ver = ''
    flow_id = ''
    setup_id = ''
    dsgn_rev = ''
    eng_id = ''
    rom_cod = ''
    serl_num = ''
    supr_nam = ''

	# stdf v3 specific fields
	hand_id = ''
	prb_card = ''

	#cat("DEBUG: parsing MIR record\n")

    valid_record = TRUE     

    if ( (rec_len<15) || ((rec_len<18)&&(Stdf_Version==3)) ) {  
        cat("WARNING: MIR record shorter than expected \n")
        valid_record = FALSE
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } else if (Stdf_Version==3) {
		cpu_type = readBin(Stdf[(Ptr)],integer(),n=1,size=1,signed=FALSE)
		stdf_ver = readBin(Stdf[(Ptr+1)],integer(),n=1,size=1,signed=FALSE)
        mode_cod = rawToChar(Stdf[(Ptr+2)])
		stat_num = readBin(Stdf[(Ptr+3)],integer(),n=1,size=1,signed=FALSE)
		test_cod = rawToChar(Stdf[(Ptr+4):(Ptr+6)])
        rtst_cod = rawToChar(Stdf[(Ptr+7)])
        prot_cod = rawToChar(Stdf[Ptr+8])
        cmod_cod = rawToChar(Stdf[Ptr+9])
        #setup_t = readBin(Stdf[(Ptr+10):(Ptr+13)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        setup_t = readBin(Stdf[(Ptr+10):(Ptr+13)],integer(),n=1,size=4,endian=endy)
		if(setup_t<0)  setup_t = setup_t + 2^32
        #start_t = readBin(Stdf[(Ptr+14):(Ptr+17)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        start_t = readBin(Stdf[(Ptr+14):(Ptr+17)],integer(),n=1,size=4,endian=endy)
		if(start_t<0)  start_t = start_t + 2^32
        Ptr <<- Ptr + 18
        rec_len = rec_len - 18
	} else {
        #setup_t = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        setup_t = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		if(setup_t<0)  setup_t = setup_t + 2^32
        #start_t = readBin(Stdf[(Ptr+4):(Ptr+7)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        start_t = readBin(Stdf[(Ptr+4):(Ptr+7)],integer(),n=1,size=4,endian=endy)
		if(start_t<0)  start_t = start_t + 2^32
        stat_num = readBin(Stdf[(Ptr+8)],integer(),n=1,size=1,signed=FALSE)
        Ptr <<- Ptr + 9
        mode_cod = rawToChar(Stdf[Ptr])
        rtst_cod = rawToChar(Stdf[Ptr+1])
        prot_cod = rawToChar(Stdf[Ptr+2])
        Ptr <<- Ptr + 3
        burn_tim = readBin(Stdf[Ptr:(Ptr+1)],integer(),n=1,size=2,endian=endy,signed=FALSE)
        Ptr <<- Ptr + 2
        cmod_cod = rawToChar(Stdf[Ptr])
        Ptr <<- Ptr + 1
        rec_len = rec_len - 15
    }

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        lot_id = str_list$string
        rec_len = str_list$bytes_left

        if (Debug1) {
            cat(sprintf("LOTID: %s ...\n",lot_id))
        }
    }

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        part_typ = str_list$string
        rec_len = str_list$bytes_left
    }

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        if (Stdf_Version==3) {
			job_nam = str_list$string
		} else {
			node_nam = str_list$string
        }
		rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        if (Stdf_Version==3) {
			oper_nam = str_list$string
		} else {
	        tstr_typ = str_list$string
	        cat(sprintf("Tester Type is: %s \n",tstr_typ))
        }
		rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        if (Stdf_Version==3) {
			node_nam = str_list$string
		} else {
			job_nam = str_list$string
        }
		rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        if (Stdf_Version==3) {
			tstr_typ = str_list$string
	        cat(sprintf("Tester Type is: %s \n",tstr_typ))
		} else {
			job_rev = str_list$string
        }
		rec_len = str_list$bytes_left
    } 

	# if stdf3 or stdf4, tstr_typ has now been processed, can update global variable...
	MIR_Tester_type <<- tstr_typ


    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        if (Stdf_Version==3) {
			exec_typ = str_list$string
		} else {
			sblot_id = str_list$string
        }
        rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        if (Stdf_Version==3) {
			supr_nam = str_list$string
		} else {
			oper_nam = str_list$string
        }
		rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        if (Stdf_Version==3) {
			hand_id = str_list$string
		} else {
			exec_typ = str_list$string
        }
		rec_len = str_list$bytes_left
    } 
    Executive_type <<- exec_typ
    if (substr(tstr_typ,1,1)=='P') {   
	    cat(sprintf("...treat as Tester Type 93000 \n"))
        Executive_type <<- '93000'	   
    }
    if (substr(Executive_type,1,6)=='IMAGE ') {   
        Executive_type <<- 'Image'
    }
    if (substr(tstr_typ,1,3)=='ETS') {   
        Executive_type <<- 'Eagle'
    }
    if (substr(tstr_typ,1,5)=='83000') {   
        Executive_type <<- '93000'     # haven't verified any 83k files... 
    }
    if (substr(tstr_typ,1,5)=='93000') {   
        Executive_type <<- '93000'	   # haven't verified any linux files...
    }
    if (tstr_typ=='Credence') {
        Executive_type <<- 'Credence'
    }
    if (tstr_typ=='IntegraFlex') {
        Executive_type <<- 'IntegraFlex'
    }
    if (tstr_typ=='CMT') {
        Executive_type <<- 'T2000'
    }

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        if (Stdf_Version==3) {
			sblot_id = str_list$string
		} else {
			exec_ver = str_list$string
    		Executive_version <<- exec_ver	# "s/w rev. 6.5.4.13 (E), 16-Aug-12"
			#browser()
        }
		rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        if (Stdf_Version==3) {
			job_rev = str_list$string
		} else {
			test_cod = str_list$string
        }
		rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        if (Stdf_Version==3) {
			proc_id = str_list$string
		} else {
			tst_temp = str_list$string
        }
		rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        if (Stdf_Version==3) {
			prb_card = str_list$string
		} else {
			user_txt = str_list$string
        }
		rec_len = str_list$bytes_left
    } 

    if ( (rec_len > 0) && (Stdf_Version==3) ) {
        cat(sprintf("WARNING: MIR record longer than expected \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } 

	if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        aux_file = str_list$string
        rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        pkg_typ = str_list$string
        rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        famly_id = str_list$string
        rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        date_cod = str_list$string
        rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        facil_id = str_list$string
        rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        floor_id = str_list$string
        rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        proc_id = str_list$string
        rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        oper_frq = str_list$string
        rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        spec_nam = str_list$string
        rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        spec_ver = str_list$string
        rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        flow_id = str_list$string
        rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        setup_id = str_list$string
        rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        dsgn_rev = str_list$string
        rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        eng_id = str_list$string
        rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        rom_cod = str_list$string
        rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        serl_num = str_list$string
        rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        supr_nam = str_list$string
        rec_len = str_list$bytes_left
    } 


    # throw away any remaining portion of the record we aren't interested in...
    #--------------------------------------------------------------------------
    if (rec_len > 0) {
        cat(sprintf("WARNING: MIR record longer than expected \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } 


    # now update LotInfoFrame
    #---------------------------------
    if (valid_record) {
		# sometimes 93K files have \r at end of string, remove these
		lot_id = sub("\r","",lot_id)
		sblot_id = sub("\r","",sblot_id)
        my_list = list( lotid=lot_id, sublotid=sblot_id,
                    start_t=start_t, program=job_nam,
                    tester_type=tstr_typ, tester_id=node_nam,
                    handler=hand_id, finish_t="",
					mode_cod=mode_cod,rtst_cod=rtst_cod,
					part_typ=part_typ,job_rev=job_rev,
					oper_nam=oper_nam,
					exec_typ=exec_typ,exec_ver=exec_ver,
					test_cod=test_cod)
        LotInfoFrame <<- data.frame(rbind(my_list))
    }
}

	
#############################################################################
parse_SDR_record <- function(rec_len,endy) {

    # initialize variables
    head_num = 0
	site_grp = 0
	site_cnt = 0
	site_nums = 0
	hand_typ = ''
	hand_id = ''
	card_typ = ''
	card_id = ''
	load_typ = ''
	load_id = ''
	dib_typ = ''
	dib_id = ''
	cabl_typ = ''
	cabl_id = ''
	cont_typ = ''
	cont_id = ''
	lasr_typ = ''
	lasr_id = ''
	extr_typ = ''
	extr_id = ''

    valid_record = TRUE


    if (rec_len<20) {
        valid_record = FALSE
        cat("WARNING: SDR record shorter than expected \n")
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
	} else {
        head_num = as.integer(Stdf[Ptr])
        site_grp = as.integer(Stdf[Ptr+1])
        site_cnt = as.integer(Stdf[Ptr+2])
        Ptr <<- Ptr + 3
        rec_len = rec_len - 3
	}

	if (rec_len >= site_cnt) {
		if (site_cnt > 0) {
			site_nums = as.integer(Stdf[Ptr:(Ptr+site_cnt-1)])
			Ptr <<- Ptr + site_cnt
			rec_len = rec_len - site_cnt
		}
	} else {
        #cat(sprintf("WARNING: SDR record incomplete \n"))
        Ptr <<- Ptr + rec_len
        rec_len = 0
	}

	if (rec_len >0) {
		str_list = readSTDFstring(rec_len)
		hand_typ = str_list$string
		rec_len = str_list$bytes_left
	}

	if (rec_len >0) {
		str_list = readSTDFstring(rec_len)
		hand_id = str_list$string
		rec_len = str_list$bytes_left
	}

	if (rec_len >0) {
		str_list = readSTDFstring(rec_len)
		card_typ = str_list$string
		rec_len = str_list$bytes_left
	}

	if (rec_len >0) {
		str_list = readSTDFstring(rec_len)
		card_id = str_list$string
		rec_len = str_list$bytes_left
	}

	if (rec_len >0) {
		str_list = readSTDFstring(rec_len)
		load_typ = str_list$string
		rec_len = str_list$bytes_left
	}

	if (rec_len >0) {
		str_list = readSTDFstring(rec_len)
		load_id = str_list$string
		rec_len = str_list$bytes_left
	}

	if (rec_len >0) {
		str_list = readSTDFstring(rec_len)
		dib_typ = str_list$string
		rec_len = str_list$bytes_left
	}

	if (rec_len >0) {
		str_list = readSTDFstring(rec_len)
		dib_id = str_list$string
		rec_len = str_list$bytes_left
	}

	if (rec_len >0) {
		str_list = readSTDFstring(rec_len)
		cabl_typ = str_list$string
		rec_len = str_list$bytes_left
	}

	if (rec_len >0) {
		str_list = readSTDFstring(rec_len)
		cabl_id = str_list$string
		rec_len = str_list$bytes_left
	}

	if (rec_len >0) {
		str_list = readSTDFstring(rec_len)
		cont_typ = str_list$string
		rec_len = str_list$bytes_left
	}

	if (rec_len >0) {
		str_list = readSTDFstring(rec_len)
		cont_id = str_list$string
		rec_len = str_list$bytes_left
	}

	if (rec_len >0) {
		str_list = readSTDFstring(rec_len)
		lasr_typ = str_list$string
		rec_len = str_list$bytes_left
	}

	if (rec_len >0) {
		str_list = readSTDFstring(rec_len)
		lasr_id = str_list$string
		rec_len = str_list$bytes_left
	}

	if (rec_len >0) {
		str_list = readSTDFstring(rec_len)
		extr_typ = str_list$string
		rec_len = str_list$bytes_left
	}

	if (rec_len >0) {
		str_list = readSTDFstring(rec_len)
		extr_id = str_list$string
		rec_len = str_list$bytes_left
	}


    # throw away any remaining portion of the record we aren't interested in...
    #--------------------------------------------------------------------------
    #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
    Ptr <<- Ptr + rec_len
    rec_len = 0


	if (valid_record) {

		LotInfoFrame[1,"handler"] <<- hand_id
	}
}


#############################################################################
parse_MRR_record <- function(rec_len,endy) {

    # initialize variables
    finish_t = 0
    disp_cod = ' '
    usr_desc = ''
    exc_desc = ''

	# stdf v3 fields
    part_cnt = NaN
    rtst_cnt = NaN
    abrt_cnt = NaN
    good_cnt = NaN
    func_cnt = NaN

    valid_record = TRUE

	#cat("DEBUG: parsing MRR record\n")

    if (rec_len<4) {
        valid_record = FALSE
        cat("WARNING: MRR record shorter than expected \n")
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } else {
        #finish_t = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        finish_t = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		if(finish_t<0)  finish_t = finish_t + 2^32
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    }

	if (Stdf_Version==3) {
	    if (rec_len>=4) {
	        #part_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
	        part_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
			if(part_cnt<0)  part_cnt = part_cnt + 2^32
	        Ptr <<- Ptr + 4
	        rec_len = rec_len - 4
	    } else {
	        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
	        Ptr <<- Ptr + rec_len
	        rec_len = 0
	    }
	
	    if (rec_len>=4) {
	        #rtst_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
	        rtst_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
			if(rtst_cnt<0)  rtst_cnt = rtst_cnt + 2^32
	        Ptr <<- Ptr + 4
	        rec_len = rec_len - 4
	    } else {
	        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
	        Ptr <<- Ptr + rec_len
	        rec_len = 0
	    }
	
	    if (rec_len>=4) {
	        #abrt_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
	        abrt_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
			if(abrt_cnt<0)  abrt_cnt = abrt_cnt + 2^32
	        Ptr <<- Ptr + 4
	        rec_len = rec_len - 4
	    } else {
	        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
	        Ptr <<- Ptr + rec_len
	        rec_len = 0
	    }
	
	    if (rec_len>=4) {
	        #good_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
	        good_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
			if(good_cnt<0)  good_cnt = good_cnt + 2^32
	        Ptr <<- Ptr + 4
	        rec_len = rec_len - 4
	    } else {
	        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
	        Ptr <<- Ptr + rec_len
	        rec_len = 0
	    }
	
	    if (rec_len>=4) {
	        #func_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
	        func_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
			if(func_cnt<0)  func_cnt = func_cnt + 2^32
	        Ptr <<- Ptr + 4
	        rec_len = rec_len - 4
	    } else {
	        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
	        Ptr <<- Ptr + rec_len
	        rec_len = 0
	    }
	}

    if (rec_len >0) {
        disp_cod = rawToChar(Stdf[Ptr])
        Ptr <<- Ptr + 1
        rec_len = rec_len - 1
    }

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        usr_desc = str_list$string
        rec_len = str_list$bytes_left
    }
    
    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        exc_desc = str_list$string
        rec_len = str_list$bytes_left
    }
    
    if (rec_len >0) {
        cat(sprintf("WARNING: MRR record longer than expected \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } 

    if (valid_record) {
        LotInfoFrame[1,"finish_t"] <<- finish_t
    }

}


#############################################################################
parse_PMR_record <- function(rec_len,endy) {

    # initialize variables
    pmr_indx = NaN
    chan_typ = NaN
    chan_nam = ''
    phy_nam = ''
    log_nam = ''
    head_num = 0
    site_num = 0

    valid_record = TRUE

    if (rec_len<4) {
        valid_record = FALSE
        cat("WARNING: PMR record shorter than expected \n")
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } else {
        pmr_indx = readBin(Stdf[Ptr:(Ptr+1)],integer(),n=1,size=2,endian=endy,signed=FALSE)
        chan_typ = readBin(Stdf[(Ptr+2):(Ptr+3)],integer(),n=1,size=2,endian=endy,signed=FALSE)
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    }

    if (rec_len>0) {
        str_list = readSTDFstring(rec_len)
        chan_nam = str_list$string
        rec_len = str_list$bytes_left   
    }

    if (rec_len>0) {
        str_list = readSTDFstring(rec_len)
        phy_nam = str_list$string
        rec_len = str_list$bytes_left   
    }

    if (rec_len>0) {
        str_list = readSTDFstring(rec_len)
        log_nam = str_list$string
        rec_len = str_list$bytes_left   
    }

    if (rec_len>0) {
        #head_num = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        head_num = as.integer(Stdf[Ptr])
        Ptr <<- Ptr + 1
        rec_len = rec_len - 1
    }

    if (rec_len>0) {
        #site_num = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        site_num = as.integer(Stdf[Ptr])
        Ptr <<- Ptr + 1
        rec_len = rec_len - 1
    }

    if (Debug3) {
        cat(sprintf("PMR: pmr_indx=%d log_nam=%s chan_nam=%s \n",
                pmr_indx,log_nam,chan_nam))
    }


    # throw away any remaining portion of the record...
    #--------------------------------------------------------------------------
    if (rec_len > 0) {
        cat(sprintf("WARNING: PMR record longer than expected \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } 

    
    # now update pinname_xref for MPR records
    #----------------------------------------------------
    if (valid_record) {

        #  chan_nam   phy_nam   log_nam
        #   pin name  resource   x        : 93k  (HP/Agilent/Verigy)
        #   x         x          pin name : quartet (Credence)
        #   resource  pin_name   x        : HFi (LTX Fusion)
        if (Executive_type=="93000") {
            Pin_names[pmr_indx] <<- chan_nam
        } else if (Executive_type=="enVision") {
            Pin_names[pmr_indx] <<- phy_nam
        } else {
            Pin_names[pmr_indx] <<- log_nam
        }
    }
}



#############################################################################
parse_WCR_record <- function(rec_len,endy) {

    # initialize variables
    wafr_siz = NaN
    die_ht = NaN
    die_wid = NaN
    wf_units = NaN
    wf_flat = ""
    center_x = NaN
    center_y = NaN
    pos_x = ""
    pos_y = ""

    valid_record = TRUE

	#browser()

    if (rec_len==0) {
        valid_record = FALSE
        cat("WARNING: WCR record with length 0 \n")
	} else if (rec_len<4) {
        valid_record = FALSE
        cat("WARNING: WCR record shorter than expected \n")
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } else {
        wafr_siz = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    }

    if (rec_len>3) {
        die_ht = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    } else {
        #cat(sprintf("WARNING: WCR record incomplete \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    }

    if (rec_len>3) {
        die_wid = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    } else {
        #cat(sprintf("WARNING: WCR record incomplete \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    }

    if (rec_len>0) {
        #wf_units = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        wf_units = as.integer(Stdf[Ptr])
        Ptr <<- Ptr + 1
        rec_len = rec_len - 1
    }

    if (rec_len>0) {
        wf_flat = rawToChar(Stdf[Ptr])
        Ptr <<- Ptr + 1
        rec_len = rec_len - 1
    }

    if (rec_len>1) {
        center_x = readBin(Stdf[Ptr:(Ptr+1)],integer(),n=1,size=2,endian=endy)
        Ptr <<- Ptr + 2
        rec_len = rec_len - 2
    } else {
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    }

    if (rec_len>1) {
        center_y = readBin(Stdf[Ptr:(Ptr+1)],integer(),n=1,size=2,endian=endy)
        Ptr <<- Ptr + 2
        rec_len = rec_len - 2
    } else {
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    }

    if (rec_len>0) {
        pos_x = rawToChar(Stdf[Ptr])
        Ptr <<- Ptr + 1
        rec_len = rec_len - 1
    }

    if (rec_len>0) {
        pos_y = rawToChar(Stdf[Ptr])
        Ptr <<- Ptr + 1
        rec_len = rec_len - 1
    }


    #--------------------------------------------------------

    if (rec_len>0) {
        cat(sprintf("WARNING: WCR record longer than expected \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } 

    if (valid_record) {
        my_list = list( wafr_siz=wafr_siz, die_ht=die_ht,
                        die_wid=die_wid, wf_units=wf_units,
                        wf_flat=wf_flat, center_x=center_x,
                        center_y=center_y, pos_x=pos_x,
                        pos_y=pos_y)
        WaferInfoFrame <<- data.frame(rbind(my_list))
        Valid_WCR <<- TRUE
    }

}


#############################################################################
parse_WIR_record <- function(rec_len,endy) {

    # initialize variables
    head_num = NaN
    site_grp = NaN
    start_t = NaN
    wafer_id = ""

    valid_record = TRUE

    if (rec_len<6) {
        valid_record = FALSE
        cat("WARNING: WIR record shorter than expected \n")
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } else {
        head_num = as.integer(Stdf[Ptr])
        site_grp = as.integer(Stdf[Ptr+1])	# really pad_byte if stdf v3
        Ptr <<- Ptr + 2
        #start_t = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        start_t = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		if(start_t<0)  start_t = start_t + 2^32
        Ptr <<- Ptr + 4
        rec_len = rec_len - 6
    }

    if (rec_len>0) {
        str_list = readSTDFstring(rec_len)
        wafer_id = str_list$string
        rec_len = str_list$bytes_left   
    }

    #--------------------------------------------------------

    if (rec_len>0) {
        cat(sprintf("WARNING: WIR record longer than expected \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } 

    if (valid_record) {
		# NOTE: these wafer_id fixes NEED to be done in WRR section too!
		# sometimes 93K files have \r at end of string, remove these
		wafer_id = sub("\r","",wafer_id)
		# sometimes IntegraFlex files have "\n      " out to 254 char
		# at end of string, remove these
		# browser()
		wafer_id = sub("[[:space:]]+$","",wafer_id)
        Wafer_count <<- Wafer_count + 1
        my_list = list(wafer_id=wafer_id, start_t=start_t,
                finish_t=NaN,part_cnt=NaN,
                good_cnt=NaN)
        WafersFrame[Wafer_count,] <<- my_list
    }
}


#############################################################################
parse_WRR_record <- function(rec_len,endy) {

    # initialize variables
    head_num = NaN
    site_grp = NaN
    finish_t = NaN
    part_cnt = NaN
    rtst_cnt = NaN
    abrt_cnt = NaN
    good_cnt = NaN
    func_cnt = NaN
    wafer_id = ""
    fabwf_id = ""
    frame_id = ""
    mask_id = ""
    usr_desc = ""
    exc_desc = ""

	# stdf v3 fields
	hand_id = ""
	prb_card = ""


    valid_record = TRUE

    if (rec_len<6) {
        valid_record = FALSE
        cat("WARNING: WRR record shorter than expected \n")
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } else if (Stdf_Version==3) {
        #finish_t = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        finish_t = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		if(finish_t<0)  finish_t = finish_t + 2^32
        head_num = as.integer(Stdf[Ptr+4])
        site_grp = as.integer(Stdf[Ptr+5])	# really pad_byte
        Ptr <<- Ptr + 6
        rec_len = rec_len - 6
    } else {
        #head_num = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        head_num = as.integer(Stdf[Ptr])
        #site_grp = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        site_grp = as.integer(Stdf[Ptr+1])
        Ptr <<- Ptr + 2
        #finish_t = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        finish_t = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		if(finish_t<0)  finish_t = finish_t + 2^32
        Ptr <<- Ptr + 4
        rec_len = rec_len - 6
    }

    if (rec_len>=4) {
        #part_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        part_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		if(part_cnt<0)  part_cnt = part_cnt + 2^32
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    } else {
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    }

    if (rec_len>=4) {
        #rtst_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        rtst_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		if(rtst_cnt<0)  rtst_cnt = rtst_cnt + 2^32
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    } else {
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    }

    if (rec_len>=4) {
        #abrt_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        abrt_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		if(abrt_cnt<0)  abrt_cnt = abrt_cnt + 2^32
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    } else {
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    }

    if (rec_len>=4) {
        #good_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        good_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		if(good_cnt<0)  good_cnt = good_cnt + 2^32
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    } else {
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    }

    if (rec_len>=4) {
        #func_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        func_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		if(func_cnt<0)  func_cnt = func_cnt + 2^32
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    } else {
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    }

    if (rec_len>0) {
        str_list = readSTDFstring(rec_len)
        wafer_id = str_list$string
        rec_len = str_list$bytes_left   
    }

    if (rec_len>0) {
        str_list = readSTDFstring(rec_len)
		if (Stdf_Version==3) {
			hand_id = str_list$string
		} else {
			fabwf_id = str_list$string
        }
		rec_len = str_list$bytes_left   
    }

    if (rec_len>0) {
        str_list = readSTDFstring(rec_len)
		if (Stdf_Version==3) {
			prb_card = str_list$string
		} else {
			frame_id = str_list$string
        }
		rec_len = str_list$bytes_left   
    }

    if ((rec_len>0)&&(Stdf_Version!=3)) {
        str_list = readSTDFstring(rec_len)
        mask_id = str_list$string
        rec_len = str_list$bytes_left   
    }

    if (rec_len>0) {
        str_list = readSTDFstring(rec_len)
        usr_desc = str_list$string
        rec_len = str_list$bytes_left   
    }

    if (rec_len>0) {
        str_list = readSTDFstring(rec_len)
        exc_desc = str_list$string
        rec_len = str_list$bytes_left   
    }

    #--------------------------------------------------------

    if (rec_len>0) {
        cat(sprintf("WARNING: WRR record longer than expected \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } 

    if (valid_record) {
		# sometimes 93K files have \r at end of string, remove these
		wafer_id = sub("\r","",wafer_id)
		# sometimes IntegraFlex files have "\n      " out to 254 char
		# at end of string, remove these
		# browser()
		wafer_id = sub("[[:space:]]+$","",wafer_id)
        WafersFrame[Wafer_count,"wafer_id"]<<- wafer_id
        WafersFrame[Wafer_count,"finish_t"]<<- finish_t
        WafersFrame[Wafer_count,"part_cnt"]<<- part_cnt
        WafersFrame[Wafer_count,"good_cnt"]<<- good_cnt
    }

}



#############################################################################
parse_PIR_record <- function(rec_len,endy) {

    # initialize variables
    head_num = 0
    site_num = 0

	# stdf v3 fields
	x_coord = NaN
	y_coord = NaN
	part_id = " "


    valid_record = TRUE

    if (rec_len<2) {
        valid_record = FALSE
        cat("WARNING: PIR record shorter than expected \n")
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } else {
        #head_num = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        head_num = as.integer(Stdf[Ptr])
        #site_num = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        site_num = as.integer(Stdf[Ptr+1])
        Ptr <<- Ptr + 2
        rec_len = rec_len - 2
    }

	if (Stdf_Version==3) {
	    if (rec_len>1) {
			x_coord = readBin(Stdf[Ptr:(Ptr+1)],integer(),n=1,size=2,endian=endy,signed=TRUE)
			Ptr <<- Ptr + 2
			rec_len = rec_len - 2
        }
		
	    if (rec_len>1) {
			y_coord = readBin(Stdf[Ptr:(Ptr+1)],integer(),n=1,size=2,endian=endy,signed=TRUE)
			Ptr <<- Ptr + 2
			rec_len = rec_len - 2
        }

		if (rec_len>0) {
			str_list = readSTDFstring(rec_len)
			part_id = str_list$string
			rec_len = str_list$bytes_left   
		}
	}

    if (rec_len>0) {
        cat(sprintf("WARNING: PIR record longer than expected \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } 

    if (valid_record) {
        # starting another device... 
        # ... add element to DevicesFrame vectors
        # ... add row to ResultsMatrix
        # ... add row to ConditionsMatrix
        Device_count <<- Device_count + 1
		PIR_count <<- PIR_count + 1
		Test_order_counter <<- 0
		# some systems start at site0, R arrays start at 1 so add 1 to site_num
		Open_site[site_num+1] <<- Device_count	
        my_list = list(part_id=part_id, temp=NaN,
                        x_coord=x_coord, y_coord=y_coord,
                        wafer_index=NaN,
                        softbin=NaN, hardbin=NaN,
                        testtime=NaN, site=NaN)

        if ( sum(dim(ResultsMatrix))==0) {
			if (Stdf_Version==3) {
				Devices_part_id <<- part_id      # <<- as.character(DevicesFrame$part_id)
				Devices_x_coord <<- x_coord      # <<- as.integer(
				Devices_y_coord <<- y_coord      # <<- as.integer(
			} else {
				Devices_part_id <<- NA      # <<- as.character(DevicesFrame$part_id)
				Devices_x_coord <<- NA      # <<- as.integer(
				Devices_y_coord <<- NA      # <<- as.integer(
			}
            Devices_temp <<- NA         # <<- as.numeric(
            Devices_wafer_index <<- NA  # <<- as.integer(
            Devices_soft_bin <<- NA     # <<- as.integer(
            Devices_hard_bin <<- NA     # <<- as.integer(
            Devices_testtime <<- NA     # <<- as.numeric(
            Devices_site <<- site_num	# <<- as.numeric(
            Devices_PIR_count <<- PIR_count

            #DevicesFrame[Device_count,] <<- my_list
            ResultsMatrix<<- array(NaN, dim=c(1,0))
            if(Do_testflag_matrix)  TestFlagMatrix<<- array(NaN, dim=c(1,0))
            if(Do_test_order_matrix)  TestOrderMatrix<<- array(NaN, dim=c(1,0))
            if(Do_mult_limits>0)  MultLimIndexMatrix<<- array(NaN, dim=c(1,0))
        } else {
            # wait 5 devices before allocating in chunks to allow 
            # parameters frame length to stabilize a bit.
            if (Device_count>=5) {
                # allocate memory in bigger chunks less often 
                # to improve speed...
                chunk = 200
                if (Device_count==5) {
                    #DevicesFrame[5:chunk,] <<- my_list
                    Devices_part_id[5:chunk] <<- NA     # <<- as.character(DevicesFrame$part_id)
                    Devices_temp[5:chunk] <<- NA        # <<- as.numeric(
                    Devices_x_coord[5:chunk] <<- NA     # <<- as.integer(
                    Devices_y_coord[5:chunk] <<- NA     # <<- as.integer(
                    Devices_wafer_index[5:chunk] <<- NA # <<- as.integer(
                    Devices_soft_bin[5:chunk] <<- NA    # <<- as.integer(
                    Devices_hard_bin[5:chunk] <<- NA    # <<- as.integer(
                    Devices_testtime[5:chunk] <<-NA     # <<- as.numeric(
                    Devices_site[5:chunk] <<-NA			# <<- as.numeric(
                    Devices_PIR_count[5:chunk] <<-NA	# <<- as.numeric(

                    my_dims = dim(ResultsMatrix)
                    ResultsMatrix<<- rbind(ResultsMatrix,matrix(NaN,nrow=chunk-4,
                                        ncol=my_dims[2]))
                    if(Do_testflag_matrix)  TestFlagMatrix<<- rbind(TestFlagMatrix,matrix(NaN,nrow=chunk-4,
                                        ncol=my_dims[2]))
                    if(Do_test_order_matrix)  TestOrderMatrix<<- rbind(TestOrderMatrix,matrix(NaN,nrow=chunk-4,
                                        ncol=my_dims[2]))
                    if(Do_mult_limits>0)  MultLimIndexMatrix<<- rbind(MultLimIndexMatrix,matrix(NaN,nrow=chunk-4,
                                        ncol=my_dims[2]))
                } else {
                    mod_d = Device_count %% chunk  # modulus
                    if(mod_d==0) {
                        #DevicesFrame[(Device_count+1):(Device_count+chunk),] <<- my_list
                        Devices_part_id[(Device_count+1):(Device_count+chunk)] <<- NA   
                        Devices_temp[(Device_count+1):(Device_count+chunk)] <<- NA      
                        Devices_x_coord[(Device_count+1):(Device_count+chunk)] <<- NA   
                        Devices_y_coord[(Device_count+1):(Device_count+chunk)] <<- NA   
                        Devices_wafer_index[(Device_count+1):(Device_count+chunk)] <<- NA 
                        Devices_soft_bin[(Device_count+1):(Device_count+chunk)] <<- NA  
                        Devices_hard_bin[(Device_count+1):(Device_count+chunk)] <<- NA  
                        Devices_testtime[(Device_count+1):(Device_count+chunk)] <<-NA   
                        Devices_site[(Device_count+1):(Device_count+chunk)] <<-NA   
                        Devices_PIR_count[(Device_count+1):(Device_count+chunk)] <<-NA   

                        my_dims = dim(ResultsMatrix)
                        ResultsMatrix<<- rbind(ResultsMatrix,matrix(NaN,nrow=chunk,
                                        ncol=my_dims[2]))
                        if(Do_testflag_matrix)  TestFlagMatrix<<- rbind(TestFlagMatrix,matrix(NaN,nrow=chunk,
                                        ncol=my_dims[2]))
                        if(Do_test_order_matrix)  TestOrderMatrix<<- rbind(TestOrderMatrix,matrix(NaN,nrow=chunk,
                                        ncol=my_dims[2]))
                        if(Do_mult_limits>0)  MultLimIndexMatrix<<- rbind(MultLimIndexMatrix,matrix(NaN,nrow=chunk,
                                        ncol=my_dims[2]))
                    }
                }
				Devices_part_id[Device_count] <<- part_id 
				Devices_x_coord[Device_count] <<- x_coord 
				Devices_y_coord[Device_count] <<- y_coord
				Devices_site[Device_count] <<- site_num 
				Devices_PIR_count[Device_count] <<- PIR_count 
           } else {
                #DevicesFrame[Device_count,] <<- my_list
                Devices_part_id[Device_count] <<- part_id   
                Devices_temp[Device_count] <<- NA       
                Devices_x_coord[Device_count] <<- x_coord    
                Devices_y_coord[Device_count] <<- y_coord  
                Devices_wafer_index[Device_count] <<- NA 
                Devices_soft_bin[Device_count] <<- NA   
                Devices_hard_bin[Device_count] <<- NA   
                Devices_testtime[Device_count] <<-NA    
                Devices_site[Device_count] <<- site_num 
				Devices_PIR_count[Device_count] <<- PIR_count 
                
				# below is just to hide warning when processing interleaved multisite data...
				params = dim(ResultsMatrix)[2]
				if(params<1) {
					ResultsMatrix<<-array(NaN, dim=c(Device_count,0))
					if(Do_testflag_matrix)  TestFlagMatrix<<-array(NaN, dim=c(Device_count,0))
					if(Do_test_order_matrix)  TestOrderMatrix<<-array(NaN, dim=c(Device_count,0))
					if(Do_mult_limits>0)  MultLimIndexMatrix<<-array(NaN, dim=c(Device_count,0))
					
				} else {
					ResultsMatrix<<- rbind(ResultsMatrix,NaN)
					if(Do_testflag_matrix)  TestFlagMatrix<<- rbind(TestFlagMatrix,NaN)
					if(Do_test_order_matrix)  TestOrderMatrix<<- rbind(TestOrderMatrix,NaN)
					if(Do_mult_limits>0)  MultLimIndexMatrix<<- rbind(MultLimIndexMatrix,NaN)
				}
            }
        }

		if (Do_conditions) {
			if ( sum(dim(ConditionsMatrix))==0) {
				ConditionsMatrix <<- array(NaN,dim=c(1,0))
			} else if ((dim(ConditionsMatrix)[2])<1) {
					ConditionsMatrix <<- array(NaN,dim=c(Device_count,0))
			} else {
					ConditionsMatrix <<- rbind(ConditionsMatrix,NaN)
			}
		}
		if (Do_DTRs) {
			if ( sum(dim(DTRsMatrix))==0) {
				DTRsMatrix <<- array(NaN,dim=c(1,0))
			} else if ((dim(DTRsMatrix)[2])<1) {
					DTRsMatrix <<- array(NaN,dim=c(Device_count,0))
			} else {
					DTRsMatrix <<- rbind(DTRsMatrix,NaN)
			}
		}
		if (Parse_PRR_part_txt) {
			if ( sum(dim(PRRtxt_Matrix))==0) {
				PRRtxt_Matrix <<- array(NaN,dim=c(1,0))
			} else if ((dim(PRRtxt_Matrix)[2])<1) {
					PRRtxt_Matrix <<- array(NaN,dim=c(Device_count,0))
			} else {
					PRRtxt_Matrix <<- rbind(PRRtxt_Matrix,NaN)
			}
		}
   }
    
}


#############################################################################
parse_PRR_record <- function(rec_len,endy) {

    # initialize variables
    head_num = NaN
    site_num = NaN
    part_flg = 0
    num_test = 0
    hard_bin = NaN
    soft_bin = NaN
    x_coord = NaN
    y_coord = NaN
    test_t = NaN
    part_id = ''
    part_txt = ''
    part_fix = ''

	# stdf v3 fields
	pad_byte = 0


    valid_record = TRUE

    if ( (rec_len<10) || ((rec_len<13)&&(Stdf_Version!=3)) ) {
        valid_record = FALSE
        cat("WARNING: PRR record shorter than expected \n")
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } else if (Stdf_Version==3) {
        head_num = as.integer(Stdf[Ptr])
        site_num = as.integer(Stdf[Ptr+1])
        num_test = readBin(Stdf[(Ptr+2):(Ptr+3)],integer(),n=1,size=2,endian=endy,signed=FALSE)
        hard_bin = readBin(Stdf[(Ptr+4):(Ptr+5)],integer(),n=1,size=2,endian=endy,signed=FALSE)
        soft_bin = readBin(Stdf[(Ptr+6):(Ptr+7)],integer(),n=1,size=2,endian=endy,signed=FALSE)
        part_flg = as.integer(Stdf[Ptr+8])
        pad_byte = as.integer(Stdf[Ptr+9])
    } else {
        #head_num = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        head_num = as.integer(Stdf[Ptr])
        #site_num = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        site_num = as.integer(Stdf[Ptr+1])
        #part_flg = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        part_flg = as.integer(Stdf[Ptr+2])
        Ptr <<- Ptr + 3
        num_test = readBin(Stdf[Ptr:(Ptr+1)],integer(),n=1,size=2,endian=endy,signed=FALSE)
        hard_bin = readBin(Stdf[(Ptr+2):(Ptr+3)],integer(),n=1,size=2,endian=endy,signed=FALSE)
        soft_bin = readBin(Stdf[(Ptr+4):(Ptr+5)],integer(),n=1,size=2,endian=endy,signed=FALSE)
        x_coord = readBin(Stdf[(Ptr+6):(Ptr+7)],integer(),n=1,size=2,endian=endy,signed=TRUE)
        y_coord = readBin(Stdf[(Ptr+8):(Ptr+9)],integer(),n=1,size=2,endian=endy,signed=TRUE)
        Ptr <<- Ptr + 10
        rec_len = rec_len - 13  
    }

	if (Stdf_Version==3) {
		if (rec_len>1) {
			x_coord = readBin(Stdf[(Ptr):(Ptr+1)],integer(),n=1,size=2,endian=endy,signed=TRUE)
			Ptr <<- Ptr + 2
			rec_len = rec_len - 2
		}

		if (rec_len>1) {
			y_coord = readBin(Stdf[(Ptr):(Ptr+1)],integer(),n=1,size=2,endian=endy,signed=TRUE)
			Ptr <<- Ptr + 2
			rec_len = rec_len - 2
		}
	} else {
	    if (rec_len>3) {
		    #test_t = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
		    test_t = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
			if(test_t<0)  test_t = test_t + 2^32
			Ptr <<- Ptr + 4
			rec_len = rec_len - 4
		} else {
			cat(sprintf("WARNING: PRR record incomplete \n"))
			#bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
			Ptr <<- Ptr + rec_len
			rec_len = 0
		}
	}

    if (rec_len>0) {
        str_list = readSTDFstring(rec_len)
        part_id = str_list$string
        rec_len = str_list$bytes_left   
    }

    if (rec_len>0) {
        str_list = readSTDFstring(rec_len)
        part_txt = str_list$string
        rec_len = str_list$bytes_left   
    }

    if (rec_len>0) {
        str_list = readSTDFstring(rec_len)
        part_fix = str_list$string
        rec_len = str_list$bytes_left   
    }


    if (rec_len >0) {
        cat(sprintf("WARNING: PRR record longer than expected \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } 

    #valid_record=FALSE # this fixes the slower and slower behaviour...
    if (valid_record) {

		if(Parse_PRR_part_txt) {
			# split based on ","
			my_substrings = strsplit(part_txt,",")[[1]]	# returns list, want vector of strings
			if(length(my_substrings)>0) {
				for (j in 1:length(my_substrings)) {
					#browser()
					tokens = strsplit(my_substrings[j],"=")[[1]]
					if(length(tokens)==2) {
						name = tokens[1]
						value = tokens[2]

						# has this field already been created?
						field_index = match(name,PRRtxt_Names,nomatch=0)
						if (field_index<1) {
							PRRtxt_Names_count <<- PRRtxt_Names_count + 1
							field_index = PRRtxt_Names_count
							PRRtxt_Names[field_index] <<- name

							# we added a new PRRtxt field, so we need
							# to add a column to PRRtxt_Matrix
							if( sum(dim(PRRtxt_Matrix))==0) {
								PRRtxt_Matrix <<- array(NaN,dim=c(0,1))
							} else if ((dim(PRRtxt_Matrix)[1])<1) {
								PRRtxt_Matrix <<- array(NaN,dim=c(0,field_index))
							} else {
								PRRtxt_Matrix <<- cbind(PRRtxt_Matrix,NaN)
							}
						}
						# update PRRtxt_Matrix
						#----------------------
						device_count = Open_site[site_num+1]
						PRRtxt_Matrix[device_count,field_index] <<- value
					} else {
						# nasty message
					}
				}
			}
		}


        #    the below syntax is faster than DevicesFrame[Device_count,"part_id"]<<-part_id
        #    but it still has the slower and slower behaviour...
        # DevicesFrame$part_id[[Device_count]]<<-part_id
        #    so go to individual vectors, combine into data.frame once at end of stdf.
		device_count = Open_site[site_num+1]
		if(device_count>0) {
			if (Do_conditions) {
				indices = which(Devices_PIR_count==Devices_PIR_count[device_count])
			} else {
				indices = device_count
			}
			if ( (as.raw(part_flg) & as.raw(16+8+4))==0 ) {
				pass_flag = 1
			} else {
				pass_flag = 0
			}
			Devices_part_id[indices]<<-as.character(part_id)
			Devices_x_coord[indices]<<-as.integer(x_coord)
			Devices_y_coord[indices]<<-as.integer(y_coord)
			Devices_wafer_index[indices]<<-as.integer(Wafer_count)
			Devices_hard_bin[indices]<<-as.integer(hard_bin)
			Devices_soft_bin[indices]<<-as.integer(soft_bin)
			Devices_pass_flag[device_count]<<-as.integer(pass_flag)
			Devices_testtime[device_count]<<-as.numeric(test_t)
			Devices_site[device_count]<<-as.numeric(site_num)

			Open_site[site_num+1] <<- 0
		} else {
			# never supposed to get here!
		}
    }

}


#############################################################################
parse_TSR_record <- function(rec_len,endy) {

    # initialize variables
    head_num = 0
    site_num = 0
    test_typ = ' '
    test_num = 0
    exec_cnt = 0
    fail_cnt = 0
    alrm_cnt = 0
    test_nam = ''
    seq_name = ''
    test_lbl = ''
    opt_flag = 0
    test_tim = 0
    test_min = 0
    test_max = 0
    tst_sums = 0
    tst_sqrs = 0

	# stdf version 3 fields
	pad_byte = 0
	tst_mean = NaN
	tst_sdev = NaN


    valid_record = TRUE

    if ( (rec_len<16) || ((rec_len<19)&&(Stdf_Version!=3)) ) {
        valid_record = FALSE
        cat("WARNING: TSR record shorter than expected \n")
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } else if (Stdf_Version==3) {
		head_num = 255
        #test_num = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        test_num = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		if(test_num<0)  test_num = test_num + 2^32
        Ptr <<- Ptr + 4
        #exec_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        exec_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		if(exec_cnt<0)  exec_cnt = exec_cnt + 2^32
        Ptr <<- Ptr + 4
        #fail_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        fail_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		if(fail_cnt<0)  fail_cnt = fail_cnt + 2^32
        Ptr <<- Ptr + 4
        #alrm_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        alrm_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		if(alrm_cnt<0)  alrm_cnt = alrm_cnt + 2^32
        Ptr <<- Ptr + 4
        rec_len = rec_len - 16
    } else {
        #head_num = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        head_num = as.integer(Stdf[Ptr])
        #site_num = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        site_num = as.integer(Stdf[Ptr+1])
        #test_typ = readChar(STDF,1)
        test_typ = rawToChar(Stdf[Ptr+2])
        Ptr <<- Ptr + 3
        #test_num = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        test_num = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		if(test_num<0)  test_num = test_num + 2^32
        Ptr <<- Ptr + 4
        #exec_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        exec_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		if(exec_cnt<0)  exec_cnt = exec_cnt + 2^32
        Ptr <<- Ptr + 4
        #fail_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        fail_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		if(fail_cnt<0)  fail_cnt = fail_cnt + 2^32
        Ptr <<- Ptr + 4
        #alrm_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        alrm_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		if(alrm_cnt<0)  alrm_cnt = alrm_cnt + 2^32
        Ptr <<- Ptr + 4
        rec_len = rec_len - 19
    }

    if (Stdf_Version!=3) {
		if (rec_len>0) {
			str_list = readSTDFstring(rec_len)
			test_nam = str_list$string
			rec_len = str_list$bytes_left   
		}

		if (rec_len>0) {
			str_list = readSTDFstring(rec_len)
			seq_name = str_list$string
			rec_len = str_list$bytes_left   
		}

		if (rec_len>0) {
			str_list = readSTDFstring(rec_len)
			test_lbl = str_list$string
			rec_len = str_list$bytes_left   
		}
	}

    if (rec_len>0) {
        #opt_flag = readChar(STDF,1)
        opt_flag = rawToChar(Stdf[Ptr])
        Ptr <<- Ptr + 1
        rec_len = rec_len - 1
    }

    if (Stdf_Version==3) {
		if (rec_len>0) {
			pad_byte = rawToChar(Stdf[Ptr])
			Ptr <<- Ptr + 1
			rec_len = rec_len - 1
		}
	} else {
		if (rec_len>3) {
			test_tim = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
			Ptr <<- Ptr + 4
			rec_len = rec_len - 4
		} else {
			#cat(sprintf("WARNING: TSR record incomplete \n"))
			#bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
			Ptr <<- Ptr + rec_len
			rec_len = 0
		}
	}

    if (rec_len>3) {
        test_min = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    } else {
        #cat(sprintf("WARNING: TSR record incomplete \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    }

    if (rec_len>3) {
        test_max = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    } else {
        #cat(sprintf("WARNING: TSR record incomplete \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    }

    if (Stdf_Version==3) {
		if (rec_len>3) {
			tst_mean = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
			Ptr <<- Ptr + 4
			rec_len = rec_len - 4
		} else {
			Ptr <<- Ptr + rec_len
			rec_len = 0
		}

		if (rec_len>3) {
			tst_sdev = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
			Ptr <<- Ptr + 4
			rec_len = rec_len - 4
		} else {
			Ptr <<- Ptr + rec_len
			rec_len = 0
		}
	}

	if (rec_len>3) {
        tst_sums = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    } else {
        #cat(sprintf("WARNING: TSR record incomplete \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    }

    if (rec_len>3) {
        tst_sqrs = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    } else {
        #cat(sprintf("WARNING: TSR record incomplete \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    }

    if (Stdf_Version==3) {
		if (rec_len>0) {
			str_list = readSTDFstring(rec_len)
			test_nam = str_list$string
			rec_len = str_list$bytes_left   
		}

		if (rec_len>0) {
			str_list = readSTDFstring(rec_len)
			seq_name = str_list$string
			rec_len = str_list$bytes_left   
		}
	}


    # throw away any remaining portion of the record we aren't interested in...
    #--------------------------------------------------------------------------
    if (rec_len > 0) {
        cat(sprintf("WARNING: TSR record longer than expected \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } 


	#browser()


    # now update TSRFrame
    #---------------------------------
    if (nchar(test_nam)<1) {
		if (Executive_type=="ig900plus") {
			# J973 etc...
			# build testname from seq_name + :: + test_num
			test_nam = sprintf("%s::%d",seq_name,test_num)
			# update Parameters_Names for corresponding test_num...
			if ((Previous_param_i < Parameter_count) &&
					(test_num==Parameters_testnum[Previous_param_i+1])) {
				par_index = Previous_param_i+1
				Good_guesses <<- Good_guesses + 1
			} else {
				par_index = match(test_num,Parameters_testnum,nomatch=0)
			}
			if(par_index>0) {
				Parameters_Names[par_index] <<- test_nam
			}
		} else {
			valid_record = FALSE
			cat(sprintf("ERROR: test_nam field empty in TSR for test_num %d \n",
                    test_num))
		}
    } else {
        # Fusion: remove "/" at end of test name if it is there
        if (Executive_type=="enVision") {
			# if R14.4.1, now puts < No Name > at end too!
            test_nam = sub("< No Name >$","",test_nam)
			if(Ignore_testname_objects) {
				test_nam = sub("/.*/","",test_nam)
			}
            test_nam = sub("/$","",test_nam)
        }
        if (Executive_type=="Image") {
            # need to remove <>... stuff ?? no, it is only in PTR/FTR/MPR, not TSR
        }
    }

	# for now, ignore per-site records, just grab overall summary records...
	# head_num or site_num!!!  
	if (Executive_type=="ig900plus") {
		if (site_num>0)  valid_record = FALSE
	} else if (Executive_type=="enVision") {
		# If single site, then TSR for site only, no overall TSR provided...
		# NOTE: overall TSR is optional! for multi-site, so
		# we really do need to track all sites and if no overall count, generate it from sum of sites
		#
		if (site_num>0) {
			if (!is.finite(TSR_siteA)) TSR_siteA <<- site_num
			if (site_num != TSR_siteA)  valid_record = FALSE
		}
	} else if (Executive_type=="Image") {
		if (head_num<255)  valid_record = FALSE		# uses head_num=255 as overall TSR
	} else if (Executive_type=="T2000") {
		if (site_num<255)  valid_record = FALSE
	} else if (Executive_type=="93000") {
		if ((site_num>0) && (site_num<255))  valid_record = FALSE
	} else if (Executive_type!="Credence") {
		if (site_num<255)  valid_record = FALSE
	}

	if (substr(MIR_Tester_type,1,9)=="Fusion_HF") {
		# ignore gratuitous "PATTERNS" TSR's that enVision adds
		if (test_nam=="PATTERNS") {
			valid_record = FALSE
		}
		if (test_nam=="<Non-unique Test Number>") {
			# need to use testnumber to lookup testname that conflicted with enVision auto "PATTERNS"
			valid_record = FALSE
		}
	}
	#browser()
	
	
    if (valid_record) {
		raw_test_nam = test_nam
		if (Duplicate_testnames) {
			test_nam = sprintf("%s_%d",test_nam,test_num)
		}
        if ((Parameter_count>0) && (!Do_Raw_TSRs)) {
			if ((Previous_param_i < Parameter_count) &&
					(test_nam==Parameters_Names[Previous_param_i+1])) {
				par_index = Previous_param_i+1
				Previous_param_i <<- par_index
				Good_guesses <<- Good_guesses + 1
			} else {
				par_index = match(test_nam,Parameters_Names,nomatch=0)
			}
        } else {
            par_index = 0
        }
        
        if (Debug5) {
            cat(sprintf("Doing TSR for %s testnum %d \n",test_nam,test_num))
			cat(sprintf("     par_index = %d, TSR_count = %d, Good_guesses = %d \n",
					par_index,TSR_count,Good_guesses))
		
        }

        if ((par_index<1) && (!Do_Raw_TSRs)) {
            if (exec_cnt>0) {
                # check if it is an MPR record...
                # ParametersFrame will have testnam + "/" + pinname
                # so need to search for testnam/ as substring in testnames
                my_text = sprintf("^%s/",test_nam)
                par_indices = grep(my_text,Parameters_Names)

                if (Executive_type=="93000") {
                    if (length(par_indices)<1) {
                        # TSR can have shorter name than MPR, truncated at ":" char...
                        my_text = sprintf("^%s:",test_nam)
                        par_indices = grep(my_text,Parameters_Names)
                    }
                    if (length(par_indices)<1) {
                        # testname can have [ or ] characters...
                        par_indices = grep(test_nam,Parameters_Names,fixed=TRUE)
                    }
                }

				if (Executive_type=="IntegraFlex") {
					# for some reason, the testname in PTR is testname pinname channel <> comment
					#    but in the TSR, they drop the <> comment, so need to strip that...
					# if duplicate testnames flag set, then _tnum at end of Parameters testname too
					if (Auto_flex) {
						# remove channel from TSR name
						test_nam = sub(" [[:digit:]]+[.][[:alpha:]][[:digit:]]+$","",raw_test_nam)
					}
					if (Duplicate_testnames) {
						test_nam = sprintf("%s_%d",test_nam,test_num)
						# REVISIT.. may want to check for [] and other special grep chars in test_nam 
						grep_test_nam = sprintf("^%s.*_%d$",test_nam,test_num)
						par_indices = grep(grep_test_nam,Parameters_Names)
					} else {
						par_indices = grep(test_nam,Parameters_Names,fixed=TRUE)
					}
				}
                if (length(par_indices)<1) {
					if (Max_parts!=0) {
                    	cat(sprintf("WARNING: TSR with no PTR/MPR/FTRs for test %s \n",
                       		test_nam))
					}
                } else {
					if (is.finite(TSR_siteA) && (site_num == TSR_siteA)) {
						TSR_count_siteA <<- TSR_count_siteA + 1
						TSRs_siteA_testnum[TSR_count_siteA] <<- as.integer(test_num)
						TSRs_siteA_testname[TSR_count_siteA] <<- as.character(test_nam)
						TSRs_siteA_test_typ[TSR_count_siteA] <<- as.character(test_typ)
						TSRs_siteA_exec_cnt[TSR_count_siteA] <<- as.integer(exec_cnt)
						TSRs_siteA_fail_cnt[TSR_count_siteA] <<- as.integer(fail_cnt)
						TSRs_siteA_fixed_exec_cnt[TSR_count_siteA] <<- as.integer(exec_cnt)
						TSRs_siteA_fixed_fail_cnt[TSR_count_siteA] <<- as.integer(fail_cnt)
					} else {
						TSR_count <<- TSR_count + 1
						TSRs_testnum[TSR_count] <<- as.integer(test_num)
						TSRs_testname[TSR_count] <<- as.character(test_nam)
						TSRs_test_typ[TSR_count] <<- as.character(test_typ)
						TSRs_exec_cnt[TSR_count] <<- as.integer(exec_cnt)
						TSRs_fail_cnt[TSR_count] <<- as.integer(fail_cnt)
						TSRs_fixed_exec_cnt[TSR_count] <<- as.integer(exec_cnt)
						TSRs_fixed_fail_cnt[TSR_count] <<- as.integer(fail_cnt)
					}
                }
            }
        } else {
            # add TSR info to TSRFrame
            #--------------------------------
			if (is.finite(TSR_siteA) && (site_num == TSR_siteA)) {
				TSR_count_siteA <<- TSR_count_siteA + 1
				TSRs_siteA_testnum[TSR_count_siteA] <<- as.integer(test_num)
				TSRs_siteA_testname[TSR_count_siteA] <<- as.character(test_nam)
				TSRs_siteA_test_typ[TSR_count_siteA] <<- as.character(test_typ)
				TSRs_siteA_exec_cnt[TSR_count_siteA] <<- as.integer(exec_cnt)
				TSRs_siteA_fail_cnt[TSR_count_siteA] <<- as.integer(fail_cnt)
				TSRs_siteA_fixed_exec_cnt[TSR_count_siteA] <<- as.integer(exec_cnt)
				TSRs_siteA_fixed_fail_cnt[TSR_count_siteA] <<- as.integer(fail_cnt)
			} else {
				TSR_count <<- TSR_count + 1
				TSRs_testnum[TSR_count] <<- as.integer(test_num)
				TSRs_testname[TSR_count] <<- as.character(test_nam)
				TSRs_test_typ[TSR_count] <<- as.character(test_typ)
				TSRs_exec_cnt[TSR_count] <<- as.integer(exec_cnt)
				TSRs_fail_cnt[TSR_count] <<- as.integer(fail_cnt)
				TSRs_fixed_exec_cnt[TSR_count] <<- as.integer(exec_cnt)
				TSRs_fixed_fail_cnt[TSR_count] <<- as.integer(fail_cnt)
			}
        }
    }

}


#############################################################################
parse_HBR_record <- function(rec_len,endy) {

    # initialize variables
    head_num = 0
    site_num = 0
    hbin_num = 0
    hbin_cnt = 0
    hbin_pf = ''
    hbin_nam = ''

    valid_record = TRUE

    if ( (rec_len<6) || ((rec_len<9)&&(Stdf_Version!=3)) ) {
        valid_record = FALSE
        cat("WARNING: HBR record shorter than expected \n")
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } else if (Stdf_Version==3) {
		head_num = 255
        hbin_num = readBin(Stdf[Ptr:(Ptr+1)],integer(),n=1,size=2,endian=endy,signed=FALSE)
        #hbin_cnt = readBin(Stdf[(Ptr+2):(Ptr+5)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        hbin_cnt = readBin(Stdf[(Ptr+2):(Ptr+5)],integer(),n=1,size=4,endian=endy)
		if(hbin_cnt<0)  hbin_cnt = hbin_cnt + 2^32
        Ptr <<- Ptr + 6
        rec_len = rec_len - 6
    } else {
        #head_num = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        head_num = as.integer(Stdf[Ptr])
        #site_num = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        site_num = as.integer(Stdf[Ptr+1])
        Ptr <<- Ptr + 2
        hbin_num = readBin(Stdf[Ptr:(Ptr+1)],integer(),n=1,size=2,endian=endy,signed=FALSE)
        Ptr <<- Ptr + 2
        #hbin_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        hbin_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		if(hbin_cnt<0)  hbin_cnt = hbin_cnt + 2^32
        Ptr <<- Ptr + 4
        #hbin_pf = readChar(STDF,1)
        if(Stdf[Ptr]>0)  hbin_pf = rawToChar(Stdf[Ptr])
        Ptr <<- Ptr + 1
        rec_len = rec_len - 9
    }

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        hbin_nam = str_list$string
        rec_len = str_list$bytes_left
    }

    # throw away any remaining portion of the record we aren't interested in...
    #--------------------------------------------------------------------------
    if (rec_len > 0) {
        cat(sprintf("WARNING: HBR record longer than expected \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } 

    # is this a valid record to update HbinInfoFrame?
    #----------------------------------------------------------------------
	# for now, ignore per-site records, just grab overall summary records...
	if (Executive_type=="ig900plus") {
		if (head_num>0)  valid_record = FALSE
	} else if (Executive_type=="T2000") {
		if (site_num<255)  valid_record = FALSE
	} else if (Executive_type!="Credence") {
		if (head_num<255)  valid_record = FALSE
	}
    if (valid_record) {
        Hbin_count <<- Hbin_count + 1
        my_list = list(hbin_num=hbin_num, hbin_cnt=hbin_cnt,
                        hbin_pf=hbin_pf, hbin_nam=hbin_nam)
        HbinInfoFrame[Hbin_count,] <<- my_list
    }
}


#############################################################################
parse_SBR_record <- function(rec_len,endy) {

    # initialize variables
    head_num = 0
    site_num = 0
    sbin_num = 0
    sbin_cnt = 0
    sbin_pf = ''
    sbin_nam = ''

    valid_record = TRUE

    if ( (rec_len<6) || ((rec_len<9)&&(Stdf_Version!=3)) ) {
        valid_record = FALSE
        cat("WARNING: SBR record shorter than expected \n")
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } else if (Stdf_Version==3) {
		head_num = 255
        sbin_num = readBin(Stdf[Ptr:(Ptr+1)],integer(),n=1,size=2,endian=endy,signed=FALSE)
        #sbin_cnt = readBin(Stdf[(Ptr+2):(Ptr+5)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        sbin_cnt = readBin(Stdf[(Ptr+2):(Ptr+5)],integer(),n=1,size=4,endian=endy)
		if(sbin_cnt<0)  sbin_cnt = sbin_cnt + 2^32		# unsigned 4 byte integer value in 64bit float
        Ptr <<- Ptr + 6
        rec_len = rec_len - 6
    } else {
        #head_num = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        head_num = as.integer(Stdf[Ptr])
        #site_num = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        site_num = as.integer(Stdf[Ptr+1])
        Ptr <<- Ptr + 2
        sbin_num = readBin(Stdf[Ptr:(Ptr+1)],integer(),n=1,size=2,endian=endy,signed=FALSE)
        Ptr <<- Ptr + 2
        #sbin_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        sbin_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		#if(sbin_cnt<0)  sbin_cnt = sbin_cnt + 2^32		# unsigned 4 byte integer value in 64bit float
		# above line is correct, but 93K smar 7.1 will occasionally have value of -1 instead of 0
		# so the below should deal with smar.  sbin_cnt should be below 4.2 billion anyways.
		if(sbin_cnt<0)  sbin_cnt = 0
        Ptr <<- Ptr + 4
        #sbin_pf = readChar(STDF,1)
        if(Stdf[Ptr]==0)  sbin_pf = ""
		else  sbin_pf = rawToChar(Stdf[Ptr])
        Ptr <<- Ptr + 1
        rec_len = rec_len - 9
    }

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        sbin_nam = str_list$string
        rec_len = str_list$bytes_left
    }

    # throw away any remaining portion of the record we aren't interested in...
    #--------------------------------------------------------------------------
    if (rec_len > 0) {
        cat(sprintf("WARNING: SBR record longer than expected \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } 

    # is this a valid record to update SbinInfoFrame?
    #----------------------------------------------------------------------
    if (valid_record) {
		# STDF spec.. if HEAD_NUM=255, then it is an overall (all sites) record
		#    not really intuitive, would have expected if SITE_NUM=255?

		all_sites = TRUE
		if (Executive_type=="ig900plus") {
			if (head_num>0)  all_sites = FALSE	# j9xx uses head=0 not head=255!??
		} else if (Executive_type=="T2000") {
			if (site_num<255)  all_sites = FALSE
		} else if (Executive_type=="Credence") {
			all_sites = TRUE	# Quartet.. doesn't follow the rules? 
								# no multisite examples to verify.
		} else if (head_num<255) {
			all_sites = FALSE
		}

		if(all_sites) {
			Sbin_count <<- Sbin_count + 1
			my_list = list(sbin_num=sbin_num, sbin_cnt=sbin_cnt,
							sbin_pf=sbin_pf, sbin_nam=sbin_nam)
			SbinInfoFrame[Sbin_count,] <<- my_list
		} else {
			# per site soft bin tracking

			# have we seen this sbin before?
			if(SiteSbinCount>0) {
				sbin_index = match(sbin_nam,as.character(SiteSbinInfoFrame[["sbin_nam"]]),nomatch=0)
			} else {
				sbin_index = 0
			}
			if(sbin_index<1) {
				SiteSbinCount <<- SiteSbinCount + 1
				#cat(sprintf("SiteSbinCount is now %d \n",SiteSbinCount))

				sbin_index = SiteSbinCount
				SiteSbinInfoFrame[[sbin_index,"sbin_num"]] <<- sbin_num
				SiteSbinInfoFrame[[sbin_index,"sbin_nam"]] <<- sbin_nam
				SiteSbinInfoFrame[[sbin_index,"sbin_pf"]] <<- sbin_pf

				if( sum(dim(SiteSbinCountMatrix))==0) {
					SiteSbinCountMatrix <<- array(NaN,dim=c(0,1))
				} else if ((dim(SiteSbinCountMatrix)[1])<1) {
					SiteSbinCountMatrix <<- array(NaN,dim=c(0,sbin_index))
				} else {
					SiteSbinCountMatrix <<- cbind(SiteSbinCountMatrix,NaN)
				}
			}

			# have we seen this site before?
			if(length(SiteSbinSiteVector)>0) {
				site_index = match(site_num,SiteSbinSiteVector,nomatch=0)
			} else {
				site_index = 0
			}
			if(site_index<1) {
				site_index = length(SiteSbinSiteVector) + 1
				SiteSbinSiteVector[site_index] <<- site_num

				SiteSbinCountMatrix <<- rbind(SiteSbinCountMatrix,NaN)
			}

			# put count in correct sbin / site location of Matrix
			SiteSbinCountMatrix[site_index,sbin_index] <<- sbin_cnt
		}
    }


}


#############################################################################
parse_PDR_record <- function(rec_len,endy) {

    # initialize variables
    test_num = 0
    desc_flg = NaN
    opt_flag = NaN
    res_scal = 0
    units = ''
    res_ldig = 0
    res_rdig = 0
    llm_scal = 0
    hlm_scal = 0
    llm_ldig = 0
    llm_rdig = 0
    hlm_ldig = 0
    hlm_rdig = 0
    lo_limit = NaN
    hi_limit = NaN
    test_nam = ''
    seq_name = ''


    valid_record = TRUE

	if (rec_len<6) {
        valid_record = FALSE
        cat("WARNING: PDR record shorter than expected \n")
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } else {
        #test_num = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        test_num = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy) 
		if(test_num<0)  test_num = test_num + 2^32
        Ptr <<- Ptr + 4
        desc_flg = as.integer(Stdf[Ptr])
        opt_flag = as.integer(Stdf[Ptr+1])
        Ptr <<- Ptr + 2
        rec_len = rec_len - 6
    }

	if (rec_len>0) {
        res_scal = readBin(Stdf[Ptr],integer(),n=1,size=1,endian=endy,signed=TRUE)
        Ptr <<- Ptr + 1
        rec_len = rec_len - 1
	}

	if (rec_len>6) {
        units = rawToChar(Stdf[Ptr:(Ptr+6)])
        Ptr <<- Ptr + 7
        rec_len = rec_len - 7
	} else {
        Ptr <<- Ptr + rec_len
		rec_len = 0
	}

	if (rec_len >0) {
		res_ldig = readBin(Stdf[(Ptr)],integer(),n=1,size=1,signed=FALSE)
		Ptr <<- Ptr + 1
		rec_len = rec_len - 1
	}

	if (rec_len >0) {
		res_rdig = readBin(Stdf[(Ptr)],integer(),n=1,size=1,signed=FALSE)
		Ptr <<- Ptr + 1
		rec_len = rec_len - 1
	}

	if (rec_len >0) {
		llm_scal = readBin(Stdf[(Ptr)],integer(),n=1,size=1,signed=FALSE)
		Ptr <<- Ptr + 1
		rec_len = rec_len - 1
	}

	if (rec_len >0) {
		hlm_scal = readBin(Stdf[(Ptr)],integer(),n=1,size=1,signed=FALSE)
		Ptr <<- Ptr + 1
		rec_len = rec_len - 1
	}

	if (rec_len >0) {
		llm_ldig = readBin(Stdf[(Ptr)],integer(),n=1,size=1,signed=FALSE)
		Ptr <<- Ptr + 1
		rec_len = rec_len - 1
	}

	if (rec_len >0) {
		llm_rdig = readBin(Stdf[(Ptr)],integer(),n=1,size=1,signed=FALSE)
		Ptr <<- Ptr + 1
		rec_len = rec_len - 1
	}

	if (rec_len >0) {
		hlm_ldig = readBin(Stdf[(Ptr)],integer(),n=1,size=1,signed=FALSE)
		Ptr <<- Ptr + 1
		rec_len = rec_len - 1
	}

	if (rec_len >0) {
		hlm_rdig = readBin(Stdf[(Ptr)],integer(),n=1,size=1,signed=FALSE)
		Ptr <<- Ptr + 1
		rec_len = rec_len - 1
	}

    if (rec_len >3) {
        lo_limit = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    } else {
        Ptr <<- Ptr + rec_len
        rec_len = 0
    }

    if (rec_len >3) {
        hi_limit = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    } else {
        Ptr <<- Ptr + rec_len
        rec_len = 0
    }

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        test_nam = str_list$string
        rec_len = str_list$bytes_left
    }

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        seq_name = str_list$string
        rec_len = str_list$bytes_left
    }

    # throw away any remaining portion of the record we aren't interested in...
    #--------------------------------------------------------------------------
    if (rec_len > 0) {
        cat(sprintf("WARNING: PDR record longer than expected \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } 

    # is this a valid record to update ParametersFrame and ResultsMatrix ?
    #----------------------------------------------------------------------
    if (valid_record) {
        valid_llim = FALSE
        valid_ulim = FALSE
        valid_scale = FALSE

        opt_flag = as.raw(opt_flag)   # enable bitwise math...
        #cat(sprintf("DEBUGGING: lo_limit = %f \n",lo_limit))
        if ( ((opt_flag & as.raw(64+16))==0) && is.finite(lo_limit) ) {
            valid_llim = TRUE
        }
        if ( ((opt_flag & as.raw(128+32))==0) && is.finite(hi_limit) ) {
            valid_ulim = TRUE
        }
        if ( ((opt_flag & as.raw(1))==0) && is.finite(res_scal) ) {
            valid_scale = TRUE
        }

        # has this test already been added to ParametersFrame ?
        #-------------------------------------------------------
		test_txt = test_nam
		if (Duplicate_testnames && (nchar(test_txt)>0)) {
			test_txt = sprintf("%s_%d",test_txt,test_num)
		}
        if (Parameter_count>0) {
            if (nchar(test_txt)<1) {
                par_index = match(test_num,Parameters_testnum,nomatch=0)
            } else {
				if ((Previous_param_i < Parameter_count) &&
						(test_txt==Parameters_Names[Previous_param_i+1])) {
					par_index = Previous_param_i+1
					Good_guesses <<- Good_guesses + 1
				} else {
					par_index = match(test_txt,Parameters_Names,nomatch=0)
				}
			}
        } else {
            par_index = 0
        }
		
        if (par_index<1) {
            Parameter_count <<- Parameter_count + 1
            par_index = Parameter_count

            Parameters_testnum[Parameter_count] <<- test_num
            if (nchar(test_txt)<1) {
                Parameters_Names[Parameter_count] <<- ''
            } else {
                Parameters_Names[Parameter_count] <<- test_txt
            }
            if (valid_scale) {
                Parameters_scaler[Parameter_count] <<- res_scal
            } else {
                Parameters_scaler[Parameter_count] <<- 0
            }
            Parameters_units[Parameter_count] <<- units
            if (valid_llim) {
                Parameters_ll[Parameter_count] <<- lo_limit
            } else {
                Parameters_ll[Parameter_count] <<- NaN
            }
            if (valid_ulim) {
                Parameters_ul[Parameter_count] <<- hi_limit
            } else {
                Parameters_ul[Parameter_count] <<- NaN
            }
			Parameters_ll_ge[Parameter_count] <<- 0
			Parameters_ul_ge[Parameter_count] <<- 0
            Parameters_plot_ll[Parameter_count] <<- NaN
            Parameters_plot_ul[Parameter_count] <<- NaN

            #Parameters_Names <<- as.character(ParametersFrame$testname)

            # we added a new parameter, so we need to add
            # a new column to Results Matrix...
            #---------------------------------------------
            ResultsMatrix <<- cbind(ResultsMatrix,NaN)
            if(Do_testflag_matrix)  TestFlagMatrix <<- cbind(TestFlagMatrix,NaN)
            if(Do_test_order_matrix)  TestOrderMatrix <<- cbind(TestOrderMatrix,NaN)
            if(Do_mult_limits>0)  MultLimIndexMatrix <<- cbind(MultLimIndexMatrix,NaN)
        }

		Previous_param_i <<- par_index
	}


}



#############################################################################
parse_PTR_record <- function(rec_len,endy) {

    # initialize variables
    test_num = 0
    head_num = 0
    site_num = 0
    test_flg = NaN
    parm_flg = NaN
    result = NaN
    test_txt = ''
    alarm_id = ''
    opt_flag = NaN
    res_scal = 0
    llm_scal = 0
    hlm_scal = 0
    lo_limit = NaN
    hi_limit = NaN
    units = ''
    c_resfmt = ''
    c_llmfmt = ''
    c_hlmfmt = ''
    lo_spec = NaN
    hi_spec = NaN

	# stdf version 3 fields
	res_ldig = 0
	res_rdig = 0
	desc_flag = 0
	llm_ldig = 0
	llm_rdig = 0
	hlm_ldig = 0
	hlm_rdig = 0
	test_nam = ""
	seq_name = ""


    valid_record = TRUE
    valid_opt_flag = FALSE

    if (rec_len<12) {
        valid_record = FALSE
        cat("WARNING: PTR record shorter than expected \n")
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } else {
        #test_num = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        test_num = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=TRUE)
		if(test_num<0)  test_num = test_num + 2^32
        Ptr <<- Ptr + 4
        #head_num = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        head_num = as.integer(Stdf[Ptr])
        #site_num = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        site_num = as.integer(Stdf[Ptr+1])
        #test_flg = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        test_flg = as.integer(Stdf[Ptr+2])
        #parm_flg = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        parm_flg = as.integer(Stdf[Ptr+3])
        Ptr <<- Ptr + 4
        result = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
        Ptr <<- Ptr + 4
        rec_len = rec_len - 12
    }


	if (Stdf_Version == 3) {
		if (rec_len >0) {
			opt_flag = as.integer(Stdf[Ptr])
			Ptr <<- Ptr + 1
			rec_len = rec_len - 1
			valid_opt_flag = TRUE
		}

		if (rec_len >0) {
			res_scal = readBin(Stdf[Ptr],integer(),n=1,size=1,signed=TRUE)
			Ptr <<- Ptr + 1
			rec_len = rec_len - 1
		}

		if (rec_len >0) {
			res_ldig = as.integer(Stdf[Ptr])
			Ptr <<- Ptr + 1
			rec_len = rec_len - 1
		}

		if (rec_len >0) {
			res_rdig = as.integer(Stdf[Ptr])
			Ptr <<- Ptr + 1
			rec_len = rec_len - 1
		}

		if (rec_len >0) {
			desc_flag = as.integer(Stdf[Ptr])
			Ptr <<- Ptr + 1
			rec_len = rec_len - 1
		}

		if (rec_len>6) {
	        units = rawToChar(Stdf[Ptr:(Ptr+6)])
	        Ptr <<- Ptr + 7
	        rec_len = rec_len - 7
		} else {
	        Ptr <<- Ptr + rec_len
			rec_len = 0
		}

		if (rec_len >0) {
			llm_scal = readBin(Stdf[Ptr],integer(),n=1,size=1,signed=TRUE)
			Ptr <<- Ptr + 1
			rec_len = rec_len - 1
		}

		if (rec_len >0) {
			hlm_scal = readBin(Stdf[Ptr],integer(),n=1,size=1,signed=TRUE)
			Ptr <<- Ptr + 1
			rec_len = rec_len - 1
		}

		if (rec_len >0) {
			llm_ldig = as.integer(Stdf[Ptr])
			Ptr <<- Ptr + 1
			rec_len = rec_len - 1
		}

		if (rec_len >0) {
			llm_rdig = as.integer(Stdf[Ptr])
			Ptr <<- Ptr + 1
			rec_len = rec_len - 1
		}

		if (rec_len >0) {
			hlm_ldig = as.integer(Stdf[Ptr])
			Ptr <<- Ptr + 1
			rec_len = rec_len - 1
		}

		if (rec_len >0) {
			hlm_rdig = as.integer(Stdf[Ptr])
			Ptr <<- Ptr + 1
			rec_len = rec_len - 1
		}

		if (rec_len >3) {
			lo_limit = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
			Ptr <<- Ptr + 4
			rec_len = rec_len - 4
		} else {
			#cat(sprintf("WARNING: PTR record incomplete \n"))
			#bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
			Ptr <<- Ptr + rec_len
			rec_len = 0
		}

		if (rec_len >3) {
			hi_limit = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
			Ptr <<- Ptr + 4
			rec_len = rec_len - 4
		} else {
			#cat(sprintf("WARNING: PTR record incomplete \n"))
			#bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
			Ptr <<- Ptr + rec_len
			rec_len = 0
		}

		if (rec_len >0) {
			str_list = readSTDFstring(rec_len)
			test_nam = str_list$string
			rec_len = str_list$bytes_left
		}
		
		if (rec_len >0) {
			str_list = readSTDFstring(rec_len)
			seq_name = str_list$string
			rec_len = str_list$bytes_left
		}

		if (rec_len >0) {
			str_list = readSTDFstring(rec_len)
			test_txt = str_list$string
			rec_len = str_list$bytes_left
		} else {
			test_txt = test_nam
		}

	} else {
		
		if (rec_len >0) {
			str_list = readSTDFstring(rec_len)
			test_txt = str_list$string
			rec_len = str_list$bytes_left
			Debug_testname <<- test_txt
		}
		
		if (rec_len >0) {
			str_list = readSTDFstring(rec_len)
			alarm_id = str_list$string
			rec_len = str_list$bytes_left
		}
		
		if (rec_len >0) {
			#opt_flag = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
			opt_flag = as.integer(Stdf[Ptr])
			Ptr <<- Ptr + 1
			rec_len = rec_len - 1
			valid_opt_flag = TRUE
		}

		if (rec_len >0) {
			res_scal = readBin(Stdf[Ptr],integer(),n=1,size=1,signed=TRUE)
			Ptr <<- Ptr + 1
			rec_len = rec_len - 1
		}

		if (rec_len >0) {
			llm_scal = readBin(Stdf[Ptr],integer(),n=1,size=1,signed=TRUE)
			Ptr <<- Ptr + 1
			rec_len = rec_len - 1
		}

		if (rec_len >0) {
			hlm_scal = readBin(Stdf[Ptr],integer(),n=1,size=1,signed=TRUE)
			Ptr <<- Ptr + 1
			rec_len = rec_len - 1
		}

		if (rec_len >3) {
			lo_limit = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
			Ptr <<- Ptr + 4
			rec_len = rec_len - 4
		} else {
			#cat(sprintf("WARNING: PTR record incomplete \n"))
			#bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
			Ptr <<- Ptr + rec_len
			rec_len = 0
		}

		if (rec_len >3) {
			hi_limit = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
			Ptr <<- Ptr + 4
			rec_len = rec_len - 4
		} else {
			#cat(sprintf("WARNING: PTR record incomplete \n"))
			#bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
			Ptr <<- Ptr + rec_len
			rec_len = 0
		}

		if (rec_len >0) {
			str_list = readSTDFstring(rec_len)
			units = str_list$string
			rec_len = str_list$bytes_left
		}
		
		if (rec_len >0) {
			str_list = readSTDFstring(rec_len)
			c_resfmt = str_list$string
			rec_len = str_list$bytes_left
		}
		
		if (rec_len >0) {
			str_list = readSTDFstring(rec_len)
			c_llmfmt = str_list$string
			rec_len = str_list$bytes_left
		}
		
		if (rec_len >0) {
			str_list = readSTDFstring(rec_len)
			c_hlmfmt = str_list$string
			rec_len = str_list$bytes_left
		}
		
		if (rec_len >3) {
			lo_spec = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
			Ptr <<- Ptr + 4
			rec_len = rec_len - 4
		} else {
			#cat(sprintf("WARNING: PTR record incomplete \n"))
			#bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
			Ptr <<- Ptr + rec_len
			rec_len = 0
		}

		if (rec_len >3) {
			hi_spec = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
			Ptr <<- Ptr + 4
			rec_len = rec_len - 4
		} else {
			#cat(sprintf("WARNING: PTR record incomplete \n"))
			#bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
			Ptr <<- Ptr + rec_len
			rec_len = 0
		}
	
	}
	
    # throw away any remaining portion of the record we aren't interested in...
    #--------------------------------------------------------------------------
    if (rec_len > 0) {
        cat(sprintf("WARNING: PTR record longer than expected \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } 

    # is this a valid record to update ParametersFrame and ResultsMatrix ?
    #----------------------------------------------------------------------
    if (valid_record) {
        if ( ((as.raw(test_flg)&as.raw(1))==0) || Keep_alarmed_values ) {
            valid_result = TRUE
        } else {
            valid_result = FALSE
        }
        valid_llim = FALSE
        valid_ulim = FALSE
        valid_scale = FALSE
        if (valid_opt_flag) {
            opt_flag = as.raw(opt_flag)   # enable bitwise math...
            #cat(sprintf("DEBUGGING: lo_limit = %f \n",lo_limit))
            if ( ((opt_flag & as.raw(64+16))==0) && is.finite(lo_limit) ) {
                valid_llim = TRUE
            }
            if ( ((opt_flag & as.raw(128+32))==0) && is.finite(hi_limit) ) {
                valid_ulim = TRUE
            }
            if ( ((opt_flag & as.raw(1))==0) && is.finite(res_scal) ) {
                valid_scale = TRUE
            }
        }
    } 

    if (valid_record) {
        # Catalyst: split off function call: stuff after " <> " separator
        if (Executive_type=="Image") {
            chunks = strsplit(test_txt," <> ",fixed=TRUE)
            test_txt = chunks[[1]][1]
        }
        # Fusion: remove "/" at end of test name if it is there
        if (Executive_type=="enVision") {
			# if R14.4.1, now puts < No Name > at end too!
            test_txt = sub("< No Name >$","",test_txt)
			if(Ignore_testname_objects) {
				test_txt = sub("/.*/","",test_txt)
			}
            test_txt = sub("/$","",test_txt)
        }

		# IntegraFlex: remove channel from testname, so testname is consistent between sites
		if (Executive_type=="IntegraFlex" && Auto_flex) {
			test_txt = sub(" [[:digit:]]+[.][[:alpha:]][[:digit:]]+ <>"," <>",test_txt)
		}

		
		# is this a condition rather than a test?  (test_txt starts with CONDITION=
		# .. or on 93k, will be CONDITION:
		#-----------------------------------------
		#if(Do_conditions && str_eq(substr(test_txt,1,10),"CONDITION=")) { 
		if(Do_conditions && ( 
			(substr(test_txt,1,10)=="CONDITION=") || 
			(substr(test_txt,1,10)=="CONDITION:") ) ) { 
		#if(Do_conditions && (length(grep("^CONDITION=",test_txt))>0)) {
			cond_name = sub("^CONDITION.","",test_txt)
			cond_name = sub("/.*$","",cond_name)	# if stuff appended to name, cut (envision .../path)
			cond_name = sub("[[].*$","",cond_name)	# if stuff appended to name, cut (93k ...[1])
			
			# has this condition already been created?
			#-----------------------------------------
			cond_index = match(cond_name,Condition_Names,nomatch=0)
			if (cond_index<1) {
				Conditions_count <<- Conditions_count + 1
				cond_index = Conditions_count
				Condition_Names[Conditions_count] <<- cond_name

				# we added a new condition, so we need to add
				# a new column to Conditions Matrix...
				#---------------------------------------------
				if( sum(dim(ConditionsMatrix))==0) {
					ConditionsMatrix <<- array(NaN,dim=c(0,1))
				} else if ((dim(ConditionsMatrix)[1])<1) {
					ConditionsMatrix <<- array(NaN,dim=c(0,cond_index))
				} else {
					ConditionsMatrix <<- cbind(ConditionsMatrix,NaN)
				}
			}
			# update ConditionsMatrix
			#-------------------------
			device_count = Open_site[site_num+1]
			if (is.finite(ConditionsMatrix[device_count,cond_index])) {
				# Create a new device for changed condition...
				# copy all fields from orig device...
				Device_count <<- Device_count + 1
				Test_order_counter <<- 0
				Open_site[site_num+1] <<- Device_count	
				# as in PIR section, allocate memory in chunks
				if (Device_count>=5) {
					chunk = 200
					if (Device_count==5) {
						Devices_part_id[5:chunk] <<- NA     # <<- as.character(DevicesFrame$part_id)
						Devices_temp[5:chunk] <<- NA        # <<- as.numeric(
						Devices_x_coord[5:chunk] <<- NA     # <<- as.integer(
						Devices_y_coord[5:chunk] <<- NA     # <<- as.integer(
						Devices_wafer_index[5:chunk] <<- NA # <<- as.integer(
						Devices_soft_bin[5:chunk] <<- NA    # <<- as.integer(
						Devices_hard_bin[5:chunk] <<- NA    # <<- as.integer(
						Devices_testtime[5:chunk] <<-NA     # <<- as.numeric(
						Devices_site[5:chunk] <<-NA			# <<- as.numeric(
						Devices_PIR_count[5:chunk] <<-NA	# <<- as.numeric(

						my_dims = dim(ResultsMatrix)
						ResultsMatrix<<- rbind(ResultsMatrix,matrix(NaN,nrow=chunk-4,
                                        ncol=my_dims[2]))
						if(Do_testflag_matrix)  TestFlagMatrix<<- rbind(TestFlagMatrix,matrix(NaN,nrow=chunk-4,
                                        ncol=my_dims[2]))
						if(Do_test_order_matrix)  TestOrderMatrix<<- rbind(TestOrderMatrix,matrix(NaN,nrow=chunk-4,
                                        ncol=my_dims[2]))
						if(Do_mult_limits>0)  MultLimIndexMatrix<<- rbind(MultLimIndexMatrix,matrix(NaN,nrow=chunk-4,
                                        ncol=my_dims[2]))
					} else {
						mod_d = Device_count %% chunk  # modulus
						if(mod_d==0) {
							Devices_part_id[(Device_count+1):(Device_count+chunk)] <<- NA   
							Devices_temp[(Device_count+1):(Device_count+chunk)] <<- NA      
							Devices_x_coord[(Device_count+1):(Device_count+chunk)] <<- NA   
							Devices_y_coord[(Device_count+1):(Device_count+chunk)] <<- NA   
							Devices_wafer_index[(Device_count+1):(Device_count+chunk)] <<- NA 
							Devices_soft_bin[(Device_count+1):(Device_count+chunk)] <<- NA  
							Devices_hard_bin[(Device_count+1):(Device_count+chunk)] <<- NA  
							Devices_testtime[(Device_count+1):(Device_count+chunk)] <<-NA   
							Devices_site[(Device_count+1):(Device_count+chunk)] <<-NA   
							Devices_PIR_count[(Device_count+1):(Device_count+chunk)] <<-NA   

							my_dims = dim(ResultsMatrix)
							ResultsMatrix<<- rbind(ResultsMatrix,matrix(NaN,nrow=chunk,
                                        ncol=my_dims[2]))
							if(Do_testflag_matrix)  TestFlagMatrix<<- rbind(TestFlagMatrix,matrix(NaN,nrow=chunk,
                                        ncol=my_dims[2]))
							if(Do_test_order_matrix)  TestOrderMatrix<<- rbind(TestOrderMatrix,matrix(NaN,nrow=chunk,
                                        ncol=my_dims[2]))
							if(Do_mult_limits>0)  MultLimIndexMatrix<<- rbind(MultLimIndexMatrix,matrix(NaN,nrow=chunk,
                                        ncol=my_dims[2]))
						}
					}
				}
				Devices_part_id[Device_count] <<- Devices_part_id[device_count]
				Devices_temp[Device_count] <<- Devices_temp[device_count]
				Devices_x_coord[Device_count] <<- Devices_x_coord[device_count]
				Devices_y_coord[Device_count] <<- Devices_y_coord[device_count]
				Devices_wafer_index[Device_count] <<- Devices_wafer_index[device_count]
				Devices_soft_bin[Device_count] <<- Devices_soft_bin[device_count]
				Devices_hard_bin[Device_count] <<- Devices_hard_bin[device_count]
				Devices_testtime[Device_count] <<- Devices_testtime[device_count]
				Devices_site[Device_count] <<- site_num
				Devices_PIR_count[Device_count] <<- Devices_PIR_count[device_count]

				ResultsMatrix <<- rbind(ResultsMatrix,NaN)
				if(Do_testflag_matrix)  TestFlagMatrix <<- rbind(TestFlagMatrix,NaN)
				if(Do_test_order_matrix)  TestOrderMatrix <<- rbind(TestOrderMatrix,NaN)
				if(Do_mult_limits>0)  MultLimIndexMatrix <<- rbind(MultLimIndexMatrix,NaN)
				if(Do_DTRs)  DTRsMatrix <<- rbind(DTRsMatrix,NaN)

				ConditionsMatrix <<- rbind(ConditionsMatrix,NaN)

				ConditionsMatrix[Device_count,cond_index] <<- result
			} else {
				ConditionsMatrix[device_count,cond_index] <<- result
			}
		} else if (Do_conditions && (substr(test_txt,1,6)=="SWEEP=")) {  
		#} else if (Do_conditions && str_eq(substr(test_txt,1,6),"SWEEP=")) {  
		#} else if (Do_conditions && (length(grep("^SWEEP=",test_txt))>0)) {
			sweep_name = sub(,"^SWEEP=",test_txt)
		} else {
			if (Duplicate_testnames && (nchar(test_txt)>0)) {
				test_txt = sprintf("%s_%d",test_txt,test_num)
			}

			# has this test already been added to ParametersFrame ?
			#-------------------------------------------------------
			if (Parameter_count>0) {
				if (nchar(test_txt)<1) {
					par_index = match(test_num,Parameters_testnum,nomatch=0)
				} else {
					if ((Previous_param_i < Parameter_count) &&
							(test_txt==Parameters_Names[Previous_param_i+1])) {
						par_index = Previous_param_i+1
						Good_guesses <<- Good_guesses + 1
					} else if ((Another_guess > 0) &&
							(test_txt==Parameters_Names[Another_guess])) {
						par_index = Another_guess
						Good_guesses2 <<- Good_guesses2 + 1
					} else {
						par_index = match(test_txt,Parameters_Names,nomatch=0)
					}
				}
			} else {
				par_index = 0
			}
			if (par_index<1) {
				Parameter_count <<- Parameter_count + 1
				par_index = Parameter_count

				Parameters_testnum[Parameter_count] <<- test_num
				if (nchar(test_txt)<1) {
					Parameters_Names[Parameter_count] <<- ''
				} else {
					Parameters_Names[Parameter_count] <<- test_txt
				}
				if (valid_scale) {
					Parameters_scaler[Parameter_count] <<- res_scal
				} else {
					Parameters_scaler[Parameter_count] <<- 0
				}
				Parameters_units[Parameter_count] <<- units
				if (valid_llim) {
					Parameters_ll[Parameter_count] <<- lo_limit
				} else {
					Parameters_ll[Parameter_count] <<- NaN
				}
				if (valid_ulim) {
					Parameters_ul[Parameter_count] <<- hi_limit
				} else {
					Parameters_ul[Parameter_count] <<- NaN
				}
				Parameters_plot_ll[Parameter_count] <<- NaN
				Parameters_plot_ul[Parameter_count] <<- NaN

				# is the limit "greater or equal", or just "greater"
				# if the parm bit is set, then equal is also a "PASS"
				if( (as.raw(parm_flg) & as.raw(64))>0 ) {
					Parameters_ll_ge[Parameter_count] <<- 1
				} else {
					Parameters_ll_ge[Parameter_count] <<- 0
				}
				if( (as.raw(parm_flg) & as.raw(128))>0 ) {
					Parameters_ul_ge[Parameter_count] <<- 1
				} else {
					Parameters_ul_ge[Parameter_count] <<- 0
				}

				#Parameters_Names <<- as.character(ParametersFrame$testname)

				# we added a new parameter, so we need to add
				# a new column to Results Matrix...
				#---------------------------------------------
				#browser()	# debugging MultLimIndexMatrix stuff...
				ResultsMatrix <<- cbind(ResultsMatrix,NaN)
				if(Do_testflag_matrix)  TestFlagMatrix <<- cbind(TestFlagMatrix,NaN)
				if(Do_test_order_matrix)  TestOrderMatrix <<- cbind(TestOrderMatrix,NaN)
				if(Do_mult_limits>0) {
					MultLimIndexMatrix <<- cbind(MultLimIndexMatrix,NaN)
					MultLim_idx[Parameter_count] <<- 0
					if(sum(dim(MultLim_ll_Matrix))>0) {
						# if we've already created the ll/ul matrices, we will need to grow them
						MultLim_ll_Matrix <<- rbind(MultLim_ll_Matrix,NaN)  
						MultLim_ul_Matrix <<- rbind(MultLim_ul_Matrix,NaN)  
						MultLim_ll_ge_Matrix <<- rbind(MultLim_ll_ge_Matrix,NaN)  
						MultLim_ul_ge_Matrix <<- rbind(MultLim_ul_ge_Matrix,NaN)  
					}
				}
			}
			if (Do_mult_limits>0) {
				if(valid_opt_flag) {
					# put the limit info into 4 new_xxx variables...
					new_ll = lo_limit
					if (!valid_llim)  new_ll = NaN
					new_ul = hi_limit
					if (!valid_ulim)  new_ul = NaN
					new_ll_ge = 0
					if( (as.raw(parm_flg) & as.raw(64))>0 )  new_ll_ge = 1
					new_ul_ge = 0
					if( (as.raw(parm_flg) & as.raw(128))>0 )  new_ul_ge = 1


					# need to check if limits are different from initial limits
					# if if no limit aka NaN, both are NaN
					mult_lim_idx = 0	# assume default limits are valid
					def_lims_ok = TRUE
					if(is.finite(Parameters_ll[par_index]) != is.finite(new_ll)) {
						def_lims_ok = FALSE
					} else if(is.finite(Parameters_ll[par_index]) && is.finite(new_ll) &&
						(Parameters_ll[par_index] != new_ll) ) {
						def_lims_ok = FALSE
					}
					if(is.finite(Parameters_ul[par_index]) != is.finite(new_ul)) {
						def_lims_ok = FALSE
					} else if(is.finite(Parameters_ul[par_index]) && is.finite(new_ul) &&
						(Parameters_ul[par_index] != new_ul) ) {
						def_lims_ok = FALSE
					}
					if(Parameters_ll_ge[par_index] != new_ll_ge)  def_lims_ok = FALSE
					if(Parameters_ul_ge[par_index] != new_ul_ge)  def_lims_ok = FALSE
				} else {
					# no limits in PTR record, implicitly means the limits are the same
					# as the first occurrence of this record
					mult_lim_idx = 0	# assume default limits are valid
					def_lims_ok = TRUE
				}

				if (!def_lims_ok) {
					# ok, now need to look for a match in the mult_lim matrices

					# have we created this matrix yet?
					if(sum(dim(MultLim_ll_Matrix))==0) {
						MultLim_ll_Matrix <<- array(NaN, dim=c(Parameter_count,Do_mult_limits))
						MultLim_ul_Matrix <<- array(NaN, dim=c(Parameter_count,Do_mult_limits))
						MultLim_ll_ge_Matrix <<- array(NaN, dim=c(Parameter_count,Do_mult_limits))
						MultLim_ul_ge_Matrix <<- array(NaN, dim=c(Parameter_count,Do_mult_limits))
					}

					mult_lim_idx = 1
					matching = FALSE
					#step through valid indices until we have a match, or add to end of filled portion of matrices
					# note missing limit is NaN, so need to do extra checking with is.finite()
					while( (mult_lim_idx<=MultLim_idx[par_index]) && !matching ) {
						matching = TRUE
						if(is.finite(new_ll) != is.finite(MultLim_ll_Matrix[par_index,mult_lim_idx]) ) {
							matching = FALSE
						} else if(is.finite(new_ll) && (new_ll != MultLim_ll_Matrix[par_index,mult_lim_idx]) ) {
							matching = FALSE
						}
						if(is.finite(new_ul) != is.finite(MultLim_ul_Matrix[par_index,mult_lim_idx]) ) {
							matching = FALSE
						} else if(is.finite(new_ul) && (new_ul != MultLim_ul_Matrix[par_index,mult_lim_idx]) ) {
							matching = FALSE
						}
						if(new_ll_ge != MultLim_ll_ge_Matrix[par_index,mult_lim_idx])  matching = FALSE 
						if(new_ul_ge != MultLim_ul_ge_Matrix[par_index,mult_lim_idx])  matching = FALSE 

						if(!matching)  mult_lim_idx = mult_lim_idx + 1
					}
					if(!matching) {
						if(mult_lim_idx<Do_mult_limits) {
							# ok, append new limits to end of used part of matrix, increment used counter
							MultLim_idx[par_index] <<- mult_lim_idx
							MultLim_ll_Matrix[par_index,mult_lim_idx] <<- new_ll
							MultLim_ul_Matrix[par_index,mult_lim_idx] <<- new_ul
							MultLim_ll_ge_Matrix[par_index,mult_lim_idx] <<- new_ll_ge
							MultLim_ul_ge_Matrix[par_index,mult_lim_idx] <<- new_ul_ge
						} else {
							# ok, used up all reserved space for limits... 0, 1..(max-1)
							# keep last one for tracking widest limits 

							# REVISIT
						}
					}


				}
			}


			# update ResultsMatrix
			#----------------------
			device_count = Open_site[site_num+1]
			if (valid_result) {
				ResultsMatrix[device_count,par_index] <<- result
			}


			# update TestFlagMatrix
			#-----------------------
			#  0 = pass
			#  1 = pass alternate
			# >1 = fail or not valid
			if(Do_testflag_matrix) {
				testflag = 0
				if( (as.raw(parm_flg)&as.raw(32)) > 0 ) {
					testflag = testflag + 1		# test passed alternate limits
				}
				if( (as.raw(test_flg)&as.raw(128)) > 0 ) {
					testflag = testflag + 2		# test failed
				}
				if( (as.raw(test_flg)&as.raw(64)) > 0 ) {
					testflag = testflag + 4		# pass/fail flag not valid
				}
				TestFlagMatrix[device_count,par_index] <<- testflag
			}


			# update TestOrderMatrix 
			#-----------------------
			if(Do_test_order_matrix) {
				Test_order_counter <<- Test_order_counter + 1
				TestOrderMatrix[device_count,par_index] <<- Test_order_counter
			}


			# update MultLimIndexMatrix 
			#--------------------------
			if(Do_mult_limits>0) {
				MultLimIndexMatrix[device_count,par_index] <<- mult_lim_idx
			}


			Previous_param_i <<- par_index
			Another_guess <<- par_index
	    }
	}
}


#############################################################################
parse_MPR_record <- function(rec_len,endy) {

    # initialize variables
    test_num = 0
    head_num = 0
    site_num = 0
    test_flg = 0
    parm_flg = 0
    rnt_icnt = 0
    rslt_cnt = 0
    rtn_stat = 0
    rtn_rslt = NaN
    test_txt = ''
    alarm_id = ''

    opt_flag = 0
    res_scal = 0
    llm_scal = 0
    hlm_scal = 0
    lo_limit = NaN
    hi_limit = NaN
    start_in = 0.0
    incr_in = 0.0
    rtn_indx = -1
    units = ''
    units_in = ''
    c_resfmt = ''
    c_llmfmt = ''
    c_hlmfmt = ''
    lo_spec = NaN
    hi_spec = NaN


    valid_record = TRUE
    valid_opt_flag = FALSE

    if (rec_len<12) {
        valid_record = FALSE
        cat("WARNING: MPR record shorter than expected \n")
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } else {
        #test_num = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        test_num = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		if(test_num<0)  test_num = test_num + 2^32
        Ptr <<- Ptr + 4
        #head_num = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        head_num = as.integer(Stdf[Ptr])
        #site_num = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        site_num = as.integer(Stdf[Ptr+1])
        #test_flg = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        test_flg = as.integer(Stdf[Ptr+2])
        #parm_flg = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        parm_flg = as.integer(Stdf[Ptr+3])
        Ptr <<- Ptr + 4
        rtn_icnt = readBin(Stdf[Ptr:(Ptr+1)],integer(),n=1,size=2,endian=endy,signed=FALSE)
        rslt_cnt = readBin(Stdf[(Ptr+2):(Ptr+3)],integer(),n=1,size=2,endian=endy,signed=FALSE)
        Ptr <<- Ptr + 4
        rec_len = rec_len - 12
    }
    j=rtn_icnt
    j2 = as.integer(0.6 + j/2)  # 4 bits per element... so j2 bytes for j elements
    k = rslt_cnt
    k4 = k*4
    if ((rslt_cnt<1) && (Executive_type!="93000") ) valid_record = FALSE   # REVISIT... 93K behaviour
    
    if (Debug2b) {
		options(error = recover)
        cat(sprintf("MPR: test_num = %d \n",test_num))
    }
    if (Debug2) {
        cat(sprintf("MPR: head_num = %d \n",head_num))
        cat(sprintf("MPR: site_num = %d \n",site_num))
        cat(sprintf("MPR: test_flg = %d \n",test_flg))
        cat(sprintf("MPR: parm_flg = %d \n",parm_flg))
        cat(sprintf("MPR: rtn_icnt = %d \n",rtn_icnt))
        cat(sprintf("MPR: rslt_cnt = %d \n",rslt_cnt))
        cat(sprintf("MPR: j2 = %d \n",j2))
        cat(sprintf("MPR: k4 = %d \n",k4))
    }

    test_flg = as.raw(test_flg)     # enable bitwise math...
    if ((test_flg & as.raw(64))>0) {
        # is this only true for Credence and/or Quartet MPR records?
        if(!Use_MPR_invalid_pf_data) {
			valid_record = FALSE
			Ptr <<- Ptr + rec_len
			rec_len = 0
		}
    }
    if (rec_len >= j2) {
        rtn_stat = readBin(Stdf[Ptr:(Ptr+j2)],integer(),n=j2,size=1,signed=FALSE)
        Ptr <<- Ptr + j2
        rec_len = rec_len - j2
    } 

    if (rec_len >= k4) {
        rtn_rslt = readBin(Stdf[Ptr:(Ptr+k4)],numeric(),n=k,size=4,endian=endy,signed=TRUE)
        Ptr <<- Ptr + k4
        rec_len = rec_len - k4
    }

    if (rec_len > 0) {
        str_list = readSTDFstring(rec_len)
        test_txt = str_list$string
        rec_len = str_list$bytes_left
        Debug_testname <<- test_txt
    }

    if (Debug2) {
        cat(sprintf("MPR: test_txt = <<%s>> \n",test_txt))
        cat(sprintf("MPR: bytes remaining: %d \n",rec_len))
    }
    
    #if (test_txt == "IIL_CMOSI_") {
    #   browser()
    #}
    if (rec_len > 0) {
        str_list = readSTDFstring(rec_len)
        alarm_id = str_list$string
        rec_len = str_list$bytes_left
    }
    if (Debug2) {
        cat(sprintf("MPR: alarm_id = %s  ... bytes remaining: %d \n",alarm_id,
            rec_len))
    }

    if (rec_len > 0) {
        #opt_flag = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        opt_flag = as.integer(Stdf[Ptr])
        Ptr <<- Ptr + 1
        rec_len = rec_len - 1
        valid_opt_flag = TRUE
    }
    
    if (rec_len > 0) {
        res_scal = readBin(Stdf[Ptr],integer(),n=1,size=1,signed=TRUE)
        Ptr <<- Ptr + 1
        rec_len = rec_len - 1
    }
    
    if (rec_len > 0) {
        llm_scal = readBin(Stdf[Ptr],integer(),n=1,size=1,signed=TRUE)
        Ptr <<- Ptr + 1
        rec_len = rec_len - 1
    }
    
    if (rec_len > 0) {
        hlm_scal = readBin(Stdf[Ptr],integer(),n=1,size=1,signed=TRUE)
        Ptr <<- Ptr + 1
        rec_len = rec_len - 1
    }
 
    if (rec_len >3) {
        lo_limit = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    } else {
        #cat(sprintf("WARNING: MPR record incomplete \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    }

    if (rec_len >3) {
        hi_limit = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    } else {
        #cat(sprintf("WARNING: MPR record incomplete \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    }
    if (Debug2) {
        cat(sprintf("lo_limit = %f  hi_limit = %f  \n",lo_limit,hi_limit))
        cat(sprintf(" ... bytes remaining: %d \n",rec_len))
    }

    if (rec_len >3) {
        start_in = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    } else {
        #cat(sprintf("WARNING: MPR record incomplete \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    }

    if (rec_len >3) {
        incr_in = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    } else {
        #cat(sprintf("WARNING: MPR record incomplete \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    }

    if (rec_len >=(2*j)) {
		if (j>0) {
			rtn_indx = readBin(Stdf[Ptr:(Ptr+(2*j))],integer(),n=j,size=2,endian=endy,signed=FALSE)
			Ptr <<- Ptr + (2*j)
			rec_len = rec_len - (2*j)
		}
    } else {
        #cat(sprintf("WARNING: MPR record incomplete \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    }

    if (rec_len > 0) {
        str_list = readSTDFstring(rec_len)
        units = str_list$string
        rec_len = str_list$bytes_left
    }
    if (Debug2) {
        cat(sprintf("units = %s  \n",units))
        cat(sprintf("MPR: bytes remaining: %d \n",rec_len))
    }


    if (rec_len > 0) {
        str_list = readSTDFstring(rec_len)
        units_in = str_list$string
        rec_len = str_list$bytes_left
    }


    # throw away any remaining portion of the record we aren't interested in...
    #--------------------------------------------------------------------------
    if (rec_len > 0) {
        # REVISIT  cat(sprintf("WARNING: MPR record longer than expected \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } 




    # throw away any remaining portion of the record...
    #--------------------------------------------------------------------------
    if (rec_len > 0) {
        cat(sprintf("WARNING: MPR record longer than expected \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
    } 


    if (valid_record) {
        # doesn't seem to be any per result valid flag... assume all valid.

        valid_llim = FALSE
        valid_ulim = FALSE
        valid_scale = FALSE
        if (valid_opt_flag) {
            opt_flag = as.raw(opt_flag)   # enable bitwise math...
            if ( ((opt_flag & as.raw(64+16))==0) && is.finite(lo_limit) ) {
                valid_llim = TRUE
            }
            if ( ((opt_flag & as.raw(128+32))==0) && is.finite(hi_limit) ) {
                valid_ulim = TRUE
            }
            if ( ((opt_flag & as.raw(1))==0) && is.finite(res_scal) ) {
                valid_scale = TRUE
            }
        }
    }

    if (valid_record && (rslt_cnt<1) ) {
		# 93K pass/fail, but in an MPR instead of an FTR
		device_count = Open_site[site_num+1]
		if (Duplicate_testnames) {
			test_txt = sprintf("%s_%d",test_txt,test_num)
		}
		txt_plus_pin = test_txt
		units = "fails"
	
		# has this test already been added to ParametersFrame?
		#-------------------------------------------------------
		if (Parameter_count>0) {
			if ((Previous_param_i < Parameter_count) &&
					(txt_plus_pin==Parameters_Names[Previous_param_i+1])) {
				Good_guesses <<- Good_guesses + 1
				par_index = Previous_param_i+1
			} else if ((Another_guess > 0) &&
					(txt_plus_pin==Parameters_Names[Another_guess])) {
				par_index = Another_guess
				Good_guesses2 <<- Good_guesses2 + 1
			} else {
				par_index = match(txt_plus_pin,Parameters_Names,nomatch=0)
			}
		} else {
			par_index = 0
		}
		if (par_index<1) {
			# add test to ParametersFrame
			#--------------------------------
			Parameter_count <<- Parameter_count + 1
			par_index = Parameter_count

			Parameters_testnum[par_index] <<- test_num
			Parameters_Names[Parameter_count] <<- txt_plus_pin

			if (valid_scale) {
				Parameters_scaler[par_index] <<- res_scal
			} else {
				Parameters_scaler[par_index] <<- 0
			}
			Parameters_units[par_index] <<- units
			if (valid_llim) {
				Parameters_ll[par_index] <<- lo_limit
			} else {
				Parameters_ll[par_index] <<- NaN
			}
			if (valid_ulim) {
				Parameters_ul[par_index] <<- hi_limit
			} else {
				Parameters_ul[par_index] <<- NaN
			}
			Parameters_plot_ll[par_index] <<- NaN
			Parameters_plot_ul[par_index] <<- NaN

			# is the limit "greater or equal", or just "greater"
			# if the parm bit is set, then equal is also a "PASS"
			if( (as.raw(parm_flg) & as.raw(64))>0 ) {
				Parameters_ll_ge[par_index] <<- 1
			} else {
				Parameters_ll_ge[par_index] <<- 0
			}
			if( (as.raw(parm_flg) & as.raw(128))>0 ) {
				Parameters_ul_ge[par_index] <<- 1
			} else {
				Parameters_ul_ge[par_index] <<- 0
			}

			#Parameters_Names <<- as.character(ParametersFrame$testname)

			# we added a new parameter, so we need to add
			# a new column to Results Matrix...
			#---------------------------------------------
			ResultsMatrix <<- cbind(ResultsMatrix,NaN)
			if(Do_testflag_matrix)  TestFlagMatrix <<- cbind(TestFlagMatrix,NaN)
			if(Do_test_order_matrix)  TestOrderMatrix <<- cbind(TestOrderMatrix,NaN)
			if(Do_mult_limits>0) {
				MultLimIndexMatrix <<- cbind(MultLimIndexMatrix,NaN)
				MultLim_idx[Parameter_count] <<- 0
				if(sum(dim(MultLim_ll_Matrix))>0) {
					# if we've already created the ll/ul matrices, we will need to grow them
					MultLim_ll_Matrix <<- rbind(MultLim_ll_Matrix,NaN)  
					MultLim_ul_Matrix <<- rbind(MultLim_ul_Matrix,NaN)  
					MultLim_ll_ge_Matrix <<- rbind(MultLim_ll_ge_Matrix,NaN)  
					MultLim_ul_ge_Matrix <<- rbind(MultLim_ul_ge_Matrix,NaN)  
				}
			}
		}

		# update ResultsMatrix
		#----------------------
		if (test_flg>0)  failing = 1
		else  failing = 0
		ResultsMatrix[device_count,par_index] <<- failing
		if(Do_testflag_matrix)  TestFlagMatrix[device_count,par_index] <<- failing

		# update TestOrderMatrix
		#------------------------
		Test_order_counter <<- Test_order_counter + 1
		if(Do_test_order_matrix)  TestOrderMatrix[device_count,par_index] <<- Test_order_counter	

		# update MultLim 
		#---------------
		if (Do_mult_limits>0) {
			if (valid_llim) {
				new_ll = lo_limit
			} else {
				new_ll = NaN
			}
			if (valid_ulim) {
				new_ul = hi_limit
			} else {
				new_ul = NaN
			}
			if( (as.raw(parm_flg) & as.raw(64))>0 ) {
				new_ll_ge = 1
			} else {
				new_ll_ge = 0
			}
			if( (as.raw(parm_flg) & as.raw(128))>0 ) {
				new_ul_ge = 1
			} else {
				new_ul_ge = 0
			}
			if(valid_llim || valid_ulim) {
				# explicit limits, check if different than initial limits
				mult_lim_idx = 0
				def_lims_ok = TRUE
				if(is.finite(Parameters_ll[par_index]) != is.finite(new_ll)) {
					def_lims_ok = FALSE
				} else if(is.finite(Parameters_ll[par_index]) && is.finite(new_ll) &&
					(Parameters_ll[par_index] != new_ll) ) {
					def_lims_ok = FALSE
				}
				if(is.finite(Parameters_ul[par_index]) != is.finite(new_ul)) {
					def_lims_ok = FALSE
				} else if(is.finite(Parameters_ul[par_index]) && is.finite(new_ul) &&
					(Parameters_ul[par_index] != new_ul) ) {
					def_lims_ok = FALSE
				}
				if(Parameters_ll_ge[par_index] != new_ll_ge)  def_lims_ok = FALSE
				if(Parameters_ul_ge[par_index] != new_ul_ge)  def_lims_ok = FALSE
			} else {
				# no limits in MPR record, implicitly means the limits are the same
				# as the first occurrence of this record
				mult_lim_idx = 0	# assume default limits are valid
				def_lims_ok = TRUE
			}
			if (!def_lims_ok) {
				# ok, now need to look for a match in the mult_lim matrices

				# have we created this matrix yet?
				if(sum(dim(MultLim_ll_Matrix))==0) {
					MultLim_ll_Matrix <<- array(NaN, dim=c(Parameter_count,Do_mult_limits))
					MultLim_ul_Matrix <<- array(NaN, dim=c(Parameter_count,Do_mult_limits))
					MultLim_ll_ge_Matrix <<- array(NaN, dim=c(Parameter_count,Do_mult_limits))
					MultLim_ul_ge_Matrix <<- array(NaN, dim=c(Parameter_count,Do_mult_limits))
				}

				mult_lim_idx = 1
				matching = FALSE

				#step through valid indices until we have a match, or add to end of filled portion of matrices
				# note missing limit is NaN, so need to do extra checking with is.finite()
				while( (mult_lim_idx<=MultLim_idx[par_index]) && !matching ) {
					matching = TRUE
					if(is.finite(new_ll) != is.finite(MultLim_ll_Matrix[par_index,mult_lim_idx]) ) {
						matching = FALSE
					} else if(is.finite(new_ll) && (new_ll != MultLim_ll_Matrix[par_index,mult_lim_idx]) ) {
						matching = FALSE
					}
					if(is.finite(new_ul) != is.finite(MultLim_ul_Matrix[par_index,mult_lim_idx]) ) {
						matching = FALSE
					} else if(is.finite(new_ul) && (new_ul != MultLim_ul_Matrix[par_index,mult_lim_idx]) ) {
						matching = FALSE
					}
					if(new_ll_ge != MultLim_ll_ge_Matrix[par_index,mult_lim_idx])  matching = FALSE 
					if(new_ul_ge != MultLim_ul_ge_Matrix[par_index,mult_lim_idx])  matching = FALSE 

					if(!matching)  mult_lim_idx = mult_lim_idx + 1
				}
				if(!matching) {
					if(mult_lim_idx<Do_mult_limits) {
						# ok, append new limits to end of used part of matrix, increment used counter
						MultLim_idx[par_index] <<- mult_lim_idx
						MultLim_ll_Matrix[par_index,mult_lim_idx] <<- new_ll
						MultLim_ul_Matrix[par_index,mult_lim_idx] <<- new_ul
						MultLim_ll_ge_Matrix[par_index,mult_lim_idx] <<- new_ll_ge
						MultLim_ul_ge_Matrix[par_index,mult_lim_idx] <<- new_ul_ge
					} else {
						# ok, used up all reserved space for limits... 0, 1..(max-1)
						# keep last one for tracking widest limits 

						# REVISIT
					}
				}
			}
			#...
			MultLimIndexMatrix[device_count,par_index] <<- mult_lim_idx
		}


		Previous_param_i <<- par_index
		Another_guess <<- par_index
	} else if (valid_record) {
        # Catalyst: ... don't have example with MPR, so may not be true...
        # need to split off function call.. after " <> " separator
        #-------------------------------------------------------
        if (Executive_type=="Image") {
            chunks = strsplit(test_txt," <> ",fixed=TRUE)
            test_txt = chunks[[1]][1]
        }

		# is this a condition rather than a test?  (test_txt starts with CONDITION=
		#-----------------------------------------
		#if (Do_conditions && str_eq(substr(test_txt,1,10),"CONDITION=")) {  
		if(Do_conditions && ( 
			(substr(test_txt,1,10)=="CONDITION=") || 
			(substr(test_txt,1,10)=="CONDITION:") ) ) { 
		#if(Do_conditions && (length(grep("^CONDITION=",test_txt))>0)) {
			cond_name = sub("^CONDITION.","",test_txt)
			cond_name = sub("/.*$","",cond_name)	# if stuff appended to name, cut (envision .../path)
			cond_name = sub("[[].*$","",cond_name)	# if stuff appended to name, cut (93k ...[1])

			if (rslt_cnt>1) {
				cat(sprintf("WARNING: MPR CONDITION=%s record has more than 1 result \n",
							cond_name))
			}
			# has this condition already been created?
			#-----------------------------------------
			cond_index = match(cond_name,Condition_Names,nomatch=0)
			if (cond_index<1) {
				Conditions_count <<- Conditions_count + 1
				cond_index = Conditions_count
				Condition_Names[Conditions_count] <<- cond_name

				# we added a new condition, so we need to add
				# a new column to Conditions Matrix...
				#---------------------------------------------
				if( sum(dim(ConditionsMatrix))==0) {
					ConditionsMatrix <<- array(NaN,dim=c(0,1))
				} else if ((dim(ConditionsMatrix)[1])<1) {
					ConditionsMatrix <<- array(NaN,dim=c(0,cond_index))
				} else {
					ConditionsMatrix <<- cbind(ConditionsMatrix,NaN)
				}
			}
			# update ConditionsMatrix
			#-------------------------
			device_count = Open_site[site_num+1]
			if (is.finite(ConditionsMatrix[device_count,cond_index])) {
				# Create a new device for changed condition...
				# copy all fields from orig device...
				Device_count <<- Device_count + 1
				Test_order_counter <<- 0
				Open_site[site_num+1] <<- Device_count	
				# as in PIR section, allocate memory in chunks
				if (Device_count>=5) {
					chunk = 200
					if (Device_count==5) {
						Devices_part_id[5:chunk] <<- NA     # <<- as.character(DevicesFrame$part_id)
						Devices_temp[5:chunk] <<- NA        # <<- as.numeric(
						Devices_x_coord[5:chunk] <<- NA     # <<- as.integer(
						Devices_y_coord[5:chunk] <<- NA     # <<- as.integer(
						Devices_wafer_index[5:chunk] <<- NA # <<- as.integer(
						Devices_soft_bin[5:chunk] <<- NA    # <<- as.integer(
						Devices_hard_bin[5:chunk] <<- NA    # <<- as.integer(
						Devices_testtime[5:chunk] <<-NA     # <<- as.numeric(
						Devices_site[5:chunk] <<-NA			# <<- as.numeric(
						Devices_PIR_count[5:chunk] <<-NA	# <<- as.numeric(

						my_dims = dim(ResultsMatrix)
						ResultsMatrix<<- rbind(ResultsMatrix,matrix(NaN,nrow=chunk-4,
                                        ncol=my_dims[2]))
						if(Do_testflag_matrix)  TestFlagMatrix<<- rbind(TestFlagMatrix,matrix(NaN,nrow=chunk-4,
                                        ncol=my_dims[2]))
						if(Do_test_order_matrix)  TestOrderMatrix<<- rbind(TestOrderMatrix,matrix(NaN,nrow=chunk-4,
                                        ncol=my_dims[2]))
						if(Do_mult_limits>0)  MultLimIndexMatrix<<- rbind(MultLimIndexMatrix,matrix(NaN,nrow=chunk-4,
                                        ncol=my_dims[2]))
					} else {
						mod_d = Device_count %% chunk  # modulus
						if(mod_d==0) {
							Devices_part_id[(Device_count+1):(Device_count+chunk)] <<- NA   
							Devices_temp[(Device_count+1):(Device_count+chunk)] <<- NA      
							Devices_x_coord[(Device_count+1):(Device_count+chunk)] <<- NA   
							Devices_y_coord[(Device_count+1):(Device_count+chunk)] <<- NA   
							Devices_wafer_index[(Device_count+1):(Device_count+chunk)] <<- NA 
							Devices_soft_bin[(Device_count+1):(Device_count+chunk)] <<- NA  
							Devices_hard_bin[(Device_count+1):(Device_count+chunk)] <<- NA  
							Devices_testtime[(Device_count+1):(Device_count+chunk)] <<-NA   
							Devices_site[(Device_count+1):(Device_count+chunk)] <<-NA   
							Devices_PIR_count[(Device_count+1):(Device_count+chunk)] <<-NA   

							my_dims = dim(ResultsMatrix)
							ResultsMatrix<<- rbind(ResultsMatrix,matrix(NaN,nrow=chunk,
                                        ncol=my_dims[2]))
							if(Do_testflag_matrix)  TestFlagMatrix<<- rbind(TestFlagMatrix,matrix(NaN,nrow=chunk,
                                        ncol=my_dims[2]))
							if(Do_test_order_matrix)  TestOrderMatrix<<- rbind(TestOrderMatrix,matrix(NaN,nrow=chunk,
                                        ncol=my_dims[2]))
							if(Do_mult_limits>0)  MultLimIndexMatrix<<- rbind(MultLimIndexMatrix,matrix(NaN,nrow=chunk,
                                        ncol=my_dims[2]))
						}
					}
				}
				Devices_part_id[Device_count] <<- Devices_part_id[device_count]
				Devices_temp[Device_count] <<- Devices_temp[device_count]
				Devices_x_coord[Device_count] <<- Devices_x_coord[device_count]
				Devices_y_coord[Device_count] <<- Devices_y_coord[device_count]
				Devices_wafer_index[Device_count] <<- Devices_wafer_index[device_count]
				Devices_soft_bin[Device_count] <<- Devices_soft_bin[device_count]
				Devices_hard_bin[Device_count] <<- Devices_hard_bin[device_count]
				Devices_testtime[Device_count] <<- Devices_testtime[device_count]
				Devices_site[Device_count] <<- site_num
				Devices_PIR_count[Device_count] <<- Devices_PIR_count[device_count]

				ResultsMatrix <<- rbind(ResultsMatrix,NaN)
				if(Do_testflag_matrix)  TestFlagMatrix <<- rbind(TestFlagMatrix,NaN)
				if(Do_test_order_matrix)  TestOrderMatrix <<- rbind(TestOrderMatrix,NaN)
				if(Do_mult_limits>0)  MultLimIndexMatrix <<- rbind(MultLimIndexMatrix,NaN)
				if(Do_DTRs)  DTRsMatrix <<- rbind(DTRsMatrix,NaN)
				ConditionsMatrix <<- rbind(ConditionsMatrix,NaN)

				ConditionsMatrix[Device_count,cond_index] <<- rtn_rslt[1]
			} else {
				ConditionsMatrix[device_count,cond_index] <<- rtn_rslt[1]
			}
		} else if (Do_conditions && (substr(test_txt,1,6)=="SWEEP=")) {  
			sweep_name = sub(,"^SWEEP=",test_txt)
		} else {
			if (Duplicate_testnames) {
				test_txt = sprintf("%s_%d",test_txt,test_num)
			}
			# if 2nd or later occurence of MPR, it may not contain pinnames...
			if(rtn_indx[1]<0) {
				if (nchar(test_txt)<1) {
					mpr_index = match(test_num,MPR_testnums,nomatch=0)
				} else {
					mpr_index = match(test_txt,MPR_testnames,nomatch=0)
				}
				#browser()
				if((mpr_index>0) && (test_num==MPR_testnums[mpr_index]) ) {
					i1 = MPR_pins_start[mpr_index]
					i2 = i1 + MPR_pins_length[mpr_index] - 1
					rtn_indx = MPR_all_pins[i1:i2]

					if (nchar(test_txt)<1) {
						test_txt = MPR_testnames[mpr_index]
					}
				}
			}

			# for each result in the MPR, there will be a separate ParametersFrame entry
			# test_txt + "/" + pin_name
			# or test_txt + "/" no_pin
			#-------------------------------------------------------------------
			device_count = Open_site[site_num+1]
			if(Do_testflag_matrix) {
				#  0 = pass
				#  1 = pass alternate
				# >1 = fail or not valid
				testflag = 0
				if( (as.raw(parm_flg)&as.raw(32)) > 0 ) {
					testflag = testflag + 1		# test passed alternate limits
				}
				if( (as.raw(test_flg)&as.raw(128)) > 0 ) {
					testflag = testflag + 2		# test failed
				}
				if( (as.raw(test_flg)&as.raw(64)) > 0 ) {
					testflag = testflag + 4		# pass/fail flag not valid
				}
			}
			for (j in 1:rslt_cnt) {
				if (rtn_icnt>=1) {
					if ((length(rtn_indx)<j) || (rtn_indx[j] <1)) {
						if(rslt_cnt<2) {
							txt_plus_pin = test_txt
						} else {
							txt_plus_pin = sprintf("%s/no_pin%d",test_txt,j)
						}
					} else {
						txt_plus_pin = sprintf("%s/%s",test_txt,Pin_names[rtn_indx[j]])
					}
				} else {
					if(rslt_cnt<2) {
						txt_plus_pin = test_txt
					} else {
						txt_plus_pin = sprintf("%s/no_pin%d",test_txt,j)
					}
				}
				
				# has this test already been added to ParametersFrame?
				#-------------------------------------------------------
				if (Parameter_count>0) {
					if ((Previous_param_i < Parameter_count) &&
							(txt_plus_pin==Parameters_Names[Previous_param_i+1])) {
						Good_guesses <<- Good_guesses + 1
						par_index = Previous_param_i+1
					} else if ((Another_guess > 0) &&
							(txt_plus_pin==Parameters_Names[Another_guess])) {
						par_index = Another_guess
						Good_guesses2 <<- Good_guesses2 + 1
					} else {
						par_index = match(txt_plus_pin,Parameters_Names,nomatch=0)
					}
				} else {
					par_index = 0
				}

				if (par_index<1) {
					# add MPR to MPR list, along with pinlist
					if ((j==1) && (rtn_indx[1]>0)) {
						MPR_count <<- MPR_count + 1
						MPR_testnames[MPR_count] <<- test_txt
						MPR_testnums[MPR_count] <<- test_num
						if(MPR_count==1)	i1=0
						else				i1 = length(MPR_all_pins)
						i2 = length(rtn_indx)
						MPR_pins_length[MPR_count] <<- i2
						MPR_pins_start[MPR_count] <<- i1+1
						MPR_all_pins[(i1+1):(i1+i2)] <<- rtn_indx
						#browser()
					}

					# add test to ParametersFrame
					#--------------------------------
					Parameter_count <<- Parameter_count + 1
					par_index = Parameter_count

					Parameters_testnum[par_index] <<- test_num
					Parameters_Names[Parameter_count] <<- txt_plus_pin

					if (valid_scale) {
						Parameters_scaler[par_index] <<- res_scal
					} else {
						Parameters_scaler[par_index] <<- 0
					}
					Parameters_units[par_index] <<- units
					if (valid_llim) {
						Parameters_ll[par_index] <<- lo_limit
					} else {
						Parameters_ll[par_index] <<- NaN
					}
					if (valid_ulim) {
						Parameters_ul[par_index] <<- hi_limit
					} else {
						Parameters_ul[par_index] <<- NaN
					}
					Parameters_plot_ll[par_index] <<- NaN
					Parameters_plot_ul[par_index] <<- NaN

					# is the limit "greater or equal", or just "greater"
					# if the parm bit is set, then equal is also a "PASS"
					if( (as.raw(parm_flg) & as.raw(64))>0 ) {
						Parameters_ll_ge[par_index] <<- 1
					} else {
						Parameters_ll_ge[par_index] <<- 0
					}
					if( (as.raw(parm_flg) & as.raw(128))>0 ) {
						Parameters_ul_ge[par_index] <<- 1
					} else {
						Parameters_ul_ge[par_index] <<- 0
					}

					#Parameters_Names <<- as.character(ParametersFrame$testname)

					# we added a new parameter, so we need to add
					# a new column to Results Matrix...
					#---------------------------------------------
					ResultsMatrix <<- cbind(ResultsMatrix,NaN)
					if(Do_testflag_matrix)  TestFlagMatrix <<- cbind(TestFlagMatrix,NaN)
					if(Do_test_order_matrix)  TestOrderMatrix <<- cbind(TestOrderMatrix,NaN)
					if(Do_mult_limits>0) {
						MultLimIndexMatrix <<- cbind(MultLimIndexMatrix,NaN)
						MultLim_idx[Parameter_count] <<- 0
						if(sum(dim(MultLim_ll_Matrix))>0) {
							# if we've already created the ll/ul matrices, we will need to grow them
							MultLim_ll_Matrix <<- rbind(MultLim_ll_Matrix,NaN)  
							MultLim_ul_Matrix <<- rbind(MultLim_ul_Matrix,NaN)  
							MultLim_ll_ge_Matrix <<- rbind(MultLim_ll_ge_Matrix,NaN)  
							MultLim_ul_ge_Matrix <<- rbind(MultLim_ul_ge_Matrix,NaN)  
						}
					}
				}

				# update ResultsMatrix
				#----------------------
				ResultsMatrix[device_count,par_index] <<- rtn_rslt[j]
				if(Do_testflag_matrix)  TestFlagMatrix[device_count,par_index] <<- testflag
				Test_order_counter <<- Test_order_counter + 1
				if(Do_test_order_matrix)  TestOrderMatrix[device_count,par_index] <<- Test_order_counter


				# update MultLimIndexMatrix 
				#--------------------------
				if(Do_mult_limits>0) {
					if (valid_llim) {
						new_ll = lo_limit
					} else {
						new_ll = NaN
					}
					if (valid_ulim) {
						new_ul = hi_limit
					} else {
						new_ul = NaN
					}
					if( (as.raw(parm_flg) & as.raw(64))>0 ) {
						new_ll_ge = 1
					} else {
						new_ll_ge = 0
					}
					if( (as.raw(parm_flg) & as.raw(128))>0 ) {
						new_ul_ge = 1
					} else {
						new_ul_ge = 0
					}
					if(valid_llim || valid_ulim) {
						# explicit limits, check if different than initial limits
						mult_lim_idx = 0
						def_lims_ok = TRUE
						if(is.finite(Parameters_ll[par_index]) != is.finite(new_ll)) {
							def_lims_ok = FALSE
						} else if(is.finite(Parameters_ll[par_index]) && is.finite(new_ll) &&
							(Parameters_ll[par_index] != new_ll) ) {
							def_lims_ok = FALSE
						}
						if(is.finite(Parameters_ul[par_index]) != is.finite(new_ul)) {
							def_lims_ok = FALSE
						} else if(is.finite(Parameters_ul[par_index]) && is.finite(new_ul) &&
							(Parameters_ul[par_index] != new_ul) ) {
							def_lims_ok = FALSE
						}
						if(Parameters_ll_ge[par_index] != new_ll_ge)  def_lims_ok = FALSE
						if(Parameters_ul_ge[par_index] != new_ul_ge)  def_lims_ok = FALSE
					} else {
						# no limits in MPR record, implicitly means the limits are the same
						# as the first occurrence of this record
						mult_lim_idx = 0	# assume default limits are valid
						def_lims_ok = TRUE
					}
					if (!def_lims_ok) {
						# ok, now need to look for a match in the mult_lim matrices

						# have we created this matrix yet?
						if(sum(dim(MultLim_ll_Matrix))==0) {
							MultLim_ll_Matrix <<- array(NaN, dim=c(Parameter_count,Do_mult_limits))
							MultLim_ul_Matrix <<- array(NaN, dim=c(Parameter_count,Do_mult_limits))
							MultLim_ll_ge_Matrix <<- array(NaN, dim=c(Parameter_count,Do_mult_limits))
							MultLim_ul_ge_Matrix <<- array(NaN, dim=c(Parameter_count,Do_mult_limits))
						}

						mult_lim_idx = 1
						matching = FALSE
						#step through valid indices until we have a match, or add to end of filled portion of matrices
						# note missing limit is NaN, so need to do extra checking with is.finite()
						while( (mult_lim_idx<=MultLim_idx[par_index]) && !matching ) {
							matching = TRUE
							if(is.finite(new_ll) != is.finite(MultLim_ll_Matrix[par_index,mult_lim_idx]) ) {
								matching = FALSE
							} else if(is.finite(new_ll) && (new_ll != MultLim_ll_Matrix[par_index,mult_lim_idx]) ) {
								matching = FALSE
							}
							if(is.finite(new_ul) != is.finite(MultLim_ul_Matrix[par_index,mult_lim_idx]) ) {
								matching = FALSE
							} else if(is.finite(new_ul) && (new_ul != MultLim_ul_Matrix[par_index,mult_lim_idx]) ) {
								matching = FALSE
							}
							if(new_ll_ge != MultLim_ll_ge_Matrix[par_index,mult_lim_idx])  matching = FALSE 
							if(new_ul_ge != MultLim_ul_ge_Matrix[par_index,mult_lim_idx])  matching = FALSE 

							if(!matching)  mult_lim_idx = mult_lim_idx + 1
						}
						if(!matching) {
							if(mult_lim_idx<Do_mult_limits) {
								# ok, append new limits to end of used part of matrix, increment used counter
								MultLim_idx[par_index] <<- mult_lim_idx
								MultLim_ll_Matrix[par_index,mult_lim_idx] <<- new_ll
								MultLim_ul_Matrix[par_index,mult_lim_idx] <<- new_ul
								MultLim_ll_ge_Matrix[par_index,mult_lim_idx] <<- new_ll_ge
								MultLim_ul_ge_Matrix[par_index,mult_lim_idx] <<- new_ul_ge
							} else {
								# ok, used up all reserved space for limits... 0, 1..(max-1)
								# keep last one for tracking widest limits 

								# REVISIT
							}
						}


					}


					MultLimIndexMatrix[device_count,par_index] <<- mult_lim_idx
				}


				Previous_param_i <<- par_index
				if (j==1)  Another_guess <<- par_index
			}
		}
   }
    
}



#############################################################################
parse_FDR_record <- function(rec_len,endy) {

	# initialize variables -- STDF v3 only
    test_num = 0
	desc_flg = 0
	test_nam = ""
	seq_name = ""

    valid_record = TRUE

    if (rec_len<5) {
        valid_record = FALSE
        cat("WARNING: FDR record shorter than expected \n")
        Ptr <<- Ptr + rec_len
        rec_len = 0
	} else {
        #test_num = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        test_num = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		if(test_num<0)  test_num = test_num + 2^32
        desc_flg = as.integer(Stdf[Ptr+4])
        Ptr <<- Ptr + 5
        rec_len = rec_len - 5
	}

    if (rec_len > 0) {
        str_list = readSTDFstring(rec_len)
        test_nam = str_list$string
        rec_len = str_list$bytes_left
    }

    if (rec_len > 0) {
        str_list = readSTDFstring(rec_len)
        seq_name = str_list$string
        rec_len = str_list$bytes_left
    }
	
    # throw away any remaining portion of the record...
    #--------------------------------------------------------------------------
    if (rec_len > 0) {
        cat(sprintf("WARNING: FDR record longer than expected \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } 

    if (valid_record) {
		if (Duplicate_testnames && (nchar(test_txt)>0)) {
			test_txt = sprintf("%s_%d",test_txt,test_num)
		}
		test_txt = test_nam
		
        # has this test already been added to ParametersFrame ?
        #-------------------------------------------------------
        if (Parameter_count>0) {
            if (nchar(test_txt)<1) {
                par_index = match(test_num,Parameters_testnum,nomatch=0)
            } else {
				if ((Previous_param_i < Parameter_count) &&
						(test_txt==Parameters_Names[Previous_param_i+1])) {
					par_index = Previous_param_i+1
					Good_guesses <<- Good_guesses + 1
				} else {
					par_index = match(test_txt,Parameters_Names,nomatch=0)
				}
            }
        } else {
            par_index = 0
        }
        if (par_index<1) {
            Parameter_count <<- Parameter_count + 1
            par_index = Parameter_count

            Parameters_testnum[Parameter_count] <<- test_num
            if (nchar(test_txt)<1) {
                Parameters_Names[Parameter_count] <<- ''
            } else {
                Parameters_Names[Parameter_count] <<- test_txt
            }
            Parameters_scaler[Parameter_count] <<- 0
            Parameters_units[Parameter_count] <<- 'fails'
            Parameters_ll[Parameter_count] <<- NaN
            #Parameters_ul[Parameter_count] <<- NaN
            Parameters_ul[Parameter_count] <<- 0.5		# seems like a sensible thing to do
            Parameters_plot_ll[Parameter_count] <<- NaN
            Parameters_plot_ul[Parameter_count] <<- NaN

			Parameters_ll_ge[Parameter_count] <<- 0
			Parameters_ul_ge[Parameter_count] <<- 0

            #Parameters_Names <<- as.character(ParametersFrame$testname)

            # we added a new parameter, so we need to add
            # a new column to Results Matrix...
            #---------------------------------------------
            ResultsMatrix <<- cbind(ResultsMatrix,NaN)
            if(Do_testflag_matrix)  TestFlagMatrix <<- cbind(TestFlagMatrix,NaN)
            if(Do_test_order_matrix)  TestOrderMatrix <<- cbind(TestOrderMatrix,NaN)
            if(Do_mult_limits>0)  MultLimIndexMatrix <<- cbind(MultLimIndexMatrix,NaN)
        }

		Previous_param_i <<- par_index
	}
}


#############################################################################
parse_FTR_record <- function(rec_len,endy) {

    # initialize variables
    test_num = 0
    head_num = 0
    site_num = 0
    test_flg = NaN
    opt_flag = 0
    cycl_cnt = 0
    rel_vadr = 0
    rept_cnt = 0
    num_fail = 0
    xfail_ad = 0
    yfail_ad = 0
    vect_off = 0
    rtn_icnt = 0
    pgm_icnt = 0
    rtn_indx = 0
    rtn_stat = 0
    pgm_indx = 0
    pgm_stat = 0
    fail_pin = 0
    vect_nam = ''
    time_set = ''
    op_code = ''
    test_txt = ''
    alarm_id = ''
    prog_txt = ''
    rslt_txt = ''
    patg_num = 0
    spin_map = 0

	# stdf version 3 fields
	desc_flg = 0
	vect_adr = 0
	pcp_addr = 0
	vect_dat = 0
	dev_data = 0
	rpin_map = 0
	test_nam = ""
	seq_name = ""


    valid_record = TRUE

    if (rec_len<7) {
        valid_record = FALSE
        cat("WARNING: FTR record shorter than expected \n")
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } else {
        #test_num = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        test_num = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		if(test_num<0)  test_num = test_num + 2^32
        Ptr <<- Ptr + 4
        #head_num = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        head_num = as.integer(Stdf[Ptr])
        #site_num = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        site_num = as.integer(Stdf[Ptr+1])
        #test_flg = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        test_flg = as.integer(Stdf[Ptr+2])
        Ptr <<- Ptr + 3
        rec_len = rec_len - 7
    }

	if ((Stdf_Version==3) && (rec_len>0)) {
        desc_flg = as.integer(Stdf[Ptr])
        Ptr <<- Ptr + 1
        rec_len = rec_len - 1
	}

    if (rec_len > 0) {
        #opt_flag = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
        opt_flag = as.integer(Stdf[Ptr])
        Ptr <<- Ptr + 1
        rec_len = rec_len - 1
    } 

	if ((Stdf_Version==3) && (rec_len>0)) {
        time_set = as.integer(Stdf[Ptr])
        Ptr <<- Ptr + 1
        rec_len = rec_len - 1
	}

	if ((Stdf_Version==3) && (rec_len>3)) {
        #vect_adr = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        vect_adr = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		if(vect_adr<0)  vect_adr = vect_adr + 2^32
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    } 

	if (rec_len > 3) {
        #cycl_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        cycl_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		if(cycl_cnt<0)  cycl_cnt = cycl_cnt + 2^32
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    } 

    if ((Stdf_Version!=3) && (rec_len>3)) {
        #rel_vadr = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        rel_vadr = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		if(rel_vadr<0)  rel_vadr = rel_vadr + 2^32
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    } 

    if ((Stdf_Version!=3) && (rec_len>3)) {
        #rept_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        rept_cnt = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		if(rept_cnt<0)  rept_cnt = rept_cnt + 2^32
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    } else if ((Stdf_Version==3) && (rec_len>1)) {
        rept_cnt = readBin(Stdf[Ptr:(Ptr+1)],integer(),n=1,size=2,endian=endy,signed=FALSE)
        Ptr <<- Ptr + 2
        rec_len = rec_len - 2
	}

    if ((Stdf_Version==3) && (rec_len>1)) {
        pcp_addr = readBin(Stdf[Ptr:(Ptr+1)],integer(),n=1,size=2,endian=endy,signed=FALSE)
        Ptr <<- Ptr + 2
        rec_len = rec_len - 2
	}

    if (rec_len > 3) {
        #num_fail = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        num_fail = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy)
		if(num_fail<0)  num_fail = num_fail + 2^32
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    } 

	if (Stdf_Version==3) {
	    if (rec_len > 1) {
	        raw_list = readSTDFbits(rec_len,endy)
	        fail_pin = raw_list$raw
	        rec_len = raw_list$bytes_left
	    }
		
	    if (rec_len > 1) {
	        raw_list = readSTDFbits(rec_len,endy)
	        vect_dat = raw_list$raw
	        rec_len = raw_list$bytes_left
	    }

	    if (rec_len > 1) {
	        raw_list = readSTDFbits(rec_len,endy)
	        dev_data = raw_list$raw
	        rec_len = raw_list$bytes_left
	    }

	    if (rec_len > 1) {
	        raw_list = readSTDFbits(rec_len,endy)
	        rpin_map = raw_list$raw
	        rec_len = raw_list$bytes_left
	    }

	    if (rec_len > 0) {
	        str_list = readSTDFstring(rec_len)
	        test_nam = str_list$string
	        rec_len = str_list$bytes_left
	    }

	    if (rec_len > 0) {
	        str_list = readSTDFstring(rec_len)
	        seq_name = str_list$string
	        rec_len = str_list$bytes_left
	    }

	    if (rec_len > 0) {
	        str_list = readSTDFstring(rec_len)
	        test_txt = str_list$string
	        rec_len = str_list$bytes_left
	    }

		#... mmmhh, is this what I really want to do?
		test_txt = test_nam
		
	} else {
	    if (rec_len > 3) {
	        xfail_ad = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=TRUE)
	        Ptr <<- Ptr + 4
	        rec_len = rec_len - 4
	    } 
	
	    if (rec_len > 3) {
	        yfail_ad = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=TRUE)
	        Ptr <<- Ptr + 4
	        rec_len = rec_len - 4
	    } 
	
	    if (rec_len > 1) {
	        vect_off = readBin(Stdf[Ptr:(Ptr+1)],integer(),n=1,size=2,endian=endy,signed=TRUE)
	        Ptr <<- Ptr + 2
	        rec_len = rec_len - 2
	    } 
	
	    if (rec_len > 1) {
	        rtn_icnt = readBin(Stdf[Ptr:(Ptr+1)],integer(),n=1,size=2,endian=endy,signed=FALSE)
	        Ptr <<- Ptr + 2
	        rec_len = rec_len - 2
	    } 
	
	    if (rec_len > 1) {
	        pgm_icnt = readBin(Stdf[Ptr:(Ptr+1)],integer(),n=1,size=2,endian=endy,signed=FALSE)
	        Ptr <<- Ptr + 2
	        rec_len = rec_len - 2
	    } 
	
	    j = rtn_icnt
	    k = pgm_icnt
	    j2 = as.integer(0.5001 + j/2)
	    k2 = as.integer(0.5001 + k/2)
	
	    if ( (j>0) && (rec_len >= 2*j)) {
	        rtn_indx = readBin(Stdf[Ptr:(Ptr+(2*j))],integer(),n=j,size=2,endian=endy,signed=FALSE)
	        Ptr <<- Ptr + (2*j)
	        rec_len = rec_len - (2*j)
	    } 
	
	    if ( (j2>0) && (rec_len >= j2) ) {
	        rtn_stat = readBin(Stdf[Ptr:(Ptr+j2)],integer(),n=j2,size=1,signed=FALSE)
	        Ptr <<- Ptr + j2
	        rec_len = rec_len - j2
	    } 
	
	    if ( (k>0) && (rec_len >= 2*k) ) {
	        pgm_indx = readBin(Stdf[Ptr:(Ptr+(2*k))],integer(),n=k,size=2,endian=endy,signed=FALSE)
	        Ptr <<- Ptr + (2*k)
	        rec_len = rec_len - (2*k)
	    } 
	
	    if ( (k2>0) && (rec_len >= k2) ) {
	        pgm_stat = readBin(Stdf[Ptr:(Ptr+k2)],integer(),n=k2,size=1,signed=FALSE)
	        Ptr <<- Ptr + k2
	        rec_len = rec_len - k2
	    } 
	
	    if (Debug4) {
	        browser()
	    }
	
	    if (rec_len > 1) {
			#browser()
	        raw_list = readSTDFbits(rec_len,endy)
	        fail_pin = raw_list$raw
	        rec_len = raw_list$bytes_left

			if ((Executive_type=="93000") && (num_fail>0) && (rec_len>0)) {
				#.. if next byte is zero, wipe it
				#.. some versions of 6 and 7 have extra byte if pattern fails and # of pins modulo 8 = 0
				# vect_nam is always present, so next byte should be >0 unless it is above Smartest bug.
				# WAIT... an example smartest 6.5.4 doesn't have vect_nam, so can be length 0!
		        vect_nam_len = as.integer(Stdf[Ptr])
				if ( (grepl("7.1.",Executive_version,fixed=TRUE)[1]) && (vect_name_len == 0) ) { 
					#browser()
					part_num = Open_site[site_num+1]
					cat(sprintf("... WARNING: part_num %d : zapping byte in FTR record... \n",part_num))
		        	Ptr <<- Ptr + 1
		        	rec_len = rec_len - 1
				}
			}
	    }
	
	    if (rec_len > 0) {
	        str_list = readSTDFstring(rec_len)
	        vect_nam = str_list$string
	        rec_len = str_list$bytes_left
	    }
	
	    if (rec_len > 0) {
	        str_list = readSTDFstring(rec_len)
	        time_set = str_list$string
	        rec_len = str_list$bytes_left
	    }
	
	    if (rec_len > 0) {
	        str_list = readSTDFstring(rec_len)
	        op_code = str_list$string
	        rec_len = str_list$bytes_left
	    }
	
	    if (rec_len > 0) {
	        str_list = readSTDFstring(rec_len)
	        test_txt = str_list$string
	        rec_len = str_list$bytes_left
	        Debug_testname <<- test_txt
	    }
	
	    if (rec_len > 0) {
	        str_list = readSTDFstring(rec_len)
	        alarm_id = str_list$string
	        rec_len = str_list$bytes_left
	    }
	
	    if (rec_len > 0) {
	        str_list = readSTDFstring(rec_len)
	        prog_txt = str_list$string
	        rec_len = str_list$bytes_left
	    }
	
	    if (rec_len > 0) {
	        str_list = readSTDFstring(rec_len)
	        rslt_txt = str_list$string
	        rec_len = str_list$bytes_left
	    }
	
	    if (rec_len > 0) {
	        #patg_num = readBin(STDF,integer(),n=1,size=1,signed=FALSE)
	        patg_num = as.integer(Stdf[Ptr])
	        Ptr <<- Ptr + 1
	        rec_len = rec_len - 1
	    } 
	
	    if (rec_len > 1) {
	        raw_list = readSTDFbits(rec_len,endy)
	        spin_map = raw_list$raw
	        rec_len = raw_list$bytes_left
	    }
	}

    # throw away any remaining portion of the record...
    #--------------------------------------------------------------------------
    if (rec_len > 0) {
        cat(sprintf("WARNING: FTR record longer than expected \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } 

    
    if (valid_record) {

        # tidy up test_txt if required
        #------------------------------
        if (nchar(test_txt)>=1) {
                # Catalyst:
                # need to split off function call.. after " <> " separator
                # OR
                # Fusion:
                # need to split off pin after "/" separator
                #-------------------------------------------------------
                if (Executive_type=="Image") {
                    chunks = strsplit(test_txt," <> ",fixed=TRUE)
                    test_txt = chunks[[1]][1]
                }
                if (Executive_type=="enVision") {
                    # need to strip off "/" and following...
                    chunks = strsplit(test_txt,"/")
                    test_txt = chunks[[1]][1]
                    # if (length(chunks[[1]])>1)  pinname = chunks[[1]][2]
                }
        }

      	if (Duplicate_testnames && (nchar(test_txt)>0)) {
			test_txt = sprintf("%s_%d",test_txt,test_num)
		}

		# has this test already been added to ParametersFrame ?
        #-------------------------------------------------------
        if (Parameter_count>0) {
            if (nchar(test_txt)<1) {
                par_index = match(test_num,Parameters_testnum,nomatch=0)
            } else {
				if ((Previous_param_i < Parameter_count) &&
						(test_txt==Parameters_Names[Previous_param_i+1])) {
					par_index = Previous_param_i+1
					Good_guesses <<- Good_guesses + 1
			} else if ((Another_guess > 0) &&
					(test_txt==Parameters_Names[Another_guess])) {
				par_index = Another_guess
				Good_guesses2 <<- Good_guesses2 + 1
				} else {
					par_index = match(test_txt,Parameters_Names,nomatch=0)
				}
            }
        } else {
            par_index = 0
        }
        if (par_index<1) {
            Parameter_count <<- Parameter_count + 1
            par_index = Parameter_count

            Parameters_testnum[Parameter_count] <<- test_num
            if (nchar(test_txt)<1) {
                Parameters_Names[Parameter_count] <<- ''
            } else {
                Parameters_Names[Parameter_count] <<- test_txt
            }
            Parameters_scaler[Parameter_count] <<- 0
			if(Do_FTR_fail_cycle) {
				Parameters_units[Parameter_count] <<- 'fail_cycle'
				Parameters_ll[Parameter_count] <<- NaN
				Parameters_ul[Parameter_count] <<- -0.5		# seems like a sensible thing to do
			} else {
				Parameters_units[Parameter_count] <<- 'fails'
				Parameters_ll[Parameter_count] <<- NaN
				#Parameters_ul[Parameter_count] <<- NaN
				Parameters_ul[Parameter_count] <<- 0.5		# seems like a sensible thing to do
			}
            Parameters_plot_ll[Parameter_count] <<- NaN
            Parameters_plot_ul[Parameter_count] <<- NaN

			Parameters_ll_ge[Parameter_count] <<- 0
			Parameters_ul_ge[Parameter_count] <<- 0

            #Parameters_Names <<- as.character(ParametersFrame$testname)

            # we added a new parameter, so we need to add
            # a new column to Results Matrix...
            #---------------------------------------------
            ResultsMatrix <<- cbind(ResultsMatrix,NaN)
            if(Do_testflag_matrix)  TestFlagMatrix <<- cbind(TestFlagMatrix,NaN)
            if(Do_test_order_matrix)  TestOrderMatrix <<- cbind(TestOrderMatrix,NaN)
            if(Do_mult_limits>0) {
				MultLimIndexMatrix <<- cbind(MultLimIndexMatrix,NaN)
				MultLim_idx[Parameter_count] <<- 0
				if(sum(dim(MultLim_ll_Matrix))>0) {
					# if we've already created the ll/ul matrices, we will need to grow them
					MultLim_ll_Matrix <<- rbind(MultLim_ll_Matrix,NaN)  
					MultLim_ul_Matrix <<- rbind(MultLim_ul_Matrix,NaN)  
					MultLim_ll_ge_Matrix <<- rbind(MultLim_ll_ge_Matrix,NaN)  
					MultLim_ul_ge_Matrix <<- rbind(MultLim_ul_ge_Matrix,NaN)  
				}
			}
        }

        # update ResultsMatrix
        #----------------------
		device_count = Open_site[site_num+1]
		if(Do_FTR_fail_cycle) {
			if(test_flg>0) {
				failing_cycle = cycl_cnt
			} else {
				failing_cycle = -1
			}
        	ResultsMatrix[device_count,par_index] <<- failing_cycle
		} else {
        	ResultsMatrix[device_count,par_index] <<- test_flg
		}

		# update TestFlagMatrix
		#-----------------------
        if(Do_testflag_matrix) {
			testflag = 0
			if( (as.raw(test_flg)&as.raw(128)) > 0 ) {
				testflag = testflag + 2		# test failed
			}
			if( (as.raw(test_flg)&as.raw(64)) > 0 ) {
				testflag = testflag + 4		# pass/fail flag not valid
			}
			TestFlagMatrix[device_count,par_index] <<- testflag
		}

		# update TestOrderMatrix 
		#-----------------------
		if(Do_test_order_matrix) {
			Test_order_counter <<- Test_order_counter + 1
			TestOrderMatrix[device_count,par_index] <<- Test_order_counter
		}

		# update MultLimIndexMatrix 
		#--------------------------
		if(Do_mult_limits>0) {
			# FTR limits are fixed... so not too complicated here!
			mult_lim_idx = 0
			MultLimIndexMatrix[device_count,par_index] <<- mult_lim_idx
		}

		Previous_param_i <<- par_index
		Another_guess <<- par_index

    }

}


#############################################################################
parse_DTR_record <- function(rec_len,endy) {

	text_dat = ''
	
	if (rec_len >0) {
		str_list = readSTDFstring(rec_len)
		text_dat = str_list$string
		rec_len = str_list$bytes_left
	}

    # throw away any remaining portion of the record we aren't interested in...
    #--------------------------------------------------------------------------
    if (rec_len > 0) {
        cat(sprintf("WARNING: DTR record longer than expected \n"))
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } 

	# DTR_Names
	# DTR_Names_count
	# DTRsMatrix
	
	# check if this is a special/custom DTR that we want to trigger on...
	# the below code is geared towards per-device DTR information
	valid_dtr = 0
#	if(substr(text_dat,1,9)=="DEVICEID=") {
#		# grab the string of hexadecimal characters following the = sign, 
#		#    add to DevicesFrame as device_id,
#		# .. PARTID=nnnn  would also be somewhere in the string?
#		#    this is a little more convoluted to search for than just site number.
#		# also, we don't get the partid per site until the PRR record which
#		# hasn't happened yet.
#		my_site = 1		# REVISIT!!!!
#		dtr_info = sub("DEVICEID=([0-9a-fA-F]+).*$","\\1",text_dat)
#		dtr_name = "device_id"
#		valid_dtr = 1
#	} else 
	if(substr(text_dat,1,13)=="TRACEABILITY[") {
		# TRACEABILITY[n]:sss:sss:xxxxx:nnnnnnnnn;
		#    extract [n] site number, needed to determine which device/site
		#    this applies to for multisite
		#    extract the last nnnnnnn, add to DevicesFrame as trace_id
		site_num = as.integer(sub("TRACEABILITY\\[([0-9]+)\\].*$","\\1",text_dat))
		#tokens = strsplit(text_dat,":")[[1]]
		#dtr_info = tokens[length(tokens)]
		if (Executive_type=="enVision") {	
			if ( length(grep("NOT_READ",text_dat))>0 ) {
				dtr_info = ""
			} else {
				dtr_info = sub("^[^:]+:([^;]+);.*$","\\1",text_dat)
			}
		} else {
			dtr_info = sub("^.+:([0-9a-fA-F]+);$","\\1",text_dat)
		}
		dtr_name = "trace_id"
		valid_dtr = 1
		#browser()
	} else if(substr(text_dat,1,14)=="LASER_MARKING[") {
		# LASER_MARKING[n]: ssss: field_name laser_string
		#   extract [n] site number
		#   extract field_name
		#   extract laser_string
		site_num = as.integer(sub("LASER_MARKING\\[([0-9]+)\\].*$","\\1",text_dat))

		# split text string on white spaces
		tokens = strsplit(text_dat,"\\s+")[[1]]       

		# field_name into dtr_name
		dtr_name = tokens[length(tokens)-1]

		# laser string into dtr_info
		dtr_info = tokens[length(tokens)]
		#dtr_info = sub("^.+ ([0-9a-fA-F]+)\\s*$","\\1",text_dat)

		valid_dtr = 1
		#browser()
 	}

	if(valid_dtr) {
		dtr_index = match(dtr_name,DTR_Names,nomatch=0)
		if(dtr_index<1) {
			DTR_Names_count <<- DTR_Names_count + 1
			dtr_index = DTR_Names_count
			DTR_Names[DTR_Names_count] <<- dtr_name

			# we added a new dtr_name, so we need to add
			# a new column to DTRsMatrix...
			#---------------------------------------------
			if( sum(dim(DTRsMatrix))==0) {
				DTRsMatrix <<- array(NaN,dim=c(0,1))
			} else if ((dim(DTRsMatrix)[1])<1) {
				DTRsMatrix <<- array(NaN,dim=c(0,dtr_index))
			} else {
				DTRsMatrix <<- cbind(DTRsMatrix,NaN)
			}
		}

		# update DTRsMatrix
		#-------------------
		device_count = Open_site[site_num+1]
		#cat(sprintf("at device %d, dtr_index %d, adding %s...\n",
		#		device_count,dtr_index,dtr_info))
		#cat(sprintf("...DTRsMatrix size is %d by %d\n",
		#		dim(DTRsMatrix)[1],dim(DTRsMatrix)[2] ))
		DTRsMatrix[device_count,dtr_index] <<- dtr_info
	}
}



#############################################################################
#  copy local functions to the .ConvertStdf.env and remove them
#  from the global environment
#############################################################################
	environment(valid_record_type)<-.ConvertStdf.env
	environment(print_summary)<-.ConvertStdf.env
	environment(readSTDFstring)<-.ConvertStdf.env
	environment(readSTDFbits)<-.ConvertStdf.env
	environment(parse_stdf_record)<-.ConvertStdf.env
	environment(parse_FAR_record)<-.ConvertStdf.env
	environment(unknown_record)<-.ConvertStdf.env
	environment(ignore_STDF_record)<-.ConvertStdf.env
	environment(parse_MIR_record)<-.ConvertStdf.env
	environment(parse_MRR_record)<-.ConvertStdf.env
	environment(parse_SDR_record)<-.ConvertStdf.env
	environment(parse_PMR_record)<-.ConvertStdf.env
	environment(parse_WCR_record)<-.ConvertStdf.env
	environment(parse_WIR_record)<-.ConvertStdf.env
	environment(parse_WRR_record)<-.ConvertStdf.env
	environment(parse_PIR_record)<-.ConvertStdf.env
	environment(parse_PRR_record)<-.ConvertStdf.env
	environment(parse_TSR_record)<-.ConvertStdf.env
	environment(parse_HBR_record)<-.ConvertStdf.env
	environment(parse_SBR_record)<-.ConvertStdf.env
	environment(parse_PTR_record)<-.ConvertStdf.env
	environment(parse_MPR_record)<-.ConvertStdf.env
	environment(parse_FTR_record)<-.ConvertStdf.env
	environment(parse_DTR_record)<-.ConvertStdf.env
	environment(str_eq)<-.ConvertStdf.env

assign("valid_record_type",valid_record_type,envir=.ConvertStdf.env)
rm(valid_record_type)

assign("print_summary",print_summary,envir=.ConvertStdf.env)
rm(print_summary)

assign("readSTDFstring",readSTDFstring,envir=.ConvertStdf.env)
rm(readSTDFstring)

assign("readSTDFbits",readSTDFbits,envir=.ConvertStdf.env)
rm(readSTDFbits)

assign("parse_stdf_record",parse_stdf_record,envir=.ConvertStdf.env)
rm(parse_stdf_record)

assign("parse_FAR_record",parse_FAR_record,envir=.ConvertStdf.env)
rm(parse_FAR_record)

assign("unknown_record",unknown_record,envir=.ConvertStdf.env)
rm(unknown_record)

assign("ignore_STDF_record",ignore_STDF_record,envir=.ConvertStdf.env)
rm(ignore_STDF_record)

assign("parse_MIR_record",parse_MIR_record,envir=.ConvertStdf.env)
rm(parse_MIR_record)

assign("parse_MRR_record",parse_MRR_record,envir=.ConvertStdf.env)
rm(parse_MRR_record)

assign("parse_SDR_record",parse_SDR_record,envir=.ConvertStdf.env)
rm(parse_SDR_record)

assign("parse_PMR_record",parse_PMR_record,envir=.ConvertStdf.env)
rm(parse_PMR_record)

assign("parse_WCR_record",parse_WCR_record,envir=.ConvertStdf.env)
rm(parse_WCR_record)

assign("parse_WIR_record",parse_WIR_record,envir=.ConvertStdf.env)
rm(parse_WIR_record)

assign("parse_WRR_record",parse_WRR_record,envir=.ConvertStdf.env)
rm(parse_WRR_record)

assign("parse_PIR_record",parse_PIR_record,envir=.ConvertStdf.env)
rm(parse_PIR_record)

assign("parse_PRR_record",parse_PRR_record,envir=.ConvertStdf.env)
rm(parse_PRR_record)

assign("parse_TSR_record",parse_TSR_record,envir=.ConvertStdf.env)
rm(parse_TSR_record)

assign("parse_HBR_record",parse_HBR_record,envir=.ConvertStdf.env)
rm(parse_HBR_record)

assign("parse_SBR_record",parse_SBR_record,envir=.ConvertStdf.env)
rm(parse_SBR_record)

assign("parse_PDR_record",parse_PDR_record,envir=.ConvertStdf.env)
rm(parse_PDR_record)

assign("parse_PTR_record",parse_PTR_record,envir=.ConvertStdf.env)
rm(parse_PTR_record)

assign("parse_MPR_record",parse_MPR_record,envir=.ConvertStdf.env)
rm(parse_MPR_record)

assign("parse_FDR_record",parse_FDR_record,envir=.ConvertStdf.env)
rm(parse_FDR_record)

assign("parse_FTR_record",parse_FTR_record,envir=.ConvertStdf.env)
rm(parse_FTR_record)

assign("parse_DTR_record",parse_DTR_record,envir=.ConvertStdf.env)
rm(parse_DTR_record)

assign("str_eq",str_eq,envir=.ConvertStdf.env)
rm(str_eq)


environment(ConvertStdf)<-.ConvertStdf.env

