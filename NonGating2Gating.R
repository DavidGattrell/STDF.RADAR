# NonGating2Gating.R
#
# $Id: NonGating2Gating.R,v 1.2 2020/02/17 21:51:09 david Exp $
#
# Updates DevicesFrame sbin/hbin if device was a pass, but fails specified non-gating test
# If no testname is supplied, it will dump list of tests that look like they are non-gating
#
# part of RADAR scripts, see sites.google.com/site/stdfradar
#
# Copyright (C) 2019 David Gattrell
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
NonGating2Gating <- function(in_file="",out_file="",testname="",sbin_num=0,sbin_name="",
						hbin_num=3,hbin_name="MixFAIL",force_use_limits=FALSE,
						verbose=TRUE,in_dir="") {
    # in_file  - RTDF file to look in
	# out_file - the name of the RTDF file to create 
	# testname - the non-gating test that you want to now make gating, or,
	#            if "", list out any tests that appear to be non-gating
	#            (tests that fail for pass bin devices)
	# sbin_num - the failing soft bin number to use for failures for testname
	# sbin_name - the failing soft bin name to use for failures for testname
	# hbin_num - the failing hard bin number to use for failures for testname
	# hbin_name - the failing hard bin number to use for failures for testname
	# force_use_limits - if there is a TestFlagMatrix, it will be used to 
	#            determine pass/fail.  If it does not exist, or this flag is true,
	#            it will use the limits in the ParametersFrame to determine pass/fail.
	# verbose -  print # of devices rebinned to the screen
	# in_dir -   directory to look for in_file if not same as pwd
	#-------------------------------------------------


	# load rtdf file into memory
	#---------------------------
	if (in_dir != "") {
		my_dir = getwd()
		setwd(in_dir)
	}
    my_objs = load(in_file)
	if (in_dir != "")  setwd(my_dir)


	if(match("TestFlagMatrix",my_objs,nomatch=0)) {
		use_limits = FALSE
	} else {
		use_limits = TRUE
	}
	if(force_use_limits)  use_limits = TRUE

	# names(ParametersFrame)
	# "testnum" "testname" "scaler" "units" "ll" "ul" "ll_ge" ul_ge" "plot_ll" plot_ul"


	# testname = "scan_chain"	# debugging example
	testnames = as.character(ParametersFrame[["testname"]])

	
	# determine which are the existing passing devices
	#-------------------------------------------------
	# assuming HbinInfoFrame exists...
	hbin_pf_flags = as.character(HbinInfoFrame[["hbin_pf"]])
	if(is.finite(match("P",hbin_pf_flags))) {
		pass_hbins = as.integer(HbinInfoFrame["hbin_num"][HbinInfoFrame["hbin_pf"]=="P"])
	}
	device_hbins = as.integer(DevicesFrame[,"hard_bin"])
	passing_devices = which(device_hbins %in% pass_hbins)


	if (testname=="") {
		# REVISIT
		# - search for tests that appear to be non-gating...
		cat("Looking for tests that appear to be non-gating...\n")
		cat("(have failures even for passing devices)\n")
		if(use_limits) {
			for(i in 1:dim(ResultsMatrix)[2]) {
				pass_part_results = ResultsMatrix[passing_devices,i]
				# now check against LL and UL...
			}
		} else {
			for(i in 1:dim(TestFlagMatrix)[2]) {
				test_flags = TestFlagMatrix[passing_devices,i]
				if(2 %in% test_flags) {
					cat(sprintf("    idx = %d, %s\n",i,ParametersFrame[[i,"testname"]]))
				}
			}
			cat("... done looking\n")
		}
		# TestFlagMatrix[passing_devices,]

	} else {
		idx = grep(testname,testnames,fixed=TRUE)  # don't want to interpret :@[] stuff in testnames!
		if(length(idx)<1) {
			cat(sprintf("ERROR, testname %s not found in RTDF file!\n",testname))
		} else {
			if(length(idx)>1) {
				cat(sprintf("WARNING, more than one match for testname %s in RTDF file!\n",
					testname))
				# maybe print off the first 3?
			}

			# ok, we found the alleged non-gating test, when does it fail?
			#-------------------------------------------------------------
			if(is.finite(match("TestFlagMatrix",my_objs))) {
				test_flag_matrix_found = TRUE
			} else {
				test_flag_matrix_found = FALSE
			}

			if( test_flag_matrix_found && !force_use_limits ) {
				test_fail_devices = which(TestFlagMatrix[,idx]==2)
			} else {
				# REVISIT
				cat("SORRY: didn't code this section yet!  (NonGating2Gating.R)\n")
				cat("... using ParametersFrame limits to detect fails  (NonGating2Gating.R)\n")
			}

			# which devices were PASS devices but actually failed the non-gating test?
			new_fail_devices = intersect(passing_devices,test_fail_devices)

			if(verbose) {
				cat(sprintf("%d devices change from PASS to FAIL\n",length(new_fail_devices)))
			}

			if(length(new_fail_devices)>0) {
				DevicesFrame[new_fail_devices,"soft_bin"] = sbin_num
				DevicesFrame[new_fail_devices,"hard_bin"] = hbin_num

				if(is.finite(match("SbinInfoFrame",my_objs))) {
					sbin_names = as.character(SbinInfoFrame[,"sbin_nam"])
					# if newly gating test soft bin isn't in frame, add it 
					if(!is.finite(match(sbin_name,sbin_names))) {
						SbinInfoFrame = rbind(SbinInfoFrame,list(
							sbin_num=sbin_num,sbin_cnt=length(new_fail_devices),
							sbin_pf="F",sbin_nam=sbin_name))

					}
					# clear sbin counts and
					# regenerate sbin counts from updated DevicesFrame
					SbinInfoFrame[["sbin_cnt"]] = 0
					sbin_nums = as.integer(SbinInfoFrame[,"sbin_num"])
					new_device_sbins = as.integer(DevicesFrame[["soft_bin"]])
					new_sbin_counts_frame = as.data.frame(table(new_device_sbins))
					unique_sbins = as.numeric(levels(new_sbin_counts_frame[,1]))
					new_counts = new_sbin_counts_frame[,2]
					for (i in 1:length(new_counts)) {
						sbin_num = unique_sbins[i]
						idx = match(sbin_num,sbin_nums)
						if(is.finite(idx)) {
							SbinInfoFrame[idx,"sbin_cnt"] = new_counts[i]
						} else {
							cat(sprintf("WARNING: sbin %d from DeviceFrame is NOT in SbinInfoFrame!\n",sbin_num))
						}
					}
				}

				if(is.finite(match("HbinInfoFrame",my_objs))) {
					hbin_names = as.character(HbinInfoFrame[,"hbin_nam"])
					# if newly gating test hard bin isn't in frame, add it 
					if(!is.finite(match(hbin_name,hbin_names))) {
						HbinInfoFrame = rbind(HbinInfoFrame,list(
							hbin_num=hbin_num,hbin_cnt=length(new_fail_devices),
							hbin_pf="F",hbin_nam=hbin_name))
					}
					# clear hbin counts and
					# regenerate hbin counts from updated DevicesFrame
					HbinInfoFrame[["hbin_cnt"]] = 0
					hbin_nums = as.integer(HbinInfoFrame[,"hbin_num"])
					new_device_hbins = as.integer(DevicesFrame[["hard_bin"]])
					new_hbin_counts_frame = as.data.frame(table(new_device_hbins))
					unique_hbins = as.numeric(levels(new_hbin_counts_frame[,1]))
					new_counts = new_hbin_counts_frame[,2]
					for (i in 1:length(new_counts)) {
						hbin_num = unique_hbins[i]
						idx = match(hbin_num,hbin_nums)
						if(is.finite(idx)) {
							HbinInfoFrame[idx,"hbin_cnt"] = new_counts[i]
						} else {
							cat(sprintf("WARNING: hbin %d from DeviceFrame is NOT in HbinInfoFrame!\n",hbin_num))
						}
					}
				}

				
				# generate automatic output file name if none provided
				if(out_file=="") {
					out_file = sub("(.rtdf)?(.Rtdf)?$","_ng2g.rtdf",in_file)
				}

				save(list=my_objs,file=out_file)
			}

		}
	}
	



}
