# FindFirstFails.R
#
# $Id: FindFirstFails.R,v 1.1 2013/10/06 21:35:24 david Exp $
#
# generates a "first_fail_test" field in the DevicesFrame based on
# either the TestFlagMatrix object or, if not present, the ResultsMatrix
# values vs. the ll and ul from the ParametersFrame
#
# part of RADAR scripts, see sites.google.com/site/stdfradar
#
# Copyright (C) 2013 David Gattrell
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
FindFirstFails <- function(in_file="",out_file="",force_update=FALSE,
					force_use_limits=FALSE,add_fails_count=TRUE,
					in_dir="") {

    # in_file  - RTDF file to look in
	# out_file - the name of the RTDF file to create (unless action
	#            is "report", in which case this is ignored)
	# force_update - by default, if the RTDF file already contains a field 
	#            in the DevicesFrame called "first_fail_test", this 
	#            script wouldn't change anything.  To force it to
	#            recalculate "first_fail_test", set this to TRUE
	# force_use_limits - by default, if there is a TestFlagMatrix,
	#            it will be used to determine first failing test.
	#            If you want to always use the limits from the Parameters
	#            Frame, set this flag to TRUE
	# add_fails_count - also add a field to the DevicesFrame that
	#            includes the number of tests that fail (maybe useful
	#            when datalogging in continue-on-fail mode)
	# in_dir    - directory to look for in_file if not same as
	#             pwd.
    #----------------------------------


	# load rtdf file into memory
	#---------------------------
	if (in_dir != "") {
		my_dir = getwd()
		setwd(in_dir)
	}
    my_objs = load(in_file)
	if (in_dir != "")  setwd(my_dir)



	# check if "first_fail_test" already exists in DevicesFrame
	update = force_update
	if( !("first_fail_test" %in% colnames(DevicesFrame))) {
		update = TRUE
	} 

	# check if TestFlagMatrix exists
	if( exists("TestFlagMatrix",inherits=FALSE) ) {
		test_flag_matrix_found = TRUE
	} else {
		test_flag_matrix_found = FALSE
	}


	part_count = dim(DevicesFrame)[1]
	first_fail_test = rep("",part_count)
	fail_test_count = rep(0,part_count)

	if(update) {
		if( test_flag_matrix_found && !force_use_limits ) {
			cat("Checking devices against testflags.\n")
			for (i in 1:part_count) {
				fail_tests = which(TestFlagMatrix[i,]==2)
				if(length(fail_tests)>0) {
					# check to see if device is a pass (sbin/hbin = 1?)
					# if so, non-gating tests remove from lists
					first_fail_test[i] = ParametersFrame[[fail_tests[1],"testname"]]
					fail_test_count[i] = length(fail_tests)
				} else {
					# check to see if device is a fail
					# 
					#first_fail_test[i] = ""
				}
			}
		} else {
			if(!test_flag_matrix_found)  cat("No TestFlagMatrix object found!\n")
			cat("Checking measurements against limits.\n")

			lls = as.numeric(ParametersFrame[["ll"]])
			uls = as.numeric(ParametersFrame[["ul"]])
			# if units == "fails", FTR derived, so assume ul of 0.5
			ftr_tests = which(ParametersFrame[["units"]]=="fails")
			uls[ftr_tests] = 0.5

			for (i in 1:part_count) {
				ll_fails = which( (ResultsMatrix[i,]<lls) == TRUE )
				ul_fails = which( (ResultsMatrix[i,]>uls) == TRUE )
				fail_tests = sort( c(ll_fails,ul_fails) )

				if(length(fail_tests)>0) {
					first_fail_test[i] = ParametersFrame[[fail_tests[1],"testname"]]
					fail_test_count[i] = length(fail_tests)
				} else {
				}
			}
		}

		# now append first_fail_test to DevicesFrame...
		DevicesFrame[["first_fail_test"]] = first_fail_test
		if(add_fails_count) {
			DevicesFrame[["fail_test_count"]] = fail_test_count
		}
	}

	# save to rtdf file...
	#---------------------
	if (out_file=="") {
		out_file = in_file
	}
	save(list=my_objs,file=out_file)
}

