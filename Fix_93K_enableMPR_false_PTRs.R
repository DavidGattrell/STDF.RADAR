# Fix_93K_enableMPR_false_PTRs.R
#
# $Id: Fix_93K_enableMPR_false_PTRs.R,v 1.1 2022/08/24 00:58:15 david Exp $
#
# script that parses a rtdf file that was converted from a 93K 7.3.x version tester
# which had the enableMPR flag set to false.  In this case, the limits are missing
# from 2nd+ pin PTRs from the 'flattened' MPRs.  This script looks for sequentical
# tests that contain the '#' character in their testnames and the string up to this
# character are identical.  It then copies the earlier test's limits to the following
# test
#
# Copyright (C) 2022 David Gattrell
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
#-----------------------------------------------
Fix_93K_enableMPR_false_PTRs <- function(in_file="",out_file="",in_dir="",verbose=FALSE) {

    # in_file -- string of rtdf file name to process
    # out_file -- string of rtdf file name to write to
    #             if out_file is left empty, overwrite in_file
	# in_dir -- absolute path for in_file if different
	#           than current directory.
	#-----------------------------------------------------------

	if(out_file == "") {
		#out_file = sub("(.rtdf)?(.Rtdf)?$","_mpr.rtdf",in_file)
		out_file = in_file
	}


	if (in_dir != "") {
		my_dir = getwd()
		setwd(in_dir)
	}
    my_objs = load(in_file)
	if (in_dir != "")  setwd(my_dir)
	

	# find testnames that contain the '#' character
	param_count = dim(ParametersFrame)[1]
	testnames = as.character(ParametersFrame[["testname"]])
	suspect_indices = grep("#",testnames,fixed=TRUE)

	updated_tests = 0
	for (i in 2:length(suspect_indices)) {
		#... start at 2nd occurrence, since at least the first should be sane
		# does previous test match for all the chars up to the #?
		common_str = sub("#(.+)$","#",testnames[suspect_indices[i]])
		if( length(grep(common_str,testnames[suspect_indices[i]-1],fixed=TRUE))>0 ) {
			updated_tests = updated_tests + 1

			# yes, found a match!
			# now copy scaler, units, ll, ul, ll_ge, ul_ge, plot_ll and plot_ul values to current test
			ParametersFrame[suspect_indices[i],"scaler"] = ParametersFrame[suspect_indices[i]-1,"scaler"]
			ParametersFrame[suspect_indices[i],"units"] = ParametersFrame[suspect_indices[i]-1,"units"]
			ParametersFrame[suspect_indices[i],"ll"] = ParametersFrame[suspect_indices[i]-1,"ll"]
			ParametersFrame[suspect_indices[i],"ul"] = ParametersFrame[suspect_indices[i]-1,"ul"]
			ParametersFrame[suspect_indices[i],"ll_ge"] = ParametersFrame[suspect_indices[i]-1,"ll_ge"]
			ParametersFrame[suspect_indices[i],"ul_ge"] = ParametersFrame[suspect_indices[i]-1,"ul_ge"]
			ParametersFrame[suspect_indices[i],"plot_ll"] = ParametersFrame[suspect_indices[i]-1,"plot_ll"]
			ParametersFrame[suspect_indices[i],"plot_ul"] = ParametersFrame[suspect_indices[i]-1,"plot_ul"]
			# replace the '#" with a "/"
			ParametersFrame[suspect_indices[i],"testname"] = sub("#","/",testnames[suspect_indices[i]])

			# if previous test is the first of MPR, need to fix its "#" to "/" also
			ParametersFrame[suspect_indices[i]-1,"testname"] = sub("#","/",testnames[suspect_indices[i]-1])

			if(verbose) {
				cat(sprintf(".. testname %s now becomes %s \n",
						testnames[suspect_indices[i]],
						ParametersFrame[suspect_indices[i],"testname"]))
			}
		}
	}
	cat(sprintf("%d Parameters have been updated \n",updated_tests))


	save(list=my_objs,file=out_file)

}

