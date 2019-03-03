# SplitBySubstr.R
#
# $Id: SplitBySubstr.R,v 1.2 2019/02/01 02:12:09 david Exp $
#
# script for separating data by conditions that are coded into the
# parameter names.
# ie.  you could have a test occurring 3 times
#		Timing_test
#		Timing_test_Vmin
#		Timing_test_Vmax
# the script would create 3 files, each with a parameter Timing_test,
# the first one having the Timing_test data, the next having the _Vmin
# data and the third having the Vmax data.
# 
# Copyright (C) 2010 David Gattrell
#               2018 David Gattrell
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
SplitBySubstr <- function(in_file="",substrs="",out_file="",in_dir="") {

	# in_file -- string of rtdf filename to process
	# substrs -- vector of substrings to use for sorting
	# out_file -- name of the output file.  If it is empty, name
	#			  will be based on in_file
	# in_dir -- directory to file in_file if in_file is not in cwd
	# -------------------------------------------------------------

    if (in_file == "") {
		in_file <- readline("Enter the name of the RTDF file to read: ")
    }

    # strip off .rtdf/.Rtdf/.Rdata from out_file if it is there
    #--------------------------------------------------
    if (out_file == "") {
		out_file = in_file
    }
    if (regexpr("[.]Rdata$",out_file)>0) {
		out_file = as.character(strsplit(out_file,"[.]Rdata$"))
    } else if (regexpr("[.]Rtdf$",out_file)>0) {
		out_file = as.character(strsplit(out_file,"[.]Rtdf$"))
    } else if (regexpr("[.]rtdf$",out_file)>0) {
		out_file = as.character(strsplit(out_file,"[.]rtdf$"))
    }


    my_objs = load(in_file)

	tests = dim(ParametersFrame)[1]
	cat(sprintf("... read in %d parameters \n",tests))
	# any testnames that don't match any of the conditions will
	# by put in the first condition file...
	splitting = rep(1,tests)

	testnames = as.character(ParametersFrame[["testname"]])
	new_testnames = testnames

	conditions = length(substrs)
	for (i in 1:conditions) {
		if(nchar(substrs[i])>0) {
			#grep_str = sprintf("_%s_|_%s$",substrs[i],substrs[i])
			grep_str = sprintf("_%s[^0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]|_%s$",
								substrs[i],substrs[i])
			indices = grep(grep_str,testnames)
			if(length(indices)>0) {
				sub_str = sprintf("_%s",substrs[i])
				new_testnames[indices] = sub(sub_str,"",testnames[indices])
				testnames[indices] = ""
				splitting[indices] = i
			}
		}
	}

	
	# ok, now create the split rtdf files
	#---------------------------------------
	OrigResultsMatrix = ResultsMatrix
	if(match("TestFlagMatrix",my_objs,nomatch=0)) {
		OrigTestFlagMatrix = TestFlagMatrix
	}
	ParametersFrame["testname"] = new_testnames
	OrigParametersFrame = ParametersFrame
	for (i in 1:conditions) {
		keepers = which(splitting==i)
		if(length(keepers)>0) {
			ParametersFrame = OrigParametersFrame[keepers,]
			ResultsMatrix = OrigResultsMatrix[,keepers]
			if(match("TestFlagMatrix",my_objs,nomatch=0)) {
				TestFlagMatrix = OrigTestFlagMatrix[,keepers]
			}
			out_filename = sprintf("%s_%s.rtdf",out_file,substrs[i])
			cat(sprintf("... now writing %d parameters to %s \n",
					length(keepers),out_filename))
			save(list=my_objs, file=out_filename)
		}
	}

}

