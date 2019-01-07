# ReplaceTestnames.R
#
# $Id: ReplaceTestnames.R,v 1.1 2009/09/09 00:47:09 David Exp $
#
# Script that replaces testnames in an RTDF file with those found in another
# RTDF file based on matching the test numbers.
# Typically used for J9 data where the stdf version doesn't have the pinname 
# information that can be extracted from ascii version.  
#
# Copyright (C) 2009 David Gattrell
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
#----------------------------------------------------------
ReplaceTestnames <- function(in_file="",reference_file="",out_file="",
						only_matches=TRUE,ref_dir="",in_dir="") {

	# in_file -- string for filename of rtdf file to change.  
	#
	# reference_file -- an rtdf file with the desired testnames for each test
	#				number as found in the in_file
	#
	# only_matches -- if true, remove tests that do not exist in reference
	#				file, else keep all tests
	#
	# ref_dir -- if reference_file is in a different directory, this is the
	#			  absolute path to that directory, else = ""
	#
	# out_file -- optional explicit output file name.  If empty,
	#				the output defaults to "in_file"_repl.rtdf
	#
	# in_dir -- if in_file is in a different directory, this is the
	#			  absolute path to that directory
    #--------------------------------------------------------------------

    if (reference_file == "") {
        reference_file <- readline("Enter the name of the reference_file to read: ")
    }

	if (out_file == "") {
		out_file = as.character(strsplit(in_file,"[.][rR]tdf$"))
		out_file = paste(out_file,"_repl.rtdf",sep="")
	}


	# read in reference_file
	#---------------------------
	if (ref_dir != "") {
		my_dir = getwd()
		setwd(ref_dir)
	}
	my_objects = load(reference_file)
	if (ref_dir != "")  setwd(my_dir)

	# extract testnames and testnumbers, then remove my_objects
	ref_tnums = as.numeric(ParametersFrame[["testnum"]])
	ref_tnames = as.character(ParametersFrame[["testname"]])
	refs = length(ref_tnums)
	rm(list=my_objects)


	# now load in_file
	#------------------
	if (in_dir != "") {
		my_dir = getwd()
		setwd(in_dir)
	}
	my_objects = load(in_file)
	if (in_dir != "")  setwd(my_dir)

	tests = dim(ParametersFrame)[1]
	Parameters_Names = as.character(ParametersFrame[["testname"]])


	# now search by testnumber and create new testnames list
	#--------------------------------------------------------
	matches = rep(FALSE,tests)
	prev_param_i = 0
	for (i in 1:tests) {
		tnum = as.numeric(ParametersFrame[[i,"testnum"]])
		if( (prev_param_i<refs) && (tnum==ref_tnums[prev_param_i+1]) ) {
			xref = prev_param_i+1
		} else {
			xref = match(tnum,ref_tnums,nomatch=0)
		}
		if (xref>0) {
			Parameters_Names[i] = ref_tnames[xref]
			matches[i] = TRUE
			prev_param_i = xref
		} else {
		#	cat(sprintf("WARNING: test num %d not found in reference rtdf\n",
		#		tnum))
		}
	}


	# now update parametersframe
	#---------------------------
	ParametersFrame[1:tests,"testname"] <- Parameters_Names


	# remove tests not found in reference file?
	#------------------------------------------
	both = length(which(matches==TRUE))
	cat(sprintf("in_file has %d parameters, \n",
			tests))
	cat(sprintf("... %d also found in reference_file, \n",
			both))
	cat(sprintf("... %d found only in reference_file \n",
			refs-both))
	if (only_matches) {
		ParametersFrame = ParametersFrame[matches,]
		ResultsMatrix = ResultsMatrix[,matches]
	}


	# now save my_objects to new name
	#--------------------------------
    save(list=my_objects, file=out_file)

	cat("ReplaceTestnames Done!\n")
}
