# No_Vlo_Vhi.R
#
# $Id: No_Vlo_Vhi.R,v 1.1 2009/09/09 01:33:44 David Exp $
#
# script for XXXXXXXX data that has been split by condition...
# remove parameters where there is no data,
# remove _VHI or _VLO from the ends of parameter names
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
#-----------------------------------------------
No_Vlo_Vhi <- function(in_file="") {
    # in_file --- string of rtdf filename to process

    if (in_file == "") {
		in_file <- readline("Enter the name of the RTDF file to read: ")
    }

    # strip off .rtdf/.Rtdf/.Rdata from out_file if it is there
    #--------------------------------------------------
    if (regexpr("[.]Rdata$",in_file)>0) {
		out_file = as.character(strsplit(in_file,"[.]Rdata$"))
    } else if (regexpr("[.]Rtdf$",in_file)>0) {
		out_file = as.character(strsplit(in_file,"[.]Rtdf$"))
    } else if (regexpr("[.]rtdf$",in_file)>0) {
		out_file = as.character(strsplit(in_file,"[.]rtdf$"))
    }
	out_file = paste(out_file,"_xv.rtdf",sep="")

    my_objects = load(in_file)


	tests = dim(ParametersFrame)[1]

	keepers = rep(FALSE,tests)
	for (i in 1:tests) {
		n = length(which(is.finite(ResultsMatrix[,i])))
		if (n>0) {
			keepers[i] = TRUE
			testname = as.character(ParametersFrame[i,"testname"])
			if (length(grep("_VLO$|_VHI$|_LOV$|_HIV$",testname))>0) {
				testname = sub("_VLO$|_VHI$|_LOV$|_HIV$","",testname)
				ParametersFrame[i,"testname"] = testname
			}
		}
	}
	ParametersFrame = ParametersFrame[keepers,]
	ResultsMatrix = ResultsMatrix[,keepers]


	# now save my_objects to new name
	#--------------------------------
    save(list=my_objects, file=out_file)
}

