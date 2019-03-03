# SplitWafers.R
#
# $Id: SplitWafers.R,v 1.7 2019/02/05 01:47:14 david Exp $
#
# script that reads in an rtdf file and generates new rtdf
# files, one per wafer
#
# Copyright (C) 2006-2010 David Gattrell
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
SplitWafers <- function(in_file="",out_file="",in_dir="") {

    # in_file --- string of rtdf filename to process
    # out_file -- optional string of rtdf filename to write to,
    #             otherwise appends wafer number to in_file name
    # ---------------------------------------------------------
    if (in_file == "") {
		in_file <- readline("Enter the name of the RTDF file to read: ")
    }

    if (out_file == "") {
		out_file = in_file
    }
    # strip off .rtdf/.Rtdf/.Rdata from out_file if it is there
    #--------------------------------------------------
    if (regexpr("[.]Rdata$",out_file)>0) {
		out_file = as.character(strsplit(out_file,"[.]Rdata$"))
    } else if (regexpr("[.]Rtdf$",out_file)>0) {
		out_file = as.character(strsplit(out_file,"[.]Rtdf$"))
    } else if (regexpr("[.]rtdf$",out_file)>0) {
		out_file = as.character(strsplit(out_file,"[.]rtdf$"))
    }

	if (in_dir != "") {
		my_dir = getwd()
		setwd(in_dir)
	}
    my_objs = load(in_file)
	if (in_dir != "")  setwd(my_dir)

    AllDevicesFrame = DevicesFrame
    AllResultsMatrix = ResultsMatrix
	if(match("TestFlagMatrix",my_objs,nomatch=0)) {
    	AllTestFlagMatrix = TestFlagMatrix
	}
    AllWafersFrame = WafersFrame

    wafer_count = dim(WafersFrame)[1]
    wafers = as.numeric(DevicesFrame[,"wafer_index"])

    
    for (j in 1:wafer_count) {

		DevicesFrame = AllDevicesFrame[wafers==j,]
		ResultsMatrix = AllResultsMatrix[wafers==j,]
		if(match("TestFlagMatrix",my_objs,nomatch=0)) {
			TestFlagMatrix = AllTestFlagMatrix[wafers==j,]
		}
		WafersFrame = AllWafersFrame[j,]
		DevicesFrame["wafer_index"] = 1

		wafer_id = as.character(AllWafersFrame[j,"wafer_id"])

		# check if multiple runs of the same wafer in this file, append _run2, etc if so...
		idx=which(AllWafersFrame[["wafer_id"]] == wafer_id)
		if(length(idx)>1) {
			run_num = which(idx == j)
			if(run_num>1) {
				wafer_id = paste(wafer_id,"_run",run_num,sep="")
			}
		}

		# need to build wafer_file from out_file + wafer_id + ".rtdf"
		wafer_file = paste(out_file,"_wafer",wafer_id,".rtdf",sep="")

#		my_list = c("LotInfoFrame","ParametersFrame","DevicesFrame","ResultsMatrix",
#			    "WafersFrame")
#        if (exists("HbinInfoFrame",inherits=FALSE))  my_list[length(my_list)+1] = "HbinInfoFrame"
#        if (exists("SbinInfoFrame",inherits=FALSE))  my_list[length(my_list)+1] = "SbinInfoFrame"
#        if (exists("TSRFrame",inherits=FALSE))  my_list[length(my_list)+1] = "TSRFrame"
#		if (exists("WafersFrame",inherits=FALSE))  my_list[length(my_list)+1] = "WafersFrame"
#        if (exists("WaferInfoFrame",inherits=FALSE))  my_list[length(my_list)+1] = "WaferInfoFrame"
#        save(list=my_list,file=wafer_file)
        save(list=my_objs,file=wafer_file)
    }

}

