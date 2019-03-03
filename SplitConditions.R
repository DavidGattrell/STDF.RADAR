# SplitConditions.R
#
# $Id: SplitConditions.R,v 1.9 2019/02/05 02:06:32 david Exp $
#
# script that reads in an rtdf file and generates new rtdf
# files, one per set of conditions in DevicesFrame
# ie  Vdd=3.6  or Vdd=3.3 or Vdd=3.0
#
#
# Copyright (C) 2009-18 David Gattrell
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
SplitConditions <- function(in_file="",out_file="",in_dir="") {

    # in_file --- string of rtdf filename to process
    # out_file -- optional string of rtdf filename to write to,
    #             otherwise appends site number to in_file name
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

	# determine conditions...
	main_fields = c("part_id","temp","x_coord","y_coord","wafer_index",
			"soft_bin","hard_bin","testtime","site","trace_id","device_id",
			"source_dataset","first_fail_test","fail_test_count","orig_partid")
			# trace_id, device_id are extracted from DTR records...
			# really custom code for a specific application.
			# first_fail_test and fail_test_count if ran 'find_first_fails'
			# orig_partid if ran "XYWid2Partid"
	my_fields = labels(DevicesFrame[1,])[[2]]
	which_fields = rep(TRUE,length(my_fields))
	for (i in 1:length(main_fields)) {
		idx = match(main_fields[i],my_fields,nomatch=0)
		if (idx>0)  which_fields[idx] = FALSE
	}
	cond_fields = which(which_fields)
	if (length(cond_fields)<1) {
		cat("No Conditions found in DevicesFrame...")
	} else {

		# build matrix of conditions
		cond_mtrx = data.matrix(DevicesFrame[,cond_fields])
		unique_conds = unique(cond_mtrx)
		conds_count = dim(unique_conds)[1]

		cat(sprintf("%d unique Conditions found in DevicesFrame...\n",conds_count))
		
		for (j in 1:conds_count) {
			my_matches = rep(TRUE,dim(cond_mtrx)[1])
			for (i in 1:dim(cond_mtrx)[2]) {
				my_matches[which(as.numeric(cond_mtrx[,i]) != as.numeric(unique_conds[j,i]))]=FALSE
			}
			DevicesFrame = AllDevicesFrame[my_matches,]
			ResultsMatrix = AllResultsMatrix[my_matches,]
			if(match("TestFlagMatrix",my_objs,nomatch=0)) {
				TestFlagMatrix = AllTestFlagMatrix[my_matches,]
			}

			cond_id = sprintf("%d",j)

			# need to build conditon_file from out_file + cond_id + ".rtdf"
			cond_file = paste(out_file,"_cond",cond_id,".rtdf",sep="")

#			my_list = c("LotInfoFrame","ParametersFrame","DevicesFrame","ResultsMatrix")
#			if (exists("HbinInfoFrame",inherits=FALSE))   my_list[length(my_list)+1] = "HbinInfoFrame"
#			if (exists("SbinInfoFrame",inherits=FALSE))   my_list[length(my_list)+1] = "SbinInfoFrame"
#			if (exists("TSRFrame",inherits=FALSE))        my_list[length(my_list)+1] = "TSRFrame"
#			if (exists("WaferInfoFrame",inherits=FALSE))  my_list[length(my_list)+1] = "WaferInfoFrame"
#			if (exists("WafersFrame",inherits=FALSE))     my_list[length(my_list)+1] = "WafersFrame"
#			save(list=my_list,file=cond_file)
			save(list=my_objs,file=cond_file)
		}
		cat("Done\n")
	}
}

