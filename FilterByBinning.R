# FilterByBinning.R
#
# $Id: FilterByBinning.R,v 1.4 2017/08/11 00:55:53 david Exp $
#
# script that reads in an rtdf file and generates a new rtdf
# file that only includes devices that match the specified
# binning parameters
#
#
# Copyright (C) 2009-10 David Gattrell
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
FilterByBinning <- function(in_file="",action="keep",bin_type="hbin",
				bins=1,out_file="",in_dir="") {

    # in_file	- string of rtdf filename to process
	# action	- one of "remove", "keep", or "report"
	# bin_type	- one of "sbin" or "hbin"
	# bins		- vector of bin numbers (0<=  <=100)
    # out_file	- string of rtdf filename to write to
	# in_dir	- absolute path for in_file if different
	#             than current directory.
    # -----------------------------------------------------
    if (in_file == "") {
		in_file <- readline("Enter the name of the RTDF file to read: ")
    }

    if (out_file == "") {
		out_file <- readline("Enter the name of the RTDF file to write: ")
    }

	if (in_dir != "") {
		my_dir = getwd()
		setwd(in_dir)
	}
    my_objs = load(in_file)
	if (in_dir != "")  setwd(my_dir)

	if (bin_type == "hbin") {
	    binning = as.numeric(DevicesFrame[,"hard_bin"])
	} else {
	    binning = as.numeric(DevicesFrame[,"soft_bin"])
	}

	indices = which(binning %in% bins)

    if (length(indices)>0) {
		cat(sprintf("%d of %d Devices match the binning criteria\n",
			length(indices),length(binning) ))
		
		if(action=="keep") {
			DevicesFrame = DevicesFrame[indices,]
			ResultsMatrix = ResultsMatrix[indices,]
			if (exists("TestFlagMatrix",inherits=FALSE))  TestFlagMatrix = TestFlagMatrix[indices,]
			do_write = TRUE
		} else if (action=="remove") {
			keepers = rep(TRUE,length(binning))
			keepers[indices] = FALSE
			DevicesFrame = DevicesFrame[keepers,]
			ResultsMatrix = ResultsMatrix[keepers,]
			if (exists("TestFlagMatrix",inherits=FALSE))  TestFlagMatrix = TestFlagMatrix[keepers,]
			do_write = TRUE
			do_write = TRUE
		} else {
			device_names = DevicesFrame[indices,"part_id"]
			for (i in 1:length(indices)) {
				cat(sprintf("Index: %-3d Part_id: %-5s  %s: %d\n",
					indices[i],device_names[i],bin_type,binning[indices[i]]))
			}
			do_write = FALSE
		}

		if (do_write) {
			# my_list = c("LotInfoFrame","ParametersFrame","DevicesFrame","ResultsMatrix")
			# if (exists("HbinInfoFrame",inherits=FALSE))  my_list[length(my_list)+1] = "HbinInfoFrame"
			# if (exists("SbinInfoFrame",inherits=FALSE))  my_list[length(my_list)+1] = "SbinInfoFrame"
			# if (exists("TSRFrame",inherits=FALSE))  my_list[length(my_list)+1] = "TSRFrame"
			# if (exists("WafersFrame",inherits=FALSE))  my_list[length(my_list)+1] = "WafersFrame"
			# if (exists("WaferInfoFrame",inherits=FALSE))  my_list[length(my_list)+1] = "WaferInfoFrame"
			# save(list=my_list,file=out_file)
			save(list=my_objs,file=out_file)
		}
    } else {
		cat("No Devices match the binning criteria\n")
    }
}



