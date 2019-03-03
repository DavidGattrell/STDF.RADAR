# XYWid2Partid.R
# 
# $Id: XYWid2Partid.R,v 1.1 2017/10/17 02:17:21 david Exp $
#
# script that reads in a RTDF file, looks for X/Y coord and wafer_id
# from the DevicesFrame and overwrites the part_id field of 
# DevicesFrame, optionally, pull the X,Y,Wafer ID info from ParametersFrame
# and ResultsMatrix
#
# Copyright (C) 2017 David Gattrell
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
XYWid2Partid <- function(in_rtdf="",out_rtdf="",in_dir="",output_dir="",
				from_params=FALSE,
				save_prev_part_id="prev_part_id",
				x_test_substr="X_COORD",y_test_substr="Y_COORD",
				w_test_substr="WAFER_ID") {

    # in_rtdf  - RTDF file to look in
	# out_rtdf - the name of the RTDF file to create 
	#            if empty, overwrite in_rtdf file
	# in_dir    - directory to look for in_file if not same as pwd.
	# output_dir - directory to write to, if not same as pwd
	# from_params - if the original stdf file did not have the PIR/PRR/
	#            WIR/WRR records populated, but instead had the X/Y coords and 
	#			 wafer id datalogged as regular tests, set this flag to true,
	#            and specify the below fields
	# save_prev_part_id - will save part_id to this DeviceFrame field if not ""
	#			 before overwriting part_id field.
	# x_test_substr - a unique substring of testname for the x coordinate
	# y_test_substr - a unique substring of testname for the y coordinate
	# w_test_substr - a unique substring of testname for the wafer number
    #----------------------------------


	# load rtdf file into memory
	#---------------------------
	if (in_dir != "") {
		my_dir = getwd()
		setwd(in_dir)
	}
    my_objs = load(in_rtdf)
	if (in_dir != "")  setwd(my_dir)


	part_count = dim(ResultsMatrix)[1]


	if (from_params) {
		# get info from ParamtersFrame/ResultsMatrix
		testnames = as.character(ParametersFrame[["testname"]])
		
		idx = grep(x_test_substr,testnames)
		if(length(idx)==1) {
			x_coords = ResultsMatrix[,idx]
		} else if(length(idx)>1) {
			cat("confused with X_COORD: ")
			cat(sprintf("more than one testname matches %s (%d matches)\n",
						x_test_substr,length(idx)))
			x_coords = rep(NaN,part_count)
		} else {
			cat("confused with X_COORD: ")
			cat(sprintf("no testnames match %s \n",
						x_test_substr))
			x_coords = rep(NaN,part_count)
		}
		idx = grep(y_test_substr,testnames)
		if(length(idx)==1) {
			y_coords = ResultsMatrix[,idx]
		} else if(length(idx)>1) {
			cat("confused with Y_COORD: ")
			cat(sprintf("more than one testname matches %s (%d matches)\n",
						y_test_substr,length(idx)))
			y_coords = rep(NaN,part_count)
		} else {
			cat("confused with Y_COORD: ")
			cat(sprintf("no testnames match %s \n",
						y_test_substr))
			y_coords = rep(NaN,part_count)
		}
		idx = grep(w_test_substr,testnames)
		if(length(idx)==1) {
			wafer_ids = ResultsMatrix[,idx]
		} else if(length(idx)>1) {
			cat("confused with WAFER_ID: ")
			cat(sprintf("more than one testname matches %s (%d matches)\n",
						w_test_substr,length(idx)))
			wafer_ids = rep(NaN,part_count)
		} else {
			cat("confused with WAFER_ID: ")
			cat(sprintf("no testnames match %s \n",
						w_test_substr))
			wafer_ids = rep(NaN,part_count)
		}

		# replace NaNs or NAs with 99
		x_coords[ which(!is.finite(x_coords)) ] = 99
		y_coords[ which(!is.finite(y_coords)) ] = 99
		wafer_ids[ which(!is.finite(wafer_ids)) ] = 99

		trace_id = mapply(function(x,y,z) sprintf("x%d_y%d_w%d",x,y,z),
					x_coords,y_coords,wafer_ids)

	} else {
		# get info from DevicesFrame
		x_coords = as.integer(DevicesFrame[["x_coord"]])
		y_coords = as.integer(DevicesFrame[["y_coord"]])
		wafer_idxs = as.integer(DevicesFrame[["wafer_index"]])
		wafer_ids = as.integer(WafersFrame[wafer_idxs,"wafer_id"])

		# replace NaNs or NAs with 99
		x_coords[ which(!is.finite(x_coords)) ] = 99
		y_coords[ which(!is.finite(y_coords)) ] = 99
		wafer_ids[ which(!is.finite(wafer_ids)) ] = 99

		# replace -32768s with 99
		x_coords[ which(x_coords==-32768) ] = 99
		y_coords[ which(y_coords==-32768) ] = 99

		trace_id = mapply(function(x,y,z) sprintf("x%d_y%d_w%d",x,y,z),
					x_coords,y_coords,wafer_ids)

	}


	# save part_id's then overwrite them with trace_ids
	orig_part_id = as.character(DevicesFrame[["part_id"]])
	if (save_prev_part_id != "") {
		DevicesFrame[[save_prev_part_id]] = orig_part_id
	}
	DevicesFrame[["part_id"]] = trace_id


	if(out_rtdf=="") {
		out_rtdf = rtdf_name
	}
	if(output_dir != "") {
		my_dir = getwd()
		setwd(output_dir)
	}
	save(list=my_objs,file=out_rtdf)
	if(output_dir != "") setwd(my_dir)
}


