# SplitTestsRetests.R
#
# $Id: SplitTestsRetests.R,v 1.1 2021/08/02 00:29:58 david Exp $
#
# script that reads in an rtdf file and splits the devices into first
# run or into retest run, generates 2 files  xxxx_first.rtdf and xxxx_retest.rtdf
#
#
# Copyright (C) 2021 David Gattrell
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
SplitTestsRetests <- function(in_file="",out_file="",in_dir="",use_xy_coords=FALSE) {

    # in_file -- string of rtdf file name to process
    # out_file -- string of rtdf file name to write to
    #             if out_file is left empty, overwrite in_file
	# in_dir -- absolute path for in_file if different
	#           than current directory.
	# use_xy_coords -- if set to TRUE, then look for matching x_coord,y_coord
	#			pairs rather than matching part_id 

    if (out_file == "")  out_file = in_file


	if (in_dir != "") {
		my_dir = getwd()
		setwd(in_dir)
	}
    my_objs = load(in_file)
	if (in_dir != "")  setwd(my_dir)


	devs = dim(DevicesFrame)[1]

	if(use_xy_coords) {
		x_coords = as.character(DevicesFrame[["x_coord"]])
		y_coords = as.character(DevicesFrame[["y_coord"]])
		xys = array(c(x_coords,y_coords),dim=c(devs,2))
		xy_strings = paste(x_coords,y_coords,sep=",")

		part_ids = xy_strings
	} else {
		part_ids = as.character(DevicesFrame[["part_id"]])
	}

	dup_indices = which(duplicated(part_ids))

	duplicates = rep(FALSE,devs)
	duplicates[dup_indices] = TRUE
	uniq_indices = which(duplicates==FALSE)

	AllDevicesFrame = DevicesFrame
	AllResultsMatrix = ResultsMatrix
	if(is.finite(match("TestFlagMatrix",my_objs))) {
		AllTestFlagMatrix = TestFlagMatrix
	}
	if(is.finite(match("TestOrderMatrix",my_objs))) {
		AllTestOrderMatrix = TestOrderMatrix
	}


	if(length(dup_indices)>0) {
		# save the retest parts to their own file
		DevicesFrame = DevicesFrame[dup_indices,]
		ResultsMatrix = ResultsMatrix[dup_indices,]
		if(is.finite(match("TestFlagMatrix",my_objs))) {
			TestFlagMatrix = TestFlagMatrix[dup_indices,]
		}
		if(is.finite(match("TestOrderMatrix",my_objs))) {
			TestOrderMatrix = TestOrderMatrix[dup_indices,]
		}

		cat(sprintf("%d runs are retests \n",length(dup_indices)))


		out_name = sub("(.rtdf)$","_retest.rtdf",out_file)
		save(list=my_objs,file=out_name)


		# save first run parts to their own file
		DevicesFrame = AllDevicesFrame[uniq_indices,]
		ResultsMatrix = AllResultsMatrix[uniq_indices,]
		if(is.finite(match("TestFlagMatrix",my_objs))) {
			TestFlagMatrix = AllTestFlagMatrix[uniq_indices,]
		}
		if(is.finite(match("TestOrderMatrix",my_objs))) {
			TestOrderMatrix = AllTestOrderMatrix[uniq_indices,]
		}

		cat(sprintf("%d runs are unique devices \n",devs - length(dup_indices)))


		out_name = sub("(.rtdf)$","_first.rtdf",out_file)
		save(list=my_objs,file=out_name)

	}


}
