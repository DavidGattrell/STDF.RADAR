#  ShrinkRetests.R
#
# $Id: ShrinkRetests.R,v 1.6 2020/02/17 21:48:28 david Exp $
#
# script that parses rtdf file and removes devices if there is
#  a retest for that part_id later in the file
#
# Copyright (C) 2007-2010 David Gattrell
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
ShrinkRetests <- function(in_file="",out_file="",in_dir="",use_xy_coords=FALSE,
						  keep_first_pass=FALSE,verbose=FALSE) {

    # in_file -- string of rtdf file name to process
    # out_file -- string of rtdf file name to write to
    #             if out_file is left empty, overwrite in_file
	# in_dir -- absolute path for in_file if different
	#           than current directory.
	# use_xy_coords -- if set to TRUE, then look for matching x_coord,y_coord
	#			pairs rather than matching part_id 
	#
	# REVISIT: started coding below, but did NOT finish!
	# keep_first_pass -- if a part is tested multiple times, 
	#           ignore subsequent fail runs once it has passed.
	# verbose -- 
	# ------------------------------------------------------------

    if (out_file == "")  out_file = in_file


	if (in_dir != "") {
		my_dir = getwd()
		setwd(in_dir)
	}
    my_objs = load(in_file)
	if (in_dir != "")  setwd(my_dir)


    devs = dim(DevicesFrame)[1]
    keepers = rep(1, times=devs)

	if(keep_first_pass) {
		# figure out which die are PASS die
		hbins = as.integer(DevicesFrame[["hard_bin"]])
		# HbinInfoFrame .. to determine passing hbin values
		# pass_fail_results = 
	}

	if (use_xy_coords) {
		x_coords = as.character(DevicesFrame[["x_coord"]])
		y_coords = as.character(DevicesFrame[["y_coord"]])
		xys = array(c(x_coords,y_coords),dim=c(devs,2))

		indices = which(duplicated(xys,fromLast=TRUE))

		if(length(indices)>0) {
			if(verbose) {
				cat(sprintf("repeats count is %d... \n",length(indices)))
			}
			if(keep_first_pass) {
				# rather than just blindly keeping the most recent test run for a part,
				# keep the first PASSING run, discard the rest.
				already_reviewed = rep(FALSE, times=devs)
				xy_strings = paste(x_coords,y_coords,sep=",")

				for (i in 1:length(indices)) {
					idx = indices[i]

					if(!already_reviewed[idx]) {
						repeats = which(xy_strings==xy_strings[idx])
						#passes = which(pass_fail_results[repeats]==1)

					}
				}
			} else {
				keepers[indices]=0
			}
		}
	} else {
		x_coord = as.integer(DevicesFrame[[1,"x_coord"]])
		if(is.finite(x_coord) && x_coord>-32768) {
			cat("WARNING: you may really have wanted to have the use_xy_coords flag set!\n")
		}
		part_ids = as.character(DevicesFrame[["part_id"]])

		indices = which(duplicated(part_ids,fromLast=TRUE))

		if(length(indices)>0) {
			if(keep_first_pass) {
			} else {
				keepers[indices]=0
			}
		}

		#for (i in 1:devs) {
		#	part_id = part_ids[i]
		#	index = match(part_id,part_ids[(i+1):devs],nomatch=NaN)
		#	if (is.finite(index))  keepers[i]=0
		#}

	}
    DevicesFrame = DevicesFrame[keepers==1,]
    ResultsMatrix = ResultsMatrix[keepers==1,]
	if(match("TestFlagMatrix",my_objs,nomatch=0)) {
		TestFlagMatrix = TestFlagMatrix[keepers==1,]
	}


    # save rtdf file
    #------------------
    save(list=my_objs, file=out_file)
    dims = dim(ResultsMatrix)
    cat(sprintf("Removed %d Devices, now %d Devices x %d Parameters \n",
			(devs - dims[1]),dims[1],dims[2]))
		
}
