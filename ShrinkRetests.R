#  ShrinkRetests.R
#
# $Id: ShrinkRetests.R,v 1.5 2019/02/01 02:10:15 david Exp $
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
ShrinkRetests <- function(in_file="",out_file="",in_dir="",use_xy_coords=FALSE) {

    # in_file -- string of rtdf file name to process
    # out_file -- string of rtdf file name to write to
    #             if out_file is left empty, overwrite in_file
	# in_dir -- absolute path for in_file if different
	#           than current directory.
	# use_xy_coords -- if set to TRUE, then look for matching x_coord,y_coord
	#			pairs rather than matching part_id 
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

	if (use_xy_coords) {
		x_coords = as.character(DevicesFrame[["x_coord"]])
		y_coords = as.character(DevicesFrame[["y_coord"]])
		xys = array(c(x_coords,y_coords),dim=c(devs,2))

		indices = which(duplicated(xys,fromLast=TRUE))
		if(length(indices)>0)  keepers[indices]=0
	} else {
		x_coord = as.integer(DevicesFrame[[1,"x_coord"]])
		if(is.finite(x_coord) && x_coord>-32768) {
			cat("WARNING: you may really have wanted to have the use_xy_coords flag set!\n")
		}
		part_ids = as.character(DevicesFrame[["part_id"]])

		indices = which(duplicated(part_ids,fromLast=TRUE))
		if(length(indices)>0)  keepers[indices]=0

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
#    my_list = c("LotInfoFrame","ParametersFrame","DevicesFrame","ResultsMatrix")
#    if (exists("HbinInfoFrame",inherits=FALSE))  my_list[length(my_list)+1] = "HbinInfoFrame"
#    if (exists("SbinInfoFrame",inherits=FALSE))  my_list[length(my_list)+1] = "SbinInfoFrame"
#  	 if (exists("TSRFrame",inherits=FALSE))		 my_list[length(my_list)+1] = "TSRFrame"
#    if (exists("WafersFrame",inherits=FALSE))    my_list[length(my_list)+1] = "WafersFrame"
#    if (exists("WaferInfoFrame",inherits=FALSE)) my_list[length(my_list)+1] = "WaferInfoFrame"
#    
#    save(list=my_list, file=out_file)
    save(list=my_objs, file=out_file)
    dims = dim(ResultsMatrix)
    cat(sprintf("Removed %d Devices, now %d Devices x %d Parameters \n",
			(devs - dims[1]),dims[1],dims[2]))
		
}
