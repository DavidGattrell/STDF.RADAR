#  RemoveAtXY.R
#
# $Id: RemoveAtXY.R,v 1.3 2021/09/09 00:04:26 david Exp $
#
# script that parses rtdf file and removes devices if they have both an X coord.
#  of -1 and a Y coord. of -1.  another 93K'ism.  (Or you can enter your own
#  X and Y coordinate pair)
#
# Copyright (C) 2011 David Gattrell
#               2018 David Gattrell
#               2021 David Gattrell
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
RemoveAtXY <- function(in_file="",out_file="",in_dir="",x_coord=-1,y_coord=-1,action="remove") {

    # in_file -- string of rtdf file name to process
    # out_file -- string of rtdf file name to write to
    #             if out_file is left empty, overwrite in_file
	# in_dir -- absolute path for in_file if different
	#           than current directory.
	# x_coord -- vector of x coords of die you want to remove
	# y_coord -- vector of y coords of die you want to remove
	# action -- one of "remove" or "report"
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

	x_coords = as.integer(DevicesFrame[["x_coord"]])
	y_coords = as.integer(DevicesFrame[["y_coord"]])

	# now can do vector of die rather than single die as input
	#indices = which((x_coords==x_coord) & (y_coords==y_coord))
	die_indices <- function(my_x,my_y) which((x_coords==my_x) & (y_coords==my_y))
	indices = unlist(mapply(die_indices,x_coord,y_coord))
	if(length(indices)>0)  keepers[indices]=0

	if(action=="report") {
		if(length(indices)>0) {
			cat(sprintf("%d die found that match specified coordinates \n",length(indices)))
    		device_names = DevicesFrame[indices,"part_id"]
			for (i in 1:length(indices)) {
				cat(sprintf("Index: %-3d  Part_id: %-5s X %-3d Y %-3d \n",
				indices[i],device_names[i],x_coords[indices[i]],y_coords[indices[i]]))	
			}
		} else {
			cat("No devices found that match these coordinates! \n")
		}
	} else {
		DevicesFrame = DevicesFrame[keepers==1,]
		ResultsMatrix = ResultsMatrix[keepers==1,]
		if (is.finite(match("TestFlagMatrix",my_objs))) {
			TestFlagMatrix = TestFlagMatrix[keepers==1,]
		}
		dims = dim(ResultsMatrix)
		cat(sprintf("Removed %d Devices, now %d Devices x %d Parameters \n",
				(devs - dims[1]),dims[1],dims[2]))
	}


    # save rtdf file
    #------------------
#    my_list = c("LotInfoFrame","ParametersFrame","DevicesFrame","ResultsMatrix")
#    if (exists("HbinInfoFrame",inherits=FALSE))  my_list[length(my_list)+1] = "HbinInfoFrame"
#    if (exists("SbinInfoFrame",inherits=FALSE))  my_list[length(my_list)+1] = "SbinInfoFrame"
#	 if (exists("TSRFrame",inherits=FALSE))		 my_list[length(my_list)+1] = "TSRFrame"
#    if (exists("WafersFrame",inherits=FALSE))    my_list[length(my_list)+1] = "WafersFrame"
#    if (exists("WaferInfoFrame",inherits=FALSE)) my_list[length(my_list)+1] = "WaferInfoFrame"
#    
#    save(list=my_list, file=out_file)
    save(list=my_objs, file=out_file)
		
}
