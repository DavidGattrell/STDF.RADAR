#  RemoveAtXY.R
#
# $Id: RemoveAtXY.R,v 1.2 2019/02/01 01:54:24 david Exp $
#
# script that parses rtdf file and removes devices if they have both an X coord.
#  of -1 and a Y coord. of -1.  another 93K'ism.  (Or you can enter your own
#  X and Y coordinate pair)
#
# Copyright (C) 2011 David Gattrell
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
RemoveAtXY <- function(in_file="",out_file="",in_dir="",x_coord=-1,y_coord=-1) {

    # in_file -- string of rtdf file name to process
    # out_file -- string of rtdf file name to write to
    #             if out_file is left empty, overwrite in_file
	# in_dir -- absolute path for in_file if different
	#           than current directory.
	# x_coord -- 
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

	x_coords = as.character(DevicesFrame[["x_coord"]])
	y_coords = as.character(DevicesFrame[["y_coord"]])

	indices = which((x_coords==x_coord) & (y_coords==y_coord))
	if(length(indices)>0)  keepers[indices]=0


    DevicesFrame = DevicesFrame[keepers==1,]
    ResultsMatrix = ResultsMatrix[keepers==1,]
	if (is.finite(match("TestFlagMatrix",my_objs))) {
    	TestFlagMatrix = TestFlagMatrix[keepers==1,]
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
    dims = dim(ResultsMatrix)
    cat(sprintf("Removed %d Devices, now %d Devices x %d Parameters \n",
			(devs - dims[1]),dims[1],dims[2]))
		
}
