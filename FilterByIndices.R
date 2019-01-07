# FilterByIndices.R
#
# $Id: FilterByIndices.R,v 1.1 2013/09/01 23:44:02 david Exp $
#
# script that removes or keeps individual devices from the rtdf file
#
# part of RADAR scripts, see sites.google.com/site/stdfradar
#
# Copyright (C) 2013 David Gattrell
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
FilterByIndices <- function(in_file="",indices=NaN,action="remove",
				out_file="",in_dir="") {

    # in_file  - RTDF file to look in
    # indices  - vector of device indices to remove/keep/report or if boolean,
	#            a vector to cycle through to select devices
    #            NOTE: this is not part_id's
	#			 ie. if indices is c(TRUE,FALSE,FALSE), then every
	#            first of three devices would be selected.
	# action - one of "remove", "keep", or "report", hopefully
	#            this is self explanatory
	# out_file - the name of the RTDF file to create (unless action
	#            is "report", in which case this is ignored)
	# in_dir    - directory to look for in_file if not same as
	#             pwd.
    #
    #----------------------------------

	if (in_dir != "") {
		my_dir = getwd()
		setwd(in_dir)
	}
    my_objs = load(in_file)
	if (in_dir != "")  setwd(my_dir)

    keepers = rep(TRUE,dim(DevicesFrame)[1])

	if (is.logical(indices)) {
		keepers = rep(indices,length.out=length(keepers))
	} else {
    	keepers[indices] = FALSE
	}


	# keep track of objects loaded, use that list for saving?

	if(action=="remove") {
	    DevicesFrame = DevicesFrame[keepers,]
	    ResultsMatrix = ResultsMatrix[keepers,]
		do_outfile = TRUE
	}
	else if(action=="keep") {
	    DevicesFrame = DevicesFrame[!keepers,]
	    ResultsMatrix = ResultsMatrix[!keepers,]
		do_outfile = TRUE
	} 
	else {	# action=="report" 
		indices = which(keepers==TRUE)
    	device_names = DevicesFrame[indices,"part_id"]
		for (i in 1:length(indices)) {
		    cat(sprintf("Index: %-3d  Part_id: %-5s \n",
			indices[i],device_names[i]))	
		}
		do_outfile = FALSE
	}


	if(do_outfile) {
		if (out_file=="") {
			out_file = in_file
		}

#		my_list = c("LotInfoFrame","ParametersFrame","DevicesFrame","ResultsMatrix")
#		if (exists("HbinInfoFrame",inherits=FALSE))  my_list[length(my_list)+1] = "HbinInfoFrame"
#		if (exists("SbinInfoFrame",inherits=FALSE))  my_list[length(my_list)+1] = "SbinInfoFrame"
#		if (exists("TSRFrame",inherits=FALSE))       my_list[length(my_list)+1] = "TSRFrame"
#		if (exists("WafersFrame",inherits=FALSE))  my_list[length(my_list)+1] = "WafersFrame"
#		if (exists("WaferInfoFrame",inherits=FALSE))  my_list[length(my_list)+1] = "WaferInfoFrame"
#		save(list=my_list,file=out_file)
		save(list=my_objs,file=out_file)
	}
}
