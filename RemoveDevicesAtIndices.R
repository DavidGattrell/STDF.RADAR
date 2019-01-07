# RemoveDevicesAtIndices.R
#
# $Id: RemoveDevicesAtIndices.R,v 1.2 2010/01/17 22:34:16 David Exp $
#
# script that removes individual devices from the rtdf file
#
# part of RADAR scripts, see www.Stdf.Radar.googlepages.com/home
#
# Copyright (C) 2006-2010 David Gattrell
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
RemoveDevicesAtIndices <- function(in_file="",indices=NaN,out_file="",
				in_dir="") {

    # in_file  - 
    # devices  - vector of device indices to remove
    #            NOTE: this is not part_id's
	# in_dir    - directory to look for in_file if not same as
	#             pwd.
    #
    #----------------------------------

	if (in_dir != "") {
		my_dir = getwd()
		setwd(in_dir)
	}
    load(in_file)
	if (in_dir != "")  setwd(my_dir)

    keepers = rep(TRUE,dim(DevicesFrame)[1])
    keepers[indices] = FALSE
    devices = DevicesFrame[indices,"part_id"]

    # could show device part id's here and ask to confirm before deleting!

    DevicesFrame = DevicesFrame[keepers,]
    ResultsMatrix = ResultsMatrix[keepers,]

    if (out_file=="") {
		out_file = in_file
    }

    my_list = c("LotInfoFrame","ParametersFrame","DevicesFrame","ResultsMatrix")
    if (exists("HbinInfoFrame",inherits=FALSE))  my_list[length(my_list)+1] = "HbinInfoFrame"
    if (exists("SbinInfoFrame",inherits=FALSE))  my_list[length(my_list)+1] = "SbinInfoFrame"
    if (exists("TSRFrame",inherits=FALSE))       my_list[length(my_list)+1] = "TSRFrame"
	if (exists("WafersFrame",inherits=FALSE))  my_list[length(my_list)+1] = "WafersFrame"
	if (exists("WaferInfoFrame",inherits=FALSE))  my_list[length(my_list)+1] = "WaferInfoFrame"
    save(list=my_list,file=out_file)

}

