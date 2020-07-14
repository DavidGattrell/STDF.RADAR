# SaveRtdf.R
#
# $Id: SaveRtdf.R,v 1.11 2020/07/14 23:11:49 david Exp $
#
# script that writes an Rdata file containing the 4 key
# objects of the rtdf format and any optional ones if they
# exist.
#
# Copyright (C) 2006-2013 David Gattrell
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
SaveRtdf <- function(rtdf_name="",output_dir="",use_RtdfObjects=TRUE) {


	if(use_RtdfObjects) {
		if (!exists("RtdfObjects")) {
			use_RtdfObjects = FALSE	
		}
	}
	if(use_RtdfObjects) {
		my_list = RtdfObjects
	} else {
		my_list = c("LotInfoFrame","ParametersFrame","DevicesFrame","ResultsMatrix")
		if (exists("TestOrderMatrix"))  my_list[length(my_list)+1] = "TestOrderMatrix"
		if (exists("TestFlagMatrix"))  my_list[length(my_list)+1] = "TestFlagMatrix"
		if (exists("HbinInfoFrame"))  my_list[length(my_list)+1] = "HbinInfoFrame"
		if (exists("SbinInfoFrame"))  my_list[length(my_list)+1] = "SbinInfoFrame"
		if (exists("TSRFrame"))  my_list[length(my_list)+1] = "TSRFrame"
		if (exists("WafersFrame"))  my_list[length(my_list)+1] = "WafersFrame"
		if (exists("WaferInfoFrame"))  my_list[length(my_list)+1] = "WaferInfoFrame"
		if (exists("SiteSbinInfoFrame"))  my_list[length(my_list)+1] = "SiteSbinInfoFrame"
		if (exists("SiteSbinSiteVector"))  my_list[length(my_list)+1] = "SiteSbinSiteVector"
		if (exists("SiteSbinCountMatrix"))  my_list[length(my_list)+1] = "SiteSbinCountMatrix"
	}

	if(output_dir != "") {
		my_dir = getwd()
		setwd(output_dir)
	}
	save(list=my_list,file=rtdf_name)
	if(output_dir != "") setwd(my_dir)

}
