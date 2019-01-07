# LoadRtdf.R
#
# $Id: LoadRtdf.R,v 1.3 2013/02/19 02:15:56 david Exp $
#
# script that clears the RTDF objects and then loads
# the specified rtdf file.
#
# Copyright (C) 2008-2013 David Gattrell
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
LoadRtdf <- function(rtdf_name="",in_dir="") {

	if(exists("LotInfoFrame",where=".GlobalEnv",inherits=FALSE))  rm(
			LotInfoFrame,pos=".GlobalEnv",inherits=FALSE)
	if(exists("ParametersFrame",where=".GlobalEnv",inherits=FALSE))  rm(
			ParametersFrame,pos=".GlobalEnv",inherits=FALSE)
	if(exists("DevicesFrame",where=".GlobalEnv",inherits=FALSE))  rm(
			DevicesFrame,pos=".GlobalEnv",inherits=FALSE)
	if(exists("ResultsMatrix",where=".GlobalEnv",inherits=FALSE))  rm(
			ResultsMatrix,pos=".GlobalEnv",inherits=FALSE)

	if(exists("TestFlagMatrix",where=".GlobalEnv",inherits=FALSE))  rm(
			TestFlagMatrix,pos=".GlobalEnv",inherits=FALSE)

	if(exists("HbinInfoFrame",where=".GlobalEnv",inherits=FALSE))  rm(
			HbinInfoFrame,pos=".GlobalEnv",inherits=FALSE)
	if(exists("SbinInfoFrame",where=".GlobalEnv",inherits=FALSE))  rm(
			SbinInfoFrame,pos=".GlobalEnv",inherits=FALSE)
	if(exists("WaferInfoFrame",where=".GlobalEnv",inherits=FALSE))  rm(
			WaferInfoFrame,pos=".GlobalEnv",inherits=FALSE)
	if(exists("TSRFrame",where=".GlobalEnv",inherits=FALSE))  rm(
			TSRFrame,pos=".GlobalEnv",inherits=FALSE)
	if(exists("WafersFrame",where=".GlobalEnv",inherits=FALSE))  rm(
			WafersFrame,pos=".GlobalEnv",inherits=FALSE)

	if(exists("SiteSbinInfoFrame",where=".GlobalEnv",inherits=FALSE))  rm(
			SiteSbinInfoFrame,pos=".GlobalEnv",inherits=FALSE)
	if(exists("SiteSbinSiteVector",where=".GlobalEnv",inherits=FALSE))  rm(
			SiteSbinSiteVector,pos=".GlobalEnv",inherits=FALSE)
	if(exists("SiteSbinCountMatrix",where=".GlobalEnv",inherits=FALSE))  rm(
			SiteSbinCountMatrix,pos=".GlobalEnv",inherits=FALSE)

	if(in_dir != "") {
		my_dir = getwd()
		setwd(in_dir)
	}
	load(rtdf_name)
	if(in_dir != "") setwd(my_dir)

	if(exists("LotInfoFrame",inherits=FALSE))  LotInfoFrame<<-LotInfoFrame
	if(exists("ParametersFrame",inherits=FALSE))  ParametersFrame<<-ParametersFrame
	if(exists("DevicesFrame",inherits=FALSE))  DevicesFrame<<-DevicesFrame
	if(exists("ResultsMatrix",inherits=FALSE))  ResultsMatrix<<-ResultsMatrix

	if(exists("TestFlagMatrix",inherits=FALSE))  TestFlagMatrix<<-TestFlagMatrix

	if(exists("HbinInfoFrame",inherits=FALSE))  HbinInfoFrame<<-HbinInfoFrame
	if(exists("SbinInfoFrame",inherits=FALSE))  SbinInfoFrame<<-SbinInfoFrame
	if(exists("WaferInfoFrame",inherits=FALSE))  WaferInfoFrame<<-WaferInfoFrame
	if(exists("TSRFrame",inherits=FALSE))  TSRFrame<<-TSRFrame
	if(exists("WafersFrame",inherits=FALSE))  WafersFrame<<-WafersFrame

	if(exists("SiteSbinInfoFrame",inherits=FALSE))  SiteSbinInfoFrame<<-SiteSbinInfoFrame
	if(exists("SiteSbinSiteVector",inherits=FALSE))  SiteSbinSiteVector<<-SiteSbinSiteVector
	if(exists("SiteSbinCountMatrix",inherits=FALSE))  SiteSbinCountMatrix<<-SiteSbinCountMatrix

}


#environment(LoadRtdf)<-.GlobalEnv

