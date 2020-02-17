# SanityCheck.R
#
# $Id: SanityCheck.R,v 1.1 2020/02/17 21:44:40 david Exp $
#
# script that reads in an RTDF file and does a sanity check on
# the various objects
#  - verify the sizes of the objects are consistent
#  - ... what else?
#
# Copyright (C) 2019 David Gattrell
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
SanityCheck <- function(rtdf_name="",in_dir="") {


	if(in_dir != "") {
		my_dir = getwd()
		setwd(in_dir)
	}
	my_objs = load(rtdf_name)
	if(in_dir != "") setwd(my_dir)


	# check to make sure at least the basic objects are present:
	#   LotInfoFrame
	#   ParametersFrame
	#   DevicesFrame
	#   ResultsMatrix
	if(match("LotInfoFrame",my_objs,nomatch=0)) {
	} else {
		cat(sprintf("ERROR: LotInfoFrame is missing\n"))
	}
	if(match("ParametersFrame",my_objs,nomatch=0)) {
		test_count = dim(ParametersFrame)[1]
		param_fields = dim(ParametersFrame)[2]

		if(param_fields<10) {
			cat(sprintf("ERROR: ParametersFrame has %d fields, expected at least 10\n",param_fields))
		}
	} else {
		cat(sprintf("ERROR: ParametersFrame is missing\n"))
	}
	if(match("DevicesFrame",my_objs,nomatch=0)) {
		part_count = dim(DevicesFrame)[1]
		device_fields = dim(DevicesFrame)[2]

		if(device_fields<9) {
			cat(sprintf("ERROR: DevicesFrame has %d fields, expected at least 9\n",device_fields))
		}
	} else {
		cat(sprintf("ERROR: DevicesFrame is missing\n"))
	}
	if(match("ResultsMatrix",my_objs,nomatch=0)) {
		parts = dim(ResultsMatrix)[1]
		tests = dim(ResultsMatrix)[2]

		if(parts != part_count) {
			cat(sprintf("ERROR: ResultsMatrix size does not match DevicesFrame size %d != %d\n",parts,part_count))
		}
		if(tests != test_count) {
			cat(sprintf("ERROR: ResultsMatrix size does not match ParametersFrame size %d != %d\n",tests,test_count))
		}
	} else {
		cat(sprintf("ERROR: ResultsMatrix is missing\n"))
	}



}



