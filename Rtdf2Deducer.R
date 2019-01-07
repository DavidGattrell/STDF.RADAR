# Rtdf2Deducer.R
#
# $Id: Rtdf2Deducer.R,v 1.1 2012/10/21 19:59:49 david Exp $
#
# script that formats the RTDF objects into something palatable for Deducer
# specifically:
#   - replace deducer unfriendly characters in testnames with "."
#   - scale data to values in the 0.1 to 1000 range (ie A to uA where appropriate)
#   - append scaling and units to end of testname
#   - assign testnames to ResultsMatrix column names
#   - combine DevicesFrame and ResultsMatrix into MyFrame
#
# Copyright (C) 2012 David Gattrell
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
Rtdf2Deducer <- function(rtdf_name="",rdata_name="",in_dir="",output_dir="") {
	# rtdf_name - name of the rtdf file to read in and convert
	# rdata_name - name of the rdata file to create from the rtdf file
	#              if this is left blank, the script will use the rtdf_name,
	#              stripping off the .rtdf ending and putting .Rdata at the end
	# in_dir - if the rtdf file is in another folder, this tells the script
	#              where to look
	# output_dir - if you want the .Rdata file created in a different folder,
	#              this tells the script where to write the file
	#-----------------------------------------------------------------------


	# load rtdf file
	#----------------
	if(in_dir != "") {
		my_dir = getwd()
		setwd(in_dir)
	}
	load(rtdf_name)
	if(in_dir != "") setwd(my_dir)



	part_count = dim(ResultsMatrix)[1]
	test_count = dim(ResultsMatrix)[2]


	# sanitize testnames for deducer/jgr/iplots
	#------------------------------------------
	testnames = as.character(ParametersFrame[["testname"]])
	#clean_names = gsub("[^[:alnum:]_]","_",testnames)
	clean_names = gsub("[[:punct:][:space:]]",".",testnames)

	# check for non-uniqueness...
	for (i in 1:length(clean_names)) {
		idx = which(clean_names == clean_names[i])
		if(length(idx)>1) {
			# append _n, incrementing n until it is unique
			j = 1
			new_name = sprintf("%s_%d",clean_names[i],j)
			idx = which(clean_names == new_name)
			while( (length(idx)>0) && (j<1000) ) {
				j = j + 1
				new_name = sprintf("%s_%d",clean_names[i],j)
				idx = which(clean_names == new_name)	
			}
			clean_names[i] = new_name
		}
	}


	# auto scale data for deducer/jgr/iplots
	# ie. convert A to uA if that makes sense
	#   choose largest of absolute values of 0.05 and 0.95 quantiles, 
	#   and choose scaling so this is 0.1<  <1000.0 engineering notation
	for (i in 1:test_count) {
		my_q05 = quantile(ResultsMatrix[,i],0.05, na.rm = TRUE)
		my_q95 = quantile(ResultsMatrix[,i],0.95, na.rm = TRUE)
		if(abs(my_q05)>abs(my_q95))  range=abs(my_q05)
		else  range=abs(my_q95)
		if(range>0.99e9) {
			scaler=-9
			prefix = "G"
		} else if(range>0.99e6) {
			scaler=-6
			prefix = "M"
		} else if(range>0.99e3) {
			scaler=-3
			prefix = "K"
		} else if(range>0.99) {
			scaler=0
			prefix = ""
		} else if(range>0.99e-3) {
			scaler=3
			prefix = "m"
		} else if(range>0.99e-6) {
			scaler=6
			prefix = "u"
		} else if(range>0.99e-9) {
			scaler=9
			prefix = "n"
		} else {
			scaler=12
			prefix = "p"
		}
		scale = 10^scaler
		ResultsMatrix[,i] = ResultsMatrix[,i] * scale

		# also append scaling+units to end of testnames...
		# ie "_uA" 
		units = ParametersFrame[[i,"units"]]
		if(nchar(prefix)>0 || nchar(units)>0) {
			clean_names[i] = sprintf("%s_%s%s",clean_names[i],prefix,units)
		}
	}


	# assign testnames+units to ResultsMatrix column names
	colnames(ResultsMatrix) = clean_names


	# combine Devices Frame and Results Matrix into single Deducer friendly Frame
	MtxFrame = as.data.frame(ResultsMatrix)
	MyFrame = cbind(DevicesFrame,MtxFrame)


	if(rdata_name=="") {
		rdata_name = sub("([.]rtdf)?$",".Rdata",rtdf_name)
	}
	if(output_dir != "") {
		my_dir = getwd()
		setwd(output_dir)
	}
	save(list="MyFrame",file=rdata_name)
	if(output_dir != "") setwd(my_dir)
}
