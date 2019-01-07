# RobustFilter.R
#
# $Id: RobustFilter.R,v 1.2 2010/01/17 22:34:53 David Exp $
#
# script that reads in an rtdf file and generates a new rtdf
# file that has outlier devices removed.
# Outliers are determined as devices that have measurements
# for any test that are outside of the specified number of
# robust standard deviations from the robust mean.
#
# Copyright (C) 2008-10 David Gattrell
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
RobustFilter <- function(in_file="",limit=6.0,out_file="",in_dir="") {

    # in_file -- string of rtdf filename to process
	# limit -- number of robust standard deviations away from robust mean to 
	#          use to screen devices
    # out_file -- string of rtdf filename to write to,
	# in_dir -- absolute path for in_file if different
	#           than current directory.
    # -----------------------------------------------------
	if (in_file == "") {
		in_file <- readline("Enter the name of the RTDF file to read: ")
    }

    if (out_file == "") {
		out_file <- readline("Enter the name of the RTDF file to write: ")
    }

	if (in_dir != "") {
		my_dir = getwd()
		setwd(in_dir)
	}
    load(in_file)
	if (in_dir != "")  setwd(my_dir)

	dims <- dim(ResultsMatrix)
	devices = dims[1]
	parameters = dims[2]

	# variables for screened.csv file
	#---------------------------------
	screen_parameters = vector()	# vector of strings; testnames
	screen_lo_limits = vector()	# robust low limit used 
	screen_hi_limits = vector()	# robust high limit used 
	screen_matrix = array(NaN, dim=dims)

	keepers = rep(TRUE,devices)
	for(j in 1:parameters) {
		results = ResultsMatrix[,j]
		finite_results = results[is.finite(results)]
        my_q25 = quantile(finite_results,0.25)
        my_q50 = median(finite_results)
        my_q75 = quantile(finite_results,0.75)
        r_sdev = abs(my_q75 - my_q25)/1.34898

		if (r_sdev>0) {
			lo_screen = my_q50 - (limit*r_sdev)
			hi_screen = my_q50 + (limit*r_sdev)

			indices = which(results<lo_screen)
			if(length(indices)>0) {
				screen_parameters[j] <- ParametersFrame[j,"testname"]
				screen_lo_limits[j] <- lo_screen

			}
			keepers[indices] = FALSE
			screen_matrix[indices,j] <- ResultsMatrix[indices,j]

			indices = which(results>hi_screen)
			if(length(indices)>0) {
				screen_parameters[j] <- ParametersFrame[j,"testname"]
				screen_hi_limits[j] <- hi_screen

			}
			keepers[indices] = FALSE
			screen_matrix[indices,j] <- ResultsMatrix[indices,j]
		}
	}

	# REVISIT: need to report how many parts at each test are removed...
	# reduce matrix... just devices that are removed
	# just parameters that invoked screening
	BadDevsFrame = DevicesFrame[!keepers,]
	BadResMatrix = screen_matrix[!keepers,]


	DevicesFrame = DevicesFrame[keepers,]
	ResultsMatrix = ResultsMatrix[keepers,]

    my_list = c("LotInfoFrame","ParametersFrame","DevicesFrame","ResultsMatrix")
    if (exists("HbinInfoFrame",inherits=FALSE))  my_list[length(my_list)+1] = "HbinInfoFrame"
    if (exists("SbinInfoFrame",inherits=FALSE))  my_list[length(my_list)+1] = "SbinInfoFrame"
	if (exists("TSRFrame",inherits=FALSE))  my_list[length(my_list)+1] = "TSRFrame"
	if (exists("WafersFrame",inherits=FALSE))  my_list[length(my_list)+1] = "WafersFrame"
	if (exists("WaferInfoFrame",inherits=FALSE))  my_list[length(my_list)+1] = "WaferInfoFrame"
    save(list=my_list,file=out_file)

}
