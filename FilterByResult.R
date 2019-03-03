# FilterByResult.R
#
# $Id: FilterByResult.R,v 1.4 2016/12/22 02:23:41 david Exp $
#
# script that filters rtdf file based on results of specified parameter
#
# Copyright (C) 2008-2010 David Gattrell
#               2014 David Gattrell
#               2016 David Gattrell
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
#------------------------------------------------------
FilterByResult <- function(in_file="",action="remove",
		filter="result",testname="",condition=">",value=0.0,scaler="",
		out_file="ick.rtdf",in_dir="") {
    
    # in_file   - RTDF file to look in
	# action    - one of "remove", "keep", or "report"
	# filter    - one of "result" or "device" - ie. remove only
	#             the results for that one parameter that meet the
	#             criteria, or remove the entire device
    # testname  - parameter testname to search
    # condition - one of ">",">=","<","<=", search criteria
    # value     - value to use for search threshold
    # scaler    - character, one of f,p,n,u,m,k,K,M,G,T or ""
    # out_file  - RTDF file to write, if action is "remove" or "keep"
	# in_dir    - directory to look for in_file if not same as
	#             pwd.
    # -----------------------------------------------------

	if (in_dir != "") {
		my_dir = getwd()
		setwd(in_dir)
	}
    my_objs = load(in_file)
	if (in_dir != "")  setwd(my_dir)

    index = match(testname,ParametersFrame[["testname"]],nomatch=0)
	if (index<1 && nchar(testname)>0) {
		# 'feature' of tclvalue()?  removes trailing whitespace
		# if we didn't find a match, can we if we consider trailing whitespace?
		my_text = sprintf("^%s[[:blank:]]*$",testname)
		index = grep(my_text,ParametersFrame[["testname"]])
		if (length(index)>0)  index = index[1]
		else  index = 0
	}

    # scale value based on scaler
    #-----------------------------
    if (nchar(scaler)>0) {
		scaler = substr(scaler,1,1)
		scale = switch(EXPR=scaler,
		    f=1e-15,
		    p=1e-12,
		    n=1e-9,
		    u=1e-6,
		    m=1e-3,
		    k=1e3,
		    K=1e3,
		    M=1e6,
		    G=1e9,
		    T=1e12,
		    1.0)
    } else {
		scale = 1.0
    }
    value = value*scale

    if (condition==">") {
		indices = which(ResultsMatrix[,index]>value)
    } else if(condition==">=") {
		indices = which(ResultsMatrix[,index]>=value)
    } else if(condition=="<=") {
		indices = which(ResultsMatrix[,index]<=value)
    } else if(condition=="<") {
		indices = which(ResultsMatrix[,index]<value)
    } else {
		indices = NA
		length(indices) <- 0
    }

    # now print out indices to screen
    #---------------------------------
    if (length(indices)>0) {
        device_names = DevicesFrame[indices,"part_id"]
		values = ResultsMatrix[indices,index]/scale
		for (i in 1:length(indices)) {
		    cat(sprintf("Index: %-3d  Part_id: %-5s  Value: %f %s\n",
			indices[i],device_names[i],values[i],scaler))	
		}

		if(action=="remove") {
			keepers = rep(TRUE,dim(DevicesFrame)[1])
			keepers[indices] = FALSE
			do_outfile = TRUE
		}
		else if(action=="keep") {
			keepers = rep(FALSE,dim(DevicesFrame)[1])
			keepers[indices] = TRUE
			do_outfile = TRUE
		}
		else {
			do_outfile = FALSE
		}

		if(do_outfile) {
			if(filter=="result") {
				ResultsMatrix[!keepers,index] = NaN
			}
			else if(filter=="device") {
				DevicesFrame = DevicesFrame[keepers,]
				ResultsMatrix = ResultsMatrix[keepers,]
			}

#			my_list = c("LotInfoFrame","ParametersFrame","DevicesFrame","ResultsMatrix")
#			if (exists("HbinInfoFrame",inherits=FALSE))  my_list[length(my_list)+1] = "HbinInfoFrame"
#			if (exists("SbinInfoFrame",inherits=FALSE))  my_list[length(my_list)+1] = "SbinInfoFrame"
#			if (exists("TSRFrame",inherits=FALSE))       my_list[length(my_list)+1] = "TSRFrame"
#			if (exists("WafersFrame",inherits=FALSE))  my_list[length(my_list)+1] = "WafersFrame"
#			if (exists("WaferInfoFrame",inherits=FALSE))  my_list[length(my_list)+1] = "WaferInfoFrame"
#			save(list=my_list,file=out_file)
			save(list=my_objs,file=out_file)
		}
    } else {
		cat("No Devices match the criteria\n")
    }
}
