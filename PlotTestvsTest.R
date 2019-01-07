# PlotTestvsTest.R
#
# $Id: PlotTestvsTest.R,v 1.9 2014/08/03 00:17:54 david Exp $
#
# script that searches rtdf file and returns DevicesFrame indices
# that match the search criteria
#
# Copyright (C) 2006-2014 David Gattrell
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
PlotTestvsTest <- function(rtdf_file="",testX="",testY="",show_part_ids=FALSE,
					rtdf_dir="",png_flag=FALSE,png_file="",limits_flag=0,
					limits=c(0,0,0,0)) {
    
    # rtdf_file - RTDF file to look in
    # testX     - testname to use for Xaxis of plot
    # testY     - testname to use for Yaxis of plot
	# show_part_ids - if TRUE, show part_id next to each point on plot
	# rtdf_dir -- absolute path for rtdf file if not in current 
	#             directory/folder.. else ""
	# png_flag - if TRUE, dump to .png file
	# png_file - if png_flag==TRUE
	#				if "", auto create name of png_file:
	#						testX_vs_testY.png
	#				if name doesn't end in .png, append .png to name
	#				else output to png_file
	# limits_flag - force the plot xmin,xmax,ymin,ymax values
	#               1 = force xmin
	#               2 = force xmax
	#               4 = force ymin
	#               8 = force ymax
	# limits - a vector of 4 values, usage depends on limits_flag
	#               limits[1] = xmin
	#               limits[2] = xmax
	#               limits[3] = ymin
	#               limits[4] = ymax
    # -----------------------------------------------------
    big_prefixes = c("10x","100x","K","10xK","100xK","M","10xM","100xM",
		    "G","10xG","100xG","T")
    lil_prefixes = c("d","c","m","100xu","10xu","u","100xn","10xn","n",
		    "100xp","10xp","p","100xf","10xf","f")

	if (rtdf_dir != "") {
		my_dir = getwd()
		setwd(rtdf_dir)
	}
    load(rtdf_file)
	if (rtdf_dir != "")  setwd(my_dir)

    index1 = match(testX,ParametersFrame[["testname"]],nomatch=0)
	if (index1<1 && nchar(testX)>0) {
		# 'feature' of tclvalue()?  removes trailing whitespace
		# if we didn't find a match, can we if we consider trailing whitespace?
		my_text = sprintf("^%s[[:blank:]]*$",testX)
		index1 = grep(my_text,ParametersFrame[["testname"]])
		if (length(index1)>0)  index1 = index1[1]
		else  index1 = 0
	}
    index2 = match(testY,ParametersFrame[["testname"]],nomatch=0)
	if (index2<1 && nchar(testY)>0) {
		# 'feature' of tclvalue()?  removes trailing whitespace
		# if we didn't find a match, can we if we consider trailing whitespace?
		my_text = sprintf("^%s[[:blank:]]*$",testY)
		index2 = grep(my_text,ParametersFrame[["testname"]])
		if (length(index2)>0)  index2 = index2[1]
		else  index2 = 0
	}

    scaleX = as.numeric(ParametersFrame[index1,"scaler"])
    scaleY = as.numeric(ParametersFrame[index2,"scaler"])

    if (scaleX<0) {
		prefixX = big_prefixes[-1*scaleX]
    } else if (scaleX==0) {
        prefixX = ""
    } else {
		prefixX = lil_prefixes[scaleX]
    }
    if (scaleY<0) {
		prefixY = big_prefixes[-1*scaleY]
    } else if (scaleY==0) {
        prefixY = ""
    } else {
		prefixY = lil_prefixes[scaleY]
    }
    scaleX = 10^scaleX
    scaleY = 10^scaleY

    unitsX = sprintf("%s%s",prefixX,ParametersFrame[index1,"units"])
    unitsY = sprintf("%s%s",prefixY,ParametersFrame[index2,"units"])

    vectorX = ResultsMatrix[,index1]*scaleX
    vectorY = ResultsMatrix[,index2]*scaleY
	valid_xys = is.finite(vectorX) & is.finite(vectorY)

    my_title = rtdf_file
    my_xlab = sprintf("%s (%s)",testX,unitsX)
    my_ylab = sprintf("%s (%s)",testY,unitsY)
    today = date()

	#browser()

	xlims = min(vectorX[valid_xys])
	xlims[2] = max(vectorX[valid_xys])
	ylims = min(vectorY[valid_xys])
	ylims[2] = max(vectorY[valid_xys])

	if( (as.raw(limits_flag)&as.raw(1)) > 0 ) {
		if(is.finite(limits[1])) {
			if(limits[1]<xlims[2]) {
				xlims[1] = limits[1]
			} else {
				cat("WARNING: Xmin > largest X, ignoring\n")
			}
		} else {
			cat("WARNING: Xmin is invalid, ignoring\n")
		}
	}
	if( (as.raw(limits_flag)&as.raw(2)) > 0 ) {
		if(is.finite(limits[2])) {
			if(limits[2]>xlims[1]) {
				xlims[2] = limits[2]
			} else {
				cat("WARNING: Xmax < smallest X, ignoring\n")
			}
		} else {
			cat("WARNING: Xmax is invalid, ignoring\n")
		}
	}
	if( (as.raw(limits_flag)&as.raw(4)) > 0 ) {
		if(is.finite(limits[3])) {
			if(limits[3]<ylims[2]) {
				ylims[1] = limits[3]
			} else {
				cat("WARNING: Ymin > largest Y, ignoring\n")
			}
		} else {
			cat("WARNING: Ymin is invalid, ignoring\n")
		}
	}
	if( (as.raw(limits_flag)&as.raw(8)) > 0 ) {
		if(is.finite(limits[4])) {
			if(limits[4]>ylims[1]) {
				ylims[2] = limits[4]
			} else {
				cat("WARNING: Ymax < smallest Y, ignoring\n")
			}
		} else {
			cat("WARNING: Ymax is invalid, ignoring\n")
		}
	}
	
	plot(xlims,ylims,type="n",xlab=my_xlab,ylab=my_ylab,
			main=my_title,sub=today)
    points(vectorX[valid_xys],vectorY[valid_xys],pch="+")

	if (show_part_ids) {
		part_ids = DevicesFrame[["part_id"]]
		text(vectorX[valid_xys],vectorY[valid_xys],part_ids[valid_xys],
				pos=4,offset=0.25,col="blue",cex=0.8)
	}

    grid()

	if (png_flag) {
		if (nchar(png_file)<1) {
			png_file = paste(testX,"_vs_",testY,".png",sep="")
		} else if (length(grep("[.]png$",png_file))<1) {
			png_file = paste(png_file,".png",sep="")
		}

		savePlot(filename=png_file,type="png")
	}
}
