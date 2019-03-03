# StackedWaferMap.R
#
# $Id: StackedWaferMap.R,v 1.2 2018/05/03 00:44:07 david Exp $
#
# reads in rtdf file(s) and generates wafermap(s)
#
# Copyright (C) 2018 David Gattrell
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

# create environment for the function to hide local functions
#------------------------------------------------------------
if(exists(".StackedWaferMap.env")) rm(.StackedWaferMap.env) 
.StackedWaferMap.env <- new.env()



StackedWaferMap <- function(rtdf_names="",pdf_name="stacked_wafer_map.pdf",type="sbin",
		    x_coord_alpha=FALSE,panel=FALSE,x_left=FALSE,
		    y_down=FALSE,pass_bins=-1,rtdf_dir="",
			notch="",
			rotate_ccw=0,x_reverse_polarity=0,y_reverse_polarity=0,
			x_shift=0,y_shift=0,
			param_col_start=0.03,param_col_end=0.24,param_col_rev_flag=FALSE,
			borders_off=7000) {
    # rtdf_names -- name of the rtdf file(s) to read
    # pdf_name -- name of the pdf file to generate
    # type = "sbin" or "hbin" 
    # x_coord_alpha -- if from Tapestry... show X as A,B,C ... not 1,2,3,...
    # panel -- if strip handler panel, then use landscape page orientation
    # x_left -- flip wafermap: incrementing X goes right to left
    # y_down -- flip wafermap: incrementing Y goes top to bottom
	# pass_bins -- for sbin or hbin yield info, use the specified bin or
	#			  bins as pass bins, default is bin 1.
	#			  if -1 then 'automatic' mode...
	#			  ... if SbinInfoFrame or HbinInfoFrame exists, use sbin_pf flag, else...
	#			  ... for hbin, automatic mode is hbin=1 is pass
	#			  ... for sbin, automatic mode is sbins where hbin=1 are passes
	# rtdf_dir -- absolute path for rtdf file if not in current 
	#             directory/folder.. else ""
	# notch -- will add "NOTCH" to map at one of n, s, e, w locations if
	#			set to "n", or "s" or "e" or "w" , or "" to suppress
	#----------------------
	#   for transforming coordinate data...
	# ** these transforms are done after the x_left/y_down manipulations
	# rotate_ccw -- take X,Y coordinate data and rotate wafer counter clock wise
	#              input should be one of 0, 90, 180, or 270 (degrees)
	# x_reverse_polarity -- after applying the rotation, the 'new' x axis
	#              polarity can be reversed if this is set to 1, default=0
	# y_reverse_polarity -- similar to above
	# x_shift -- after applying rotation and reverse polarity, shift the x value
	#              by this amount
	# y_shift -- similar to above
	#----------------------------------
	#   for stacked wafer maps...
	# param_col_start -- by default, the color starts
	#              at red for the lowest value.  [0.03]  Setting this overrides that
	#              default
	# param_col_end -- by default, the color ends
	#              at green for the highest value.  [0.24]  Setting this overrides
	#              that default
	# param_col_rev_flag -- by default, the colors increase in value.  setting this
	#              to true reverses the color gradient.
	#              The above 3 inputs are applied to the R rainbow() function.
	#----------------------------------
	# borders_off -- If die count is >= borders_off, then turn off border
	#            that is drawn around each die in the wafer map
	#            if -1, always do border
	#            if 0, always suppress border
    ### other things not yet done... ###
    # - minx, maxx, miny, maxy to override wafer map size
    # - add vector for gaps for panels.. X or X and Y??


	# scaler prefix definitions
    #------------------------------
    big_prefixes = c("10x","100x","K","10xK","100xK","M","10xM","100xM",
		    "G","10xG","100xG","T")
    lil_prefixes = c("d","c","m","100xu","10xu","u","100xn","10xn","n",
		    "100xp","10xp","p","100xf","10xf","f")


    # timestamp...
    #-----------------
    timestamp0 = proc.time()
    timestamp0 = timestamp0[3]
    timestamp1 = timestamp0
    now = substr(as.character(Sys.time()),1,19)


	# if filenames not defined, prompt for them
    #------------------------------------------
    if (length(rtdf_names)<2 && (rtdf_names == "")) {
		rtdf_names <- readline("Enter the name of the RTDF file to read: ")
    }
	datasets = length(rtdf_names)


    # select page orientaion and fixed width font
    #---------------------------------------------
    to_pdf = TRUE
    if (to_pdf) {
		if (panel) {
		    pdf(file=pdf_name,paper="USr",width=10.5,height=8,family="Courier")
		} else {
		    pdf(file=pdf_name,paper="letter",width=8,height=10.5,family="Courier")
		}
    } else {
		par(family="mono")
    }


    # per page layout
    #-----------------
    figs = array(NaN, dim=c(0,4))   # 0 rows, 4 columns

    # top 2.5" of page are for text information, bottom part is plot
    if (panel) {
		# landscape mode...
		figs = rbind(figs,c(0.0,1.0,5.0/7.5,1.0))	# text portion
		figs = rbind(figs,c(0.0,1.0,0.0,5.0/7.5))	# wafer map portion
    } else {
		# portrait mode...
		figs = rbind(figs,c(0.0,1.0,7.5/10.0,1.0))	# text portion
		figs = rbind(figs,c(0.0,1.0,0.0,7.5/10.0))	# wafer map portion
    }

    screens = split.screen(figs)


	# define overall goods and raw maps
	#----------------------------------
	overall_max_x = integer()
	overall_min_x = integer()
	overall_max_y = integer()
	overall_min_y = integer()
	overall_tested_map = array()
	overall_passed_map = array()
	overall_wafer_count = 0
	overall_lotids = character(0)
	

	# loop through the data from the files to build composite map
	#------------------------------------------------------------
    for (j in 1:datasets) {
		if (rtdf_dir != "") {
			my_dir = getwd()
			setwd(rtdf_dir)
		}
		my_objs = load(rtdf_names[j])
		if (rtdf_dir != "")  setwd(my_dir)


		# does file have valid sbin or hbin type information?
		#----------------------------------------------------
		valid_sbin_types = FALSE
		if(is.finite(match("SbinInfoFrame",my_objs))) {
			sbin_types = as.character(SbinInfoFrame[["sbin_pf"]])
			if(is.finite(match("P",sbin_types)) || is.finite(match("F",sbin_types))) {
				valid_sbin_types = TRUE
				valid_pass_sbins = as.integer(SbinInfoFrame["sbin_num"][SbinInfoFrame["sbin_pf"]=="P"])
			}
		}
		valid_hbin_types = FALSE
		if(is.finite(match("HbinInfoFrame",my_objs))) {
			hbin_types = as.character(HbinInfoFrame[["hbin_pf"]])
			if(is.finite(match("P",hbin_types)) || is.finite(match("F",hbin_types))) {
				valid_hbin_types = TRUE
				valid_pass_hbins = as.integer(HbinInfoFrame["hbin_num"][HbinInfoFrame["hbin_pf"]=="P"])
			}
		}


		# single or multiple wafers in this file?
		#----------------------------------------
		if(is.finite(match("WafersFrame",my_objs))) {
			all_wi = as.integer(DevicesFrame[,"wafer_index"])
			unique_wis = unique(all_wi)
			wafers_count = length(unique_wis)
		} else {
			wafers_count = 1
		}
		all_xs = as.integer(DevicesFrame[,"x_coord"])
		if (x_left)  all_xs = all_xs * -1

		all_ys = as.integer(DevicesFrame[,"y_coord"])
		if (y_down)  all_ys = all_ys * -1

		all_sbins = as.integer(DevicesFrame[,"soft_bin"])
		all_hbins = as.integer(DevicesFrame[,"hard_bin"])


		# check pass_bins
		#-------------------
		if (length(pass_bins)>1) {
			my_pass_bins = pass_bins
		} else if(pass_bins>=0) {
			my_pass_bins = pass_bins
		} else {	# automatic mode
			if (type=="sbin") {
				if(valid_sbin_types) {
					my_pass_bins = valid_pass_sbins
				} else if(length(which(is.finite(all_hbins)))<1) {
					my_pass_bins = 1
				} else if(valid_hbin_types) {
					my_sbins = all_sbins[all_hbins %in% valid_pass_hbins] 
					my_pass_bins = unique(my_sbins)
				} else {
					my_sbins = all_sbins[all_hbins==1] 
					my_pass_bins = unique(my_sbins)
				}
			} else {
				if(valid_hbin_types) {
					my_pass_bins = valid_pass_hbins
				} else {
					my_pass_bins = 1
				}
			}
		}

		for (wafer in 1:wafers_count) {

			# print progress report to screen every 5 seconds
			#--------------------------------------------------
			timestamp2 = proc.time()
			timestamp2 = timestamp2[3]
			if (timestamp2>(timestamp1+5)) {
				timestamp1 = timestamp2
				cat(sprintf("...now processing file %d of %d ...\n",
					j,datasets))
			}


			overall_wafer_count = overall_wafer_count + 1
			overall_lotids[overall_wafer_count] = as.character(LotInfoFrame[[1,"lotid"]])


			# extract x,y, and sbin info from Frame
			#---------------------------------------
			if(is.finite(match("WafersFrame",my_objs))) {
				indxs = which(all_wi==unique_wis[wafer])
				xs = all_xs[indxs]
				ys = all_ys[indxs]
				sbins = all_sbins[indxs]
				hbins = all_hbins[indxs]
			} else {
				xs = all_xs
				ys = all_ys
				sbins = all_sbins
				hbins = all_hbins
			}

			# remove any die with bad coordinates
			#-------------------------------------------
			valid_xs = which((xs>-32768)&(xs<32768))
			valid_ys = which((ys>-32768)&(ys<32768))
		
			valid_dice = intersect(valid_xs,valid_ys)
		
			xs = xs[valid_dice]
			ys = ys[valid_dice]
			sbins = sbins[valid_dice]
			hbins = hbins[valid_dice]
			if (type=="sbin") {
				if(length(which(sbins %in% my_pass_bins))>0) {	
					passes = which(sbins %in% my_pass_bins)
				} else {
					passes = numeric()
				}
			} else {
				# type == "hbin"
				if(length(which(hbins %in% my_pass_bins))>0) {	
					passes = which(hbins %in% my_pass_bins)
				} else {
					passes = numeric()
				}
			}
			if(length(passes)>0) {
				pass_xs = xs[passes]
				pass_ys = ys[passes]
			} else {
				pass_xs = numeric()
				pass_ys = numeric()
			}

			# find min,max of x,y...
			#-------------------------------------------
			min_x = min(xs)
			max_x = max(xs)
			min_y = min(ys)
			max_y = max(ys)


			# build wafer map in array
			#----------------------------------------------
			my_sbin_map = array(NaN,dim=c(max_x-min_x+1,max_y-min_y+1))
			my_hbin_map = array(NaN,dim=c(max_x-min_x+1,max_y-min_y+1))
			ixy = array(c(xs-min_x+1,ys-min_y+1),dim=c(length(sbins),2))
			my_sbin_map[ixy]=sbins
			my_hbin_map[ixy]=hbins
			pass_ixy = array(c(pass_xs-min_x+1,pass_ys-min_y+1),dim=c(length(passes),2))

			# do we need to resize the overall maps?
			#---------------------------------------
			#browser()

			do_resize=FALSE
			new_max_x = overall_max_x
			new_min_x = overall_min_x
			new_max_y = overall_max_y
			new_min_y = overall_min_y
			if(length(dim(overall_tested_map))>1) { # ie. not first wafer
				if(!length(overall_max_x) || (max_x>overall_max_x)) {
					new_max_x = max_x
					do_resize=TRUE
				}
				if(!length(overall_min_x) || (min_x<overall_min_x)) {
					new_min_x = min_x
					do_resize=TRUE
				}
				if(!length(overall_max_y) || (max_y>overall_max_y)) {
					new_max_y = max_y
					do_resize=TRUE
				}
				if(!length(overall_min_y) || (min_y<overall_min_y)) {
					new_min_y = min_y
					do_resize=TRUE
				}
			}
			if(do_resize) {
				# create new, larger maps
				new_tested_map = array(0,dim=c(new_max_x-new_min_x+1,new_max_y-new_min_y+1))
				new_passed_map = array(0,dim=c(new_max_x-new_min_x+1,new_max_y-new_min_y+1))
				
				# put old map inside new map
				dx = overall_min_x - new_min_x 
				dy = overall_min_y - new_min_y
				new_tested_map[ (dx+overall_min_x):(dx+overall_max_x),
								(dy+overall_min_y):(dy+overall_max_y)] = overall_tested_map
				new_passed_map[ (dx+overall_min_x):(dx+overall_max_x),
								(dy+overall_min_y):(dy+overall_max_y)] = overall_passed_map

				# overwrite old map info with new map info
				overall_min_x = new_min_x
				overall_max_x = new_max_x
				overall_min_y = new_min_y
				overall_max_y = new_max_y
				overall_tested_map = new_tested_map
				overall_passed_map = new_passed_map

				ixy = array(c(xs-overall_min_x+1,ys-overall_min_y+1),dim=c(length(sbins),2))
				pass_ixy = array(c(pass_xs-overall_min_x+1,pass_ys-overall_min_y+1),dim=c(length(passes),2))

				# increment tested and passed counts for valid die
				overall_tested_map[ixy] = overall_tested_map[ixy] + 1		
				overall_passed_map[pass_ixy] = overall_passed_map[pass_ixy] + 1
			} else if(length(dim(overall_tested_map))<2) {
				# first map, so just copy single to overall
				overall_tested_map = array(0,dim=c(max_x-min_x+1,max_y-min_y+1))
				overall_passed_map = array(0,dim=c(max_x-min_x+1,max_y-min_y+1))
				overall_tested_map[ixy] = 1		
				overall_passed_map[pass_ixy] = 1
				overall_max_x = max_x
				overall_min_x = min_x
				overall_max_y = max_y
				overall_min_y = min_y
			} else {
				ixy = array(c(xs-overall_min_x+1,ys-overall_min_y+1),dim=c(length(sbins),2))
				pass_ixy = array(c(pass_xs-overall_min_x+1,pass_ys-overall_min_y+1),dim=c(length(passes),2))

				# increment tested and passed counts for valid die
				overall_tested_map[ixy] = overall_tested_map[ixy] + 1		
				overall_passed_map[pass_ixy] = overall_passed_map[pass_ixy] + 1
			}

		} # for wafers
	} # for datasets


	good_die = sum(overall_passed_map)
	total_die = sum(overall_tested_map)

	if( ((total_die/overall_wafer_count)<borders_off)||(borders_off<0) )  do_borders = TRUE
	else  do_borders = FALSE


	# do text stuff in top portion
	#------------------------------
	screen(1)
	# print lot id, sublot id
	unique_lotids = unique(overall_lotids)
	if(length(unique_lotids)==1) {
		my_title = sprintf("%d WAFERS from LOT:%s",
			overall_wafer_count,
			unique_lotids[1]
			)
	} else {
		my_title = sprintf("%d WAFERS from LOTS:%s,...",
			overall_wafer_count,
			unique_lotids[1]
			)
	}
	my_yield = 100.0*good_die/total_die
	# print total die  yield as %
	text_line = sprintf("%s   YIELD: %.1f percent  [%d/%d]",my_title,
						my_yield,good_die,total_die)
	line_length = strwidth(text_line)
	if (line_length>0.90)  my_cex = 0.90/line_length
	else  my_cex = 1.0
	text(0,9.5/10,text_line,pos=4,cex=my_cex)
	
	
	# map parametric color range against 0% to 100% yield
	#----------------------------------------------------
	z0 = 0.0
	z1 = 100.0
	breaks = z0 + (z1 - z0)*seq(0,1,len=100)

	# my_colors = rainbow(100,start=0.03,end=0.24)
	# my_colors = grey(seq(0,1,len=100))    # greyscale vs color...
	my_colors = rainbow(100,start=param_col_start,end=param_col_end)
	if(param_col_rev_flag)  my_colors = rev(my_colors)

	# print out parameter name being used,
	# then green square with max value >=
	# red square with min value <=

	range = 100.00
	scaler = 0
	scale = 1
	prefix = ""
	my_units = "pct"

	text(0.07,6.5/8,sprintf("Parameter: Yield"),pos=4,cex=1.0)

	rect(0,5.1/8,0.05,5.9/8,col=my_colors[100])
	text(0.07,5.5/8,sprintf("Value>= %.3f %s",scale*breaks[100],my_units),pos=4,cex=1.0)

	rect(0,4.1/8,0.05,4.9/8,col=my_colors[75])
	text(0.07,4.5/8,sprintf("Value= %.3f",scale*breaks[75]),pos=4,cex=1.0)

	rect(0,3.1/8,0.05,3.9/8,col=my_colors[50])
	text(0.07,3.5/8,sprintf("Value= %.3f",scale*breaks[50]),pos=4,cex=1.0)

	rect(0,2.1/8,0.05,2.9/8,col=my_colors[25])
	text(0.07,2.5/8,sprintf("Value= %.3f",scale*breaks[25]),pos=4,cex=1.0)

	rect(0,1.1/8,0.05,1.9/8,col=my_colors[1])
	text(0.07,1.5/8,sprintf("Value<= %.3f",scale*breaks[1]),pos=4,cex=1.0)


	# do wafer map in bottom portion
	#--------------------------------
	screen(2)
	par(mar=c(4.1,4.1,0.5,0.5))

	# NOTE: if x_left has been set, then x has been *-1
	#       if y_down has been set, then y has been *-1
	orig_min_x = overall_min_x
	orig_min_y = overall_min_y
	orig_max_x = overall_max_x
	orig_max_y = overall_max_y

	orig_param_map = 100.0*(overall_passed_map/overall_tested_map)
	results_map = orig_param_map

	degrees = NaN
	if (notch=="s") {
		degrees = 0
	} else if (notch=="n") {
		degrees = 180			
	} else if (notch=="e") {
		degrees = 90
	} else if (notch=="w") {
		degrees = 270
	}

	# do transforms as required...
	#   rotate_ccw
	#   x_reverse_polarity, y_reverse_polarity
	#   x_shift, y_shift
	degrees = (degrees + rotate_ccw) %% 360

	results_map = matrix_rotate(results_map,rotate_ccw)
	if (rotate_ccw==90) {
		max_x = -1 * orig_min_y
		min_x = -1 * orig_max_y
		max_y = orig_max_x
		min_y = orig_min_x

		if(x_left)  y_inv = -1  else  y_inv = 1
		if(y_down)  x_inv = 1  else  x_inv = -1

		y_coord_alpha = x_coord_alpha
		x_coord_alpha = 0
	} else if (rotate_ccw==180) {
		max_x = -1 * orig_min_x
		min_x = -1 * orig_max_x
		max_y = -1 * orig_min_y
		min_y = -1 * orig_max_y

		if(x_left)  x_inv = 1  else  x_inv = -1
		if(y_down)  y_inv = 1  else  y_inv = -1

		y_coord_alpha = 0
	} else if (rotate_ccw==270) {
		max_x = orig_max_y
		min_x = orig_min_y
		max_y = -1 * orig_min_x
		min_y = -1 * orig_max_x

		if(x_left)  y_inv = 1  else  y_inv = -1
		if(y_down)  x_inv = -1  else  x_inv = 1

		y_coord_alpha = x_coord_alpha
		x_coord_alpha = 0
	} else {
		if(x_left)  x_inv = -1  else  x_inv = 1
		if(y_down)  y_inv = -1  else  y_inv = 1

		y_coord_alpha = 0
	}

	if(x_reverse_polarity)  x_inv = -1 * x_inv
	if(y_reverse_polarity)  y_inv = -1 * y_inv

	max_x = max_x + (x_inv * x_shift)
	min_x = min_x + (x_inv * x_shift)

	max_y = max_y + (y_inv * y_shift)
	min_y = min_y + (y_inv * y_shift)

	xdim = max_x-min_x+1

	# non-default X or Y axis information, so need to
	# suppress automatic axis and do it manually...
	#----------------------------------------------------
	plot(c(min_x-0.5,max_x+0.5),c(min_y-0.5,max_y+0.5),type="n",
	xlab="",ylab="",xaxt="n",yaxt="n")
	my_xaxp = par("xaxp")
	x0 = my_xaxp[1]
	x1 = my_xaxp[2]
	xt = my_xaxp[3]
	x_s = seq(x0,x1,by=(x1-x0)/xt)

	my_yaxp = par("yaxp")
	y0 = my_yaxp[1]
	y1 = my_yaxp[2]
	yt = my_yaxp[3]
	y_s = seq(y0,y1,by=(y1-y0)/yt)

	if (x_coord_alpha)  x_chars = coord2char(x_inv * x_s)
	else 				x_chars = as.character(x_inv * x_s)
	Axis(at=x_s,labels=x_chars,side=1)

	if (y_coord_alpha)  y_chars = coord2char(y_inv * y_s)
	else 				y_chars = as.character(y_inv * y_s)
	Axis(at=y_s,labels=y_chars,side=2)

	#browser()
	if (is.finite(degrees)) {
		if (degrees==0.0) {
			text((min_x+max_x)/2.0,min_y-0.5,"NOTCH",pos=1)		# pos=3 above
		} else if (degrees==180) {
			text((min_x+max_x)/2.0,max_y+0.5,"NOTCH",pos=3)		# pos=1 below
		} else if (degrees==90) {
			text(max_x+0.5,(min_y+max_y)/2.0,"NOTCH",adj=c(0.5,1.0),srt=90)		# pos=2 left
		} else if (degrees==270) {
			text(min_x-0.5,(min_y+max_y)/2.0,"NOTCH",adj=c(0.5,0.0),srt=90)		# pos=4 right
		}
	}
	grid()

	results_map[is.finite(results_map) & (results_map>breaks[100])] = breaks[100]
	results_map[is.finite(results_map) & (results_map<=breaks[1])] = 
			(breaks[1]+breaks[2])/2.0
	cut_results = cut(results_map,breaks,include.lowest=TRUE)
	my_levels = levels(cut_results)
	#browser()
	for( i in 1:length(my_levels)) {
		die = which(cut_results==my_levels[i])
		if (length(die)>0) {
			x = min_x + ((die-1) %% xdim)
			y = min_y + floor((die-1)/xdim)
			if (do_borders)  my_border = NULL
			else  my_border = my_colors[i]
			#rect(x-0.4,y-0.4,x+0.4,y+0.4,col=my_colors[i])
			rect(x-0.4,y-0.4,x+0.4,y+0.4,col=my_colors[i],border=my_border)
		}
	}
	

    if (to_pdf) {
		close.screen(all=TRUE)
		new_device_info = dev.off()   	# close pdf file
    }


    timestamp9 = proc.time()
    timestamp9 = timestamp9[3] - timestamp0
	cat(sprintf("...generated wafermap in %.2f seconds \n",
        timestamp9))
}

##############################################################################
matrix_rotate <- function(my_matrix,ccw_degrees) {

	# assumes origin is LL (ie x+ to right, y+ up)
	# NOTE: R displays matrices as x+ to right, y+ down!!!
	if (ccw_degrees==0) {
		# nothing to change
	} else if (ccw_degrees==90) {
		my_matrix = t(my_matrix[,ncol(my_matrix):1])
	} else if (ccw_degrees==180) {
		my_matrix = my_matrix[nrow(my_matrix):1,ncol(my_matrix):1]
	} else if (ccw_degrees==270) {
		my_matrix = t(my_matrix[nrow(my_matrix):1,])
	} else {
		# print nasty message, do nothing
	}

	return( my_matrix )
}


##############################################################################
coord2char <- function(ints) {

    # expect only positive integers, so map anything <0 to 0
    # but this gets confused with Z's, so make NAs for now...
    ints[ints<1] = NA


    # 1->A, 2->B,... 26->Z,27->AA,...o
    # note: even though it is modulo 26, since there
    #       is no 0, there is no 10, Z is the 10, AA is the 11
    #       so subtract 1 and map 0->A, etc, to keep modulus simple
    #----------------------------------------------------------
    ints = ints - 1
    msd = floor(ints/26)
    msd[!is.finite(msd)] = 0		#convert NA's to 0's
    lsd = ints%%26			# least significant digit
    lsd[!is.finite(lsd)] = -1		# convert NA's to -1's

    c_lsd = rawToChar(as.raw(lsd+65))	# 0->A, 1->B,...25->Z
    c_lsd = strsplit(c_lsd,"")		# rawToChar puts into single string!
    c_lsd = c_lsd[[1]]			# convert from list to vector
    c_lsd[c_lsd=="@"] = ""		# 0's became -1's became "@"s, make "" ie empty

    chars = c_lsd

    # if need to add additional significant digits...
    while (max(msd)>0) {
	
	ints = msd - 1
	ints[ints<0] = NA	    # make zeros NA's so they don't become Z's
	msd = floor(ints/26)
	msd[!is.finite(msd)] = 0
	lsd = ints%%26		    # least significant digit
	lsd[!is.finite(lsd)] = -1   # turn NA's back to -1s now.
	c_lsd = rawToChar(as.raw(lsd+65))
	c_lsd = strsplit(c_lsd,"")
	c_lsd = c_lsd[[1]]
	c_lsd[c_lsd=="@"] = ""

	chars = paste(c_lsd,chars,sep="")
    }

    return( chars )
}


#############################################################################
#  copy local functions to the .StackedWaferMap.env and remove them
#  from the global environment
#############################################################################
assign("coord2char",coord2char,envir=.StackedWaferMap.env)
rm(coord2char)

assign("matrix_rotate",matrix_rotate,envir=.StackedWaferMap.env)
rm(matrix_rotate)

environment(StackedWaferMap)<-.StackedWaferMap.env

