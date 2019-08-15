# WaferMap.R
#
# $Id: WaferMap.R,v 1.24 2019/08/15 22:16:32 david Exp $
#
# reads in rtdf file(s) and generates wafermap(s)
#
# Copyright (C) 2006-2019 David Gattrell
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
if(exists(".WaferMap.env")) rm(.WaferMap.env) 
.WaferMap.env <- new.env()



WaferMap <- function(rtdf_name="",pdf_name="wafer_map.pdf",type="sbin",
		    x_coord_alpha=FALSE,panel=FALSE,x_left=FALSE,
		    y_down=FALSE,parameter="",pass_bins=-1,rtdf_dir="",
			notch="",
			rotate_ccw=0,x_reverse_polarity=0,y_reverse_polarity=0,
			x_shift=0,y_shift=0,
			param_col_start=0.03,param_col_end=0.24,param_col_rev_flag=FALSE,
			bin_vs_colors="",bin_vs_colors_path="",generate_bins_file=0,
			borders_off=7000,bin_nums_in_map=FALSE) {
    # rtdf_name -- name of the rtdf file to read
    # pdf_name -- name of the pdf file to generate
    # type = "sbin"  or "hbin" or "parameter" 
    # x_coord_alpha -- if from Tapestry... show X as A,B,C ... not 1,2,3,...
    # panel -- if strip handler panel, then use landscape page orientation
    # x_left -- flip wafermap: incrementing X goes right to left
    # y_down -- flip wafermap: incrementing Y goes top to bottom
    # parameter -- testname to use for gradient map (if type = "parameter")
	#              if type is "parameter" and this is "", do maps for all tests
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
	#   for parametric wafer maps...
	# param_col_start -- by default, if doing parametric wafer maps, the color starts
	#              at red for the lowest value.  [0.03]  Setting this overrides that
	#              default
	# param_col_end -- by default, if doing parametric wafer maps, the color ends
	#              at green for the highest value.  [0.24]  Setting this overrides
	#              that default
	# param_col_rev_flag -- by default, the colors increase in value.  setting this
	#              to true reverses the color gradient.
	#              The above 3 inputs are applied to the R rainbow() function.
	#----------------------------------
	# bin_vs_colors -- if this file is specified, it will be read in and used to
	#              assign the bin vs color mapping.  This would give you
	#              consistent color assignments across wafermaps
	# bin_vs_colors_path -- if non-empty, this is the path to the bin_vs_colors file
	# generate_bins_file -- if true, will create a csv file of the format used
	#              by bin_vs_colors with the bin/color mapping used for this wafermap
	#              (pdfname with either _hbins.csv or _sbins,csv extension vs .pdf)
	# borders_off -- If die count is >= borders_off, then turn off border
	#            that is drawn around each die in the wafer map
	#            if -1, always do border
	#            if 0, always suppress border
	# bin_nums_in_map -- If true, the soft or hard bin numbers will be printed inside each die
	#              in the wafer map
    ### other things not yet done... ###
    # - minx, maxx, miny, maxy to override wafer map size
    # - add vector for gaps for panels.. X or X and Y??


	# scaler prefix definitions
    #------------------------------
    big_prefixes = c("10x","100x","K","10xK","100xK","M","10xM","100xM",
		    "G","10xG","100xG","T")
    lil_prefixes = c("d","c","m","100xu","10xu","u","100xn","10xn","n",
		    "100xp","10xp","p","100xf","10xf","f")


	# colour definitions (9 each, or 8 + rest)
	#------------------------------------------
	pass_colors = c("green","forestgreen","springgreen","olivedrab4","seagreen",
				"limegreen","palegreen4","palegreen","darkgreen")
	fail_colors = c("yellow","orange","red","cyan","pink","purple","brown4",
				"blue","hotpink")
	more_colors = c("sienna","tan","orangered","orchid")
	pass_color_bins = rep(NA,length(pass_colors))
	fail_color_bins = rep(NA,length(fail_colors))
	more_color_bins = rep(NA,length(more_colors))


	# if bin_vs_colors defined, potentially override colors
	#------------------------------------------------------
	if (bin_vs_colors != "" && ((type=="sbin")||(type=="hbin"))  ) {
		# only bother parsing if map type is sbin or hbin

		if (bin_vs_colors_path != "") {
			my_dir = getwd()
			setwd(bin_vs_colors_path)
		}
		# need to load csv file... bin#,pass/fail,color,binname
		# - first line will be header line   "bin_num,pass_fail,color,bin_name"
		CSV <- gzfile(bin_vs_colors,"r")
		row1 <- scan(file=CSV,what=character(0),sep=",",strip.white=TRUE,
				multi.line=FALSE,fill=TRUE,nlines=1,quiet=TRUE,na.strings="NaN")
		cols = length(row1)
		# read in the rest of the lines...
		my_list <- scan(file=CSV,what=as.list(character(cols)),sep=",",
				strip.white=TRUE,multi.line=FALSE,fill=TRUE,na.strings="NaN")
		close(CSV)
		if (bin_vs_colors_path != "") {
			setwd(my_dir)
		}

		cells = unlist(my_list)
		cell_count = length(cells)
		rows = cell_count/cols

		bins_csv = matrix(cells,nrow=rows,ncol=cols,byrow=FALSE)

		predef_bins = as.integer(bins_csv[,1])
		predef_colors = as.character(bins_csv[,3])
		# lazy.. could sanity check sbin name and P/F flag.. or just ignore for now

		avail_pass_colors = pass_colors[which(!pass_colors %in% predef_colors)]
		avail_fail_colors = fail_colors[which(!fail_colors %in% predef_colors)]

	}


    # timestamp...
    #-----------------
    timestamp0 = proc.time()
    timestamp0 = timestamp0[3]
    timestamp1 = timestamp0
    now = substr(as.character(Sys.time()),1,19)


	# if filenames not defined, prompt for them
    #------------------------------------------
    if (length(rtdf_name)<2 && (rtdf_name == "")) {
		rtdf_name <- readline("Enter the name of the RTDF file to read: ")
    }

	if (rtdf_dir != "") {
		my_dir = getwd()
		setwd(rtdf_dir)
	}
    my_objs = load(rtdf_name)
	if (rtdf_dir != "")  setwd(my_dir)

    # check if extra wafer info or multiple wafers...
    # does WaferInfoFrame exist?  dim(WafersFrame)>1??

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


    # if parametric wafer map, find test index
    #------------------------------------------
    if (type=="parameter") {
		param_index = match(parameter,ParametersFrame[["testname"]],nomatch=NaN)
		#browser()
		if (!is.finite(param_index) && nchar(parameter)>0) {
			# 'feature' of tclvalue()?  removes trailing whitespace
			# if we didn't find a match, can we if we consider trailing whitespace?
			my_text = sprintf("^%s[[:blank:]]*$",parameter)
			param_index = grep(my_text,ParametersFrame[["testname"]])
			if (length(param_index)>0) {
				param_index = param_index[1]
			} else {
				param_index = NaN
			}
			#browser()
		}
		all_results = ResultsMatrix[,param_index]
		units = ParametersFrame[[param_index,"units"]]
    }


    # single or multiple wafer plots?
    #---------------------------------
    if (exists("WafersFrame",inherits=FALSE)) {
		#wafers_count = dim(WafersFrame)
		#wafers_count = wafers_count[1]
		all_wi = as.integer(DevicesFrame[,"wafer_index"])
		all_wi = all_wi[is.finite(all_wi)]
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



	# add for loop here if parameters and parameter name is empty..
	# then loop through all tests, do parametric wafer maps...
	if (type=="parameter" && !is.finite(param_index)) {
		param_index = 1
		max_param_index = dim(ParametersFrame)[1]
	} else if (type=="parameter") {
		max_param_index = param_index
	} else {
		param_index = 1
		max_param_index = param_index
	}
	page_number = 0
	max_pages = (max_param_index - param_index + 1)*wafers_count
	for (p_idx in param_index:max_param_index) {
		all_results = ResultsMatrix[,p_idx]
		units = ParametersFrame[[p_idx,"units"]]
		parameter = ParametersFrame[[p_idx,"testname"]]
		for (wafer in 1:wafers_count) {
			if (page_number>0) {
				if (!to_pdf)  ick <- readline("Enter CR to continue...")
				close.screen(all=TRUE)
				split.screen(figs)
			}
			page_number = page_number + 1

			# print progress report to screen every 5 seconds
			#--------------------------------------------------
			timestamp2 = proc.time()
			timestamp2 = timestamp2[3]
			if (timestamp2>(timestamp1+5)) {
				timestamp1 = timestamp2
				cat(sprintf("...now generating map %d of %d ...\n",
					page_number,max_pages))
			}


			# extract x,y, and sbin info from Frame
			#---------------------------------------
			if (exists("WafersFrame",inherits=FALSE)) {
				indxs = which(all_wi==unique_wis[wafer])
				xs = all_xs[indxs]
				ys = all_ys[indxs]
				sbins = all_sbins[indxs]
				hbins = all_hbins[indxs]
				if (type=="parameter")  results = all_results[indxs]
			} else {
				xs = all_xs
				ys = all_ys
				sbins = all_sbins
				hbins = all_hbins
				if (type=="parameter")  results = all_results
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
		

			# find min,max of x,y...
			#-------------------------------------------
			min_x = min(xs)
			max_x = max(xs)
			min_y = min(ys)
			max_y = max(ys)


			# build wafer map in array
			#----------------------------------------------
			my_map = array(NaN,dim=c(max_x-min_x+1,max_y-min_y+1))
			ixy = array(c(xs-min_x+1,ys-min_y+1),dim=c(length(sbins),2))
			my_map[ixy]=sbins
			my_hbin_map = array(NaN,dim=c(max_x-min_x+1,max_y-min_y+1))
			my_hbin_map[ixy]=hbins
			if (type=="parameter") {
				results_map = array(NaN,dim=c(max_x-min_x+1,max_y-min_y+1))
				results_map[ixy]=results
			}

			# determine number of unique sbins and count for each sbin
			#----------------------------------

			if ((type=="sbin")&&(length(which(is.finite(all_sbins)))>0)) {
				sbin_summary = as.data.frame(table(my_map))
				sbins = as.numeric(levels(sbin_summary[,1]))
				counts = sbin_summary[,2]
				total_die = sum(counts)

				# separate out PASSes from list
				if(length(which(sbins %in% my_pass_bins))>0) {		
					i_bin1s = which(sbins %in% my_pass_bins)
					good_sbins = sbins[i_bin1s]
					good_counts = counts[i_bin1s]
					good_die = sum(good_counts)
					sbins = sbins[-i_bin1s]
					counts = counts[-i_bin1s]
				} else {
					good_sbins = numeric()
					good_counts = numeric()
					good_die=0
				}
				# if overriding the bin colours,...
				# .. multiple bins may be grouped into "other"
				if (bin_vs_colors != "") {
				}

				#sort list
				sorted=sort(counts,decreasing=TRUE,index.return=TRUE)
				xrefs = sorted$ix
			} else if (type=="sbin") { 
				# oh dear, shouldn't be here...
				#REVISIT...
				cat("ERROR: asking for Softbin map, but no soft bin data in file!")
				good_die=0
			} else if ((type=="hbin")&&(length(which(is.finite(all_hbins)))>0)) { 
				hbin_summary = as.data.frame(table(my_hbin_map))
				hbins = as.numeric(levels(hbin_summary[,1]))
				counts = hbin_summary[,2]
				total_die = sum(counts)

				# separate out bin1 from list
				if(length(which(hbins %in% my_pass_bins))>0) {
					i_bin1s = which(hbins %in% my_pass_bins)
					good_hbins = hbins[i_bin1s]
					good_counts = counts[i_bin1s]
					good_die = sum(good_counts)
					hbins = hbins[-i_bin1s]
					counts = counts[-i_bin1s]
				} else {
					good_hbins = numeric()
					good_counts = numeric()
					good_die=0
				}
				#sort list
				sorted=sort(counts,decreasing=TRUE,index.return=TRUE)
				xrefs = sorted$ix
			} else if (type=="hbin") { 
				# oh dear, shouldn't be here...
				#REVISIT...
				cat("ERROR: asking for Hardbin map, but no hard bin data in file!")
				good_die=0
			} else {  # .. set good_die for parameters type too.
				if(length(which(is.finite(all_hbins)))>0) {
					hbin_summary = as.data.frame(table(my_hbin_map))
					hbins = as.numeric(levels(hbin_summary[,1]))
					counts = hbin_summary[,2]
					total_die = sum(counts)

					# separate out bin1 from list
					if(length(which(hbins %in% my_pass_bins))>0) {
						i_bin1s = which(hbins %in% my_pass_bins)
						good_hbins = hbins[i_bin1s]
						good_counts = counts[i_bin1s]
						good_die = sum(good_counts)
						hbins = hbins[-i_bin1s]
						counts = counts[-i_bin1s]
					} else {
						good_die=0
					}
					#sort list
					sorted=sort(counts,decreasing=TRUE,index.return=TRUE)
					xrefs = sorted$ix
				} else if(length(which(is.finite(all_sbins)))>0) {
					sbin_summary = as.data.frame(table(my_map))
					sbins = as.numeric(levels(sbin_summary[,1]))
					counts = sbin_summary[,2]
					total_die = sum(counts)

					# separate out PASSes from list
					if(length(which(sbins %in% my_pass_bins))>0) {		
						i_bin1s = which(sbins %in% my_pass_bins)
						good_sbins = sbins[i_bin1s]
						good_counts = counts[i_bin1s]
						good_die = sum(good_counts)
						sbins = sbins[-i_bin1s]
						counts = counts[-i_bin1s]
					} else {
						good_sbins = numeric()
						good_counts = numeric()
						good_die=0
					}
					#sort list
					sorted=sort(counts,decreasing=TRUE,index.return=TRUE)
					xrefs = sorted$ix
				} else {
					good_die=0
				}
			}
			if( (total_die<borders_off)||(borders_off<0) )  do_borders = TRUE
			else  do_borders = FALSE


			# do text stuff in top portion
			#------------------------------
			screen(1)
			# print lot id, sublot id
			if (exists("WafersFrame",inherits=FALSE)) {
				my_title = sprintf("LOT:%s   SUBLOT:%s  WAFER:%s",
					as.character(LotInfoFrame[[1,"lotid"]]),
					as.character(LotInfoFrame[[1,"sublotid"]]),
					as.character(WafersFrame[[wafer,"wafer_id"]])
					)
			} else {
				my_title = sprintf("LOT:%s   SUBLOT:%s",
					as.character(LotInfoFrame[[1,"lotid"]]),
					as.character(LotInfoFrame[[1,"sublotid"]])
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
			
			if (generate_bins_file) {
				# if multiple wafers, clear vector at start of each wafer,
				# you will just get the bins assignment for the last wafer,
				# rather than mess of conflicting assignments
				bins_csv_lines <- vector()
			}

			if (type=="sbin") {
				if  (length(good_sbins)>1) {
					if ( (length(good_sbins) + length(sbins)) >7) shrink=TRUE
					else  shrink=FALSE
				} else {
					if ( (length(good_sbins) + length(sbins)) >9) shrink=TRUE
					else  shrink=FALSE
				}
				row=9
				col=0
				if (shrink) {
					wid = 0.02
					font = 0.7
				} else {
					wid = 0.04
					font = 1.0
				}
				if  (length(good_sbins)>1) {
					text(col+wid+0.01,(row-0.5)/10,sprintf("PASS Softbins"),pos=4,cex=font)
					row = row - 1
				}
				if(length(good_sbins)>9)  max_i=9  else  max_i=length(good_sbins)
				if(max_i>0) {
					i_ap = 0;	# index to avail_pass_colors vector
					ran_out_of_colors = FALSE

					for (i in 1:max_i) {
						bin = good_sbins[i]
						if(bin_vs_colors != "") {
							idx = which(predef_bins == bin)
							if(length(idx) > 0) {
								my_color = bins_csv[idx[1],3]
							} else {
								# move to next unassigned pass bin color
								i_ap = i_ap + 1
								if(i_ap>=length(avail_pass_colors)) {
									i_ap = length(avail_pass_colors)
									ran_out_of_colors = true;
								}
								my_color = avail_pass_colors[i_ap]
							}
						} else {
							my_color = pass_colors[i]
						}
						rect(col,(row-0.9)/10,col+wid,(row-0.1)/10,col=my_color)
						if ( ran_out_of_colors || 
							 ( (i==9) && (length(good_sbins)>9) ) ) {
							sbin_nam = "the rest"
							goods_left = sum(good_counts[-(1:8)])
							# REVISIT strwidth() to get length of string in plot units,
							# may adjust cex for really long names, or truncate? or both??
							text(col+wid+0.01,(row-0.5)/10,sprintf("sbin xxxx: %4d (%5.1f%%) %s",
									goods_left,
									100.0*goods_left/total_die,
									sbin_nam),pos=4,cex=font)
						} else {
							if (exists("SbinInfoFrame",inherits=FALSE)) {
								j = match(bin,SbinInfoFrame[["sbin_num"]],nomatch=0)
								if(j>0)  sbin_nam = SbinInfoFrame[[j,"sbin_nam"]]
								else  sbin_nam = "PASSES"
							} else {
								sbin_nam = "PASSES"
							}
							text(col+wid+0.01,(row-0.5)/10,sprintf("sbin %4d: %4d (%5.1f%%) %s",
									bin,
									good_counts[i],
									100.0*good_counts[i]/total_die,
									sbin_nam),pos=4,cex=font)
							if(generate_bins_file) {
								my_str = sprintf("%d,P,%s,%s",bin,my_color,sbin_nam)
								bins_csv_lines = c(bins_csv_lines,my_str)
							}
						}
						row = row - 1
						if (row<1) {
							row = 9
							col = 0.5
						}
					}
				}
				if  (length(good_sbins)>1) {
					text(col+wid+0.01,(row-0.5)/10,sprintf("FAIL Softbins"),pos=4,cex=font)
					row = row - 1
					if (row<1) {
						row = 9
						col = 0.5
					}
				}
				remaining_die = total_die - good_die

				# determine how many unique fail bins we can print in remaining text header
				# max size of 2col x 9row
				#   minus pass bin(s)
				#   if >1 pass bin, minus 2 for pass/fail bin headers
				if(shrink) {
					slots_left = 18		# smaller font, 2 columns
				} else {
					slots_left = 9		
				}
				if(length(good_sbins)>1) {
					slots_left = slots_left - 2		# pass/fail bin header lines
					if (length(good_sbins)>9) {
						slots_left = slots_left - 9
					} else {
						slots_left = slots_left - length(good_sbins)
					}
				}

				#if(length(xrefs)>9)  max_i=9  else  max_i=length(xrefs)
				if(length(xrefs)>slots_left)  max_i=slots_left  else  max_i=length(xrefs)
				if(max_i>9)  max_i=9
				if(max_i>0) {
					i_af = 0;	# index to avail_fail_colors vector
					ran_out_of_colors = FALSE

					for (i in 1:max_i) {
						bin = sbins[xrefs[i]]
						if(bin_vs_colors != "") {
							idx = which(predef_bins == bin)
							if(length(idx) > 0) {
								my_color = bins_csv[idx[1],3]
							} else {
								# move to next unassigned fail bin color
								i_af = i_af + 1
								if(i_af>=length(avail_fail_colors)) {
									i_af = length(avail_fail_colors)
									ran_out_of_colors = true;
								}
								my_color = avail_fail_colors[i_af]
							}
						} else {
							my_color = fail_colors[i]
						}
						rect(col,(row-0.9)/10,col+wid,(row-0.1)/10,col=my_color)
						#if ((i==9) && (length(xrefs)>9)) {
						if ( ran_out_of_colors ||
							 ( (i==max_i) && (length(xrefs)>max_i) ) ) {
							sbin_nam = "the rest"
							text(col+wid+0.01,(row-0.5)/10,sprintf("sbin xxxx: %4d (%5.1f%%) %s",
									remaining_die,
									100.0*remaining_die/total_die,
									sbin_nam),pos=4,cex=font)
						} else {
							if (exists("SbinInfoFrame",inherits=FALSE)) {
								j = match(bin,SbinInfoFrame[["sbin_num"]],nomatch=0)
								if(j>0)  sbin_nam = SbinInfoFrame[[j,"sbin_nam"]]
								else  sbin_nam = ""
							} else {
								sbin_nam = ""
							}
							text(col+wid+0.01,(row-0.5)/10,sprintf("sbin %4d: %4d (%5.1f%%) %s",
									bin,
									counts[xrefs[i]],
									100.0*counts[xrefs[i]]/total_die,
									sbin_nam),pos=4,cex=font)
							if(generate_bins_file) {
								my_str = sprintf("%d,F,%s,%s",bin,my_color,sbin_nam)
								bins_csv_lines = c(bins_csv_lines,my_str)
							}
							remaining_die = remaining_die - counts[xrefs[i]]
						}
						row = row - 1
						if (row<1) {
							row = 9
							col = 0.5
						}
					}
				}
			} else if (type=="hbin") {
				if  (length(good_hbins)>1) {
					if ( (length(good_hbins) + length(hbins)) >7) shrink=TRUE
					else  shrink=FALSE
				} else {
					if ( (length(good_hbins) + length(hbins)) >9) shrink=TRUE
					else  shrink=FALSE
				}
				row=9
				col=0
				if (shrink) {
					wid = 0.02
					font = 0.7
				} else {
					wid = 0.04
					font = 1.0
				}
				if  (length(good_hbins)>1) {
					text(col+wid+0.01,(row-0.5)/10,sprintf("PASS Hardbins"),pos=4,cex=font)
					row = row - 1
				}
				if(length(good_hbins)>9)  max_i=9  else  max_i=length(good_hbins)
				if(max_i>0) {
					i_ap = 0;	# index to avail_pass_colors vector
					ran_out_of_colors = FALSE
					
					for (i in 1:max_i) {
						bin = good_hbins[i]
						if(bin_vs_colors != "") {
							idx = which(predef_bins == bin)
							if(length(idx) > 0) {
								my_color = bins_csv[idx[1],3]
							} else {
								# move to next unassigned pass bin color
								i_ap = i_ap + 1
								if(i_ap>=length(avail_pass_colors)) {
									i_ap = length(avail_pass_colors)
									ran_out_of_colors = true;
								}
								my_color = avail_pass_colors[i_ap]
							}
						} else {
							my_color = pass_colors[i]
						}
						rect(col,(row-0.9)/10,col+wid,(row-0.1)/10,col=my_color)
						if ( ran_out_of_colors || 
							 ( (i==9) && (length(good_hbins)>9) ) ) {
							hbin_nam = "the rest"
							goods_left = sum(good_counts[-(1:8)])
							text(col+wid+0.01,(row-0.5)/10,sprintf("hbin xx: %4d (%5.1f%%) %s",
									goods_left,
									100.0*goods_left/total_die,
									hbin_nam),pos=4,cex=font)
						} else {
							if (exists("HbinInfoFrame",inherits=FALSE)) {
								j = match(bin,HbinInfoFrame[["hbin_num"]],nomatch=0)
								if(j>0)  hbin_nam = HbinInfoFrame[[j,"hbin_nam"]]
								else  hbin_nam = "PASSES"
							} else {
								hbin_nam = "PASSES"
							}
							text(col+wid+0.01,(row-0.5)/10,sprintf("hbin %2d: %4d (%5.1f%%) %s",
									bin,
									good_counts[i],
									100.0*good_counts[i]/total_die,
									hbin_nam),pos=4,cex=font)
							if(generate_bins_file) {
								my_str = sprintf("%d,P,%s,%s",bin,my_color,hbin_nam)
								bins_csv_lines = c(bins_csv_lines,my_str)
							}
						}
						row = row - 1
						if (row<1) {
							row = 9
							col = 0.5
						}
					}
				}
				remaining_die = total_die - good_die
				if(length(xrefs)>9)  max_i=9  else  max_i=length(xrefs)
				if(max_i>0) {
					i_af = 0;	# index to avail_fail_colors vector
					ran_out_of_colors = FALSE

					for (i in 1:max_i) {
						bin = hbins[xrefs[i]]
						if(bin_vs_colors != "") {
							idx = which(predef_bins == bin)
							if(length(idx) > 0) {
								my_color = bins_csv[idx[1],3]
							} else {
								# move to next unassigned fail bin color
								i_af = i_af + 1
								if(i_af>=length(avail_fail_colors)) {
									i_af = length(avail_fail_colors)
									ran_out_of_colors = true;
								}
								my_color = avail_fail_colors[i_af]
							}
						} else {
							my_color = fail_colors[i]
						}
						rect(col,(row-0.9)/10,col+wid,(row-0.1)/10,col=my_color)
						if ( ran_out_of_colors ||
						     ( (i==9) && (length(xrefs)>9) ) ) {
							hbin_nam = "the rest"
							text(col+wid+0.01,(row-0.5)/10,sprintf("hbin xx: %4d (%5.1f%%) %s",
									remaining_die,
									100.0*remaining_die/total_die,
									hbin_nam),pos=4,cex=font)
						} else {
							if (exists("HbinInfoFrame",inherits=FALSE)) {
								j = match(bin,HbinInfoFrame[["hbin_num"]],nomatch=0)
								if(j>0)  hbin_nam = HbinInfoFrame[[j,"hbin_nam"]]
								else  hbin_nam = ""
							} else {
								hbin_nam = ""
							}
							text(col+wid+0.01,(row-0.5)/10,sprintf("hbin %2d: %4d (%5.1f%%) %s",
									bin,
									counts[xrefs[i]],
									100.0*counts[xrefs[i]]/total_die,
									hbin_nam),pos=4,cex=font)
							if(generate_bins_file) {
								my_str = sprintf("%d,F,%s,%s",bin,my_color,hbin_nam)
								bins_csv_lines = c(bins_csv_lines,my_str)
							}
							remaining_die = remaining_die - counts[xrefs[i]]
						}
						row = row - 1
						if (row<1) {
							row = 9
							col = 0.5
						}
					}
				}
			} else if (type=="parameter") {

				# determine range to plot...
				# scale range to go from -3 robust sd to +3 robust sd
				# if min/max results are tighter than this, use them
				#----------------------------------------------------- 
				my_results = results[is.finite(results)]
				if(length(my_results)<1)  valid_map=FALSE
				else  valid_map=TRUE
				if(valid_map) {
					my_q25 = quantile(my_results,0.25)
					my_q50 = median(my_results)
					my_q75 = quantile(my_results,0.75)
					r_sdev = abs(my_q75 - my_q25)/1.34898
					my_min = min(my_results)
					my_max = max(my_results)
					z0 = my_q50 - 3*r_sdev
					z1 = my_q50 + 3*r_sdev
					if (my_min>z0)  z0 = my_min
					if (my_max<z1)  z1 = my_max
				
					if (z1>z0) {
						breaks = z0 + (z1 - z0)*seq(0,1,len=100)
					} else if (my_max>my_min) {
						breaks = my_min + (my_max - my_min)*seq(0,1,len=100)
					} else {
						breaks = my_min + seq(0,1,len=100)
					}
					# my_colors = rainbow(100,start=0.03,end=0.24)
					# my_colors = grey(seq(0,1,len=100))    # greyscale vs color...
					my_colors = rainbow(100,start=param_col_start,end=param_col_end)
					if(param_col_rev_flag)  my_colors = rev(my_colors)
		
		
					# print out parameter name being used,
					# then green square with max value >=
					# red square with min value <=
		
					# determine scaling...
					if(abs(z0)>abs(z1))  range=abs(z0)
					else  range=abs(z1)
					if(range>0.99e9)       scaler=-9
					else if(range>0.99e6)  scaler=-6
					else if(range>0.99e3)  scaler=-3
					else if(range>0.99)     scaler=0
					else if(range>0.99e-3)  scaler=3
					else if(range>0.99e-6)  scaler=6
					else if(range>0.99e-9)  scaler=9
					else   scaler=12
		
					if (scaler<0) {
						prefix = big_prefixes[-1*scaler]
					} else if (scaler==0) {
						prefix = ""
					} else {
						prefix = lil_prefixes[scaler]
					}
					scale = 10^scaler
					my_units = paste(prefix,units,sep="")
				}
				text(0.07,6.5/8,sprintf("Parameter: %s",parameter),pos=4,cex=1.0)
		
				if(valid_map) {
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
				}
			}
		
		
			# do wafer map in bottom portion
			#--------------------------------
			screen(2)
			par(mar=c(4.1,4.1,0.5,0.5))

			# NOTE: if x_left has been set, then x has been *-1
			#       if y_down has been set, then y has been *-1
			orig_min_x = min_x
			orig_min_y = min_y
			orig_max_x = max_x
			orig_max_y = max_y

			orig_sbin_map = my_map
			orig_hbin_map = my_hbin_map
			if (type=="parameter") {
				orig_param_map = results_map
			}

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

			my_map = matrix_rotate(my_map,rotate_ccw)
			my_hbin_map = matrix_rotate(my_hbin_map,rotate_ccw)
			if (type=="parameter") {
				results_map = matrix_rotate(results_map,rotate_ccw)
			}
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
		
			if (type=="sbin") {
				i_ap = 0
				i_af = 0

				for( i in 1:length(good_sbins)) {
					bin1s=which(my_map==good_sbins[i])
					x= min_x + ((bin1s-1) %% xdim)
					y= min_y + floor((bin1s-1)/xdim)
					if(bin_vs_colors != "") {
						idx = which(predef_bins == good_sbins[i])
						if(length(idx) > 0) {
							my_col = bins_csv[idx[1],3]
						} else {
							# move to next unassigned pass bin color
							i_ap = i_ap + 1
							if(i_ap>=length(avail_pass_colors)) {
								i_ap = length(avail_pass_colors)
							}
							my_col = avail_pass_colors[i_ap]
						}
					} else {
						if (i>8)  my_col = pass_colors[9]
						else  my_col = pass_colors[i]
					}
					if (do_borders)  my_border = NULL
					else  my_border = my_col
					rect(x-0.4,y-0.4,x+0.4,y+0.4,col=my_col,border=my_border)
					if(bin_nums_in_map) {
						# need to scale font based on how big the die are...
						shrink_size = 1.0
						if( (max_x - min_x)>20) {
							shrink_size = 15.0/(1.0*(max_x - min_x))
						}
						text(x,y,sprintf("%d",good_sbins[i]),cex=shrink_size)
					}
				}
				for( i in 1:length(xrefs)) {
					fbins=which(my_map==sbins[xrefs[i]])
					x= min_x + ((fbins-1) %% xdim)
					y= min_y + floor((fbins-1)/xdim)
					if(bin_vs_colors != "") {
						idx = which(predef_bins == sbins[xrefs[i]])
						if(length(idx) > 0) {
							my_col = bins_csv[idx[1],3]
						} else {
							# move to next unassigned fail bin color
							i_af = i_af + 1
							if(i_af>=length(avail_fail_colors)) {
								i_af = length(avail_fail_colors)
							}
							my_col = avail_fail_colors[i_af]
						}
					} else {
						if (i>8)  my_col = fail_colors[9]
						else  my_col = fail_colors[i]
					}
					if (do_borders)  my_border = NULL
					else  my_border = my_col
					rect(x-0.4,y-0.4,x+0.4,y+0.4,col=my_col,border=my_border)
					if(bin_nums_in_map) {
						# need to scale font based on how big the die are...
						shrink_size = 1.0
						if( (max_x - min_x)>20) {
							shrink_size = 15.0/(1.0*(max_x - min_x))
						}
						text(x,y,sprintf("%d",sbins[xrefs[i]]),cex=shrink_size)
					}
				}
			} else if (type=="hbin") {
				i_ap = 0
				i_af = 0

				for( i in 1:length(good_hbins)) {
					bin1s=which(my_hbin_map==good_hbins[i])
					x= min_x + ((bin1s-1) %% xdim)
					y= min_y + floor((bin1s-1)/xdim)
					if(bin_vs_colors != "") {
						idx = which(predef_bins == good_hbins[i])
						if(length(idx) > 0) {
							my_col = bins_csv[idx[1],3]
						} else {
							# move to next unassigned pass bin color
							i_ap = i_ap + 1
							if(i_ap>=length(avail_pass_colors)) {
								i_ap = length(avail_pass_colors)
							}
							my_col = avail_pass_colors[i_ap]
						}
					} else {
						if (i>8)  my_col = pass_colors[9]
						else  my_col = pass_colors[i]
					}
					if (do_borders)  my_border = NULL
					else  my_border = my_col
					rect(x-0.4,y-0.4,x+0.4,y+0.4,col=my_col,border=my_border)
					if(bin_nums_in_map) {
						# need to scale font based on how big the die are...
						shrink_size = 1.0
						if( (max_x - min_x)>20) {
							shrink_size = 15.0/(1.0*(max_x - min_x))
						}
						text(x,y,sprintf("%d",good_hbins[i]),cex=shrink_size)
					}
				}
				for( i in 1:length(xrefs)) {
					fbins=which(my_hbin_map==hbins[xrefs[i]])
					x= min_x + ((fbins-1) %% xdim)
					y= min_y + floor((fbins-1)/xdim)
					if(bin_vs_colors != "") {
						idx = which(predef_bins == hbins[xrefs[i]])
						if(length(idx) > 0) {
							my_col = bins_csv[idx[1],3]
						} else {
							# move to next unassigned fail bin color
							i_af = i_af + 1
							if(i_af>=length(avail_fail_colors)) {
								i_af = length(avail_fail_colors)
							}
							my_col = avail_fail_colors[i_af]
						}
					} else {
						if (i>8)  my_col = fail_colors[9]
						else  my_col = fail_colors[i]
					}
					if (do_borders)  my_border = NULL
					else  my_border = my_col
					rect(x-0.4,y-0.4,x+0.4,y+0.4,col=my_col,border=my_border)
					if(bin_nums_in_map) {
						# need to scale font based on how big the die are...
						shrink_size = 1.0
						if( (max_x - min_x)>20) {
							shrink_size = 15.0/(1.0*(max_x - min_x))
						}
						text(x,y,sprintf("%d",hbins[xrefs[i]]),cex=shrink_size)
					}
				}
			} else if ((type=="parameter") && valid_map) {
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
			}
		}	# end of for wafers loop
	}	# end of for parameters loop
	
	if(generate_bins_file && ((type=="sbin")||(type=="hbin")) ) {
		# REVISIT... dump csv file with header line
		# buid name of csv file
		if(type=="sbin") {
			csv_name = paste(as.character(strsplit(pdf_name,"[.]pdf$")),"_sbins.csv",sep="")
		} else {
			csv_name = paste(as.character(strsplit(pdf_name,"[.]pdf$")),"_hbins.csv",sep="")
		}
		csv_conn = file(csv_name,"w")
		# add header
		cat("bin_num,pass_fail,color,binname",file=csv_conn)
		# print vector of strings bins_csv_lines
		for(j in 1:length(bins_csv_lines)) {
			the_string = sprintf("\n%s",bins_csv_lines[j])
			cat(the_string,file=csv_conn)
		}
		close(csv_conn)
	}

    if (to_pdf) {
		close.screen(all=TRUE)
		new_device_info = dev.off()   	# close pdf file
    }


    timestamp9 = proc.time()
    timestamp9 = timestamp9[3] - timestamp0
	if (page_number>1) {
		cat(sprintf("...generated %d wafermaps in %.2f seconds \n",
            page_number,timestamp9))
	} else {
		cat(sprintf("...generated %d wafermap in %.2f seconds \n",
            page_number,timestamp9))
	}
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
#  copy local functions to the .WaferMap.env and remove them
#  from the global environment
#############################################################################
assign("coord2char",coord2char,envir=.WaferMap.env)
rm(coord2char)

assign("matrix_rotate",matrix_rotate,envir=.WaferMap.env)
rm(matrix_rotate)

environment(WaferMap)<-.WaferMap.env

