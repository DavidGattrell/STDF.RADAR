# AsciiWaferMap.R
#
# $Id: AsciiWaferMap.R,v 1.10 2019/05/29 00:48:18 david Exp $
#
# reads in rtdf file(s) and generates ascii wafermap(s)
#
# Copyright (C) 2009-11,2015,2018,2019 David Gattrell
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




AsciiWaferMap <- function(rtdf_name="",wmap_name="wafer_map.wmap",type="sbin",
		    x_left=FALSE,y_down=FALSE,notch="S",pass_bins=-1,rtdf_dir="",
			test_floor="",product_id="",lot_id="",wafer_id="",do_yield=TRUE,
			multi_binning=FALSE, multi_bin_terse=FALSE,skip_die_minus=TRUE,
			mirror_die="",sinf_fmt=FALSE,x_step="",y_step="") {
    # rtdf_name -- name of the rtdf file to read
    # wmap_name -- name of the ascii file to generate  (xxx.wmap)
	#			if "", then "LOTID_waferWAFERID.wmap" will be used
	#			where LOTID is the lot_id, and WAFERID is the wafer_id
    # type = "sbin"  or "hbin"... which binning to use to determine "good" die 
    # x_left -- flip wafermap: incrementing X goes right to left
    # y_down -- flip wafermap: incrementing Y goes top to bottom
    # notch -- notch orientation (or major flat) of the rtdf file, one of "N","E","W",or "S"
	#			the wafer will be rotated so that notch is down for the text wafermap
	# pass_bins -- for sbin or hbin yield info, use the specified bin or
	#			  bins as pass bins, default is bin 1.
	#			  if -1 then 'automatic' mode...
	#			  ... if SbinInfoFrame or HbinInfoFrame exists, use sbin_pf flag, else...
	#			  ... for hbin, automatic mode is hbin=1 is pass
	#			  ... for sbin, automatic mode is sbins where hbin=1 are passes
	# rtdf_dir -- absolute path for rtdf file if not in current 
	#             directory/folder.. else ""
	# test_floor -- test floor id, overrides what is in rtdf...
	#			... note: rtdf does not currently have this field in the
	#			LotInfoFrame!
	# product_id -- overrides what is in rtdf... 
	#			... note: rtdf does not currently have this field in the
	#			LotInfoFrame!  use "part_typ" 
	# lot_id -- overrides what is in the rtdf if not ""
	# wafer_id -- overrides what is in the rtdf if not ""
	# do_yield -- append yield line to end of wmap file
	# multi_binning -- if TRUE, then map has binning numbers, rather than "1"s and
	#			"x"s in the map.  It also shows counts and percent per bin and
	#           it also gives overall good bins count if multiple bins flagged
	#           as pass bins.
	# multi_bin_terse -- if TRUE and multi_binning is true, map is "1"s, "2"s,
	#			etc. based on number of pass bins, and fails are all "x"s
	# skip_die_minus -- for die that are skipped and have surrounding tested die,
	#           use a "-" instead of a "." to indicate these locations
	# mirror_die -- if you want "M"s on the map to indicate mirror die,
	#           then give string of X Y coordinates for location(s) of M's
	#           (space separated)
	# sinf_fmt -- instead of .wmap format, output in a sinf version
	#           sinf header fields
	#           DEVICE: - fetch from LotInfoFrame[["part_typ"]]
	#           LOT:    - fetch from LotInfoFrame[["lotid"]]
	#           WAFER:  - fetch from WafersFrame[["wafer_id"]], if doesn't exist then... ?
	#           FNLOC:  - fetch from above input 'notch'
	#           ROWCT:  - calculated from DevicesFrame x_coord, y_coord
	#           COLCT:  - calculated from DevicesFrame x_coord, y_coord
	#           BCEQU:  - fetch from SbinInfoFrame
	#           REFPX:  - leave blank, with comment
	#           REFPY:  - leave blank, with comment
	#           DUTMS:  - hardcode as mm .. or check WaferInfoFrame? die_ht, die_wid, wf_units
	#           XDIES:  - variable below or if empty, pull from WaferInfoFrame?
	#           YDIES:  - similar to XDIESa
	#           also forces multi_binning to TRUE, multi_bin_terse to FALSE
	# x_step -- override for die X size, 
	#           if .wmap add field "Die X size",
	#           if .sinf override WaferInfoFrame value.. (as DUTMS, haven't coded WaferInfoFrame support!)
	# y_step -- similar to x_step
	#--------------------------------------------------------------------------



	# force specific map format if sinf_fmt
	if(sinf_fmt) {
		multi_binning = TRUE
		multi_bin_terse = FALSE
	}


	# process mirror_die if there are any
	#------------------------------------
	valid_m_die = FALSE
	if(nchar(mirror_die)>0) {
		coords = as.integer(strsplit(mirror_die," ")[[1]])
		coords = coords[is.finite(coords)]		# remove NA's from multiple white space

		if( (length(coords)>1) && ((length(coords) %% 2) ==0 )) {
			valid_m_die = TRUE
			mirror_xs = coords[c(TRUE,FALSE)]
			mirror_ys = coords[c(FALSE,TRUE)]
		} else {
			# nasty message	
			cat("ERROR: Odd number of Mirror die coordinates, input is ignored!")
		}
	}


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


    # single or multiple wafer plots?
    #---------------------------------
    if (is.finite(match("WafersFrame",my_objs))) {
		#wafers_count = dim(WafersFrame)[1]
		all_wi = as.integer(DevicesFrame[,"wafer_index"])
		unique_wafer_ids = unique(all_wi)
		wafers_count = length(unique_wafer_ids)
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


	override_lot_id = lot_id
	override_wafer_id = wafer_id

    for (wafer in 1:wafers_count) {
		


		# set lot_id and wafer_id
		#---------------------------
		lot_id = as.character(LotInfoFrame[[1,"lotid"]])
		sublot_id = as.character(LotInfoFrame[[1,"sublotid"]])
		if(is.finite(match("WafersFrame",my_objs))) {
			wafer = unique_wafer_ids[wafer]
			wafer_id = as.character(WafersFrame[[wafer,"wafer_id"]])
			if(nchar(sublot_id)>0)  lot_id = paste(lot_id,":",sublot_id,sep="")
		} else {
			cat("WARNING: wafer_id needs to be verified!\n")
			if(wafers_count>1) {
				wafer_id = as.character(wafer)
				if(nchar(sublot_id)>0)  lot_id = paste(lot_id,":",sublot_id,sep="")
			} else {
				wafer_id = sublot_id
			}
		}
		if(nchar(override_lot_id)>0)  lot_id = override_lot_id
		if(nchar(override_wafer_id)>0)  wafer_id = override_wafer_id

		# determine output file name
		#-----------------------------
		if(nchar(wmap_name)>0) {
			wafer_file = wmap_name
			if (wafers_count>1) {
				if(sinf_fmt) {
					txt_name = as.character(strsplit(wmap_name,"[.]sinf$"))
					wafer_file = paste(txt_name,"_wafer",wafer_id,".sinf",sep="")
				} else {
					txt_name = as.character(strsplit(wmap_name,"[.]wmap$"))
					wafer_file = paste(txt_name,"_wafer",wafer_id,".wmap",sep="")
				}
			}
		} else {
			wafer_file = paste(lot_id,"_wafer",wafer_id,sep="")
			# need to replace any ":" or "-" etc...
			wafer_file = gsub("[^0-9A-Za-z]","_",wafer_file)
			if(sinf_fmt) {
				wafer_file = paste(wafer_file,".sinf",sep="")
			} else {
				wafer_file = paste(wafer_file,".wmap",sep="")
			}
		}
		out_conn = file(wafer_file,"w")

		# extract x,y, and sbin info from Frame
		#---------------------------------------
		if (is.finite(match("WafersFrame",my_objs))) {
		    indxs = which(all_wi==wafer)
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
		valid_xs = which((xs>-32768)&(xs<32768))  # just to see if anyone is using -32768 :)
		valid_ys = which((ys>-32768)&(ys<32768))
	
		valid_dice = intersect(valid_xs,valid_ys)
	
		xs = xs[valid_dice]
		ys = ys[valid_dice]
		sbins = sbins[valid_dice]
		hbins = hbins[valid_dice]


		# remove any die that are later retested
		#-------------------------------------------
		xys = array(c(xs,ys),dim=c(length(xs),2))
		indices = which(duplicated(xys,fromLast=TRUE))
		if(length(indices)>0) {
			cat(sprintf("WARNING: %d die reprobed, ignoring initial probing\n",
						length(indices)))
			xs = xs[-indices]
			ys = ys[-indices]
			sbins = sbins[-indices]
			hbins = hbins[-indices]
		}


		# find min,max of x,y...
		#-------------------------------------------
	    min_x = min(xs)
		max_x = max(xs)
	    min_y = min(ys)
		max_y = max(ys)


		# separate into goods and bads...
		if (type=="sbin")  my_bins = sbins
		else  my_bins = hbins
		goods = my_bins %in% my_pass_bins
		bads = !goods
		good_i_s = which(goods)
		bad_i_s = which(bads)

		unique_bins = sort(unique(my_bins))
		if(max(unique_bins)>999)  digits=4
		else if(max(unique_bins)>99)  digits=3
		else  digits=2
		
		if(multi_binning && !multi_bin_terse) {
			if(digits>3) {
				if(sinf_fmt)  die_str = "____ "
				else          die_str = ".... "
				mirror_str = "MMMM "
			} else if(digits>2) {
				if(sinf_fmt)  die_str = "___ "
				else          die_str = "... "
				mirror_str = "MMM "
			} else {
				if(sinf_fmt)  die_str = "__ "
				else          die_str = ".. "
				mirror_str = "MM "
			}
		} else {
			if(sinf_fmt)  die_str = "_"
			else          die_str = "."
			die_str = "."
			mirror_str = "M"
		}


		# rotating map so that notch is at the bottom
		#--------------------------------------------
		if(notch=="N") {
			my_map = array(die_str,dim=c(max_x-min_x+6,max_y-min_y+2))
			map_xs = 3 + max_x - xs
			map_ys = 2 + ys - min_y
			if (length(good_i_s)>0) {
				my_g_xs = 3 + max_x - xs[good_i_s] 
				my_g_ys = 2 + ys[good_i_s] - min_y
			}
			if (length(bad_i_s)>0) {
				my_b_xs = 3 + max_x - xs[bad_i_s] 
				my_b_ys = 2 + ys[bad_i_s] - min_y
			}
			if (valid_m_die) {
				my_m_xs = 3 + max_x - mirror_xs
				my_m_ys = 2 + mirror_ys - min_y
			}
		} else if (notch=="W") {
			my_map = array(die_str,dim=c(max_y-min_y+6,max_x-min_x+2))
			map_xs = 3 + max_y - ys 
			map_ys = 2 + max_x - xs 
			if (length(good_i_s)>0) {
				my_g_xs = 3 + max_y - ys[good_i_s] 
				my_g_ys = 2 + max_x - xs[good_i_s] 
			}
			if (length(bad_i_s)>0) {
				my_b_xs = 3 + max_y - ys[bad_i_s] 
				my_b_ys = 2 + max_x - xs[bad_i_s] 
			}
			if (valid_m_die) {
				my_m_xs = 3 + max_y - mirror_ys
				my_m_ys = 2 + max_x - mirror_xs
			}
		} else if (notch=="E") {
			my_map = array(die_str,dim=c(max_y-min_y+6,max_x-min_x+2))
			map_xs = 3 + ys - min_y
			map_ys = 2 + xs - min_x
			if (length(good_i_s)>0) {
				my_g_xs = 3 + ys[good_i_s] - min_y
				my_g_ys = 2 + xs[good_i_s] - min_x
			}
			if (length(bad_i_s)>0) {
				my_b_xs = 3 + ys[bad_i_s] - min_y
				my_b_ys = 2 + xs[bad_i_s] - min_x
			}
			if (valid_m_die) {
				my_m_xs = 3 - min_y + mirror_ys
				my_m_ys = 2 - min_x + mirror_xs
			}
		} else {
			if (notch!="S") {
				# print warning to screen... assuming notch is already south
				cat("WARNING: unrecognized notch value, assuming \"S\" \n")
			}
			my_map = array(die_str,dim=c(max_x-min_x+6,max_y-min_y+2))
			map_xs = 3 + xs - min_x
			map_ys = 2 + max_y - ys 
			if (length(good_i_s)>0) {
				my_g_xs = 3 + xs[good_i_s] - min_x
				my_g_ys = 2 + max_y - ys[good_i_s] 
			}
			if (length(bad_i_s)>0) {
				my_b_xs = 3 + xs[bad_i_s] - min_x
				my_b_ys = 2 + max_y - ys[bad_i_s] 
			}
			if (valid_m_die) {
				my_m_xs = 3 - min_x + mirror_xs
				my_m_ys = 2 - mirror_ys + max_y
			}
		}

		#browser()
		# do text stuff in top portion
		#------------------------------
		if(product_id=="") {
			product_id = as.character(LotInfoFrame[["part_typ"]])[1]
		}
		if(sinf_fmt) {
			the_string = sprintf("DEVICE:%s\n",product_id)
			cat(the_string,file=out_conn)

			the_string = sprintf("LOT:%s\n",lot_id)
			cat(the_string,file=out_conn)

			the_string = sprintf("WAFER:%s\n",wafer_id)
			cat(the_string,file=out_conn)

			the_string = "FNLOC:180;   wafer notch position (0=TOP, 90=RIGHT, 180=BOTTOM, 270=LEFT)\n"
			cat(the_string,file=out_conn)

			if( (notch=="W") | (notch=="E") ) {
				row_count = max_x - min_y + 1
				col_count = max_y - min_y + 1
			} else {
				row_count = max_y - min_y + 1
				col_count = max_x - min_x + 1
			}
			the_string = sprintf("ROWCT:%d;  number of rows\n",row_count)
			cat(the_string,file=out_conn)

			the_string = sprintf("COLCT:%d;  number of columns\n",col_count)
			cat(the_string,file=out_conn)

			my_str = "BCEQU:"
			for (i in 1:length(my_pass_bins)) {
				if (i>1)  my_str = sprintf("%s,",my_str)	# add comma between bin #'s
				if (digits>3)       my_bin_str = sprintf("%04d",my_pass_bins[i])
				else if (digits>2)  my_bin_str = sprintf("%03d",my_pass_bins[i])
				else                my_bin_str = sprintf("%02d",my_pass_bins[i])
				my_str = sprintf("%s%s",my_str,my_bin_str)
			}
			the_string = sprintf("%s;   list of bin codes that are good\n",my_str)
			cat(the_string,file=out_conn)

			the_string = "REFPX:;   x-coord of reference die (optional)\n"
			cat(the_string,file=out_conn)

			the_string = "REFPY:;   y-coord of reference die (optional)\n"
			cat(the_string,file=out_conn)

			the_string = "DUTMS:mm;   die units of measure (mm or mil)\n"
			cat(the_string,file=out_conn)

			the_string = sprintf("XDIES:%s\n",x_step)
			cat(the_string,file=out_conn)

			the_string = sprintf("YDIES:%s\n",y_step)
			cat(the_string,file=out_conn)
		} else {
			the_string = sprintf("Test Floor:  %s\n",test_floor)
			cat(the_string,file=out_conn)

			the_string = sprintf("Product Id:  %s\n",product_id)
			cat(the_string,file=out_conn)

			the_string = sprintf("Lot Id:      %s\n",lot_id)
			cat(the_string,file=out_conn)

			if(nchar(wafer_id)<1)  cat("ERROR: wafer_id missing!\n")
			the_string = sprintf("Wafer Id:    %s\n",wafer_id)
			cat(the_string,file=out_conn)

			finish_t = ISOdatetime(1970,1,1,0,0,0) + as.numeric(LotInfoFrame[["finish_t"]])
			my_tz = ""
			if(length(finish_t)<1)  finish_t = "na"
			else {
				my_t = as.POSIXlt(finish_t)
				my_tzs = attr(my_t,"tzone")
				if (length(my_tzs)==3) {
					my_tz = as.character(my_tzs[2+(my_t$isdst)])
				} else {
					my_tz = as.character(my_tzs)
				}
			}
			the_string = sprintf("Finish Time: %s %s\n",finish_t,my_tz)	# REVISIT
			cat(the_string,file=out_conn)

			if ( (x_step != "") | (y_step!= "") ) {
				the_string = sprintf("X step size: %s\n",x_step)
				cat(the_string,file=out_conn)

				the_string = sprintf("Y step size: %s\n",y_step)
				cat(the_string,file=out_conn)
			}	
		}

		
		# do wafer map in bottom portion
		#--------------------------------
		if(!sinf_fmt)  cat("<Bin_Map>\n",file=out_conn)
		terse_bin_count=0
		if(multi_binning) {
			# for each bin, update map with "01 " or whatever the bin # is...
			bin_counts = vector()
			bin_flags = vector()
			bin_names = vector()
			terse_bin = vector()
			total = length(my_bins)
			for (i in 1:length(unique_bins)) {
				if(unique_bins[i] %in% my_pass_bins)  bin_flags[i] = "P"
				else  bin_flags[i] = "F"
				if(multi_bin_terse) {
					if (bin_flags[i]=="P") {
						terse_bin_count = terse_bin_count + 1
						terse_bin[i] = terse_bin_count
						my_str = sprintf("%d",terse_bin[i])
					} else {
						terse_bin[i]=0
						my_str = "x"
					}
				} else if(digits>3)  my_str = sprintf("%04d ",unique_bins[i])
				else if(digits>2)  my_str = sprintf("%03d ",unique_bins[i])
				else  my_str = sprintf("%02d ",unique_bins[i])
				indices = which(my_bins==unique_bins[i])
				if(length(indices)>0) {
					my_xs = map_xs[indices]
					my_ys = map_ys[indices]
					xys = array(c(my_xs,my_ys),dim=c(length(my_xs),2))
					my_map[xys] = my_str
				}
				bin_counts[i] = length(indices)
				bin_names[i] = ""
				if (type=="sbin") {
					if(is.finite(match("SbinInfoFrame",my_objs))) {
						idx = match(unique_bins[i],SbinInfoFrame[["sbin_num"]],nomatch=0)
						if( (length(idx)>0) && (idx[1]>0) ) {
							bin_names[i] = as.character(SbinInfoFrame[[idx[1],"sbin_nam"]])
						} else {
							bin_names[i] = ""
						}
					}
				} else {
					if(is.finite(match("HbinInfoFrame",my_objs))) {
						idx = match(unique_bins[i],HbinInfoFrame[["hbin_num"]],nomatch=0)
						if( (length(idx)>0) && (idx[1]>0) ) {
							bin_names[i] = as.character(HbinInfoFrame[[idx[1],"hbin_nam"]])
						} else {
							bin_names[i] = ""
						}
					}
				}	
			}
			if(valid_m_die) {
				if(digits>3)  my_str = "MMMM "
				else if(digits>2)  my_str = "MMM "
				else my_str = "MM "
				xys = array(c(my_m_xs,my_m_ys),dim=c(length(my_m_xs),2))
				my_map[xys] = my_str
			}
		} else {
			if (length(bad_i_s)>0) {
				xys = array(c(my_b_xs,my_b_ys),dim=c(length(my_b_xs),2))
				my_map[xys] = "x"
			}
			if (length(good_i_s)>0) {
				xys = array(c(my_g_xs,my_g_ys),dim=c(length(my_g_xs),2))
				my_map[xys] = "1"
			}
			if ((valid_m_die) && (length(mirror_xs)>0)) {
				xys = array(c(my_m_xs,my_m_ys),dim=c(length(my_m_xs),2))
				my_map[xys] = "M"
			}
		}

		a_max_x = dim(my_map)[1]
		a_max_y = dim(my_map)[2]

		# change some of the .'s to -'s if there are tested die on all
		# sides of the untested die...
		if (skip_die_minus) {
			inside_die = matrix(TRUE,a_max_x,a_max_y)

			if(multi_binning && !multi_bin_terse) {
				if(digits>2) {
					die_str = "... "
					new_str = "--- "
				} else {
					die_str = ".. "
					new_str = "-- "
				}
			} else {
				die_str = "."
				new_str = "-"
			}

			# find die that have a tested die to the left and right
			for(i in 1:a_max_y) {
				my_row = which(my_map[,i] != die_str)
				if(length(my_row)>0) {
					inside_die[1:min(my_row),i] = FALSE
					inside_die[max(my_row):a_max_x,i] = FALSE
				} else {
					inside_die[,i] = FALSE
				}
			}

			# find die that have a tested die above and below
			for(i in 1:a_max_x) {
				my_col = which(my_map[i,] != die_str)
				if(length(my_col)>0) {
					inside_die[i,1:min(my_col)] = FALSE
					inside_die[i,max(my_col):a_max_y] = FALSE
				} else {
					inside_die[i,] = FALSE
				}
			}
			#browser()

			# any die that are inside_die and die_str become new_str
			xys = intersect(which(my_map==die_str),which(inside_die))
			my_map[xys] = new_str
		}

		if(multi_binning && !multi_bin_terse) {
			if(digits>3)  a_max_x = a_max_x * 5
			else if(digits>2)  a_max_x = a_max_x * 4
			else if(digits>1)  a_max_x = a_max_x * 3
		}


		# dumping ascii map to file
		# (there is some undocumented superstition here)
		#  .wmap format has
		#			2 padded columns to the left of wafer 
		#			3 padded columns to the right of wafer
		#        	1 padded row above wafer
		#			0 padded rows below wafer
		#  .sinf format has 
		#			0 padded columns to the left of wafer 
		#			0 padded columns to the right of wafer
		#        	1 padded row above wafer
		#			0 padded rows below wafer
		for(i in 1:a_max_y) {
			if(sinf_fmt) {
				cols = dim(my_map)[1]
				the_string = paste(my_map[3:(cols-3),i],sep="",collapse="")	
				the_string = paste("RowData:",the_string,"\n",sep="")	
			} else {
				the_string = paste(my_map[,i],sep="",collapse="")	
				the_string = paste(the_string,"\n",sep="")	
			}
			cat(the_string,file=out_conn)
		}
		spaces = as.integer((a_max_x - 10)/2)
		if (spaces<0)  space_str = ""
		else  space_str = paste(rep(" ",times=spaces),sep="",collapse="")
		the_string = paste(space_str,"Notch Down \n",sep="")
		cat(the_string,file=out_conn)
		if(sinf_fmt)  cat("\n",file=out_conn)
		else          cat("<\\Bin_Map>\n",file=out_conn)

		# now add summary information
		if(multi_binning && !multi_bin_terse) {
			if(type=="sbin")  type_str = "Sbin"
			else  type_str = "Hbin"
			the_string = sprintf("\nBin: %s  Count  Yield  P/F  Bin_Name\n",
					type_str)
			cat(the_string,file=out_conn)
			for (i in 1:length(unique_bins)) {
				the_string = sprintf("Bin: %4d  %5d   %4.1f%%   %s   %s\n",
					unique_bins[i],bin_counts[i],100.0*bin_counts[i]/total,
					bin_flags[i],bin_names[i])
				cat(the_string,file=out_conn)
			}
			good_count = length(good_i_s)
			my_yield = 100.0*good_count/total
			the_string = sprintf("\nYield: %.1f percent [%d/%d]\n",
					my_yield,good_count,total)
			cat(the_string,file=out_conn)
		} else if(multi_binning) {
			if(type=="sbin")  type_str = "Sbin"
			else  type_str = "Hbin"
			the_string = sprintf("\nBin:  Num   Count  Yield  P/F  %s_Name\n",
					type_str)
			cat(the_string,file=out_conn)
			for (i in 1:length(unique_bins)) {
				if(terse_bin[i]>0) {
					# REVISIT.... i vs. unique_bins[i] ...
					the_string = sprintf("Bin: %4d  %5d   %4.1f%%   %s   %s\n",
						terse_bin[i],bin_counts[i],100.0*bin_counts[i]/total,
						bin_flags[i],bin_names[i])
					cat(the_string,file=out_conn)
				}
			}			
			good_count = length(good_i_s)
			my_yield = 100.0*good_count/total
			the_string = sprintf("\nYield: %.1f percent [%d/%d]\n",
					my_yield,good_count,total)
			cat(the_string,file=out_conn)
		} else if(do_yield) {
			good_count = length(which(my_map=="1"))
			bad_count = length(which(my_map=="x"))
			total = good_count + bad_count
			my_yield = 100.0*good_count/total
			the_string = sprintf("\nYield: %.1f percent [%d/%d]\n",
					my_yield,good_count,total)
			cat(the_string,file=out_conn)
		}
		close(out_conn)

    }	# end of for wafers loop

}

