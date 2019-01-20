#  ProbeVsReprobe.R
#
# $Id: ProbeVsReprobe.R,v 1.2 2015/05/07 00:49:48 david Exp $
#
#  R script that processes an RTDF file and generates separate
#  first pass, retest, overall summary information
#  based on x,y coordinates to determine retests
#  
#
# Copyright (C) 2015 David Gattrell
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
#-------------------------------------------------------------------


ProbeVsReprobe <- function(rtdf_name="",out_name="probe_reprobe.sum",
			type="sbin",rtdf_dir="",site_pct_vs_site=TRUE) {
    # rtdf_name -- name of the rtdf file to read
    # out_name -- name of the summary file to generate  (xxx.sum)
	#             if multiple wafers in rtdf file, multiple
	#             summary files will be generated with wafer id added
	#             to filename
    # type = "sbin"  or "hbin"... which binning to use 
	# rtdf_dir -- absolute path for rtdf file if not in current 
	#             directory/folder.. else ""
	# site_pct_vs_site - for per-site percents, do either as % of
	#             total tested units of all sites (false) or as % of
	#             total units tested for that site (true)
	#-------------------------------------------------------------

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
		wafers_count = dim(WafersFrame)[1]
		all_wi = as.integer(DevicesFrame[,"wafer_index"])
    } else {
		wafers_count = 1
    }

	# determine coordinates and bins used
	#------------------------------------
    all_xs = as.integer(DevicesFrame[,"x_coord"])
    all_ys = as.integer(DevicesFrame[,"y_coord"])

	all_sbins = as.integer(DevicesFrame[,"soft_bin"])
    all_hbins = as.integer(DevicesFrame[,"hard_bin"])

	all_sites = as.integer(DevicesFrame[,"site"])



    for (wafer in 1:wafers_count) {

		# set lot_id and wafer_id
		#---------------------------
		lot_id = as.character(LotInfoFrame[[1,"lotid"]])
		sublot_id = as.character(LotInfoFrame[[1,"sublotid"]])
		if(is.finite(match("WafersFrame",my_objs))) {
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

		# determine output file name
		#-----------------------------
		if(nchar(out_name)>0) {
			wafer_file = out_name
			if (wafers_count>1) {
				txt_name = as.character(strsplit(out_name,"[.]sum$"))
				wafer_file = paste(txt_name,"_wafer",wafer_id,".sum",sep="")
			}
		} else {
			wafer_file = paste(lot_id,"_wafer",wafer_id,sep="")
			# need to replace any ":" or "-" etc...
			wafer_file = gsub("[^0-9A-Za-z]","_",wafer_file)
			wafer_file = paste(wafer_file,".sum",sep="")
		}
		out_conn = file(wafer_file,"w")

		# extract x,y, and bin info from Frame
		#---------------------------------------
		if (is.finite(match("WafersFrame",my_objs))) {
		    indxs = which(all_wi==wafer)
		    xs = all_xs[indxs]
		    ys = all_ys[indxs]
		    sbins = all_sbins[indxs]
		    hbins = all_hbins[indxs]
		    sites = all_sites[indxs]
		} else {
		    xs = all_xs
		    ys = all_ys
		    sbins = all_sbins
		    hbins = all_hbins
		    sites = all_sites
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
		sites = sites[valid_dice]


		# find retested die
		#-------------------
		xys = array(c(xs,ys),dim=c(length(xs),2))
		reprobing_indices = which(duplicated(xys,fromLast=FALSE))
		if(length(reprobing_indices)>0) {
			# separate out first touch results
			#---------------------------------
			first_xs = xs[-reprobing_indices]
			first_ys = ys[-reprobing_indices]
			first_sbins = sbins[-reprobing_indices]
			first_hbins = hbins[-reprobing_indices]
			first_sites = sites[-reprobing_indices]
		} else {
			first_xs = xs
			first_ys = ys
			first_sbins = sbins
			first_hbins = hbins
			first_sites = sites
		}
		reprobed_indices = which(duplicated(xys,fromLast=TRUE))
		if(length(reprobed_indices)>0) {
			# now remove initial results from die that have been reprobed
			#------------------------------------------------------------
			cat(sprintf("NOTE: %d die reprobed\n",
						length(reprobed_indices)))
			revised_xs = xs[-reprobed_indices]
			revised_ys = ys[-reprobed_indices]
			revised_sbins = sbins[-reprobed_indices]
			revised_hbins = hbins[-reprobed_indices]
			revised_sites = sites[-reprobed_indices]
		} else {
			revised_xs = xs
			revised_ys = ys
			revised_sbins = sbins
			revised_hbins = hbins
			revised_sites = sites
		}


		# determine number of sites
		#--------------------------
		unique_sites = sort(unique(sites))
		site_count = length(unique_sites)

		part_count = length(first_sites)	# should == length(revised_sites)


		# are we tracking hard bins or soft bins?
		#----------------------------------------
		max_chars = 4;		# number of characters for the bin names column
		if (type=="sbin") {
			my_bins = sbins
			first_bins = first_sbins
			revised_bins = revised_sbins
			type_str = "SBIN"
			unique_bins = sort(unique(my_bins))
			for (i in 1:length(unique_bins)) {
				if(is.finite(match("SbinInfoFrame",my_objs))) {
					idx = match(unique_bins[i],SbinInfoFrame[["sbin_num"]],nomatch=0)
					if( (length(idx)>0) && (idx[1]>0) ) {
						bin_name = as.character(SbinInfoFrame[[idx[1],"sbin_nam"]])
						if(nchar(bin_name)>max_chars)  max_chars = nchar(bin_name)
					}
				}
			}
		} else {
			my_bins = hbins
			first_bins = first_hbins
			revised_bins = revised_hbins
			type_str = "HBIN"
			unique_bins = sort(unique(my_bins))
			for (i in 1:length(unique_bins)) {
				if(is.finite(match("HbinInfoFrame",my_objs))) {
					idx = match(unique_bins[i],HbinInfoFrame[["hbin_num"]],nomatch=0)
					if( (length(idx)>0) && (idx[1]>0) ) {
						bin_name = as.character(HbinInfoFrame[[idx[1],"hbin_nam"]])
						if(nchar(bin_name)>max_chars)  max_chars = nchar(bin_name)
					}
				}
			}
		}



		# print summary information - header section
		#-------------------------------------------
		the_string = sprintf("\n")
		cat(the_string,file=out_conn)

		# REVISIT.. could check max bin name length instead of hardcoded 24 chars
		# max_chars
		title1_string = sprintf("%4s , %-24s ,%5s, %5s ,%5s, %5s ",
					type_str,type_str,"Overall",""  ,"Overall",""  )
		title2_string = sprintf("%4s , %-24s , %5s , %5s , %5s , %5s ",
					"#"     ,"name"  ,"1stY"   ,"%","FinY"   ,"%")
		if(site_count>1) {
			title1_string2 = ""
			title2_string2 = ""
			for (site in 1:site_count) {
				site_str = sprintf("Site %d",unique_sites[site])
				title1_string2 = sprintf("%s, %5s, %5s , %5s , %5s ",
							title1_string2,site_str," " ," "   ," " )
				title2_string2 = sprintf("%s, %5s , %5s , %5s , %5s ",
							title2_string2,"1stY"  ,"%","FinY","%")
			}
			cat(title1_string,file=out_conn)
			cat(title1_string2,file=out_conn)
			cat("\n",file=out_conn)
			cat(title2_string,file=out_conn)
			cat(title2_string2,file=out_conn)
			cat("\n",file=out_conn)
		} else {
			cat(title1_string,file=out_conn)
			cat("\n",file=out_conn)
			cat(title2_string,file=out_conn)
			cat("\n",file=out_conn)
		}

		# print summary information - per-bin lines section
		#--------------------------------------------------
		for (i in 1:length(unique_bins)) {
			bin_name = ""
			if (type=="sbin") {
				if(is.finite(match("SbinInfoFrame",my_objs))) {
					idx = match(unique_bins[i],SbinInfoFrame[["sbin_num"]],nomatch=0)
					if( (length(idx)>0) && (idx[1]>0) ) {
						bin_name = as.character(SbinInfoFrame[[idx[1],"sbin_nam"]])
					}
				}
			} else {
				if(is.finite(match("HbinInfoFrame",my_objs))) {
					idx = match(unique_bins[i],HbinInfoFrame[["hbin_num"]],nomatch=0)
					if( (length(idx)>0) && (idx[1]>0) ) {
						bin_name = as.character(HbinInfoFrame[[idx[1],"hbin_nam"]])
					}
				}
			}
			first_indices = which(first_bins==unique_bins[i])
			first_count = length(first_indices)
			first_pct = 100.0*first_count/part_count
			revised_indices = which(revised_bins==unique_bins[i])
			revised_count = length(revised_indices)
			revised_pct = 100.0*revised_count/part_count

			the_string = sprintf("%4d , %-24s , %5d , %5.2f , %5d , %5.2f ",
						unique_bins[i],bin_name,first_count,first_pct,revised_count,revised_pct )
			cat(the_string,file=out_conn)
			if(site_count>1) {
				for (site in 1:site_count) {
					site_indices = which(first_sites==unique_sites[site])
					site_total = length(site_indices)
					s_f_indices = intersect(first_indices,site_indices)
					s_f_count = length(s_f_indices)
					s_r_indices = intersect(revised_indices,site_indices)
					s_r_count = length(s_r_indices)
					if (site_pct_vs_site) {
						first_pct = 100.0*s_f_count/site_total
						revised_pct = 100.0*s_r_count/site_total
					} else {
						first_pct = 100.0*s_f_count/part_count
						revised_pct = 100.0*s_r_count/part_count
					}
					the_string = sprintf(", %5d , %5.2f , %5d , %5.2f ",
						s_f_count,first_pct,s_r_count,revised_pct )
					cat(the_string,file=out_conn)
				}
			}
			cat("\n",file=out_conn)
		}
		close(out_conn)

	}  # end of: for (wafer in 1:wafers_count) {

}
