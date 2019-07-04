#  PlotRtdf.R
#
# $Id: PlotRtdf.R,v 1.30 2019/07/04 01:02:59 david Exp $
#
# script used to generate statistics, histograms, and xy plots from Rtdf files
#
# Copyright (C) 2006-2014 David Gattrell
#               2010 Vincent Horng
#               2019 David Gattrell
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
if(exists(".PlotRtdf.env")) rm(.PlotRtdf.env) 
.PlotRtdf.env <- new.env()

PlotRtdf <- function(rtdf_name="",pdf_name="",param_name="",dataset_name="",
            do_xy_plots=FALSE,do_hist_and_xy=FALSE,title="PlotRtdf output",
            auto_scale=TRUE, save_workspace_to="",do_csv=TRUE,
            do_robust_stats=0,alt_limits="",use_alt_lims=0,
            min_plots_per_page=4,do_guardbands=FALSE,use_csv_formulas=TRUE,
            use_OOCalc_csv=TRUE,add_normal_curve=FALSE,plot_using_test_limits=TRUE,
            collapse_MPRs=FALSE,rtdf_dirs="",param_dir="",alt_lim_dir="",
			plot_widest_limits_or_values=FALSE,superimpose_hist=FALSE,
			just_superimposed_histo=FALSE,do_norm_prob_plots=FALSE,
			to_png=FALSE,max_tests=2000,plot_limits_plus_10pct=FALSE,
			outside_limits_count=FALSE) {

    # rtdf_name -- vector of strings for the filenames containing
    #              rtdf formatted data (.Rdata files)
    # pdf_name -- string for filename of pdf file to output to
    # param_name -- name of file containing rtdf formatted 
    #               ParametersFrame containing the list of parameters
    #               and limits to use for plotting... if empty, this
    #               program will use the first rtdf_name's
    #               ParametersFrame
    # dataset_name -- same # of strings as rtdf_name... contains
    #               strings that will be printed per histogram
    #               for each dataset, if empty, default is to use the
    #               names of the rtdf files
    # do_xy_plots -- for 2nd to nth datasets, plot as xy vs 1st dataset,
    #                    instead of as histograms
    # do_hist_and_xy -- like above, but does as histograms too.
    # title -- title to appear at the bottom of each pdf page
    # auto_scale -- override scaler in stdf file, and guess at an
    #                    appropriate scale... good idea with LTX files.
    # save_workspace_to -- saves workspace to file so you can load
    #                      it into an R session for further analysis
    # do_csv -- generates pdf_name.csv file that has page num,
    #           testnum, name, ll, ul, cpks, etc... 
    # do_robust_stats -- use robust mean and robust sdev on plots
    #			0 / FALSE -> use mean, sdev on plots
    #           1 / TRUE -> use robust mean, robust sdev on plots
    #           		robust mean = median = Quartile 0.50
    #           		robust sdev = (Quartile 0.75 - Quartile 0.25)/1.34898
    #           2 -> use 2 sided robust sdev, robust mean on plots
    #                   robust ll_sdev = 2*(Quartile 0.50 - Quartile 0.25)/1.34898
    #                   robust ul_sdev = 2*(Quartile 0.75 - Quartile 0.50)/1.34898
    # alt_limits -- a vector of strings containing the filename of an rtdf format
    #               file containing at least the ParametersFrame.  The
    #               file will be used for alternate limits rather than
    #               what is found in param_name, if the use_alt_lims
    #               value is non-zero for that dataset, it is the index of this vector
    # use_alt_lims -- a vector of integer the same length as the rtdf_name
    #                 vector.  If >0, for these data sets use the alternate
    #                 set of limits rather than those in the param_name file
    # min_plots_per_page -- program tends to put one parameter per page.
    #                if there is only one dataset, you get one plot per
    #                page.  If this is set to 4, you would get 4 parameters
    #                per page if single dataset, 2 parameters per page if
    #                2 datasets.
    # do_guardbands -- if true, will generate a guardbanding.csv file. 
    #                using this flag does:
    #                - dataset1 uses limits, other sets use alt limits
    #                  (if no alt limits, use limits as alt limits)
    #                - assumes xy plots on
    #                - assumes datasets are for the same devices, different
    #                  conditions, ie set1 = final test at room, set2 = 
    #                  QC at hot, set3=QC at cold
    #                xy linear fit should have R>0.9 to be meaningful
	#				 if # of datasets is <2, or xy plots is off, this
	#				 option will be ignored
    # use_csv_formulas -- if FALSE, just write values to cells in csv file,
    #                otherwise, use formulas for cells that are functions of
    #                other cells. (really just Cpk columns)  NOTE: need
    #                to set use_OOCalc_csv flag properly... OOCalc formula syntax
    #                is different from Excel. 
    # use_OOCalc_csv -- if true, adds OpenOfficeCalc syntax to gb.csv file
    #                to highlight marginal limits.  (you still need to 
    #                define a style "yellow" in OOCalc with a yellow background
    #                once you've loaded the csv file.  (Not excel compatable)
    #                if false, use Excel syntax
    # add_normal_curve -- if TRUE, superimposes guassian curve on histograms
    #                based on mean/sd
    # plot_using_test_limits -- if FALSE, use min and max measurements for
    #                histogram plot extremes.
    # collapse_MPRs -- if true, don't break out MPR data into per pin 
    #                histograms, but combine into a single histogram.
    #                looks for adjacent parameters with the same test number,
    #                testname upto last slash, with same limits and groups
    #                these into a single histogram.
    # rtdf_dirs -- same # of strings as rtdf_name... contains absolute
	#                directory paths for rtdf files if different than
	#                execution directory
	# param_dir -- contains absolute directory path for param_name rtdf 
	#                file if different than execution directory
	# alt_lim_dir -- contains absolute directory path for alt_limits rtdf 
	#                file if different than execution directory
	# plot_widest_limits_or_values -- overrides plot_using_test_limits flag,
	#				 will plot to whichever is wider, the min value or the LL
	#				 and the max value or the UL
	# superimpose_hist -- adds an extra histogram which has the different
	#				 histograms superimposed on the same plot in different
	#				 colours
	# just_superimposed_histo -- only does the superimposed histogram, 
	#                suppresses the individual dataset histograms.
	# do_norm_prob_plots -- instead of histograms, do normal probability plots
	# to_png -- dump plots to individual .png files rather than to a single
	#				.pdf file
	# max_tests -- safety check.. did you really want to plot more than this #
	#                of tests?   negative number skips this check
	# plot_limits_plus_10pct -- overrides "plot_using_test_limits flag,
	#                makes plots wider by 10%
	# outside_limits_count -- replace "Off the plot" statistic on left side with
	#                "Outside limits" statistic.
	# -----------------------------------------------------------------------


	#attach(.PlotRtdf.env)  ... now using environment()


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


    # default values for options...
    #--------------------------------
    if(to_png) {
		to_pdf = FALSE 
		png_dpi = 150  # 300	# default of 75 is too crude
	} else {
		to_pdf = TRUE
	}
	
    # auto_scale = TRUE
    # sort parameters alphabetically or numerically

	if(just_superimposed_histo) superimpose_hist = TRUE

	# after setting limits, expand by 10%, so make sure limits flag is on.
	if(plot_limits_plus_10pct)  plot_using_test_limits = TRUE

	if(plot_widest_limits_or_values)  plot_using_test_limits = FALSE

    if(do_hist_and_xy)  do_xy_plots = TRUE
    if(do_guardbands && !do_xy_plots) {
		cat("WARNING: turning off do_guardbands since do_xy_plots is FALSE\n")
		do_guardbands = FALSE
	}
    if(add_normal_curve) {
        norm_x0 = c(-25:25)/5
        norm_y0 = dnorm(norm_x0)
    }
	if(do_xy_plots && collapse_MPRs) {
		# one or the other, not both!!!
		# print message and clear do_hist_and_xy, do_guardbands, do_xy_plots
		cat("WARNING: can't collapse MPRs and also do XY plots...\n")
		cat("         disabling XY plots...\n")
		do_xy_plots = FALSE
		if(do_hist_and_xy) {
			cat("         disabling do_hist_and_xy flag \n")
			do_hist_and_xy = FALSE
		}
		if(do_guardbands) {
			cat("         disabling do_guardbands flag \n")
			do_guardbands = FALSE
		}
	}
	if(do_xy_plots && superimpose_hist) {
		cat("WARNING: xy plot is on, turning off superimpose histos\n")
		superimpose_hist = FALSE
		just_superimposed_histo = FALSE
	}


    # if filenames not defined, prompt for them
    #------------------------------------------
    if (length(rtdf_name)<2 && (rtdf_name == "")) {
        rtdf_name <- readline("Enter the name of the RTDF file to read: ")
    }

    if (pdf_name == "") {
        pdf_name <- readline("Enter the name of the PDF file to write: ")
    }
    if (do_csv) {
        csv_name = paste(as.character(strsplit(pdf_name,"[.]pdf$")),".csv",sep="")
        csv_header = TRUE
    }
    if (do_guardbands) {
        gb_name = paste(as.character(strsplit(pdf_name,"[.]pdf$")),"_gb.csv",sep="")
        gb_header = TRUE
    }


    if (param_name == "") {
        # build PlotParamsFrame from superset of all 
        # datasets' ParametersFrames
        build_params_frame = TRUE
    } else {
		if (param_dir != "") {
			my_dir = getwd()
			setwd(param_dir)
		}
        load(param_name)
		if (param_dir != "")  setwd(my_dir)
        # revisit.. add error checking
        PlotParametersFrame = ParametersFrame
        build_params_frame = FALSE
    }
    
    if (dataset_name[1] == "") {
        # no string supplied, use filename as title...
		# REVISIT: if list >1, and later elements valid, clobbered.
        dataset_name = rtdf_name
    }


    datasets = length(rtdf_name)    # number of Rtdf files to plot...
	if(datasets<2) {
		if (do_guardbands) {
			cat("WARNING: do_guardbands disabled, requires 2 or more datasets\n")
			do_guardbands = FALSE
		}
		if (do_xy_plots) {
			cat("WARNING: do_xy_plots disabled, requires 2 or more datasets\n")
			do_xy_plots = FALSE
		}
		if (do_hist_and_xy) {
			cat("WARNING: do_hist_and_xy disabled, requires 2 or more datasets\n")
			do_hist_and_xy = FALSE
		}
		if (superimpose_hist) {
			cat("WARNING: superimpose_hist disabled, requires 2 or more datasets\n")
			superimpose_hist = FALSE
			just_superimposed_histo = FALSE
		}
	}

    len = length(rtdf_dirs)
    if(len<datasets)  rtdf_dirs[(len+1):datasets]=""
    for (j in 1:datasets) {
		if (rtdf_dirs[j] != "") {
			#cat(sprintf("j is %d, dir ==%s==\n",j,rtdf_dirs[j]))
			my_dir = getwd()
			setwd(rtdf_dirs[j])
		}
        # REVISIT: could clear 4 objects first, load, then check objects exist
        load(rtdf_name[j])            # assume Rtdf: ParametersFrame, 
                                    # ResultsMatrix, ...
		if (rtdf_dirs[j] != "")  setwd(my_dir)

        if(j==1) {
            DevicesFrameList=list(DevicesFrame)
            ResultsMatrixList=list(ResultsMatrix)

            if(do_xy_plots) {
                PlotDevicesFrame = DevicesFrame
                max_devices = dim(PlotDevicesFrame)
                max_devices = max_devices[1]
                device_xrefs = array(NaN,dim=c(max_devices,datasets))
                device_xrefs[,1]=c(1:max_devices)
            }

            if(build_params_frame)  PlotParametersFrame = ParametersFrame
            max_params = dim(PlotParametersFrame)
            max_params = max_params[1]
            param_xrefs = array(NaN,dim=c(max_params,datasets))

            if(build_params_frame) {
                param_xrefs[,1]=c(1:max_params)
            } else {
                for (i in 1:max_params) {
                    test_nam = PlotParametersFrame[i,"testname"]
                    index = match(test_nam,ParametersFrame[["testname"]],
                                nomatch=NaN)
                    param_xrefs[i,j]=index
                }
                # REVISIT... no checking for duplicate names/numbers
            }
        } else {
            DevicesFrameList[j]=list(DevicesFrame)
            ResultsMatrixList[j]=list(ResultsMatrix)

            if(do_xy_plots) {
                for (i in 1:max_devices) {
                    part_id = PlotDevicesFrame[i,"part_id"]        # string
                    index = match(part_id,DevicesFrame[["part_id"]],
                            nomatch=NaN)
                    device_xrefs[i,j]=index
                }
            }
            for (i in 1:max_params) {
                test_nam = PlotParametersFrame[i,"testname"]
                index = match(test_nam,ParametersFrame[["testname"]],
                            nomatch=NaN)
                param_xrefs[i,j]=index
            }
			my_idxs = c(1:dim(ParametersFrame)[1])
			my_xrefs = param_xrefs[,j]
			new_tests = setdiff(my_idxs,my_xrefs)
			if (build_params_frame && (length(new_tests)>0)) {
				#... appending to a Frame is slow...
				cat(sprintf("Adding %d new tests from dataset %d \n",length(new_tests),j))
				my_tnums = as.numeric(ParametersFrame[new_tests,"testnum"])
				my_tnames = as.character(ParametersFrame[new_tests,"testname"])
				my_scalers = as.numeric(ParametersFrame[new_tests,"scaler"])
				my_units = as.character(ParametersFrame[new_tests,"units"])
				my_lls = as.numeric(ParametersFrame[new_tests,"ll"])
				my_uls = as.numeric(ParametersFrame[new_tests,"ul"])
				my_plot_lls = as.numeric(ParametersFrame[new_tests,"plot_ll"])
				my_plot_uls = as.numeric(ParametersFrame[new_tests,"plot_ul"])
				i1 = max_params + 1
				i2 = max_params + length(new_tests)
				#browser()
				PlotParametersFrame[i1:i2,"testnum"] <- my_tnums
				PlotParametersFrame[i1:i2,"testname"] <- my_tnames
				PlotParametersFrame[i1:i2,"scaler"] <- my_scalers
				PlotParametersFrame[i1:i2,"units"] <- my_units
				PlotParametersFrame[i1:i2,"ll"] <- my_lls
				PlotParametersFrame[i1:i2,"ul"] <- my_uls
				PlotParametersFrame[i1:i2,"plot_ll"] <- my_plot_lls
				PlotParametersFrame[i1:i2,"plot_ul"] <- my_plot_uls
				# now need to update param_xrefs[]...
				nan = array(NaN,dim=c(length(new_tests),datasets))
				param_xrefs = rbind(param_xrefs,nan)
				param_xrefs[i1:i2,j] = new_tests
				max_params = i2
			}
        }
    }


    # if alternate limits, add them to PlotParametersFrame
	# ... kluge.. add support for 2 sets of alt, but not more
    #--------------------------------------------------------
    valid_alt_limits = FALSE
    valid_alt2_limits = FALSE
    if(alt_limits[1] != "") {
		if (alt_lim_dir != "") {
			my_dir = getwd()
			setwd(alt_lim_dir)
		}
        load(alt_limits[1])
		if (alt_lim_dir != "")  setwd(my_dir)

        for (i in 1:max_params) {
            test_name = PlotParametersFrame[i,"testname"]
            index = match(test_name,ParametersFrame[["testname"]],nomatch=NaN)
            if (is.finite(index)) {
                PlotParametersFrame[i,"alt_ll"] = ParametersFrame[[index,"ll"]]
                PlotParametersFrame[i,"alt_ul"] = ParametersFrame[[index,"ul"]]
            } else {
                PlotParametersFrame[i,"alt_ll"] = NaN
                PlotParametersFrame[i,"alt_ul"] = NaN
            }
        }
        valid_alt_limits = TRUE
        len = length(use_alt_lims)
        if(len<datasets)  use_alt_lims[(len+1):datasets]=0
            
		if((length(alt_limits)>1)&&(alt_limits[2] != "")) {
			if (alt_lim_dir != "") {
				my_dir = getwd()
				setwd(alt_lim_dir)
			}
			load(alt_limits[2])
			if (alt_lim_dir != "")  setwd(my_dir)
			for (i in 1:max_params) {
				test_name = PlotParametersFrame[i,"testname"]
				index = match(test_name,ParametersFrame[["testname"]],nomatch=NaN)
				if (is.finite(index)) {
					PlotParametersFrame[i,"alt2_ll"] = ParametersFrame[[index,"ll"]]
					PlotParametersFrame[i,"alt2_ul"] = ParametersFrame[[index,"ul"]]
				} else {
					PlotParametersFrame[i,"alt2_ll"] = NaN
					PlotParametersFrame[i,"alt2_ul"] = NaN
				}
			}
        	valid_alt2_limits = TRUE
		}
    } else if(do_guardbands) {
        PlotParametersFrame["alt_ll"]=as.numeric(PlotParametersFrame[["ll"]])
        PlotParametersFrame["alt_ul"]=as.numeric(PlotParametersFrame[["ul"]])
        valid_alt_limits = TRUE
    } else {
        valid_alt_limits = FALSE
    }
    if(do_guardbands) {
        use_alt_lims[1:datasets]=TRUE
        use_alt_lims[1]=FALSE
    }


    # how many parameters can we plot on a page?
    #-------------------------------------------
    if(do_hist_and_xy) {
        plots_per_param = datasets + 2*as.integer(datasets/2)
    } else if (do_xy_plots) {
        plots_per_param = 1 + 2*as.integer(datasets/2)
    } else {
        plots_per_param = datasets
    }
    if (superimpose_hist) plots_per_param = plots_per_param + 1 #this is for extra superimposed histogram
	if (just_superimposed_histo) plots_per_param = 1

	if(plots_per_param<min_plots_per_page) {
        params_per_page = ceiling(min_plots_per_page/plots_per_param)
        plots_per_page = plots_per_param*params_per_page
		if(to_png) {
			png_page_length = 1.0/params_per_page
			params_per_page = 1
			plots_per_page = plots_per_param * params_per_page
		}
    } else {
        params_per_page = 1
        plots_per_page = plots_per_param
		if(to_png) {
			png_page_length = 1.0/params_per_page
		}
    }
    
    if (to_pdf) {
        #pdf(file=pdf_name,paper="letter",width=8,height=10.5,
        #    horizontal=FALSE)
        pdf(file=pdf_name,paper="letter",width=8,height=10.5)
		scex=1.0
    } else if (to_png) {
		scex=png_dpi/75.0		# fonts assume 75dpi? in png files, boost if higher res.
	} else {
        # to avoid: "Error in plot.new() : figure margins too large"
        # this doesn't actually solve the problem!
        windows(width=8,height=10)
    }


    # per page layout...
    #--------------------
    footer_height = 0.25/10.5
    title_height = 0.5/10.5
    hist_height = (1.0 - 0.01 - footer_height -
                    (params_per_page * title_height))/(1.0*plots_per_page)
    ytop = 1.0
	if(to_png) {
		ht_inches = (10.5 - 0.25) * png_page_length + 0.25
		footer_height = 0.25/ht_inches
		title_height = 0.5/ht_inches
		hist_height = (1.0 - 0.01 - footer_height -
                    (params_per_page * title_height))/(1.0*plots_per_page)
	}


    # print out footer for first page
    #---------------------------------
    page_count = 1
    ytop=1.0        # new page, move ptr to top of page
    if (to_pdf) {
        footer = 1
        subscreen_num = split.screen(matrix(c(0.0,1.0,0.01,footer_height),1,4,byrow=FALSE))
        screen(subscreen_num)
        text(0.0,0.5,sprintf("%s  %s",title,now),pos=4,cex=0.7*scex)
        text(1.0,0.5,sprintf("Page %d",page_count),pos=2,cex=0.7*scex)
        close.screen(subscreen_num)
    } else {
        footer = 0
    }


	# do a sanity check on the number of parameters...
	#--------------------------------------------------
	#if((collapse_MPRs) || (max_params>2000)) {
	if( (collapse_MPRs) || ( (max_tests>0) && (max_params>max_tests) ) ) {
		cat("...checking for MPR parameters...\n")
		OrigPlotParametersFrame = PlotParametersFrame
		i2 = 1	# index for condensed/collapsed ParametersFrame
		for (i in 1:max_params) {
			# build collapsed MPR Parameters Frame...
			if (i<2) {
				Parameters_testname = OrigPlotParametersFrame[[i,"testname"]]
				Parameters_testnum = OrigPlotParametersFrame[[i,"testnum"]]
				Parameters_scaler = OrigPlotParametersFrame[[i,"scaler"]]
				Parameters_units = OrigPlotParametersFrame[[i,"units"]]
				Parameters_plot_ll = OrigPlotParametersFrame[[i,"plot_ll"]]
				Parameters_plot_ul = OrigPlotParametersFrame[[i,"plot_ul"]]
				Parameters_ll = OrigPlotParametersFrame[[i,"ll"]]
				Parameters_ul = OrigPlotParametersFrame[[i,"ul"]]
				if (valid_alt_limits) {				
					Parameters_alt_ll = OrigPlotParametersFrame[[i,"alt_ll"]]
					Parameters_alt_ul = OrigPlotParametersFrame[[i,"alt_ul"]]
				}
				if (valid_alt2_limits) {				
					Parameters_alt2_ll = OrigPlotParametersFrame[[i,"alt2_ll"]]
					Parameters_alt2_ul = OrigPlotParametersFrame[[i,"alt2_ul"]]
				}
				Parameters_start_index = i
				Parameters_index_count = 1
			} else {
				# check if this is same as previous parameter...
				diff=FALSE
				if(PlotParametersFrame[[i,"testnum"]] != Parameters_testnum[i2])  diff=TRUE
				if(!diff) {
					this = PlotParametersFrame[[i,"ll"]]
					that = Parameters_ll[i2]
					if(xor(is.finite(this),is.finite(that)))  diff=TRUE
					else if(is.finite(this) && (this != that)) diff=TRUE

					this = PlotParametersFrame[[i,"ul"]]
					that = Parameters_ul[i2]
					if(xor(is.finite(this),is.finite(that)))  diff=TRUE
					else if(is.finite(this) && (this != that)) diff=TRUE

					this = PlotParametersFrame[[i,"plot_ll"]]
					that = Parameters_plot_ll[i2]
					if(xor(is.finite(this),is.finite(that)))  diff=TRUE
					else if(is.finite(this) && (this != that)) diff=TRUE

					this = PlotParametersFrame[[i,"plot_ul"]]
					that = Parameters_plot_ul[i2]
					if(xor(is.finite(this),is.finite(that)))  diff=TRUE
					else if(is.finite(this) && (this != that)) diff=TRUE
				}
				if(!diff && valid_alt_limits) {
					this = PlotParametersFrame[[i,"alt_ll"]]
					that = Parameters_alt_ll[i2]
					if(xor(is.finite(this),is.finite(that)))  diff=TRUE
					else if(is.finite(this) && (this != that)) diff=TRUE

					this = PlotParametersFrame[[i,"alt_ul"]]
					that = Parameters_alt_ul[i2]
					if(xor(is.finite(this),is.finite(that)))  diff=TRUE
					else if(is.finite(this) && (this != that)) diff=TRUE
				}
				if(!diff && valid_alt2_limits) {
					this = PlotParametersFrame[[i,"alt2_ll"]]
					that = Parameters_alt2_ll[i2]
					if(xor(is.finite(this),is.finite(that)))  diff=TRUE
					else if(is.finite(this) && (this != that)) diff=TRUE

					this = PlotParametersFrame[[i,"alt2_ul"]]
					that = Parameters_alt2_ul[i2]
					if(xor(is.finite(this),is.finite(that)))  diff=TRUE
					else if(is.finite(this) && (this != that)) diff=TRUE
				}
				if(!diff) {
					# check if testname is same upto last "/"  ie before pinname
					testname = sub("/[^/]*$","/",PlotParametersFrame[[i,"testname"]])
					testname2 = sub("/[^/]*$","/",Parameters_testname[i2])
					if( testname == testname2 ) {
						# This is an MPR with >1 pins: collapse it
						#------------------------------------------

						# make sure pinname is removed from testname
						Parameters_testname[i2] = testname2

						# add index to new parameters last indices vector
						Parameters_index_count[i2] = Parameters_index_count[i2] + 1
					} else  diff=TRUE
				}
				if(diff) {
					# add new entry to new parameters frame
					i2 = i2+1
					Parameters_testname[i2] = OrigPlotParametersFrame[[i,"testname"]]
					Parameters_testnum[i2] = OrigPlotParametersFrame[[i,"testnum"]]
					Parameters_scaler[i2] = OrigPlotParametersFrame[[i,"scaler"]]
					Parameters_units[i2] = OrigPlotParametersFrame[[i,"units"]]
					Parameters_plot_ll[i2] = OrigPlotParametersFrame[[i,"plot_ll"]]
					Parameters_plot_ul[i2] = OrigPlotParametersFrame[[i,"plot_ul"]]
					Parameters_ll[i2] = OrigPlotParametersFrame[[i,"ll"]]
					Parameters_ul[i2] = OrigPlotParametersFrame[[i,"ul"]]
					if (valid_alt_limits) {				
						Parameters_alt_ll[i2] = OrigPlotParametersFrame[[i,"alt_ll"]]
						Parameters_alt_ul[i2] = OrigPlotParametersFrame[[i,"alt_ul"]]
					}
					if (valid_alt2_limits) {				
						Parameters_alt2_ll[i2] = OrigPlotParametersFrame[[i,"alt2_ll"]]
						Parameters_alt2_ul[i2] = OrigPlotParametersFrame[[i,"alt2_ul"]]
					}
					Parameters_start_index[i2] = i
					Parameters_index_count[i2] = 1
				}
			}
		}

		# if param count is >2K, ask if should continue or quit?
		#if(i2>2000) {
		if( (max_tests>0) && (i2>max_tests) ) {
			# even collapsed there will be a largish number of
			# histograms (and huge .pdf file!)
			cat(sprintf("WARNING: there are %d parameters!\n",max_params))
			cat(sprintf("         collapsing MPRs only reduces this to %d parameters\n",i2))
			reply=readline("Do you want to abort? [y/n] ")
			if (reply=="y" || reply=="Y") {
				return
			}
		} else if (!collapse_MPRs) {
			# ask and maybe set collapse_MPRs
			cat(sprintf("WARNING: there are %d parameters!\n",max_params))
			cat(sprintf("         collapsing MPRs reduces this to %d parameters\n",i2))
			reply=readline("Do you want to collapse MPRs? [y/n] ")
			if (reply=="y" || reply=="Y") {
				cat("... setting collapse_MPRs flag to TRUE... \n")
				collapse_MPRs = TRUE
			}
		}


		if(collapse_MPRs) {
			orig_max_params = max_params
			max_params = i2

			rm(PlotParametersFrame)

			# build PlotParametersFrame
			if (valid_alt2_limits) {
				my_list = list( testnum=NaN, testname="",
								scaler=NaN, units="",
								ll=NaN, ul=NaN,
								plot_ll=NaN, plot_ul=NaN,
								alt_ll=NaN, alt_ul=NaN,
								alt2_ll=NaN, alt2_ul=NaN,
								start_index=NaN, index_count=1)
			} else if (valid_alt_limits) {
				my_list = list( testnum=NaN, testname="",
								scaler=NaN, units="",
								ll=NaN, ul=NaN,
								plot_ll=NaN, plot_ul=NaN,
								alt_ll=NaN, alt_ul=NaN,
								start_index=NaN, index_count=1)
			} else {
				my_list = list( testnum=NaN, testname="",
								scaler=NaN, units="",
								ll=NaN, ul=NaN,
								plot_ll=NaN, plot_ul=NaN,
								start_index=NaN, index_count=1)
			}
			PlotParametersFrame <- data.frame(rbind(my_list))
			PlotParametersFrame[1:max_params,"testnum"] <- Parameters_testnum[1:max_params]
			PlotParametersFrame[1:max_params,"testname"] <- Parameters_testname[1:max_params]
			PlotParametersFrame[1:max_params,"scaler"] <- Parameters_scaler[1:max_params]
			PlotParametersFrame[1:max_params,"units"] <- Parameters_units[1:max_params]
			PlotParametersFrame[1:max_params,"ll"] <- Parameters_ll[1:max_params]
			PlotParametersFrame[1:max_params,"ul"] <- Parameters_ul[1:max_params]
			PlotParametersFrame[1:max_params,"plot_ll"] <- Parameters_plot_ll[1:max_params]
			PlotParametersFrame[1:max_params,"plot_ul"] <- Parameters_plot_ul[1:max_params]
			if (valid_alt_limits) {
				PlotParametersFrame[1:max_params,"alt_ll"] <- Parameters_alt_ll[1:max_params]
				PlotParametersFrame[1:max_params,"alt_ul"] <- Parameters_alt_ul[1:max_params]
			}
			if (valid_alt2_limits) {
				PlotParametersFrame[1:max_params,"alt2_ll"] <- Parameters_alt2_ll[1:max_params]
				PlotParametersFrame[1:max_params,"alt2_ul"] <- Parameters_alt2_ul[1:max_params]
			}
			PlotParametersFrame[1:max_params,"start_index"] <- Parameters_start_index[1:max_params]
			PlotParametersFrame[1:max_params,"index_count"] <- Parameters_index_count[1:max_params]

			#cat("... should have collapsed PlotParametersFrame...\n")
			#browser()
		}


	}


    # For each Parameter, generate plot(s)
    #-------------------------------------
    for (i in 1:max_params) {


        # print progress report to screen every 5 seconds
        #--------------------------------------------------
        timestamp2 = proc.time()
        timestamp2 = timestamp2[3]
        if (timestamp2>(timestamp1+5)) {
            timestamp1 = timestamp2
            cat(sprintf("...now processing parameter %d of %d ...\n",
                i,max_params))
        }


        # take care of paging
        #---------------------------------------
        subscreen = ( (i-1) %% params_per_page ) + 1
        if (subscreen==1) {
            ytop=1.0        # new page, move ptr to top of page
            if (to_pdf) {
				if (i>1) {
					page_count = page_count + 1
					subscreen_num = split.screen(matrix(c(0.0,1.0,0.01,footer_height),1,4,byrow=FALSE))
					screen(subscreen_num)
					text(0.0,0.5,sprintf("%s   %s",title,now),pos=4,cex=0.7*scex)
					text(1.0,0.5,sprintf("Page %d",page_count),pos=2,cex=0.7*scex)
					close.screen(subscreen_num)
				}
            } else if (to_png) {
				if(i>1) {
			        close.screen(all=TRUE)
					new_device_info = dev.off()           # close png file
				}
				# REVISIT
				test_nam = PlotParametersFrame[[i,"testname"]]
				# need to make sure no funny characters in test_nam... remove
				test_nam = gsub("[^[:alnum:]_]","_",test_nam)
				png_name = paste(test_nam,"_pg",page_count,".png",sep="")
				png(file=png_name,width=8*png_dpi,height=ht_inches*png_dpi)

                subscreen_num = split.screen(matrix(c(0.0,1.0,0.01,footer_height),1,4,byrow=FALSE))
                screen(subscreen_num)
                text(0.0,0.5,sprintf("%s   %s",title,now),pos=4,cex=0.7*scex)
                text(1.0,0.5,sprintf("Page %d",page_count),pos=2,cex=0.7*scex)
                close.screen(subscreen_num)

				page_count = page_count + 1
			} else {
                ick <- readline("Enter CR to continue...")
                # new page...
                subscreen_num = split.screen(matrix(c(0.0,1.0,0.01,footer_height),1,4,byrow=FALSE))
                #screen(subscreen_num)
                #text(0.0,0.5,sprintf("%s   %s",title,now),pos=4,cex=0.7)
                #text(1.0,0.5,sprintf("Page %d",page_count),pos=2,cex=0.7)
                #close.screen(subscreen_num)
            }
        }

        scaler = PlotParametersFrame[[i,"scaler"]]
        if (is.na(scaler))  scaler=0  
        test_nam = PlotParametersFrame[[i,"testname"]]
        test_num = PlotParametersFrame[[i,"testnum"]]
        units = PlotParametersFrame[[i,"units"]]
        plot_ll = PlotParametersFrame[[i,"plot_ll"]]
        plot_ul = PlotParametersFrame[[i,"plot_ul"]]
        ll = PlotParametersFrame[[i,"ll"]]
        ul = PlotParametersFrame[[i,"ul"]]
        if (valid_alt2_limits) {
            alt2_ll = PlotParametersFrame[[i,"alt2_ll"]]
            alt2_ul = PlotParametersFrame[[i,"alt2_ul"]]
            alt_ll = PlotParametersFrame[[i,"alt_ll"]]
            alt_ul = PlotParametersFrame[[i,"alt_ul"]]
        } else if (valid_alt_limits) {
            alt2_ll = NaN
            alt2_ul = NaN
            alt_ll = PlotParametersFrame[[i,"alt_ll"]]
            alt_ul = PlotParametersFrame[[i,"alt_ul"]]
        } else {
            alt2_ll = NaN
            alt2_ul = NaN
            alt_ll = NaN
            alt_ul = NaN
        }
		llim = NaN
		if(is.finite(alt2_ll)) {
			llim = alt2_ll
		}
		if(is.finite(alt_ll)) {
			if(is.finite(llim) && (alt_ll<llim)) {
				llim = alt_ll
			} else if(!is.finite(llim)) {
				llim = alt_ll
			}
		}
		if(is.finite(ll)) {
			if(is.finite(llim) && (ll<llim)) {
				llim = ll
			} else if(!is.finite(llim)) {
				llim = ll
			}
		}
		ulim = NaN
		if(is.finite(alt2_ul)) {
			ulim = alt2_ul
		}
		if(is.finite(alt_ul)) {
			if(is.finite(ulim) && (alt_ul>ulim)) {
				ulim = alt_ul
			} else if(!is.finite(ulim)) {
				ulim = alt_ul
			}
		}
		if(is.finite(ul)) {
			if(is.finite(ulim) && (ul>ulim)) {
				ulim = ul
			} else if(!is.finite(ulim)) {
				ulim = ul
			}
		}

        # extract all the parameter result vectors from the dataset matrices
        #-------------------------------------------------------------------
        #browser()  # ... uncomment for debugging
        for(j in 1:datasets) {
            if(collapse_MPRs) {
				i_start = PlotParametersFrame[[i,"start_index"]]
				i_count = PlotParametersFrame[[i,"index_count"]]
				if(length(i_count)<1) {
					browser()
				}
				if(i_count>1) {
					i_stop = i_start+i_count-1
					ix = param_xrefs[(i_start:i_stop),j]
				} else {
					ix = param_xrefs[i_start,j]
				}
				results_vector = ResultsMatrixList[[j]][,ix]
			} else {
				ix = param_xrefs[i,j]
				results_vector = ResultsMatrixList[[j]][,ix]
			}
            if (do_xy_plots) {
                xy_results_vector = results_vector[ device_xrefs[,j] ]
                results_vector = xy_results_vector[is.finite(xy_results_vector)]
            } else {
                results_vector = results_vector[is.finite(results_vector)]
            }
            if(j==1) {
                results_list = list(results_vector)
                if(do_xy_plots)  xy_results_list = list(xy_results_vector)
            } else {
                results_list[j] = list(results_vector)
                if(do_xy_plots)  xy_results_list[j] = list(xy_results_vector)
            }
        }

        # determine plot limits...
        # if plot_ll/ul defined, use that value,
        # else if plot_using_test_limits=TRUE and ll/ul defined, use that,
        # else use min/max of finite measurements
        #-----------------------------------------------------
		# plot_widest_limits_or_values
		if(is.finite(plot_ll)) {
            my_ll = plot_ll
        } else if(plot_using_test_limits && is.finite(llim)) {
            my_ll = llim
        } else {
            my_ll = NaN
            for(j in 1:datasets) {
                if(length(results_list[[j]])>0) {
                    local_min = min(results_list[[j]])
                    if(is.na(my_ll))  my_ll = local_min
                    else if(local_min<my_ll)  my_ll=local_min
                }
            }
			if(is.finite(my_ll)) {
				if(plot_widest_limits_or_values && is.finite(llim)) {
					if(llim<my_ll)  my_ll = llim
				}
			} else if(is.finite(llim)) {
				my_ll = llim
			}
        }
        if(is.finite(plot_ul)) {
            my_ul = plot_ul
        } else if(plot_using_test_limits && is.finite(ulim)) {
            my_ul = ulim
        } else {
            my_ul = NaN
            for(j in 1:datasets) {
                if(length(results_list[[j]])>0) {
                    local_max = max(results_list[[j]])
                    if(is.na(my_ul))  my_ul = local_max
                    else if(local_max>my_ul)  my_ul=local_max
                }
            }
			if(is.finite(my_ul)) {
				if(plot_widest_limits_or_values && is.finite(ulim)) {
					if(ulim>my_ul)  my_ul = ulim
				}
			} else if(is.finite(ulim)) {
				my_ul = ulim
			}
        }
        

        if(is.finite(my_ll) && is.finite(my_ul)) {
			if(plot_limits_plus_10pct) {
				# ok, move plot limits wider 10%
				middle = (my_ll + my_ul)/2.0
				my_ll = middle - 1.1*(middle-my_ll)
				my_ul = middle + 1.1*(my_ul-middle)
			}
            xlim = c(my_ll,my_ul)
            # now scale things...
            if (auto_scale) {
                # override scaling, based on data...
                # use range of plot, ENG notation
                #------------------------------------
                if(abs(my_ll)>abs(my_ul))  range=abs(my_ll)
                else  range=abs(my_ul)
                if(range>0.99e9)  scaler=-9
                else if(range>0.99e6)  scaler=-6
                else if(range>0.99e3)  scaler=-3
                else if(range>0.99)  scaler=0
                else if(range>0.99e-3)  scaler=3
                else if(range>0.99e-6)  scaler=6
                else if(range>0.99e-9)  scaler=9
                else   scaler=12
            }
            if (scaler<0) {
                prefix = big_prefixes[-1*scaler]
            } else if (scaler==0) {
                prefix = ""
            } else {
                prefix = lil_prefixes[scaler]
            }
            scale = 10^scaler

            xlim = xlim*scale
            if(is.finite(ll))  ll=ll*scale
            if(is.finite(ul))  ul=ul*scale

            # print out title line for this parameter
            #-----------------------------------------
            subscreen_num = split.screen(matrix(c(0.0,1.0,ytop-title_height,ytop),
                            1,4,byrow=FALSE),erase=FALSE)
            screen(subscreen_num)
            my_units = paste(prefix,sep="",units)
			# changed test_num from %d to %.0f for very large tnums
            my_title = sprintf("%.0f  %s",test_num,test_nam)
			title_width = strwidth(my_title,cex=1.0*scex)
			if (valid_alt_limits) {
				if (valid_alt2_limits) {
					if(is.finite(alt2_ll))  alt2_ll=alt2_ll*scale
					if(is.finite(alt2_ul))  alt2_ul=alt2_ul*scale
				}
                if(is.finite(alt_ll))  alt_ll=alt_ll*scale
                if(is.finite(alt_ul))  alt_ul=alt_ul*scale
				if (valid_alt2_limits) {
					my_limits1 = sprintf(" LL=%.2f ",ll)
					my_limits2 = sprintf("/ %.2f ",alt_ll)
					my_limits3 = sprintf("/ %.2f ",alt2_ll)
					my_limits4 = sprintf(" UL=%.2f ",ul)
					my_limits5 = sprintf("/ %.2f ",alt_ul)
					my_limits6 = sprintf("/ %.2f ",alt2_ul)
					my_limits7 = sprintf(" %s",my_units)
					my_limits = paste(my_limits1,my_limits2,my_limits3,my_limits4,my_limits5,my_limits6,my_limits7)
				} else {
					my_limits1 = sprintf(" LL=%.2f  UL=%.2f ",ll,ul)
					my_limits2 = sprintf(" LL2=%.2f  UL2=%.2f ",alt_ll,alt_ul)
					my_limits3 = sprintf(" %s",my_units)
					my_limits = paste(my_limits1,my_limits2,my_limits3)
				}
				# determine font size if testname+limits is too big for page width
				limits_width = strwidth(my_limits,cex=0.8*scex)
				title_cex=1.0*scex
				limits_cex=0.8*scex
				if((title_width+limits_width)>0.90) {
					adjust = 0.90/(title_width+limits_width)
					title_cex = title_cex*adjust
					limits_cex = limits_cex*adjust
				}
				text(0.0,0.3,sprintf("%s",my_title),pos=4,cex=title_cex)
                # shrink the font a little, since string is longer
                text(1.0,0.3,sprintf("%s",my_limits),pos=2, cex=limits_cex)
                # now underline with blue and green dashed lines, (and orange if 3 sets of limits)
				if (valid_alt2_limits) {
					widths=strwidth(c(my_limits1,my_limits2,my_limits3,my_limits4,my_limits5,my_limits6,my_limits7),
							cex=limits_cex)
					xll0a = 0.98 - widths[7] - widths[6] - widths[5] - widths[4] - widths[3] - 
							widths[2] - widths[1] + 0.01
					xll0b = 0.98 - widths[7] - widths[6] - widths[5] - widths[4] - widths[3] - 
							widths[2] - 0.01
					xll1a = 0.98 - widths[7] - widths[6] - widths[5] - widths[4] - widths[3] - 
							widths[2] + 0.01
					xll1b = 0.98 - widths[7] - widths[6] - widths[5] - widths[4] - widths[3] - 0.01
					xll2a = 0.98 - widths[7] - widths[6] - widths[5] - widths[4] - widths[3] + 0.01
					xll2b = 0.98 - widths[7] - widths[6] - widths[5] - widths[4] - 0.01
					xul0a = 0.98 - widths[7] - widths[6] - widths[5] - widths[4] + 0.01
					xul0b = 0.98 - widths[7] - widths[6] - widths[5] - 0.01
					xul1a = 0.98 - widths[7] - widths[6] - widths[5] + 0.01
					xul1b = 0.98 - widths[7] - widths[6] - 0.01
					xul2a = 0.98 - widths[7] - widths[6] + 0.01
					xul2b = 0.98 - widths[7] - 0.01
					lines(c(xll0a,xll0b),c(0.1,0.1),lty="dashed",lwd=2,col="blue")
					lines(c(xll1a,xll1b),c(0.1,0.1),lty="dotdash",lwd=2,col="green")
					lines(c(xll2a,xll2b),c(0.1,0.1),lty="dashed",lwd=2,col="orange")
					lines(c(xul0a,xul0b),c(0.1,0.1),lty="dashed",lwd=2,col="blue")
					lines(c(xul1a,xul1b),c(0.1,0.1),lty="dotdash",lwd=2,col="green")
					lines(c(xul2a,xul2b),c(0.1,0.1),lty="dashed",lwd=2,col="orange")

				} else {
					widths=strwidth(c(my_limits1,my_limits2,my_limits3),cex=limits_cex)
					x0 = 0.98 - widths[3] - widths[2] - widths[1] + 0.01
					x1 = 0.98 - widths[3] - widths[2] - 0.01
					x2 = 0.98 - widths[3] - widths[2] + 0.01
					x3 = 0.98 - widths[3] - 0.01
					lines(c(x0,x1),c(0.1,0.1),lty="dashed",lwd=2,col="blue")
					lines(c(x2,x3),c(0.1,0.1),lty="dotdash",lwd=2,col="green")
				}
            } else {
                my_limits = sprintf("LL=%.2f  UL=%.2f  %s",ll,ul,my_units)
				# determine font size if testname+limits is too big for page width
				limits_width = strwidth(my_limits,cex=0.9*scex)
				title_cex=1.0*scex
				limits_cex=0.9*scex
				if((title_width+limits_width)>0.90) {
					adjust = 0.90/(title_width+limits_width)
					title_cex = title_cex*adjust
					limits_cex = limits_cex*adjust
				}
				text(0.0,0.3,sprintf("%s",my_title),pos=4,cex=title_cex)
                text(1.0,0.3,sprintf("%s",my_limits),pos=2, cex=limits_cex)
            }
            close.screen(subscreen_num)
            ytop = ytop - title_height

            # for each dataset...
            #-----------------------
            if (do_xy_plots && !do_hist_and_xy)  hist_datasets = 1
            else  hist_datasets = datasets
			for(j in 1:hist_datasets) {
				results = results_list[[j]]
				results2=results[(results>=my_ll)]
				results2=results2[(results2<=my_ul)]

				results = results*scale
				results2 = results2*scale
				my_count = length(results)
				omitted = my_count - length(results2)
				if(my_count>0) {
					the_mean = mean(results)
					the_sdev = sd(results)
					my_min = min(results)
					my_max = max(results)
					my_q25 = quantile(results,0.25)
					my_q50 = median(results)
					my_q75 = quantile(results,0.75)
					r_sdev = abs(my_q75 - my_q25)/1.34898
					r_ll_sdev = 2.0*abs(my_q50 - my_q25)/1.34898
					r_ul_sdev = 2.0*abs(my_q75 - my_q50)/1.34898
				} else {
					the_mean = NaN
					the_sdev = NaN
					my_min = NaN
					my_max = NaN
					my_q25 = NaN
					my_q50 = NaN
					my_q75 = NaN
					r_sdev = NaN
					r_ll_sdev = NaN
					r_ul_sdev = NaN
				}
				if(do_guardbands && (j==1)) {
					gb_means=the_mean
					gb_sds=the_sdev
					gb_slopes=1             # just creating variable here...
					gb_yint_ll=0			# just creating variable here...
					gb_yint_ul=0			# just creating variable here...
				}
				if(do_robust_stats>1) {
					my_mean = my_q50
					my_sdev = r_sdev
					ll_sdev = r_ll_sdev
					ul_sdev = r_ul_sdev
				} else if(do_robust_stats) {
					my_mean = my_q50
					my_sdev = r_sdev
					ll_sdev = my_sdev
					ul_sdev = my_sdev
				} else {
					my_mean = the_mean
					my_sdev = the_sdev
					ll_sdev = my_sdev
					ul_sdev = my_sdev
				}
				lo4sd = my_mean - (4*ll_sdev)
				lo6sd = my_mean - (6*ll_sdev)
				hi4sd = my_mean + (4*ul_sdev)
				hi6sd = my_mean + (6*ul_sdev)
				if(valid_alt2_limits && (use_alt_lims[j]==2)) {
					if(is.finite(alt2_ll))  cpklo = (my_mean - alt2_ll)/(3.0*ll_sdev)
					else  cpklo = 99.99
					if(is.finite(alt2_ul))  cpkhi = (alt2_ul - my_mean)/(3.0*ul_sdev)
					else  cpkhi = 99.99
				} else if(valid_alt_limits && (use_alt_lims[j]==1)) {
					if(is.finite(alt_ll))  cpklo = (my_mean - alt_ll)/(3.0*ll_sdev)
					else  cpklo = 99.99
					if(is.finite(alt_ul))  cpkhi = (alt_ul - my_mean)/(3.0*ul_sdev)
					else  cpkhi = 99.99
				} else {
					if(is.finite(ll))  cpklo = (my_mean - ll)/(3.0*ll_sdev)
					else  cpklo = 99.99
					if(is.finite(ul))  cpkhi = (ul - my_mean)/(3.0*ul_sdev)
					else  cpkhi = 99.99
				}
				if(is.finite(cpklo) && (cpklo>99.99))  cpklo = 99.99
				if(is.finite(cpkhi) && (cpkhi>99.99))  cpkhi = 99.99
				cpk = min(c(cpklo,cpkhi))


				# stats part of plot
				#-----------------------
				if(!just_superimposed_histo) {
					subscreen_num = split.screen(matrix(c(0.0,0.45,ytop-hist_height,ytop),
									1,4,byrow=FALSE),erase=FALSE)
					screen(subscreen_num)
					text(0.0,0.84,dataset_name[j],pos=4,cex=0.6*scex)
					sh = 0.0
					if(do_robust_stats>1) {
						text(0.0,0.70,sprintf("R_Mean = %.3f",my_mean),pos=4,cex=0.6*scex)
						text(0.0,0.56,sprintf("R_LL_SDev = %.3f",ll_sdev),pos=4,cex=0.6*scex)
						text(0.0,0.42,sprintf("R_UL_SDev = %.3f",ul_sdev),pos=4,cex=0.6*scex)
						sh = 0.14
					} else if(do_robust_stats) {
						text(0.0,0.70,sprintf("R_Mean = %.3f",my_mean),pos=4,cex=0.6*scex)
						text(0.0,0.56,sprintf("R_SDev = %.3f",my_sdev),pos=4,cex=0.6*scex)
					} else {
						text(0.0,0.70,sprintf("Mean = %.3f",my_mean),pos=4,cex=0.6*scex)
						text(0.0,0.56,sprintf("SDev = %.3f",my_sdev),pos=4,cex=0.6*scex)
					}
					text(0.0,0.42-sh,sprintf("Count = %d",my_count),pos=4,cex=0.6*scex)
					if (outside_limits_count) {
						if(valid_alt2_limits && (use_alt_lims[j]==2)) {
							llim = alt2_ll
							ulim = alt2_ul
						} else if(valid_alt_limits && (use_alt_lims[j]==1)) {
							llim = alt_ll
							ulim = alt_ul
						} else {
							llim = ll
							ulim = ul
						}
						if(is.finite(llim))  ll_fails = length(results[(results<llim)])
						else  ll_fails = 0
						if(is.finite(ulim))  ul_fails = length(results[(results>ulim)])
						else  ul_fails = 0
						fails = ll_fails + ul_fails
						if (fails>0) {
							text(0.0,0.28-sh,sprintf("Outside limits = %d",fails),
								pos=4,cex=0.6*scex,col="red")
						} else {
							text(0.0,0.28-sh,sprintf("Outside limits = %d",fails),
								pos=4,cex=0.6*scex)
						}
					} else {
						if (omitted>0) {
							text(0.0,0.28-sh,sprintf("Off the plot = %d",omitted),
								pos=4,cex=0.6*scex,col="red")
						} else {
							text(0.0,0.28-sh,sprintf("Off the plot = %d",omitted),
								pos=4,cex=0.6*scex)
						}
					}
					if (is.na(cpklo) || (cpklo<1.33)) {
						text(0.33,0.70,sprintf("Cpklo = %.2f",cpklo),
							pos=4,cex=0.6*scex,col="red")
					} else {
						text(0.33,0.70,sprintf("Cpklo = %.2f",cpklo),
							pos=4,cex=0.6*scex)
					}
					text(0.33,0.56,sprintf("Lo4sd = %.2f",lo4sd),pos=4,cex=0.6*scex)
					text(0.33,0.42,sprintf("Lo6sd = %.2f",lo6sd),pos=4,cex=0.6*scex)
					text(0.33,0.28,sprintf("Min = %.3f",my_min),pos=4,cex=0.6*scex)
					if (is.na(cpkhi) || (cpkhi<1.33)) {
						text(0.67,0.70,sprintf("Cpkhi = %.2f",cpkhi),
							pos=4,cex=0.6*scex,col="red")
					} else {
						text(0.67,0.70,sprintf("Cpkhi = %.2f",cpkhi),pos=4,cex=0.6*scex)
					}
					text(0.67,0.56,sprintf("Hi4sd = %.2f",hi4sd),pos=4,cex=0.6*scex)
					text(0.67,0.42,sprintf("Hi6sd = %.2f",hi6sd),pos=4,cex=0.6*scex)
					text(0.67,0.28,sprintf("Max = %.3f",my_max),pos=4,cex=0.6*scex)
					close.screen(subscreen_num)
				}

				# dump stats info to .csv file 
				#------------------------------
				if (do_csv) {
					if (csv_header) {
						csv_conn = file(csv_name,"w")
						the_string = "page,tnum,testname,subplot,units,ll,ul,mean,sd,"
						cat(the_string,file=csv_conn)
						the_string = "count,min,q25,q50,q75,max,robust_sd,"
						cat(the_string,file=csv_conn)
						the_string = "cpk,cpklo,cpkhi,rcpk,rcpklo,rcpkhi,"
						cat(the_string,file=csv_conn)
						the_string = "fails,ll_fails,ul_fails,lo4sd,lo6sd,hi4sd,hi6sd,"
						cat(the_string,file=csv_conn)
						the_string = "robust_ll_sd,robust_ul_sd,r2cpk,r2cpklo,r2cpkhi \n"
						cat(the_string,file=csv_conn)
						csv_row = 1
						csv_header = FALSE
					}
					csv_row = csv_row + 1
					if(valid_alt2_limits && (use_alt_lims[j]==2)) {
						llim = alt2_ll
						ulim = alt2_ul
					} else if(valid_alt_limits && (use_alt_lims[j]==1)) {
						llim = alt_ll
						ulim = alt_ul
					} else {
						llim = ll
						ulim = ul
					}
					if(is.finite(llim))  my_llim = sprintf("%g",llim)
					else  my_llim = ""
					if(is.finite(ulim))  my_ulim = sprintf("%g",ulim)
					else  my_ulim = ""
					# tnum is uint32, not int32, so %d doesn't always work, changed to %.0f
					# cols A,B,C,D,E,F,G,H,I,
					the_string = sprintf("%d,%.0f,%s,%d,%s,%s,%s,%g,%g,",
								page_count,        test_num,        test_nam,
								j,                my_units,        my_llim,
								my_ulim,        the_mean,        the_sdev)
					cat(the_string,file=csv_conn)
					# cols J,K,L,M,N,O,P,
					the_string = sprintf("%d,%g,%g,%g,%g,%g,%g,",
								my_count,        my_min,                my_q25,
								my_q50,                my_q75,                my_max,
								r_sdev)
					cat(the_string,file=csv_conn)
					# cols Q,R,S,
					if(use_csv_formulas && use_OOCalc_csv) {
						n = csv_row        #datasets*(i-1)+j+1
						the_string1 = sprintf("=MIN(R%d;S%d),",n,n)
						the_string2 = sprintf("=IF(ISNUMBER(F%d);(H%d-F%d)/(3*I%d);99),",n,n,n,n)
						the_string3 = sprintf("=IF(ISNUMBER(G%d);(G%d-H%d)/(3*I%d);99),",n,n,n,n)
						the_string = sprintf("%s%s%s",the_string1,the_string2,the_string3)
					} else if(use_csv_formulas) {
						n = csv_row        #datasets*(i-1)+j+1
						the_string1 = sprintf("\"=MIN(R%d,S%d)\",",n,n)
						the_string2 = sprintf("\"=IF(ISNUMBER(F%d),(H%d-F%d)/(3*I%d),99)\",",n,n,n,n)
						the_string3 = sprintf("\"=IF(ISNUMBER(G%d),(G%d-H%d)/(3*I%d),99)\",",n,n,n,n)
						the_string = sprintf("%s%s%s",the_string1,the_string2,the_string3)
					} else {
						the_string = sprintf("%g,%g,%g,",
								cpk,                cpklo,                cpkhi)
					}
					cat(the_string,file=csv_conn)
					# cols T,U,V  (rcpk,rcpklo,rcpkhi)
					if(use_csv_formulas && use_OOCalc_csv) {
						the_string1 = sprintf("=MIN(U%d;V%d),",n,n)
						the_string2 = sprintf("=IF(ISNUMBER(F%d);(M%d-F%d)/(3*P%d);99),",n,n,n,n)
						the_string3 = sprintf("=IF(ISNUMBER(G%d);(G%d-M%d)/(3*P%d);99),",n,n,n,n)
						the_string = sprintf("%s%s%s",the_string1,the_string2,the_string3)
					} else if(use_csv_formulas) {
						the_string1 = sprintf("\"=MIN(U%d,V%d)\",",n,n)
						the_string2 = sprintf("\"=IF(ISNUMBER(F%d),(M%d-F%d)/(3*P%d),99)\",",n,n,n,n)
						the_string3 = sprintf("\"=IF(ISNUMBER(G%d),(G%d-M%d)/(3*P%d),99)\",",n,n,n,n)
						the_string = sprintf("%s%s%s",the_string1,the_string2,the_string3)
					} else {
						if(is.finite(llim))  rcpklo = (my_q50-llim)/(3*r_sdev)
						else  rcpklo = 99
						if(is.finite(ulim))  rcpkhi = (ulim-my_q50)/(3*r_sdev)
						else  rcpkhi = 99
						rcpk = min(c(rcpklo,rcpkhi))
						the_string = sprintf("%g,%g,%g,",
								rcpk,                rcpklo,                rcpkhi)
					}
					cat(the_string,file=csv_conn)
					# cols W,X,Y  (fails,ll_fails,ul_fails)
					if(is.finite(llim))  ll_fails = length(results[results<llim])
					else  ll_fails = 0
					if(is.finite(ulim))  ul_fails = length(results[results>ulim])
					else  ul_fails = 0
					fails = ll_fails + ul_fails
					the_string = sprintf("%g,%g,%g,",
								fails,                ll_fails,            ul_fails)
					cat(the_string,file=csv_conn)
					# cols Z,AA,AB,AC (lo4sd,lo6sd,hi4sd,hi6sd)
					the_string = sprintf("%g,%g,%g,%g,",
								lo4sd,                lo6sd,
								hi4sd,                hi6sd)
					cat(the_string,file=csv_conn)
					# cols AD,AE,AF,AG,AH (robust_ll_sd,robust_ul_sd,r2cpk,r2cpklo,r2cpkhi)
					the_string = sprintf("%g,%g,",
								r_ll_sdev,                r_ul_sdev)
					cat(the_string,file=csv_conn)
					if(use_csv_formulas && use_OOCalc_csv) {
						the_string1 = sprintf("=MIN(AG%d;AH%d),",n,n)
						the_string2 = sprintf("=IF(ISNUMBER(F%d);(M%d-F%d)/(3*AD%d);99),",n,n,n,n)
						the_string3 = sprintf("=IF(ISNUMBER(G%d);(G%d-M%d)/(3*AE%d);99) \n",n,n,n,n)
						the_string = sprintf("%s%s%s",the_string1,the_string2,the_string3)
					} else if(use_csv_formulas) {
						the_string1 = sprintf("\"=MIN(AG%d,AH%d)\",",n,n)
						the_string2 = sprintf("\"=IF(ISNUMBER(F%d),(M%d-F%d)/(3*AD%d),99)\",",n,n,n,n)
						the_string3 = sprintf("\"=IF(ISNUMBER(G%d),(G%d-M%d)/(3*AE%d),99)\" \n",n,n,n,n)
						the_string = sprintf("%s%s%s",the_string1,the_string2,the_string3)
					} else {
						if(is.finite(llim))  rcpklo = (my_q50-llim)/(3*r_ll_sdev)
						else  rcpklo = 99
						if(is.finite(ulim))  rcpkhi = (ulim-my_q50)/(3*r_ul_sdev)
						else  rcpkhi = 99
						rcpk = min(c(rcpklo,rcpkhi))
						the_string = sprintf("%g,%g,%g \n",
								rcpk,                rcpklo,                rcpkhi)
					}
					cat(the_string,file=csv_conn)
				}


				# histogram part of the plot
				#----------------------------
				if(!just_superimposed_histo) {
					do_boxes = TRUE        #FALSE
					if (do_boxes) {
						# do <plot box and >plot box
						# omitted / my_count  is all omitted, we want < and > counts...
						if (omitted>0) {
							#browser()
							off_left = length(results[(results<(scale*my_ll))])/my_count
							off_right = length(results[(results>(scale*my_ul))])/my_count
						} else {
							off_left = 0.0
							off_right = 0.0
						}
						# devices off left side of histogram
						#------------------------------------
						subscreen_num=split.screen(matrix(c(0.45,0.46,ytop-hist_height,ytop),
									1,4,byrow=FALSE),erase=FALSE)
						screen(subscreen_num)
						par(plt=c(0.0,1.0,0.20,0.95))
						plot(c(0,1),c(0,1),xlab="",ylab="",type="n",xaxt="n",yaxt="n",
											yaxs="i",xaxs="i")
						rect(0,0,1,off_left,col="red")
						close.screen(subscreen_num)

						# devices off right side of histogram
						#-------------------------------------
						subscreen_num=split.screen(matrix(c(0.98,0.99,ytop-hist_height,ytop),
									1,4,byrow=FALSE),erase=FALSE)
						screen(subscreen_num)
						par(plt=c(0.0,1.0,0.20,0.95))
						plot(c(0,1),c(0,1),xlab="",ylab="",type="n",xaxt="n",yaxt="n",
											yaxs="i",xaxs="i")
						rect(0,0,1,off_right,col="red")
						close.screen(subscreen_num)
					}

					# do histogram plot
					#------------------
					subscreen_num=split.screen(matrix(c(0.46,0.98,ytop-hist_height,ytop),
									1,4,byrow=FALSE),erase=FALSE)
					screen(subscreen_num)

					breaks=25
					if (length(results2)>200)  breaks=50
					if (length(results2)>0) {
						if (my_ul<=my_ll) {
							par(plt=c(0.1,0.95,0.20,0.95))
						} else {
							if (do_norm_prob_plots) {
								# format x and y data...
								x = sort(results2)
								y = qnorm(ppoints(length(x)))

								# y axis tick locations...
								probs <- c(.01, .05, seq(.1,.9, by=.1), .95, .99)
								if(length(x)>=1000)  probs <- c(0.001, probs, .999)
								qprobs <- qnorm(probs)
 
								# do basic plot...
								par(plt=c(0.1,0.95,0.20,0.95))
								plot(x, y, axes=FALSE, type="n", ylim=range(c(y,qprobs)),
										xlim=c(my_ll*scale,my_ul*scale), xlab="", ylab="",
										mgp=c(2,0.4*scex,0),cex.axis=0.6*scex,xaxs="i")
								points(x, y, pch="+")

								# grid lines...
								box()
 								abline(h=qprobs, col="grey",lty="dotted")
								axis(1,cex.axis=0.6*scex,mgp=c(2,0.4*scex,0),xaxs="i")
								axis(2, at=qprobs, labels=100*probs,cex.axis=0.6*scex)
								grid(ny=NA)

							} else {
								if (max(results2) <= min(results2)) {
									if (min(results2)<my_ll*scale)  bin_ll=min(results2)
									else  bin_ll = my_ll*scale
									if (max(results2)>my_ul*scale)  bin_ul=max(results2)
									else  bin_ul = my_ul*scale
									bin_width=(bin_ul-bin_ll)/200.0
									breaks=seq(bin_ll-bin_width,bin_ul+bin_width,by=bin_width)
									# browser()
								}
								hist_obj = hist(results2,breaks=breaks,plot=FALSE)
								par(plt=c(0.1,0.95,0.20,0.95))
								plot(hist_obj,col="cyan",xlim=xlim,main="",xlab="",ylab="",
								   bty="o",mgp=c(2,0.4*scex,0),cex.axis=0.6*scex,xaxs="i")        
								# need to try plot ( xaxt="n" to suppress axis,
								# then use axis() command to see if we can tweak things

								#hist(results2,breaks=breaks,plt=c(0.1,0.95,0.20,0.95),
								#        col="cyan",xlim=xlim,main="",xlab="",ylab="",
								#        bty="o",cex.axis=0.8)
							}

							if (valid_alt2_limits && (use_alt_lims[j]==2)) {
								if (is.finite(alt2_ll))  abline(v=alt2_ll,lty="dashed", lwd=2,col="orange") 
								if (is.finite(alt2_ul))  abline(v=alt2_ul,lty="dashed", lwd=2,col="orange")
							} else if (valid_alt_limits && (use_alt_lims[j]==1)) {
								if (is.finite(alt_ll))  abline(v=alt_ll,lty="dotdash", lwd=2,col="green") 
								if (is.finite(alt_ul))  abline(v=alt_ul,lty="dotdash", lwd=2,col="green")
							} else {
								if (is.finite(ll))  abline(v=ll,lty="dashed",lwd=2,col="blue") 
								if (is.finite(ul))  abline(v=ul,lty="dashed",lwd=2,col="blue")
							}
							if (add_normal_curve)  {
								if (do_norm_prob_plots) {
									y_q25 = qnorm(0.25)
									y_q75 = qnorm(0.75)
									slope = (y_q75 - y_q25)/(my_q75 - my_q25)
									y_intercept = y_q25 - slope*my_q25
									if(is.finite(slope) && is.finite(y_intercept))
										abline(a=y_intercept,b=slope,col="black")									
								} else {	
									norm_x = norm_x0*my_sdev + my_mean
									my_breaks=hist_obj$breaks
									bin_width = my_breaks[2] - my_breaks[1]
									norm_y = norm_y0*my_count*bin_width/my_sdev
									lines(norm_x,norm_y,lty="dotted",lwd=2,col="black")
								}
							}
						}
					} else {
						par(plt=c(0.1,0.95,0.20,0.95))
					}
					grid()
					close.screen(subscreen_num)
					ytop = ytop - hist_height
				}
			}
            if(superimpose_hist) {
				super_ll = ll
				super_ul = ul
           
				# modify super_ul, super_ll if necessary
#				if (superimpose_hist_use_alt_lim && (alt_limits != "") ) {
#					load(alt_limits)
#					index = match(test_nam,ParametersFrame[["testname"]],nomatch=NaN)
#					if (is.finite(index)) {
#						temp_ll = ParametersFrame[[index,"ll"]]
#						temp_ul = ParametersFrame[[index,"ul"]]
#						if( is.finite(temp_ll) && is.finite(temp_ul) ) {
#							super_ll = temp_ll*scale
#							super_ul = temp_ul*scale             
#						}
#					}
#				}
				if(do_csv) {
					csv_row = csv_row + 1
					# cols A,B,C,D,E,
					the_string = sprintf("%d,%d,%s,%d,%s,",
								page_count,        test_num,        test_nam,
								j+1,               my_units)
					cat(the_string,file=csv_conn)
				}
				PlotSuperHist(hist_datasets,results_list, do_guardbands , do_robust_stats,
							ytop, hist_height, dataset_name, my_ul, my_ll, xlim, 
							super_ul, super_ll, add_normal_curve, scale,
							do_csv,csv_conn, csv_row, use_csv_formulas, use_OOCalc_csv,
							do_norm_prob_plots, scex)
				ytop = ytop - hist_height
            }
            if(do_xy_plots) {
                x_vector = scale*xy_results_list[[1]]
                for(j in 2:datasets) {
                    y_vector = scale*xy_results_list[[j]]
                    valid_xys = is.finite(x_vector) & is.finite(y_vector)
                    
                    #save(list=ls(all=TRUE),file=sprintf("workspace_%d_%d.Rdata",i,j))

                    # determine linear fit based on middle 80% of shifts
                    #----------------------------------------------------
                    if(length(y_vector[valid_xys])>9) {
                        y_v2 = y_vector[valid_xys]
                        x_v2 = x_vector[valid_xys]

                        # getting middle 80% of shifted data
                        #-------------------------------------
                        shifts = y_v2 - x_v2
                        sorted=sort(shifts,decreasing=FALSE,index.return=TRUE)
                        xrefs = sorted$ix
                        lenx = length(xrefs)
                        i10pct = as.integer(0.1*lenx+0.5)
                        i90pct = lenx - i10pct
                        xrefs_2 = xrefs[i10pct+1:i90pct]

                        #browser()

                        # do least squares linear fit
                        #----------------------------
                        x_range = max(x_v2[xrefs_2]) - min(x_v2[xrefs_2])
                        y_range = max(y_v2[xrefs_2]) - min(y_v2[xrefs_2])
                        if( (x_range>0.00001) && (y_range>0.00001) ) {
                            my_fit = lsfit(x_v2[xrefs_2],y_v2[xrefs_2])
                            slope = as.numeric(my_fit$coefficients[2])
                            y_intercept = as.numeric(my_fit$coefficients[1])
                            nom_y = slope * x_vector[valid_xys] + y_intercept
                            my_r = cor(nom_y,y_vector[valid_xys])
                            if (!is.finite(my_r)) {
                                #browser()
                                my_r = 0.0
                            }
                        } else {
                            my_r = 0.0
                        }
                        # if dataset is too noisy for accurate lsfit(), use
                        # alternative method:
                        # slope of line based on s_devs, going
                        # through the mean x / mean y point.
                        #--------------------------------------------------
                        if ( (my_r<0.9) && (my_r>0.0) ) {
                            mean_x = mean(x_v2[xrefs_2])
                            mean_y = mean(y_v2[xrefs_2])
                            sd_x = sd(x_v2[xrefs_2])
                            sd_y = sd(y_v2[xrefs_2])
                            slope = sd_y/sd_x
                            y_intercept = mean_y - slope*mean_x
                            nom_y = slope * x_vector[valid_xys] + y_intercept
                            my_r = cor(nom_y,y_vector[valid_xys])
                        } 

                        if (my_r>0) {
                            # for guardband calculations, use lines shifted
                            # +/- 4sd above and below lsfit line  to determine
                           # x to y limit relationships
                            #--------------------------------------------------
                            shifts = nom_y - y_vector[valid_xys]
                            shift_mean = mean(shifts)   
                            shift_sd = sd(shifts)
                            y_intercept_ul = y_intercept + 4*shift_sd + shift_mean
                            y_intercept_ll = y_intercept - 4*shift_sd + shift_mean
                        } else {
                            slope = NaN
                            my_r = 0 
                            y_intercept = NaN
                            y_intercept_ll = NaN
                            y_intercept_ul = NaN
                        }

                    } else {
                        slope = NaN
                        my_r = 0 
                        y_intercept = NaN
                        y_intercept_ll = NaN
                        y_intercept_ul = NaN
                    }


                    # xy plot statistics subscreen
                    #----------------------------------

                    # if rightside plot, need to adjust ytop up, shift x over...
                    #------------------------------------------------------------
                    if((j%%2)>0) {
                        x0 = 0.5
                        ytop = ytop + (2*hist_height)
                    } else {
                        x0 = 0.0
                    }
                    subscreen_num = split.screen(matrix(c(x0,x0+0.47,ytop-(2*hist_height),ytop),
                                    1,4,byrow=FALSE),erase=FALSE)
                    screen(subscreen_num)
                    text(0.0,0.90,sprintf("%s",dataset_name[j]),pos=4,cex=0.7*scex)
                    text(0.0,0.76,sprintf("Slope = %.3f",slope),pos=4,cex=0.6*scex)
                    text(0.0,0.62,sprintf("Yint = %.3f",y_intercept),pos=4,cex=0.6*scex)
                    text(0.0,0.48,sprintf("UL_yint = %.3f",y_intercept_ul),pos=4,cex=0.6*scex)
                    text(0.0,0.34,sprintf("LL_yint = %.3f",y_intercept_ll),pos=4,cex=0.6*scex)
                    if (my_r<0.9) {
                        text(0.0,0.20,sprintf("R = %.2f",my_r),pos=4,cex=0.6*scex,col="red")
                    } else {
                        text(0.0,0.20,sprintf("R = %.2f",my_r),pos=4,cex=0.6*scex)
                    }
                    close.screen(subscreen_num)

                    # xy plot subscreen
                    #-------------------
                    subscreen_num = split.screen(matrix(c(x0+0.17,x0+0.5,ytop-(2*hist_height),ytop),
                                    1,4,byrow=FALSE),erase=FALSE)
                    screen(subscreen_num)
                    #if (hist_height>0.14)  par(plt=c(0.1,0.95,0.20,0.85))
                    #else  par(plt=c(0.1,0.95,0.20,0.75))
                    par(plt=c(0.1,0.95,0.15,0.80))
                    if(length(y_vector[valid_xys])>0) {
                        plot(x_vector,y_vector,main="",xlab="",ylab="",
                            mgp=c(1.4,0.4*scex,0),cex.axis=0.6*scex,cex.lab=0.7*scex,pch="+")
                        #plot(x_vector,y_vector,main=dataset_name[j],xlab="",
                        #    ylab="",mgp=c(2,0.4,0),cex.axis=0.6,cex.main=0.6)
                        if (my_r>0.0) {
                            abline(y_intercept,slope,lty="dashed",col="green") 
                            abline(y_intercept_ul,slope,lty="dashed",col="red")
                            abline(y_intercept_ll,slope,lty="dashed",col="red")
                        }
                        # or abline(reg=my_fit,...) 
                            grid()
                    }
                    close.screen(subscreen_num)
                    ytop = ytop - (2*hist_height)


                    # save per dataset statistics for guardbanding
                    if (do_guardbands) {
                        results = results_list[[j]]
                        my_count = length(results)
                        if(my_count>0) {
                            gb_means[j]=scale*mean(results)
                            gb_sds[j]=scale*sd(results)
                        } else {
                            gb_means[j]=NaN
                            gb_sds[j]=NaN
                        }
                        gb_slopes[j]=slope
                        gb_yint_ll[j]=y_intercept_ll
                        gb_yint_ul[j]=y_intercept_ul
                    }
                }
                if (do_guardbands) {
                    # dump info to the csv file in csv format
                    #------------------------------------------
                    if (gb_header) {
                        gb_conn = file(gb_name,"w")
                        # col A,B,C,D,E
                        the_string = "tnum,testname,units,\"Lim1\nll\",\"Lim1\nul\","
                        cat(the_string,file=gb_conn)
                        # col F,G,H,I,J,K
                        the_string = "cpkll,cpkul,gbll,gbul,cpgbll,cpgbul,"
                        cat(the_string,file=gb_conn)
                        # col L,M
                        the_string = "\"Lim2\nll\",\"Lim2\nul\","
                        cat(the_string,file=gb_conn)
                        # col N,O,P,Q,R,S
                        the_string = "cpkll,cpkul,gbll,gbul,cpgbll,cpgbul,"
                        cat(the_string,file=gb_conn)
                        # col T,U,
                        the_string = "\"Lim1\nmean\",\"Lim1\nsd\","
                        cat(the_string,file=gb_conn)
                        for(j in 2:datasets) {
                            # col V,W,X,Y,Z
                            the_string = "\"Set2\nmean\",\"Set\nsd\",yintll,yintul,slope,"
                            cat(the_string,file=gb_conn)
                            # col AA,AB,AC,AD,AE,AF,
                            the_string = "cpkll,cpkul,gbll,gbul,cpgbll,cpgbul,"
                            cat(the_string,file=gb_conn)
                            # col AG,AH,
                            the_string = "l1gbll,l1gbul,"
                            cat(the_string,file=gb_conn)
                        }
                        the_string = "\n"
                        cat(the_string,file=gb_conn)
                        gb_header=FALSE
                    }

                    # col A,B,C  tnum testname units
                    the_string = sprintf("%d,%s,%s,",test_num,test_nam,my_units)
                    cat(the_string,file=gb_conn)
                    # col D,E  llim ulim
                    if(is.finite(ll))  my_ll = sprintf("%g,",ll)
                    else  my_ll = ","
                    if(is.finite(ul))  my_ul = sprintf("%g,",ul)
                    else  my_ul = ","
                    the_string = paste(my_ll,my_ul,sep="")
                    cat(the_string,file=gb_conn)
                    # col F,G  cpk_llim cpk_ulim
                    n=i+1
                    if(use_OOCalc_csv) {
                        the_string = sprintf("=IF(ISNUMBER(D%d);(T%d-D%d)/(3*U%d);99)",n,n,n,n)
                        the_string = paste(the_string,
                                "+STYLE(IF(CURRENT()<2;\"yellow\";\"Default\")),",sep="")
                        cat(the_string,file=gb_conn)
                        the_string = sprintf("=IF(ISNUMBER(E%d);(E%d-T%d)/(3*U%d);99)",n,n,n,n)
                        the_string = paste(the_string,
                                "+STYLE(IF(CURRENT()<2;\"yellow\";\"Default\")),",sep="")
                    } else {
                        the_string = sprintf("\"=IF(ISNUMBER(D%d),(T%d-D%d)/(3*U%d),99)\"",n,n,n,n)
                        the_string = paste(the_string,",",sep="")
                        cat(the_string,file=gb_conn)
                        the_string = sprintf("\"=IF(ISNUMBER(E%d),(E%d-T%d)/(3*U%d),99)\"",n,n,n,n)
                        the_string = paste(the_string,",",sep="")
                    }
                    cat(the_string,file=gb_conn)

                    # col H, I  gb_llim gb_ulim
                    cols = 21+12+13*((2:datasets)-2)        # l1gbll
                    cells = paste(colnum2char(cols),n,sep="")
                    if(use_OOCalc_csv)  cells = paste(cells,collapse=";")
                    else  cells = paste(cells,collapse=",")
                    the_string = sprintf("=MAX(%s),",cells)
                    cat(the_string,file=gb_conn)
                    cols = cols + 1            # l1gbll
                    cells = paste(colnum2char(cols),n,sep="")
                    if(use_OOCalc_csv)  cells = paste(cells,collapse=";")
                    else  cells = paste(cells,collapse=",")
                    the_string = sprintf("=MIN(%s),",cells)
                    cat(the_string,file=gb_conn)
                    # col J, K  cp_gb_llim cp_gb_ulim
                    if(use_OOCalc_csv) {
                        the_string = sprintf("=IF(AND(ISNUMBER(D%d);ISNUMBER(L%d));(D%d-H%d)/(3*U%d)",
                                                n,n,n,n,n)
                        the_string = paste(the_string,
                            "+STYLE(IF(CURRENT()<0;\"yellow\";\"Default\"));\"\"),",sep="")
                        cat(the_string,file=gb_conn)
                        the_string = sprintf("=IF(AND(ISNUMBER(E%d);ISNUMBER(M%d));(I%d-E%d)/(3*U%d)",
                                                n,n,n,n,n)
                        the_string = paste(the_string,
                            "+STYLE(IF(CURRENT()<0;\"yellow\";\"Default\"));\"\"),",sep="")
                    } else {
                        the_string = sprintf("\"=IF(AND(ISNUMBER(D%d),ISNUMBER(L%d)),(D%d-H%d)/(3*U%d)",
                                                n,n,n,n,n)
                        the_string = paste(the_string,",\"\")\",",sep="")
                        cat(the_string,file=gb_conn)
                        the_string = sprintf("\"=IF(AND(ISNUMBER(E%d),ISNUMBER(M%d)),(I%d-E%d)/(3*U%d)",
                                                n,n,n,n,n)
                        the_string = paste(the_string,",\"\")\",",sep="")
                    }
                    cat(the_string,file=gb_conn)

                    # col L, M   alt_llim alt_ulim
                    if(is.finite(alt_ll))  my_ll = sprintf("%g,",alt_ll)
                    else  my_ll = ","
                    if(is.finite(alt_ul))  my_ul = sprintf("%g,",alt_ul)
                    else  my_ul = ","
                    the_string = paste(my_ll,my_ul,sep="")
                    cat(the_string,file=gb_conn)
                    # col N,O,P,Q,R,S  cpk_alt_llim cpk_alt_ulim gb_alt_ll gb_alt_ul cp_gb_alt_ll cp_gb_alt_ul 
                    cols = 21+6+13*((2:datasets)-2)        # cpkll
                    cells = paste(colnum2char(cols),n,sep="")
                    if(use_OOCalc_csv) {
                        cells = paste(cells,collapse=";")
                            the_string = sprintf("=MIN(%s)",cells)
                        the_string = paste(the_string,
                            "+STYLE(IF(CURRENT()<2;\"yellow\";\"Default\")),",sep="")
                    } else {
                        cells = paste(cells,collapse=",")
                            the_string = sprintf("\"=MIN(%s)\"",cells)
                        the_string = paste(the_string,",",sep="")
                    }
                    cat(the_string,file=gb_conn)
                    cols = cols+1        # cpkul
                    cells = paste(colnum2char(cols),n,sep="")
                    if(use_OOCalc_csv) {
                        cells = paste(cells,collapse=";")
                        the_string = sprintf("=MIN(%s)",cells)
                        the_string = paste(the_string,
                            "+STYLE(IF(CURRENT()<2;\"yellow\";\"Default\")),",sep="")
                    } else {
                        cells = paste(cells,collapse=",")
                        the_string = sprintf("\"=MIN(%s)\"",cells)
                        the_string = paste(the_string,",",sep="")
                    }
                    cat(the_string,file=gb_conn)
                    cols = cols+1        # gbll
                    cells = paste(colnum2char(cols),n,sep="")
                    if(use_OOCalc_csv) {
                        cells = paste(cells,collapse=";")
                        the_string = sprintf("=MIN(%s),",cells)
                    } else {
                        cells = paste(cells,collapse=",")
                        the_string = sprintf("\"=MIN(%s)\",",cells)
                    }
                    cat(the_string,file=gb_conn)
                    cols = cols+1        # gbul
                    cells = paste(colnum2char(cols),n,sep="")
                    if(use_OOCalc_csv) {
                        cells = paste(cells,collapse=";")
                        the_string = sprintf("=MAX(%s),",cells)
                    } else {
                        cells = paste(cells,collapse=",")
                        the_string = sprintf("\"=MAX(%s)\",",cells)
                    }
                    cat(the_string,file=gb_conn)
                    cols = cols+1        # cpgbll
                    cells = paste(colnum2char(cols),n,sep="")
                    if(use_OOCalc_csv) {
                        cells = paste(cells,collapse=";")
                        the_string = sprintf("=MIN(%s)",cells)
                        the_string = paste(the_string,
                            "+STYLE(IF(CURRENT()<0;\"yellow\";\"Default\")),",sep="")
                    } else {
                        cells = paste(cells,collapse=",")
                        the_string = sprintf("\"=MIN(%s)\"",cells)
                        the_string = paste(the_string,",",sep="")
                    }
                    cat(the_string,file=gb_conn)
                    cols = cols+1        # cpgbul
                    cells = paste(colnum2char(cols),n,sep="")
                    if(use_OOCalc_csv) {
                        cells = paste(cells,collapse=";")
                        the_string = sprintf("=MIN(%s)",cells)
                        the_string = paste(the_string,
                            "+STYLE(IF(CURRENT()<0;\"yellow\";\"Default\")),",sep="")
                    } else {
                        cells = paste(cells,collapse=",")
                        the_string = sprintf("\"=MIN(%s)\"",cells)
                        the_string = paste(the_string,",",sep="")
                    }
                    cat(the_string,file=gb_conn)
                    # col T,U  dataset 1 mean and sd 
                    the_string = sprintf("%g,%g,",gb_means[1],gb_sds[1])
                    cat(the_string,file=gb_conn)

                    for(j in 2:datasets) {
                        my_mean = gb_means[j]
                        my_sd = gb_sds[j]
                        my_yll = gb_yint_ll[j]
                        my_yul = gb_yint_ul[j]
                        my_slope = gb_slopes[j]
                        # col V,W,X,Y,Z  set2 mean sd yintll yintul slope
                        the_string = sprintf("%g,%g,%g,%g,%g,",my_mean,my_sd,my_yll,my_yul,my_slope)
                        cat(the_string,file=gb_conn)
                        # col AA,AB,AC,AD,AE,AF,  cpkll cpkul gbll gbul cpgbll cpgbul
                        c1 = colnum2char(21+1+13*(j-2))            # mean
                        c2 = colnum2char(21+2+13*(j-2))            # sd
                        c3 = colnum2char(21+3+13*(j-2))            # yint_ll
                        c4 = colnum2char(21+4+13*(j-2))            # yint_ul
                        c5 = colnum2char(21+5+13*(j-2))            # slope
                        c6 = colnum2char(21+8+13*(j-2))            # gbll
                        c7 = colnum2char(21+9+13*(j-2))            # gbul
                        if(use_OOCalc_csv) {
                            the_string = sprintf("=IF(ISNUMBER(L%d);(%s%d-L%d)/(3*%s%d);99),",n,c1,n,n,c2,n)
                            cat(the_string,file=gb_conn)
                            the_string = sprintf("=IF(ISNUMBER(M%d);(M%d-%s%d)/(3*%s%d);99),",n,n,c1,n,c2,n)
                            cat(the_string,file=gb_conn)
                            the_string = sprintf("=IF(ISNUMBER(D%d);D%d*%s%d+%s%d;\"\"),",n,n,c5,n,c3,n)
                            cat(the_string,file=gb_conn)
                            the_string = sprintf("=IF(ISNUMBER(E%d);E%d*%s%d+%s%d;\"\"),",n,n,c5,n,c4,n)
                            cat(the_string,file=gb_conn)
                            the_string = sprintf("=IF(AND(ISNUMBER(D%d);ISNUMBER(L%d));(%s%d-L%d)/(3*%s%d);\"\"),",
                                                n,n,c6,n,n,c2,n)
                            cat(the_string,file=gb_conn)
                            the_string = sprintf("=IF(AND(ISNUMBER(E%d);ISNUMBER(M%d));(M%d-%s%d)/(3*%s%d);\"\"),",
                                                n,n,n,c7,n,c2,n)
                            cat(the_string,file=gb_conn)
                            # col AG,AH,  l1gbll l1gbul
                            the_string = sprintf("=IF(ISNUMBER(L%d);(L%d-%s%d)/%s%d;99),",n,n,c3,n,c5,n)
                            cat(the_string,file=gb_conn)
                            the_string = sprintf("=IF(ISNUMBER(M%d);(M%d-%s%d)/%s%d;99),",n,n,c4,n,c5,n)
                            cat(the_string,file=gb_conn)
                        } else {
                            the_string = sprintf("\"=IF(ISNUMBER(L%d),(%s%d-L%d)/(3*%s%d),99)\",",n,c1,n,n,c2,n)
                            cat(the_string,file=gb_conn)
                            the_string = sprintf("\"=IF(ISNUMBER(M%d),(M%d-%s%d)/(3*%s%d),99)\",",n,n,c1,n,c2,n)
                            cat(the_string,file=gb_conn)
                            the_string = sprintf("\"=IF(ISNUMBER(D%d),D%d*%s%d+%s%d,\"\")\",",n,n,c5,n,c3,n)
                            cat(the_string,file=gb_conn)
                            the_string = sprintf("\"=IF(ISNUMBER(E%d),E%d*%s%d+%s%d,\"\")\",",n,n,c5,n,c4,n)
                            cat(the_string,file=gb_conn)
                            the_string = sprintf("\"=IF(AND(ISNUMBER(D%d),ISNUMBER(L%d)),(%s%d-L%d)/(3*%s%d),\"\")\",",
                                                n,n,c6,n,n,c2,n)
                            cat(the_string,file=gb_conn)
                            the_string = sprintf("\"=IF(AND(ISNUMBER(E%d),ISNUMBER(M%d)),(M%d-%s%d)/(3*%s%d),\"\")\",",
                                                n,n,n,c7,n,c2,n)
                            cat(the_string,file=gb_conn)
                            # col AG,AH,  l1gbll l1gbul
                            the_string = sprintf("\"=IF(ISNUMBER(L%d),(L%d-%s%d)/%s%d,99)\",",n,n,c3,n,c5,n)
                            cat(the_string,file=gb_conn)
                            the_string = sprintf("\"=IF(ISNUMBER(M%d),(M%d-%s%d)/%s%d,99)\",",n,n,c4,n,c5,n)
                            cat(the_string,file=gb_conn)
                        }
                    }
                    the_string = "\n"
                    cat(the_string,file=gb_conn)

                    # DAVE  REVISIT...
                }
            }
        } else {
            # empty plot... no data available
            #--------------------------------
            if (scaler<0) {
                prefix = big_prefixes[-1*scaler]
            } else if (scaler==0) {
                prefix = ""
            } else {
                prefix = lil_prefixes[scaler]
            }
            scale = 10^scaler

            if(is.finite(ll))  ll=ll*scale
            if(is.finite(ul))  ul=ul*scale
            # print out title line for this parameter
            #-----------------------------------------
            subscreen_num = split.screen(matrix(c(0.0,1.0,ytop-title_height,ytop),
                            1,4,byrow=FALSE),erase=FALSE)
            screen(subscreen_num)
            my_units = paste(prefix,units,sep="")
			# test_num is uint32, %d is only int32, so changed to %.0f
            my_title = sprintf("%.0f  %s",test_num,test_nam)
			title_width = strwidth(my_title,cex=1.0*scex)
            if (valid_alt_limits) {
				if (valid_alt2_limits) {
					if(is.finite(alt2_ll))  alt2_ll=alt2_ll*scale
					if(is.finite(alt2_ul))  alt2_ul=alt2_ul*scale
				}
                if(is.finite(alt_ll))  alt_ll=alt_ll*scale
                if(is.finite(alt_ul))  alt_ul=alt_ul*scale
				if (valid_alt2_limits) {
					my_limits1 = sprintf(" LL=%.2f ",ll)
					my_limits2 = sprintf("/ %.2f ",alt_ll)
					my_limits3 = sprintf("/ %.2f ",alt2_ll)
					my_limits4 = sprintf(" UL=%.2f ",ul)
					my_limits5 = sprintf("/ %.2f ",alt_ul)
					my_limits6 = sprintf("/ %.2f ",alt2_ul)
					my_limits7 = sprintf(" %s",my_units)
					my_limits = paste(my_limits1,my_limits2,my_limits3,my_limits4,my_limits5,my_limits6,my_limits7)
				} else {
					my_limits1 = sprintf(" LL=%.2f  UL=%.2f ",ll,ul)
					my_limits2 = sprintf(" LL2=%.2f  UL2=%.2f ",alt_ll,alt_ul)
					my_limits3 = sprintf(" %s",my_units)
					my_limits = paste(my_limits1,my_limits2,my_limits3)
				}
				# determine font size if testname+limits is too big for page width
				limits_width = strwidth(my_limits,cex=0.8*scex)
				title_cex=1.0*scex
				limits_cex=0.8*scex
				if((title_width+limits_width)>0.90) {
					adjust = 0.90/(title_width+limits_width)
					title_cex = title_cex*adjust
					limits_cex = limits_cex*adjust
				}
	            text(0.0,0.3,sprintf("%s",my_title),pos=4,cex=title_cex)
                # shrink the font a little, since string is longer
                text(1.0,0.3,sprintf("%s",my_limits),pos=2, cex=limits_cex)
                # now underline with blue and green dashed lines, (and orange if 3 sets of limits)
				if (valid_alt2_limits) {
					widths=strwidth(c(my_limits1,my_limits2,my_limits3,my_limits4,my_limits5,my_limits6,my_limits7),
							cex=limits_cex)
					xll0a = 0.98 - widths[7] - widths[6] - widths[5] - widths[4] - widths[3] - 
							widths[2] - widths[1] + 0.01
					xll0b = 0.98 - widths[7] - widths[6] - widths[5] - widths[4] - widths[3] - 
							widths[2] - 0.01
					xll1a = 0.98 - widths[7] - widths[6] - widths[5] - widths[4] - widths[3] - 
							widths[2] + 0.01
					xll1b = 0.98 - widths[7] - widths[6] - widths[5] - widths[4] - widths[3] - 0.01
					xll2a = 0.98 - widths[7] - widths[6] - widths[5] - widths[4] - widths[3] + 0.01
					xll2b = 0.98 - widths[7] - widths[6] - widths[5] - widths[4] - 0.01
					xul0a = 0.98 - widths[7] - widths[6] - widths[5] - widths[4] + 0.01
					xul0b = 0.98 - widths[7] - widths[6] - widths[5] - 0.01
					xul1a = 0.98 - widths[7] - widths[6] - widths[5] + 0.01
					xul1b = 0.98 - widths[7] - widths[6] - 0.01
					xul2a = 0.98 - widths[7] - widths[6] + 0.01
					xul2b = 0.98 - widths[7] - 0.01
					lines(c(xll0a,xll0b),c(0.1,0.1),lty="dashed",lwd=2,col="blue")
					lines(c(xll1a,xll1b),c(0.1,0.1),lty="dotdash",lwd=2,col="green")
					lines(c(xll2a,xll2b),c(0.1,0.1),lty="dashed",lwd=2,col="orange")
					lines(c(xul0a,xul0b),c(0.1,0.1),lty="dashed",lwd=2,col="blue")
					lines(c(xul1a,xul1b),c(0.1,0.1),lty="dotdash",lwd=2,col="green")
					lines(c(xul2a,xul2b),c(0.1,0.1),lty="dashed",lwd=2,col="orange")

				} else {
					widths=strwidth(c(my_limits1,my_limits2,my_limits3),cex=limits_cex)
					x0 = 0.98 - widths[3] - widths[2] - widths[1] + 0.01
					x1 = 0.98 - widths[3] - widths[2] - 0.01
					x2 = 0.98 - widths[3] - widths[2] + 0.01
					x3 = 0.98 - widths[3] - 0.01
					lines(c(x0,x1),c(0.1,0.1),lty="dashed",lwd=2,col="blue")
					lines(c(x2,x3),c(0.1,0.1),lty="dotdash",lwd=2,col="green")
				}
			} else {
				my_limits = sprintf("LL=%.2f  UL=%.2f  %s",ll,ul,my_units)
				# determine font size if testname+limits is too big for page width
				limits_width = strwidth(my_limits,cex=0.9*scex)
				title_cex=1.0*scex
				limits_cex=0.9*scex
				if((title_width+limits_width)>0.90) {
					adjust = 0.90/(title_width+limits_width)
					title_cex = title_cex*adjust
					limits_cex = limits_cex*adjust
				}
	            text(0.0,0.3,sprintf("%s",my_title),pos=4,cex=title_cex)
				text(1.0,0.3,sprintf("%s",my_limits),pos=2, cex=limits_cex)
			}
            close.screen(subscreen_num)
            ytop = ytop - title_height

            for(j in 1:datasets) {
                # stats part of plot
                #-----------------------
                subscreen_num = split.screen(matrix(c(0.0,0.5,ytop-hist_height,ytop),
                                1,4,byrow=FALSE),erase=FALSE)
                screen(subscreen_num)
                text(0.0,0.8,sprintf("Mean = NaN"),pos=4,cex=0.6*scex)
                text(0.0,0.6,sprintf("SDev = NaN"),pos=4,cex=0.6*scex)
                text(0.0,0.4,sprintf("Count = 0"),pos=4,cex=0.6*scex)
                close.screen(subscreen_num)

                # histogram part of the plot
                #----------------------------
                subscreen_num=split.screen(matrix(c(0.5,1.0,ytop-hist_height,ytop),
                                1,4,byrow=FALSE),erase=FALSE)
                screen(subscreen_num)
                par(plt=c(0.1,0.95,0.20,0.95))
                #grid()
                close.screen(subscreen_num)
                ytop = ytop - hist_height
            }
        }
    }
    if(do_csv)  close(csv_conn)
    if(do_guardbands)  close(gb_conn)

    if (to_pdf) {
        close.screen(all=TRUE)
        new_device_info = dev.off()           # close pdf file
    } else if (to_png) {
		# REVISIT
        close.screen(all=TRUE)
        new_device_info = dev.off()           # close png file
	}

    if (save_workspace_to != "") {
        if(do_xy_plots) {
            save(PlotParametersFrame,DevicesFrameList,ResultsMatrixList,
                PlotDevicesFrame,device_xrefs,param_xrefs,datasets,
                file=save_workspace_to)
        } else {
            save(PlotParametersFrame,DevicesFrameList,ResultsMatrixList,
                PlotDevicesFrame,param_xrefs,datasets,
                file=save_workspace_to)
        }
    }

    timestamp9 = proc.time()
    timestamp9 = timestamp9[3] - timestamp0
    cat(sprintf("Finished! processed %d parameters in %.2f seconds \n",
            max_params,timestamp9))



	#detach(.PlotRtdf.env) ... now using environment()

}
##############################################################################
colnum2char <- function(ints) {

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
    msd[!is.finite(msd)] = 0                #convert NA's to 0's
    lsd = ints%%26                        # least significant digit
    lsd[!is.finite(lsd)] = -1                # convert NA's to -1's

    c_lsd = rawToChar(as.raw(lsd+65))        # 0->A, 1->B,...25->Z
    c_lsd = strsplit(c_lsd,"")                # rawToChar puts into single string!
    c_lsd = c_lsd[[1]]                        # convert from list to vector
    c_lsd[c_lsd=="@"] = ""                # 0's became -1's became "@"s, make "" ie empty

    chars = c_lsd

    # if need to add additional significant digits...
    while (max(msd)>0) {
        
        ints = msd - 1
        ints[ints<0] = NA            # make zeros NA's so they don't become Z's
        msd = floor(ints/26)
        msd[!is.finite(msd)] = 0
        lsd = ints%%26                    # least significant digit
        lsd[!is.finite(lsd)] = -1   # turn NA's back to -1s now.
        c_lsd = rawToChar(as.raw(lsd+65))
        c_lsd = strsplit(c_lsd,"")
        c_lsd = c_lsd[[1]]
        c_lsd[c_lsd=="@"] = ""

        chars = paste(c_lsd,chars,sep="")
    }

    return( chars )
}


##############################################################################
PlotSuperHist <- function(num_datasets,results_list, do_guardbands, do_robust_stats,
					ytop, hist_height, dataset_name, plot_ul, plot_ll, xlim,
					cpk_ul, cpk_ll, add_normal_curve, scale,
					do_csv, csv_conn, csv_row, use_csv_formulas, use_OOCalc_csv,
					do_norm_prob_plots, scex)
{

  breaks = 50
  myylim = -1000000
  allresults = c()
  
  for(j in 1:num_datasets) {

    results = results_list[[j]]
    results2=results[(results>=plot_ll)]
    results2=results2[(results2<=plot_ul)]
              
    if (length(results2) > 0)
    {
      m = max(hist(results2, breaks=breaks, plot=FALSE)$counts)
      if(m>myylim) myylim = m
	
      allresults = append(allresults,results)
    }
  }

  allresults2 = allresults[(allresults>=plot_ll)]
  allresults2 = allresults2[(allresults2<=plot_ul)]
  
  allresults = allresults*scale
  allresults2 = allresults2*scale
  
  my_count = length(allresults)
  omitted = my_count - length(allresults2)
  
  if(my_count>0) {
      the_mean = mean(allresults)
      the_sdev = sd(allresults)
      my_min = min(allresults)
      my_max = max(allresults)
      my_q25 = quantile(allresults,0.25)
      my_q50 = median(allresults)
      my_q75 = quantile(allresults,0.75)
      r_sdev = abs(my_q75 - my_q25)/1.34898
	  r_ll_sdev = 2.0*abs(my_q50 - my_q25)/1.34898
	  r_ul_sdev = 2.0*abs(my_q75 - my_q50)/1.34898
  } else {
      the_mean = NaN
      the_sdev = NaN
      my_min = NaN
      my_max = NaN
      my_q25 = NaN
      my_q50 = NaN
      my_q75 = NaN
      r_sdev = NaN
	  r_ll_sdev = NaN
	  r_ul_sdev = NaN
  }
  
  if(do_robust_stats>1) {
  	  my_mean = my_q50
	  my_sdev = r_sdev
	  ll_sdev = r_ll_sdev
	  ul_sdev = r_ul_sdev
  } else if(do_robust_stats) {
	  my_mean = my_q50
	  my_sdev = r_sdev
	  ll_sdev = my_sdev
	  ul_sdev = my_sdev
  } else {
	  my_mean = the_mean
	  my_sdev = the_sdev
	  ll_sdev = my_sdev
	  ul_sdev = my_sdev
  }
  
  lo4sd = my_mean - (4*ll_sdev)
  lo6sd = my_mean - (6*ll_sdev)
  hi4sd = my_mean + (4*ul_sdev)
  hi6sd = my_mean + (6*ul_sdev)

  if(is.finite(cpk_ll))  cpklo = (my_mean - cpk_ll)/(3.0*ll_sdev)
  else  cpklo = 99.99

  if(is.finite(cpk_ul))  cpkhi = (cpk_ul - my_mean)/(3.0*ul_sdev)
  else  cpkhi = 99.99
      
  if(is.finite(cpklo) && (cpklo>99.99))  cpklo = 99.99
  if(is.finite(cpkhi) && (cpkhi>99.99))  cpkhi = 99.99
  
  cpk = min(c(cpklo,cpkhi))
  
  
  # stats part of plot
  #-----------------------
  subscreen_num = split.screen(matrix(c(0.0,0.45,ytop-hist_height,ytop),
                  1,4,byrow=FALSE),erase=FALSE)
  screen(subscreen_num)
  text(0.0,0.84,paste(dataset_name, collapse="  vs  "),pos=4,cex=0.6*scex)
  sh = 0.0
  if(do_robust_stats>1) {
	  text(0.0,0.70,sprintf("R_Mean = %.3f",my_mean),pos=4,cex=0.6*scex)
	  text(0.0,0.56,sprintf("R_LL_SDev = %.3f",ll_sdev),pos=4,cex=0.6*scex)
	  text(0.0,0.42,sprintf("R_UL_SDev = %.3f",ul_sdev),pos=4,cex=0.6*scex)
	  sh = 0.14
  } else if(do_robust_stats) {
      text(0.0,0.70,sprintf("R_Mean = %.3f",my_mean),pos=4,cex=0.6*scex)
      text(0.0,0.56,sprintf("R_SDev = %.3f",my_sdev),pos=4,cex=0.6*scex)
  } else {
      text(0.0,0.70,sprintf("Mean = %.3f",my_mean),pos=4,cex=0.6*scex)
      text(0.0,0.56,sprintf("SDev = %.3f",my_sdev),pos=4,cex=0.6*scex)
  }
  text(0.0,0.42-sh,sprintf("Count = %d",my_count),pos=4,cex=0.6*scex)
  if (omitted>0) {
      text(0.0,0.28-sh,sprintf("Off the plot = %d",omitted),
          pos=4,cex=0.6*scex,col="red")
  } else {
      text(0.0,0.28-sh,sprintf("Off the plot = %d",omitted),
          pos=4,cex=0.6*scex)
  }
  if (is.na(cpklo) || (cpklo<1.33)) {
      text(0.33,0.70,sprintf("Cpklo = %.2f",cpklo),
          pos=4,cex=0.6*scex,col="red")
  } else {
      text(0.33,0.70,sprintf("Cpklo = %.2f",cpklo),
          pos=4,cex=0.6*scex)
  }
  text(0.33,0.56,sprintf("Lo4sd = %.2f",lo4sd),pos=4,cex=0.6*scex)
  text(0.33,0.42,sprintf("Lo6sd = %.2f",lo6sd),pos=4,cex=0.6*scex)
  text(0.33,0.28,sprintf("Min = %.3f",my_min),pos=4,cex=0.6*scex)
  if (is.na(cpkhi) || (cpkhi<1.33)) {
      text(0.67,0.70,sprintf("Cpkhi = %.2f",cpkhi),
          pos=4,cex=0.6*scex,col="red")
  } else {
      text(0.67,0.70,sprintf("Cpkhi = %.2f",cpkhi),pos=4,cex=0.6*scex)
  }
  text(0.67,0.56,sprintf("Hi4sd = %.2f",hi4sd),pos=4,cex=0.6*scex)
  text(0.67,0.42,sprintf("Hi6sd = %.2f",hi6sd),pos=4,cex=0.6*scex)
  text(0.67,0.28,sprintf("Max = %.3f",my_max),pos=4,cex=0.6*scex)
  close.screen(subscreen_num)

  if (do_csv) {
		if(is.finite(cpk_ll))  my_llim = sprintf("%g",cpk_ll)
		else  my_llim = ""
		if(is.finite(cpk_ul))  my_ulim = sprintf("%g",cpk_ul)
		else  my_ulim = ""
		# cols F,G,H,I,
		the_string = sprintf("%s,%s,%g,%g,",
					my_llim,	my_ulim,	 the_mean,   the_sdev)
		cat(the_string,file=csv_conn)
		# cols J,K,L,M,N,O,P,
		the_string = sprintf("%d,%g,%g,%g,%g,%g,%g,",
					my_count,        my_min,                my_q25,
					my_q50,          my_q75,                my_max,
					r_sdev)
		cat(the_string,file=csv_conn)
		# cols Q,R,S,
		if(use_csv_formulas && use_OOCalc_csv) {
			n = csv_row        #datasets*(i-1)+j+1
			the_string1 = sprintf("=MIN(R%d;S%d),",n,n)
			the_string2 = sprintf("=IF(ISNUMBER(F%d);(H%d-F%d)/(3*I%d);99),",n,n,n,n)
			the_string3 = sprintf("=IF(ISNUMBER(G%d);(G%d-H%d)/(3*I%d);99),",n,n,n,n)
			the_string = sprintf("%s%s%s",the_string1,the_string2,the_string3)
		} else if(use_csv_formulas) {
			n = csv_row        #datasets*(i-1)+j+1
			the_string1 = sprintf("\"=MIN(R%d,S%d)\",",n,n)
			the_string2 = sprintf("\"=IF(ISNUMBER(F%d),(H%d-F%d)/(3*I%d),99)\",",n,n,n,n)
			the_string3 = sprintf("\"=IF(ISNUMBER(G%d),(G%d-H%d)/(3*I%d),99)\",",n,n,n,n)
			the_string = sprintf("%s%s%s",the_string1,the_string2,the_string3)
		} else {
			the_string = sprintf("%g,%g,%g,",
					cpk,                cpklo,                cpkhi)
		}
		cat(the_string,file=csv_conn)
		# cols T,U,V  (rcpk,rcpklo,rcpkhi)
		if(use_csv_formulas && use_OOCalc_csv) {
			the_string1 = sprintf("=MIN(U%d;V%d),",n,n)
			the_string2 = sprintf("=IF(ISNUMBER(F%d);(M%d-F%d)/(3*P%d);99),",n,n,n,n)
			the_string3 = sprintf("=IF(ISNUMBER(G%d);(G%d-M%d)/(3*P%d);99),",n,n,n,n)
			the_string = sprintf("%s%s%s",the_string1,the_string2,the_string3)
		} else if(use_csv_formulas) {
			the_string1 = sprintf("\"=MIN(U%d,V%d)\",",n,n)
			the_string2 = sprintf("\"=IF(ISNUMBER(F%d),(M%d-F%d)/(3*P%d),99)\",",n,n,n,n)
			the_string3 = sprintf("\"=IF(ISNUMBER(G%d),(G%d-M%d)/(3*P%d),99)\",",n,n,n,n)
			the_string = sprintf("%s%s%s",the_string1,the_string2,the_string3)
		} else {
			if(is.finite(cpk_ll))  rcpklo = (my_q50-cpk_ll)/(3*r_sdev)
			else  rcpklo = 99
			if(is.finite(cpk_ul))  rcpkhi = (cpk_ul-my_q50)/(3*r_sdev)
			else  rcpkhi = 99
			rcpk = min(c(rcpklo,rcpkhi))
			the_string = sprintf("%g,%g,%g,",
					rcpk,                rcpklo,                rcpkhi)
		}
		cat(the_string,file=csv_conn)
		# cols W,X,Y  (fails,ll_fails,ul_fails)
		if(is.finite(cpk_ll))  ll_fails = length(allresults[allresults<cpk_ll])
		else  ll_fails = 0
		if(is.finite(cpk_ul))  ul_fails = length(allresults[allresults>cpk_ul])
		else  ul_fails = 0
		fails = ll_fails + ul_fails
		the_string = sprintf("%g,%g,%g,",
					fails,                ll_fails,            ul_fails)
		cat(the_string,file=csv_conn)
		# cols Z,AA,AB,AC
		the_string = sprintf("%g,%g,%g,%g,",
					lo4sd,                lo6sd,
					hi4sd,                hi6sd)
		cat(the_string,file=csv_conn)
		# cols AD,AE,AF,AG,AH (robust_ll_sd,robust_ul_sd,r2cpk,r2cpklo,r2cpkhi)
		the_string = sprintf("%g,%g,",
					r_ll_sdev,                r_ul_sdev)
		cat(the_string,file=csv_conn)
		if(use_csv_formulas && use_OOCalc_csv) {
			the_string1 = sprintf("=MIN(AG%d;AH%d),",n,n)
			the_string2 = sprintf("=IF(ISNUMBER(F%d);(M%d-F%d)/(3*AD%d);99),",n,n,n,n)
			the_string3 = sprintf("=IF(ISNUMBER(G%d);(G%d-M%d)/(3*AE%d);99) \n",n,n,n,n)
			the_string = sprintf("%s%s%s",the_string1,the_string2,the_string3)
		} else if(use_csv_formulas) {
			the_string1 = sprintf("\"=MIN(AG%d,AH%d)\",",n,n)
			the_string2 = sprintf("\"=IF(ISNUMBER(F%d),(M%d-F%d)/(3*AD%d),99)\",",n,n,n,n)
			the_string3 = sprintf("\"=IF(ISNUMBER(G%d),(G%d-M%d)/(3*AE%d),99)\" \n",n,n,n,n)
			the_string = sprintf("%s%s%s",the_string1,the_string2,the_string3)
		} else {
			if(is.finite(llim))  rcpklo = (my_q50-llim)/(3*r_ll_sdev)
			else  rcpklo = 99
			if(is.finite(ulim))  rcpkhi = (ulim-my_q50)/(3*r_ul_sdev)
			else  rcpkhi = 99
			rcpk = min(c(rcpklo,rcpkhi))
			the_string = sprintf("%g,%g,%g \n",
					rcpk,                rcpklo,                rcpkhi)
		}
		cat(the_string,file=csv_conn)
  }

	
# originally, code for do_box
	  
  # do <plot box and >plot box
  # omitted / my_count  is all omitted, we want < and > counts...
  if (omitted>0) {
      #browser()
      off_left = length(allresults[(allresults<(scale*plot_ll))])/my_count
      off_right = length(allresults[(allresults>(scale*plot_ul))])/my_count
  } else {
      off_left = 0.0
      off_right = 0.0
  }
  # devices off left side of histogram
  #------------------------------------
  subscreen_num=split.screen(matrix(c(0.45,0.46,ytop-hist_height,ytop),1,4,byrow=FALSE),erase=FALSE)
  screen(subscreen_num)
  par(plt=c(0.0,1.0,0.20,0.95))
  plot(c(0,1),c(0,1),xlab="",ylab="",type="n",xaxt="n",yaxt="n",yaxs="i",xaxs="i")
  rect(0,0,1,off_left,col="red")
  close.screen(subscreen_num)
  
  # devices off right side of histogram
  #-------------------------------------
  subscreen_num=split.screen(matrix(c(0.98,0.99,ytop-hist_height,ytop),1,4,byrow=FALSE),erase=FALSE)
  screen(subscreen_num)
  par(plt=c(0.0,1.0,0.20,0.95))
  plot(c(0,1),c(0,1),xlab="",ylab="",type="n",xaxt="n",yaxt="n",
                      yaxs="i",xaxs="i")
  rect(0,0,1,off_right,col="red")
  close.screen(subscreen_num)
  



# do histogram plot
#------------------
  subscreen_num=split.screen(matrix(c(0.46,0.98,ytop-hist_height,ytop),
                             1,4,byrow=FALSE),erase=FALSE)
  screen(subscreen_num)
  
  par(plt=c(0.1,0.95,0.20,0.95))
  
  for(j in 1:num_datasets) 
  {
    
    results = results_list[[j]]
    results2=results[(results>=plot_ll)]
    results2=results2[(results2<=plot_ul)]
    
    
    results = results*scale
    results2 = results2*scale
    
    mycol = c("#00EEEE80","#EE762180","#BCEE6880","#EED5B7","#EEC591") #cyan2, cocolate2, dartolivegreen2, bisque2, burlywood2
    jc = j%%length(mycol) + 1
    myborder = c("#008B8B80", "#8B451380", "#6E8B3D80", "#8B7D6B", "#8B7355") #cyan4, cocolate4, dartolivegreen4, bisque4, burlywood4
    jb = j%%length(myborder) + 1   

    if ((length(results2)>0) && (plot_ul>plot_ll) ) 
    {
	  if (do_norm_prob_plots) {
		x = sort(results2)
		y = qnorm(ppoints(length(x)))

		# y axis tick locations...
		probs <- c(.01, .05, seq(.1,.9, by=.1), .95, .99)
		if(length(x)>=1000)  probs <- c(0.001, probs, .999)
		qprobs <- qnorm(probs)

		if (j==1) {
			plot(x, y, axes=FALSE, type="n", ylim=range(c(y,qprobs)),
					xlim=xlim, xlab="", ylab="",
					mgp=c(2,0.4*scex,0),cex.axis=0.6*scex,xaxs="i")
			points(x, y, col=mycol[jc], pch="+")
			# grid lines...
			box()
 			abline(h=qprobs, col="grey",lty="dotted")
			axis(1,cex.axis=0.6*scex,mgp=c(2,0.4*scex,0),xaxs="i")
			axis(2, at=qprobs, labels=100*probs,cex.axis=0.6*scex)
			grid(ny=NA)

			if (is.finite(cpk_ll))  abline(v=cpk_ll,lty="dashed",lwd=2,col="blue") 
			if (is.finite(cpk_ul))  abline(v=cpk_ul,lty="dashed",lwd=2,col="blue")
		} else {
			points(x, y, col=mycol[jc], pch="+")
		}
	  } else {
		if (max(results2) <= min(results2)) {
			if (min(results2)<plot_ll*scale)  bin_ll=min(results2)

			else  bin_ll = plot_ll*scale

			if (max(results2)>plot_ul*scale)  bin_ul=max(results2)

			else  bin_ul = plot_ul*scale

			bin_width=(bin_ul-bin_ll)/200.0

			breaks=seq(bin_ll-bin_width,bin_ul+bin_width,by=bin_width)

		}
		hist_obj = hist(results2,breaks=breaks,plot=FALSE)
        
		if (j==1) {
			plot(hist_obj,col=mycol[jc],border=myborder[jb],xlim=xlim,
					ylim=c(0,myylim),main="",xlab="",ylab="",
					bty="o",mgp=c(2,0.4*scex,0),cex.axis=0.6*scex,xaxs="i")        
			if (is.finite(cpk_ll))  abline(v=cpk_ll,lty="dashed",lwd=2,col="blue") 
			if (is.finite(cpk_ul))  abline(v=cpk_ul,lty="dashed",lwd=2,col="blue")
		} else {
			lines(hist_obj,col=mycol[jc],border=myborder[jb],xlim=xlim,
					ylim=c(0,myylim),bty="o",mgp=c(2,0.4*scex,0))        
		} 
      
      }
	}
  }



  if ( (add_normal_curve) && (length(allresults2)>0) )  {
    if (do_norm_prob_plots) {
		y_q25 = qnorm(0.25)
		y_q75 = qnorm(0.75)
		slope = (y_q75 - y_q25)/(my_q75 - my_q25)
		y_intercept = y_q25 - slope*my_q25
		if(is.finite(slope) && is.finite(y_intercept))
			abline(a=y_intercept,b=slope,col="black")									
	} else {
		norm_x0 = c(-25:25)/5
		norm_y0 = dnorm(norm_x0)

		norm_x = norm_x0*my_sdev + my_mean

		my_breaks=hist(allresults2, breaks=breaks, plot=FALSE)$breaks
		bin_width = my_breaks[2] - my_breaks[1]
		norm_y = norm_y0*my_count*bin_width/my_sdev
		lines(norm_x,norm_y,lty="dotted",lwd=2,col="black")
	}
  }


  grid()
  close.screen(subscreen_num)
}
 
# http://research.stowers-institute.org/efg/R/Color/Chart/ColorChart.pdf

#############################################################################
#  copy local functions to the .PlotRtdf.env and remove them
#  from the global environment
#############################################################################
assign("colnum2char",colnum2char,envir=.PlotRtdf.env)
rm(colnum2char)

assign("PlotSuperHist",PlotSuperHist,envir=.PlotRtdf.env)
rm(PlotSuperHist)


environment(PlotRtdf)<-.PlotRtdf.env

