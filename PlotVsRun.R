#  PlotVsRun.R
#
# $Id: PlotVsRun.R,v 1.3 2019/02/01 01:45:35 david Exp $
#
# script used plot data with x-axis being device run from Rtdf files
#
# Copyright (C) 2012,2014 David Gattrell
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

PlotVsRun <- function(rtdf_name="",pdf_name="",param_name="",dataset_name="",
            title="PlotVsTime output",auto_scale=TRUE,
            do_robust_stats=0,alt_limits="",use_alt_lims=FALSE,
            min_plots_per_page=4,plot_using_limits=3.2,
            collapse_MPRs=FALSE,rtdf_dirs="",param_dir="",alt_lim_dir="",
			do_landscape=TRUE,to_png=FALSE) {

    # rtdf_name -- vector of strings for the filenames containing
    #              rtdf formatted data (.Rdata files)
    # pdf_name -- string for filename of pdf file to output to
    # param_name -- name of file containing rtdf formatted 
    #               ParametersFrame containing the list of parameters
    #               and limits to use for plotting... if empty, this
    #               program will use the first rtdf_name's
    #               ParametersFrame
    # dataset_name -- same # of strings as rtdf_name... contains
    #               strings that will be printed per chart
    #               for each dataset, if empty, default is to use the
    #               names of the rtdf files
    # title -- title to appear at the bottom of each pdf page
    # auto_scale -- override scaler in stdf file, and guess at an
    #                    appropriate scale... good idea with LTX files.
    # do_robust_stats -- use robust mean and robust sdev on plots
    #			0 / FALSE -> use mean, sdev on plots
    #           1 / TRUE -> use robust mean, robust sdev on plots
    #           		robust mean = median = Quartile 0.50
    #           		robust sdev = (Quartile 0.75 - Quartile 0.25)/1.34898
    #           2 -> use 2 sided robust sdev, robust mean on plots
    #                   robust ll_sdev = 2*(Quartile 0.50 - Quartile 0.25)/1.34898
    #                   robust ul_sdev = 2*(Quartile 0.75 - Quartile 0.50)/1.34898
    # alt_limits -- a string containing the filename of an rtdf format
    #               file containing at least the ParametersFrame.  The
    #               file will be used for alternate limits rather than
    #               what is found in param_name, if the use_alt_lims
    #               flag is set to TRUE for that dataset.
    # use_alt_lims -- a vector of boolean the same length as the rtdf_name
    #                 vector.  If TRUE, for these data sets use the alternate
    #                 set of limits rather than those in the param_name file
    # min_plots_per_page -- program tends to put one parameter per page.
    #                if there is only one dataset, you get one plot per
    #                page.  If this is set to 4, you would get 4 parameters
    #                per page if single dataset, 2 parameters per page if
    #                2 datasets.
    # plot_using_limits -- controls Y axis min/max decision...
    #                0.x = use population min/max +x
	#                1.x = use limits +x
	#                2.x = if pop min/max is tighter than limit, use pop
	#                      else use limit +x
	#                3.x = if pop min/max is tighter than limit, use limit
	#                      else use limit +x
	#                NOTE: plot_ul/plot_ll will override if present
	#                if want to extend plot y range 10% add 0.1, 20%.. add 0.2, etc.
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
	# do_landscape -- if TRUE, page is landscape mode instead of portrait mode
	#                ie. pdf page 8.5 wide x 11 tall becomes 11 wide by 8.5 tall
	# to_png -- dump plots to individual .png files rather than to a single
	#				.pdf file
	#
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
	

    # if filenames not defined, prompt for them
    #------------------------------------------
    if (length(rtdf_name)<2 && (rtdf_name == "")) {
        rtdf_name <- readline("Enter the name of the RTDF file to read: ")
    }

    if (pdf_name == "") {
        pdf_name <- readline("Enter the name of the PDF file to write: ")
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

    len = length(rtdf_dirs)
    if(len<datasets)  rtdf_dirs[(len+1):datasets]=""
    for (j in 1:datasets) {
		if (rtdf_dirs[j] != "") {
			#cat(sprintf("j is %d, dir ==%s==\n",j,rtdf_dirs[j]))
			my_dir = getwd()
			setwd(rtdf_dirs[j])
		}
        # REVISIT: could clear 4 objects first, load, then check objects exist
        my_objs = load(rtdf_name[j])    # assume Rtdf: ParametersFrame, 
                                    	# ResultsMatrix, ...
		if (rtdf_dirs[j] != "")  setwd(my_dir)

		# create vector of passing devices
		# - first determine pass bins...
 		all_sbins = as.integer(DevicesFrame[,"soft_bin"])
		all_hbins = as.integer(DevicesFrame[,"hard_bin"])

		valid_sbin_types = FALSE
		if(is.finite(match("SbinInfoFrame",my_objs))) {
			sbin_types = as.character(SbinInfoFrame[["sbin_pf"]])
			if(is.finite(match("P",sbin_types)) || 
					is.finite(match("F",sbin_types))) {
				valid_sbin_types = TRUE
				valid_pass_sbins = as.integer(SbinInfoFrame["sbin_num"]
										[SbinInfoFrame["sbin_pf"]=="P"])
			}
		}
		valid_hbin_types = FALSE
		if(is.finite(match("HbinInfoFrame",my_objs))) {
			hbin_types = as.character(HbinInfoFrame[["hbin_pf"]])
			if(is.finite(match("P",hbin_types)) ||
					is.finite(match("F",hbin_types))) {
				valid_hbin_types = TRUE
				valid_pass_hbins = as.integer(HbinInfoFrame["hbin_num"]
										[HbinInfoFrame["hbin_pf"]=="P"])
			}
		} 
		if(valid_sbin_types) {
			passing_parts = all_sbins %in% valid_pass_sbins
		} else if (valid_hbin_types) {
			passing_parts = all_hbins %in% valid_pass_hbins
		} else {	# assume hbin=1 is pass bin
			passing_parts = (all_hbins==1)
		}

        if(j==1) {
            DevicesFrameList=list(DevicesFrame)
            ResultsMatrixList=list(ResultsMatrix)
			passing_parts_list=list(passing_parts)

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
			passing_parts_list[j]=list(passing_parts)

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
    #-------------------------------------------------------
    if(alt_limits != "") {
		if (alt_lim_dir != "") {
			my_dir = getwd()
			setwd(alt_lim_dir)
		}
        load(alt_limits)
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
        if(len<datasets)  use_alt_lims[(len+1):datasets]=FALSE
            
    } else {
        valid_alt_limits = FALSE
    }


    # how many parameters can we plot on a page?
    #-------------------------------------------
    plots_per_param = datasets
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
    }
    
    if (to_pdf) {
		if(do_landscape) {
			pdf(file=pdf_name,paper="USr",width=10.5,height=8,family="Courier")
		} else {
			pdf(file=pdf_name,paper="letter",width=8,height=10.5)
		}
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
    if(do_landscape) {
		footer_height = 0.25/8.0
		title_height = 0.5/8.0
		stats_width = 0.20
	} else {
		footer_height = 0.25/10.5
		title_height = 0.5/10.5
		stats_width = 0.25
    }
    chart_height = (1.0 - 0.01 - footer_height -
                    (params_per_page * title_height))/(1.0*plots_per_page)
    ytop = 1.0
    
	if(to_png) {
    	if(do_landscape) {
			ht_inches = (8.0 - 0.25) * png_page_length + 0.25
		} else {
			ht_inches = (10.5 - 0.25) * png_page_length + 0.25
		}
		footer_height = 0.25/ht_inches
		title_height = 0.5/ht_inches
		chart_height = (1.0 - 0.01 - footer_height -
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
	if((collapse_MPRs) || (max_params>2000)) {
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
					Parameters_start_index[i2] = i
					Parameters_index_count[i2] = 1
				}
			}
		}

		# if param count is >2K, ask if should continue or quit?
		if(i2>2000) {
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
			if (valid_alt_limits) {
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
        if (valid_alt_limits) {
            alt_ll = PlotParametersFrame[[i,"alt_ll"]]
            alt_ul = PlotParametersFrame[[i,"alt_ul"]]
        } else {
            alt_ll = NaN
            alt_ul = NaN
        }
        if(is.finite(ll) && is.finite(alt_ll)) {
            if(ll<alt_ll)  llim=ll
            else  llim=alt_ll
        } else if(is.finite(ll)) {
            llim = ll
        } else {
            llim = alt_ll
        }
        if(is.finite(ul) && is.finite(alt_ul)) {
            if(ul>alt_ul)  ulim=ul
            else  ulim=alt_ul
        } else if(is.finite(ul)) {
            ulim = ul
        } else {
            ulim = alt_ul
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
            results2_vector = results_vector[is.finite(results_vector)]
            if(j==1) {
                results_list = list(results_vector)
                results2_list = list(results2_vector)
            } else {
                results_list[j] = list(results_vector)
                results2_list[j] = list(results2_vector)
            }
        }


        # determine plot limits...
        # if plot_ll/ul defined, use that value,
        # else if plot_using_limits=...
        #-----------------------------------------------------
		trunc_plot_using_limits = trunc(plot_using_limits)
		lim_pct = plot_using_limits - trunc_plot_using_limits	# get fractional part
		ll_pct = TRUE
		ul_pct = TRUE
		if(is.finite(plot_ll)) {
            my_ll = plot_ll
			ll_pct = FALSE
        } else if((trunc_plot_using_limits==1) && is.finite(llim)) {
            my_ll = llim
        } else {
            pop_min = NaN
            for(j in 1:datasets) {
				res_vect = results_list[[j]] 
                if(length(res_vect)>0) {
					res_vect = res_vect[is.finite(res_vect)]
					if(length(res_vect)>0) {
						local_min = min(res_vect)
                    	if(is.na(pop_min))  pop_min = local_min
                    	else if(is.finite(local_min) && (local_min<pop_min))  pop_min=local_min
					}
                }
            }
			if(trunc_plot_using_limits<=1) {
				my_ll = pop_min
			} else if(is.finite(llim)) {
				if(is.finite(pop_min) && (pop_min>llim)) {
					if(trunc_plot_using_limits==2) {
						my_ll = pop_min
						ll_pct = FALSE
					} else {
						my_ll = llim
						ll_pct = FALSE
					}
				} else {
					my_ll = llim
				}
			} else {
				my_ll = pop_min
				ll_pct = FALSE
			}
        }
        if(is.finite(plot_ul)) {
            my_ul = plot_ul
			ul_pct = FALSE
        } else if((trunc_plot_using_limits==1) && is.finite(ulim)) {
            my_ul = ulim
        } else {
            pop_max = NaN
            for(j in 1:datasets) {
				res_vect = results_list[[j]] 
                if(length(res_vect)>0) {
					res_vect = res_vect[is.finite(res_vect)]
					if(length(res_vect)>0) {
						local_max = max(res_vect)
                    	if(is.na(pop_max))  pop_max = local_max
                    	else if(is.finite(local_max) && (local_max>pop_max))  pop_max=local_max
					}
                }
            }
			if(trunc_plot_using_limits<=1) {
				my_ul = pop_max
			} else if(is.finite(ulim)) {
				if(is.finite(pop_max) && (pop_max<ulim)) {
					if(trunc_plot_using_limits==2) {
						my_ul = pop_max
						ul_pct = FALSE
					} else {
						my_ul = ulim
						ul_pct = FALSE
					}
				} else {
					my_ul = ulim
				}
			} else {
				my_ul = pop_max
				ul_pct = FALSE
			}
        }
        

        if(is.finite(my_ll) && is.finite(my_ul)) {
        	my_pct = lim_pct*(my_ul - my_ll)
        	if(ll_pct) my_ll = my_ll - my_pct
        	if(ul_pct) my_ul = my_ul + my_pct
        	
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

            if(is.finite(my_ll))  my_ll=my_ll*scale
            if(is.finite(my_ul))  my_ul=my_ul*scale
            if(is.finite(ll))  ll=ll*scale
            if(is.finite(ul))  ul=ul*scale


            # print out title line for this parameter
            #-----------------------------------------
            subscreen_num = split.screen(matrix(c(0.0,1.0,ytop-title_height,ytop),
                            1,4,byrow=FALSE),erase=FALSE)
            screen(subscreen_num)
            my_units = paste(prefix,sep="",units)
            my_title = sprintf("%.0f  %s",test_num,test_nam)  # change tnum from %d to %.0f for uint32
			title_width = strwidth(my_title,cex=1.0*scex)
            if (valid_alt_limits) {
                if(is.finite(alt_ll))  alt_ll=alt_ll*scale
                if(is.finite(alt_ul))  alt_ul=alt_ul*scale
                my_limits1 = sprintf(" LL=%.2f  UL=%.2f ",ll,ul)
                my_limits2 = sprintf(" LL2=%.2f  UL2=%.2f ",alt_ll,alt_ul)
                my_limits3 = sprintf(" %s",my_units)
                my_limits = paste(my_limits1,my_limits2,my_limits3)
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
                # now underline with blue and green dashed lines
                widths=strwidth(c(my_limits1,my_limits2,my_limits3),cex=limits_cex)
                x0 = 0.98 - widths[3] - widths[2] - widths[1] + 0.01
                x1 = 0.98 - widths[3] - widths[2] - 0.01
                x2 = 0.98 - widths[3] - widths[2] + 0.01
                x3 = 0.98 - widths[3] - 0.01
                lines(c(x0,x1),c(0.1,0.1),lty="dashed",lwd=2,col="blue")
                lines(c(x2,x3),c(0.1,0.1),lty="dotdash",lwd=2,col="green")
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
			for(j in 1:datasets) {
				raw_results = results_list[[j]]
				raw_results = raw_results*scale
				results_count = length(raw_results)		# num of devices

				results = results2_list[[j]]
				results = results*scale
				my_count = length(results)			# num of finite results
				results2=results[(results>=my_ll)]
				results2=results2[(results2<=my_ul)]

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
				if(valid_alt_limits) {
					if(use_alt_lims[j]) {
						if(is.finite(alt_ll))  cpklo = (my_mean - alt_ll)/(3.0*ll_sdev)
						else  cpklo = 99.99
					} else {
						if(is.finite(ll))  cpklo = (my_mean - ll)/(3.0*ll_sdev)
						else  cpklo = 99.99
					}
				} else {
					if(is.finite(ll))  cpklo = (my_mean - ll)/(3.0*ll_sdev)
					else  cpklo = 99.99
				}
				if(is.finite(cpklo) && (cpklo>99.99))  cpklo = 99.99
				if(valid_alt_limits) {
					if(use_alt_lims[j]) {
						if(is.finite(alt_ul))  cpkhi = (alt_ul - my_mean)/(3.0*ul_sdev)
						else  cpkhi = 99.99
					} else {
						if(is.finite(ul))  cpkhi = (ul - my_mean)/(3.0*ul_sdev)
						else  cpkhi = 99.99
					}
				} else {
					if(is.finite(ul))  cpkhi = (ul - my_mean)/(3.0*ul_sdev)
					else  cpkhi = 99.99
				}
				if(is.finite(cpkhi) && (cpkhi>99.99))  cpkhi = 99.99
				cpk = min(c(cpklo,cpkhi))


				# stats part of plot
				#-----------------------
				subscreen_num = split.screen(matrix(c(0.0,stats_width,ytop-chart_height,ytop),
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
				if (omitted>0) {
					text(0.0,0.28-sh,sprintf("Off the plot = %d",omitted),
						pos=4,cex=0.6*scex,col="red")
				} else {
					text(0.0,0.28-sh,sprintf("Off the plot = %d",omitted),
						pos=4,cex=0.6*scex)
				}
				close.screen(subscreen_num)


				# chart part of the plot
				#-----------------------
				subscreen_num=split.screen(matrix(c(stats_width+0.01,0.98,ytop-chart_height,ytop),
								1,4,byrow=FALSE),erase=FALSE)
				screen(subscreen_num)

				par(plt=c(0.02,0.98,0.20,0.95))
				chart_min = my_ll
				chart_max = my_ul

				my_colors = rep(1,times=results_count)

				# parts that eventually fail will be grey
				bad_parts = !passing_parts_list[[j]]
				my_colors[bad_parts] = 5

				# parts above upper limit will be red circle
				if(is.finite(ul)) {
					bad_parts = which(raw_results>ul)
					my_colors[bad_parts] = 4
				}

				# parts off the top of the chart will be red triangle
				bad_parts = which(raw_results>chart_max)
				my_colors[bad_parts] = 2
				raw_results[bad_parts] = chart_max

				# parts below lower limit will be red circle
				if(is.finite(ll)) {
					bad_parts = which(raw_results<ll)
					my_colors[bad_parts] = 4
				}

				# parts off the bottom of the chart will be red triangle
				bad_parts = which(raw_results<chart_min)
				my_colors[bad_parts] = 3
				raw_results[bad_parts] = chart_min
				
				x_range = c(1,results_count)
				y_range = c(chart_min,chart_max)

				plot(x_range,y_range,type="n",main="",xlab="",ylab="",
						bty="o",mgp=c(2,0.4,0),cex.axis=0.6,xaxs="i")
				abline(h=my_mean,lty="dashed",col="grey")

				if (is.finite(ll))  abline(h=ll,lty="dashed",lwd=2,col="green")
				if (is.finite(ul))  abline(h=ul,lty="dashed",lwd=2,col="green")
				lines(raw_results)

				indices = which(my_colors==3)
				points(indices,raw_results[indices],cex=0.6,pch=6,col="red",
							main="",xlab="",ylab="")
				indices = which(my_colors==2)
				points(indices,raw_results[indices],cex=0.6,pch=2,col="red",
							main="",xlab="",ylab="")
				indices = which(my_colors==4)
				points(indices,raw_results[indices],cex=0.6,col="red",
							main="",xlab="",ylab="")
				indices = which(my_colors==5)
				points(indices,raw_results[indices],cex=0.6,col="grey",
							main="",xlab="",ylab="")
				indices = which(my_colors==1)
				points(indices,raw_results[indices],cex=0.6,col="blue",
							main="",xlab="",ylab="")


				close.screen(subscreen_num)
				ytop = ytop - chart_height
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
            my_title = sprintf("%.0f  %s",test_num,test_nam)  # changed %d to %.0f for uint32
			title_width = strwidth(my_title,cex=1.0*scex)
            if (valid_alt_limits) {
                if(is.finite(alt_ll))  alt_ll=alt_ll*scale
                if(is.finite(alt_ul))  alt_ul=alt_ul*scale
                my_limits1 = sprintf(" LL=%.2f  UL=%.2f ",ll,ul)
                my_limits2 = sprintf(" LL2=%.2f  UL2=%.2f ",alt_ll,alt_ul)
                my_limits3 = sprintf(" %s",my_units)
                my_limits = paste(my_limits1,my_limits2,my_limits3)
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
                # now underline with blue and green dashed lines
                widths=strwidth(c(my_limits1,my_limits2,my_limits3),cex=limits_cex)
                x0 = 0.98 - widths[3] - widths[2] - widths[1] + 0.01
                x1 = 0.98 - widths[3] - widths[2] - 0.01
                x2 = 0.98 - widths[3] - widths[2] + 0.01
                x3 = 0.98 - widths[3] - 0.01
                lines(c(x0,x1),c(0.1,0.1),lty="dashed",lwd=2,col="blue")
                lines(c(x2,x3),c(0.1,0.1),lty="dotdash",lwd=2,col="green")
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
                subscreen_num = split.screen(matrix(c(0.0,0.5,ytop-chart_height,ytop),
                                1,4,byrow=FALSE),erase=FALSE)
                screen(subscreen_num)
                text(0.0,0.8,sprintf("Mean = NaN"),pos=4,cex=0.6*scex)
                text(0.0,0.6,sprintf("SDev = NaN"),pos=4,cex=0.6*scex)
                text(0.0,0.4,sprintf("Count = 0"),pos=4,cex=0.6*scex)
                close.screen(subscreen_num)

                # chart part of the plot
                #----------------------------
                subscreen_num=split.screen(matrix(c(0.5,1.0,ytop-chart_height,ytop),
                                1,4,byrow=FALSE),erase=FALSE)
                screen(subscreen_num)
                par(plt=c(0.02,0.98,0.20,0.95))
                #grid()
                close.screen(subscreen_num)
                ytop = ytop - chart_height
            }
        }
    }

    if (to_pdf) {
        close.screen(all=TRUE)
        new_device_info = dev.off()           # close pdf file
    } else if (to_png) {
		# REVISIT
        close.screen(all=TRUE)
        new_device_info = dev.off()           # close png file
	}

    timestamp9 = proc.time()
    timestamp9 = timestamp9[3] - timestamp0
    cat(sprintf("Finished! processed %d parameters in %.2f seconds \n",
            max_params,timestamp9))

}

