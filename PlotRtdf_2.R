#  PlotRtdf_2.R
#
# $Id: PlotRtdf_2.R,v 1.7 2019/02/01 01:27:42 david Exp $
#
# script used to generate statistics, histograms, and xy plots from Rtdf files
#
# Copyright (C) 2018 David Gattrell
#                    Darren Wadden
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

PlotRtdf_2 <- function(rtdf_name="",pdf_name="",param_name="",dataset_name="",
            title="PlotRtdf_2 output",plot_types="",
            auto_scale=TRUE, save_workspace_to="",do_csv=TRUE,
            do_robust_stats=0,alt_limits="",use_alt_lims=FALSE,
            tests_per_page=1,sync_mh_bin_widths=TRUE,use_csv_formulas=TRUE,
            use_OOCalc_csv=TRUE,add_normal_curve=TRUE,plot_using_test_limits=TRUE,
            rtdf_dirs="",param_dir="",alt_lim_dir="",
			plot_widest_limits_or_values=FALSE,to_png=FALSE) {

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
    #               for each dataset, if empty, default is to use 
    #               "dataset #", with # corresponding to position 
    # title -- title to appear at the bottom of each pdf page
	# plot_types -- vector of strings describing what kind of plot and
	#               what dataset(s) to use:
	#               H/n - histogram for dataset 'n' 
	#               MH/n [m] [p] - multihist (colors B,R,G)
	#               P/n - normal probability plot for dataset 'n'
	#               MP/n [m] [p] - multiplot (colors B,R,G)
	#               XY/n m - XY plot x=dataset n, Y = dataset m
	#               MXY/n m p q - multiXY plot x=n, Y = m/p/q (colors B/R/G)
	#               - if empty, defaults to an "H/n" for length of rtdf_name
	#               For MH, also supports option /label my text which would
	#               add "my text" to left side of histogram.
	#               eg. "MH/2 1 3/label HOT"
    # auto_scale -- override scaler in stdf file, and guess at an
    #                    appropriate scale... good idea with LTX files.
    # save_workspace_to -- saves workspace to file so you can load
    #                      it into an R session for further analysis
    # do_csv -- generates pdf_name.csv file that has page num,
    #           testnum, name, ll, ul, cpks, etc... 
    # do_robust_stats -- use robust mean and robust sdev on H-type plot_types
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
    # tests_per_page -- how many tests per 8.5 x 11 page of PDF
    #                if you only have a few plots per test, you can increase
    #                this from the default value of 1 to get a more dense output
	# sync_mh_bin_widths -- if TRUE, set histogram bin widths based on the dataset
	#                with the narrowest bins.
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
	# to_png -- dump plots to individual .png files rather than to a single
	#				.pdf file
	# -----------------------------------------------------------------------

	debug1 = FALSE 

	
	# scaler prefix definitions
    #------------------------------
    big_prefixes = c("10x","100x","K","10xK","100xK","M","10xM","100xM",
                    "G","10xG","100xG","T")
    lil_prefixes = c("d","c","m","100xu","10xu","u","100xn","10xn","n",
                    "100xp","10xp","p","100xf","10xf","f")

	
	# define colors for multiple histograms
	default_MH_colors = c( rgb(0,0,1,0.5), rgb(1,0,0,0.5), rgb(0,1,0,0.5), "yellow" )


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

	# always define these
    if(add_normal_curve) {
        norm_x0 = c(-25:25)/5
        norm_y0 = dnorm(norm_x0)
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

    datasets = length(rtdf_name)    # number of Rtdf files to plot...
    if ( (dataset_name[1] == "") && (datasets>0) ) {
        # no string supplied, use filename as title...
		for (j in 1:datasets) {
        	dataset_name[j] = sprintf("dataset %d",j)
		}
    }

	len = length(plot_types)
	if( (len<1) || (nchar(plot_types[1])<3) ) {
		# if empty, default to "H/n" for length of datasets
		for (j in 1:datasets) {
			plot_types[j] = sprintf("H/%d",j)
		}
	}
	
	do_xy_plots = FALSE
	# sanity check plot_type vector of strings
	len = length(plot_types)
	if(len>0) {
		for (j in 1:len) {
			# plot_types is a vector of strings describing what to plot
			# string will have "/" separators to break into different sections, details tbd.
			# first part = plot type, 2nd part = datasets to use
			#    H/3			- histogram of dataset 3
			#    MH/2 1 3       - superimposed histograms, datasets 2, 1, and 3  (up to 3 sets)
			#    XY/1 3         - xy plot, x= dataset 1, y= dataset 3
			#    MXY/1 4 3 7	- xy plot, x= dataset 1, 3 different y's...  (RGB colors)
			#    P/4			- normal probability plot, dataset 4
			#
			# REVISIT! .. do more sanity checking, unpacking
			plot_type_sections = unlist(strsplit(plot_types[j], split="/"))
			my_datasets = as.integer(unlist(strsplit(plot_type_sections[2],split=" ")))

			#browser()

			if(substr(plot_type_sections[1],1,1) == "H") {
				if(length(my_datasets)!=1) {
					cat(sprintf("CONFUSED with plot_type: %s",plot_types[j]))
				}
			} else if(substr(plot_type_sections[1],1,1) == "X") {
				do_xy_plots = TRUE
			} else if(substr(plot_type_sections[1],1,1) == "P") {
			} else if(substr(plot_type_sections[1],1,1) == "M") {
				# multiple plots on same graph...
				if(substr(plot_type_sections[1],2,2) == "H") {
					if( (length(my_datasets)<1) || (length(my_datasets)>4) ) {
						cat(sprintf("CONFUSED with plot_type: %s",plot_types[j]))
					}
				} else if(substr(plot_type_sections[1],2,2) == "X") {
					do_xy_plots = TRUE
				} else if(substr(plot_type_sections[1],2,2) == "P") {
				}
			}
		}
	} else {
		# nasty message, shouldn't be here!
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


    # how squished do we make our plots to fit in page boundaries?
	#===================================================================
	# H and MH 
	# - take up 1 unit of height
	# XY and MXY, P and MP
	# - take up 2 unit height but 0.5 width, so can put second beside it
    #-------------------------------------------------------------------
	plots_per_param = 0
	spare_2x0p5 = FALSE
	len = length(plot_types)
	if(len>0) {
		for (j in 1:len) {
			if ( (substr(plot_types[j],1,1) == "H") ||
			     (substr(plot_types[j],1,2) == "MH") ) {
				plots_per_param = plots_per_param + 1
				spare_2x0p5 = FALSE
			} else {
				# all the rest are 2h x 0.5w
				if (spare_2x0p5) {
					spare_2x0p5 = FALSE
				} else {
					spare_2x0p5 = TRUE
					plots_per_param = plots_per_param + 2
				}
			}
		}
	}

    plots_per_page = plots_per_param*tests_per_page
	if(to_png) {
		png_page_length = 1.0/tests_per_page
		tests_per_page = 1
		plots_per_page = plots_per_param
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
                    (tests_per_page * title_height))/(1.0*plots_per_page)
    ytop = 1.0
	if(to_png) {
		ht_inches = (10.5 - 0.25) * png_page_length + 0.25
		footer_height = 0.25/ht_inches
		title_height = 0.5/ht_inches
		hist_height = (1.0 - 0.01 - footer_height -
                    (tests_per_page * title_height))/(1.0*plots_per_page)
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


	# REVISIT...
	# add title page with
	# dataset information


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
        subscreen = ( (i-1) %% tests_per_page ) + 1
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
			ix = param_xrefs[i,j]
			results_vector = ResultsMatrixList[[j]][,ix]
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

# REVISIT: ================== as far as here with PlotRtdf_2 ================
#... actually, go back to line 220 or so, sanity checking stuff

			# orig code did histos first, looped through all histograms,
			# then did xy, loop through all xy,
			# etc.
			# ... but now, we'll do in order of plot_types...
			# also, orig code did csv stats for histos as plotted,
			# we should now have separate loop for dataset stats

			dataset_means = vector()
			dataset_sdevs = vector()
			dataset_counts = vector()
			dataset_results = list()
			dataset_omitted = vector()
			dataset_hist_ranges = vector()

			# for each dataset, calculate stats and dump to csv file
			#-------------------------------------------------------
			for(j in 1:datasets) {
				# calculate the statistics...
				#----------------------------
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
				if(length(results2)>0) {
					hist_min = min(results2)
					hist_max = max(results2)
					hist_range = hist_max - hist_min
				} else {
					hist_min = NaN
					hist_max = NaN
					hist_range = NaN
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
					

				# now save dataset info so we call pull it in plot_types section as needed
				# only save the parts we'll need:
				dataset_means[j] = my_mean
				dataset_sdevs[j] = my_sdev
				dataset_counts[j] = my_count
				dataset_results[[j]] = results2	
				dataset_omitted[j] = omitted
				dataset_hist_ranges[j] = hist_range
				
				if (do_csv) {
					# print statistics to csv file...
					#--------------------------------
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
					if(valid_alt_limits) {
						if(use_alt_lims[j]) {
							llim = alt_ll
							ulim = alt_ul
						} else {
							llim = ll
							ulim = ul
						}
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
				}  # end of if(do_csv)
			}  # end of for j .. datasets


			# now loop through all the plots to do for this test/parameter
			#-------------------------------------------------------------
			spare_2x0p5 = FALSE
			for(j in 1:length(plot_types)) {
				plot_type_sections = unlist(strsplit(plot_types[j], split="/"))
				my_datasets = as.integer(unlist(strsplit(plot_type_sections[2],split=" ")))
				
				#if(substr(plot_types[j],1,1) == "H") {
				#
				#} else if(substr(plot_types[j],1,2) == "MH") {
				#}
				if( (substr(plot_types[j],1,1) == "H") ||
					(substr(plot_types[j],1,2) == "MH") ) {

					spare_2x0p5 = FALSE
					
					# split.screen() is pretty restrictive, 
					# you need to finish all your writes to a screen before moving to the next one
					#
					# so we'll need to loop through the datasets for the stats portion,
					# and loop again for the plots portion


					# stats portion of plot
					#------------------------
					stats_subscreen_num = split.screen(matrix(c(0.0,0.45,ytop-hist_height,ytop),
									1,4,byrow=FALSE),erase=FALSE)
					screen(stats_subscreen_num)

					# if there is a label defined for this plot, use it...
					idx = grep("^label ",plot_type_sections)
					if(length(idx)>0) {
						my_label = gsub("^label ","",plot_type_sections[idx[1]])
						text(0.95,0.50,my_label,pos=3,cex=0.6*scex)
					}

					# if there are colors defined for this plot, use them...
					idx = grep("^colors ",plot_type_sections)
					if(length(idx)>0) {
						colors_string = gsub("^colors ","",plot_type_sections[idx[1]])
						colors_vector = unlist(strsplit(colors_string, split=" "))
						MH_colors = colors_vector
					} else {
						MH_colors = default_MH_colors
					}

					# loop through multiple datasets that go on same subplot
					# (expect 2 or 3 datasets)
					for(k in 1:length(my_datasets)) {
						ds_j = my_datasets[k]

						# stats portion of subplot
						# 6 lines centered at:
						# 0.84
						# 0.70
						# 0.56
						# 0.42
						# 0.28
						# 0.14
						#
						my_mean = dataset_means[ds_j]
						my_omitted = dataset_omitted[ds_j]
						if(length(my_datasets)>3) {
							# need to scrunch line spacing a bit to squeeze things in
							rect(0,1.09-(k*0.22),1/20,1.15-(k*0.22),col=MH_colors[k])
							text(1/15,1.12-(k*0.22),dataset_name[ds_j],pos=4,cex=0.6*scex)
							if(do_robust_stats) {
								text(1/15,1.01-(k*0.22),sprintf("Median = %.3f",my_mean),pos=4,cex=0.6*scex)
							} else {
								text(1/15,1.01-(k*0.22),sprintf("Mean = %.3f",my_mean),pos=4,cex=0.6*scex)
							}
							if(my_omitted>0) {
								text(0.33,1.01-(k*0.22),sprintf("Off the plot = %d",my_omitted),pos=4,
																cex=0.6*scex,col="red")
							}
						} else {
							rect(0,1.08-(k*0.28),1/20,1.16-(k*0.28),col=MH_colors[k])
							text(1/15,1.12-(k*0.28),dataset_name[ds_j],pos=4,cex=0.6*scex)
							if(do_robust_stats) {
								text(1/15,0.98-(k*0.28),sprintf("Median = %.3f",my_mean),pos=4,cex=0.6*scex)
							} else {
								text(1/15,0.98-(k*0.28),sprintf("Mean = %.3f",my_mean),pos=4,cex=0.6*scex)
							}
							if(my_omitted>0) {
								text(0.33,0.98-(k*0.28),sprintf("Off the plot = %d",my_omitted),pos=4,
																cex=0.6*scex,col="red")
							}
						}
					}
					close.screen(stats_subscreen_num)


					# histogram portion of plot
					#---------------------------
					histo_subscreen_num = split.screen(matrix(c(0.46,0.98,ytop-hist_height,ytop),
									1,4,byrow=FALSE),erase=FALSE)

					screen(histo_subscreen_num)

					# go through datasets, determine smallest histogram range,
					# use that for all dataset histogram binwidths, if this flag is set
					if(sync_mh_bin_widths) {
						valid_range_found = FALSE
						min_hist_range = NaN
						for(k in 1:length(my_datasets)) {
							ds_j = my_datasets[k]
							
							my_range = dataset_hist_ranges[ds_j]

							if( is.finite(my_range) && (my_range>0) ) {
								if(valid_range_found) {
									if(min_hist_range > my_range) {
										min_hist_range = my_range
									}
								} else {
									valid_range_found = TRUE
									min_hist_range = my_range
								}
							}
						}
					}

					# loop through multiple datasets that go on same subplot
					# (expect 2 or 3 datasets)
					for(k in 1:length(my_datasets)) {
						ds_j = my_datasets[k]

						my_mean = dataset_means[ds_j]
						my_sdev = dataset_sdevs[ds_j]
						my_count = dataset_counts[ds_j]
						my_omitted = dataset_omitted[ds_j]
						results2 = dataset_results[[ds_j]]
						my_hist_range = dataset_hist_ranges[ds_j]
						
						breaks=25
						if (length(results2)>200)  breaks=50
						if (length(results2)>0) {
							if (my_ul<=my_ll) {
								par(plt=c(0.1,0.95,0.20,0.95))
							} else {
								if (max(results2) <= min(results2)) {
									# if bad data or single value, force 200 bins across plot
									if (min(results2)<my_ll*scale)  bin_ll=min(results2)
									else  bin_ll = my_ll*scale
									if (max(results2)>my_ul*scale)  bin_ul=max(results2)
									else  bin_ul = my_ul*scale
									bin_width=(bin_ul-bin_ll)/200.0
									breaks=seq(bin_ll-bin_width,bin_ul+bin_width,by=bin_width)
									# browser()
								} else if(sync_mh_bin_widths && is.finite(min_hist_range)) {
									breaks = breaks * my_hist_range / min_hist_range
								}
								hist_obj = hist(results2,breaks=breaks,plot=FALSE)
								par(plt=c(0.1,0.95,0.20,0.95))
								if(k==1) {
									plot(hist_obj,col=MH_colors[k],border=MH_colors[k],
										xlim=xlim,main="",xlab="",ylab="",
									    bty="o",mgp=c(2,0.4*scex,0),cex.axis=0.6*scex,xaxs="i")
								} else {
									plot(hist_obj,col=MH_colors[k],border=MH_colors[k],
										xlim=xlim,main="",xlab="",ylab="",
									   	bty="o",mgp=c(2,0.4*scex,0),cex.axis=0.6*scex,xaxs="i",add=TRUE)
								}
								if (valid_alt_limits) {
									if(use_alt_lims[j]) {
										if (is.finite(alt_ll))  abline(v=alt_ll,lty="dotdash",
																	lwd=2,col="green") 
										if (is.finite(alt_ul))  abline(v=alt_ul,lty="dotdash",
																	lwd=2,col="green")
								   } else {
										if (is.finite(ll))  abline(v=ll,lty="dashed",
																	lwd=2,col="blue") 
										if (is.finite(ul))  abline(v=ul,lty="dashed",
																	lwd=2,col="blue")
									}
								} else {
									if (is.finite(ll))  abline(v=ll,lty="dashed",lwd=2,col="blue") 
									if (is.finite(ul))  abline(v=ul,lty="dashed",lwd=2,col="blue")
								}
								if (add_normal_curve)  {
									norm_x = norm_x0*my_sdev + my_mean
									my_breaks=hist_obj$breaks
									bin_width = my_breaks[2] - my_breaks[1]
									norm_y = norm_y0*my_count*bin_width/my_sdev
									lines(norm_x,norm_y,lty="dotted",lwd=2,col="black")
								}
							}
						}
					} # end of for(k datasets
					grid()
					close.screen(histo_subscreen_num)
					ytop = ytop - hist_height
				} else if(substr(plot_types[j],1,2) == "XY") {
					# line 1343 PlotRtdf.R
					x_j = my_datasets[1]
					y_j = my_datasets[2]
                	x_vector = scale*xy_results_list[[x_j]]
                	y_vector = scale*xy_results_list[[y_j]]
                    valid_xys = is.finite(x_vector) & is.finite(y_vector)

					if(debug1) cat(sprintf("x_j is %d, y_j is %d, points %d\n",
						x_j,y_j,length(y_vector[valid_xys])))

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

					if(spare_2x0p5) {
						# use spare right half of last plot for this plot
						x0 = 0.5
						ytop = ytop + (2*hist_height)
						spare_2x0p5 = FALSE
					} else {
						x0 = 0
						spare_2x0p5 = TRUE
					}


					# text portion of XY plot
					#-------------------------
                    subscreen_num = split.screen(matrix(c(x0,x0+0.47,ytop-(2*hist_height),ytop),
                                    1,4,byrow=FALSE),erase=FALSE)
                    screen(subscreen_num)
					if(debug1) cat("starting text portion...\n")

                    text(0.0,0.90,sprintf("%s vs %s",dataset_name[y_j],dataset_name[x_j]),pos=4,cex=0.7*scex)
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



					# graph portion of XY plot
					#--------------------------
                    subscreen_num = split.screen(matrix(c(x0+0.17,x0+0.5,ytop-(2*hist_height),ytop),
                                    1,4,byrow=FALSE),erase=FALSE)
                    screen(subscreen_num)
					if(debug1) cat("starting graph portion...\n")

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


				} else if(substr(plot_types[j],1,3) == "MXY") {
				} else if(substr(plot_types[j],1,1) == "P") {
				} else if(substr(plot_types[j],1,2) == "MP") {
				} else {
					# shouldn't get here!
				}
			}
			

        }  # if data range finite
    }
    if(do_csv)  close(csv_conn)

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

}
 
