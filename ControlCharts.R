#  ControlCharts.R
#
# $Id: ControlCharts.R,v 1.4 2015/04/18 01:33:55 david Exp $
#
# script used to generate control charts on a test by test basis from Rtdf files
#
# Copyright (C) 2009,2014 David Gattrell
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

ControlCharts <- function(rtdf_name="",pdf_name="",param_name="",
			title="ControlCharts.R output",do_western_electric=FALSE,
			start_n=1,count_n=50,charts_per_page=7,do_landscape=FALSE,
			rtdf_dir="",param_dir="") {

    # rtdf_name -- the name of the rtdf file to process
    # pdf_name -- string for filename of pdf file to output to
    # param_name -- name of file containing rtdf formatted 
    #               ParametersFrame containing the list of parameters
    #               and limits to use for plotting... if empty, this
    #               program will use the first rtdf_name's
    #               ParametersFrame
    # title -- title to appear at the bottom of each pdf page
    # do_western_electric -- if TRUE, add statistic for "WER fails",
	#               apply the Western Electric Rules to the control
	#               chart data, and mark these outliers with orange on chart.
	# start_n,count_n -- to determine the mean and standard deviation
	#               to use for all the devices, use the results for
	#               the start_n to start_n + count_n -1 devices.
	# charts_per_page -- how many tests/control charts to fit on a page
	# do_landscape -- if TRUE, page is landscape mode instead of portrait mode
	#               ie. pdf page 8.5 wide x 11 tall becomes 11 wide by 8.5 tall
	# rtdf_dir -- contains absolute directory path for rtdf file.  I
	#				 empty, use current directory.
	# param_dir -- contains absolute directory path for param_name rtdf 
	#                file if different than execution directory
	# -----------------------------------------------------------------------


	# scaler prefix definitions
    #------------------------------
    big_prefixes = c("10x","100x","K","10xK","100xK","M","10xM","100xM",
                    "G","10xG","100xG","T")
    lil_prefixes = c("d","c","m","100xu","10xu","u","100xn","10xn","n",
                    "100xp","10xp","p","100xf","10xf","f")
	auto_scale = TRUE


    # timestamp...
    #-----------------
    timestamp0 = proc.time()
    timestamp0 = timestamp0[3]
    timestamp1 = timestamp0
    now = substr(as.character(Sys.time()),1,19)


    # default values for options...
    #--------------------------------
    to_pdf = TRUE 


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


	if (rtdf_dir != "") {
		my_dir = getwd()
		setwd(rtdf_dir)
	}
    # REVISIT: could clear 4 objects first, load, then check objects exist
    load(rtdf_name)          # assume Rtdf: ParametersFrame, 
                                # ResultsMatrix, ...
	if (rtdf_dir != "")  setwd(my_dir)


    if(build_params_frame)  PlotParametersFrame = ParametersFrame
	max_params = dim(PlotParametersFrame)[1]
	param_xrefs = rep(NaN,times=max_params)
    if(build_params_frame) {
		param_xrefs=c(1:max_params)
	} else {
        for (i in 1:max_params) {
            test_nam = PlotParametersFrame[i,"testname"]
            index = match(test_nam,ParametersFrame[["testname"]],
                            nomatch=NaN)
            param_xrefs[i]=index
        }
    }


    if (to_pdf) {
		if(do_landscape) {
			pdf(file=pdf_name,paper="USr",width=10.5,height=8,family="Courier")
		} else {
			pdf(file=pdf_name,paper="letter",width=8,height=10.5)
		}
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
		stats_width = 0.12
	} else {
		footer_height = 0.25/10.5
		title_height = 0.5/10.5
		stats_width = 0.15
    }
	chart_height = (1.0 - 0.01 - footer_height - (charts_per_page * title_height))/
							(1.0*charts_per_page)
    ytop = 1.0


    # print out footer for first page
    #---------------------------------
    page_count = 1
    ytop=1.0        # new page, move ptr to top of page
    if (to_pdf) {
        footer = 1
        subscreen_num = split.screen(matrix(c(0.0,1.0,0.01,footer_height),1,4,byrow=FALSE))
        screen(subscreen_num)
        text(0.0,0.5,sprintf("%s  %s",title,now),pos=4,cex=0.7)
        text(1.0,0.5,sprintf("Page %d",page_count),pos=2,cex=0.7)
        close.screen(subscreen_num)
    } else {
        footer = 0
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
        subscreen = ( (i-1) %% charts_per_page ) + 1
        if( (i>1) && (subscreen==1)) {
            ytop=1.0        # new page, move ptr to top of page
            if (to_pdf) {
                page_count = page_count + 1
                subscreen_num = split.screen(matrix(c(0.0,1.0,0.01,footer_height),1,4,byrow=FALSE))
                screen(subscreen_num)
                text(0.0,0.5,sprintf("%s   %s",title,now),pos=4,cex=0.7)
                text(1.0,0.5,sprintf("Page %d",page_count),pos=2,cex=0.7)
                close.screen(subscreen_num)
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
        ll = PlotParametersFrame[[i,"ll"]]
        ul = PlotParametersFrame[[i,"ul"]]

		ix = param_xrefs[i]
		results_vector = ResultsMatrix[,ix]
		results_count = length(results_vector)
			
		xrefs = which(is.finite(results_vector))
		samples = results_vector[xrefs]
		my_parts = length(samples)

		
		# auto scaling
		#--------------
		if(my_parts>0) {
			if (auto_scale) {
				range = max(abs(samples))
				if(is.finite(range) && (range>0.0)) {
					if(range>0.99e9)		scaler=-9
					else if(range>0.99e6)	scaler=-6
					else if(range>0.99e3)	scaler=-3
					else if(range>0.99)		scaler=0
					else if(range>0.99e-3)  scaler=3
					else if(range>0.99e-6)  scaler=6
					else if(range>0.99e-9)  scaler=9
					else   scaler=12
				}
			}
			if (scaler<0) {
				prefix = big_prefixes[-1*scaler]
			} else if (scaler==0) {
				prefix = ""
			} else {
				prefix = lil_prefixes[scaler]
			}
			my_units = paste(prefix,sep="",units)

			scale = 10^scaler
			if(is.finite(ll))  ll=ll*scale
			if(is.finite(ul))  ul=ul*scale
			results_vector = results_vector*scale
			samples = samples*scale
			

			# to determine plot y axis, need to get mean and sd first
			#---------------------------------------------------------
			stop_n = start_n + count_n - 1
			if (stop_n>results_count) {
				stop_n = results_count
			}
			if (start_n>results_count) {
				start_n = 1
			}
			# my_mean = mean(results_vector[start_n:stop_n])
			# my_sd =     sd(results_vector[start_n:stop_n])
			# USE ROBUST STATISTICS!
			stat_parts = results_vector[start_n:stop_n]
			stat_parts = stat_parts[which(is.finite(stat_parts))]
			my_mean = median(stat_parts)
			my_q25 = quantile(stat_parts,0.25)
			my_q75 = quantile(stat_parts,0.75)
			my_sd = abs(my_q75 - my_q25)/1.34898
			

			# 3 colors of parts...
			# color 1: green - the parts in control
			# color 2: blue - the ones used to gen mean/sd
			# color 3: red - the ones out of control
			#----------------------------
			my_colors = rep(1,times=results_count)
			my_colors[start_n:stop_n] = 2


			# Western Electric rules for outliers
			#-------------------------------------
			if(do_western_electric && is.finite(my_sd) && (my_sd>0.0)) {

				end = my_parts

				# rule2: 2 of 3 consecutive parts >+2sd or 
				#        2 of 3 consecutive parts <-2sd
				#----------------------------------------
				zones = as.integer(samples>(my_mean + 2*my_sd))
				filt = filter(zones,rep(1,3)/2,method="conv",sides=1)
				filt2 = as.integer(filt>=1)
				filt2[(end+1):(end+2)]=0
				filt3 = filter(filt2,rep(1,3),method="conv",sides=1)[3:(end+2)]
				bad_parts = which((zones>=1) & (filt3>=1))
				my_colors[xrefs[bad_parts]] = 6

				zones = as.integer(samples<(my_mean - 2*my_sd))
				filt = filter(zones,rep(1,3)/2,method="conv",sides=1)
				filt2 = as.integer(filt>=1)
				filt2[(end+1):(end+2)]=0
				filt3 = filter(filt2,rep(1,3),method="conv",sides=1)[3:(end+2)]
				bad_parts = which((zones>=1) & (filt3>=1))
				my_colors[xrefs[bad_parts]] = 6

				# rule3: 4 of 5 consecutive parts >+1sd or 
				#        4 of 5 consecutive parts <-1sd
				#----------------------------------------
				zones = as.integer(samples>(my_mean + my_sd))
				filt = filter(zones,rep(1,5)/4,method="conv",sides=1)
				filt2 = as.integer(filt>=1)
				filt2[(end+1):(end+4)]=0
				filt3 = filter(filt2,rep(1,5),method="conv",sides=1)[5:(end+4)]
				bad_parts = which((zones>=1) & (filt3>=1))
				my_colors[xrefs[bad_parts]] = 6

				zones = as.integer(samples<(my_mean - my_sd))
				filt = filter(zones,rep(1,5)/4,method="conv",sides=1)
				filt2 = as.integer(filt>=1)
				filt2[(end+1):(end+4)]=0
				filt3 = filter(filt2,rep(1,5),method="conv",sides=1)[5:(end+4)]
				bad_parts = which((zones>=1) & (filt3>=1))
				my_colors[xrefs[bad_parts]] = 6

				# rule4: 9 consecutive parts on same side of mean 
				#-------------------------------------------------
				zones = as.integer(samples>my_mean)
				filt = filter(zones,rep(1,9)/9,method="conv",sides=1)
				filt2 = as.integer(filt>=1)
				filt2[(end+1):(end+8)]=0
				filt3 = filter(filt2,rep(1,9),method="conv",sides=1)[9:(end+8)]
				bad_parts = which((zones>=1) & (filt3>=1))
				my_colors[xrefs[bad_parts]] = 6

				zones = as.integer(samples<my_mean)
				filt = filter(zones,rep(1,9)/9,method="conv",sides=1)
				filt2 = as.integer(filt>=1)
				filt2[(end+1):(end+8)]=0
				filt3 = filter(filt2,rep(1,9),method="conv",sides=1)[9:(end+8)]
				bad_parts = which((zones>=1) & (filt3>=1))
				my_colors[xrefs[bad_parts]] = 6

				# variant...
				# rule4 should be 8 consecutive parts
				# add trend rules:
				# 6 consecutive parts trending in same direction
				# 14 consecutive parts alternating trend direction
				#--------------------------------------------------
			}

			if(is.finite(my_sd)) {
				# rule1: any point outside +/-3sd
				#-----------------------------------
				clip = my_mean + 3.0*my_sd
				bad_parts = which(results_vector>clip)
				my_colors[bad_parts] = 3

				clip = my_mean - 3.0*my_sd
				bad_parts = which(results_vector<clip)
				my_colors[bad_parts] = 3
			}

			bad_count = length(which(my_colors==3))

			weco_count = length(which(my_colors==6)) + bad_count + length(which(my_colors==7))


			# clip results outside +/-4sd
			#-----------------------------------
			if (is.finite(my_sd) && (my_sd>0.0)) {
				clip = my_mean + 4.01*my_sd
				bad_parts = which(results_vector>clip)
				my_colors[bad_parts] = 4
				results_vector[bad_parts] = clip

				clip = my_mean - 4.01*my_sd
				bad_parts = which(results_vector<clip)
				my_colors[bad_parts] = 5
				results_vector[bad_parts] = clip
			}

			valid_results = results_vector[which(is.finite(results_vector))]
			my_max = max(valid_results)
			my_min = min(valid_results)
		} else {
			# invalid data... we'll want to do an empty plot...
			# but need to make sure parameter text variables are set
			my_units = paste(prefix,sep="",units)
		}


        # print out title line for this parameter
        #-----------------------------------------
        subscreen_num = split.screen(matrix(c(0.0,1.0,ytop-title_height,ytop),
                        1,4,byrow=FALSE),erase=FALSE)
        screen(subscreen_num)
        my_title = sprintf("%.0f  %s",test_num,test_nam)	# test_num can be >%d, use %.0f (uint32)
        my_limits = sprintf("LL=%.2f  UL=%.2f  %s",ll,ul,my_units)
		# determine font size if testname+limits is too big for page width
		title_width = strwidth(my_title,cex=1.0)
		limits_width = strwidth(my_limits,cex=0.9)
		title_cex=1.0
		limits_cex=0.9
		if((title_width+limits_width)>0.90) {
			adjust = 0.90/(title_width+limits_width)
			title_cex = title_cex*adjust
			limits_cex = limits_cex*adjust
		}
		text(0.0,0.3,sprintf("%s",my_title),pos=4,cex=title_cex)
        text(1.0,0.3,sprintf("%s",my_limits),pos=2, cex=limits_cex)

        close.screen(subscreen_num)
        ytop = ytop - title_height


		# stats part of plot
		#-----------------------
        subscreen_num = split.screen(matrix(c(0.0,stats_width,ytop-chart_height,ytop),
                                1,4,byrow=FALSE),erase=FALSE)
        screen(subscreen_num)
		if(my_parts>0) {
			text(0.0,0.84,sprintf("Mean = %.3f",my_mean),pos=4,cex=0.6)
			text(0.0,0.70,sprintf("SDev = %.3f",my_sd),pos=4,cex=0.6)
			text(0.0,0.56,sprintf("Count = %d",my_parts),pos=4,cex=0.6)
			if (bad_count>0) {
				text(0.0,0.42,sprintf("3sdev Fails = %d",bad_count),pos=4,cex=0.6,col="red")
			} else {
				text(0.0,0.42,sprintf("3sdev Fails = %d",bad_count),pos=4,cex=0.6)
			}
			if (do_western_electric) {
				if (weco_count>0) {
					text(0.0,0.28,sprintf("WER Fails = %d",weco_count),pos=4,cex=0.6,col="red")
				} else {
					text(0.0,0.28,sprintf("WER Fails = %d",weco_count),pos=4,cex=0.6)
				}
			}
		} else {
			text(0.0,0.84,sprintf("Mean = na"),pos=4,cex=0.6)
			text(0.0,0.70,sprintf("SDev = na"),pos=4,cex=0.6)
			text(0.0,0.56,sprintf("Count = 0"),pos=4,cex=0.6)
		}
		close.screen(subscreen_num)


		# chart part of the plot
		#----------------------------
        subscreen_num=split.screen(matrix(c(stats_width+0.01,0.98,ytop-chart_height,ytop),
                                1,4,byrow=FALSE),erase=FALSE)
        screen(subscreen_num)

		par(plt=c(0.1,0.95,0.20,0.95))
		if(my_parts>0) {
			chart_min = my_mean-3.1*my_sd
			chart_max = my_mean+3.1*my_sd
			if(!is.finite(chart_min) || (my_min<chart_min))  chart_min = my_min
			if(!is.finite(chart_max) || (my_max>chart_max))  chart_max = my_max
			if (is.finite(my_sd)&&(my_sd<=0.0)) {
				if (is.finite(ul)&&(ul>my_mean)) {
					chart_max = ul
					clip = chart_max
					bad_parts = which(results_vector>clip)
					my_colors[bad_parts] = 4
					results_vector[bad_parts] = clip
				}
				if (is.finite(ll)&&(ll<my_mean)) {
					chart_min = ll
					clip = chart_min
					bad_parts = which(results_vector<clip)
					my_colors[bad_parts] = 5
					results_vector[bad_parts] = clip
				}
			}
			x_range = c(1,results_count)
			y_range = c(chart_min,chart_max)
			plot(x_range,y_range,type="n",main="",xlab="",ylab="",
				bty="o",mgp=c(2,0.4,0),cex.axis=0.6,xaxs="i")
			abline(h=my_mean,lty="dashed",col="blue")
			abline(h=(my_mean+my_sd),lty="dotted",col="cyan")
			abline(h=(my_mean-my_sd),lty="dotted",col="cyan")
			abline(h=(my_mean+2*my_sd),lty="dotted",col="cyan")
			abline(h=(my_mean-2*my_sd),lty="dotted",col="cyan")
			abline(h=(my_mean+3*my_sd),lty="dashed",col="cyan")
			abline(h=(my_mean-3*my_sd),lty="dashed",col="cyan")
			if (is.finite(ll))  abline(h=ll,lty="dashed",lwd=2,col="red")
			if (is.finite(ul))  abline(h=ul,lty="dashed",lwd=2,col="red")
			lines(results_vector)

			indices = which(my_colors==5)
			points(indices,results_vector[indices],cex=0.6,pch=6,col="red",main="",xlab="",ylab="")
			indices = which(my_colors==4)
			points(indices,results_vector[indices],cex=0.6,pch=2,col="red",main="",xlab="",ylab="")
			indices = which(my_colors==7)
			points(indices,results_vector[indices],cex=0.6,col="yellow",main="",xlab="",ylab="")
			indices = which(my_colors==6)
			points(indices,results_vector[indices],cex=0.6,col="orange",main="",xlab="",ylab="")
			indices = which(my_colors==3)
			points(indices,results_vector[indices],cex=0.6,col="red",main="",xlab="",ylab="")

			indices = which(my_colors==2)
			points(indices,results_vector[indices],cex=0.6,col="blue",main="",xlab="",ylab="")

			indices = which(my_colors==1)
			points(indices,results_vector[indices],cex=0.6,col="green",main="",xlab="",ylab="")

			grid(ny=NA)
		}
        close.screen(subscreen_num)
        ytop = ytop - chart_height

		# if sd<(mean/100)... annotate as mean+number rather than number?

	}


    if (to_pdf) {
        close.screen(all=TRUE)
        new_device_info = dev.off()           # close pdf file
    }


    timestamp9 = proc.time()
    timestamp9 = timestamp9[3] - timestamp0
    cat(sprintf("Finished! processed %d parameters in %.2f seconds \n",
            max_params,timestamp9))

}
