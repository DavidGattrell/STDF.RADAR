# ConvertParameters.R
#
# $Id: ConvertParameters.R,v 1.9 2015/04/18 01:38:00 david Exp $
#
# script that extracts the ParametersFrame information from an
# rtdf file and converts it to text or csv format, or reads in the
# text or csv format and generates an rtdf file with the information
# stored in a ParametersFrame object
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
#----------------------------------------------------------
ConvertParameters <- function(in_file="",out_file="",in_dir="") {

    # in_file -- string for filename of file to convert.  Filename
    #            is expected to end in one of 3 types:
    #            .txt  -- whitespace separated fields
    #            .csv  -- comma separated fields for spreadsheets
    #		 .Rtdf or .rtdf or .Rdata -- A R data file containing
    #            at least a ParametersFrame
    # out_file -- optional explicit output file name.  If the
    #             input is .txt or .csv, the default output is the 
    #             same name but with a .rtdf extension.  If the
    #             input is .rtdf or similar, the output defaults
    #             to a .txt extension and format.
	# in_dir -- if in_file is in a different directory, this is the
	#			  absolute path to that directory
    #--------------------------------------------------------------------

    # define scaler prefixes
    #------------------------
    big_prefixes = c("10x","100x","K","10xK","100xK","M","10xM","100xM",
		    "G","10xG","100xG","T")
    lil_prefixes = c("d","c","m","100xu","10xu","u","100xn","10xn","n",
		    "100xp","10xp","p","100xf","10xf","f")


    # if in_file hasn't been set, prompt for it
    #-------------------------------------------
    if (in_file == "") {
		in_file <- readline("Enter the name of the Rdata or txt file to convert: ")
    }
    
    
    # if the file ends in .txt, generate the .Rtdf version,
    # if in_file ends in .Rdata or .Rtdf or .rtdf,
    # generate .txt version
    #----------------------------------------------
    if (regexpr("txt$",in_file)>0) {
		# build output .Rtdf filename
		#----------------------------
		if (out_file == "") {
		    out_file = paste(as.character(strsplit(in_file,"txt$")),"rtdf",sep="")
		}

		# read in .txt file
		#-------------------
		if (in_dir != "") {
			my_dir = getwd()
			setwd(in_dir)
		}
		raw_param_frame = as.data.frame( 
			scan(
				file=in_file,what=list(
						testnum=0,testname="", scaler="",units="",
						ll=0,ul=0,plot_ll=0,plot_ul=0
				),
				skip=1,fill=TRUE
			)
		)
		if (in_dir != "") setwd(my_dir)
		size=dim(raw_param_frame)
		max_params = size[1]
		my_list = list(testnum=NaN, testname="",
				scaler=NaN, units="",
				ll=NaN, ul=NaN,
				plot_ll=NaN, plot_ul=NaN)
		ParametersFrame = data.frame(rbind(my_list))
		for (i in 1:max_params) {
		    # convert scaler back to numeric format and scale limits accordingly
		    #-------------------------------------------------------------------
		    scaler = as.character(raw_param_frame[i,"scaler"])
		    plot_ll = raw_param_frame[[i,"plot_ll"]]
		    plot_ul = raw_param_frame[[i,"plot_ul"]]
		    ll = raw_param_frame[[i,"ll"]]
		    ul = raw_param_frame[[i,"ul"]]
		    index = match(scaler,big_prefixes,nomatch=NaN)
		    if (is.finite(index)) {
				scaler = -1*index
		    } else {
				index = match(scaler,lil_prefixes,nomatch=NaN)
				if (is.finite(index)) {
				    scaler = index
				} else {
				    scaler = 0
				}
		    }
		    scale = 10^scaler
		    if(is.finite(ll))  ll=ll/scale
		    if(is.finite(ul))  ul=ul/scale
		    if(is.finite(plot_ll))  plot_ll= plot_ll/scale
		    if(is.finite(plot_ul))  plot_ul= plot_ul/scale

		    testname = as.character(raw_param_frame[i,"testname"])
		    units = as.character(raw_param_frame[i,"units"])
		    if (units=="_")  units=""
		    ParametersFrame[[i,"testnum"]] =  raw_param_frame[[i,"testnum"]]
		    ParametersFrame[[i,"testname"]] = testname
		    ParametersFrame[[i,"plot_ll"]] = plot_ll
		    ParametersFrame[[i,"plot_ul"]] = plot_ul
		    ParametersFrame[[i,"ll"]] = ll
		    ParametersFrame[[i,"ul"]] = ul
			ParametersFrame[[i,"scaler"]] = scaler
		    ParametersFrame[[i,"units"]] = units
		}
		save(ParametersFrame,file=out_file)

    } else if ( (regexpr("Rdata$",in_file)>0) ||
			(regexpr("Rtdf$",in_file)>0) ||
			(regexpr("rtdf$",in_file)>0) ) {
		# build output .txt filename
		#----------------------------
		if (out_file == "") {
		    if (regexpr("Rdata$",in_file)>0) {
				out_file = paste(as.character(strsplit(in_file,"Rdata$")),"txt",sep="")
		    } else if (regexpr("Rtdf$",in_file)>0) {
				out_file = paste(as.character(strsplit(in_file,"Rtdf$")),"txt",sep="")
		    } else if (regexpr("rtdf$",in_file)>0) {
				out_file = paste(as.character(strsplit(in_file,"rtdf$")),"txt",sep="")
		    }
		}
		if (regexpr("csv$",out_file)>0)  do_csv=TRUE
		else  do_csv=FALSE

		if (in_dir != "") {
			my_dir = getwd()
			setwd(in_dir)
		}
		load(in_file)
		if (in_dir != "") setwd(my_dir)

		if (exists("PlotParametersFrame",inherits=FALSE)) {
		   ParametersFrame = PlotParametersFrame
		} 
		if (exists("ParametersFrame",inherits=FALSE)) {
		    # dump ParametersFrame to .txt file
		    #---------------------------------------
		    size=dim(ParametersFrame)
		    max_params = size[1]

		    for (i in 1:max_params) {

			tnum = ParametersFrame[[i,"testnum"]]
			testname = ParametersFrame[[i,"testname"]]
			plot_ll = ParametersFrame[[i,"plot_ll"]]
			plot_ul = ParametersFrame[[i,"plot_ul"]]
			ll = ParametersFrame[[i,"ll"]]
			ul = ParametersFrame[[i,"ul"]]
			scaler = ParametersFrame[[i,"scaler"]]
			units = ParametersFrame[[i,"units"]]

			if (units=="")  units="_"
    
			if (is.na(scaler))  scaler=0  
			if (scaler<0) {
			    prefix = big_prefixes[-1*scaler]
			} else if (scaler==0) {
			    prefix = "_"
			} else {
			    prefix = lil_prefixes[scaler]
			}
			scale = 10^scaler
			if(is.finite(ll))  ll=ll*scale
			if(is.finite(ul))  ul=ul*scale
			if(is.finite(plot_ll))  plot_ll= plot_ll*scale
			if(is.finite(plot_ul))  plot_ul= plot_ul*scale


			# check if spaces or commas in testname, if so, add quotes
			#----------------------------------------------------------
			if ((regexpr(" ",testname)>0) || (regexpr(",",testname)>0)) {
			    testname=paste('"',testname,'"',sep="")
			}
		# check if testname is null string, if so, add quotes
		#-----------------------------------------------------
		if (nchar(testname)<1) {
		    testname=paste('"',testname,'"',sep="")
		}

		if(do_csv) {
		    if (i==1) {
			out_conn = file(out_file,"w")
			the_string = "test_num_,test_name_________,pref_,units___,lowlim,hi_lim,plotll,plothl \n"
			cat(the_string,file=out_conn)
		    }
		    if(is.finite(ll))  my_ll = sprintf("%g",ll)
		    else  my_ll = ""
		    if(is.finite(ul))  my_ul = sprintf("%g",ul)
		    else  my_ul = ""
		    if(is.finite(plot_ll))  my_plot_ll = sprintf("%g",plot_ll)
		    else  my_plot_ll = ""
		    if(is.finite(plot_ul))  my_plot_ul = sprintf("%g",plot_ul)
		    else  my_plot_ul = ""
		    # change tnum from %d to %.0f, %d is int32, need uint32 for tnum
			the_string = sprintf("%.0f,%s,%s,%s,%s,%s,%s,%s\n",
			    tnum,
			    testname,
			    prefix,
			    units,
			    my_ll,
			    my_ul,
			    my_plot_ll,
			    my_plot_ul
			    )
		    cat(the_string,file=out_conn)
		} else {
		    if (i==1) {
			out_conn = file(out_file,"w")
			the_string = "test_num_ test_name_________ pref_ units___ lowlim hi_lim plotll plothl \n"
			cat(the_string,file=out_conn)
		    }
		    the_string = sprintf("%9.0f %18s %5s %8s %6g %6g %6g %6g \n",
			    tnum,
			    testname,
			    prefix,
			    units,
			    ll,
			    ul,
			    plot_ll,
			    plot_ul
			    )
		    cat(the_string,file=out_conn)
		}
	    }
	    close(out_conn)

	} 

    } else if ( regexpr("csv$",in_file)>0 ) {
	# build output .Rtdf filename
	#----------------------------
	if (out_file == "") {
	    out_file = paste(as.character(strsplit(in_file,"csv$")),"rtdf",sep="")
	}

	# read in .csv file
	#-------------------
	if (in_dir != "") {
		my_dir = getwd()
		setwd(in_dir)
	}
	raw_param_frame = as.data.frame( 
			    scan(
				file=in_file,what=list(
				    testnum=0,testname="", scaler="",units="",
				    ll=0,ul=0,plot_ll=0,plot_ul=0
				),
				sep=",",strip.white=TRUE,skip=1,fill=TRUE,na.strings = "NaN"
			    )
			)
	if (in_dir != "") setwd(my_dir)

	size=dim(raw_param_frame)
	max_params = size[1]
	my_list = list(testnum=NaN, testname="",
		scaler=NaN, units="",
		ll=NaN, ul=NaN,
		plot_ll=NaN, plot_ul=NaN)
	ParametersFrame = data.frame(rbind(my_list))
	for (i in 1:max_params) {
	    # convert scaler back to numeric format and scale limits accordingly
	    #-------------------------------------------------------------------
	    scaler = as.character(raw_param_frame[i,"scaler"])
	    plot_ll = raw_param_frame[[i,"plot_ll"]]
	    plot_ul = raw_param_frame[[i,"plot_ul"]]
	    ll = raw_param_frame[[i,"ll"]]
	    ul = raw_param_frame[[i,"ul"]]
	    index = match(scaler,big_prefixes,nomatch=NaN)
	    if (is.finite(index)) {
		scaler = -1*index
	    } else {
		index = match(scaler,lil_prefixes,nomatch=NaN)
		if (is.finite(index)) {
		    scaler = index
		} else {
		    scaler = 0
		}
	    }
	    scale = 10^scaler
	    if(is.finite(ll))  ll=ll/scale
	    else ll=NaN
	    if(is.finite(ul))  ul=ul/scale
	    else ul=NaN
	    if(is.finite(plot_ll))  plot_ll= plot_ll/scale
	    else plot_ll=NaN
	    if(is.finite(plot_ul))  plot_ul= plot_ul/scale
	    else plot_ul=NaN

	    testname = as.character(raw_param_frame[i,"testname"])
	    units = as.character(raw_param_frame[i,"units"])
	    if (units=="_")  units=""
	    ParametersFrame[[i,"testnum"]] =  raw_param_frame[[i,"testnum"]]
	    ParametersFrame[[i,"testname"]] = testname
	    ParametersFrame[[i,"plot_ll"]] = plot_ll
	    ParametersFrame[[i,"plot_ul"]] = plot_ul
	    ParametersFrame[[i,"ll"]] = ll
	    ParametersFrame[[i,"ul"]] = ul
	    ParametersFrame[[i,"scaler"]] = scaler
	    ParametersFrame[[i,"units"]] = units
	}
	save(ParametersFrame,file=out_file)
	
    } else {
	# print nasty message
    }

}
