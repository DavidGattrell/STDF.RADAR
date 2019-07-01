#  ConvertCsv.R
#
# $Id: ConvertCsv.R,v 1.17 2019/07/01 20:10:39 david Exp $
#
#  R script that reads either csv or rtdf files and generates
#  the equivalent rtdf or csv version.  
#
# Copyright (C) 2009-15 David Gattrell
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


##########################################################################
ConvertCsv <- function(in_name="",out_name="",rows_are_tests=TRUE,in_dir="",
					posix_time=TRUE,lim_eq_flags=FALSE) {

	# in_name - .saf or .saf.gz or .csv or .csv.gz or .rtdf
    # out_name - if empty, build it from in_name
    # rows_are_tests - csv can be either:
	#				- devices in columns, tests in rows
	#               OR
	#				- devices in rows, tests in columns
	# in_dir - if input file is in a different directory, this is the
	#			 absolute path to that directory
	# posix_time - when converting to csv, show times in posix format,
	#            else, convert to ascii (start_t and finish_t)
	# lim_eq_flags - adds columns for "equals limit is PASS" flags for 
	#            lower and upper limits to CSV file ll_eq_pass,ul_eq_pass
    #---------------------------------------------

    # define scaler prefixes
    #------------------------
    big_prefixes = c("10x","100x","K","10xK","100xK","M","10xM","100xM",
		    "G","10xG","100xG","T")
    lil_prefixes = c("d","c","m","100xu","10xu","u","100xn","10xn","n",
		    "100xp","10xp","p","100xf","10xf","f")

	all_prefixes = c("_",lil_prefixes,big_prefixes)
	prefix_xrefs = c(0,1:15,-1:-12)

	timestamp0 = proc.time()
    timestamp0 = timestamp0[3]  # start time
    timestamp1 = timestamp0   # time since last screen write


    # if input filename not defined, prompt for it
    #------------------------------------------
    if (in_name == "") {
        in_name <- readline("Enter the name of the file you want to convert: ")
    }

	if (length(grep("[rR]tdf$",in_name))>0) {
		in_type="RTDF"
	    if (out_name == "") {
	        out_name = sub("[.]?[rR]tdf$",".csv",in_name)
	    }
	} else {
		in_type="CSV"
	    if (out_name == "") {
			out_name = sub("(csv)?(.gz)?$","rtdf",in_name)
	    }
	}

	if (in_type=="CSV") {
		if (in_dir != "") {
			my_dir = getwd()
			setwd(in_dir)
		}
		CSV <- gzfile(in_name,"r")
		# use scan, it does a good job of not getting confused by
		# commas in testnames, etc...
		row1 <- scan(file=CSV,what=character(0),sep=",",strip.white=TRUE,
				multi.line=FALSE,fill=TRUE,nlines=1,quiet=TRUE,na.strings="NaN")
		cols = length(row1)
		# read in the rest of the lines...
		my_list <- scan(file=CSV,what=as.list(character(cols)),sep=",",
				strip.white=TRUE,multi.line=FALSE,fill=TRUE,na.strings="NaN")
		close(CSV)
		if (in_dir != "")  setwd(my_dir)

		cells = unlist(my_list)
		cell_count = length(cells)
		rows = cell_count/cols
		if(max(as.numeric(row1))>1.5) {
			my_csv = cbind(row1,matrix(cells,nrow=cols,ncol=rows,byrow=TRUE))
			xrows = rows
			rows = cols
			cols = xrows + 1
		} else {
			my_csv = rbind(row1,matrix(cells,nrow=rows,ncol=cols,byrow=FALSE))
			rows = rows + 1
		}
		
		# csv should now be a matrix of character strings,
		# with rows being tests, cols being devices

		# check if start_t/finish_t are in ascii format.. "2002-12-02 09:18:54 EST"
		if( length(grep(":",my_csv[4,3]))>0 ) {
			tokens = strsplit(my_csv[4,3]," ")[[1]]
			# ignore timezone, not valid / reliable 
			#my_tz = tokens[3]
			# issue here!  It will create a string called "EDT", but it won't
			# recognize it to convert back!
			#is_dst = FALSE
			#if(substr(my_tz,2,2)=="D") {
			#	is_dst = TRUE
			#	substr(my_tz,2,2) <- "S"
			#}
			ascii_t = paste(tokens[1],tokens[2])
			if( length(grep("/",ascii_t))>0 ) {
				# if  in format "%m/%d/%Y %H:%M", need to 
				# reformat to "%Y-%m-%d %H:%M:%S"
				#
				# below didn't work:
				#unix_t = as.numeric(as.POSIXct(ascii_t,"%m/%d/%Y %H:%M"))

				items = strsplit(tokens[1],"/")[[1]]
				t_items = strsplit(tokens[2],":")[[1]]
				if(length(t_items)<3) {
					cat("WARNING: seconds subfield missing from start_t field, setting to 00\n")
					cat(" .. Did csv file come from LibreOffice?\n")
					t_items[3] = "00"	# add seconds if missing
				}
				ascii_t = sprintf("%s-%s-%s %s:%s:%s",items[3],items[1],items[2],
										t_items[1],t_items[2],t_items[3])
			} 

			# NOTE: if it doesn't recognize the tz string, it just ignores it
			# The tz string created by POSIXlt is not necessarily guaranteed
			# to be recognized by POSIXct, EDT is a prime example!

			# should really get the current timezone and compare against the ascii
			# timezone and adjust by the difference.  But typically they are
			# the same so this bold presumption should go unnoticed :)
			unix_t = as.numeric(as.POSIXct(ascii_t,"%Y-%m-%d %H:%M:%S"))
			
			# correct for daylight savings... needed?
			my_time = as.POSIXlt(unix_t,origin=ISOdatetime(1970,1,1,0,0,0))
			if(my_time[["isdst"]]) {
				unix_t = unix_t - 3600	# an assumption here!
			}
			my_csv[4,3] = unix_t
		}
		if( length(grep(":",my_csv[9,3]))>0 ) {
			tokens = strsplit(my_csv[9,3]," ")[[1]]
			#my_tz = tokens[3]
			ascii_t = paste(tokens[1],tokens[2])
			if( length(grep("/",ascii_t))>0 ) {
				items = strsplit(tokens[1],"/")[[1]]
				t_items = strsplit(tokens[2],":")[[1]]
				if(length(t_items)<3) {
					cat("WARNING: seconds subfield missing from finish_t field, setting to 00\n")
					cat(" .. Did csv file come from LibreOffice?\n")
					t_items[3] = "00"	# add seconds if missing
				}
				ascii_t = sprintf("%s-%s-%s %s:%s:%s",items[3],items[1],items[2],
										t_items[1],t_items[2],t_items[3])
			} 
			unix_t = as.numeric(as.POSIXct(ascii_t,"%Y-%m-%d %H:%M:%S"))

			# correct for daylight savings... needed?
			my_time = as.POSIXlt(unix_t,origin=ISOdatetime(1970,1,1,0,0,0))
			if(my_time[["isdst"]])  unix_t = unix_t - 3600

			my_csv[9,3] = unix_t
		}

		# extract LotInfoFrame
		#-------------------------------
		my_list = list( lotid=my_csv[2,3], 
						sublotid=my_csv[3,3],
						start_t=as.numeric(my_csv[4,3]), 
						program=my_csv[5,3],
						tester_type=my_csv[6,3], 
						tester_id=my_csv[7,3],
						handler=my_csv[8,3],
						finish_t=as.numeric(my_csv[9,3]),
						job_rev=my_csv[10,3]
						)
		LotInfoFrame = data.frame(rbind(my_list))

		# determine if there are test conditions in the DevicesFrame
		#-------------------------------------------------------------
		first_part = match(1,my_csv[1,],nomatch=0)
		first_test = match(2,my_csv[,1],nomatch=0)
		devices = cols - first_part + 1
		conds = 0
		if (first_test>12) {
			conds = first_test - 12
			Condition_Names = my_csv[11:(11+conds-1),(first_part-1)]
			#ConditionsMatrix = array(as.numeric(my_csv[11:(11+conds-1),first_part:cols]),dim=c(conds,devices))
			# can now be "source_dataset", conditions, or DTRs, so not always numeric...
			ConditionsMatrix = array(my_csv[11:(11+conds-1),first_part:cols],dim=c(conds,devices))
		} else if (first_test<12) {
			# oh dear, something is wrong...
			cat("ERROR: first test is too soon in CSV file!\n")
		}
		tests = rows - first_test + 1

		# extract ParametersFrame
		#--------------------------------
		Parameters_Names = my_csv[first_test:rows,3]
		Parameters_testnum = as.integer(my_csv[first_test:rows,2])
		prefix = my_csv[first_test:rows,4]
		prefix_factored = factor(prefix,levels=all_prefixes)
		Parameters_scaler = prefix_xrefs[as.integer(prefix_factored)]
		bad_idx = which(is.na(Parameters_scaler))
		Parameters_scaler[bad_idx] = 0
		scale = 10^Parameters_scaler
		Parameters_units = my_csv[first_test:rows,5]
		Parameters_ll = as.numeric(my_csv[first_test:rows,6])/scale
		Parameters_ul = as.numeric(my_csv[first_test:rows,7])/scale

		if(my_csv[(first_test-1),8] == "ll_eq_pass") {
			Parameters_ll_ge = as.integer(my_csv[first_test:rows,8])
			Parameters_ul_ge = as.integer(my_csv[first_test:rows,9])
			Parameters_plot_ll = as.numeric(my_csv[first_test:rows,10])/scale
			Parameters_plot_ul = as.numeric(my_csv[first_test:rows,11])/scale
		} else {
			Parameters_ll_ge = rep(NaN,tests)
			Parameters_ul_ge = rep(NaN,tests)
			Parameters_plot_ll = as.numeric(my_csv[first_test:rows,8])/scale
			Parameters_plot_ul = as.numeric(my_csv[first_test:rows,9])/scale
		}

		# extract DevicesFrame
		#----------------------------
		Devices_part_id = my_csv[2,first_part:cols]
		Devices_temp = as.numeric(my_csv[3,first_part:cols])
		Devices_x_coord = as.numeric(my_csv[4,first_part:cols])
		Devices_y_coord = as.numeric(my_csv[5,first_part:cols])
		Devices_wafer_ids = my_csv[6,first_part:cols]
		Devices_soft_bin = as.numeric(my_csv[7,first_part:cols])
		Devices_hard_bin = as.numeric(my_csv[8,first_part:cols])
		Devices_testtime = as.numeric(my_csv[9,first_part:cols])
		Devices_site = as.numeric(my_csv[10,first_part:cols])
		factored_wfr_ids = factor(Devices_wafer_ids)
		Devices_wafer_index = as.numeric(factored_wfr_ids)
		wafer_ids = levels(factored_wfr_ids)
		if( (length(wafer_ids)>1) || (wafer_ids[1]!="") ) {
			# create WafersFrame
			my_list = list(wafer_id=wafer_ids[1], start_t=NaN,
                finish_t=NaN,part_cnt=NaN,
                good_cnt=NaN)
			WafersFrame <- data.frame(rbind(my_list))
			WafersFrame[1:length(wafer_ids),"wafer_id"] <- wafer_ids
			valid_wafersframe = TRUE
		} else  valid_wafersframe = FALSE

		# extract ResultsMatrix
		#--------------------------
		#browser()   # uncomment for debugging...
		ResultsMatrix = t(array(as.numeric(my_csv[first_test:rows,first_part:cols])/scale,dim=c(tests,devices)))
		ResultsMatrix[which(is.na(ResultsMatrix))] = NaN

		# bind Frames together
		#---------------------
	    if(conds>0) {
			my_code =    "my_list = list(part_id=\"\", temp=NaN,"
			my_code[2] = "x_coord=NaN, y_coord=NaN,"
			my_code[3] = "wafer_index=NaN,"
			my_code[4] = "soft_bin=NaN, hard_bin=NaN,"
			my_code[5] = "testtime=NaN, site=NaN"
			for (i in 1:conds) {
				my_code[5+i] = sprintf(",%s=NaN",Condition_Names[i])
			}
			my_code[6+i] = ")"

			eval(parse(text=my_code))
		} else {
			my_list = list(part_id="", temp=NaN,
                x_coord=NaN, y_coord=NaN,
                wafer_index=NaN,
                soft_bin=NaN, hard_bin=NaN,
                testtime=NaN, site=NaN)
		}
	    DevicesFrame <- data.frame(rbind(my_list))
	    DevicesFrame[1:devices,"part_id"] <- Devices_part_id
	    DevicesFrame[1:devices,"temp"] <- Devices_temp
	    DevicesFrame[1:devices,"x_coord"] <- Devices_x_coord
	    DevicesFrame[1:devices,"y_coord"] <- Devices_y_coord
	    DevicesFrame[1:devices,"wafer_index"] <- Devices_wafer_index
	    DevicesFrame[1:devices,"soft_bin"] <- Devices_soft_bin
	    DevicesFrame[1:devices,"hard_bin"] <- Devices_hard_bin
	    DevicesFrame[1:devices,"testtime"] <- Devices_testtime
	    DevicesFrame[1:devices,"site"] <- Devices_site
		if(conds>0) {
			for (i in 1:conds) {
				DevicesFrame[1:devices,Condition_Names[i]] <- ConditionsMatrix[i,]
			}
		}

		my_list = list(testnum=NaN, testname="",
		                scaler=NaN, units="",
		                ll=NaN, ul=NaN,
						ll_ge=NaN, ul_ge=NaN,
		                plot_ll=NaN, plot_ul=NaN)
		ParametersFrame <- data.frame(rbind(my_list))
		ParametersFrame[1:tests,"testnum"] <- Parameters_testnum
		ParametersFrame[1:tests,"testname"] <- Parameters_Names
		ParametersFrame[1:tests,"scaler"] <- Parameters_scaler
		ParametersFrame[1:tests,"units"] <- Parameters_units
		ParametersFrame[1:tests,"ll"] <- Parameters_ll
		ParametersFrame[1:tests,"ul"] <- Parameters_ul
		ParametersFrame[1:tests,"ll_ge"] <- Parameters_ll_ge
		ParametersFrame[1:tests,"ul_ge"] <- Parameters_ul_ge
		ParametersFrame[1:tests,"plot_ll"] <- Parameters_plot_ll
		ParametersFrame[1:tests,"plot_ul"] <- Parameters_plot_ul
		
		# save Rtdf file
	    #-----------------
	    my_list = c("LotInfoFrame","ParametersFrame","DevicesFrame","ResultsMatrix")
		if(valid_wafersframe)  my_list[length(my_list)+1] = "WafersFrame"
	    save(list=my_list, file=out_name)

	} else {
		if (in_dir != "") {
			my_dir = getwd()
			setwd(in_dir)
		}
		load(in_name)
		if (in_dir != "")  setwd(my_dir)


		devices = dim(ResultsMatrix)[1]
		tests = dim(ResultsMatrix)[2]
		# determine conditions...
		main_fields = c("part_id","temp","x_coord","y_coord","wafer_index",
				"soft_bin","hard_bin","testtime","site")
		#my_fields = labels(DevicesFrame[1,])[[2]]
		my_fields = names(DevicesFrame)
		which_fields = rep(TRUE,length(my_fields))
		for (i in 1:length(main_fields)) {
			idx = match(main_fields[i],my_fields,nomatch=0)
			if (idx>0)  which_fields[idx] = FALSE
		}
		cond_fields = which(which_fields)
		conditions = length(cond_fields)

		# what fields are in the LotInfoFrame?
		lif_names = names(LotInfoFrame)
		valid_job_rev = FALSE
		if(match("job_rev",lif_names,nomatch=0)>0)  valid_job_rev = TRUE  

		# does this rtdf file have ll_ge and ul_ge fields in its ParametersFrame?
		valid_lim_eq_data = FALSE
		if(lim_eq_flags) {
			my_names = names(ParametersFrame)
			if(length(which(my_names=="ll_ge"))>0) {
				valid_lim_eq_data = TRUE
			} else {
				cat("WARNING: This RTDF file does not have ll_ge/ul_ge fields in its ParametersFrame\n")
			}
		}

		csv_conn = file(out_name,"w")

		if (rows_are_tests) {
			cols = devices + 9
			rows = tests + conditions + 11

			if(lim_eq_flags) {
				my_vect = c((1:7)/10,0.72,0.74,0.8,0.9,rep(1,devices))
				add_2cols = ",,"
				cols = cols + 2
			} else {
				my_vect = c((1:9)/10,rep(1,devices))
				add_2cols = ""
			}
			cat(my_vect,file=csv_conn,sep=",")
			cat("\n",file=csv_conn)

			# dump DevicesFrame and LotInfoFrame stuff
			the_string = sprintf("0.2,lotid,%s,,,,,%s,part_id,",
						as.character(LotInfoFrame[[1,"lotid"]]),
						add_2cols)
			cat(the_string,file=csv_conn)
			cat(as.character(DevicesFrame[["part_id"]]),file=csv_conn,sep=",")
			cat("\n",file=csv_conn)
			
			the_string = sprintf("0.3,sublotid,%s,,,,,%s,temp,",
						as.character(LotInfoFrame[[1,"sublotid"]]),
						add_2cols)
			cat(the_string,file=csv_conn)
			tmp = as.character(DevicesFrame[["temp"]])
			tmp[which(is.na(DevicesFrame[["temp"]]))]=""
			cat(tmp,file=csv_conn,sep=",")
			cat("\n",file=csv_conn)
			
			if(posix_time) {
				start_t = as.character(LotInfoFrame[[1,"start_t"]][1])
			} else {
				start_t = ISOdatetime(1970,1,1,0,0,0) + as.numeric(LotInfoFrame[["start_t"]][1])
				my_t = as.POSIXlt(start_t)
				my_tzs = attr(my_t,"tzone")
				if (length(my_tzs)==3) {
					my_tz = as.character(my_tzs[2+(my_t$isdst)])
				} else {
					my_tz = as.character(my_tzs)
				}
				# don't track timezone info, it isn't reliable.
				#start_t = paste(start_t," ",my_tz,sep="")
			}
			the_string = sprintf("0.4,start_t,%s,,,,,%s,x_coord,",
						start_t, add_2cols)
			cat(the_string,file=csv_conn)
			tmp = as.character(DevicesFrame[["x_coord"]])
			tmp[which(is.na(DevicesFrame[["x_coord"]]))]=""
			cat(tmp,file=csv_conn,sep=",")
			cat("\n",file=csv_conn)
			
			the_string = sprintf("0.5,program,%s,,,,,%s,y_coord,",
						as.character(LotInfoFrame[[1,"program"]]),
						add_2cols)
			cat(the_string,file=csv_conn)
			tmp = as.character(DevicesFrame[["y_coord"]])
			tmp[which(is.na(DevicesFrame[["y_coord"]]))]=""
			cat(tmp,file=csv_conn,sep=",")
			cat("\n",file=csv_conn)

			the_string = sprintf("0.6,tester_type,%s,,,,,%s,wafer_id,",
						as.character(LotInfoFrame[[1,"tester_type"]]),
						add_2cols)
			cat(the_string,file=csv_conn)
			tmp2 = as.numeric(DevicesFrame[["wafer_index"]])
			indx = which(tmp2>0)
			tmp = rep("",length(tmp2))
			if(exists("WafersFrame",inherits=FALSE)) {
				tmp[indx] = as.character(WafersFrame[tmp2[indx],"wafer_id"])
			}
			cat(tmp,file=csv_conn,sep=",")
			cat("\n",file=csv_conn)

			the_string = sprintf("0.7,tester_id,%s,,,,,%s,soft_bin,",
						as.character(LotInfoFrame[[1,"tester_id"]]),
						add_2cols)
			cat(the_string,file=csv_conn)
			cat(as.character(DevicesFrame[["soft_bin"]]),file=csv_conn,sep=",")
			cat("\n",file=csv_conn)

			the_string = sprintf("0.8,handler,%s,,,,,%s,hard_bin,",
						as.character(LotInfoFrame[[1,"handler"]]),
						add_2cols)
			cat(the_string,file=csv_conn)
			cat(as.character(DevicesFrame[["hard_bin"]]),file=csv_conn,sep=",")
			cat("\n",file=csv_conn)

			if (is.finite(match("finish_t",names(LotInfoFrame)))) {
				if(posix_time) {
					finish_t = as.character(LotInfoFrame[[1,"finish_t"]][1])
				} else {
					finish_t = ISOdatetime(1970,1,1,0,0,0) + as.numeric(LotInfoFrame[["finish_t"]][1])
					my_t = as.POSIXlt(finish_t)
					my_tzs = attr(my_t,"tzone")
					if (length(my_tzs)==3) {
						my_tz = as.character(my_tzs[2+(my_t$isdst)])
					} else {
						my_tz = as.character(my_tzs)
					}
					# don't add timezone, it isn't valid
					#finish_t = paste(finish_t," ",my_tz,sep="")
				}
			} else finish_t = ""
			the_string = sprintf("0.85,finish_t,%s,,,,,%s,testtime,",
						finish_t, add_2cols)
			cat(the_string,file=csv_conn)
			tmp = as.character(DevicesFrame[["testtime"]])
			tmp[which(is.na(DevicesFrame[["testtime"]]))]=""
			cat(tmp,file=csv_conn,sep=",")
			cat("\n",file=csv_conn)

			if(valid_job_rev) {
				the_string = sprintf("0.90,job_rev,%s,,,,,%s,site,", 
									as.character(LotInfoFrame[["job_rev"]][1]),add_2cols)
			} else {
				the_string = sprintf("0.90,,,,,,,%s,site,", add_2cols)
			}
			cat(the_string,file=csv_conn)
			if (match("site",names(DevicesFrame),nomatch=0)>0) {
				tmp = as.character(DevicesFrame[["site"]])
				tmp[which(is.na(DevicesFrame[["site"]]))]=""
			} else {
				tmp = rep("",devices)
			}
			cat(tmp,file=csv_conn,sep=",")
			cat("\n",file=csv_conn)

			# now loop for any conditions
			if (conditions>0) {
				for (i in 1:conditions) {
					sort_field = 0.90 + i/1000.0
					cond_name = my_fields[cond_fields[i]]
					the_string = sprintf("%f,,,,,,,%s,%s,",sort_field,
								add_2cols, cond_name)
					cat(the_string,file=csv_conn)
					tmp = as.character(DevicesFrame[[cond_name]])
					tmp[which(is.na(DevicesFrame[[cond_name]]))]=""
					# remove any double quotes from condition 'values'
					tmp = gsub("\"","",tmp)
					# add leading and trailing double quotes to condition 'values'
					tmp = sub("(.*)","\"\\1\"",tmp)
					cat(tmp,file=csv_conn,sep=",")
					cat("\n",file=csv_conn)
				}
			}



			
			if(lim_eq_flags) {
				eq_pass_cols = "ll_eq_pass,ul_eq_pass,"
			} else {
				eq_pass_cols = ""
			}
			the_string = sprintf("0.99,testnum,testname,scaler,units,ll,ul,%splot_ll,plot_ul\n",
						eq_pass_cols)
			cat(the_string,file=csv_conn)

			# dump ParametersFrame and ResultsMatrix stuff
			for (i in 1:tests) {
			    timestamp2 = proc.time()
				timestamp2 = timestamp2[3]
				if (timestamp2>(timestamp1+5.0)) {
					timestamp1 = timestamp2
					pct = 100.0 * i / tests
					cat(sprintf("processing parameter %d of %d... ",i,tests))
					cat(sprintf(" %.1f%% through file \n",pct))
				}
				if(lim_eq_flags) {
					if(valid_lim_eq_data) {
						add_2cols = sprintf("%d,%d,",
								ParametersFrame[[i,"ll_ge"]],
								ParametersFrame[[i,"ul_ge"]])
					} else {
						add_2cols = ",,"
					}
				} else {
					add_2cols = ""
				}
				plot_ll = ParametersFrame[[i,"plot_ll"]]
				plot_ul = ParametersFrame[[i,"plot_ul"]]
				ll = ParametersFrame[[i,"ll"]]
				ul = ParametersFrame[[i,"ul"]]
				scaler = ParametersFrame[[i,"scaler"]]
				if (is.na(scaler))  scaler=0  
				if (scaler<0) {
				    prefix = big_prefixes[-1*scaler]
				} else if (scaler==0) {
				    prefix = "_"
				} else {
				    prefix = lil_prefixes[scaler]
				}
				scale = 10^scaler
				if(is.finite(ll))  ll=as.character(ll*scale)
				else  ll = ""
				if(is.finite(ul))  ul=as.character(ul*scale)
				else  ul = ""
				if(is.finite(plot_ll))  plot_ll= as.character(plot_ll*scale)
				else  plot_ll = ""
				if(is.finite(plot_ul))  plot_ul= as.character(plot_ul*scale)
				else  plot_ul = ""
				# IF testname contains " " or "," character, need to put double quotes
				# around testname
				testname = ParametersFrame[[i,"testname"]]
				if ((regexpr(" ",testname)>0) || (regexpr(",",testname)>0)) {
				    testname=paste('"',testname,'"',sep="")
				}
				#  REVISIT...
				# testnum can be > %d, so change to %.0f
				the_string = sprintf("2,%.0f,%s,%s,%s,%s,%s,%s%s,%s,",
						ParametersFrame[[i,"testnum"]],
						testname,		#ParametersFrame[[i,"testname"]],
						prefix,
						ParametersFrame[[i,"units"]],
						ll,
						ul,
						add_2cols,
						plot_ll,
						plot_ul
					)
				cat(the_string,file=csv_conn)
				tmp = as.character(ResultsMatrix[,i]*scale)
				tmp[which(is.na(ResultsMatrix[,i]))]=""
				cat(tmp,file=csv_conn,sep=",")
				cat("\n",file=csv_conn)
				
			}
		} else {	### rows are devices, cols are tests/parameters ###
			rows = devices + 9
			cols = tests + conditions + 11
			if (conditions>0) {
				cond_commas = paste(rep(",",conditions),sep="",collapse="")
				conds = 0.90 + (1:conditions)/1000.0
				my_vect = c((1:8)/10,0.85,0.9,conds,0.99,rep(2,tests))
			} else {
				cond_commas = ""
				my_vect = c((1:8)/10,0.85,0.9,0.99,rep(2,tests))
			}
			cat(my_vect,file=csv_conn,sep=",")
			cat("\n",file=csv_conn)

			# dump ParametersFrame and LotInfoFrame stuff
			the_string1 = "0.2,lotid,sublotid,start_t,program,tester_type,"
			if(valid_job_rev) {
				the_string2 = "tester_id,handler,finish_t,job_rev,"
			} else {
				the_string2 = "tester_id,handler,finish_t,,"
			}
			the_string3 = "testnum,"
			the_string = paste(the_string1,the_string2,cond_commas,the_string3,sep="")
			cat(the_string,file=csv_conn)
			cat(as.character(ParametersFrame[["testnum"]]),file=csv_conn,sep=",")
			cat("\n",file=csv_conn)

			if(posix_time) {
				start_t = as.character(LotInfoFrame[[1,"start_t"]])
			} else {
				start_t = ISOdatetime(1970,1,1,0,0,0) + as.numeric(LotInfoFrame[["start_t"]])
				my_t = as.POSIXlt(start_t)
				my_tzs = attr(my_t,"tzone")
				if (length(my_tzs)==3) {
					my_tz = as.character(my_tzs[2+(my_t$isdst)])
				} else {
					my_tz = as.character(my_tzs)
				}
				# don't track timezone, it isn't really valid
				#start_t = paste(start_t," ",my_tz,sep="")
			}
			if (is.finite(match("finish_t",names(LotInfoFrame)))) {
				if(posix_time) {
					finish_t = as.character(LotInfoFrame[[1,"finish_t"]])
				} else {
					finish_t = ISOdatetime(1970,1,1,0,0,0) + as.numeric(LotInfoFrame[["finish_t"]])
					my_t = as.POSIXlt(finish_t)
					my_tzs = attr(my_t,"tzone")
					if (length(my_tzs)==3) {
						my_tz = as.character(my_tzs[2+(my_t$isdst)])
					} else {
						my_tz = as.character(my_tzs)
					}
					# don't track timezone, it isn't really valid
					#finish_t = paste(finish_t," ",my_tz,sep="")
				}
			} else finish_t = ""
			if(valid_job_rev) {
				job_rev = as.character(LotInfoFrame[[1,"job_rev"]][1])
			} else {
				job_rev = ""
			}
			the_string = sprintf("0.3,%s,%s,%s,%s,%s,%s,%s,%s,%s%s,testname,",
					as.character(LotInfoFrame[[1,"lotid"]][1]),
					as.character(LotInfoFrame[[1,"sublotid"]][1]),
					start_t[1],
					as.character(LotInfoFrame[[1,"program"]][1]),
					as.character(LotInfoFrame[[1,"tester_type"]][1]),
					as.character(LotInfoFrame[[1,"tester_id"]][1]),
					as.character(LotInfoFrame[[1,"handler"]][1]),
					finish_t[1],
					job_rev,
					cond_commas
				)
			cat(the_string,file=csv_conn)
			testnames = as.character(ParametersFrame[["testname"]])
			# remove any double quotes from testnames
			testnames = gsub("\"","",testnames)
			# add leading and trailing double quotes to testnames
			testnames = sub("(.*)","\"\\1\"",testnames)
			cat(testnames,file=csv_conn,sep=",")
			cat("\n",file=csv_conn)

			the_string = sprintf("0.4,,,,,,,,,%s,scaler,",cond_commas)
			cat(the_string,file=csv_conn)
			scaler = as.numeric(ParametersFrame[["scaler"]])
			scaler[which(is.na(scaler))] = 0
			prefix = NA
			prefix[which(scaler==0)] = "_"
			idx = which(scaler>0)
			prefix[idx]=lil_prefixes[scaler[idx]]
			idx = which(scaler<0)
			prefix[idx] = big_prefixes[-1*scaler[idx]]
			cat(prefix,file=csv_conn,sep=",")
			cat("\n",file=csv_conn)
			scale = 10^scaler

			the_string = sprintf("0.5,,,,,,,,,%s,units,",cond_commas)
			cat(the_string,file=csv_conn)
			cat(as.character(ParametersFrame[["units"]]),file=csv_conn,sep=",")
			cat("\n",file=csv_conn)

			the_string = sprintf("0.6,,,,,,,,,%s,ll,",cond_commas)
			cat(the_string,file=csv_conn)
			ll = as.character(as.numeric(ParametersFrame[["ll"]])*scale)
			ll[which(is.na(ParametersFrame[["ll"]]))] = ""
			cat(ll,file=csv_conn,sep=",")
			cat("\n",file=csv_conn)

			the_string = sprintf("0.7,,,,,,,,,%s,ul,",cond_commas)
			cat(the_string,file=csv_conn)
			ul = as.character(as.numeric(ParametersFrame[["ul"]])*scale)
			ul[which(is.na(ParametersFrame[["ul"]]))] = ""
			cat(ul,file=csv_conn,sep=",")
			cat("\n",file=csv_conn)

			if(lim_eq_flags) {
				the_string = sprintf("0.72,,,,,,,,,%s,ll_eq_pass,",cond_commas)
				cat(the_string,file=csv_conn)
				if(valid_lim_eq_data) {
					ll_ge = as.character(as.numeric(ParametersFrame[["ll_ge"]]))
					ll_ge[which(is.na(ParametersFrame[["ll_ge"]]))] = ""
					cat(ll_ge,file=csv_conn,sep=",")
				}
				cat("\n",file=csv_conn)

				the_string = sprintf("0.74,,,,,,,,,%s,ul_eq_pass,",cond_commas)
				cat(the_string,file=csv_conn)
				if(valid_lim_eq_data) {
					ul_ge = as.character(as.numeric(ParametersFrame[["ul_ge"]]))
					ul_ge[which(is.na(ParametersFrame[["ul_ge"]]))] = ""
					cat(ul_ge,file=csv_conn,sep=",")
				}
				cat("\n",file=csv_conn)
			}

			the_string = sprintf("0.8,,,,,,,,,%s,plot_ll,",cond_commas)
			cat(the_string,file=csv_conn)
			ll = as.character(as.numeric(ParametersFrame[["plot_ll"]])*scale)
			ll[which(is.na(ParametersFrame[["plot_ll"]]))] = ""
			cat(ll,file=csv_conn,sep=",")
			cat("\n",file=csv_conn)

			if (conditions>0) {
				cond_string = paste(my_fields[cond_fields],sep="",collapse=",")
				the_string3 = ",plot_ul,"
			} else {
				cond_string = ""
				the_string3 = "plot_ul,"
			}
			the_string1 = "0.9,part_id,temp,x_coord,y_coord,wafer_id,soft_bin,"
			the_string2 = "hard_bin,test_time,site,"
			the_string = paste(the_string1,the_string2,cond_string,the_string3,sep="")
			cat(the_string,file=csv_conn)
			ul = as.character(as.numeric(ParametersFrame[["plot_ul"]])*scale)
			ul[which(is.na(ParametersFrame[["plot_ul"]]))] = ""
			cat(ul,file=csv_conn,sep=",")
			cat("\n",file=csv_conn)

			# dump DevicesFrame and ResultsMatrix stuff
			for (i in 1:devices) {
			    timestamp2 = proc.time()
				timestamp2 = timestamp2[3]
				if (timestamp2>(timestamp1+5.0)) {
					timestamp1 = timestamp2
					pct = 100.0 * i / devices
					cat(sprintf("processing device %d of %d... ",i,devices))
					cat(sprintf(" %.1f%% through file \n",pct))
				}
				temp = as.numeric(DevicesFrame[[i,"temp"]])
				x_coord = as.numeric(DevicesFrame[[i,"x_coord"]])
				y_coord = as.numeric(DevicesFrame[[i,"y_coord"]])
				wafer_index = as.numeric(DevicesFrame[[i,"wafer_index"]])
				testtime = as.numeric(DevicesFrame[[i,"testtime"]])
				if (match("site",names(DevicesFrame),nomatch=0)>0) {
					site = DevicesFrame[[i,"site"]]
				} else {
					site = NA
				}
				if(is.finite(temp))  temp = as.character(temp)
				else  temp = ""
				if(is.finite(x_coord))  x_coord = as.character(x_coord)
				else  x_coord = ""
				if(is.finite(y_coord))  y_coord = as.character(y_coord)
				else  y_coord = ""
				if(is.finite(wafer_index)&&(wafer_index>0)&&
						exists("WafersFrame",inherits=FALSE)) {
					wafer_index = as.character(WafersFrame[wafer_index,"wafer_id"])
				} else  wafer_index = ""
				if(is.finite(testtime))  testtime = as.character(testtime)
				else  testtime = ""
				if(is.finite(site))  site = as.character(site)
				else  site = ""
				if (conditions>0) {
					# as.character() on a vector doesn't always do what you expect
					# if elements have unusual characters like []:, etc.
					# but behaves okay one-at-a-time, so now use for loop...
					first_loop = TRUE
					for (j in cond_fields) {
						cond_value = as.character(DevicesFrame[i,j])
						# for values that contain ,'s, we need to double quote,
						# so need to make sure no double quotes in values first
						cond_value = gsub("\"","",cond_value)
						cond_value = sub("(.*)","\"\\1\"",cond_value)
						if(first_loop) {
							first_loop = FALSE
							cond_string = cond_value
						} else {
							cond_string = paste(cond_string,cond_value,
											sep=",",collapse=",")
						}
					}
					last_commas = ",,"
				} else {
					cond_string = ""
					last_commas = ","
				}
				the_string = sprintf("1,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s%s",
						as.character(DevicesFrame[[i,"part_id"]]),
						temp,
						x_coord,
						y_coord,
						wafer_index,
						as.character(DevicesFrame[[i,"soft_bin"]]),
						as.character(DevicesFrame[[i,"hard_bin"]]),
						testtime,
						site,
						cond_string,
						last_commas
					)
				cat(the_string,file=csv_conn)
				tmp = as.character(ResultsMatrix[i,]*scale)
				tmp[which(is.na(ResultsMatrix[i,]))]=""
				cat(tmp,file=csv_conn,sep=",")
				cat("\n",file=csv_conn)
			}
		}
		close(csv_conn)

		if(cols>255) {
			cat("WARNING: More than 255 columns in CSV, need at least OpenOffice3,")
			cat("or Excel2007\n")
		}
		if(rows>65535) {
			cat("WARNING: More than 65535 rows in CSV, need at least OpenOffice3.")
			cat("or Excel2007\n")
		}
		cat(sprintf("CSV file generated with %d rows and %d columns\n",
			rows,cols))
	}
    timestamp9 = proc.time()
    timestamp9 = timestamp9[3] - timestamp0
    if (timestamp9<200.0) {
        cat(sprintf("Conversion Finished! \n processed %d Devices x %d Parameters in %.2f seconds\n",
                devices,tests,timestamp9))
    } else {
        cat(sprintf("Conversion Finished! \n processed %d Devices x %d Parameters in %.2f minutes\n",
                devices,tests,timestamp9/60.0))
    }
	
}


#############################################################################

