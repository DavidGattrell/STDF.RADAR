#  ConvertEagleCSV.R
#
# $Id: ConvertEagleCSV.R,v 1.3 2012/04/04 01:03:44 David Exp $
#
#  R script that reads in a Eagle ETS text datalog file and converts it into a
#  set of R data.frames/matrix aka rtdf format.
#
# Copyright (C) 2011 David Gattrell
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
#  LotInfoFrame
#      LotInfoFrame[[index,"lotid"]]
#                         ,"sublotid"]]
#                         ,"start_t"]]
#                         ,"program"]]
#                         ,"tester_type"]]
#                         ,"tester_id"]]
#                         ,"handler"]]
#  HbinInfoFrame  (optional)
#      HbinInfoFrame[[index,"hbin_num"]]
#                          ,"hbin_cnt"]]
#                          ,"hbin_pf"]]
#                          ,"hbin_nam"]]
#  SbinInfoFrame  (optional)
#      SbinInfoFrame[[index,"sbin_num"]]
#                          ,"sbin_cnt"]]
#                          ,"sbin_pf"]]
#                          ,"sbin_nam"]]
#  WaferInfoFrame  (optional)
#      WaferInfoFrame[[index,"wafr_siz"]]
#                           ,"die_ht"]]
#                           ,"die_wid"]]
#                           ,"wf_units"]]
#                           ,"wf_flat"]]
#                           ,"center_x"]]
#                           ,"center_y"]]
#                           ,"pos_x"]]
#                           ,"pos_y"]]
#  WafersFrame  (optional)
#      WafersFrame[[index,"wafer_id"]]
#                        ,"start_t"]]
#                        ,"finish_t"]]
#                        ,"part_cnt"]]
#                        ,"good_cnt"]]
#  DevicesFrame
#      DevicesFrame[[index,"devnum"]]
#                         ,"temp"]]
#                         ,"x_coord"]]
#                         ,"y_coord"]]
#                         ,"wafer_index"]]
#                         ,"soft_bin"]]
#                         ,"hard_bin"]]
#                         ,"testtime"]]
#                         ,"site"]]
#  ParametersFrame
#      ParametersFrame[[index,"testnum"]]
#                            ,"testname"]]
#                            ,"scaler"]]
#                            ,"units"]]
#                            ,"ll"]]
#                            ,"ul"]]
#                            ,plot_ll"]]
#                            ,plot_ul"]]
#  TSRFrame
#      TSRFrame[[index,"testnum"]]
#                     ,"testname"]]
#                     ,"test_typ"]]
#                     ,"exec_cnt"]]
#                     ,"fail_cnt"]]
#                     ,"fixed_exec_cnt"]]
#                     ,"fixed_fail_cnt"]]
#  ResultsMatrix
#      rows = devices
#      cols = parameters
#
#--------------------------------------------------------------

# namespace for the function... main function Global variables and local functions
#-------------------------------------------------------
if(exists(".ConvertEagleCSV.env")) rm(.ConvertEagleCSV.env) 
.ConvertEagleCSV.env <- new.env()

assign("Timestamp1",0.0,envir=.ConvertEagleCSV.env)		# timer to suppress prints to screen to >5sec

assign("Device_count",0,envir=.ConvertEagleCSV.env)		# number of devices processed (PIR/PRR records)
assign("Parameter_count",0,envir=.ConvertEagleCSV.env)	# number of tests processed (PTR/FTR/MPR*pins records)
assign("Wafer_count",0,envir=.ConvertEagleCSV.env)		# number of wafers processed (WIR/WRR records)

assign("TSR_count",0,envir=.ConvertEagleCSV.env)		# number of TSR records processed 
assign("Hbin_count",0,envir=.ConvertEagleCSV.env)		# number of Hard bins processed (HBR records)
assign("Sbin_count",0,envir=.ConvertEagleCSV.env)		# number of Soft bins processed (SBR records)
assign("Previous_param_i",0,envir=.ConvertEagleCSV.env)	# the ParametersFrame index from the previous PTR/FTR/MPR
assign("Good_guesses",0,envir=.ConvertEagleCSV.env)		# how often Previous_param_i+1 worked

assign("LotInfoFrame",data.frame(rbind(
		list( lotid="", sublotid="",
                start_t=NaN, program="",
                tester_type="", tester_id="",
                handler="")
		)),envir=.ConvertEagleCSV.env)

assign("ResultsMatrix",array(NaN, dim=c(0,0)),envir=.ConvertEagleCSV.env)

assign("TSRFrame",data.frame(rbind(
		list(testnum=NaN, testname="",
						test_typ="",
						exec_cnt=NaN,fail_cnt=NaN,
						fixed_exec_cnt=NaN,
						fixed_fail_cnt=NaN)
		)),envir=.ConvertEagleCSV.env)

assign("WafersFrame",data.frame(rbind(
		list(wafer_id='', start_t=NaN,
					finish_t=NaN, part_cnt=NaN,
					good_cnt=NaN)
		)),envir=.ConvertEagleCSV.env)

# due to speed issues, it is better to deal with vectors than data frames,
# so create the following vectors that duplicate data frame information
assign("Parameters_Names",NA,envir=.ConvertEagleCSV.env)     # <<- as.character(ParametersFrame$testname)
assign("Parameters_testnum",NA,envir=.ConvertEagleCSV.env)   # <<- as.integer(
assign("Parameters_scaler",NA,envir=.ConvertEagleCSV.env)    # <<- as.integer(
assign("Parameters_units",NA,envir=.ConvertEagleCSV.env)     # <<- as.character(
assign("Parameters_ll",NA,envir=.ConvertEagleCSV.env)		# <<- as.numeric(
assign("Parameters_ul",NA,envir=.ConvertEagleCSV.env)		# <<- as.numeric(
assign("Parameters_plot_ll",NA,envir=.ConvertEagleCSV.env)	# <<- as.numeric(
assign("Parameters_plot_ul",NA,envir=.ConvertEagleCSV.env)	# <<- as.numeric(

assign("Devices_part_id",NA,envir=.ConvertEagleCSV.env)     # <<- as.character(DevicesFrame$part_id)
assign("Devices_temp",NA,envir=.ConvertEagleCSV.env)        # <<- as.numeric(
assign("Devices_x_coord",NA,envir=.ConvertEagleCSV.env)     # <<- as.integer(
assign("Devices_y_coord",NA,envir=.ConvertEagleCSV.env)     # <<- as.integer(
assign("Devices_wafer_index",NA,envir=.ConvertEagleCSV.env) # <<- as.integer(
assign("Devices_soft_bin",NA,envir=.ConvertEagleCSV.env)    # <<- as.integer(
assign("Devices_hard_bin",NA,envir=.ConvertEagleCSV.env)	   # <<- as.integer(
assign("Devices_testtime",NA,envir=.ConvertEagleCSV.env)	   # <<- as.numeric(
assign("Devices_site",NA,envir=.ConvertEagleCSV.env)		   # <<- as.numeric(

assign("TSRs_testnum",NA,envir=.ConvertEagleCSV.env)		# <<- as.integer(TSRFrame$testnum)
assign("TSRs_testname",NA,envir=.ConvertEagleCSV.env)
assign("TSRs_test_typ",NA,envir=.ConvertEagleCSV.env)
assign("TSRs_exec_cnt",NA,envir=.ConvertEagleCSV.env)
assign("TSRs_fail_cnt",NA,envir=.ConvertEagleCSV.env)
assign("TSRs_fixed_exec_cnt",NA,envir=.ConvertEagleCSV.env)
assign("TSRs_fixed_fail_cnt",NA,envir=.ConvertEagleCSV.env)


##########################################################################
ConvertEagleCSV <- function(ets_name="",rtdf_name="",ets_dir="",
					duplicate_testnames=FALSE,do_summary=TRUE,
					just_fail_tests_summary=TRUE) {
    # ets_name - name of Eagle text .log datalog file to convert to rtdf format,
	#            this can be gzipped
    # rtdf_name - name to give to rtdf formatted file
	# ets_dir - if Eagle .log file is in a different directory, this is the
	#			 absolute path to that directory
	# duplicate_testnames - if TRUE, then append _testnum to end of testname
	#			  so that testnames become unique
    # do_summary - generates .summary text file from csv .log file
    # just_fail_tests_summary - if do_summary, when dumping per test information
    #             only report the tests that had failures 
	#---------------------------------------------

    timestamp0 = proc.time()
    timestamp0 = timestamp0[3]  # start time
    Timestamp1 <<- timestamp0   # time since last screen write


	LotInfoFrame[["lotid"]] <<- ""
	LotInfoFrame[["sublotid"]] <<- ""
	LotInfoFrame[["start_t"]] <<- NaN
	LotInfoFrame[["program"]] <<- ""
	LotInfoFrame[["tester_type"]] <<- ""
	LotInfoFrame[["tester_id"]] <<- ""
	LotInfoFrame[["handler"]] <<- ""
	LotInfoFrame[["finish_t"]] <<- NaN

    Device_count <<- 0
    Parameter_count <<- 0
	Parameters_Names <<- NA
	Parameters_testnum <<- NA
    Wafer_count <<- 0
	TSR_count <<- 0
    #Hbin_count <<- 0
    Sbin_count <<- 0
	ResultsMatrix <<- array(NaN, dim=c(0,0))

    my_list = list(sbin_num=NaN, sbin_cnt=NaN, sbin_pf="",sbin_nam="")
    SbinInfoFrame <<- data.frame(rbind(my_list))

	#Pin_names <<- NA

	in_device = FALSE
	
	
    # if filenames not defined, prompt for them
    #------------------------------------------
    if (ets_name == "") {
        ets_name <- readline("Enter the name of the Eagle CSV datalog file to read: ")
    }

    if (rtdf_name == "") {
        rtdf_name <- readline("Enter the name of the Rdata file to write: ")
    }


    # open .data file for reading...
    # suck whole file into memory
    #---------------------------------------
	if (ets_dir != "") {
		my_dir = getwd()
		setwd(ets_dir)
	}
    ETS <- gzfile(ets_name,"r")

	# use scan, it does a good job of not getting confused by
	# commas in testnames, etc...
	cols = 11	# maximum number of tokens expected on a line
	my_list <- scan(file=ETS,what=as.list(character(cols)),sep=",",
			strip.white=TRUE,multi.line=FALSE,fill=TRUE,na.strings="NaN")
    close(ETS)
	if (ets_dir != "")  setwd(my_dir)

	my_matrix <- matrix(unlist(my_list),ncol=11,byrow=FALSE)
	lines <- dim(my_matrix)[1]


	# parse ETStxt lines...
	#--------------------------
	for (line in 1:lines) {
        timestamp2 = proc.time()
        timestamp2 = timestamp2[3]
        if (timestamp2>(Timestamp1+5.0)) {
			Timestamp1 <<- timestamp2
            pct = 100.0 * line / lines
            cat(sprintf("processing Device %d ... ",Device_count+1))
            cat(sprintf(" %.1f%% through file \n",pct))
        }

		tokens = my_matrix[line,]
		record = as.numeric(tokens[1])
		if (record==100) {	# check for this one first, it is the most frequent
			# sort of like a PTR record...
			
			if(!in_device) {
				in_device = TRUE
				add_device()
			}
			tmp = strsplit(tokens[2],"[.]")[[1]]
			test_numA = as.numeric(tmp[1])
			test_numB = as.numeric(tmp[2])
			test_num = test_numA * 100000 + test_numB
			site_num = 1	# hard code for now... REVISIT!!
			tmp = tokens[4]		# "P" or "F"
			if(tmp=="P")  pass_flag = TRUE
			else  pass_flag = FALSE
			result = as.numeric(tokens[5])
			
			if (Parameter_count>0) {
				if ((Previous_param_i < Parameter_count) &&
						(test_num==Parameters_testnum[Previous_param_i+1])) {
					par_index = Previous_param_i+1
					Good_guesses <<- Good_guesses + 1
				} else {
					par_index = match(test_num,Parameters_testnum,nomatch=0)
				}
		    } else {
		       par_index = 0
		    }
		    if (par_index<1) {
				#nasty message about no PDR before PTR
				cat(sprintf("No record 10 found for test number %d\n",test_num))
				#browser()
			} else {
           		ResultsMatrix[Device_count,par_index] <<- result

				Previous_param_i <<- par_index
			}			
		} else if (record==2) {
			# ignore these, info is also in a record==1
		} else if (record==1) {
			# sort of like a TSR record...
			tmp = strsplit(tokens[2],"[.]")[[1]]
			test_numA = as.numeric(tmp[1])
			test_numB = as.numeric(tmp[2])
			test_num = test_numA * 100000 + test_numB
			site_num = 1	# hard code for now... REVISIT!!
			test_nam = gsub("\"","",tokens[3])
			# NOTE: fix inconsistency... PDR record will have spaces
			#       where TSR record will have commas within testnames
			test_nam = gsub(","," ",test_nam)
			good_cnt = as.numeric(tokens[4])
			fail_cnt = as.numeric(tokens[5])
			test_typ = "P"
			if(test_numB>=0) {
				if(duplicate_testnames) {
					test_nam = sprintf("%s_%d",test_nam,test_num)
				}
                my_text = sprintf("^%s$",test_nam)	# no leading or trailing chars
                par_indices = grep(my_text,Parameters_Names)
                if (length(par_indices)<1) {
					# if name has [] chars or other special chars...
					par_indices = grep(test_nam,Parameters_Names,fixed=TRUE)
				}
                if (length(par_indices)<1) {
					#browser()
					# REVISIT.. need to debug issue with duplicate_testnames=TRUE...
                    cat(sprintf("WARNING: TSR (record 1) with no PDR (record 10) for test %s \n",
                        test_nam))
                } else {
					exec_cnt = good_cnt + fail_cnt
					TSR_count <<- TSR_count + 1
					TSRs_testnum[TSR_count] <<- as.integer(test_num)
					TSRs_testname[TSR_count] <<- as.character(test_nam)
					TSRs_test_typ[TSR_count] <<- as.character(test_typ)
					TSRs_exec_cnt[TSR_count] <<- as.integer(exec_cnt)
					TSRs_fail_cnt[TSR_count] <<- as.integer(fail_cnt)
					TSRs_fixed_exec_cnt[TSR_count] <<- as.integer(exec_cnt)
					TSRs_fixed_fail_cnt[TSR_count] <<- as.integer(fail_cnt)
				}				
				# REVISIT..
				# build summary info
			}

		} else if (record==3) {
			raw_cnt = as.numeric(tokens[2])
			pass_cnt = as.numeric(tokens[3])

			# REVISIT..
		} else if (record==10) {
			# sort of like a PDR record...
			tmp = strsplit(tokens[2],"[.]")[[1]]
			test_numA = as.numeric(tmp[1])
			test_numB = as.numeric(tmp[2])
			test_num = test_numA * 100000 + test_numB
			site_num = 1	# hard code for now... REVISIT!!
			hi_limit = as.numeric(tokens[4])
			lo_limit = as.numeric(tokens[5])
			units = gsub("\"","",tokens[6])
			test_nam = gsub("\"","",tokens[7])
			if(duplicate_testnames) {
				test_nam = sprintf("%s_%d",test_nam,test_num)
			}
			test_txt = test_nam

			scale_p10 = 0	# REVISIT, do we want to scale things?
			
			#browser()

			if (Parameter_count>0) {
				if ((Previous_param_i < Parameter_count) &&
						(test_num==Parameters_testnum[Previous_param_i+1])) {
					par_index = Previous_param_i+1
					Good_guesses <<- Good_guesses + 1
				} else {
					par_index = match(test_num,Parameters_testnum,nomatch=0)
				}
			} else {
			   par_index = 0
			}
		    if (par_index<1) {
				Parameter_count <<- Parameter_count + 1
				par_index = Parameter_count

				Parameters_testnum[Parameter_count] <<- as.numeric(test_num)
       			if (nchar(test_txt)<1) {
       			    Parameters_Names[Parameter_count] <<- ''
       			} else {
       			    Parameters_Names[Parameter_count] <<- test_txt
       			}
       			Parameters_scaler[Parameter_count] <<- -1*scale_p10	# ie power of 10, 0 = value of 1
       			Parameters_units[Parameter_count] <<- units
				Parameters_ll[Parameter_count] <<- lo_limit
       			Parameters_ul[Parameter_count] <<- hi_limit
       			Parameters_plot_ll[Parameter_count] <<- NaN
       			Parameters_plot_ul[Parameter_count] <<- NaN
	
       			#Parameters_Names <<- as.character(ParametersFrame$testname)

       			# we added a new parameter, so we need to add
      			# a new column to Results Matrix...
       			#---------------------------------------------
       			#ResultsMatrix <<- cbind(ResultsMatrix,NaN)
		   		# the above gives warnings... need to do this better!!!
		   		if(dim(ResultsMatrix)[1]>0) {
					ResultsMatrix <<- cbind(ResultsMatrix,NaN)
				} else {
					ResultsMatrix <<- array(NaN, dim=c(0,Parameter_count))
				}
			}
		} else if (record==30) {
			start_t = gsub("\"","",tokens[2])
			stop_t = gsub("\"","",tokens[3])
			
			# REVISIT...
		} else if (record==50) {
			# similar to SBR...
			sbin = as.numeric(tokens[2])
			if(tokens[3]=="\"P\"") {
				pass_flag = TRUE
				sbin_pf = "P"
			} else {
				pass_flag = FALSE
				sbin_pf = "F"
			}
			bin_count = as.numeric(tokens[4])
			bin_name = gsub("\"","",tokens[5])
			
			Sbin_count <<- Sbin_count + 1
			my_list = list(sbin_num=sbin, sbin_cnt=bin_count,
                        sbin_pf=sbin_pf, sbin_nam=bin_name)
			SbinInfoFrame[Sbin_count,] <<- my_list
			
		} else if (record==120) {
			# similar to MIR
			# can extract lot_id and sublot_id
			tester_type = gsub("\"","",tokens[3])
			lot_id = gsub("\"","",tokens[6])
			sublot_id = gsub("\"","",tokens[7])
			tester_id = gsub("\"","",tokens[9])

			LotInfoFrame[["tester_id"]] <<- tester_id
			LotInfoFrame[["tester_type"]] <<- tester_type
			#LotInfoFrame[["program"]] <<- program
			LotInfoFrame[["lotid"]] <<- lot_id
			LotInfoFrame[["sublotid"]] <<- sublot_id

		} else if (record==125) {

		} else if (record==130) {
			# similar to PRR
			site = tokens[2]
			time = tokens[3]	# note: string has "" that need to be removed
			part_id = gsub("\"","",tokens[4])
			#browser()
			if(tokens[5]=="\"P\"")  pass_flag = TRUE
			else  pass_flag = FALSE
			x_coord = as.numeric(tokens[6])
			y_coord = as.numeric(tokens[7])
			sbin = tokens[8]
			
			Devices_part_id[Device_count] <<- part_id
			Devices_site[Device_count] <<- site
			Devices_soft_bin[Device_count] <<- sbin
			if(is.finite(x_coord))  Devices_x_coord[Device_count] = x_coord
			if(is.finite(y_coord))  Devices_y_coord[Device_count] = y_coord
			if(pass_flag)  Devices_hard_bin[Device_count] = 1
			else  Devices_hard_bin[Device_count] = 2
			
			# REVISIT: convert time string to ms, use delta as run time?
			# Devices_testtime[Device_count] = 
			
			in_device = FALSE
		} else if (record==140) {

		} else if (record==145) {

		} else if (record==201) {
			# ignore these, info is also in a record==1
		} else if (record==202) {
			# ignore these, info is also in a record==1
		} else if (record==250) {
			# ignore these, info is also in a record=50
		} else if (record==300) {

		} else if (record==301) {

		} else if (record==999) {
			# ignore this record... could set an in_summary flag
		} else {
			# print message about unrecognized record type...
			# REVISIT
		}
	}


    # resize from allocated size to used size,
	# also pack into Frame if needed
    #----------------------------------------------
    ResultsMatrix <<- ResultsMatrix[1:Device_count,]
	# above creates a vector if only 1 device instead of a matrix, so below fixes this.
	if(!is.matrix(ResultsMatrix)) {
		ResultsMatrix <<- matrix(data=ResultsMatrix,nrow=Device_count,ncol=Parameter_count)
	}

    #DevicesFrame <<- DevicesFrame[1:Device_count,]
    my_list = list(part_id="", temp=NaN,
                x_coord=NaN, y_coord=NaN,
                wafer_index=NaN,
                soft_bin=NaN, hard_bin=NaN,
                testtime=NaN)
    DevicesFrame <- data.frame(rbind(my_list))
    DevicesFrame[1:Device_count,"part_id"] <- Devices_part_id[1:Device_count]
    DevicesFrame[1:Device_count,"temp"] <- Devices_temp[1:Device_count]
    DevicesFrame[1:Device_count,"x_coord"] <- Devices_x_coord[1:Device_count]
    DevicesFrame[1:Device_count,"y_coord"] <- Devices_y_coord[1:Device_count]
    DevicesFrame[1:Device_count,"wafer_index"] <- Devices_wafer_index[1:Device_count]
    DevicesFrame[1:Device_count,"soft_bin"] <- Devices_soft_bin[1:Device_count]
    DevicesFrame[1:Device_count,"hard_bin"] <- Devices_hard_bin[1:Device_count]
    DevicesFrame[1:Device_count,"testtime"] <- Devices_testtime[1:Device_count]
    DevicesFrame[1:Device_count,"site"] <- Devices_site[1:Device_count]

	my_list = list(testnum=NaN, testname="",
	                scaler=NaN, units="",
	                ll=NaN, ul=NaN,
	                plot_ll=NaN, plot_ul=NaN)
	ParametersFrame <- data.frame(rbind(my_list))
	ParametersFrame[1:Parameter_count,"testnum"] <- Parameters_testnum[1:Parameter_count]
	ParametersFrame[1:Parameter_count,"testname"] <- Parameters_Names[1:Parameter_count]
	ParametersFrame[1:Parameter_count,"scaler"] <- Parameters_scaler[1:Parameter_count]
	ParametersFrame[1:Parameter_count,"units"] <- Parameters_units[1:Parameter_count]
	ParametersFrame[1:Parameter_count,"ll"] <- Parameters_ll[1:Parameter_count]
	ParametersFrame[1:Parameter_count,"ul"] <- Parameters_ul[1:Parameter_count]
	ParametersFrame[1:Parameter_count,"plot_ll"] <- Parameters_plot_ll[1:Parameter_count]
	ParametersFrame[1:Parameter_count,"plot_ul"] <- Parameters_plot_ul[1:Parameter_count]

	if(TSR_count>0) {
        my_list = list(testnum=NaN, testname="",
						test_typ="",
						exec_cnt=NaN,fail_cnt=NaN,
						fixed_exec_cnt=NaN,
						fixed_fail_cnt=NaN)
		TSRFrame <<- data.frame(rbind(my_list))
		TSRFrame[1:TSR_count,"testnum"] <<- TSRs_testnum[1:TSR_count]
		TSRFrame[1:TSR_count,"testname"] <<- TSRs_testname[1:TSR_count]
		TSRFrame[1:TSR_count,"test_typ"] <<- TSRs_test_typ[1:TSR_count]
		TSRFrame[1:TSR_count,"exec_cnt"] <<- TSRs_exec_cnt[1:TSR_count]
		TSRFrame[1:TSR_count,"fail_cnt"] <<- TSRs_fail_cnt[1:TSR_count]
		TSRFrame[1:TSR_count,"fixed_exec_cnt"] <<- TSRs_fixed_exec_cnt[1:TSR_count]
		TSRFrame[1:TSR_count,"fixed_fail_cnt"] <<- TSRs_fixed_fail_cnt[1:TSR_count]
	}


	# save Rtdf file
    #-----------------
    my_list = c("LotInfoFrame","ParametersFrame","DevicesFrame","ResultsMatrix")

    if (Sbin_count>0) {
        my_list[length(my_list)+1] = "SbinInfoFrame"
    } else {
        cat(sprintf("End of file reached with no softbin summary \n"))
    }

    if (TSR_count>0) {
        my_list[length(my_list)+1] = "TSRFrame"
    }
	
	if (Wafer_count>0) {
        my_list[length(my_list)+1] = "WafersFrame"
        #if (Valid_WCR)  my_list[length(my_list)+1] = "WaferInfoFrame"
    }
	
    save(list=my_list, file=rtdf_name)

	if (do_summary) {
        # build filename
        #---------------
		out_file = rtdf_name
        if (regexpr("[.]Rdata$",rtdf_name)>0) {
            out_file = as.character(strsplit(rtdf_name,"[.]Rdata$"))
        } else if (regexpr("[.]Rtdf$",rtdf_name)>0) {
            out_file = as.character(strsplit(rtdf_name,"[.]Rtdf$"))
        } else if (regexpr("[.]rtdf$",rtdf_name)>0) {
            out_file = as.character(strsplit(rtdf_name,"[.]rtdf$"))
        }
        out_file = paste(out_file,".summary",sep="")
        cat(sprintf("Writing summary file to %s...\n",out_file))
        print_summary(ets_name,out_file,just_fail_tests_summary)
        cat(sprintf("...done summary file\n"))
	}

    timestamp9 = proc.time()
    timestamp9 = timestamp9[3] - timestamp0
    if (timestamp9<200.0) {
        cat(sprintf("Conversion Finished! \n processed %d Devices x %d Parameters in %.2f seconds\n",
                Device_count,Parameter_count,timestamp9))
    } else {
        cat(sprintf("Conversion Finished! \n processed %d Devices x %d Parameters in %.2f minutes\n",
                Device_count,Parameter_count,timestamp9/60.0))
    }

}


###############################################################################
print_summary <- function(stdf_name,out_file,just_fail_tests_summary) {

        # dump MIR information
        #-----------------------
        my_string = paste("SUMMARY for file: ",stdf_name,"\n",sep="")
        cat(my_string,file=out_file)
        my_string = "------------------------------------------------------------------------\n"
        cat(my_string,file=out_file,append=TRUE)
        my_string = paste("Lot ID:       ",as.character(LotInfoFrame[["lotid"]]),"\n",sep="")
        cat(my_string,file=out_file,append=TRUE)
        my_string = paste("Sublot ID:    ",as.character(LotInfoFrame[["sublotid"]]),"\n",sep="")
        cat(my_string,file=out_file,append=TRUE)
        my_string = paste("Test Program: ",as.character(LotInfoFrame[["program"]]),"\n",sep="")
        cat(my_string,file=out_file,append=TRUE)
        my_string = paste("Tester Type:  ",as.character(LotInfoFrame[["tester_type"]]),"\n",sep="")
        cat(my_string,file=out_file,append=TRUE)
        my_string = paste("Tester ID:    ",as.character(LotInfoFrame[["tester_id"]]),"\n",sep="")
        cat(my_string,file=out_file,append=TRUE)
        start_t = ISOdatetime(1970,1,1,0,0,0) + as.numeric(LotInfoFrame[["start_t"]])
        finish_t = ISOdatetime(1970,1,1,0,0,0) + as.numeric(LotInfoFrame[["finish_t"]])
        my_t = as.POSIXlt(start_t)
        my_tzs = attr(my_t,"tzone")
        if (length(my_tzs)==3) {
            my_tz = as.character(my_tzs[2+(my_t$isdst)])
        } else {
            my_tz = as.character(my_tzs)
        }
        my_string = paste("Start Time:   ",start_t," ",my_tz,"\n",sep="")
        cat(my_string,file=out_file,append=TRUE)
        my_string = paste("Finish Time:  ",finish_t,"\n",sep="")
        cat(my_string,file=out_file,append=TRUE)
        my_string = "------------------------------------------------------------------------\n"
        cat(my_string,file=out_file,append=TRUE)
        
        if (Sbin_count>0) {
            # SBR stuff: sort SbinInfoFrame by sbin_cnt...
            #------------------------------------------
            sbin_cnts = as.integer(SbinInfoFrame[["sbin_cnt"]])
            sorted = sort(sbin_cnts,decreasing=TRUE,index.return=TRUE)
            xrefs = sorted$ix
            total = sum(sbin_cnts)

            my_string = paste("Soft Binning Summary: Part Count ",total,"\n\n",sep="")
			cat(my_string,file=out_file,append=TRUE)

            my_string = "_Count  _____% Bin_no  _  Soft_Bin_Name_____________\n"
            cat(my_string,file=out_file,append=TRUE)
            # dump sbin count, %, bin #, type, name
            for (i in 1:length(xrefs)) {
                my_string = sprintf("%6d  %6.1f %6d  %s  %s \n",
                            as.integer(SbinInfoFrame[xrefs[i],"sbin_cnt"]),
                            100.0*as.numeric(SbinInfoFrame[xrefs[i],"sbin_cnt"])/total,
                            as.integer(SbinInfoFrame[xrefs[i],"sbin_num"]),
                            as.character(SbinInfoFrame[xrefs[i],"sbin_pf"]),
                            as.character(SbinInfoFrame[xrefs[i],"sbin_nam"]) )
                cat(my_string,file=out_file,append=TRUE)
            }
            my_string = "------------------------------------------------------------------------\n"
            cat(my_string,file=out_file,append=TRUE)
        }


        # HBR stuff
        #  sort HbinInfoFrame by hbin_num...

        # TSR stuff
        #------------------------------------------------------
        if (TSR_count>0) {       
            total = TSRFrame[[1,"exec_cnt"]]    # or should we grab largest number?
            my_string = paste("Test Summary: Part Count ",total,"\n\n",sep="")
            cat(my_string,file=out_file,append=TRUE)

            my_string = "%_of_all  %_of_exec  execs_  fails_  Test_Name_____________\n"
            cat(my_string,file=out_file,append=TRUE)

            #  use TSRFrame order
            for (i in 1:dim(TSRFrame)[1]) {
                exec_cnt = as.numeric(TSRFrame[i,"fixed_exec_cnt"])
                fail_cnt = as.numeric(TSRFrame[i,"fixed_fail_cnt"])
                testname = as.character(TSRFrame[i,"testname"])

                if (!just_fail_tests_summary || (fail_cnt>0)) {
					#browser()
                    pct = 100.0*fail_cnt/total
                    pct2 = 100.0*fail_cnt/exec_cnt

                    my_string = sprintf("  %6.1f     %6.1f  %6d  %6d  %s \n",
                                pct, pct2, exec_cnt, fail_cnt, testname )
                    cat(my_string,file=out_file,append=TRUE)
                }
            }
        }

        my_string = "------------------------------------------------------------------------\n"
        cat(my_string,file=out_file,append=TRUE)

        # PRR stuff
}


###############################################################################
check_prefix <- function(units) {

	my_chars = strsplit(units,"")[[1]]

	if(length(my_chars)>0) {
		if		(my_chars[1]=="T")	scale_p10 = 12
		else if (my_chars[1]=="G")	scale_p10 = 9
		else if (my_chars[1]=="M")	scale_p10 = 6
		else if (my_chars[1]=="K")	scale_p10 = 3
		else if (my_chars[1]=="k")	scale_p10 = 3
		else if (my_chars[1]=="m")	scale_p10 = -3
		else if (my_chars[1]=="u")	scale_p10 = -6
		else if (my_chars[1]=="n")	scale_p10 = -9
		else if (my_chars[1]=="p")	scale_p10 = -12
		else if (my_chars[1]=="f")	scale_p10 = -15
		else						scale_p10 = NA
	} else							scale_p10 = NA
		
	
	return( scale_p10 )
}


###############################################################################
add_device <- function() {

	part_id = ""

    Device_count <<- Device_count + 1
	
	if ( sum(dim(ResultsMatrix))==0) {
		Devices_part_id <<- part_id # <<- as.character(DevicesFrame$part_id)
		Devices_temp <<- NA         # <<- as.numeric(
		Devices_x_coord <<- NA      # <<- as.integer(
		Devices_y_coord <<- NA      # <<- as.integer(
		Devices_wafer_index <<- NA  # <<- as.integer(
		Devices_soft_bin <<- NA     # <<- as.integer(
		Devices_hard_bin <<- NA     # <<- as.integer(
		Devices_testtime <<- NA     # <<- as.numeric(
		Devices_site <<- NA			# <<- as.numeric(
            
        ResultsMatrix<<- array(NaN, dim=c(1,0))
	} else {
        # wait 5 devices before allocating in chunks to allow 
        # parameters frame length to stabilize a bit.
        if (Device_count>=5) {
            # allocate memory in bigger chunks less often 
            # to improve speed...
            chunk = 200
            if (Device_count==5) {
                Devices_part_id[5:chunk] <<- NA     # <<- as.character(DevicesFrame$part_id)
                Devices_temp[5:chunk] <<- NA        # <<- as.numeric(
                Devices_x_coord[5:chunk] <<- NA     # <<- as.integer(
                Devices_y_coord[5:chunk] <<- NA     # <<- as.integer(
                Devices_wafer_index[5:chunk] <<- NA # <<- as.integer(
                Devices_soft_bin[5:chunk] <<- NA    # <<- as.integer(
                Devices_hard_bin[5:chunk] <<- NA    # <<- as.integer(
                Devices_testtime[5:chunk] <<-NA     # <<- as.numeric(
                Devices_site[5:chunk] <<-NA			# <<- as.numeric(

                my_dims = dim(ResultsMatrix)
                ResultsMatrix<<- rbind(ResultsMatrix,matrix(NaN,nrow=chunk-4,
                                    ncol=my_dims[2]))
            } else {
                mod_d = Device_count %% chunk  # modulus
                if(mod_d==0) {
                    #DevicesFrame[(Device_count+1):(Device_count+chunk),] <<- my_list
                    Devices_part_id[(Device_count+1):(Device_count+chunk)] <<- NA   
                    Devices_temp[(Device_count+1):(Device_count+chunk)] <<- NA      
                    Devices_x_coord[(Device_count+1):(Device_count+chunk)] <<- NA   
                    Devices_y_coord[(Device_count+1):(Device_count+chunk)] <<- NA   
                    Devices_wafer_index[(Device_count+1):(Device_count+chunk)] <<- NA 
                    Devices_soft_bin[(Device_count+1):(Device_count+chunk)] <<- NA  
                    Devices_hard_bin[(Device_count+1):(Device_count+chunk)] <<- NA  
                    Devices_testtime[(Device_count+1):(Device_count+chunk)] <<-NA   
                    Devices_site[(Device_count+1):(Device_count+chunk)] <<-NA   

                    my_dims = dim(ResultsMatrix)
                    ResultsMatrix<<- rbind(ResultsMatrix,matrix(NaN,nrow=chunk,
                                    ncol=my_dims[2]))
                }
            }
            Devices_part_id[Device_count] <<- part_id  
        } else {
            #DevicesFrame[Device_count,] <<- my_list
            Devices_part_id[Device_count] <<- NA  
            Devices_temp[Device_count] <<- NA   
            Devices_x_coord[Device_count] <<- NA   
            Devices_y_coord[Device_count] <<- NA  
            Devices_wafer_index[Device_count] <<- NA
            Devices_soft_bin[Device_count] <<- NA  
            Devices_hard_bin[Device_count] <<- NA   
            Devices_testtime[Device_count] <<-NA  
            Devices_site[Device_count] <<-NA    

            ResultsMatrix<<- rbind(ResultsMatrix,NaN)
        }
		
	}
	
	Devices_part_id[Device_count] <<- part_id  
	Devices_temp[Device_count] <<- NaN   
	Devices_x_coord[Device_count] <<- NaN   
	Devices_y_coord[Device_count] <<- NaN   
	Devices_wafer_index[Device_count] <<- NaN
	Devices_soft_bin[Device_count] <<- NaN  
	Devices_hard_bin[Device_count] <<- NaN   
	Devices_testtime[Device_count] <<-NaN  
	Devices_site[Device_count] <<-NaN    
}



#############################################################################
#  copy local functions to the .ConvertEagleCSV.env and remove them
#  from the global environment
#############################################################################
environment(print_summary)<-.ConvertEagleCSV.env
assign("print_summary",print_summary,envir=.ConvertEagleCSV.env)
rm(print_summary)

environment(check_prefix)<-.ConvertEagleCSV.env
assign("check_prefix",check_prefix,envir=.ConvertEagleCSV.env)
rm(check_prefix)

#environment(update_LotInfoFrame)<-.ConvertEagleCSV.env
#assign("update_LotInfoFrame",update_LotInfoFrame,envir=.ConvertEagleCSV.env)
#rm(update_LotInfoFrame)

environment(add_device)<-.ConvertEagleCSV.env
assign("add_device",add_device,envir=.ConvertEagleCSV.env)
rm(add_device)



environment(ConvertEagleCSV)<-.ConvertEagleCSV.env

