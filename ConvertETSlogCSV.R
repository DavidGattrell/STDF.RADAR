#  ConvertETSlogCSV.R
#
# $Id: ConvertETSlogCSV.R,v 1.1 2023/02/22 02:43:38 david Exp $
#
#  R script that reads in a csv file generated from an HP947x tester
#  and converts it into a set of R data.frames/matrix aka rtdf format.
#
# Copyright (C) 2023 David Gattrell
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
if(exists(".ConvertETSlogCSV.env")) rm(.ConvertETSlogCSV.env) 
.ConvertETSlogCSV.env <- new.env(hash=TRUE, size=35L)

assign("Timestamp1",0.0,envir=.ConvertETSlogCSV.env)		# timer to suppress prints to screen to >5sec

assign("Device_count",0,envir=.ConvertETSlogCSV.env)		# number of devices processed (PIR/PRR records)
assign("Parameter_count",0,envir=.ConvertETSlogCSV.env)	# number of tests processed (PTR/FTR/MPR*pins records)
assign("Wafer_count",0,envir=.ConvertETSlogCSV.env)		# number of wafers processed (WIR/WRR records)

assign("Hbin_count",0,envir=.ConvertETSlogCSV.env)			# number of Hard bins processed (HBR records)
assign("Sbin_count",0,envir=.ConvertETSlogCSV.env)			# number of Soft bins processed (SBR records)
assign("Previous_param_i",0,envir=.ConvertETSlogCSV.env)	# the ParametersFrame index from the previous PTR/FTR/MPR
assign("Good_guesses",0,envir=.ConvertETSlogCSV.env)		# how often Previous_param_i+1 worked

assign("LotInfoFrame",data.frame(rbind(
		list( lotid="", sublotid="",
                start_t=NaN, program="",
                tester_type="", tester_id="",
                handler_type="",handler="",part_typ="",
				oper_nam="",exec_typ="",exec_ver="")
		)),envir=.ConvertETSlogCSV.env)

assign("ResultsMatrix",array(NaN, dim=c(0,0)),envir=.ConvertETSlogCSV.env)

assign("WafersFrame",data.frame(rbind(
		list(wafer_id='', start_t=NaN,
					finish_t=NaN, part_cnt=NaN,
					good_cnt=NaN)
		)),envir=.ConvertETSlogCSV.env)

# due to speed issues, it is better to deal with vectors than data frames,
# so create the following vectors that duplicate data frame information
assign("Parameters_Names",NA,envir=.ConvertETSlogCSV.env)     # <<- as.character(ParametersFrame$testname)
assign("Parameters_testnum",NA,envir=.ConvertETSlogCSV.env)   # <<- as.integer(
assign("Parameters_scaler",NA,envir=.ConvertETSlogCSV.env)    # <<- as.integer(
assign("Parameters_units",NA,envir=.ConvertETSlogCSV.env)     # <<- as.character(
assign("Parameters_ll",NA,envir=.ConvertETSlogCSV.env)		# <<- as.numeric(
assign("Parameters_ul",NA,envir=.ConvertETSlogCSV.env)		# <<- as.numeric(
assign("Parameters_plot_ll",NA,envir=.ConvertETSlogCSV.env)	# <<- as.numeric(
assign("Parameters_plot_ul",NA,envir=.ConvertETSlogCSV.env)	# <<- as.numeric(

assign("Devices_part_id",NA,envir=.ConvertETSlogCSV.env)     # <<- as.character(DevicesFrame$part_id)
assign("Devices_temp",NA,envir=.ConvertETSlogCSV.env)        # <<- as.numeric(
assign("Devices_x_coord",NA,envir=.ConvertETSlogCSV.env)     # <<- as.integer(
assign("Devices_y_coord",NA,envir=.ConvertETSlogCSV.env)     # <<- as.integer(
assign("Devices_wafer_index",NA,envir=.ConvertETSlogCSV.env) # <<- as.integer(
assign("Devices_soft_bin",NA,envir=.ConvertETSlogCSV.env)    # <<- as.integer(
assign("Devices_hard_bin",NA,envir=.ConvertETSlogCSV.env)	   # <<- as.integer(
assign("Devices_testtime",NA,envir=.ConvertETSlogCSV.env)	   # <<- as.numeric(
assign("Devices_stoptime",NA,envir=.ConvertETSlogCSV.env)	   # <<- as.numeric(
assign("Devices_site",NA,envir=.ConvertETSlogCSV.env)		   # <<- as.numeric(


##########################################################################
ConvertETSlogCSV <- function(etslog_name="",rtdf_name="",etslog_dir="") {
    # etslog_name - name of ETS csv datalog file to convert to rtdf format,
	#            this can be gzipped
    # rtdf_name - name to give to rtdf formatted file
	# etslog_dir - if ETS .log file is in a different directory, this is the
	#			 absolute path to that directory
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
	LotInfoFrame[["handler_type"]] <<- ""
	LotInfoFrame[["handler"]] <<- ""
	LotInfoFrame[["part_typ"]] <<- ""
	LotInfoFrame[["oper_nam"]] <<- ""
	LotInfoFrame[["exec_typ"]] <<- ""
	LotInfoFrame[["exec_ver"]] <<- ""

    Device_count <<- 0
	In_Device <<- 0			# clear with record 130, if clear, set with record 100
    Parameter_count <<- 0
	Parameters_Names <<- NA
    Wafer_count <<- 0
    #Hbin_count <<- 0
    #Sbin_count <<- 0
	ResultsMatrix <<- array(NaN, dim=c(0,0))

    Pin_names <<- NA


    # if filenames not defined, prompt for them
    #------------------------------------------
    if (etslog_name == "") {
        etslog_name <- readline("Enter the name of the ETS datalog file to read: ")
    }

    if (rtdf_name == "") {
        rtdf_name <- readline("Enter the name of the rtdf file to write: ")
    }


	# load csv file into memory
	#-----------------------------
	if (etslog_dir != "") {
		my_dir = getwd()
		setwd(etslog_dir)
	}
	csv_main = read.csv(etslog_name,header=FALSE,colClasses="character")
	if (etslog_dir != "")  setwd(my_dir)

	csv_size = dim(csv_main)
	records = csv_size[1]
	cols = csv_size[2]


	for (record in 1:records) {
        timestamp2 = proc.time()
        timestamp2 = timestamp2[3]
        if (timestamp2>(Timestamp1+5.0)) {
			Timestamp1 <<- timestamp2
            pct = 100.0 * record / records
            cat(sprintf("processing Device %d ... ",Device_count))
            cat(sprintf(" %.1f%% through file \n",pct))
        }

		rec_typ = csv_main[record,1]
		if(rec_typ==120) {
			# some of MIR information
			# Tester Description Record .. goes into LotInfoFrame
			tester = csv_main[record,3]		# ETS-88
			lot_id = csv_main[record,6]		# <not specified>
			sblot_id = csv_main[record,7]	# <not specified>
			node_nam = csv_main[record,9]	# EXCIGENCE_Z6002
			oper_nam = csv_main[record,8]	# Valued Eagle Customer
			exec_ver = csv_main[record,10]	# 2019.1.1005.32  (really ETS Datalog reporter sw version?)

			LotInfoFrame[["lotid"]] <<- lot_id
			LotInfoFrame[["sublotid"]] <<- sblot_id
			LotInfoFrame[["tester_type"]] <<- tester 
			LotInfoFrame[["tester_id"]] <<- node_nam 
			LotInfoFrame[["oper_nam"]] <<- oper_nam 

			#browser()
		} else if (rec_typ==125) {
		} else if (rec_typ==140) {
			# Test Program records .. goes into LotInfoFrame
			rec_sub = csv_main[record,2]
			if(rec_sub==1) {
				job_nam = csv_main[record,4]
				LotInfoFrame[["program"]] <<- job_nam
			} else if(rec_sub==2) {
			} else if(rec_sub==3) {
			} else if(rec_sub==4) {
				part_typ = csv_main[record,3]
				LotInfoFrame[["part_typ"]] <<- part_typ
			} else if(rec_sub==5) {
			}


		} else if (rec_typ==150) {
			# maybe Site Description Record ..
		} else if (rec_typ==10) {
			# Test Description Record .. goes into ParametersFrame
			test_num_str = csv_main[record,2]
			decimal_places = as.integer(csv_main[record,3])
			low_lim = as.numeric(csv_main[record,5])
			high_lim = as.numeric(csv_main[record,4])
			units = csv_main[record,6]
			testname = csv_main[record,7]
			
			tnum_parts = unlist(strsplit(test_num_str,split='.',fixed=TRUE))
			big_tnum = as.integer(tnum_parts[1])
			small_tnum = as.integer(tnum_parts[2])
			test_num = 100000 * big_tnum + small_tnum 

			# check that we haven't seen this before
			if (Parameter_count >0) {
				if( test_num %in% Parameters_testnum ) {
					par_index = which(test_num==Parameters_testnum)[1]
					cat(sprintf("WARNING: overwriting ParametersFrame testnum %d at idx %d\n",
							test_num,par_index))
				} else {
					par_index = 0;
				}
			} else {
				par_index = 0;
			}
			
			# need to break units into scaler + units...
			# MHZ  scaler=6  units="Hz"
			# uA   scaler=-6 units="A"
			# mS   scaler=-3 units="S"
			# sort of stuff..
			my_scale = check_prefix(units)
			if (is.finite(my_scale)) {
				scale_p10 = my_scale
				scaler = 10^(scale_p10)

				my_chars = strsplit(units,"")[[1]]
				end = length(my_chars)
				if(end>1) {
					units = paste(my_chars[2:end],sep="",collapse="")
				} else {
					units = ""
				}
			} else {
				scale_p10 = 0;
				scaler = 1;
			}

			if(par_index<1) {
				# new test.. append to end of ParametersFrame 
				Parameter_count <<- Parameter_count + 1
				par_index = Parameter_count
			}


			Parameters_testnum[par_index] <<- test_num
			Parameters_Names[par_index] <<- testname
			Parameters_ll[par_index] <<- low_lim * scaler
			Parameters_ul[par_index] <<- high_lim * scaler
			Parameters_units[par_index] <<- units
			Parameters_scaler[par_index] <<- -1 * scale_p10


			#browser()
		} else if (rec_typ==100) {
			# Test Result Record .. goes into ResultsMatrix
			test_num_str = csv_main[record,2]
			pass_flag = csv_main[record,4]
			result = as.numeric(csv_main[record,5])

			tnum_parts = unlist(strsplit(test_num_str,split='.',fixed=TRUE))
			big_tnum = as.integer(tnum_parts[1])
			small_tnum = as.integer(tnum_parts[2])
			test_num = 100000 * big_tnum + small_tnum 

			if(In_Device<1) {
				# ok, starting a new device
				In_Device <<- 1
				if(Device_count<1) {
					ResultsMatrix <<- array(NaN, dim=c(1,Parameter_count))
				} else {
					ResultsMatrix <<- rbind(ResultsMatrix,NaN)
				}
				Device_count <<- Device_count + 1
			}

			# find matching ParametersFrame index
			par_index = which(test_num==Parameters_testnum)
			if(length(par_index)<1) {
				# undefined test!
			} else if(length(par_index)>1) {
				# duplicate testnumber matches!
			}

			scale_p10 = -1 * Parameters_scaler[par_index]
			scaler = 10^(scale_p10)

			#browser()
			ResultsMatrix[Device_count,par_index] <<- result * scaler

		} else if (rec_typ==130) {
			# Part Result Record
			part_id = csv_main[record,4]	# <not specified>
			pass_flg = csv_main[record,5]	# P or F
			stop_time = csv_main[record,3]	# 02/15/2023  14:42:36
			hard_bin = as.integer(csv_main[record,9])
			soft_bin = as.integer(csv_main[record,8])

			if(hard_bin<0)  hard_bin = 65535

			In_Device <<- 0		# ok, we finished dataloging the current device.

			Devices_part_id[Device_count] <<- part_id
			Devices_temp[Device_count] <<- NaN
			Devices_x_coord[Device_count] <<- NaN
			Devices_y_coord[Device_count] <<- NaN
			Devices_wafer_index[Device_count] <<- 0
			Devices_soft_bin[Device_count] <<- soft_bin 
			Devices_hard_bin[Device_count] <<- hard_bin 
			Devices_stoptime[Device_count] <<- stop_time
			if(Device_count>1) {
				# REVISIT: eventually pull apart stop time strings and use save test time + index time
				Devices_testtime[Device_count] <<- NaN 
			} else {
				Devices_testtime[Device_count] <<- NaN 
			}
			Devices_site[Device_count] <<- 0 	# REVISIT if we ever get a multi-site example!:w

			#browser()
		} else if (rec_typ==999) {
		} else if (rec_typ==50) {
		} else if (rec_typ==60) {
		} else if (rec_typ==1) {
		} else if (rec_typ==2) {
		} else if (rec_typ==3) {
		} else if (rec_typ==250) {
		} else if (rec_typ==260) {
		} else if (rec_typ==201) {
		} else if (rec_typ==202) {
		} else if (rec_typ==203) {
		} else if (rec_typ==300) {
		} else if (rec_typ==301) {
		} else if (rec_typ==30) {
		}

	}

	# and now build RTDF objects and save them
	#-----------------------------------------
	# LotInfoFrame already good to go.


	# ParametersFrame
	my_list = list(testnum=NaN, testname="",
	                scaler=NaN, units="",
	                ll=NaN, ul=NaN,
	                plot_ll=NaN, plot_ul=NaN)
	ParametersFrame <- data.frame(rbind(my_list))
	ParametersFrame[1:Parameter_count,"testnum"] <- Parameters_testnum
	ParametersFrame[1:Parameter_count,"testname"] <- Parameters_Names
	ParametersFrame[1:Parameter_count,"scaler"] <- Parameters_scaler
	ParametersFrame[1:Parameter_count,"units"] <- Parameters_units
	ParametersFrame[1:Parameter_count,"ll"] <- Parameters_ll
	ParametersFrame[1:Parameter_count,"ul"] <- Parameters_ul
	ParametersFrame[1:Parameter_count,"plot_ll"] <- NaN
	ParametersFrame[1:Parameter_count,"plot_ul"] <- NaN


	# DevicesFrame
    my_list = list(part_id="", temp=NaN,
                x_coord=NaN, y_coord=NaN,
                wafer_index=NaN,
                soft_bin=NaN, hard_bin=NaN,
                testtime=NaN)
    DevicesFrame <- data.frame(rbind(my_list))
    DevicesFrame[1:Device_count,"part_id"] <- Devices_part_id
    DevicesFrame[1:Device_count,"temp"] <- NaN
    DevicesFrame[1:Device_count,"x_coord"] <- NaN
    DevicesFrame[1:Device_count,"y_coord"] <- NaN
    DevicesFrame[1:Device_count,"wafer_index"] <- NaN
    DevicesFrame[1:Device_count,"soft_bin"] <- Devices_soft_bin
    DevicesFrame[1:Device_count,"hard_bin"] <- Devices_hard_bin	
    DevicesFrame[1:Device_count,"testtime"] <- NaN
    DevicesFrame[1:Device_count,"source_dataset"] <- etslog_name
    DevicesFrame[1:Device_count,"stoptime"] <- Devices_stoptime 
    DevicesFrame[1:Device_count,"site"] <- 0


	# ResultsMatrix
	if(!is.matrix(ResultsMatrix)) {
		ResultsMatrix <<- matrix(data=ResultsMatrix,nrow=Device_count,ncol=Parameter_count)
	}


	# save Rtdf file
    #-----------------
	my_list = c("LotInfoFrame","ParametersFrame","DevicesFrame","ResultsMatrix")

    if (Wafer_count>0) {
        my_list[length(my_list)+1] = "WafersFrame"
        #if (Valid_WCR)  my_list[length(my_list)+1] = "WaferInfoFrame"
    }

    save(list=my_list, file=rtdf_name)


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
check_prefix <- function(units) {

	my_chars = strsplit(units,"")[[1]]

	if(length(my_chars)>0) {
		if		(my_chars[1]=="T")	scale_p10 = 12
		else if (my_chars[1]=="G")	scale_p10 = 9
		else if (my_chars[1]=="M")	scale_p10 = 6
		else if (my_chars[1]=="K")	scale_p10 = 3
		else if (my_chars[1]=="k")	scale_p10 = 3
		else if (my_chars[1]=="%")	scale_p10 = -2		# Eagle STDF converts % to 'c' 
		else if (my_chars[1]=="m")	scale_p10 = -3
		else if (my_chars[1]=="u")	scale_p10 = -6
		else if (my_chars[1]=="n")	scale_p10 = -9
		else if (my_chars[1]=="p")	scale_p10 = -12
		else if (my_chars[1]=="f")	scale_p10 = -15
		else						scale_p10 = NA
	} else							scale_p10 = NA
		
	
	return( scale_p10 )
}



#############################################################################
#  copy local functions to the .ConvertETSlogCSV.env and remove them
#  from the global environment
#############################################################################
environment(check_prefix)<-.ConvertETSlogCSV.env
assign("check_prefix",check_prefix,envir=.ConvertETSlogCSV.env)
rm(check_prefix)

environment(ConvertETSlogCSV)<-.ConvertETSlogCSV.env

