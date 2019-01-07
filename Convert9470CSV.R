#  Convert9470CSV.R
#
# $Id: Convert9470CSV.R,v 1.2 2012/02/01 02:27:08 David Exp $
#
#  R script that reads in a csv file generated from an HP947x tester
#  and converts it into a set of R data.frames/matrix aka rtdf format.
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
if(exists(".Convert9470CSV.env")) rm(.Convert9470CSV.env) 
.Convert9470CSV.env <- new.env(hash=TRUE, size=35L)

assign("Timestamp1",0.0,envir=.Convert9470CSV.env)		# timer to suppress prints to screen to >5sec

assign("Device_count",0,envir=.Convert9470CSV.env)		# number of devices processed (PIR/PRR records)
assign("Parameter_count",0,envir=.Convert9470CSV.env)	# number of tests processed (PTR/FTR/MPR*pins records)
assign("Wafer_count",0,envir=.Convert9470CSV.env)		# number of wafers processed (WIR/WRR records)

assign("Hbin_count",0,envir=.Convert9470CSV.env)			# number of Hard bins processed (HBR records)
assign("Sbin_count",0,envir=.Convert9470CSV.env)			# number of Soft bins processed (SBR records)
assign("Previous_param_i",0,envir=.Convert9470CSV.env)	# the ParametersFrame index from the previous PTR/FTR/MPR
assign("Good_guesses",0,envir=.Convert9470CSV.env)		# how often Previous_param_i+1 worked

assign("LotInfoFrame",data.frame(rbind(
		list( lotid="", sublotid="",
                start_t=NaN, program="",
                tester_type="", tester_id="",
                handler="")
		)),envir=.Convert9470CSV.env)

assign("ResultsMatrix",array(NaN, dim=c(0,0)),envir=.Convert9470CSV.env)

assign("WafersFrame",data.frame(rbind(
		list(wafer_id='', start_t=NaN,
					finish_t=NaN, part_cnt=NaN,
					good_cnt=NaN)
		)),envir=.Convert9470CSV.env)

# due to speed issues, it is better to deal with vectors than data frames,
# so create the following vectors that duplicate data frame information
assign("Parameters_Names",NA,envir=.Convert9470CSV.env)     # <<- as.character(ParametersFrame$testname)
assign("Parameters_testnum",NA,envir=.Convert9470CSV.env)   # <<- as.integer(
assign("Parameters_scaler",NA,envir=.Convert9470CSV.env)    # <<- as.integer(
assign("Parameters_units",NA,envir=.Convert9470CSV.env)     # <<- as.character(
assign("Parameters_ll",NA,envir=.Convert9470CSV.env)		# <<- as.numeric(
assign("Parameters_ul",NA,envir=.Convert9470CSV.env)		# <<- as.numeric(
assign("Parameters_plot_ll",NA,envir=.Convert9470CSV.env)	# <<- as.numeric(
assign("Parameters_plot_ul",NA,envir=.Convert9470CSV.env)	# <<- as.numeric(

assign("Devices_part_id",NA,envir=.Convert9470CSV.env)     # <<- as.character(DevicesFrame$part_id)
assign("Devices_temp",NA,envir=.Convert9470CSV.env)        # <<- as.numeric(
assign("Devices_x_coord",NA,envir=.Convert9470CSV.env)     # <<- as.integer(
assign("Devices_y_coord",NA,envir=.Convert9470CSV.env)     # <<- as.integer(
assign("Devices_wafer_index",NA,envir=.Convert9470CSV.env) # <<- as.integer(
assign("Devices_soft_bin",NA,envir=.Convert9470CSV.env)    # <<- as.integer(
assign("Devices_hard_bin",NA,envir=.Convert9470CSV.env)	   # <<- as.integer(
assign("Devices_testtime",NA,envir=.Convert9470CSV.env)	   # <<- as.numeric(
assign("Devices_site",NA,envir=.Convert9470CSV.env)		   # <<- as.numeric(


##########################################################################
Convert9470CSV <- function(csv94_name="",rtdf_name="",csv94_dir="") {
    # csv94_name - name of HP94xx csv datalog file to convert to rtdf format,
	#            this can be gzipped
    # rtdf_name - name to give to rtdf formatted file
	# csv94_dir - if HP94xx .csv file is in a different directory, this is the
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
	LotInfoFrame[["handler"]] <<- ""
	LotInfoFrame[["finish_t"]] <<- NaN

    Device_count <<- 0
    Parameter_count <<- 0
	Parameters_Names <<- NA
    Wafer_count <<- 0
    #Hbin_count <<- 0
    #Sbin_count <<- 0
	ResultsMatrix <<- array(NaN, dim=c(0,0))

    Pin_names <<- NA


    # if filenames not defined, prompt for them
    #------------------------------------------
    if (csv94_name == "") {
        csv94_name <- readline("Enter the name of the HP94xx csv datalog file to read: ")
    }

    if (rtdf_name == "") {
        rtdf_name <- readline("Enter the name of the Rdata file to write: ")
    }


	# load csv file into memory
	#-----------------------------
	if (csv94_dir != "") {
		my_dir = getwd()
		setwd(csv94_dir)
	}
	csv_top = read.csv(csv94_name,header=FALSE,colClasses="character",nrows=7)
	csv_main = read.csv(csv94_name,header=FALSE,skip=8)
	if (csv94_dir != "")  setwd(my_dir)

	csv_size = dim(csv_main)
	rows = csv_size[1]
	cols = csv_size[2]


	# extract LotInfoFrame information
	#---------------------------------
	LotInfoFrame[["program"]] <<- as.character(csv_top[1,3])
	LotInfoFrame[["tester_type"]] <<- "HP9472"


	# extract DevicesFrame information
	#---------------------------------
	Devices_part_id = as.numeric(csv_main[1:rows,1])
	Devices_soft_bin = as.numeric(csv_main[1:rows,2])
	Device_count = rows

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
    DevicesFrame[1:Device_count,"hard_bin"] <- Devices_soft_bin	# duplicated sbin...
    DevicesFrame[1:Device_count,"testtime"] <- NaN
    DevicesFrame[1:Device_count,"site"] <- 0


	# extract ParametersFrame information
	#------------------------------------
	my_seq = seq(6,cols,2)

	#browser()

	tnums = as.numeric(csv_top[1,my_seq])
	lls = as.numeric(csv_top[4,my_seq])
	uls = as.numeric(csv_top[3,my_seq])
	units = as.character(csv_top[5,my_seq])
	names = as.character(csv_top[7,my_seq])

	skip = which(names=="")		# indices to remove, not real tests... "JUMP_BIN"

	Parameters_testnum = tnums[-skip]
	Parameters_ll = lls[-skip]
	Parameters_ul = uls[-skip]
	Parameters_units = units[-skip]
	Parameters_Names = paste(names[-skip],tnums[-skip],sep="__")

	Parameter_count = length(Parameters_Names)

	my_list = list(testnum=NaN, testname="",
	                scaler=NaN, units="",
	                ll=NaN, ul=NaN,
	                plot_ll=NaN, plot_ul=NaN)
	ParametersFrame <- data.frame(rbind(my_list))
	ParametersFrame[1:Parameter_count,"testnum"] <- Parameters_testnum
	ParametersFrame[1:Parameter_count,"testname"] <- Parameters_Names
	ParametersFrame[1:Parameter_count,"scaler"] <- 0
	ParametersFrame[1:Parameter_count,"units"] <- Parameters_units
	ParametersFrame[1:Parameter_count,"ll"] <- Parameters_ll
	ParametersFrame[1:Parameter_count,"ul"] <- Parameters_ul
	ParametersFrame[1:Parameter_count,"plot_ll"] <- NaN
	ParametersFrame[1:Parameter_count,"plot_ul"] <- NaN


	# extract ResultsMatrix information
	#----------------------------------
	results = csv_main[1:rows,my_seq]
	ResultsMatrix <<- as.matrix(results[,-skip])
	# above creates a vector if only 1 device instead of a matrix, so below fixes this.
	if(!is.matrix(ResultsMatrix)) {
		ResultsMatrix <<- matrix(data=ResultsMatrix,nrow=rows,ncol=Parameter_count)
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



#############################################################################
#  copy local functions to the .Convert9470CSV.env and remove them
#  from the global environment
#############################################################################
environment(Convert9470CSV)<-.Convert9470CSV.env

