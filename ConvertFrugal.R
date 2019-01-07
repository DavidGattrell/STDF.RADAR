#  ConvertFrugal.R
#
# $Id: ConvertFrugal.R,v 1.1 2010/11/23 01:20:07 David Exp $
#
#  R script that reads in a Frugal text datalog file and converts it into a
#  set of R data.frames/matrix aka rtdf format.
#
# Copyright (C) 2010 David Gattrell
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
if(exists(".ConvertFrugal.env")) rm(.ConvertFrugal.env) 
.ConvertFrugal.env <- new.env()

assign("Timestamp1",0.0,envir=.ConvertFrugal.env)		# timer to suppress prints to screen to >5sec

assign("Device_count",0,envir=.ConvertFrugal.env)		# number of devices processed (PIR/PRR records)
assign("Parameter_count",0,envir=.ConvertFrugal.env)	# number of tests processed (PTR/FTR/MPR*pins records)
assign("Wafer_count",0,envir=.ConvertFrugal.env)		# number of wafers processed (WIR/WRR records)

assign("Hbin_count",0,envir=.ConvertFrugal.env)			# number of Hard bins processed (HBR records)
assign("Sbin_count",0,envir=.ConvertFrugal.env)			# number of Soft bins processed (SBR records)
assign("Previous_param_i",0,envir=.ConvertFrugal.env)	# the ParametersFrame index from the previous PTR/FTR/MPR
assign("Good_guesses",0,envir=.ConvertFrugal.env)		# how often Previous_param_i+1 worked

assign("LotInfoFrame",data.frame(rbind(
		list( lotid="", sublotid="",
                start_t=NaN, program="",
                tester_type="", tester_id="",
                handler="")
		)),envir=.ConvertFrugal.env)

assign("ResultsMatrix",array(NaN, dim=c(0,0)),envir=.ConvertFrugal.env)

assign("WafersFrame",data.frame(rbind(
		list(wafer_id='', start_t=NaN,
					finish_t=NaN, part_cnt=NaN,
					good_cnt=NaN)
		)),envir=.ConvertFrugal.env)

# due to speed issues, it is better to deal with vectors than data frames,
# so create the following vectors that duplicate data frame information
assign("Parameters_Names",NA,envir=.ConvertFrugal.env)     # <<- as.character(ParametersFrame$testname)
assign("Parameters_testnum",NA,envir=.ConvertFrugal.env)   # <<- as.integer(
assign("Parameters_scaler",NA,envir=.ConvertFrugal.env)    # <<- as.integer(
assign("Parameters_units",NA,envir=.ConvertFrugal.env)     # <<- as.character(
assign("Parameters_ll",NA,envir=.ConvertFrugal.env)		# <<- as.numeric(
assign("Parameters_ul",NA,envir=.ConvertFrugal.env)		# <<- as.numeric(
assign("Parameters_plot_ll",NA,envir=.ConvertFrugal.env)	# <<- as.numeric(
assign("Parameters_plot_ul",NA,envir=.ConvertFrugal.env)	# <<- as.numeric(

assign("Devices_part_id",NA,envir=.ConvertFrugal.env)     # <<- as.character(DevicesFrame$part_id)
assign("Devices_temp",NA,envir=.ConvertFrugal.env)        # <<- as.numeric(
assign("Devices_x_coord",NA,envir=.ConvertFrugal.env)     # <<- as.integer(
assign("Devices_y_coord",NA,envir=.ConvertFrugal.env)     # <<- as.integer(
assign("Devices_wafer_index",NA,envir=.ConvertFrugal.env) # <<- as.integer(
assign("Devices_soft_bin",NA,envir=.ConvertFrugal.env)    # <<- as.integer(
assign("Devices_hard_bin",NA,envir=.ConvertFrugal.env)	   # <<- as.integer(
assign("Devices_testtime",NA,envir=.ConvertFrugal.env)	   # <<- as.numeric(
assign("Devices_site",NA,envir=.ConvertFrugal.env)		   # <<- as.numeric(


##########################################################################
ConvertFrugal <- function(frug_name="",rtdf_name="",frug_dir="") {
    # frug_name - name of frugal data .txt datalog file to convert to rtdf format,
	#            this can be gzipped
    # rtdf_name - name to give to rtdf formatted file
	# frug_dir - if frugal data file is in a different directory, this is the
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
    if (frug_name == "") {
        frug_name <- readline("Enter the name of the Frugal datalog file to read: ")
    }

    if (rtdf_name == "") {
        rtdf_name <- readline("Enter the name of the Rdata file to write: ")
    }


    # open .txt file for reading...
    # suck whole file into memory
    #---------------------------------------
	if (frug_dir != "") {
		my_dir = getwd()
		setwd(frug_dir)
	}
    FUCT <- gzfile(frug_name,"r")	# Frugal USB Controlled Tester
    FUCTtxt <- readLines(FUCT)
    close(FUCT)
	if (frug_dir != "")  setwd(my_dir)
	lines <- length(FUCTtxt)


	# parse FUCTtxt lines...
	#--------------------------
	updated_lotinfoframe=FALSE	# only extract header info from first device to keep parser fast
	program_updated=FALSE		# only extract header info from first device to keep parser fast
	updated_lotinfo_start_t=FALSE
	for (line in 1:lines) {
        timestamp2 = proc.time()
        timestamp2 = timestamp2[3]
        if (timestamp2>(Timestamp1+5.0)) {
			Timestamp1 <<- timestamp2
            pct = 100.0 * line / lines
            cat(sprintf("processing Device %d ... ",Device_count+1))
            cat(sprintf(" %.1f%% through file \n",pct))
        }

		if (substr(FUCTtxt[line],1,7)=="Tester:") {
			if(!updated_lotinfoframe) {
				temp = strsplit(FUCTtxt[line],"[[:space:]]+")[[1]]
				tester_id = temp[2]
				i_offset = 0
				if(tester_id=="LotID:") {
					tester_id = ""
					i_offset =  i_offset - 1
				}
				lotid = temp[4 + i_offset]
				if(lotid=="WaferID:") {
					lotid = ""
					i_offset = i_offset - 1
				}
				if(length(temp)>(5+i_offset)) {
					waferid = temp[6+i_offset]
					# for now, not making WafersFrame, 
					# assume single wafer per file
					LotInfoFrame[["sublotid"]] <<- waferid
				}
				LotInfoFrame[["tester_id"]] <<- tester_id
				LotInfoFrame[["tester_type"]] <<- "Frugal"
				LotInfoFrame[["lotid"]] <<- lotid
			}
		} else if (substr(FUCTtxt[line],1,8)=="Program:") {
			temp = strsplit(FUCTtxt[line],"[[:space:]]+")[[1]]
			if(!program_updated) {
				program = temp[2]
				LotInfoFrame[["program"]] <<- program
				program_updated=TRUE
			}
		} else if (substr(FUCTtxt[line],1,4)=="BIN:") {
			temp = strsplit(FUCTtxt[line],"[[:space:]]+")[[1]]
			sbin = temp[2]
			Devices_soft_bin[Device_count] <<- sbin
			# all the rest of the info is redundant, so we're done
		} else if (substr(FUCTtxt[line],1,7)=="Device:") {
			temp = strsplit(FUCTtxt[line],"[[:space:]]+")[[1]]
			part_id = temp[2]
			site = temp[4]
			# now update DevicesFrame
			add_device()
			Devices_part_id[Device_count] <<- part_id
			Devices_wafer_index[Device_count] <<- Wafer_count
			Devices_site[Device_count] <<- site
			if(length(temp)>7) {
				x_coord = temp[6]
				y_coord = temp[8]
				Devices_x_coord[Device_count] <<- x_coord
				Devices_y_coord[Device_count] <<- y_coord
			}
		} else if (substr(FUCTtxt[line],1,6)=="Start:") {
			temp = strsplit(FUCTtxt[line],"[[:space:]]+")[[1]]
			start_date = temp[2]
			start_time = temp[3]
			start_time_ms =  as.numeric(temp[4])
			if(!updated_lotinfo_start_t) {
				my_tz = paste(temp[-(1:5)],collapse=" ")
				# dang!  "Eastern Daylight Time" is not an R recognized string..
				# will need to cross patch to EST5EDT and flag any other ones
				# as they arise.
				ascii_t = paste(start_date,start_time)
				# takes the time, assumes current time zone and converts to specified tz..
				# but doesn't seem to take DST into account!
				unix_t = as.numeric(as.POSIXct(ascii_t,"%Y-%m-%d %H:%M:%S",tz="GMT"))
				# so check if it is DST, and shift the hour needed
				my_time = as.POSIXlt(unix_t,origin=ISOdatetime(1970,1,1,0,0,0))
				if(my_time[["isdst"]])  unix_t = unix_t - 3600
				LotInfoFrame[["start_t"]] <<- unix_t
				updated_lotinfo_start_t=TRUE
			}
			# add bogus parameter start time? or index time
		} else if (substr(FUCTtxt[line],1,5)=="Done:") {
			temp = strsplit(FUCTtxt[line],"[[:space:]]+")[[1]]
			done_date = temp[2]
			done_time = temp[3]
			done_time_ms =  as.numeric(temp[4])
			# figure out elapsed time and update DevicesFrame field
			# .. assume time is <1hours... just look at mins, secs, msecs
			temp = strsplit(start_time,":")[[1]]
			start_min = as.numeric(temp[2])
			start_sec =  as.numeric(temp[3])
			starting = start_min*60 + start_sec + start_time_ms/1000.0
			temp = strsplit(done_time,":")[[1]]
			done_min =  as.numeric(temp[2])
			done_sec =  as.numeric(temp[3])
			ending = done_min*60 + done_sec + done_time_ms/1000.0
			if(ending<starting) ending= ending + 3600	# add an hour
			Devices_testtime[Device_count] <<- ending - starting
			# add bogus parameter done time?
		} else if (length(grep("^[[:space:]]*[0-9]+[[:space:]]+",FUCTtxt[line]))>0) {
			# reset variables...
			test_num = -1
			testname = ""
			units = ""
			lo_limit = NaN
			hi_limit = NaN
			result = NaN
			scale_p10 = 0
			scaler = 1
			
			# remove leading spaces...
			my_line = sub("^[[:space:]]*","",FUCTtxt[line])
			temp = strsplit(my_line,"[[:space:]]+")[[1]]
			if(temp[3]=="Pass") {
				# digital pattern test
				#---------------------
				test_num = temp[1]
				testname = temp[2]
				result = 0	# fails count
				units = "_fails"
			} else if (temp[3]=="Fail") {
				# digital pattern test
				#---------------------
				test_num = temp[1]
				testname = temp[2]
				result = 1	# fails count
				units = "_fails"
			} else {
				# parametric test
				#---------------
				test_num = temp[1]
				testname = temp[2]
				check_lims = gregexpr("<",(FUCTtxt[line]))[[1]]
				if(check_lims[1]<0) {
					# no limits
					do_ll = 0
					do_ul = 0
				} else if(length(check_lims)>1) {
					# ll and ul
					do_ll = 1
					do_ul = 1
				} else if(check_lims[1]<47) {
					# just ll 
					do_ll = 1
					do_ul = 0
				} else {
					# just ul
					do_ll = 0
					do_ul = 1
				}
				idx=3
				if(do_ll) {
					lo_limit = as.numeric(temp[idx])
					if(temp[idx+1]!="<")  cat("Oh dear... LL\n")
					idx = idx + 2
				}
				result = as.numeric(temp[idx])
				idx = idx + 1
				if((idx<=length(temp)) && (temp[idx]=="F"))  idx = idx + 1
				if(do_ul) {
					if(temp[idx]!="<")  cat("Oh dear... UL\n")
					hi_limit = as.numeric(temp[idx+1])
					idx = idx + 2
				}
				if(idx<=length(temp)) {
					# grab units
					units = temp[idx]
					my_chars = strsplit(units,"")[[1]]
					end = length(my_chars)
					my_scale = check_prefix(units)	
					if (is.finite(my_scale)) {
						scale_p10 = my_scale
						scaler = 10^(scale_p10)
						units = paste(my_chars[2:end],sep="",collapse="")
					} else {
						scaler = 1
						scale_p10 = 0
					}
					lo_limit = lo_limit * scaler
					hi_limit = hi_limit * scaler
					result = result * scaler
				}
			}
				
			test_txt = testname

			if (Parameter_count>0) {
			    if (nchar(test_txt)<1) {
		             par_index = match(test_num,Parameters_testnum,nomatch=0)
		        } else {
					if ((Previous_param_i < Parameter_count) &&
							(test_txt==Parameters_Names[Previous_param_i+1])) {
						par_index = Previous_param_i+1
						Good_guesses <<- Good_guesses + 1
					} else {
						par_index = match(test_txt,Parameters_Names,nomatch=0)
					}
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
       			ResultsMatrix <<- cbind(ResultsMatrix,NaN)
   			}
	
       		ResultsMatrix[Device_count,par_index] <<- result

			Previous_param_i <<- par_index
		}
			
	}
	# update finish_t each part...
	ascii_t = paste(done_date,done_time)
	unix_t = as.numeric(as.POSIXct(ascii_t,"%Y-%m-%d %H:%M:%S",tz="GMT"))
	#my_tz = Sys.timezone()	# doesn't work on linux...
	my_time = as.POSIXlt(Sys.time())
	if(my_time[["isdst"]])  unix_t = unix_t - 3600
	LotInfoFrame[["finish_t"]] <<- unix_t


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


	# save Rtdf file
    #-----------------
    my_list = c("LotInfoFrame","ParametersFrame","DevicesFrame","ResultsMatrix")

	
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
#  copy local functions to the .ConvertEDL.env and remove them
#  from the global environment
#############################################################################
environment(check_prefix)<-.ConvertFrugal.env
assign("check_prefix",check_prefix,envir=.ConvertFrugal.env)
rm(check_prefix)

#environment(update_LotInfoFrame)<-.ConvertFrugal.env
#assign("update_LotInfoFrame",update_LotInfoFrame,envir=.ConvertFrugal.env)
#rm(update_LotInfoFrame)

environment(add_device)<-.ConvertFrugal.env
assign("add_device",add_device,envir=.ConvertFrugal.env)
rm(add_device)



environment(ConvertFrugal)<-.ConvertFrugal.env

