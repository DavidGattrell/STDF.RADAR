#  ConvertKDF.R
#
# $Id: ConvertKDF.R,v 1.2 2011/02/18 02:16:58 David Exp $
#
#  R script that reads in a Keithley text datalog file and converts it into a
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
if(exists(".ConvertKDF.env")) rm(.ConvertKDF.env) 
.ConvertKDF.env <- new.env()

assign("Timestamp1",0.0,envir=.ConvertKDF.env)		# timer to suppress prints to screen to >5sec

assign("Device_count",0,envir=.ConvertKDF.env)		# number of devices processed (PIR/PRR records)
assign("Parameter_count",0,envir=.ConvertKDF.env)	# number of tests processed (PTR/FTR/MPR*pins records)
assign("Wafer_count",0,envir=.ConvertKDF.env)		# number of wafers processed (WIR/WRR records)

assign("Hbin_count",0,envir=.ConvertKDF.env)			# number of Hard bins processed (HBR records)
assign("Sbin_count",0,envir=.ConvertKDF.env)			# number of Soft bins processed (SBR records)
assign("Previous_param_i",0,envir=.ConvertKDF.env)	# the ParametersFrame index from the previous PTR/FTR/MPR
assign("Good_guesses",0,envir=.ConvertKDF.env)		# how often Previous_param_i+1 worked

assign("LotInfoFrame",data.frame(rbind(
		list( lotid="", sublotid="",
                start_t=NaN, program="",
                tester_type="", tester_id="",
                handler="")
		)),envir=.ConvertKDF.env)

assign("ResultsMatrix",array(NaN, dim=c(0,0)),envir=.ConvertKDF.env)

assign("WafersFrame",data.frame(rbind(
		list(wafer_id='', start_t=NaN,
					finish_t=NaN, part_cnt=NaN,
					good_cnt=NaN)
		)),envir=.ConvertKDF.env)

# due to speed issues, it is better to deal with vectors than data frames,
# so create the following vectors that duplicate data frame information
assign("Parameters_Names",NA,envir=.ConvertKDF.env)     # <<- as.character(ParametersFrame$testname)
assign("Parameters_testnum",NA,envir=.ConvertKDF.env)   # <<- as.integer(
assign("Parameters_scaler",NA,envir=.ConvertKDF.env)    # <<- as.integer(
assign("Parameters_units",NA,envir=.ConvertKDF.env)     # <<- as.character(
assign("Parameters_ll",NA,envir=.ConvertKDF.env)		# <<- as.numeric(
assign("Parameters_ul",NA,envir=.ConvertKDF.env)		# <<- as.numeric(
assign("Parameters_plot_ll",NA,envir=.ConvertKDF.env)	# <<- as.numeric(
assign("Parameters_plot_ul",NA,envir=.ConvertKDF.env)	# <<- as.numeric(

assign("Devices_part_id",NA,envir=.ConvertKDF.env)     # <<- as.character(DevicesFrame$part_id)
assign("Devices_temp",NA,envir=.ConvertKDF.env)        # <<- as.numeric(
assign("Devices_x_coord",NA,envir=.ConvertKDF.env)     # <<- as.integer(
assign("Devices_y_coord",NA,envir=.ConvertKDF.env)     # <<- as.integer(
assign("Devices_wafer_index",NA,envir=.ConvertKDF.env) # <<- as.integer(
assign("Devices_soft_bin",NA,envir=.ConvertKDF.env)    # <<- as.integer(
assign("Devices_hard_bin",NA,envir=.ConvertKDF.env)	   # <<- as.integer(
assign("Devices_testtime",NA,envir=.ConvertKDF.env)	   # <<- as.numeric(
assign("Devices_site",NA,envir=.ConvertKDF.env)		   # <<- as.numeric(


##########################################################################
ConvertKDF <- function(kdf_name="",rtdf_name="",kdf_dir="",klf_name="",klf_dir="") {
    # kdf_name - name of Keithley .kdf text datalog file to convert to rtdf format,
	#            this can be gzipped
    # rtdf_name - name to give to rtdf formatted file
	# kdf_dir - if Keithley .kdf file is in a different directory, this is the
	#			 absolute path to that directory
	# klf_name - name of Keithley .klf text limits file to use to properly
	#            populate the ParametersFrame units, limits info.
	# klf_dir - if Keithley .klf file is in a different directory, this is the
	#            absolute path to that directory
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


    # if filenames not defined, prompt for them, 
    # if klf file is empty assume one isn't available
    #------------------------------------------
    if (kdf_name == "") {
        kdf_name <- readline("Enter the name of the Keithley datalog file to read: ")
    }

    if (rtdf_name == "") {
        rtdf_name <- readline("Enter the name of the RTDF file to write: ")
    }


	# open .klf file for reading...
	#---------------------------------------
	if (klf_name != "") {
		if (klf_dir != "") {
			my_dir = getwd()
			setwd(klf_dir)
		}
		KLF <- gzfile(klf_name,"r")
		KLFtxt <- readLines(KLF)
		close(KLF)
		if (klf_dir != "")  setwd(my_dir)
		lines <- length(KLFtxt)
		
		# KLF format:
		#
		#  <EOH>  -- end of header separator line
		#  <EOL>  -- end of limits separator line
		#
		# The first lines up to the <EOH> contain header information:
		#  Version,21.0
		#  File,/path/filename.klf
		#  Date,02/24/2010   (month,date,year)
		#  Comment,blah blah blah
		#
		# - we'll just ignore all of this
		#
		# Each parameter will have a series of lines followed by a <EOL> line
		#  ID,BipN8x8_Bvces
		#  NAM,BipN8x8_Bvces
		#  UNT,V
		#  RPT,1
		#  CRT,4
		#  TAR, 0.0000e+00
		#  AF,0
		#  AL,0
		#  VAL,-1.0000e+16, 1.0000e+16
		#  SPC, 3.0000e+01, 1.5000e+02
		#  CNT,-1.0000e+16, 1.0000e+16
		#  ENG,-1.0000e+16, 1.0000e+16
		#  <EOL> 
		#
		# from this we'll extract...
		#  NAM -- get parameter name
		#  UNT -- get parameter units
		#  SPC -- get lower and upper limits
		#

		
		
		# parse KLFtxt lines...
		#--------------------------
		in_header=TRUE		# first lines should be header info.
		for (line in 1:lines) {
		    timestamp2 = proc.time()
		    timestamp2 = timestamp2[3]
		    if (timestamp2>(Timestamp1+5.0)) {
				Timestamp1 <<- timestamp2
		        pct = 100.0 * line / lines
		        cat(sprintf("processing Parameter %d ... ",Device_count+1))
		        cat(sprintf(" %.1f%% through klf file \n",pct))
		    }

			if(substr(KLFtxt[line],1,5)=="<EOH>") {
				in_header=FALSE
			} else if(substr(KLFtxt[line],1,5)=="<EOL>") {
				# save collected information to Parameters vectors
				if (Parameter_count>0) {
					if ((Previous_param_i < Parameter_count) &&
							(test_txt==Parameters_Names[Previous_param_i+1])) {
						par_index = Previous_param_i+1
						Good_guesses <<- Good_guesses + 1
					} else {
						par_index = match(test_txt,Parameters_Names,nomatch=0)
					}
			    } else {
			       par_index = 0
			    }
			    
				if (par_index<1) {
					Parameter_count <<- Parameter_count + 1
					par_index = Parameter_count

					Parameters_testnum[Parameter_count] <<- 0
		   			if (nchar(test_txt)<1) {
		   			    Parameters_Names[Parameter_count] <<- ''
		   			} else {
		   			    Parameters_Names[Parameter_count] <<- test_txt
		   			}
		   			Parameters_scaler[Parameter_count] <<- 0
		   			Parameters_units[Parameter_count] <<- units
					Parameters_ll[Parameter_count] <<- ll
		   			Parameters_ul[Parameter_count] <<- ul
		   			Parameters_plot_ll[Parameter_count] <<- NaN
		   			Parameters_plot_ul[Parameter_count] <<- NaN
	
		   			# we added a new parameter, so we need to add
		   			# a new column to Results Matrix...
		   			#---------------------------------------------
		   			#ResultsMatrix <<- cbind(ResultsMatrix,NaN)
		   			# the above gives warnings... need to do this better!!!
		   			ResultsMatrix <<- array(NaN, dim=c(0,Parameter_count))
		   			#browser()
	   			} else {
	   				# hey we have duplicate test names...
        			cat(sprintf("WARNING: duplicate test name in limits file >>%s<<\n",
                		test_txt))
	   			}
				Previous_param_i <<- par_index
				
				# clear variables
				test_txt = ""
				units = ""
				ll = NaN
				ul = NaN
			} else if(!in_header) {
				# collect the parameter information
				if(substr(KLFtxt[line],1,4)=="NAM,") {
					test_txt = sub("^NAM,","",KLFtxt[line])
					test_txt = sub("[[:space:]]*$","",test_txt)	# remove trailing whitespace
				} else if(substr(KLFtxt[line],1,4)=="UNT,") {
					units = sub("^UNT,","",KLFtxt[line])
					units = sub("[[:space:]]*$","",units)	# remove trailing whitespace
				} else if(substr(KLFtxt[line],1,4)=="SPC,") {
					temp = strsplit(KLFtxt[line],",")[[1]]
					ll = as.numeric(temp[2])
					ul = as.numeric(temp[3])
					if (ll==-1.0e16)  ll = NaN
					if (ul==1.0e16)  ul = NaN
				}
			}
		}
	}
	
	
    # open .kdf file for reading...
    # suck whole file into memory
    #---------------------------------------
	if (kdf_dir != "") {
		my_dir = getwd()
		setwd(kdf_dir)
	}
    KDF <- gzfile(kdf_name,"r")
    KDFtxt <- readLines(KDF)
    close(KDF)
	if (kdf_dir != "")  setwd(my_dir)
	lines <- length(KDFtxt)

	# KDF format:
	#  <EOH>  -- end of header separator line
	#  <EOW>  -- end of wafer separator line
	#  <EOS>  -- end of site separator line
	#
	# The first lines up to the <EOH> contain header information:
	#  TYP,part_type
	#  LOT,lot_name  
	#  PRC,process_id
	#  DEV,device_type?
	#  TST,tester_type?
	#  SYS,system_name?
	#  TSN,tester_serial_num?
	#  OPR,operator_name?
	#  STT,start_time
	#  LMT,??
	#  WDF,??
	#<EOH>
	#
	# each wafer begins with a line containing various info...
	#  wafer_num,??,??,??  -- just extract wafer number for now
	#
	#  followed by a number of sites followed by <EOW> line
	#
	# each site begins with a line containing various info...
	#  text_location,x_coord,y_coord,...  -- just extract x and y coords
	# followed by various test/parameter lines
	#  testname,value
	# and ends with the <EOS> line
	#
	
	
	
	# parse KDFtxt lines...
	#--------------------------
	in_header=TRUE		# first lines should be header info.
	in_wafer=FALSE	
	in_site=FALSE
	for (line in 1:lines) {
        timestamp2 = proc.time()
        timestamp2 = timestamp2[3]
        if (timestamp2>(Timestamp1+5.0)) {
			Timestamp1 <<- timestamp2
            pct = 100.0 * line / lines
            cat(sprintf("processing Device %d ... ",Device_count+1))
            cat(sprintf(" %.1f%% through file \n",pct))
        }

		if(substr(KDFtxt[line],1,5)=="<EOH>") {
			in_header=FALSE
			in_wafer=FALSE
			in_site=FALSE
		} else if(substr(KDFtxt[line],1,5)=="<EOW>") {
			in_header=FALSE
			in_wafer=FALSE
			in_site=FALSE
		} else if(substr(KDFtxt[line],1,5)=="<EOS>") {
			in_header=FALSE
			in_site=FALSE
		} else {
			if(in_header) {
				if(substr(KDFtxt[line],1,4)=="LOT,") {
					lotid = sub("^LOT,","",KDFtxt[line])
					lotid = sub("[[:space:]]*$","",lotid)	# remove trailing whitespace
					LotInfoFrame[["lotid"]] <<- lotid
				} else if(substr(KDFtxt[line],1,4)=="TST,") {
					tester_type = sub("^TST,","",KDFtxt[line])
					tester_type = sub("[[:space:]]*$","",tester_type)	# remove trailing whitespace
					LotInfoFrame[["tester_type"]] <<- tester_type
				} else if(substr(KDFtxt[line],1,4)=="SYS,") {
					tester_id = sub("^SYS,","",KDFtxt[line])
					tester_id = sub("[[:space:]]*$","",tester_id)	# remove trailing whitespace
					LotInfoFrame[["tester_id"]] <<- tester_id
				} else if(substr(KDFtxt[line],1,4)=="STT,") {
					start_t = sub("^STT,","",KDFtxt[line])
					start_t = sub("[[:space:]]*$","",start_t)	# remove trailing whitespace
					LotInfoFrame[["start_t"]] <<- start_t
				}
			} else if(in_site) {
				temp = strsplit(KDFtxt[line],",")[[1]]
				test_txt = temp[1]
				result = as.numeric(temp[2])

				#browser()
				
				if (Parameter_count>0) {
					if ((Previous_param_i < Parameter_count) &&
							(test_txt==Parameters_Names[Previous_param_i+1])) {
						par_index = Previous_param_i+1
						Good_guesses <<- Good_guesses + 1
					} else {
						par_index = match(test_txt,Parameters_Names,nomatch=0)
					}
			    } else {
			       par_index = 0
			    }
			    
				if (par_index<1) {
					Parameter_count <<- Parameter_count + 1
					par_index = Parameter_count

					Parameters_testnum[Parameter_count] <<- 0
		   			if (nchar(test_txt)<1) {
		   			    Parameters_Names[Parameter_count] <<- ''
		   			} else {
		   			    Parameters_Names[Parameter_count] <<- test_txt
		   			}
		   			Parameters_scaler[Parameter_count] <<- 0
		   			Parameters_units[Parameter_count] <<- ''
					Parameters_ll[Parameter_count] <<- NaN
		   			Parameters_ul[Parameter_count] <<- NaN
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

			} else if(in_wafer) {
				#if we are in_wafer, but not in_site, then this line must be site info...
				temp = strsplit(KDFtxt[line],",")[[1]]
				part_id = temp[1]
				x_coord = as.numeric(temp[2])
				y_coord = as.numeric(temp[3])
				
				add_device()
				Devices_part_id[Device_count] <<- part_id
				Devices_wafer_index[Device_count] <<- Wafer_count
				Devices_x_coord[Device_count] <<- x_coord
				Devices_y_coord[Device_count] <<- y_coord	
				
				in_site = TRUE			
			} else {
				#if we are not in_wafer, and not in_header, then this line must be wafer info...
				temp = strsplit(KDFtxt[line],",")[[1]]
				wafer_id = temp[1]

				Wafer_count <<- Wafer_count + 1
				my_list = list(wafer_id=wafer_id, start_t=NaN,
					finish_t=NaN,part_cnt=NaN,
					good_cnt=NaN)
				WafersFrame[Wafer_count,] <<- my_list
				
				in_wafer = TRUE
			}
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
#  copy local functions to the .ConvertKDF.env and remove them
#  from the global environment
#############################################################################
environment(add_device)<-.ConvertKDF.env
assign("add_device",add_device,envir=.ConvertKDF.env)
rm(add_device)



environment(ConvertKDF)<-.ConvertKDF.env

