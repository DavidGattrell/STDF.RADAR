#  ConvertJ9.R
#
# $Id: ConvertJ9.R,v 1.4 2010/11/22 01:57:01 David Exp $
#
#  R script that reads in a J973 text datalog file and converts it into a
#  set of R data.frames/matrix aka rtdf format.
#
# Copyright (C) 2009-2010 David Gattrell
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
if(exists(".ConvertJ9.env")) rm(.ConvertJ9.env) 
.ConvertJ9.env <- new.env()

assign("Timestamp1",0.0,envir=.ConvertJ9.env)		# timer to suppress prints to screen to >5sec

assign("Device_count",0,envir=.ConvertJ9.env)		# number of devices processed (PIR/PRR records)
assign("Parameter_count",0,envir=.ConvertJ9.env)	# number of tests processed (PTR/FTR/MPR*pins records)

assign("Hbin_count",0,envir=.ConvertJ9.env)			# number of Hard bins processed (HBR records)
assign("Sbin_count",0,envir=.ConvertJ9.env)			# number of Soft bins processed (SBR records)
assign("Previous_param_i",0,envir=.ConvertJ9.env)	# the ParametersFrame index from the previous PTR/FTR/MPR
assign("Good_guesses",0,envir=.ConvertJ9.env)		# how often Previous_param_i+1 worked

assign("LotInfoFrame",data.frame(rbind(
		list( lotid="", sublotid="",
                start_t=NaN, program="",
                tester_type="", tester_id="",
                handler="")
		)),envir=.ConvertJ9.env)

assign("ResultsMatrix",array(NaN, dim=c(0,0)),envir=.ConvertJ9.env)

# due to speed issues, it is better to deal with vectors than data frames,
# so create the following vectors that duplicate data frame information
assign("Parameters_Names",NA,envir=.ConvertJ9.env)     # <<- as.character(ParametersFrame$testname)
assign("Parameters_testnum",NA,envir=.ConvertJ9.env)   # <<- as.integer(
assign("Parameters_scaler",NA,envir=.ConvertJ9.env)    # <<- as.integer(
assign("Parameters_units",NA,envir=.ConvertJ9.env)     # <<- as.character(
assign("Parameters_ll",NA,envir=.ConvertJ9.env)		# <<- as.numeric(
assign("Parameters_ul",NA,envir=.ConvertJ9.env)		# <<- as.numeric(
assign("Parameters_plot_ll",NA,envir=.ConvertJ9.env)	# <<- as.numeric(
assign("Parameters_plot_ul",NA,envir=.ConvertJ9.env)	# <<- as.numeric(

assign("Devices_part_id",NA,envir=.ConvertJ9.env)     # <<- as.character(DevicesFrame$part_id)
assign("Devices_temp",NA,envir=.ConvertJ9.env)        # <<- as.numeric(
assign("Devices_x_coord",NA,envir=.ConvertJ9.env)     # <<- as.integer(
assign("Devices_y_coord",NA,envir=.ConvertJ9.env)     # <<- as.integer(
assign("Devices_wafer_index",NA,envir=.ConvertJ9.env) # <<- as.integer(
assign("Devices_soft_bin",NA,envir=.ConvertJ9.env)    # <<- as.integer(
assign("Devices_hard_bin",NA,envir=.ConvertJ9.env)	   # <<- as.integer(
assign("Devices_testtime",NA,envir=.ConvertJ9.env)	   # <<- as.numeric(
assign("Devices_site",NA,envir=.ConvertJ9.env)		   # <<- as.numeric(


##########################################################################
ConvertJ9 <- function(j9_txt_name="",rtdf_name="",j9_dir="") {
    # j9_txt_name - name of j973 text datalog file to convert to rtdf format,
	#            this can be gzipped
    # rtdf_name - name to give to rtdf formatted file
	# j9_dir - if j9 file is in a different directory, this is the
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

	Device_count <<- 0
    Parameter_count <<- 0
	Parameters_Names <<- NA
    # Wafer_count <<- 0
    #Hbin_count <<- 0
    #Sbin_count <<- 0
	ResultsMatrix <<- array(NaN, dim=c(0,0))

    Pin_names <<- NA


    # if filenames not defined, prompt for them
    #------------------------------------------
    if (j9_txt_name == "") {
        j9_txt_name <- readline("Enter the name of the J973 datalog file to read: ")
    }

    if (rtdf_name == "") {
        rtdf_name <- readline("Enter the name of the Rdata file to write: ")
    }


    # open j9 file for reading...
    # suck whole file into memory
    #---------------------------------------
	if (j9_dir != "") {
		my_dir = getwd()
		setwd(j9_dir)
	}
    J9 <- gzfile(j9_txt_name,"r")
    J9txt <- readLines(J9)
    close(J9)
	if (j9_dir != "")  setwd(my_dir)
	lines <- length(J9txt)


	# parse J9txt lines...
	#--------------------------
	in_header=TRUE			# only extract header info from first device to keep parser fast
	for (line in 1:lines) {
        timestamp2 = proc.time()
        timestamp2 = timestamp2[3]
        if (timestamp2>(Timestamp1+5.0)) {
			Timestamp1 <<- timestamp2
            pct = 100.0 * line / lines
            cat(sprintf("processing Device %d ... ",Device_count+1))
            cat(sprintf(" %.1f%% through file \n",pct))
        }

		if (in_header) {
			# HEADER PART
			#===========================================================
			#if (length(grep("^Station",J9txt[line]))>0) {
			if (substr(J9txt[line],1,7)=="Station") {
				in_header=FALSE
			} else if (length(grep("D A T A L O G  R E P O R T",J9txt[line]))>0) {
				# next line has start time...
			} 
			# REVISIT, need to extract more header stuff...
		} else if (substr(J9txt[line],1,3)=="Bin") {
		#} else if (length(grep("^Bin",J9txt[line]))>0) {
			temp = strsplit(J9txt[line],"[[:space:]]+")[[1]]
			sbin = temp[2]
		} else if (substr(J9txt[line],1,7)=="Part id") {
		#} else if (length(grep("^Part id",J9txt[line]))>0) {
			temp = strsplit(J9txt[line],"[[:space:]]+")[[1]]
			part_id = temp[3]
			# now update DevicesFrame
			Devices_part_id[Device_count] <<- part_id
			Devices_soft_bin[Device_count] <<- sbin
			Devices_site[Device_count] <<- site
		} else if (substr(J9txt[line],1,5)=="Sites") {
		#} else if (length(grep("^Sites",J9txt[line]))>0) {
			temp = strsplit(J9txt[line],"[[:space:]]+")[[1]]
			site = temp[4]		# REVISIT.. when we have a multisite J9 example
		} else if (substr(J9txt[line],1,25)=="file_flowtable_Template: ") {
		#} else if (length(grep("^file_flowtable_Template: ",J9txt[line]))>0) {
			# starting a new device...
			add_device()
		} else if (length(grep("_Template: ",J9txt[line]))>0) {
			# starting a new test...
			temp = strsplit(J9txt[line],": ")[[1]]
			template = temp[1]
			testname = temp[2]
			test_num = 0
			lo_limit = NaN
			hi_limit = NaN
			scale_p10 = 0
			units = ""
			result = NaN
			multitest = FALSE	# is the test an MPR?, don't know yet.
		} else if (length(grep("^[[:space:]]*[0-9]+[.][[:space:]]+",J9txt[line]))>0) {
			valid = TRUE
			if (!multitest) {
				# check if next line is also a test...
				if (length(grep("^[[:space:]]*[0-9]+[.][[:space:]]+",J9txt[line+1]))>0) {
					multitest = TRUE
				}
			}
			my_line = sub("^[[:space:]]*","",J9txt[line])	# remove leading whitespace
			temp = strsplit(my_line,"@")[[1]]
			measurement = temp[1]
			if (length(temp)>1) {
				condition = temp[2]
			} else {
				condition = ""
			}
			
			# is measurement pass/fail?
			# can be one of:
			# 3400. PASS
			# or
			# 7000.  -2.500 uA <=           PASS <=   2.500 uA
			temp = strsplit(measurement,"[<=]+")[[1]]
			if (length(temp)>1) {
				tempA = sub("^[[:space:]]*","",temp[1])			# remove leading spaces
				tempA = strsplit(tempA,"[[:space:]]+")[[1]]		# split on white space
				test_num = tempA[1]
				lo_limit = as.numeric(tempA[2])
				units = tempA[3]

				tempC = sub("^[[:space:]]*","",temp[3])
				tempC = strsplit(tempC,"[[:space:]]+")[[1]]		# split on white space
				hi_limit = as.numeric(tempC[1])
				# units = tempC[2]

				tempB = sub("^[[:space:]]*","",temp[2])
				tempB = strsplit(tempB,"[[:space:]]+")[[1]]		# split on white space
				if (length(tempB)>1) {
					result = as.numeric(tempB[1])
					# units = tempB[2]
					flag = tempB[3]
				} else {
					# pass/fail, no measurement
					flag = tempB
					#if (flag=="PASS")	result = (lo_limit+hi_limit)/2.0
					#else				result = NaN
					result = NaN
				}

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
				
			} else {
				# 3400. PASS   # digital pattern result
				temp = sub("^[[:space:]]*","",temp)			# remove leading spaces
				temp = strsplit(temp,"[[:space:]]+")[[1]]	# split on white space
				test_num = temp[1]
				flag = temp[2]
				units = "fails"
				# if pass, value=0, else value = 128
				if (flag=="PASS")	result = 0
				else				result = 128
			}

			if(multitest) {
				# need to append /pinname to testname
				temp = sub("^[[:space:]]*","",condition)	# remove leading spaces
				temp = strsplit(temp,"[[:space:]]+")[[1]]
				if (length(temp)>3) {
					pinname = temp[4]
					if (pinname == "tt_null_pin")  valid = FALSE
					test_txt = paste(testname,"/",pinname,sep="")
				}
			} else {
				# need to append _tnum to testname
				# since sometimes template name is not unique...
				test_txt = paste(testname,"_",test_num,sep="")
			}

			if (valid) {
				# has this test been added to ParametersFrame yet?
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
				# REVISIT!!!!
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
environment(check_prefix)<-.ConvertJ9.env
assign("check_prefix",check_prefix,envir=.ConvertJ9.env)
rm(check_prefix)

#environment(update_LotInfoFrame)<-.ConvertJ9.env
#assign("update_LotInfoFrame",update_LotInfoFrame,envir=.ConvertJ9.env)
#rm(update_LotInfoFrame)

environment(add_device)<-.ConvertJ9.env
assign("add_device",add_device,envir=.ConvertJ9.env)
rm(add_device)



environment(ConvertJ9)<-.ConvertJ9.env

