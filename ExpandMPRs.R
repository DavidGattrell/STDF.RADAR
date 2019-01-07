# ExpandMPRs.R
#
# $Id: ExpandMPRs.R,v 1.2 2009/02/24 02:49:24 David Exp $
#
# R script to parse through stdf file and replace any MPR records
# with multiple PTR records
#
# Copyright (C) 2009 David Gattrell
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

# namespace for the function... main function Global variables and local functions
#-------------------------------------------------------
if(exists(".ExpandMPRs.env")) rm(.ExpandMPRs.env) 
.ExpandMPRs.env <- new.env()

assign("Verbose0",FALSE,envir=.ExpandMPRs.env)    # print per record info
assign("Verbose2",FALSE,envir=.ExpandMPRs.env)    # print per record info

assign("Timestamp1",0.0,envir=.ExpandMPRs.env)    # timer to suppress prints to screen to >5sec

assign("Device_count",0,envir=.ExpandMPRs.env)		# number of devices processed (PIR/PRR records)

assign("Executive_type",'',envir=.ExpandMPRs.env)	# "enVision" or "Image" or ...
assign("Inc_tnums",0,envir=.ExpandMPRs.env)			# see input parameter inc_tnums
assign("Pin_names",NA,envir=.ExpandMPRs.env)		# array of strings extracted from PMRs, used by MPRs
assign("Endy","big",envir=.ExpandMPRs.env)			# initial guess at Endian type of stdf file
assign("Stdf",raw(0),envir=.ExpandMPRs.env)		# vector containing stdf file contents
assign("Stdf_size",0,envir=.ExpandMPRs.env)		# number of bytes in stdf stream
assign("Ptr",1,envir=.ExpandMPRs.env)				# pointer to next position in Stdf (vector of raw)

assign("New_Stdf",raw(0),envir=.ExpandMPRs.env)	# vector containing new stdf file contents
assign("New_Ptr",1,envir=.ExpandMPRs.env)			# pointer to next position in New_Stdf (vector of raw)


##########################################################################
ExpandMPRs <- function(stdf_name="",new_stdf="",inc_tnums=50,in_dir="") {
    # stdf_name - name of stdf file to modify (if the file is gzipped,
	#             this script will deal with it transparently
    # new_stdf - name to give to generated stdf file that has MPR records
	#             replaced with multiple PTR records.  NOTE: if the
	#             name provided ends in .gz, this script will save it
	#             gzipped.
	# inc_tnums - if set to 0, PTRs inherit the MPR test number, otherwise,
	#             the tnum is incremented for each PTR.  A Warning is 
	#             generated if the tnum incrementing goes beyond the value
	#             of inc_tnums
	# in_dir - if input stdf file is in a different directory, this is the
	#			  absolute path to that directory
    #---------------------------------------------

    timestamp0 = proc.time()
    timestamp0 = timestamp0[3]  # start time
    Timestamp1 <<- timestamp0   # time since last screen write


	if (stdf_name == "") {
		stdf_name <- readline("Enter the name of the STDF file to read: ")
    }

    if (new_stdf == "") {
		new_stdf <- readline("Enter the name of the new STDF file to write: ")
    }



    # initialize Global variables
    #------------------------------
	Inc_tnums <<- inc_tnums
    Pin_names <<- NA
    Device_count <<- 0


	# suck in whole file to memory...
	# if .gz, guess at 95% compression...
    #-----------------------------------------------------
	if (in_dir != "") {
		my_dir = getwd()
		setwd(in_dir)
	}
    STDF <- gzfile(stdf_name,"rb")
    raw_bytes <- file.info(stdf_name)[["size"]]
    if (regexpr("gz$",stdf_name)>0) {
        read_bytes = raw_bytes * 30
    } else {
        read_bytes = raw_bytes + 1      # try to read more bytes than exist...
    }
    Stdf <<- readBin(STDF,raw(),n=read_bytes)
    Ptr <<- 1
    in_bytes = length(Stdf)
    Stdf_size <<- in_bytes
    if (in_bytes >= read_bytes) {
        # we didn't read in the entire file... we need to read in more

        # while loop adding another raw_bytes until
        # it is 
        # REVISIT!!!
        # should really query user if he wants to continue..
        # and after a few more reads, prompt again...
        #
        cat(sprintf("input stdf file is longer than expected!\n"))
    }
    close(STDF)
	if (in_dir != "")  setwd(my_dir)


	# open up output file either as regular stdf or as 
	# a compressed .stdf.gz file
	#--------------------------------------------------------------
	if(regexpr("gz$",new_stdf)>0) {
		NEW <- gzfile(new_stdf,"wb")
	} else {
		NEW <- file(new_stdf,"wb")
	}
	New_Stdf <<- raw(0)		# should allocate a big section of memory
							# to make it faster!  allocate 6Mbyte,
							# and flush to file once it goes above
							# 5Mbyte...
	New_Stdf[6e6] <<- as.raw(0)
    New_Ptr <<- 1


    # guess at the endian-ness of the stdf file
    #-------------------------------------------
    Endy <<- "big"      # initial guess.  The first record should be 
                        # FAR, which will tell us if we are big or
                        # little.  If there is no FAR record, then
                        # the initial guess will be used.
                        # STDF files from Sun boxes, etc are big
                        # STDF files from Intel boxes, etc are little


    # parse STDF records...
    #------------------------
    rec_len = readBin(Stdf[Ptr:(Ptr+1)],integer(),n=1,size=2,endian=Endy,signed=FALSE)
	New_Stdf[New_Ptr:(New_Ptr+1)] <<- Stdf[Ptr:(Ptr+1)]
    Ptr <<- Ptr+2
    New_Ptr <<- New_Ptr+2
    while ((Ptr+rec_len) <= in_bytes) {
        rec_typ = readBin(Stdf[Ptr],integer(),n=1,size=1,signed=FALSE)
        rec_sub = readBin(Stdf[Ptr+1],integer(),n=1,size=1,signed=FALSE)
		New_Stdf[New_Ptr:(New_Ptr+1)] <<- Stdf[Ptr:(Ptr+1)]
        Ptr <<- Ptr+2
	    New_Ptr <<- New_Ptr+2
        # for debugging
        #cat(sprintf("rec_len is %d , rec_typ is %d, rec_sub is %d \n", 
        #           rec_len, rec_typ, rec_sub))

        parse_stdf_record(rec_typ,rec_sub,rec_len,Endy) # also uses Stdf,Ptr
        
        if ((Ptr+2) < in_bytes) {
			# first check to see if we have over 5Mbytes of generated stdf data,
			# if so, then dump it to the file and reset the New_Ptr
			if(New_Ptr>5e6) {
				cat(sprintf("... dumping 5Mbyte chunk to output file ...\n"))
				writeBin(New_Stdf[1:(New_Ptr-1)],NEW)
				New_Ptr <<- 1
			}
            rec_len = readBin(Stdf[Ptr:(Ptr+1)],integer(),n=1,size=2,endian=Endy,signed=FALSE)
			New_Stdf[New_Ptr:(New_Ptr+1)] <<- Stdf[Ptr:(Ptr+1)]
            Ptr <<- Ptr+2
		    New_Ptr <<- New_Ptr+2
        } else {
            Ptr <<- in_bytes + 1    # stop processing the stdf stream.
        }
    }

    writeBin(New_Stdf[1:(New_Ptr-1)],NEW)
    close(NEW)

    timestamp9 = proc.time()
    timestamp9 = timestamp9[3] - timestamp0
    if (timestamp9<200.0) {
        cat(sprintf("Conversion Finished! \n processed %d Devices in %.2f seconds\n",
                Device_count,timestamp9))
    } else {
        cat(sprintf("Conversion Finished! \n processed %d Devices in %.2f minutes\n",
                Device_count,timestamp9/60.0))
    }

}


###############################################################################
parse_stdf_record <- function(rec_typ,rec_sub,...) {

    if (rec_typ == 0) {     # per file information
        if (rec_sub == 10) {
            if (Verbose2) cat("processing FAR record... \n")
            parse_FAR_record(...)
        } else if (rec_sub == 20) {
            #if (Verbose2) cat("processing ATR record... \n")
            # parse_ATR_record(...)
            if (Verbose2) cat("copying ATR record... \n")
            copy_STDF_record(...)
        } else {
            if (Verbose2) cat(sprintf(
                "copying unknown record: type %d subtype %d... \n",
                rec_typ, rec_sub))
            copy_STDF_record(...)
        }
    } else if (rec_typ == 1) {  # per lot information
        if (rec_sub == 10) {
            if (Verbose2) cat("processing MIR record... \n")
            parse_MIR_record(...)
        } else if (rec_sub == 20) {
            if (Verbose2) cat("processing MRR record... \n")
            copy_STDF_record(...)
        } else if (rec_sub == 30) {
            #parse_PCR_record(...)
            if (Verbose2) cat("skipping PCR record... \n")
            copy_STDF_record(...)
        } else if (rec_sub == 40) {
            copy_STDF_record(...)
            #if (Verbose2) cat("skipping HBR record... \n")
            #ignore_STDF_record(...)
        } else if (rec_sub == 50) {
            copy_STDF_record(...)
            #if (Verbose2) cat("skipping SBR record... \n")
            #ignore_STDF_record(...)
        } else if (rec_sub == 60) {
            if (Verbose2) cat("processing PMR record... \n")
            parse_PMR_record(...)
            #if (Verbose2) cat("skipping PMR record... \n")
            #ignore_STDF_record(...)
        } else if (rec_sub == 62) {
            #parse_PGR_record(...)
            if (Verbose2) cat("skipping PGR record... \n")
            copy_STDF_record(...)
        } else if (rec_sub == 63) {
            #parse_PLR_record(...)
            if (Verbose2) cat("skipping PLR record... \n")
            copy_STDF_record(...)
        } else if (rec_sub == 70) {
            #parse_RDR_record(...)
            if (Verbose2) cat("skipping RDR record... \n")
            copy_STDF_record(...)
        } else if (rec_sub == 80) {
            #parse_SDR_record(...)
            if (Verbose2) cat("skipping SDR record... \n")
            copy_STDF_record(...)
        } else {
            if (Verbose2) cat(sprintf(
                "skipping unknown record: type %d subtype %d... \n", 
                rec_typ, rec_sub))
            copy_STDF_record(...)
        }
    } else if (rec_typ == 2) {  # per wafer information
        if (rec_sub == 10) {
            if (Verbose2) cat("processing WIR record... \n")
            copy_STDF_record(...)
            #if (Verbose2) cat("skipping WIR record... \n")
            #ignore_STDF_record(...)
        } else if (rec_sub == 20) {
            if (Verbose2) cat("processing WRR record... \n")
            copy_STDF_record(...)
            #if (Verbose2) cat("skipping WRR record... \n")
            #ignore_STDF_record(...)
        } else if (rec_sub == 30) {
            if (Verbose2) cat("processing WCR record... \n")
            copy_STDF_record(...)
            #if (Verbose2) cat("skipping WCR record... \n")
            #ignore_STDF_record(...)
        } else {
            if (Verbose2) cat(sprintf(
                "skipping unknown record: type %d subtype %d... \n",
                rec_typ, rec_sub))
            copy_STDF_record(...)
        }
    } else if (rec_typ == 5) {  # per part information
        if (rec_sub == 10) {
            if (Verbose0 || Verbose2) {
                cat(sprintf("processing PIR record %d ... \n",Device_count+1))
            } else {
                timestamp2 = proc.time()
                timestamp2 = timestamp2[3]
                if (timestamp2>(Timestamp1+5.0)) {
                    Timestamp1 <<- timestamp2
                    pct = 100.0 * Ptr / Stdf_size
                    cat(sprintf("processing PIR record %d ... ",Device_count+1))
                    cat(sprintf(" %.1f%% through file \n",pct))
                }
            }
			Device_count <<- Device_count + 1
            copy_STDF_record(...)
        } else if (rec_sub == 20) {
            if (Verbose2) cat("processing PRR record... \n")
            copy_STDF_record(...)
        } else {
            if (Verbose2) cat(sprintf(
                "skipping unknown record: type %d subtype %d... \n",
                rec_typ, rec_sub))
            copy_STDF_record(...)
        }
    } else if (rec_typ == 10) { # per test information
        if (rec_sub == 30) {
            if (Verbose2) cat("processing TSR record... \n")
            #parse_TSR_record(...)
			copy_STDF_record(...)
        } else {
            if (Verbose2) cat(sprintf(
                "skipping unknown record: type %d subtype %d... \n",
                rec_typ, rec_sub))
            copy_STDF_record(...)
        }
    } else if (rec_typ == 15) { # per test execution information
        if (rec_sub == 10) {
            if (Verbose2) cat("processing PTR record... \n")
            copy_STDF_record(...)
        } else if (rec_sub == 15) {
            if (Verbose2) cat("processing MPR record... \n")
            parse_MPR_record(...)  
        } else if (rec_sub == 20) {
            if (Verbose2) cat("processing FTR record... \n")
            copy_STDF_record(...)
        } else {
            if (Verbose2) cat(sprintf(
                "skipping unknown record: type %d subtype %d... \n",
                rec_typ, rec_sub))
            copy_STDF_record(...)
        }
    } else if (rec_typ == 20) { # per program segment information
        copy_STDF_record(...)
    } else if (rec_typ == 50) { # generic data
        copy_STDF_record(...)
    } else {                    # unknown record type
        copy_STDF_record(...)
    }

}


###############################################################################
parse_FAR_record <- function(rec_len,endy) {
	# verify endian is correct,
	# copy record to new stdf stream,
	# append new ATR record
	#-----------------------------------

	cpu_type = 0
    stdf_ver = 0

    # make sure we have the correct endian...
    #----------------------------------------
    if (rec_len == 512) {
        if (endy == "big")  Endy <<- "little"  else  Endy <<- "big"
        rec_len = 2
    }

	# push FAR record to new stdf
	#------------------------------
	New_Stdf[New_Ptr:(New_Ptr+rec_len-1)] <<- Stdf[Ptr:(Ptr+rec_len-1)]
    Ptr <<- Ptr + rec_len
	New_Ptr <<- New_Ptr + rec_len
    rec_len = 0

	# add ATR record
	#----------------
	rec_typ = 0
	rec_sub = 20
	mod_tim = as.integer(24*60*60*as.numeric(Sys.time() - ISOdatetime(1970,1,1,0,0,0)) + 0.5)
	cmd_line = "ExpandMPRs  # http://stdf.radar.googlepages.com"
	x = nchar(cmd_line)
	rec_len = x+5
	New_Stdf[New_Ptr:(New_Ptr+1)] <<- writeBin(as.integer(rec_len),raw(),size=2,endian=Endy)
	New_Stdf[New_Ptr+2] <<- writeBin(as.integer(rec_typ),raw(),size=1)
	New_Stdf[New_Ptr+3] <<- writeBin(as.integer(rec_sub),raw(),size=1)
	New_Stdf[(New_Ptr+4):(New_Ptr+7)] <<- writeBin(as.integer(mod_tim),raw(),size=4,endian=Endy)
	temp <- writeBin(cmd_line,raw(),size=1,endian=Endy)
	New_Stdf[New_Ptr+8] <<- writeBin(as.integer(x),raw(),size=1)
	New_Stdf[(New_Ptr+9):(New_Ptr+x+8)] <<- temp[1:x]
	New_Ptr <<- New_Ptr + 9 + x

}



###############################################################################
copy_STDF_record <- function(rec_len,endy) {

	New_Stdf[New_Ptr:(New_Ptr+rec_len-1)] <<- Stdf[Ptr:(Ptr+rec_len-1)]
    Ptr <<- Ptr + rec_len
	New_Ptr <<- New_Ptr + rec_len
    rec_len = 0
}


###############################################################################
parse_MIR_record <- function(rec_len,endy) {

	New_Stdf[New_Ptr:(New_Ptr+rec_len-1)] <<- Stdf[Ptr:(Ptr+rec_len-1)]
	New_Ptr <<- New_Ptr + rec_len

	# need to parse the MIR record to determine executive type,
	# so we know which field in the PMR we should use in the MPRs

    # initialize variables
    setup_t = 0
    start_t = 0
    stat_num = 0
    mode_cod = ' '
    rtst_cod = ' '
    prot_cod = ' '
    burn_tim = 0
    cmod_cod = ' '
    lot_id = ''
    part_typ = ''
    node_nam = ''
    tstr_typ = ''
    job_nam = ''
    job_rev = ''
    sblot_id = ''
    oper_nam = ''
    exec_typ = ''
    exec_ver = ''
    test_cod = ''
    tst_temp = ''
    user_txt = ''
    aux_file = ''
    pkg_typ = ''
    famly_id = ''
    date_cod = ''
    facil_id = ''
    floor_id = ''
    proc_id = ''
    oper_frq = ''
    spec_nam = ''
    spec_ver = ''
    flow_id = ''
    setup_id = ''
    dsgn_rev = ''
    eng_id = ''
    rom_cod = ''
    serl_num = ''
    supr_nam = ''

    valid_record = TRUE     

    if (rec_len<15) {  
        cat("WARNING: MIR record shorter than expected \n")
        valid_record = FALSE
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } else {
        setup_t = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        start_t = readBin(Stdf[(Ptr+4):(Ptr+7)],integer(),n=1,size=4,endian=endy,signed=FALSE)
        stat_num = readBin(Stdf[(Ptr+8)],integer(),n=1,size=1,signed=FALSE)
        Ptr <<- Ptr + 9
        mode_cod = rawToChar(Stdf[Ptr])
        rtst_cod = rawToChar(Stdf[Ptr+1])
        prot_cod = rawToChar(Stdf[Ptr+2])
        Ptr <<- Ptr + 3
        burn_tim = readBin(Stdf[Ptr:(Ptr+1)],integer(),n=1,size=2,endian=endy,signed=FALSE)
        Ptr <<- Ptr + 2
        cmod_cod = rawToChar(Stdf[Ptr])
        Ptr <<- Ptr + 1
        rec_len = rec_len - 15
    }

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        lot_id = str_list$string
        rec_len = str_list$bytes_left
    }

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        part_typ = str_list$string
        rec_len = str_list$bytes_left
    }

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        node_nam = str_list$string
        rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        tstr_typ = str_list$string
        rec_len = str_list$bytes_left
        cat(sprintf("Tester Type is: %s \n",tstr_typ))
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        job_nam = str_list$string
        rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        job_rev = str_list$string
        rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        sblot_id = str_list$string
        rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        oper_nam = str_list$string
        rec_len = str_list$bytes_left
    } 

    if (rec_len >0) {
        str_list = readSTDFstring(rec_len)
        exec_typ = str_list$string
        rec_len = str_list$bytes_left
    } 
    Executive_type <<- exec_typ
    if (substr(Executive_type,1,6)=='IMAGE ') {   
        Executive_type <<- 'Image'
    }
    if (substr(tstr_typ,1,5)=='93000') {   
        Executive_type <<- '93000'      # could probably do same for 83000...
    }
    if (tstr_typ=='Credence') {
        Executive_type <<- 'Credence'
    }
	
    # throw away any remaining portion of the record we aren't interested in...
    #--------------------------------------------------------------------------
    if (rec_len > 0) {
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } 
}


###############################################################################
parse_PMR_record <- function(rec_len,endy) {

	New_Stdf[New_Ptr:(New_Ptr+rec_len-1)] <<- Stdf[Ptr:(Ptr+rec_len-1)]
	New_Ptr <<- New_Ptr + rec_len


    # initialize variables
    pmr_indx = NaN
    chan_typ = NaN
    chan_nam = ''
    phy_nam = ''
    log_nam = ''
    head_num = 0
    site_num = 0

    valid_record = TRUE

    if (rec_len<4) {
        valid_record = FALSE
        cat("WARNING: PMR record shorter than expected \n")
        #bit_bucket = readBin(STDF,integer(),n=rec_len,size=1)
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } else {
        pmr_indx = readBin(Stdf[Ptr:(Ptr+1)],integer(),n=1,size=2,endian=endy,signed=FALSE)
        chan_typ = readBin(Stdf[(Ptr+2):(Ptr+3)],integer(),n=1,size=2,endian=endy,signed=FALSE)
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    }

    if (rec_len>0) {
        str_list = readSTDFstring(rec_len)
        chan_nam = str_list$string
        rec_len = str_list$bytes_left   
    }

    if (rec_len>0) {
        str_list = readSTDFstring(rec_len)
        phy_nam = str_list$string
        rec_len = str_list$bytes_left   
    }

    if (rec_len>0) {
        str_list = readSTDFstring(rec_len)
        log_nam = str_list$string
        rec_len = str_list$bytes_left   
    }

    if (rec_len>0) {
        head_num = as.integer(Stdf[Ptr])
        Ptr <<- Ptr + 1
        rec_len = rec_len - 1
    }

    if (rec_len>0) {
        site_num = as.integer(Stdf[Ptr])
        Ptr <<- Ptr + 1
        rec_len = rec_len - 1
    }


    # throw away any remaining portion of the record...
    #--------------------------------------------------------------------------
    if (rec_len > 0) {
        cat(sprintf("WARNING: PMR record longer than expected \n"))
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } 

    
    # now update pinname_xref for MPR records
    #----------------------------------------------------
    if (valid_record) {

        #  chan_nam   phy_nam   log_nam
        #   pin name  resource   x        : 93k  (HP/Agilent/Verigy)
        #   x         x          pin name : quartet (Credence)
        #   resource  pin_name   x        : HFi (LTX Fusion)
        if (Executive_type=="93000") {
            Pin_names[pmr_indx] <<- chan_nam
        } else if (Executive_type=="enVision") {
            Pin_names[pmr_indx] <<- phy_nam
        } else {
            Pin_names[pmr_indx] <<- log_nam
        }
    }
}


###############################################################################
parse_MPR_record <- function(rec_len,endy) {

    # initialize variables
    test_num = 0
    head_num = 0
    site_num = 0
    test_flg = 0
    parm_flg = 0
    rnt_icnt = 0
    rslt_cnt = 0
    rtn_stat = 0
    rtn_rslt = NaN
    test_txt = ''
    alarm_id = ''

    opt_flag = 0
    res_scal = 0
    llm_scal = 0
    hlm_scal = 0
    lo_limit = 0
    hi_limit = 0
    start_in = 0.0
    incr_in = 0.0
    rtn_indx = 0
    units = ''
    units_in = ''
    c_resfmt = ''
    c_llmfmt = ''
    c_hlmfmt = ''
    lo_spec = 0
    hi_spec = 0


    valid_record = TRUE
    valid_opt_flag = FALSE
	short_mpr = TRUE
	specs_valid = FALSE
	sweep_mpr = FALSE	# MPR can be multi-pin or sweep
	sweep_rec_len = rec_len
	
	partA = raw(0)
	partB = raw(0)
	partC = raw(0)
	
    if (rec_len<12) {
        valid_record = FALSE
        cat("WARNING: MPR record shorter than expected \n")
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } else {
        test_num = readBin(Stdf[Ptr:(Ptr+3)],integer(),n=1,size=4,endian=endy,signed=FALSE)
		if (Inc_tnums>0)  partA = Stdf[(Ptr+4):(Ptr+7)]		# upto RESULT field
		else  partA = Stdf[Ptr:(Ptr+7)]		# upto RESULT field
        Ptr <<- Ptr + 4
        head_num = as.integer(Stdf[Ptr])
        site_num = as.integer(Stdf[Ptr+1])
        test_flg = as.integer(Stdf[Ptr+2])
        parm_flg = as.integer(Stdf[Ptr+3])
        Ptr <<- Ptr + 4
        rtn_icnt = readBin(Stdf[Ptr:(Ptr+1)],integer(),n=1,size=2,endian=endy,signed=FALSE)
        rslt_cnt = readBin(Stdf[(Ptr+2):(Ptr+3)],integer(),n=1,size=2,endian=endy,signed=FALSE)
        Ptr <<- Ptr + 4
        rec_len = rec_len - 12
    }
	if(rtn_icnt<1)  sweep_mpr=TRUE
    j=rtn_icnt
    j2 = as.integer(0.6 + j/2)  # 4 bits per element... so j2 bytes for j elements
    k = rslt_cnt
    k4 = k*4
    if (rslt_cnt<1)  valid_record = FALSE   # REVISIT... 93K behaviour
    

    test_flg = as.raw(test_flg)     # enable bitwise math...
    if ((test_flg & as.raw(64))>0) {
        # is this only true for Credence and/or Quartet MPR records?
        valid_record = FALSE
        Ptr <<- Ptr + rec_len
        rec_len = 0
    }
    if (rec_len >= j2) {
        rtn_stat = readBin(Stdf[Ptr:(Ptr+j2)],integer(),n=j2,size=1,signed=FALSE)
        Ptr <<- Ptr + j2
        rec_len = rec_len - j2
    } 

    if (rec_len >= k4) {
        rtn_rslt = readBin(Stdf[Ptr:(Ptr+k4)],numeric(),n=k,size=4,endian=endy,signed=TRUE)
        Ptr <<- Ptr + k4
        rec_len = rec_len - k4
    }

    if (rec_len > 0) {
        str_list = readSTDFstring(rec_len)
        test_txt = str_list$string
        rec_len = str_list$bytes_left
        #Debug_testname <<- test_txt
    }

    if (rec_len > 0) {
        str_list = readSTDFstring(rec_len)
        alarm_id = str_list$string
        rec_len = str_list$bytes_left
    }

    if (rec_len > 0) {
        opt_flag = as.integer(Stdf[Ptr])
        Ptr <<- Ptr + 1
        rec_len = rec_len - 1
        valid_opt_flag = TRUE
    }
    
    if (rec_len > 0) {
		if (rec_len>=11) {
			partB = Stdf[Ptr:(Ptr+10)]	# all after OPT_FLAG field upto START_IN
		} else {
			partB = Stdf[Ptr:(Ptr+rec_len-1)]	# all after OPT_FLAG field
		}
        res_scal = readBin(Stdf[Ptr],integer(),n=1,size=1,signed=TRUE)
        Ptr <<- Ptr + 1
        rec_len = rec_len - 1
    }
    
    if (rec_len > 0) {
        llm_scal = readBin(Stdf[Ptr],integer(),n=1,size=1,signed=TRUE)
        Ptr <<- Ptr + 1
        rec_len = rec_len - 1
    }
    
    if (rec_len > 0) {
        hlm_scal = readBin(Stdf[Ptr],integer(),n=1,size=1,signed=TRUE)
        Ptr <<- Ptr + 1
        rec_len = rec_len - 1
    }
 
    if (rec_len >3) {
        lo_limit = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    } else {
        Ptr <<- Ptr + rec_len
        rec_len = 0
    }

    if (rec_len >3) {
        hi_limit = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    } else {
        Ptr <<- Ptr + rec_len
        rec_len = 0
    }

    if (rec_len >3) {
        start_in = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    } else {
        Ptr <<- Ptr + rec_len
        rec_len = 0
    }

    if (rec_len >3) {
        incr_in = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
        Ptr <<- Ptr + 4
        rec_len = rec_len - 4
    } else {
        Ptr <<- Ptr + rec_len
        rec_len = 0
    }

    if (rec_len >=(2*j)) {
        rtn_indx = readBin(Stdf[Ptr:(Ptr+(2*j))],integer(),n=j,size=2,endian=endy,signed=FALSE)
        Ptr <<- Ptr + (2*j)
        rec_len = rec_len - (2*j)
    } else {
        Ptr <<- Ptr + rec_len
        rec_len = 0
    }

    if (rec_len > 0) {
		short_mpr = FALSE
        str_list = readSTDFstring(rec_len)
        units = str_list$string
        rec_len = str_list$bytes_left
    }

    if (rec_len > 0) {
        str_list = readSTDFstring(rec_len)
        units_in = str_list$string
        rec_len = str_list$bytes_left
    }


    if (rec_len > 0) {
		partC = Stdf[Ptr:(Ptr+rec_len-1)]	# all after UNITS_IN field
    } 


# === don't parse below, it slows things down and there is no need ===
#    if (rec_len > 0) {
#        str_list = readSTDFstring(rec_len)
#        c_resfmt = str_list$string
#        rec_len = str_list$bytes_left
#    }
#
#
#    if (rec_len > 0) {
#        str_list = readSTDFstring(rec_len)
#        c_llmfmt = str_list$string
#        rec_len = str_list$bytes_left
#    }
#
#	
#    if (rec_len > 0) {
#        str_list = readSTDFstring(rec_len)
#        c_hlmfmt = str_list$string
#        rec_len = str_list$bytes_left
#    }
#
#	
#    if (rec_len >3) {
#		specs_valid = TRUE
#        lo_spec = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
#        Ptr <<- Ptr + 4
#        rec_len = rec_len - 4
#    } else {
#        Ptr <<- Ptr + rec_len
#        rec_len = 0
#    }
#
#
#    if (rec_len >3) {
#		hi_spec = readBin(Stdf[Ptr:(Ptr+3)],numeric(),n=1,size=4,endian=endy)
#        Ptr <<- Ptr + 4
#        rec_len = rec_len - 4
#    } else {
#        Ptr <<- Ptr + rec_len
#        rec_len = 0
#    }

#	if(test_num==605) {
#		browser()
#	}
	

	# throw away any remaining portion of the record we aren't interested in...
    #--------------------------------------------------------------------------
    if (rec_len > 0) {
        #cat("WARNING: MPR record longer than expected \n")
        Ptr <<- Ptr + rec_len
        rec_len = 0
    } 

    if (valid_record) {
        # doesn't seem to be any per result valid flag... assume all valid.

        valid_llim = FALSE
        valid_ulim = FALSE
        valid_scale = FALSE
        if (valid_opt_flag) {
            opt_flag = as.raw(opt_flag)   # enable bitwise math...
            if ( ((opt_flag & as.raw(64+16))==0) && is.finite(lo_limit) ) {
                valid_llim = TRUE
            }
            if ( ((opt_flag & as.raw(128+32))==0) && is.finite(hi_limit) ) {
                valid_ulim = TRUE
            }
            if ( ((opt_flag & as.raw(1))==0) && is.finite(res_scal) ) {
                valid_scale = TRUE
            }
        }

		if(sweep_mpr) {
			# don't convert this one to PTRs, just dump it...
			New_Stdf[New_Ptr:(New_Ptr+sweep_rec_len-1)] <<- Stdf[(Ptr-sweep_rec_len):(Ptr-1)]
			New_Ptr <<- New_Ptr + sweep_rec_len
			valid_record=FALSE
		}
		
    } else {
		# skipping this record!
		# need to remove the first 4 bytes from the output stream...
		New_Ptr <<- New_Ptr - 4		# remove start of MPR from new stdf stream
	}

    if (valid_record) {
        # Catalyst: ... don't have example with MPR, so may not be true...
        # need to split off function call.. after " <> " separator
        #-------------------------------------------------------
        if (Executive_type=="Image") {
            chunks = strsplit(test_txt," <> ",fixed=TRUE)
            test_txt = chunks[[1]][1]
        }

        # for each result in the MPR, we will want to create a PTR with
		# a unique test name....
        # test_txt + "/" + pin_name
        # or test_txt + "/" no_pin
        #-------------------------------------------------------------------
		New_Ptr <<- New_Ptr - 4		# remove start of MPR from new stdf stream

		# build up new PTR record...
		# - rec_len = length(part2to4 aka ptr)
		# - part1 = rec_typ + rec_sub
		# - part2 = partA
		# - part3 = result + test_txt <=== part3 changes per result
		# - part4 = alarm_id + opt_flag + partB + units + partC
		rec_typ = 15
		rec_sub = 10
		
		part1 = writeBin(as.integer(rec_typ),raw(),size=1)
		part1[2] = writeBin(as.integer(rec_sub),raw(),size=1)
		part2 = as.raw(partA)
		part4 = raw(0)
		x0=1
		# add string alarm_id
		x=nchar(alarm_id)
		part4[x0] = writeBin(as.integer(x),raw(),size=1)
		if(x>0) {
			temp <- writeBin(alarm_id,raw(),size=1)
			part4[(x0+1):(x0+x)] = temp[1:x]
		}
		x0 = x0 + x + 1
		if(valid_opt_flag) {
			# add opt_flag, force bit1 to 1 first
			opt_flag = as.raw(opt_flag)
			opt_flag = opt_flag | as.raw(2)
			part4[x0] = writeBin(opt_flag,raw(),size=1)
			x0 = x0 +1
				
			x = length(partB)
			if(x>0) part4[x0:(x0+x-1)] = writeBin(partB,raw(),size=1)
			x0 = x0 + x

			# units
			if(!short_mpr) {
				x = nchar(units)
				part4[x0] = writeBin(as.integer(x),raw(),size=1)
				if(x>0) {
					temp <- writeBin(units,raw(),size=1)
					part4[(x0+1):(x0+x)] = temp[1:x]
				}
				x0 = x0 + x + 1
			}
				
			x=length(partC)
			if(x>0) part4[x0:(x0+x-1)] = writeBin(partC,raw(),size=1)
		}

		if (Inc_tnums>0) {
			if(rslt_cnt>=Inc_tnums) {
        		cat(sprintf("WARNING: MPR record has %d tests,",
                    rslt_cnt))
        		cat(sprintf(" test numbers may not be unique\n"))
        		cat(sprintf("         at test %s \n"),test_txt)
			}
		}
		
		
		# now loop through the individual results
		#-----------------------------------------
        for (j in 1:rslt_cnt) {
            if (rtn_icnt>=1) {
                if (rtn_indx[j] <1) {
                    txt_plus_pin = sprintf("%s/no_pin%d",test_txt,j)
                } else {
                    txt_plus_pin = sprintf("%s/%s",test_txt,Pin_names[rtn_indx[j]])
                }
            } else {
                txt_plus_pin = sprintf("%s/no_pin%d",test_txt,j)
            }
            
			if (Inc_tnums>0) {
				my_tnum = test_num + j
				my_ptr = writeBin(as.integer(my_tnum),raw(),size=4,endian=endy)
				my_ptr = c(my_ptr,as.raw(part2))
			} else {
				my_ptr = as.raw(part2)
			}
			my_ptr[9:12] = writeBin(rtn_rslt[j],raw(),size=4,endian=endy)
			x = nchar(txt_plus_pin)
			my_ptr[13] = writeBin(as.integer(x),raw(),size=1)
			if(x>0) {
				temp <- writeBin(txt_plus_pin,raw(),size=1)
				my_ptr[14:(13+x)] = temp[1:x]
			}
			my_ptr = c(my_ptr,part4)
			# ok, now push the ptr into the new stdf stream
			x = length(my_ptr)
			New_Stdf[New_Ptr:(New_Ptr+1)] <<- writeBin(as.integer(x),raw(),size=2,endian=endy)
			New_Stdf[(New_Ptr+2):(New_Ptr+3)] <<- part1
			New_Stdf[(New_Ptr+4):(New_Ptr+x+3)] <<- my_ptr
			New_Ptr <<- New_Ptr + x + 4
		}
	}

}



###############################################################################
parse_TSR_record <- function(rec_len,endy) {
}


###############################################################################
readSTDFstring <- function(bytes) {
    
    str_len = readBin(Stdf[Ptr],integer(),n=1,size=1,signed=FALSE)
    bytes = bytes - 1
    Ptr <<- Ptr + 1
    if (str_len > bytes) {
        cat(sprintf("ERROR: String length %d is longer than record length %d, skipping... \n",
                    str_len,bytes))
        my_string = ''
        Ptr <<- Ptr + bytes
        bytes = 0
    } else {
        if (str_len > 0) {
            chars = readChar(Stdf[Ptr:(Ptr+str_len-1)],str_len)
            Ptr <<- Ptr + str_len
            my_string = paste(chars, collapse=NULL)
            bytes = bytes - str_len
        } else {
            my_string = ""
        }
    }

    return( list(string=my_string, bytes_left=bytes) )
}


#############################################################################
#  copy local functions to the .ExpandMPRs.env and remove them
#  from the global environment
#############################################################################
environment(parse_stdf_record)<-.ExpandMPRs.env
assign("parse_stdf_record",parse_stdf_record,envir=.ExpandMPRs.env)
rm(parse_stdf_record)

environment(parse_FAR_record)<-.ExpandMPRs.env
assign("parse_FAR_record",parse_FAR_record,envir=.ExpandMPRs.env)
rm(parse_FAR_record)

environment(parse_MIR_record)<-.ExpandMPRs.env
assign("parse_MIR_record",parse_MIR_record,envir=.ExpandMPRs.env)
rm(parse_MIR_record)

environment(copy_STDF_record)<-.ExpandMPRs.env
assign("copy_STDF_record",copy_STDF_record,envir=.ExpandMPRs.env)
rm(copy_STDF_record)

environment(parse_PMR_record)<-.ExpandMPRs.env
assign("parse_PMR_record",parse_PMR_record,envir=.ExpandMPRs.env)
rm(parse_PMR_record)

environment(parse_MPR_record)<-.ExpandMPRs.env
assign("parse_MPR_record",parse_MPR_record,envir=.ExpandMPRs.env)
rm(parse_MPR_record)

environment(parse_TSR_record)<-.ExpandMPRs.env
assign("parse_TSR_record",parse_TSR_record,envir=.ExpandMPRs.env)
rm(parse_TSR_record)

environment(readSTDFstring)<-.ExpandMPRs.env
assign("readSTDFstring",readSTDFstring,envir=.ExpandMPRs.env)
rm(readSTDFstring)


environment(ExpandMPRs)<-.ExpandMPRs.env

