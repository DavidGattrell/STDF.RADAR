# MergeNewTests.R 
#
# $Id: MergeNewTests.R,v 1.2 2020/12/18 01:19:24 david Exp $
#
# script that parses 2 rtdf files, looks for new tests (Parameters) in
# the 2nd file, then looks for matching devices (either part_id or x/y coord in
# DevicesFrames) and appends the new test results to the first file to create
# a new combined rtdf that is output.
# - if part passed in file1, but failed in file2, bin info will be from file2.
#   (DevicesFrame)
#
#---------------------------------------------------------------------------
MergeNewTests <- function(in_file1="",in_file2="",in_dir1="",in_dir2="",use_xy_coords=FALSE,
						out_file="",verbose=TRUE) {

	# in_file1 -- the original file
	# in_file2 -- a file that contains additional tests for the same devices
	# in_dir1 -- absolute path for where to find in_file1
	# in_dir2 -- absolute path for where to find in_file2
	# use_xy_coords -- if set to TRUE, use x_coord,y_coord rather than part_id when
	#            matching devices between the 2 input files
	# out_file -- string of rtdf filename you want generated
	# verbose -- print status and information to console as processing
	#-------------------------------------------------------------------------------

    # if filenames not defined, prompt for them
    #------------------------------------------
	if (in_file1 == "") {
		in_file1 <- readline("Enter the name of the first RTDF file to read: ")
   }
	if (in_file2 == "") {
		in_file2 <- readline("Enter the name of the second RTDF file to read: ")
   }
	if (out_file == "") {
		out_file <- readline("Enter the name of the RTDF file to write: ")
    }
	

	if (in_dir1 != "") {
		my_dir = getwd()
		setwd(in_dir1)
	}
    my_objs = load(in_file1)
	if (in_dir1 != "")  setwd(my_dir)

	# copy key objects from in_file1 rtdf to out_file rtdf
	#-----------------------------------------------------
	AllDevicesFrame = DevicesFrame
	AllParametersFrame = ParametersFrame
	AllResultsMatrix = ResultsMatrix
	AllLotInfoFrame = LotInfoFrame

	# check if TestFlagMatrix is present
	valid_testflagmatrix = FALSE
	if (is.finite(match("TestFlagMatrix",my_objs))) {
		# is it the same size as the ResultsMatrix?
		RM_dims = dim(ResultsMatrix)
		TFM_dims = dim(TestFlagMatrix)
		if (identical(RM_dims,TFM_dims)) {
			valid_testflagmatrix = TRUE
			AllTestFlagMatrix = TestFlagMatrix
		} else {
			cat(sprintf("WARNING: TestFlagMatrix ignored in in_file1, %s, mismatch in size!\n",in_file1))
		}
	}

	AllHbin_valid = FALSE
	if (is.finite(match("HbinInfoFrame",my_objs))) {
		AllHbinInfoFrame = HbinInfoFrame
		AllHbin_valid = TRUE
	}
	AllSbin_valid = FALSE
	if (is.finite(match("SbinInfoFrame",my_objs))) {
		AllSbinInfoFrame = SbinInfoFrame
		AllSbin_valid = TRUE
	}

	# any other objects we want to keep?
	AllWafers_valid = FALSE
	# WafersFrame
	# WaferInfoFrame


	devs = dim(DevicesFrame)[1]
	if (use_xy_coords) {
		x_coords = as.character(DevicesFrame[["x_coord"]])
		y_coords = as.character(DevicesFrame[["y_coord"]])
		xys = array(c(x_coords,y_coords),dim=c(devs,2))

		indices = which(duplicated(xys,fromLast=TRUE))

	} else {
		x_coord = as.integer(DevicesFrame[[1,"x_coord"]])
		if(is.finite(x_coord) && x_coord>-32768) {
			cat("WARNING: you may really have wanted to have the use_xy_coords flag set!\n")
		}
		part_ids = as.character(DevicesFrame[["part_id"]])

		indices = which(duplicated(part_ids,fromLast=TRUE))

	}
	if(length(indices)>0) {
		if(verbose) {
			cat(sprintf("repeats count in in_file1 is %d... \n",length(indices)))
			cat(sprintf("... you should probably run ShrinkRetests first?\n"))
		}
	}


	# load 2nd file into memory...
	#------------------------------
	if (in_dir2 != "") {
		my_dir = getwd()
		setwd(in_dir2)
	}
    my_objs = load(in_file2)
	if (in_dir2 != "")  setwd(my_dir)


	# what are the new tests/Parameters?
	#-----------------------------------
	orig_testnames = as.character(AllParametersFrame[["testname"]])
	testnames = as.character(ParametersFrame[["testname"]])
	new_indices = vector()
	for (i in 1:length(testnames)) {
		index = match(testnames[i],orig_testnames,nomatch=NaN)
		if(!is.finite(index)) {
			# this test is new
			new_indices[length(new_indices)+1] = i
		}
	}

	
	if(length(new_indices)<1) {
		# nasty message, hey! there is nothing to do here!
		cat(sprintf("HEY! No new parameters in 2nd RTDF file... doing nothing!"))
	} else {
		# ok, now append new Parameters to end of AllParametersFrame
		#-----------------------------------------------------------
		cat(sprintf("Adding %d new tests from in_file2 \n",length(new_indices)))
		my_tnums = as.numeric(ParametersFrame[new_indices,"testnum"])
		my_tnames = as.character(ParametersFrame[new_indices,"testname"])
		my_scalers = as.numeric(ParametersFrame[new_indices,"scaler"])
		my_units = as.character(ParametersFrame[new_indices,"units"])
		my_lls = as.numeric(ParametersFrame[new_indices,"ll"])
		my_uls = as.numeric(ParametersFrame[new_indices,"ul"])
		my_ll_ges = as.integer(ParametersFrame[new_indices,"ll_ge"])
		my_ul_ges = as.integer(ParametersFrame[new_indices,"ul_ge"])
		my_plot_lls = as.numeric(ParametersFrame[new_indices,"plot_ll"])
		my_plot_uls = as.numeric(ParametersFrame[new_indices,"plot_ul"])
		max_params = dim(AllParametersFrame)[1]
		i1 = max_params + 1
		i2 = max_params + length(new_indices)
		AllParametersFrame[i1:i2,"testnum"] <- my_tnums
		AllParametersFrame[i1:i2,"testname"] <- my_tnames
		AllParametersFrame[i1:i2,"scaler"] <- my_scalers
		AllParametersFrame[i1:i2,"units"] <- my_units
		AllParametersFrame[i1:i2,"ll"] <- my_lls
		AllParametersFrame[i1:i2,"ul"] <- my_uls
		AllParametersFrame[i1:i2,"ll_ge"] <- my_ll_ges
		AllParametersFrame[i1:i2,"ul_ge"] <- my_ul_ges
		AllParametersFrame[i1:i2,"plot_ll"] <- my_plot_lls
		AllParametersFrame[i1:i2,"plot_ul"] <- my_plot_uls

		# need to add new columns to AllResultsMatrix
		dims = dim(AllResultsMatrix)
		AllResultsMatrix = cbind(AllResultsMatrix,matrix(data=NaN,nrow=dims[1],
						ncol=length(new_indices)))

		if(valid_testflagmatrix) {
			AllTestFlagMatrix = cbind(AllTestFlagMatrix,matrix(data=NaN,nrow=dims[1],
						ncol=length(new_indices)))
		
		}

		# now cross reference the DevicesFrames
		#----------------------------------------	
		if (use_xy_coords) {
			new_x_coords = as.character(DevicesFrame[["x_coord"]])
			new_y_coords = as.character(DevicesFrame[["y_coord"]])
		} else {
			new_part_ids = as.character(DevicesFrame[["part_id"]])

		}
		unmatched_devs = 0
		for(i in 1:devs) {
			if (use_xy_coords) {
				x_matches = which(new_x_coords==x_coords[i])
				y_matches = which(new_y_coords==y_coords[i])
				idx = x_matches[which(x_matches %in% y_matches)]
			} else {
				idx = which(new_part_ids==part_ids[i])
			}
			if(length(idx)>0) {
				# copy the results for the new tests to the AllResultsMatrix
				AllResultsMatrix[i,i1:i2] = ResultsMatrix[idx[1],new_indices]
			} else {
				unmatched_devs = unmatched_devs + 1
			}
		}

		cat(sprintf("%d of %d devices had no matching device in 2nd file\n",
					unmatched_devs,devs))


		# now update Binning in DevicesFrame and xBinInfoFrames
		#------------------------------------------------------
		# look for parts that pass the original file, but fail 2nd file
		# REVISIT .. not coded yet!

	}

	
	# move data to standard rtdf object names
	#-----------------------------------------
    LotInfoFrame = AllLotInfoFrame
    ParametersFrame = AllParametersFrame
    DevicesFrame = AllDevicesFrame
    ResultsMatrix = AllResultsMatrix
	if (valid_testflagmatrix)  TestFlagMatrix = AllTestFlagMatrix
    if (AllHbin_valid)  HbinInfoFrame = AllHbinInfoFrame
    if (AllSbin_valid)  SbinInfoFrame = AllSbinInfoFrame
	if (AllWafers_valid)  WafersFrame = AllWafersFrame


    # save rtdf file
    #------------------
    my_list = c("LotInfoFrame","ParametersFrame","DevicesFrame","ResultsMatrix")
	if (valid_testflagmatrix)  my_list[length(my_list)+1] = "TestFlagMatrix"
    if (AllHbin_valid)  my_list[length(my_list)+1] = "HbinInfoFrame"
    if (AllSbin_valid)  my_list[length(my_list)+1] = "SbinInfoFrame"
    if (AllWafers_valid) {
		my_list[length(my_list)+1] = "WafersFrame"
		if (Valid_WCR)  my_list[length(my_list)+1] = "WaferInfoFrame"
    }
    save(list=my_list, file=out_file)
    
	dims = dim(ResultsMatrix)
	dev_count = dims[1]
	param_count = dims[2]
    cat(sprintf("merged dataset: %d Devices x %d Parameters\n",
                dev_count,param_count))
}
