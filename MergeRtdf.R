#  MergeRtdf.R
#
# $Id: MergeRtdf.R,v 1.14 2020/07/14 23:20:16 david Exp $
#
# script that merges multiple rtdf files into a single rtdf file
#
# Copyright (C) 2007-2014 David Gattrell
#               2012-2013 Chad Erven
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
#-----------------------------------------------
MergeRtdf <- function(in_files="",out_file="",in_dirs="",union_of_tests=TRUE) {
    
    # in_files -- vector of strings of filenames for rtdf files to read in
    # out_file -- name of file to write superset rtdf file to
	# in_dirs -- vector of strings of paths to in_files...
	#			 if empty, assume we are in correct directory
	# union_of_tests -- if there are tests in 2nd and subsequent files
	#			that aren't in first file, append them to the
	#			ParametersFrame, else keep only the tests that
	#			exist in the first file
    # --------------------------------------------------------------------


    Wafer_count = 0
	AllWafers_valid = FALSE
	AllHbin_valid = FALSE
	AllSbin_valid = FALSE
	AllTestFlag_valid = FALSE
	Valid_WCR = FALSE

    # if filenames not defined, prompt for them
    #------------------------------------------
	if (length(in_files)<2 || (in_files[1] == "")) {
		in_files[1] <- readline("Enter the name of the first RTDF file to read: ")
		in_files[2] <- readline("Enter the name of the second RTDF file to read: ")
    }
	if (out_file == "") {
		out_file <- readline("Enter the name of the RTDF file to write: ")
    }

    datasets = length(in_files)
    len = length(in_dirs)
    if(len<datasets)  in_dirs[(len+1):datasets]=""
	for (j in 1:datasets) {
		if (in_dirs[j] != "") {
			my_dir = getwd()
			setwd(in_dirs[j])
		}
		my_objs = load(in_files[j])
		if (in_dirs[j] != "")  setwd(my_dir)
      
        # Add the source data set to the DevicesFrame data if 
        # the field is not already there
        if( !( "source_dataset" %in% colnames(DevicesFrame))) {
            DevicesFrame[,"source_dataset"] = in_files[j]
        }

		# check if TestFlagMatrix is present
		valid_testflagmatrix = FALSE
		if (is.finite(match("TestFlagMatrix",my_objs))) {
			# is it the same size as the ResultsMatrix?
			RM_dims = dim(ResultsMatrix)
			TFM_dims = dim(TestFlagMatrix)
			if (identical(RM_dims,TFM_dims)) {
				valid_testflagmatrix = TRUE
			} else {
				cat(sprintf("WARNING: TestFlagMatrix ignored in file %d, %s, mismatch in size!\n",
						j,in_files[j]))
			}
		}

		# check if WafersFrame is present
		if (is.finite(match("WafersFrame",my_objs))) {
			valid_wafersframe = TRUE
		} else {
			valid_wafersframe = FALSE
		}

		# check if WaferInfoFrame is present
		if (is.finite(match("WaferInfoFrame",my_objs))) {
			Valid_WCR = TRUE
		}

		if(j==1) {
			if (valid_wafersframe) {
				wafer_idxs = as.integer(DevicesFrame[["wafer_index"]])
				valid_idxs = unique(wafer_idxs)
				valid_idxs = valid_idxs[which(is.finite(valid_idxs))]

				new_idxs = rep(NA,times=length(wafer_idxs))
				if(length(valid_idxs >0)) {
					AllWafersFrame = WafersFrame[valid_idxs,]
					# need to update DeviceFrame to new pointer values...
					for (i in 1:length(valid_idxs)) {
						new_idxs[which(wafer_idxs==valid_idxs[i])] = i
					}
					AllWafers_valid = TRUE
				} 
				DevicesFrame[["wafer_index"]] = new_idxs
			}
			AllDevicesFrame = DevicesFrame
			AllParametersFrame = ParametersFrame
			AllResultsMatrix = ResultsMatrix
			AllLotInfoFrame = LotInfoFrame
			if (is.finite(match("HbinInfoFrame",my_objs))) {
				AllHbinInfoFrame = HbinInfoFrame
				AllHbin_valid = TRUE
			}
			if (is.finite(match("SbinInfoFrame",my_objs))) {
				AllSbinInfoFrame = SbinInfoFrame
				AllSbin_valid = TRUE
			}
			if (valid_testflagmatrix) {
				AllTestFlagMatrix = TestFlagMatrix
				AllTestFlag_valid = TRUE
			}
		} else {
			# browser()

			# merge LotInfoFrame
			#--------------------
			old_names = names(AllLotInfoFrame)
			new_names = names(LotInfoFrame)
			all_names = union(old_names,new_names)
			missing_names = setdiff(old_names,new_names)
			adding_names = setdiff(new_names,old_names)
			if(length(adding_names)>0) {
				AllLotInfoFrame[,adding_names] = NA
			}
			if(length(missing_names)>0) {
				LotInfoFrame[,missing_names] = NA
			}

			last_lotinfo = dim(AllLotInfoFrame)[1]
			new_lot_infos = dim(LotInfoFrame)[1]
			for (i in 1:new_lot_infos)  AllLotInfoFrame[last_lotinfo+i,] = LotInfoFrame[i,]

			# merge WafersFrame BEFORE merging DevicesFrame
			#----------------------------------------------
			#browser()  # ... revisit, 23feb2020, bug when merging a595 split wafers back together!
			if (valid_wafersframe) {
				if (AllWafers_valid) {
					# need to append WafersFrame to AllWafersFrame
					old_names = names(AllWafersFrame)
					new_names = names(WafersFrame)
					all_names = union(old_names,new_names)
					missing_names = setdiff(old_names,new_names)
					adding_names = setdiff(new_names,old_names)
					if(length(adding_names)>0) {
						AllWafersFrame[,adding_names] = NA
					}
					if(length(missing_names)>0) {
						WafersFrame[,missing_names] = NA
					}

					last_wafer = dim(AllWafersFrame)[1]

					wafer_idxs = as.integer(DevicesFrame[["wafer_index"]])
					valid_idxs = unique(wafer_idxs)
					valid_idxs = valid_idxs[which(is.finite(valid_idxs))]

					new_idxs = rep(NA,times=length(wafer_idxs))
					if(length(valid_idxs>0)) {
						for (i in 1:length(valid_idxs)) {
							# is this wafer number already in the AllWafersFrame?
							# .. GlobalFoundaries unique 7char wafer_id, not actually 1-25 anymore,
							#    so need to update this code.  11-jul-2020, stop those NA warnings
							wafer_id = as.character(WafersFrame[[valid_idxs[i],"wafer_id"]])
							all_wafer_ids = as.character(AllWafersFrame[["wafer_id"]])
							new_idx = match(wafer_id,all_wafer_ids,nomatch=0)
							if(new_idx>0) {
								new_idxs[which(wafer_idxs==valid_idxs[i])] = new_idx
							} else {
								last_wafer = last_wafer + 1
								#browser()
								AllWafersFrame[last_wafer,] = WafersFrame[valid_idxs[i],]
								new_idxs[which(wafer_idxs==valid_idxs[i])] = last_wafer
							}
						}
					}
					DevicesFrame[["wafer_index"]] = new_idxs
				} else {
					wafer_idxs = as.integer(DevicesFrame[["wafer_index"]])
					valid_idxs = unique(wafer_idxs)
					valid_idxs = valid_idxs[which(is.finite(valid_idxs))]

					new_idxs = rep(NA,times=length(wafer_idxs))
					if(length(valid_idxs >0)) {
						AllWafersFrame = WafersFrame[valid_idxs,]
						# need to update DeviceFrame to new pointer values...
						for (i in 1:length(valid_idxs)) {
							new_idxs[which(wafer_idxs==valid_idxs[i])] = i
						}
						AllWafers_valid = TRUE
					} 
					DevicesFrame[["wafer_index"]] = new_idxs
				}
			}


			# merge DevicesFrame
			#---------------------
			old_devs = dim(AllDevicesFrame)[1]
			new_devs = dim(DevicesFrame)[1]
			
			# browser()

			old_names = names(AllDevicesFrame)
			new_names = names(DevicesFrame)
			all_names = union(old_names,new_names)
			missing_names = setdiff(old_names,new_names)
			adding_names = setdiff(new_names,old_names)

			if(length(adding_names)>0) {
				AllDevicesFrame[,adding_names] = NA
			}
			if(length(missing_names)>0) {
				DevicesFrame[,missing_names] = NA
			}
			AllDevicesFrame[(old_devs+1):(old_devs+new_devs),all_names] = DevicesFrame[,all_names]
			#AllDevicesFrame[(old_devs+1):(old_devs+new_devs),] = DevicesFrame


			# merge SbinInfoFrame and HbinInfoFrame
			#---------------------------------------
			if (is.finite(match("HbinInfoFrame",my_objs))) {
				if(AllHbin_valid) {
					hbin_nums = as.integer(AllHbinInfoFrame[,"hbin_num"])
					new_hbin_count = dim(HbinInfoFrame)[1]
					for (i in 1:new_hbin_count) {
						hbin_num = HbinInfoFrame[[i,"hbin_num"]]
						idx = match(hbin_num,hbin_nums)
						if(is.finite(idx)) {
							AllHbinInfoFrame[idx,"hbin_cnt"] = 
										as.integer(AllHbinInfoFrame[idx,"hbin_cnt"]) +
										as.integer(HbinInfoFrame[i,"hbin_cnt"])
						} else {
							last_i = dim(AllHbinInfoFrame)[1]
							AllHbinInfoFrame[last_i+1,] = HbinInfoFrame[i,]
						}
					}
				} else {
					AllHbinInfoFrame = HbinInfoFrame
					All_Hbin_valid = TRUE
				}
			}
			if (is.finite(match("SbinInfoFrame",my_objs))) {
				if(AllSbin_valid) {
					sbin_nums = as.integer(AllSbinInfoFrame[,"sbin_num"])
					new_sbin_count = dim(SbinInfoFrame)[1]
					for (i in 1:new_sbin_count) {
						sbin_num = SbinInfoFrame[[i,"sbin_num"]]
						idx = match(sbin_num,sbin_nums)
						if(is.finite(idx)) {
							AllSbinInfoFrame[idx,"sbin_cnt"] = 
										as.integer(AllSbinInfoFrame[idx,"sbin_cnt"]) +
										as.integer(SbinInfoFrame[i,"sbin_cnt"])
						} else {
							last_i = dim(AllSbinInfoFrame)[1]
							AllSbinInfoFrame[last_i+1,] = SbinInfoFrame[i,]
						}
					}
				} else {
					AllSbinInfoFrame = SbinInfoFrame
					All_Sbin_valid = TRUE
				}
			}
			
			
			
			# cross reference the ParametersFrame to AllParametersFrame
			#-----------------------------------------------------------
			my_dims = dim(AllResultsMatrix)
			AllResultsMatrix = rbind(AllResultsMatrix,matrix(NaN,nrow=new_devs,
						ncol=my_dims[2]))
			
			if(valid_testflagmatrix && !AllTestFlag_valid) {
				# create empty flag matrix for preceding files that didn't have any
				AllTestFlagMatrix = matrix(NaN,nrow=my_dims[1],ncol=my_dims[2])
				AllTestFlag_valid = TRUE
			}
			if(AllTestFlag_valid) {
				AllTestFlagMatrix = rbind(AllTestFlagMatrix,matrix(NaN,nrow=new_devs,
						ncol=my_dims[2]))
			}

			max_params = dim(AllParametersFrame)
			max_params = max_params[1]
			param_xrefs = rep(NaN,max_params)
			for (i in 1:max_params) {
				test_nam = AllParametersFrame[i,"testname"]
				index = match(test_nam,ParametersFrame[["testname"]],
						nomatch=NaN)
				param_xrefs[i]=index

				if (is.finite(index)) {
					AllResultsMatrix[(old_devs+1):(old_devs+new_devs),i] = 
							ResultsMatrix[,index]
					if (valid_testflagmatrix) {
						AllTestFlagMatrix[(old_devs+1):(old_devs+new_devs),i] =
								TestFlagMatrix[,index]
					}

				}
			}
			my_idxs = c(1:dim(ParametersFrame)[1])
			new_tests = setdiff(my_idxs,param_xrefs)
			if (union_of_tests && (length(new_tests)>0)) {
				cat(sprintf("Adding %d new tests from dataset %d \n",length(new_tests),j))
				my_tnums = as.numeric(ParametersFrame[new_tests,"testnum"])
				my_tnames = as.character(ParametersFrame[new_tests,"testname"])
				my_scalers = as.numeric(ParametersFrame[new_tests,"scaler"])
				my_units = as.character(ParametersFrame[new_tests,"units"])
				my_lls = as.numeric(ParametersFrame[new_tests,"ll"])
				my_uls = as.numeric(ParametersFrame[new_tests,"ul"])
				my_ll_ges = as.integer(ParametersFrame[new_tests,"ll_ge"])
				my_ul_ges = as.integer(ParametersFrame[new_tests,"ul_ge"])
				my_plot_lls = as.numeric(ParametersFrame[new_tests,"plot_ll"])
				my_plot_uls = as.numeric(ParametersFrame[new_tests,"plot_ul"])
				i1 = max_params + 1
				i2 = max_params + length(new_tests)
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
				# now need to add new columns to AllResultsMatrix
				dims = dim(AllResultsMatrix)
				AllResultsMatrix = cbind(AllResultsMatrix,matrix(data=NaN,nrow=dims[1],
						ncol=length(new_tests)))
				# now add the data for the new tests
				AllResultsMatrix[(old_devs+1):(old_devs+new_devs),i1:i2] = 
							ResultsMatrix[,new_tests]
				if(AllTestFlag_valid) {
					# now need to add new columns to AllTestFlagMatrix
					AllTestFlagMatrix = cbind(AllTestFlagMatrix,matrix(data=NaN,nrow=dims[1],
							ncol=length(new_tests)))
					# now add the flag data for the new tests
					if(valid_testflagmatrix) {
						AllTestFlagMatrix[(old_devs+1):(old_devs+new_devs),i1:i2] = 
							TestFlagMatrix[,new_tests]
					} else {
						AllTestFlagMatrix[(old_devs+1):(old_devs+new_devs),i1:i2] = 
							matrix(data=NaN,nrow=new_devs,ncol=(i2-i1+1))
					}
				}
			} else if (length(new_tests)>0) {
				cat(sprintf("Ignoring %d new tests from dataset %d \n",length(new_tests),j))
			}
		}
    }
    # browser()

	# tidy up DevicesFrame row.names...
	rows = dim(AllDevicesFrame)[1]
	row.names(AllDevicesFrame) = as.character(c(1:rows))

    # move data to standard rtdf object names
    #------------------------------------------
    LotInfoFrame = AllLotInfoFrame
    ParametersFrame = AllParametersFrame
    DevicesFrame = AllDevicesFrame
    ResultsMatrix = AllResultsMatrix
    if (AllHbin_valid)  HbinInfoFrame = AllHbinInfoFrame
    if (AllSbin_valid)  SbinInfoFrame = AllSbinInfoFrame
	if (AllTestFlag_valid)  TestFlagMatrix = AllTestFlagMatrix
	if (AllWafers_valid)  WafersFrame = AllWafersFrame

    # save rtdf file
    #------------------
    my_list = c("LotInfoFrame","ParametersFrame","DevicesFrame","ResultsMatrix")
    if (AllHbin_valid)  my_list[length(my_list)+1] = "HbinInfoFrame"
    if (AllSbin_valid)  my_list[length(my_list)+1] = "SbinInfoFrame"
	if (AllTestFlag_valid)  my_list[length(my_list)+1] = "TestFlagMatrix"
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
