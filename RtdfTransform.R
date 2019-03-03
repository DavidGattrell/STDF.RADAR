# RtdfTransform.R
#
# $Id: RtdfTransform.R,v 1.3 2019/02/01 02:06:52 david Exp $
#
# script that transforms parameters in an RTDF file.
# typically used for getting bench data into a format to compare against ATE data,
# or to compare data from one tester vs. another when there are some
# name changes or basic transforms.  One example would be changing
# leakage tests with pulldowns from current to resistance to aid in correlating
# against test insert/process data.
#
# Copyright (C) 2009-2010 David Gattrell
#               2018 David Gattrell
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
#----------------------------------------------------------
RtdfTransform <- function(in_file="",transform_csv="",out_file="",csv_dir="",in_dir="") {

	# in_file -- string for filename of rtdf file to transform.  
	#
	# transform_csv -- a .csv file containing instructions on how to
	#				transform the data... columns are:
	#				testnum new_testname old_testname prefix units new_limits_flag...
	#				new_ll new_ul multiplier power log shift
	#				NOTE: first line should be column names as above
	#
	#				testnum - if empty, use old_testname's testnum
	#				new_testname - testname as it will appear in out_file
	#				old_testname - testname as it appears in in_file, if
	#					empty, name is same as new_testname
	#				prefix - if empty and units empty, use prefix from old_testname
	#				units - if empty, use old_testname's units
	#				new_limits_flag - if empty, use old_testname's limits else...
	#				new_ll - lower limit to use for new test
	#				new_ul - upper limit to use for new test
	#				multiplier - if empty, =1
	#				power - if empty, =1
	#				log - if empty, =0= no log applied,else 1= 10*log(), 2=20*log() 
	#					if -1  10^(/10), if -2 10^(/20)
	#				shift - if empty, = 0
	#
	#				new_value = shift + [10/20 * log] (multiplier * orig_value^power)
	#
	# csv_dir -- if transform_csv is in a different directory, this is the
	#			  absolute path to that directory, else = ""
	#
	# out_file -- optional explicit output file name.  If empty,
	#				the output defaults to transformed.rtdf
	#
	# in_dir -- if in_file is in a different directory, this is the
	#			  absolute path to that directory
    #--------------------------------------------------------------------

    # define scaler prefixes
    #------------------------
    big_prefixes = c("10x","100x","K","10xK","100xK","M","10xM","100xM",
		    "G","10xG","100xG","T")
    lil_prefixes = c("d","c","m","100xu","10xu","u","100xn","10xn","n",
		    "100xp","10xp","p","100xf","10xf","f")

	all_prefixes = c("_",lil_prefixes,big_prefixes)
	prefix_xrefs = c(0,1:15,-1:-12)


	# read in transform.csv file
	#---------------------------
    if (transform_csv == "") {
        transform_csv <- readline("Enter the name of the transform.csv to read: ")
    }

	if (csv_dir != "") {
		my_dir = getwd()
		setwd(csv_dir)
	}
	CSV <- gzfile(transform_csv,"r")

	row1 <- scan(file=CSV,what=character(0),sep=",",strip.white=TRUE,
				multi.line=FALSE,fill=TRUE,nlines=1,quiet=TRUE)
	cols=12
	my_csv = scan(file=CSV,what=as.list(character(cols)),sep=",",
				strip.white=TRUE,multi.line=FALSE,fill=TRUE)
	close(CSV)
	if (csv_dir != "")  setwd(my_dir)

	tnums = as.integer(my_csv[[1]])
	testnames = my_csv[[2]]
	orig_testnames = my_csv[[3]]
	prefixes = my_csv[[4]]
	units = my_csv[[5]]
	limits_flags = as.integer(my_csv[[6]])
	limits_flags[which(is.na(limits_flags))] = 0
	lls = as.numeric(my_csv[[7]])
	uls = as.numeric(my_csv[[8]])
	multipliers = as.numeric(my_csv[[9]])
	multipliers[which(is.na(multipliers))] = 1
	powers = as.numeric(my_csv[[10]])
	powers[which(is.na(powers))] = 1
	logs = as.numeric(my_csv[[11]])
	logs[which(is.na(logs))] = 0
	shifts = as.numeric(my_csv[[12]])
	shifts[which(is.na(shifts))] = 0

	prefixes_factored = factor(prefixes,levels=all_prefixes)
	scalers = prefix_xrefs[as.integer(prefixes_factored)]

	#browser()
	
	# read in .rtdf file
	#-------------------
	if (in_dir != "") {
		my_dir = getwd()
		setwd(in_dir)
	}
	my_objects = load(in_file)
	if (in_dir != "")  setwd(my_dir)

	orig_ParametersFrame = ParametersFrame
	orig_ResultsMatrix = ResultsMatrix
	if(match("TestFlagMatrix",my_objs,nomatch=0)) {
		orig_TestFlagMatrix = TestFlagMatrix
	}
	orig_testname_list = as.character(ParametersFrame[["testname"]])


	# build new ParametersFrame and ResultsMatrix
	#--------------------------
	j=1
	for (i in 1:length(testnames)) {
		if(nchar(orig_testnames[i])>0)  my_testname = orig_testnames[i]
		else  my_testname = testnames[i]
		index = match(my_testname,orig_testname_list,nomatch=0)
		if (index>0) {
			if(is.finite(tnums[i]))  tnum = tnums[i]
			else  tnum = orig_ParametersFrame[[index,"testnum"]]
			if(prefixes[i]=="")  scaler = orig_ParametersFrame[[index,"scaler"]]
			else  scaler = scalers[i]
			scale = 10^scaler
			if(units[i]=="")  my_units = orig_ParametersFrame[[index,"units"]]
			else  my_units = units[i]
			if(limits_flags[i]>0) {
				ll = lls[i]/scale
				ul = uls[i]/scale
			} else {
				ll = orig_ParametersFrame[[index,"ll"]]
				ul = orig_ParametersFrame[[index,"ul"]]
			}
			m = multipliers[i]
			p = powers[i]
			s = shifts[i]/scale
			if (logs[i]>1.5) {
				results = s + 20.0*log10(m*orig_ResultsMatrix[index,]^p)
			} else if (logs[i]>0.5) {
				results = s + 10.0*log10(m*orig_ResultsMatrix[index,]^p)
			} else if (logs[i]<(-1.5)) {
				results = s + m*(10^(orig_ResultsMatrix[,index]/20.0))^p
			} else if (logs[i]<(-0.5)) {
				results = s + m*(10^(orig_ResultsMatrix[,index]/10.0))^p
			} else {
				results = s + m*orig_ResultsMatrix[,index]^p
			}
			if(match("TestFlagMatrix",my_objs,nomatch=0)) {
				testflags = orig_TestFlagMatrix[,index]
			}
			if (j==1) {
				Parameters_Names = testnames[i]
				Parameters_testnum = tnum
				Parameters_scaler = scaler
				Parameters_units = my_units
				Parameters_ll = ll
				Parameters_ul = ul
				ResultsMatrix = results
				if(match("TestFlagMatrix",my_objs,nomatch=0)) {
					TestFlagMatrix = testflags
				}
			} else {
				Parameters_Names[j] = testnames[i]
				Parameters_testnum[j] = tnum
				Parameters_scaler[j] = scaler
				Parameters_units[j] = my_units
				Parameters_ll[j] = ll
				Parameters_ul[j] = ul
				ResultsMatrix = cbind(ResultsMatrix,results)
				if(match("TestFlagMatrix",my_objs,nomatch=0)) {
					TestFlagMatrix = cbind(TestFlagMatrix,testflags)
				}
			}
			j=j+1
		} else {
			# nasty message, testname not found...
			cat(sprintf("Could not find test %s in rtdf file\n",my_testname))
		}
	}
	tests = j-1
	my_list = list(testnum=NaN, testname="",
		                scaler=NaN, units="",
		                ll=NaN, ul=NaN,
		                plot_ll=NaN, plot_ul=NaN)
	ParametersFrame <- data.frame(rbind(my_list))
	ParametersFrame[1:tests,"testnum"] <- Parameters_testnum
	ParametersFrame[1:tests,"testname"] <- Parameters_Names
	ParametersFrame[1:tests,"scaler"] <- Parameters_scaler
	ParametersFrame[1:tests,"units"] <- Parameters_units
	ParametersFrame[1:tests,"ll"] <- Parameters_ll
	ParametersFrame[1:tests,"ul"] <- Parameters_ul
	ParametersFrame[1:tests,"plot_ll"] <- rep(NaN,tests)
	ParametersFrame[1:tests,"plot_ul"] <- rep(NaN,tests)


	# save Rtdf file
    #-----------------
    save(list=my_objects, file=out_file)

}
