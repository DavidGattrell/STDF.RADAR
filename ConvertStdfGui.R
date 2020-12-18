# ConvertStdfGui.R
#
# $Id: ConvertStdfGui.R,v 1.27 2020/12/18 01:15:53 david Exp $
#
# Tk/Tcl GUI wrapper for calling ConvertStdf.R
# called by TkRadar.R
#
# Copyright (C) 2008-2015 David Gattrell
#               2018 David Gattrell
#               2020 David Gattrell
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

# ConvertStdfGui specific variables
#-----------------------------------
stdf_name <- tclVar("")			
stdf_dir <- tclVar("")
stdf_rtdf_name <- tclVar("")	
auto_93k <- tclVar(1)
do_summary <- tclVar(1)
just_fail_tests_summary <- tclVar(1)
endian <- tclVar("little")
do_conditions <- tclVar(0)
do_dtrs <- tclVar(0)
duplicate_testnames <- tclVar(0)
use_MPR_invalid_pf_data <- tclVar(0)
ltx_ignore_testname_objects <- tclVar(1)
do_testflag_matrix <- tclVar(0)
max_parts <- tclVar(-1)
max_parts_shadow <- tclVar(-1)
do_demangle <- tclVar(0)
auto_flex <- tclVar(1)
keep_alarmed_values <- tclVar(0)
do_raw_tsrs <- tclVar(0)
do_FTR_fail_cycle <- tclVar(1)
use_testorder <- tclVar(0)
save_testorder <- tclVar(0)
mult_limits <- tclVar(0)
mult_limits_shadow <- tclVar(0)

stdf_count <- tclVar(1)
stdf_index <- tclVar(1)
multi_stdf_names <- list()
multi_stdf_rtdfs <- list()
multi_stdf_names[[1]] <- tclVar("")
multi_stdf_rtdfs[[1]] <- tclVar("")

new_stdf <- tclVar("")					# for ExpandMPRsGui, so can reuse stdf_browser
multi_stdf_stdfs <- list()				# for ExpandMPRsGui, so can reuse stdf_browser
multi_stdf_stdfs[[1]] <- tclVar("")		# for ExpandMPRsGui, so can reuse stdf_browser

# these defaults can be controlled in the .Rprofile file:
default_do_summary <- tclVar(1)					# per user customizing
default_just_fail_tests_summary <- tclVar(1)	# per user customizing
default_do_conditions <- tclVar(0)				# per user customizing
default_do_dtrs <- tclVar(0)					# per user customizing
default_duplicate_testnames <- tclVar(0)		# per user customizing
default_use_MPR_invalid_pf_data <- tclVar(0)	# per user customizing
default_ltx_ignore_testname_objects <- tclVar(1) # per user customizing
default_do_testflag_matrix <- tclVar(0)			# per user customizing
default_keep_alarmed_values <- tclVar(0)	
default_do_raw_tsrs <- tclVar(0)	
default_do_FTR_fail_cycle <- tclVar(1)	
default_use_testorder <- tclVar(0)
default_save_testorder <- tclVar(0)
default_mult_limits <- tclVar(0)


#----------------------------------------------------
stdf_browser <-function() {

	my_dir <- paste(tclObj(stdf_dir),sep="",collapse=" ")
	if (nchar(my_dir)<1) {
		my_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(my_dir)<1) {
		my_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	if(as.numeric(tclObj(Bad_Vista))>0) {
		my_str = "{{All files} *} {{stdf Files} {.stdf .std .stdf.gz .std.gz}}"
	} else {
		# TkgetOpenFile really wants an explicit extension... *_STDF doesn't work
		#my_str = "{{stdf Files} {.stdf .std .stdf.gz .std.gz}} {{STDF Files} {*_STDF *_STDF.gz}} {{All files} *}"
		my_str = "{{stdf Files} {.stdf .std .stdf.gz .std.gz}} {{All files} *}"
	}
	if (nchar(my_dir)>0) {
		name <- tclvalue(tkgetOpenFile(filetypes=my_str,
				initialdir=my_dir,multiple=TRUE))
	} else {
		name <- tclvalue(tkgetOpenFile(filetypes=my_str,
				multiple=TRUE))
	}
	names = split_multiple_filenames(name)

	if(length(names)>0) {
		for (j in 1:length(names)) {
			name = names[j]
			if (nchar(name)>0) {
				tclvalue(stdf_count) <- length(names)

				# the tkgetOpenFile returns the full path!
				# separate filename and path
				my_filepath = sub("/[^/]*$","",name)	
				name=sub("^.*/","",name)

				# update name and path variables
				if (length(multi_stdf_names)<j)  multi_stdf_names[[j]] <<- tclVar(name)
				else  tclvalue(multi_stdf_names[[j]]) <- name

				if (j==1) {
					index <- as.integer(tclObj(stdf_index))
					if (index>length(names))  tclvalue(stdf_index) <- 1

					first_path = my_filepath
					out_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
					if (nchar(out_dir)<1) {
						out_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
					}
					if (nchar(out_dir)<1) {
						out_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
					}
					if(my_filepath == out_dir)  tclvalue(stdf_dir) <- ""
					else  tclvalue(stdf_dir) <- my_filepath
				} else if (!(first_path %in% my_filepath)) {
					# nasty message... shouldn't get here...
				}

				#my_rtdf <- sub("(.stdf)?(.std)?(.gz)?$",".rtdf",name)
				my_rtdf <- sub("(_STDF)?(.stdf)?(.std)?(.gz)?$",".rtdf",name)
				if (length(multi_stdf_rtdfs)<j)  multi_stdf_rtdfs[[j]] <<- tclVar(my_rtdf)
				else  tclvalue(multi_stdf_rtdfs[[j]]) <- my_rtdf

				# for ExpandMPRs script
				my_stdf <- sub("(_STDF)?(.stdf)?(.gz)?)$","_ptr\\1",name)
				if (length(multi_stdf_stdfs)<j)  multi_stdf_stdfs[[j]] <<- tclVar(my_stdf)
				else  tclvalue(multi_stdf_stdfs[[j]]) <- my_stdf
			}
		}
		index <- as.integer(tclObj(stdf_index))
		tclvalue(stdf_name) <- tclObj(multi_stdf_names[[index]])
		tclvalue(stdf_rtdf_name) <- tclObj(multi_stdf_rtdfs[[index]])
		tclvalue(new_stdf) <- tclObj(multi_stdf_stdfs[[index]])
	}
}

#----------------------------------------------------
convertstdf_rtdf_browser <-function(...) {
	orig_name = paste(tclObj(stdf_rtdf_name),sep="",collapse=" ")
	if (nchar(orig_name)<1)  orig_name="converted.rtdf"

	init_dir = paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
	if (nchar(init_dir)<1) {
		init_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(init_dir)<1) {
		init_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	if(as.numeric(tclObj(Bad_Vista))>0) {
		my_str = "{{All files} *} {{RTDF Files} {.rtdf .Rtdf .Rdata}}"
	} else {
		my_str = "{{RTDF Files} {.rtdf .Rtdf .Rdata}} {{All files} *}"
	}
	if (nchar(init_dir)>0) {
		name <- tclvalue(tkgetSaveFile(
				initialdir=init_dir,
				initialfile=orig_name,
				filetypes=my_str))
	} else {
		name <- tclvalue(tkgetSaveFile(
				initialfile=orig_name,
				filetypes=my_str))
	}
	# the tkgetSaveFile returns the full path!
	if (nchar(name)>0) {
		# separate filename and path
		my_filepath = sub("/[^/]*$","",name)	
		name=sub("^.*/","",name)

		# update name and path variables
		tclvalue(stdf_rtdf_name) <- name
		index <- as.integer(tclObj(stdf_index))
		tclvalue(multi_stdf_rtdfs[[index]]) <- name
		out_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
		if(my_filepath != out_dir) {
			change_Rtdfs_dir(my_filepath)
		}
	}
}

#-----------------------------------------------------
inc_index <- function() {
	my_value <- as.integer(tclObj(stdf_index))
	tclvalue(multi_stdf_names[[my_value]]) <- tclObj(stdf_name)

	tclvalue(multi_stdf_stdfs[[my_value]]) <- tclObj(new_stdf)
	tclvalue(multi_stdf_rtdfs[[my_value]]) <- tclObj(stdf_rtdf_name)

	my_count <- as.integer(tclObj(stdf_count))
	if (my_value<my_count) {
		my_value <- my_value + 1
		tclvalue(stdf_name) <- tclObj(multi_stdf_names[[my_value]])
		tclvalue(new_stdf) <- tclObj(multi_stdf_stdfs[[my_value]])
		tclvalue(stdf_rtdf_name) <- tclObj(multi_stdf_rtdfs[[my_value]])
	} 
	tclvalue(stdf_index) <- my_value
}

#-----------------------------------------------------
dec_index <- function() {
	my_value <- as.integer(tclObj(stdf_index))
	tclvalue(multi_stdf_names[[my_value]]) <- tclObj(stdf_name)

	tclvalue(multi_stdf_stdfs[[my_value]]) <- tclObj(new_stdf)
	tclvalue(multi_stdf_rtdfs[[my_value]]) <- tclObj(stdf_rtdf_name)

	if (my_value>1) {
		my_value <- my_value - 1
		tclvalue(stdf_name) <- tclObj(multi_stdf_names[[my_value]])
		tclvalue(new_stdf) <- tclObj(multi_stdf_stdfs[[my_value]])
		tclvalue(stdf_rtdf_name) <- tclObj(multi_stdf_rtdfs[[my_value]])
	}
	tclvalue(stdf_index) <- my_value
}

#----------------------------------------------------
ConvertStdfGui_defaults <- function() {
	tclvalue(stdf_name) <- ""
#	tclvalue(stdf_dir) <- ""
	tclvalue(stdf_rtdf_name) <- ""

#	tclvalue(multiple_stdf) <- 0
	tclvalue(stdf_count) <- 1
	tclvalue(stdf_index) <- 1
	tclvalue(multi_stdf_names[[1]]) <- tclVar("")
	tclvalue(multi_stdf_rtdfs[[1]]) <- tclVar("")

	tclvalue(auto_93k) <- 1
	tclvalue(do_summary) <- tclObj(default_do_summary)
	tclvalue(just_fail_tests_summary) <- tclObj(default_just_fail_tests_summary)
	tclvalue(endian) <- "little"
	tclvalue(do_conditions) <- tclObj(default_do_conditions)
	tclvalue(do_dtrs) <- tclObj(default_do_dtrs)
	tclvalue(duplicate_testnames) <- tclObj(default_duplicate_testnames)
	tclvalue(use_MPR_invalid_pf_data) <- tclObj(default_use_MPR_invalid_pf_data)
	tclvalue(ltx_ignore_testname_objects) <- tclObj(default_ltx_ignore_testname_objects)
	tclvalue(do_testflag_matrix) <- tclObj(default_do_testflag_matrix)
	tclvalue(max_parts) <- -1
	tclvalue(max_parts_shadow) <- -1
	tclvalue(do_demangle) <- 0
	tclvalue(auto_flex) <- 1
	tclvalue(keep_alarmed_values) <- tclObj(default_keep_alarmed_values)
	tclvalue(do_raw_tsrs) <- tclObj(default_do_raw_tsrs)
	tclvalue(do_FTR_fail_cycle) <- tclObj(default_do_FTR_fail_cycle)
	tclvalue(use_testorder) <- tclObj(default_use_testorder)
	tclvalue(save_testorder) <- tclObj(default_save_testorder)
	tclvalue(mult_limits) <- tclObj(default_mult_limits)
	tclvalue(mult_limits_shadow) <- tclObj(default_mult_limits)
}

#----------------------------------------------------
run_ConvertStdf <-function(done=FALSE,...) {
	my_value <- as.integer(tclObj(stdf_index))
	tclvalue(multi_stdf_names[[my_value]]) <- tclObj(stdf_name)
	tclvalue(multi_stdf_rtdfs[[my_value]]) <- tclObj(stdf_rtdf_name)

#	stdf_name_ <- as.character(tclObj(stdf_name))
#	rtdf_name_ <- as.character(tclObj(stdf_rtdf_name))
	auto_93k_ <- as.logical(tclObj(auto_93k))
	do_summary_ <- as.logical(tclObj(do_summary))
	just_fails <- as.logical(tclObj(just_fail_tests_summary))
	endian_ <- as.character(tclObj(endian))
	stdf_dir_ <- paste(tclObj(stdf_dir),sep="",collapse=" ")
	output_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
	do_conditions_ <- as.logical(tclObj(do_conditions))
	duplicate_testnames_ <- as.logical(tclObj(duplicate_testnames))
	use_MPR_invalid_pf_data_ <- as.logical(tclObj(use_MPR_invalid_pf_data))
	ltx_ignore_testname_objects_ <- as.logical(tclObj(ltx_ignore_testname_objects))
	count <- as.integer(tclObj(stdf_count))
	do_testflag_matrix_ <- as.logical(tclObj(do_testflag_matrix))
	do_dtrs_ <- as.logical(tclObj(do_dtrs))
	max_parts_ <- as.integer(tclObj(max_parts))
	if(!is.finite(max_parts_))  max_parts_ = -1
	auto_demangle_ <- as.logical(tclObj(do_demangle))
	auto_flex_ <- as.logical(tclObj(auto_flex))
	keep_alarmed_values_ <- as.logical(tclObj(keep_alarmed_values))
	do_raw_tsrs_ <- as.logical(tclObj(do_raw_tsrs))
	do_FTR_fail_cycle_ <- as.logical(tclObj(do_FTR_fail_cycle))
	use_testorder_ <- as.logical(tclObj(use_testorder))
	save_testorder_ <- as.integer(tclObj(save_testorder))
	mult_limits_ <- as.integer(tclObj(mult_limits))
	if(!is.finite(mult_limits_))  mult_limits_ = 0
		
	# go to output directory...
	full_path = output_dir
	if (nchar(full_path)<1)  full_path <- paste(tclObj(Output_dir),sep="",collapse=" ")
	if (nchar(full_path)<1)  full_path <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	setwd(full_path)

	for (j in 1:count) {
		stdf_name_ <- paste(tclObj(multi_stdf_names[[j]]),sep="",collapse=" ")
		rtdf_name_ <- paste(tclObj(multi_stdf_rtdfs[[j]]),sep="",collapse=" ")

		my_expr = substitute(
			ConvertStdf(stdf_name=stdf_name_,rtdf_name=rtdf_name_,
						auto_93k=auto_93k_,do_summary=do_summary_,
						just_fail_tests_summary=just_fails,
						endian=endian_,stdf_dir=stdf_dir_,
						do_conditions=do_conditions_,
						duplicate_testnames=duplicate_testnames_,
						use_MPR_invalid_pf_data=use_MPR_invalid_pf_data_,
						ltx_ignore_testname_objects=ltx_ignore_testname_objects_,
						do_testflag_matrix=do_testflag_matrix_,
						do_DTRs=do_dtrs_,max_parts=max_parts_,
						auto_demangle=auto_demangle_,auto_flex=auto_flex_,
						keep_alarmed_values=keep_alarmed_values_,
						raw_TSRs=do_raw_tsrs_,do_FTR_fail_cycle=do_FTR_fail_cycle_,
						use_testorder_matrix=use_testorder_,save_testorder_matrix=save_testorder_,
						mult_limits=mult_limits_)
		)
		tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
		tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
		my_command = sprintf("%s\n",deparse(my_expr))
		my_cmnd = "ConvertStdf(...)\n"

		# dump timestamp and command to log file...
		if (nchar(tkradar_logfile)>0) {
			cat(sprintf("# %s\n",date()),file=tkradar_logfile,append=TRUE)
			cat(my_command,file=tkradar_logfile,append=TRUE)
		}
		# print command to console window...
		if (tkradar_verbose>0) {
			if(tkradar_verbose>=length(my_command)) {
				cat(my_command)
			} else if(tkradar_verbose<2) {
				cat(sprintf("%s      ...)\n",my_command[1]))
			} else {
				cat(sprintf("%s      ...)\n",
						paste(my_command[1:tkradar_verbose],sep="",collapse="")))
			}
		} else if (tkradar_verbose<0) {
			cat(my_command)
		} else {
			cat(my_cmnd)
		}

		# run command...
		eval(my_expr)
		#cat("Finished!\n")
	}

	tclvalue(Rtdf_name) <- rtdf_name_
	tclvalue(Rtdf_dir) <- output_dir
		
	if(done>0) {
		convertstdf_win <- get("convertstdf_win",envir=.TkRadar.wins)
		tkdestroy(convertstdf_win)
	}
}

#----------------------------------------------------
ConvertStdfGui <- function() {

	ConvertStdfGui_defaults()		# initialize variables...
	convertstdf_win <- tktoplevel()
	assign("convertstdf_win",convertstdf_win,envir=.TkRadar.wins)
	tkwm.title(convertstdf_win, "ConvertStdf")
		
	# yellow/white background stuff needs to be defined first... before DEFAULTS button
	maxparts_entry_frame <- tkframe(convertstdf_win)
	maxparts_entry <- tkentry(maxparts_entry_frame,
						width=30,
						background="white",
						textvariable=max_parts_shadow)

	multlim_entry_frame <- tkframe(convertstdf_win)
	multlim_entry <- tkentry(multlim_entry_frame,
						width=30,
						background="white",
						textvariable=mult_limits_shadow)


	bottom_row <- tkframe(convertstdf_win)
	default_button <- tkbutton(bottom_row,
						text="DEFAULTS",
						#anchor="w",
						width=12,
						command=function() {
							ConvertStdfGui_defaults()
							tkconfigure(maxparts_entry,background="white")
							tkconfigure(multlim_entry,background="white")
						})
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
						text="RUN & QUIT",
						#anchor="w",
						width=12,
						command=function() run_ConvertStdf(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
						text="QUIT",
						#anchor="w",
						width=12,
						command=function()tkdestroy(convertstdf_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
						text="RUN",
						#anchor="w",
						width=12,
						command=function() run_ConvertStdf(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	# - - - 

	dir_entry_frame <- tkframe(convertstdf_win)
	dir_entry_label <- tklabel(dir_entry_frame,
						width=10,
						text="Rtdfs_dir")
	tkpack(dir_entry_label,side="left")
	dir_entry <- tklabel(dir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=Rtdfs_dir)
	tkpack(dir_entry,side="left",fill="x",expand=1)
	dir_browse <- tkbutton(dir_entry_frame,
						text="Browse",
						command=function() dir_browser(Rtdfs_dir))
	tkpack(dir_browse,side="right")
	tkpack(dir_entry_frame,side="top",anchor="w",fill="x")

	stdf_dir_entry_frame <- tkframe(convertstdf_win)
	stdf_dir_entry_label <- tklabel(stdf_dir_entry_frame,
						width=10,
						text="stdf_dir")
	tkpack(stdf_dir_entry_label,side="left")
	stdf_dir_entry <- tklabel(stdf_dir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=stdf_dir)
	tkpack(stdf_dir_entry,side="left",fill="x",expand=1)
	stdf_dir_browse <- tkbutton(stdf_dir_entry_frame,
						text="Browse",
						command=function() dir_browser(stdf_dir))
	tkpack(stdf_dir_browse,side="right")
	tkpack(stdf_dir_entry_frame,side="top",anchor="w",fill="x")

	multiple_frame <- tkframe(convertstdf_win)
	multiple_label <- tklabel(multiple_frame,
						#width=10,
						text="file")
	tkpack(multiple_label,side="left")
	index_value <- tklabel(multiple_frame,
						width=3,
						relief="sunken",
						textvariable=stdf_index)
	tkpack(index_value,side="left")
	multiple_label2 <- tklabel(multiple_frame,
						#width=10,
						text="of")
	tkpack(multiple_label2,side="left")
	count_value <- tklabel(multiple_frame,
						width=3,
						relief="sunken",
						textvariable=stdf_count)
	tkpack(count_value,side="left")
	index_minus <- tkbutton(multiple_frame,
						text="-",
						command=function() dec_index())
	tkpack(index_minus,side="left")
	index_plus <- tkbutton(multiple_frame,
						text="+",
						command=function() inc_index())
	tkpack(index_plus,side="left")
	tkpack(multiple_frame,side="top",anchor="w",fill="x")

	stdf_entry_frame <- tkframe(convertstdf_win)
	stdf_entry_label <- tklabel(stdf_entry_frame,
						width=10,
						text="stdf_name")
	tkpack(stdf_entry_label,side="left")
	stdf_entry <- tkentry(stdf_entry_frame,
						width=50,
						textvariable=stdf_name)
	tkpack(stdf_entry,side="left",fill="x",expand=1)
	stdf_browse <- tkbutton(stdf_entry_frame,
						text="Browse",
						command=stdf_browser)
	tkpack(stdf_browse,side="right")
	tkpack(stdf_entry_frame,side="top",anchor="w",fill="x")

	rtdf_entry_frame <- tkframe(convertstdf_win)
	rtdf_entry_label <- tklabel(rtdf_entry_frame,
						width=10,
						text="rtdf_name")
	tkpack(rtdf_entry_label,side="left")
	rtdf_entry <- tkentry(rtdf_entry_frame,
						width=20,
						textvariable=stdf_rtdf_name)
	tkpack(rtdf_entry,side="left",fill="x",expand=1)
	rtdf_browse <- tkbutton(rtdf_entry_frame,
						text="Browse",
						command=convertstdf_rtdf_browser)
	tkpack(rtdf_browse,side="right")
	tkpack(rtdf_entry_frame,side="top",anchor="w",fill="x")

	auto93_button <- tkcheckbutton(convertstdf_win,
						text="auto_93k",
						variable=auto_93k)
	tkpack(auto93_button,side="top",anchor="w")

	sum_button <- tkcheckbutton(convertstdf_win,
						text="do_summary",
						variable=do_summary)
	tkpack(sum_button,side="top",anchor="w")

	sumfails_button <- tkcheckbutton(convertstdf_win,
						text="just_fail_tests_summary",
						variable=just_fail_tests_summary)
	tkpack(sumfails_button,side="top",anchor="w")

	cond_button <- tkcheckbutton(convertstdf_win,
						text="do_conditions",
						variable=do_conditions)
	tkpack(cond_button,side="top",anchor="w")

	dtr_button <- tkcheckbutton(convertstdf_win,
						text="do_dtrs",
						variable=do_dtrs)
	tkpack(dtr_button,side="top",anchor="w")

	dupl_button <- tkcheckbutton(convertstdf_win,
						text="duplicate_testnames",
						variable=duplicate_testnames)
	tkpack(dupl_button,side="top",anchor="w")

	invalid_pf_button <- tkcheckbutton(convertstdf_win,
						text="use_MPR_invalid_pf_data",
						variable=use_MPR_invalid_pf_data)
	tkpack(invalid_pf_button,side="top",anchor="w")

	ltx_tname_button <- tkcheckbutton(convertstdf_win,
						text="ltx_ignore_testname_objects",
						variable=ltx_ignore_testname_objects)
	tkpack(ltx_tname_button,side="top",anchor="w")

	tflag_mtx_button <- tkcheckbutton(convertstdf_win,
						text="do_testflag_matrix",
						variable=do_testflag_matrix)
	tkpack(tflag_mtx_button,side="top",anchor="w")

	demangle_button <- tkcheckbutton(convertstdf_win,
						text="auto_demangle",
						variable=do_demangle)
	tkpack(demangle_button,side="top",anchor="w")

	autoflex_button <- tkcheckbutton(convertstdf_win,
						text="auto_flex",
						variable=auto_flex)
	tkpack(autoflex_button,side="top",anchor="w")

	keep_alarmed_button <- tkcheckbutton(convertstdf_win,
						text="keep_alarmed_values",
						variable=keep_alarmed_values)
	tkpack(keep_alarmed_button,side="top",anchor="w")

	do_raw_tsrs_button <- tkcheckbutton(convertstdf_win,
						text="raw_TSRs",
						variable=do_raw_tsrs)
	tkpack(do_raw_tsrs_button,side="top",anchor="w")

	do_FTR_fcycles_button <- tkcheckbutton(convertstdf_win,
						text="do_FTR_fail_cycle",
						variable=do_FTR_fail_cycle)
	tkpack(do_FTR_fcycles_button,side="top",anchor="w")

	testorder_frame <- tkframe(convertstdf_win)
	testorder_label <- tklabel(testorder_frame, text="TestOrderMatrix:")
	tkpack(testorder_label,side="left")
	testorder_use <- tkcheckbutton(testorder_frame,
					text="use",
					variable=use_testorder)
	tkpack(testorder_use,side="left")
	testorder_save <- tkcheckbutton(testorder_frame,
					text="save",
					variable=save_testorder)
	tkpack(testorder_save,side="left")
	tkpack(testorder_frame,side="top",anchor="w")

	endian_frame <- tkframe(convertstdf_win)
	endian_label <- tklabel(endian_frame, text="default endian:")
	tkpack(endian_label,side="left")
	endian_big  <- tkradiobutton(endian_frame,
					text="big",
					value="big",
					variable=endian)
	tkpack(endian_big,side="left")
	endian_little <- tkradiobutton(endian_frame,
					text="little",
					value="little",
					variable=endian)
	tkpack(endian_little,side="left")
	tkpack(endian_frame,side="top",anchor="w")

	# maxparts_entry_frame defined prior to DEFAULTS button
	maxparts_entry_label <- tklabel(maxparts_entry_frame,
						width=15,
						text="max_parts")
	tkpack(maxparts_entry_label,side="left")
	# maxparts_entry defined prior to DEFAULTS button
	tkpack(maxparts_entry,side="left",fill="x",expand=1)
	tkbind(maxparts_entry,"<KeyRelease>",function() {
					tmp <- as.integer(tclObj(max_parts_shadow))
					if( (length(tmp)>0) && is.finite(tmp)) {
						tkconfigure(maxparts_entry,background="white")
						tclvalue(max_parts) <- tmp
					} else {
						tkconfigure(maxparts_entry,background="yellow")
					}
					tcl('update')
				})
	tkpack(maxparts_entry_frame,side="top",anchor="w",fill="x")

	# multlim_entry_frame defined prior to DEFAULTS button
	multlim_entry_label <- tklabel(multlim_entry_frame,
						width=15,
						text="mult_limits")
	tkpack(multlim_entry_label,side="left")
	# multlim_entry defined prior to DEFAULTS button
	tkpack(multlim_entry,side="left",fill="x",expand=1)
	tkbind(multlim_entry,"<KeyRelease>",function() {
					tmp <- as.integer(tclObj(mult_limits_shadow))
					if( (length(tmp)>0) && is.finite(tmp)) {
						tkconfigure(multlim_entry,background="white")
						tclvalue(mult_limits) <- tmp
					} else {
						tkconfigure(multlim_entry,background="yellow")
					}
					tcl('update')
				})
	tkpack(multlim_entry_frame,side="top",anchor="w",fill="x")
}


