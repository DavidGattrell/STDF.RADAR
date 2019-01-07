# ConvertEagleCSVGui.R
#
# $Id: ConvertEagleCSVGui.R,v 1.2 2012/04/04 01:02:53 David Exp $
#
# Tk/Tcl GUI wrapper for calling ConvertEagleCSV.R
# called by TkRadar.R
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
#----------------------------------------------------------

# ConvertEagleCSVGui specific variables
#-----------------------------------
csvETS_name <- tclVar("")
csvETS_dir <- tclVar("")
csvETS_rtdf_name <- tclVar("")

csvETS_do_summary <- tclVar(1)
csvETS_just_fail_tests_summary <- tclVar(1)
csvETS_duplicate_testnames <- tclVar(0)

csvETS_count <- tclVar(1)
csvETS_index <- tclVar(1)
multi_csvETS_names <- list()
multi_csvETS_rtdfs <- list()
multi_csvETS_names[[1]] <- tclVar("")
multi_csvETS_rtdfs[[1]] <- tclVar("")

# these defaults can be controlled in the .Rprofile file
# or in the DefaultsGUI... per user customizing or per product
default_csvETS_do_summary <- tclVar(1)
default_csvETS_just_fail_tests_summary <- tclVar(1)
default_csvETS_duplicate_testnames <- tclVar(0)

#----------------------------------------------------
csvETS_browser <-function() {

	my_dir <- paste(tclObj(csvETS_dir),sep="",collapse=" ")
	if (nchar(my_dir)<1) {
		my_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if(as.numeric(tclObj(Bad_Vista))>0) {
		# my_str = "{{All files} *} {{Eagle CSV log Files} {.log .log.gz .csv .csv.gz}}"
		my_str = "{{All files} *} {{Eagle .log or .csv Files} {.log .log.gz .csv .csv.gz}} {{Eagle CSV log Files} {.log .log.gz}}"
	} else {
		# my_str = "{{Eagle CSV log Files} {.log .log.gz .csv .csv.gz}} {{All files} *}"
		my_str = "{{Eagle CSV log Files} {.log .log.gz}} {{Eagle .log or .csv Files} {.log .log.gz .csv .csv.gz}} {{All files} *}"
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
				tclvalue(csvETS_count) <- length(names)

				# the tkgetOpenFile returns the full path!
				# separate filename and path
				my_filepath = sub("/[^/]*$","",name)	
				name=sub("^.*/","",name)

				# update name and path variables
				if (length(multi_csvETS_names)<j)  multi_csvETS_names[[j]] <<- tclVar(name)
				else  tclvalue(multi_csvETS_names[[j]]) <- name

				if (j==1) {
					index <- as.integer(tclObj(csvETS_index))
					if (index>length(names))  tclvalue(csvETS_index) <- 1

					first_path = my_filepath
					out_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
					if(my_filepath == out_dir)  tclvalue(csvETS_dir) <- ""
					else  tclvalue(csvETS_dir) <- my_filepath
				} else if (!(first_path %in% my_filepath)) {
					# nasty message... shouldn't get here...
				}

				my_rtdf <- sub("([.]log)?([.]gz)?$",".rtdf",name)
				if (length(multi_csvETS_rtdfs)<j)  multi_csvETS_rtdfs[[j]] <<- tclVar(my_rtdf)
				else  tclvalue(multi_csvETS_rtdfs[[j]]) <- my_rtdf
			}
		}
		index <- as.integer(tclObj(csvETS_index))
		tclvalue(csvETS_name) <- tclObj(multi_csvETS_names[[index]])
		tclvalue(csvETS_rtdf_name) <- tclObj(multi_csvETS_rtdfs[[index]])
	}
}

#----------------------------------------------------
convertcsvETS_rtdf_browser <-function(...) {
	orig_name = paste(tclObj(csvETS_rtdf_name),sep="",collapse=" ")
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
		tclvalue(csvETS_rtdf_name) <- name
		index <- as.integer(tclObj(csvETS_index))
		tclvalue(multi_csvETS_rtdfs[[index]]) <- name
		out_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
		if(my_filepath != out_dir) {
			tclvalue(Rtdf_dir) <- my_filepath
			change_Rtdfs_dir(my_filepath)
		} else {
			tclvalue(Rtdf_dir) <- ""
		}
	}
}

#-----------------------------------------------------
inc_csvETS_index <- function() {
	my_value <- as.integer(tclObj(csvETS_index))
	tclvalue(multi_csvETS_names[[my_value]]) <- tclObj(csvETS_name)
	tclvalue(multi_csvETS_rtdfs[[my_value]]) <- tclObj(csvETS_rtdf_name)

	my_count <- as.integer(tclObj(csvETS_count))
	if (my_value<my_count) {
		my_value <- my_value + 1
		tclvalue(csvETS_name) <- tclObj(multi_csvETS_names[[my_value]])
		tclvalue(csvETS_rtdf_name) <- tclObj(multi_csvETS_rtdfs[[my_value]])
	} 
	tclvalue(csvETS_index) <- my_value
}

#-----------------------------------------------------
dec_csvETS_index <- function() {
	my_value <- as.integer(tclObj(csvETS_index))
	tclvalue(multi_csvETS_names[[my_value]]) <- tclObj(csvETS_name)
	tclvalue(multi_csvETS_rtdfs[[my_value]]) <- tclObj(csvETS_rtdf_name)

	if (my_value>1) {
		my_value <- my_value - 1
		tclvalue(csvETS_name) <- tclObj(multi_csvETS_names[[my_value]])
		tclvalue(csvETS_rtdf_name) <- tclObj(multi_csvETS_rtdfs[[my_value]])
	}
	tclvalue(csvETS_index) <- my_value
}

#----------------------------------------------------
ConvertEagleCSVGui_defaults <- function() {
	tclvalue(csvETS_name) <- ""
#	tclvalue(a5xx_dir) <- ""
	tclvalue(csvETS_rtdf_name) <- ""

	tclvalue(csvETS_count) <- 1
	tclvalue(csvETS_index) <- 1
	tclvalue(multi_csvETS_names[[1]]) <- tclVar("")
	tclvalue(multi_csvETS_rtdfs[[1]]) <- tclVar("")

	tclvalue(csvETS_do_summary) <- tclObj(default_csvETS_do_summary)
	tclvalue(csvETS_just_fail_tests_summary) <- tclObj(default_csvETS_just_fail_tests_summary)
	tclvalue(csvETS_duplicate_testnames) <- tclObj(default_csvETS_duplicate_testnames)

}

#----------------------------------------------------
run_ConvertEagleCSV <-function(done=FALSE,...) {
	my_value <- as.integer(tclObj(csvETS_index))
	tclvalue(multi_csvETS_names[[my_value]]) <- tclObj(csvETS_name)
	tclvalue(multi_csvETS_rtdfs[[my_value]]) <- tclObj(csvETS_rtdf_name)

	#a5xx_name_ <- as.character(tclObj(a5xx_name))
	#rtdf_name_ <- as.character(tclObj(a5xx_rtdf_name))
	csvETS_dir_ <- paste(tclObj(csvETS_dir),sep="",collapse=" ")
	output_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")

	do_summary_ <- as.logical(tclObj(csvETS_do_summary))
	just_fails <- as.logical(tclObj(csvETS_just_fail_tests_summary))
	duplicate_testnames_ <- as.logical(tclObj(csvETS_duplicate_testnames))

	full_path = output_dir
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	setwd(full_path)

	count <- as.integer(tclObj(csvETS_count))
	for (j in 1:count) {
		csvETS_name_ <- paste(tclObj(multi_csvETS_names[[j]]),sep="",collapse=" ")
		rtdf_name_ <- paste(tclObj(multi_csvETS_rtdfs[[j]]),sep="",collapse=" ")

		my_expr = substitute(
			ConvertEagleCSV(ets_name=csvETS_name_,rtdf_name=rtdf_name_,
						ets_dir=csvETS_dir_,
						do_summary=do_summary_,
						just_fail_tests_summary=just_fails,
						duplicate_testnames=duplicate_testnames_)
		)
		tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
		tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
		my_command = sprintf("%s\n",deparse(my_expr))
		my_cmnd = "ConvertEagleCSV(...)\n"
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
	}

	tclvalue(Rtdf_name) <- rtdf_name_
	tclvalue(Rtdf_dir) <- output_dir
		
	if(done>0) {
		convertcsvETS_win <- get("convertcsvETS_win",envir=.TkRadar.wins)
		tkdestroy(convertcsvETS_win)
	}
}

#----------------------------------------------------
ConvertEagleCSVGui <- function() {

	ConvertEagleCSVGui_defaults()		# initialize variables...
	convertcsvETS_win <- tktoplevel()
	assign("convertcsvETS_win",convertcsvETS_win,envir=.TkRadar.wins)
	tkwm.title(convertcsvETS_win, "ConvertEagleCSV")
		
	bottom_row <- tkframe(convertcsvETS_win)
	default_button <- tkbutton(bottom_row,
						text="DEFAULTS",
						#anchor="w",
						width=12,
						command=ConvertEagleCSVGui_defaults)
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
						text="RUN & QUIT",
						#anchor="w",
						width=12,
						command=function() run_ConvertEagleCSV(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
						text="QUIT",
						#anchor="w",
						width=12,
						command=function()tkdestroy(convertcsvETS_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
						text="RUN",
						#anchor="w",
						width=12,
						command=function() run_ConvertEagleCSV(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	dir_entry_frame <- tkframe(convertcsvETS_win)
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

	csvETS_dir_entry_frame <- tkframe(convertcsvETS_win)
	csvETS_dir_entry_label <- tklabel(csvETS_dir_entry_frame,
						width=10,
						text="ets_dir")
	tkpack(csvETS_dir_entry_label,side="left")
	csvETS_dir_entry <- tklabel(csvETS_dir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=csvETS_dir)
	tkpack(csvETS_dir_entry,side="left",fill="x",expand=1)
	csvETS_dir_browse <- tkbutton(csvETS_dir_entry_frame,
						text="Browse",
						command=function() dir_browser(csvETS_dir))
	tkpack(csvETS_dir_browse,side="right")
	tkpack(csvETS_dir_entry_frame,side="top",anchor="w",fill="x")

	multiple_frame <- tkframe(convertcsvETS_win)
	multiple_label <- tklabel(multiple_frame,
						#width=10,
						text="file")
	tkpack(multiple_label,side="left")
	index_value <- tklabel(multiple_frame,
						width=3,
						relief="sunken",
						textvariable=csvETS_index)
	tkpack(index_value,side="left")
	multiple_label2 <- tklabel(multiple_frame,
						#width=10,
						text="of")
	tkpack(multiple_label2,side="left")
	count_value <- tklabel(multiple_frame,
						width=3,
						relief="sunken",
						textvariable=csvETS_count)
	tkpack(count_value,side="left")
	index_minus <- tkbutton(multiple_frame,
						text="-",
						command=dec_csvETS_index)
	tkpack(index_minus,side="left")
	index_plus <- tkbutton(multiple_frame,
						text="+",
						command=inc_csvETS_index)
	tkpack(index_plus,side="left")
	tkpack(multiple_frame,side="top",anchor="w",fill="x")

	csvETS_entry_frame <- tkframe(convertcsvETS_win)
	csvETS_entry_label <- tklabel(csvETS_entry_frame,
						width=10,
						text="ets_name")
	tkpack(csvETS_entry_label,side="left")
	csvETS_entry <- tkentry(csvETS_entry_frame,
						width=50,
						textvariable=csvETS_name)
	tkpack(csvETS_entry,side="left",fill="x",expand=1)
	csvETS_browse <- tkbutton(csvETS_entry_frame,
						text="Browse",
						command=csvETS_browser)
	tkpack(csvETS_browse,side="right")
	tkpack(csvETS_entry_frame,side="top",anchor="w",fill="x")

	rtdf_entry_frame <- tkframe(convertcsvETS_win)
	rtdf_entry_label <- tklabel(rtdf_entry_frame,
						width=10,
						text="rtdf_name")
	tkpack(rtdf_entry_label,side="left")
	rtdf_entry <- tkentry(rtdf_entry_frame,
						width=20,
						textvariable=csvETS_rtdf_name)
	tkpack(rtdf_entry,side="left",fill="x",expand=1)
	rtdf_browse <- tkbutton(rtdf_entry_frame,
						text="Browse",
						command=convertcsvETS_rtdf_browser)
	tkpack(rtdf_browse,side="right")
	tkpack(rtdf_entry_frame,side="top",anchor="w",fill="x")

	sum_button <- tkcheckbutton(convertcsvETS_win,
						text="do_summary",
						variable=csvETS_do_summary)
	tkpack(sum_button,side="top",anchor="w")

	sumfails_button <- tkcheckbutton(convertcsvETS_win,
						text="just_fail_tests_summary",
						variable=csvETS_just_fail_tests_summary)
	tkpack(sumfails_button,side="top",anchor="w")

	dupl_button <- tkcheckbutton(convertcsvETS_win,
						text="duplicate_testnames",
						variable=csvETS_duplicate_testnames)
	tkpack(dupl_button,side="top",anchor="w")
}


