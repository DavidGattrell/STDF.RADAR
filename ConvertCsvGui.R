# ConvertCsvGui.R
#
# $Id: ConvertCsvGui.R,v 1.16 2014/08/03 00:36:42 david Exp $
#
# Tk/Tcl GUI wrapper for calling ConvertCsv.R
# called by TkRadar.R
#
# Copyright (C) 2008-2014 David Gattrell
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
# ConvertCsvGui specific variables
#-----------------------------------------
csv_in_name <- tclVar("")
convcsv_out_name <- tclVar("")

csv_in_csv_dir <- tclVar("")	# input csv files from this dir
csv_out_csv_dir <- tclVar("")		# output csv files to this dir
# rtdf in uses Rtdf_dir
# rtdf out uses Rtdfs_dir

convcsv_type <- tclVar("r2c")	# "r2c" or "r2ct" or "c2r"
convcsv_oldtype <- tclVar("r2c")

csv_count <- tclVar(1)
csv_index <- tclVar(1)
multi_csv_ins <- list()
multi_csv_outs <- list()
multi_csv_ins[[1]] <- tclVar("")
multi_csv_outs[[1]] <- tclVar("")

csv_posix_time <- tclVar(0)
csv_lim_eq_pass_flags <- tclVar(0)

csv_autoopen <- tclVar(0)

# these defaults can be controlled in the .Rprofile file:
default_convcsv_type <- tclVar("r2c")
default_csv_posix_time <- tclVar(0)
default_csv_lim_eq_pass_flags <- tclVar(0)
default_csv_autoopen <- tclVar(1)

#----------------------------------------------------
ConvertCsvGui_defaults <- function(...) {
	tclvalue(convcsv_out_name) <- "my_rtdf.csv"
#	tclvalue(convcsv_type) <- "r2c"
#	tclvalue(convcsv_oldtype) <- "r2c"
	tclvalue(convcsv_type) <- tclObj(default_convcsv_type)
	tclvalue(convcsv_oldtype) <- tclObj(default_convcsv_type)
	set_convcsv_type()
	
	tclvalue(csv_count) <- 1
	tclvalue(csv_index) <- 1
	tclvalue(multi_csv_ins[[1]]) <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
	tclvalue(multi_csv_outs[[1]]) <- "my_rtdf.csv"

	tclvalue(csv_in_csv_dir) <- paste(tclObj(csv_out_csv_dir),sep="",collapse=" ")
	tclvalue(csv_in_name) <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
	# input RTDF dir uses Rtdf_dir

#	tclvalue(csv_posix_time) <- 1
#	tclvalue(csv_lim_eq_pass_flags) <- 0
	tclvalue(csv_posix_time) <- tclObj(default_csv_posix_time)
	tclvalue(csv_lim_eq_pass_flags) <- tclObj(default_csv_lim_eq_pass_flags)

	tclvalue(csv_autoopen) <- tclObj(default_csv_autoopen)

}


#----------------------------------------------------
convcsv_browser <- function(out=FALSE) {
	# based on convert_type and out_flag, browse
	# for rtdf or csv files

	my_type <- as.character(tclObj(convcsv_type))

	if(as.numeric(tclObj(Bad_Vista))>0) {
		rtdf_str = "{{All files} *} {{RTDF Files} {.rtdf .Rtdf .Rdata}}"
		csv_str = "{{All files} *} {{CSV Files} {.csv}}"
	} else {
		rtdf_str = "{{RTDF Files} {.rtdf .Rtdf .Rdata}} {{All files} *}"
		csv_str = "{{CSV Files} {.csv}} {{All files} *}"
	}
	if (my_type=="r2c") {
		if (out) {
			my_dir <- paste(tclObj(csv_out_csv_dir),sep="",collapse=" ")
			type_str=csv_str
		} else {
			my_dir <- paste(tclObj(Rtdf_dir),sep="",collapse=" ")
			type_str=rtdf_str
		}
	} else if (my_type=="r2ct") {
		if (out) {
			my_dir <- paste(tclObj(csv_out_csv_dir),sep="",collapse=" ")
			type_str=csv_str
		} else { 
			my_dir <- paste(tclObj(Rtdf_dir),sep="",collapse=" ")
			type_str=rtdf_str
		}
	} else if (my_type=="c2r") {
		if (out) {
			my_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
			type_str=rtdf_str
		} else {
			my_dir <- paste(tclObj(csv_out_csv_dir),sep="",collapse=" ")
			type_str=csv_str
		}
	} else {
		my_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
		type_str="{{All files} *}"
	}
	if (nchar(my_dir)<1) {
		my_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(my_dir)<1) {
		my_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}


	if (out) {
		name <- tclvalue(tkgetSaveFile(
			#initialfile=orig_name,
			filetypes=type_str,
			initialdir=my_dir))
		#if(nchar(name)>255)  cat("ERROR: path+file is >255 chars\n") 
		names = name
	} else {
		name <- tclvalue(tkgetOpenFile(
			#initialfile=orig_name,
			filetypes=type_str,
			initialdir=my_dir,multiple=TRUE))
		#if(nchar(name)>255)  cat("ERROR: path+file is >255 chars\n") 
		names = split_multiple_filenames(name)
	}


	if(length(names)>0) {
		for (j in 1:length(names)) {
			name = names[j]
			if (nchar(name)>0) {
				if (!out)  tclvalue(csv_count) <- length(names)

				# separate filename and path
				my_filepath = sub("/[^/]*$","",name)	
				name=sub("^.*/","",name)

				if(my_type=="c2r")  out_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
				else  out_dir <- paste(tclObj(csv_out_csv_dir),sep="",collapse=" ")
				if (nchar(out_dir)<1) {
					out_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
				}
				if (nchar(out_dir)<1) {
					out_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
				}
				if (out) {
					if(my_filepath != out_dir) {
						if(my_type=="c2r") {
							change_Rtdfs_dir(my_filepath)
						} else {
							out_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
							if (nchar(out_dir)<1) {
								out_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
							}
							if(out_dir == my_filepath)  tclvalue(csv_out_csv_dir) <- ""
							else  tclvalue(csv_out_csv_dir) <- my_filepath
						}
					}
					tclvalue(convcsv_out_name) <- name
					index <- as.integer(tclObj(csv_index))
					tclvalue(multi_csv_outs[[index]]) <- name
				} else {
					# update name and path variables
					if (length(multi_csv_ins)<j)  multi_csv_ins[[j]] <<- tclVar(name)
					else  tclvalue(multi_csv_ins[[j]]) <- name

					if (j==1) {
						index <- as.integer(tclObj(csv_index))
						if (index>length(names))  tclvalue(csv_index) <- 1

						first_path = my_filepath
						out_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
						if (nchar(out_dir)<1) {
							out_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
						}
						if (my_type=="c2r") { 
							if(my_filepath == out_dir)  tclvalue(csv_in_csv_dir) <- ""
							else  tclvalue(csv_in_csv_dir) <- my_filepath
						} else {
							if(my_filepath == out_dir)  tclvalue(Rtdf_dir) <- ""
							else  tclvalue(Rtdf_dir) <- my_filepath
							tclvalue(Rtdf_name) <- name
						}
					} else if (!(first_path %in% my_filepath)) {
						# nasty message... shouldn't get here...
					}
					
					if (my_type!="c2r") {
						my_out <- sub(".rtdf?$",".csv",name)
					} else {
						my_out <- sub(".csv?$",".rtdf",name)
					}
					if (length(multi_csv_outs)<j)  multi_csv_outs[[j]] <<- tclVar(my_out)
					else  tclvalue(multi_csv_outs[[j]]) <- my_out
					#tclvalue(csv_in_name) <- name
					#tclvalue(convcsv_out_name) <- my_out
				}
			}
		}
		if (!out) {
			index <- as.integer(tclObj(csv_index))
			tclvalue(csv_in_name) <- tclObj(multi_csv_ins[[index]])
			tclvalue(convcsv_out_name) <- tclObj(multi_csv_outs[[index]])
		}
	}
}

#-----------------------------------------------------
inc_csv_index <- function() {
	my_value <- as.integer(tclObj(csv_index))
	tclvalue(multi_csv_ins[[my_value]]) <- tclObj(csv_in_name)
	tclvalue(multi_csv_outs[[my_value]]) <- tclObj(convcsv_out_name)

	my_count <- as.integer(tclObj(csv_count))
	if (my_value<my_count) {
		my_value <- my_value + 1
		tclvalue(csv_in_name) <- tclObj(multi_csv_ins[[my_value]])
		tclvalue(convcsv_out_name) <- tclObj(multi_csv_outs[[my_value]])
	} 
	tclvalue(csv_index) <- my_value
}

#-----------------------------------------------------
dec_csv_index <- function() {
	my_value <- as.integer(tclObj(csv_index))
	tclvalue(multi_csv_ins[[my_value]]) <- tclObj(csv_in_name)
	tclvalue(multi_csv_outs[[my_value]]) <- tclObj(convcsv_out_name)

	if (my_value>1) {
		my_value <- my_value - 1
		tclvalue(csv_in_name) <- tclObj(multi_csv_ins[[my_value]])
		tclvalue(convcsv_out_name) <- tclObj(multi_csv_outs[[my_value]])
	}
	tclvalue(csv_index) <- my_value
}


#----------------------------------------------------
# when radiobutton clicked, update in_file and out_file
# names...
set_convcsv_type <- function() {

	my_type <- substr( as.character(tclObj(convcsv_type)) , 1 , 3 )	# r2c and r2ct similar
	my_old_type <- substr( as.character(tclObj(convcsv_oldtype)) , 1 , 3 )
	in_file <- paste(tclObj(csv_in_name),sep="",collapse=" ")
	#in_dir_ <- paste(tclObj(csv_out_csv_dir),sep="",collapse=" ")
	out_file <- paste(tclObj(convcsv_out_name),sep="",collapse=" ")

	if ((my_type!="c2r") && (my_old_type=="c2r")) {
		tclvalue(csv_index) <- 1
		tclvalue(csv_count) <- 1
		#tclvalue(csv_in_dir) <- as.character(tclObj(Rtdf_dir))
		tclvalue(csv_in_name) <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
		tclvalue(multi_csv_ins[[1]]) <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
		tclvalue(convcsv_out_name) <- "my_rtdf.csv"
		tclvalue(multi_csv_outs[[1]]) <- "my_rtdf.csv"
	} else if ((my_type=="c2r") && (my_old_type!="c2r")) {
		tclvalue(csv_index) <- 1
		tclvalue(csv_count) <- 1
		#tclvalue(csv_in_dir) <- ""
		tclvalue(csv_in_name) <- ""
		tclvalue(multi_csv_ins[[1]]) <- ""
		tclvalue(convcsv_out_name) <- "from_csv.rtdf"
		tclvalue(multi_csv_outs[[1]]) <- "from_csv.rtdf"
	}

	tclvalue(convcsv_oldtype) <- my_type
}


#----------------------------------------------------
run_ConvertCsv <-function(done=FALSE,...) {
	my_value <- as.integer(tclObj(csv_index))
	tclvalue(multi_csv_ins[[my_value]]) <- tclObj(csv_in_name)
	tclvalue(multi_csv_outs[[my_value]]) <- tclObj(convcsv_out_name)

	#in_file_ <- paste(tclObj(csv_in_name),sep="",collapse=" ")
	#out_file_ <- paste(tclObj(convcsv_out_name),sep="",collapse=" ")
	csv_in_dir_ <- paste(tclObj(csv_in_csv_dir),sep="",collapse=" ")
	rtdf_in_dir_ <- paste(tclObj(Rtdf_dir),sep="",collapse=" ")
	my_type <- as.character(tclObj(convcsv_type))

	posix_time_ <- as.logical(tclObj(csv_posix_time))
	lim_eq_flags_ <- as.logical(tclObj(csv_lim_eq_pass_flags))

	csv_autoopen_ <- as.logical(tclObj(csv_autoopen))

	if (my_type=="c2r") {
		in_dir_ = csv_in_dir_
		if (nchar(in_dir_)<1) {
			in_dir_ <- paste(tclObj(csv_out_csv_dir),sep="",collapse=" ")
		}
		output_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
		csv_autoopen_ <- FALSE
	} else {
		in_dir_ = rtdf_in_dir_
		if (nchar(in_dir_)<1) {
			in_dir_ <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
		}
		output_dir <- paste(tclObj(csv_out_csv_dir),sep="",collapse=" ")
	}
	if (my_type=="r2ct") {
		rows_tests = FALSE
	} else {
		rows_tests = TRUE
	}

	if (nchar(in_dir_)<1) {
		in_dir_ <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(in_dir_)<1) {
		in_dir_ <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	if (nchar(output_dir)<1) {
		output_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(output_dir)<1) {
		output_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	if(in_dir_==output_dir)  in_dir_ = ""
	setwd(output_dir)

	count <- as.integer(tclObj(csv_count))
	for (j in 1:count) {
		in_file_ <- paste(tclObj(multi_csv_ins[[j]]),sep="",collapse=" ")
		out_file_ <- paste(tclObj(multi_csv_outs[[j]]),sep="",collapse=" ")

		my_expr = substitute(
			ConvertCsv(in_name=in_file_,out_name=out_file_,
							  rows_are_tests=rows_tests,in_dir=in_dir_,
							  posix_time=posix_time_,
							  lim_eq_flags=lim_eq_flags_)
		)
		tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
		tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
		my_command = sprintf("%s\n",deparse(my_expr))
		my_cmnd = "ConvertCsv(...)\n"
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

		if(csv_autoopen_) {
			if (as.character(Sys.info()["sysname"])=="Windows") {
				os_com = "cmd /c start"
			} else if (as.character(Sys.info()["sysname"])=="Darwin") {  # aka Mac
				os_com = "open"
			} else {  # linux...
				os_com = "xdg-open"
			}
			command_str = paste(os_com,out_file_)
			system(command_str)
		}
	}

	if (my_type=="c2r") {
		# now update Rtdf_name and Rtdf_dir...
		tclvalue(Rtdf_name) <- out_file_
		tclvalue(Rtdf_dir) <- paste(as.character(tclObj(Rtdfs_dir)),sep="",collapse=" ")
	}

	if(done>0) {
		convertcsv_win <- get("convertcsv_win",envir=.TkRadar.wins)
		tkdestroy(convertcsv_win)
	}
}



#-----------------------------------------------------
ConvertCsvGui <-function(...) {

	ConvertCsvGui_defaults()		# initialize variables...
	convertcsv_win <- tktoplevel()
	assign("convertcsv_win",convertcsv_win,envir=.TkRadar.wins)
	tkwm.title(convertcsv_win, "ConvertCsv")
	
	bottom_row <- tkframe(convertcsv_win)
	default_button <- tkbutton(bottom_row,
					text="DEFAULTS",
					#anchor="w",
					width=12,
					command=ConvertCsvGui_defaults)
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
					text="RUN & QUIT",
					#anchor="w",
					width=12,
					command=function() run_ConvertCsv(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
					text="QUIT",
					#anchor="w",
					width=12,
					command=function()tkdestroy(convertcsv_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
					text="RUN",
					#anchor="w",
					width=12,
					command=function() run_ConvertCsv(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")


	in_dir_entry_frame <- tkframe(convertcsv_win)
	in_dir_entry_label <- tklabel(in_dir_entry_frame,
						width=14,
						text="in csv_dir")
	tkpack(in_dir_entry_label,side="left")
	in_dir_entry <- tklabel(in_dir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=csv_in_csv_dir)
	tkpack(in_dir_entry,side="left",fill="x",expand=1)
	in_dir_browse <- tkbutton(in_dir_entry_frame,
						text="Browse",
						command=function() dir_browser(csv_in_csv_dir))
	tkpack(in_dir_browse,side="right")
	tkpack(in_dir_entry_frame,side="top",anchor="w",fill="x")

	dir_entry_frame <- tkframe(convertcsv_win)
	dir_entry_label <- tklabel(dir_entry_frame,
						width=14,
						text="output csv_dir")
	tkpack(dir_entry_label,side="left")
	dir_entry <- tklabel(dir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=csv_out_csv_dir)
	tkpack(dir_entry,side="left",fill="x",expand=1)
	dir_browse <- tkbutton(dir_entry_frame,
						text="Browse",
						command=function() dir_browser(csv_out_csv_dir))
	tkpack(dir_browse,side="right")
	tkpack(dir_entry_frame,side="top",anchor="w",fill="x")


	r_in_dir_entry_frame <- tkframe(convertcsv_win)
	r_in_dir_entry_label <- tklabel(r_in_dir_entry_frame,
						width=14,
						text="in Rtdf_dir")
	tkpack(r_in_dir_entry_label,side="left")
	r_in_dir_entry <- tklabel(r_in_dir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=Rtdf_dir)
	tkpack(r_in_dir_entry,side="left",fill="x",expand=1)
	r_in_dir_browse <- tkbutton(r_in_dir_entry_frame,
						text="Browse",
						command=function() dir_browser(Rtdf_dir))
	tkpack(r_in_dir_browse,side="right")
	tkpack(r_in_dir_entry_frame,side="top",anchor="w",fill="x")

	r_out_dir_entry_frame <- tkframe(convertcsv_win)
	r_out_dir_entry_label <- tklabel(r_out_dir_entry_frame,
						width=14,
						text="output Rtdfs_dir")
	tkpack(r_out_dir_entry_label,side="left")
	r_out_dir_entry <- tklabel(r_out_dir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=Rtdfs_dir)
	tkpack(r_out_dir_entry,side="left",fill="x",expand=1)
	r_out_dir_browse <- tkbutton(r_out_dir_entry_frame,
						text="Browse",
						command=function() dir_browser(Rtdfs_dir))
	tkpack(r_out_dir_browse,side="right")
	tkpack(r_out_dir_entry_frame,side="top",anchor="w",fill="x")


	type_frame <- tkframe(convertcsv_win)
	type_r2c <- tkradiobutton(type_frame,
						text="rtdf->csv rows=tests",
						value="r2c",
						command=set_convcsv_type,
						variable=convcsv_type)
	tkpack(type_r2c,side="left")
	type_r2ct <- tkradiobutton(type_frame,
						text="rtdf->csv cols=tests",
						value="r2ct",
						command=set_convcsv_type,
						variable=convcsv_type)
	tkpack(type_r2ct,side="left")
	type_c2r <- tkradiobutton(type_frame,
						text="csv->rtdf",
						value="c2r",
						command=set_convcsv_type,
						variable=convcsv_type)
	tkpack(type_c2r,side="left")
	tkpack(type_frame,side="top",anchor="w",fill="x")


	multiple_frame <- tkframe(convertcsv_win)
	multiple_label <- tklabel(multiple_frame,
						#width=10,
						text="file")
	tkpack(multiple_label,side="left")
	index_value <- tklabel(multiple_frame,
						width=3,
						relief="sunken",
						textvariable=csv_index)
	tkpack(index_value,side="left")
	multiple_label2 <- tklabel(multiple_frame,
						#width=10,
						text="of")
	tkpack(multiple_label2,side="left")
	count_value <- tklabel(multiple_frame,
						width=3,
						relief="sunken",
						textvariable=csv_count)
	tkpack(count_value,side="left")
	index_minus <- tkbutton(multiple_frame,
						text="-",
						command=dec_csv_index)
	tkpack(index_minus,side="left")
	index_plus <- tkbutton(multiple_frame,
						text="+",
						command=inc_csv_index)
	tkpack(index_plus,side="left")
	tkpack(multiple_frame,side="top",anchor="w",fill="x")

	in_entry_frame <- tkframe(convertcsv_win)
	in_entry_label <- tklabel(in_entry_frame,
						width=12,
						text="in_file")
	tkpack(in_entry_label,side="left")
	in_entry <- tkentry(in_entry_frame,
						width=50,
						textvariable=csv_in_name)
	tkpack(in_entry,side="left",fill="x",expand=1)
	in_browse <- tkbutton(in_entry_frame,
						text="Browse",
						command=function() convcsv_browser(out=FALSE))
	tkpack(in_browse,side="right")
	tkpack(in_entry_frame,side="top",anchor="w",fill="x")

	out_entry_frame <- tkframe(convertcsv_win)
	out_entry_label <- tklabel(out_entry_frame,
						width=12,
						text="out_file")
	tkpack(out_entry_label,side="left")
	out_entry <- tkentry(out_entry_frame,
						width=20,
						textvariable=convcsv_out_name)
	tkpack(out_entry,side="left",fill="x",expand=1)
	out_browse <- tkbutton(out_entry_frame,
						text="Browse",
						command=function() convcsv_browser(out=TRUE))
	tkpack(out_browse,side="right")
	tkpack(out_entry_frame,side="top",anchor="w",fill="x")

	posix_button <- tkcheckbutton(convertcsv_win,
					text="posix_time",
					variable=csv_posix_time)
	tkpack(posix_button,side="top",anchor="w")

	lim_eq_button <- tkcheckbutton(convertcsv_win,
					text="lim_eq_flags",
					variable=csv_lim_eq_pass_flags)
	tkpack(lim_eq_button,side="top",anchor="w")

	autoopen_button <- tkcheckbutton(convertcsv_win,
						text="auto_open_csv",
						variable=csv_autoopen)
	tkpack(autoopen_button,side="top",anchor="w")

}


