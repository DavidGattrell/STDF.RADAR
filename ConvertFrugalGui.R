# ConvertFrugalGui.R
#
# $Id: ConvertFrugalGui.R,v 1.1 2010/11/24 01:28:41 David Exp $
#
# Tk/Tcl GUI wrapper for calling ConverttFrugal.R
# called by TkRadar.R
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
#----------------------------------------------------------

# ConvertFrugalGui specific variables
#-----------------------------------
frug_name <- tclVar("")
frug_dir <- tclVar("")
frug_rtdf_name <- tclVar("")

frug_count <- tclVar(1)
frug_index <- tclVar(1)
multi_frug_names <- list()
multi_frug_rtdfs <- list()
multi_frug_names[[1]] <- tclVar("")
multi_frug_rtdfs[[1]] <- tclVar("")

#----------------------------------------------------
frug_browser <-function() {

	my_dir <- paste(tclObj(frug_dir),sep="",collapse=" ")
	if (nchar(my_dir)<1) {
		my_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if(as.numeric(tclObj(Bad_Vista))>0) {
		my_str = "{{All files} *} {{Frugal Files} {.txt .txt.gz .dlog .dlog.gz}}"
	} else {
		my_str = "{{Frugal Files} {.txt .txt.gz .dlog .dlog.gz}} {{All files} *}"
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
				tclvalue(frug_count) <- length(names)

				# the tkgetOpenFile returns the full path!
				# separate filename and path
				my_filepath = sub("/[^/]*$","",name)	
				name=sub("^.*/","",name)

				# update name and path variables
				if (length(multi_frug_names)<j)  multi_frug_names[[j]] <<- tclVar(name)
				else  tclvalue(multi_frug_names[[j]]) <- name

				if (j==1) {
					index <- as.integer(tclObj(frug_index))
					if (index>length(names))  tclvalue(frug_index) <- 1

					first_path = my_filepath
					out_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
					if(my_filepath == out_dir)  tclvalue(frug_dir) <- ""
					else  tclvalue(frug_dir) <- my_filepath
				} else if (!(first_path %in% my_filepath)) {
					# nasty message... shouldn't get here...
				}

				my_rtdf <- sub("([.]txt)?([.]dlog)?([.]gz)?$",".rtdf",name)
				if (length(multi_frug_rtdfs)<j)  multi_frug_rtdfs[[j]] <<- tclVar(my_rtdf)
				else  tclvalue(multi_frug_rtdfs[[j]]) <- my_rtdf
			}
		}
		index <- as.integer(tclObj(frug_index))
		tclvalue(frug_name) <- tclObj(multi_frug_names[[index]])
		tclvalue(frug_rtdf_name) <- tclObj(multi_frug_rtdfs[[index]])
	}
}

#----------------------------------------------------
convertfrug_rtdf_browser <-function(...) {
	orig_name = paste(tclObj(frug_rtdf_name),sep="",collapse=" ")
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
		tclvalue(frug_rtdf_name) <- name
		index <- as.integer(tclObj(frug_index))
		tclvalue(multi_frug_rtdfs[[index]]) <- name
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
inc_frug_index <- function() {
	my_value <- as.integer(tclObj(frug_index))
	tclvalue(multi_frug_names[[my_value]]) <- tclObj(frug_name)
	tclvalue(multi_frug_rtdfs[[my_value]]) <- tclObj(frug_rtdf_name)

	my_count <- as.integer(tclObj(frug_count))
	if (my_value<my_count) {
		my_value <- my_value + 1
		tclvalue(frug_name) <- tclObj(multi_frug_names[[my_value]])
		tclvalue(frug_rtdf_name) <- tclObj(multi_frug_rtdfs[[my_value]])
	} 
	tclvalue(frug_index) <- my_value
}

#-----------------------------------------------------
dec_frug_index <- function() {
	my_value <- as.integer(tclObj(frug_index))
	tclvalue(multi_frug_names[[my_value]]) <- tclObj(frug_name)
	tclvalue(multi_frug_rtdfs[[my_value]]) <- tclObj(frug_rtdf_name)

	if (my_value>1) {
		my_value <- my_value - 1
		tclvalue(frug_name) <- tclObj(multi_frug_names[[my_value]])
		tclvalue(frug_rtdf_name) <- tclObj(multi_frug_rtdfs[[my_value]])
	}
	tclvalue(frug_index) <- my_value
}

#----------------------------------------------------
ConvertFrugalGui_defaults <- function() {
	tclvalue(frug_name) <- ""
#	tclvalue(frug_dir) <- ""
	tclvalue(frug_rtdf_name) <- ""

	tclvalue(frug_count) <- 1
	tclvalue(frug_index) <- 1
	tclvalue(multi_frug_names[[1]]) <- tclVar("")
	tclvalue(multi_frug_rtdfs[[1]]) <- tclVar("")

}

#----------------------------------------------------
run_ConvertFrugal <-function(done=FALSE,...) {
	my_value <- as.integer(tclObj(frug_index))
	tclvalue(multi_frug_names[[my_value]]) <- tclObj(frug_name)
	tclvalue(multi_frug_rtdfs[[my_value]]) <- tclObj(frug_rtdf_name)

	#frug_name_ <- as.character(tclObj(frug_name))
	#rtdf_name_ <- as.character(tclObj(frug_rtdf_name))
	frug_dir_ <- paste(tclObj(frug_dir),sep="",collapse=" ")
	output_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")

	full_path = output_dir
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	setwd(full_path)

	count <- as.integer(tclObj(frug_count))
	for (j in 1:count) {
		frug_name_ <- paste(tclObj(multi_frug_names[[j]]),sep="",collapse=" ")
		rtdf_name_ <- paste(tclObj(multi_frug_rtdfs[[j]]),sep="",collapse=" ")

		my_expr = substitute(
			ConvertFrugal(frug_name=frug_name_,rtdf_name=rtdf_name_,
						frug_dir=frug_dir_)
		)
		tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
		tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
		my_command = sprintf("%s\n",deparse(my_expr))
		my_cmnd = "ConvertFrugal(...)\n"
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
		convertfrug_win <- get("convertfrug_win",envir=.TkRadar.wins)
		tkdestroy(convertfrug_win)
	}
}

#----------------------------------------------------
ConvertFrugalGui <- function() {

	ConvertFrugalGui_defaults()		# initialize variables...
	convertfrug_win <- tktoplevel()
	assign("convertfrug_win",convertfrug_win,envir=.TkRadar.wins)
	tkwm.title(convertfrug_win, "ConvertFrugal")
		
	bottom_row <- tkframe(convertfrug_win)
	default_button <- tkbutton(bottom_row,
						text="DEFAULTS",
						#anchor="w",
						width=12,
						command=ConvertFrugalGui_defaults)
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
						text="RUN & QUIT",
						#anchor="w",
						width=12,
						command=function() run_ConvertFrugal(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
						text="QUIT",
						#anchor="w",
						width=12,
						command=function()tkdestroy(convertfrug_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
						text="RUN",
						#anchor="w",
						width=12,
						command=function() run_ConvertFrugal(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	dir_entry_frame <- tkframe(convertfrug_win)
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

	frug_dir_entry_frame <- tkframe(convertfrug_win)
	frug_dir_entry_label <- tklabel(frug_dir_entry_frame,
						width=10,
						text="frug_dir")
	tkpack(frug_dir_entry_label,side="left")
	frug_dir_entry <- tklabel(frug_dir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=frug_dir)
	tkpack(frug_dir_entry,side="left",fill="x",expand=1)
	frug_dir_browse <- tkbutton(frug_dir_entry_frame,
						text="Browse",
						command=function() dir_browser(frug_dir))
	tkpack(frug_dir_browse,side="right")
	tkpack(frug_dir_entry_frame,side="top",anchor="w",fill="x")

	multiple_frame <- tkframe(convertfrug_win)
	multiple_label <- tklabel(multiple_frame,
						#width=10,
						text="file")
	tkpack(multiple_label,side="left")
	index_value <- tklabel(multiple_frame,
						width=3,
						relief="sunken",
						textvariable=frug_index)
	tkpack(index_value,side="left")
	multiple_label2 <- tklabel(multiple_frame,
						#width=10,
						text="of")
	tkpack(multiple_label2,side="left")
	count_value <- tklabel(multiple_frame,
						width=3,
						relief="sunken",
						textvariable=frug_count)
	tkpack(count_value,side="left")
	index_minus <- tkbutton(multiple_frame,
						text="-",
						command=dec_frug_index)
	tkpack(index_minus,side="left")
	index_plus <- tkbutton(multiple_frame,
						text="+",
						command=inc_frug_index)
	tkpack(index_plus,side="left")
	tkpack(multiple_frame,side="top",anchor="w",fill="x")

	frug_entry_frame <- tkframe(convertfrug_win)
	frug_entry_label <- tklabel(frug_entry_frame,
						width=10,
						text="frug_name")
	tkpack(frug_entry_label,side="left")
	frug_entry <- tkentry(frug_entry_frame,
						width=50,
						textvariable=frug_name)
	tkpack(frug_entry,side="left",fill="x",expand=1)
	frug_browse <- tkbutton(frug_entry_frame,
						text="Browse",
						command=frug_browser)
	tkpack(frug_browse,side="right")
	tkpack(frug_entry_frame,side="top",anchor="w",fill="x")

	rtdf_entry_frame <- tkframe(convertfrug_win)
	rtdf_entry_label <- tklabel(rtdf_entry_frame,
						width=10,
						text="rtdf_name")
	tkpack(rtdf_entry_label,side="left")
	rtdf_entry <- tkentry(rtdf_entry_frame,
						width=20,
						textvariable=frug_rtdf_name)
	tkpack(rtdf_entry,side="left",fill="x",expand=1)
	rtdf_browse <- tkbutton(rtdf_entry_frame,
						text="Browse",
						command=convertfrug_rtdf_browser)
	tkpack(rtdf_browse,side="right")
	tkpack(rtdf_entry_frame,side="top",anchor="w",fill="x")

}


