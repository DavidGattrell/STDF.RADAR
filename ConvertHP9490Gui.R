# ConvertHP9490Gui.R
#
# $Id: ConvertHP9490Gui.R,v 1.1 2012/02/01 02:33:50 David Exp $
#
# Tk/Tcl GUI wrapper for calling ConvertHP9490.R
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

# ConvertHP9490Gui specific variables
#-----------------------------------
HP9490_name <- tclVar("")
HP9490_dir <- tclVar("")
HP9490_rtdf_name <- tclVar("")

hp_count <- tclVar(1)
hp_index <- tclVar(1)
multi_hp_names <- list()
multi_hp_rtdfs <- list()
multi_hp_names[[1]] <- tclVar("")
multi_hp_rtdfs[[1]] <- tclVar("")

#----------------------------------------------------
HP9490_browser <-function() {

	my_dir <- paste(tclObj(HP9490_dir),sep="",collapse=" ")
	if (nchar(my_dir)<1) {
		my_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if(as.numeric(tclObj(Bad_Vista))>0) {
		my_str = "{{All files} *} {{HP9490 Files} {.txt .txt.gz .dat .dat.gz}}"
	} else {
		my_str = "{{HP9490 Files} {.txt .txt.gz .dat .dat.gz}} {{All files} *}"
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
				tclvalue(hp_count) <- length(names)

				# the tkgetOpenFile returns the full path!
				# separate filename and path
				my_filepath = sub("/[^/]*$","",name)	
				name=sub("^.*/","",name)

				# update name and path variables
				if (length(multi_hp_names)<j)  multi_hp_names[[j]] <<- tclVar(name)
				else  tclvalue(multi_hp_names[[j]]) <- name

				if (j==1) {
					index <- as.integer(tclObj(hp_index))
					if (index>length(names))  tclvalue(hp_index) <- 1

					first_path = my_filepath
					out_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
					if(my_filepath == out_dir)  tclvalue(HP9490_dir) <- ""
					else  tclvalue(HP9490_dir) <- my_filepath
				} else if (!(first_path %in% my_filepath)) {
					# nasty message... shouldn't get here...
				}

				my_rtdf <- sub("([.]dat)?([.]gz)?$",".rtdf",name)
				if (length(multi_hp_rtdfs)<j)  multi_hp_rtdfs[[j]] <<- tclVar(my_rtdf)
				else  tclvalue(multi_hp_rtdfs[[j]]) <- my_rtdf
			}
		}
		index <- as.integer(tclObj(hp_index))
		tclvalue(HP9490_name) <- tclObj(multi_hp_names[[index]])
		tclvalue(HP9490_rtdf_name) <- tclObj(multi_hp_rtdfs[[index]])
	}
}

#----------------------------------------------------
convertHP9490_rtdf_browser <-function(...) {
	orig_name = paste(tclObj(HP9490_rtdf_name),sep="",collapse=" ")
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
		tclvalue(HP9490_rtdf_name) <- name
		index <- as.integer(tclObj(hp_index))
		tclvalue(multi_hp_rtdfs[[index]]) <- name
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
inc_hp_index <- function() {
	my_value <- as.integer(tclObj(hp_index))
	tclvalue(multi_hp_names[[my_value]]) <- tclObj(HP9490_name)
	tclvalue(multi_hp_rtdfs[[my_value]]) <- tclObj(HP9490_rtdf_name)

	my_count <- as.integer(tclObj(hp_count))
	if (my_value<my_count) {
		my_value <- my_value + 1
		tclvalue(HP9490_name) <- tclObj(multi_hp_names[[my_value]])
		tclvalue(HP9490_rtdf_name) <- tclObj(multi_hp_rtdfs[[my_value]])
	} 
	tclvalue(hp_index) <- my_value
}

#-----------------------------------------------------
dec_hp_index <- function() {
	my_value <- as.integer(tclObj(hp_index))
	tclvalue(multi_hp_names[[my_value]]) <- tclObj(HP9490_name)
	tclvalue(multi_hp_rtdfs[[my_value]]) <- tclObj(HP9490_rtdf_name)

	if (my_value>1) {
		my_value <- my_value - 1
		tclvalue(HP9490_name) <- tclObj(multi_hp_names[[my_value]])
		tclvalue(HP9490_rtdf_name) <- tclObj(multi_hp_rtdfs[[my_value]])
	}
	tclvalue(hp_index) <- my_value
}

#----------------------------------------------------
ConvertHP9490Gui_defaults <- function() {
	tclvalue(HP9490_name) <- ""
#	tclvalue(HP9490_dir) <- ""
	tclvalue(HP9490_rtdf_name) <- ""

	tclvalue(hp_count) <- 1
	tclvalue(hp_index) <- 1
	tclvalue(multi_hp_names[[1]]) <- tclVar("")
	tclvalue(multi_hp_rtdfs[[1]]) <- tclVar("")

}

#----------------------------------------------------
run_ConvertHP9490 <-function(done=FALSE,...) {
	my_value <- as.integer(tclObj(hp_index))
	tclvalue(multi_hp_names[[my_value]]) <- tclObj(HP9490_name)
	tclvalue(multi_hp_rtdfs[[my_value]]) <- tclObj(HP9490_rtdf_name)

	#HP9490_name_ <- as.character(tclObj(HP9490_name))
	#rtdf_name_ <- as.character(tclObj(HP9490_rtdf_name))
	HP9490_dir_ <- paste(tclObj(HP9490_dir),sep="",collapse=" ")
	output_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")

	full_path = output_dir
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	setwd(full_path)

	count <- as.integer(tclObj(hp_count))
	for (j in 1:count) {
		HP9490_name_ <- paste(tclObj(multi_hp_names[[j]]),sep="",collapse=" ")
		rtdf_name_ <- paste(tclObj(multi_hp_rtdfs[[j]]),sep="",collapse=" ")

		my_expr = substitute(
			ConvertHP9490(dat_name=HP9490_name_,rtdf_name=rtdf_name_,
						dat_dir=HP9490_dir_)
		)
		tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
		tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
		my_command = sprintf("%s\n",deparse(my_expr))
		my_cmnd = "ConvertHP9490(...)\n"
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
		convertHP9490_win <- get("convertHP9490_win",envir=.TkRadar.wins)
		tkdestroy(convertHP9490_win)
	}
}

#----------------------------------------------------
ConvertHP9490Gui <- function() {

	ConvertHP9490Gui_defaults()		# initialize variables...
	convertHP9490_win <- tktoplevel()
	assign("convertHP9490_win",convertHP9490_win,envir=.TkRadar.wins)
	tkwm.title(convertHP9490_win, "ConvertHP9490")
		
	bottom_row <- tkframe(convertHP9490_win)
	default_button <- tkbutton(bottom_row,
						text="DEFAULTS",
						#anchor="w",
						width=12,
						command=ConvertHP9490Gui_defaults)
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
						text="RUN & QUIT",
						#anchor="w",
						width=12,
						command=function() run_ConvertHP9490(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
						text="QUIT",
						#anchor="w",
						width=12,
						command=function()tkdestroy(convertHP9490_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
						text="RUN",
						#anchor="w",
						width=12,
						command=function() run_ConvertHP9490(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	dir_entry_frame <- tkframe(convertHP9490_win)
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

	HP9490_dir_entry_frame <- tkframe(convertHP9490_win)
	HP9490_dir_entry_label <- tklabel(HP9490_dir_entry_frame,
						width=10,
						text="dat_dir")
	tkpack(HP9490_dir_entry_label,side="left")
	HP9490_dir_entry <- tklabel(HP9490_dir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=HP9490_dir)
	tkpack(HP9490_dir_entry,side="left",fill="x",expand=1)
	HP9490_dir_browse <- tkbutton(HP9490_dir_entry_frame,
						text="Browse",
						command=function() dir_browser(HP9490_dir))
	tkpack(HP9490_dir_browse,side="right")
	tkpack(HP9490_dir_entry_frame,side="top",anchor="w",fill="x")

	multiple_frame <- tkframe(convertHP9490_win)
	multiple_label <- tklabel(multiple_frame,
						#width=10,
						text="file")
	tkpack(multiple_label,side="left")
	index_value <- tklabel(multiple_frame,
						width=3,
						relief="sunken",
						textvariable=hp_index)
	tkpack(index_value,side="left")
	multiple_label2 <- tklabel(multiple_frame,
						#width=10,
						text="of")
	tkpack(multiple_label2,side="left")
	count_value <- tklabel(multiple_frame,
						width=3,
						relief="sunken",
						textvariable=hp_count)
	tkpack(count_value,side="left")
	index_minus <- tkbutton(multiple_frame,
						text="-",
						command=dec_hp_index)
	tkpack(index_minus,side="left")
	index_plus <- tkbutton(multiple_frame,
						text="+",
						command=inc_hp_index)
	tkpack(index_plus,side="left")
	tkpack(multiple_frame,side="top",anchor="w",fill="x")

	HP9490_entry_frame <- tkframe(convertHP9490_win)
	HP9490_entry_label <- tklabel(HP9490_entry_frame,
						width=10,
						text="dat_name")
	tkpack(HP9490_entry_label,side="left")
	HP9490_entry <- tkentry(HP9490_entry_frame,
						width=50,
						textvariable=HP9490_name)
	tkpack(HP9490_entry,side="left",fill="x",expand=1)
	HP9490_browse <- tkbutton(HP9490_entry_frame,
						text="Browse",
						command=HP9490_browser)
	tkpack(HP9490_browse,side="right")
	tkpack(HP9490_entry_frame,side="top",anchor="w",fill="x")

	rtdf_entry_frame <- tkframe(convertHP9490_win)
	rtdf_entry_label <- tklabel(rtdf_entry_frame,
						width=10,
						text="rtdf_name")
	tkpack(rtdf_entry_label,side="left")
	rtdf_entry <- tkentry(rtdf_entry_frame,
						width=20,
						textvariable=HP9490_rtdf_name)
	tkpack(rtdf_entry,side="left",fill="x",expand=1)
	rtdf_browse <- tkbutton(rtdf_entry_frame,
						text="Browse",
						command=convertHP9490_rtdf_browser)
	tkpack(rtdf_browse,side="right")
	tkpack(rtdf_entry_frame,side="top",anchor="w",fill="x")

}


