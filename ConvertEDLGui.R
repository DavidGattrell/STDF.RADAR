# ConvertEDLGui.R
#
# $Id: ConvertEDLGui.R,v 1.8 2010/11/24 01:27:05 David Exp $
#
# Tk/Tcl GUI wrapper for calling ConvertEDL.R
# called by TkRadar.R
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
#----------------------------------------------------------

# ConvertEDLGui specific variables
#-----------------------------------
edl_name <- tclVar("")
edl_dir <- tclVar("")
edl_rtdf_name <- tclVar("")
auto_93k <- tclVar(1)
use_Pins_used <- tclVar(1)

edl_count <- tclVar(1)
edl_index <- tclVar(1)
multi_edl_names <- list()
multi_edl_rtdfs <- list()
multi_edl_names[[1]] <- tclVar("")
multi_edl_rtdfs[[1]] <- tclVar("")

#----------------------------------------------------
edl_browser <-function() {

	my_dir <- paste(tclObj(edl_dir),sep="",collapse=" ")
	if (nchar(my_dir)<1) {
		my_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(my_dir)<1) {
		my_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	if(as.numeric(tclObj(Bad_Vista))>0) {
		my_str = "{{All files} *} {{EDL Files} {.edl .edl.gz}}"
	} else {
		my_str = "{{EDL Files} {.edl .edl.gz}} {{All files} *}"
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
				tclvalue(edl_count) <- length(names)

				# the tkgetOpenFile returns the full path!
				# separate filename and path
				my_filepath = sub("/[^/]*$","",name)	
				name=sub("^.*/","",name)

				# update name and path variables
				if (length(multi_edl_names)<j)  multi_edl_names[[j]] <<- tclVar(name)
				else  tclvalue(multi_edl_names[[j]]) <- name

				if (j==1) {
					index <- as.integer(tclObj(edl_index))
					if (index>length(names))  tclvalue(edl_index) <- 1

					first_path = my_filepath
					out_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
					if(my_filepath == out_dir)  tclvalue(edl_dir) <- ""
					else  tclvalue(edl_dir) <- my_filepath
				} else if (!(first_path %in% my_filepath)) {
					# nasty message... shouldn't get here...
				}

				my_rtdf <- sub(".edl?(.gz)?$",".rtdf",name)
				if (length(multi_edl_rtdfs)<j)  multi_edl_rtdfs[[j]] <<- tclVar(my_rtdf)
				else  tclvalue(multi_edl_rtdfs[[j]]) <- my_rtdf

			}
		}
		index <- as.integer(tclObj(edl_index))
		tclvalue(edl_name) <- tclObj(multi_edl_names[[index]])
		tclvalue(edl_rtdf_name) <- tclObj(multi_edl_rtdfs[[index]])
	}
}

#----------------------------------------------------
convertedl_rtdf_browser <-function(...) {
	orig_name = paste(tclObj(edl_rtdf_name),sep="",collapse=" ")
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
		tclvalue(edl_rtdf_name) <- name
		index <- as.integer(tclObj(edl_index))
		tclvalue(multi_edl_rtdfs[[index]]) <- name
		out_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
		if(my_filepath != out_dir) {
			change_Rtdfs_dir(my_filepath)
		}
	}
}

#-----------------------------------------------------
inc_edl_index <- function() {
	my_value <- as.integer(tclObj(edl_index))
	tclvalue(multi_edl_names[[my_value]]) <- tclObj(edl_name)
	tclvalue(multi_edl_rtdfs[[my_value]]) <- tclObj(edl_rtdf_name)

	my_count <- as.integer(tclObj(edl_count))
	if (my_value<my_count) {
		my_value <- my_value + 1
		tclvalue(edl_name) <- tclObj(multi_edl_names[[my_value]])
		tclvalue(edl_rtdf_name) <- tclObj(multi_edl_rtdfs[[my_value]])
	} 
	tclvalue(edl_index) <- my_value
}

#-----------------------------------------------------
dec_edl_index <- function() {
	my_value <- as.integer(tclObj(edl_index))
	tclvalue(multi_edl_names[[my_value]]) <- tclObj(edl_name)
	tclvalue(multi_edl_rtdfs[[my_value]]) <- tclObj(edl_rtdf_name)

	if (my_value>1) {
		my_value <- my_value - 1
		tclvalue(edl_name) <- tclObj(multi_edl_names[[my_value]])
		tclvalue(edl_rtdf_name) <- tclObj(multi_edl_rtdfs[[my_value]])
	}
	tclvalue(edl_index) <- my_value
}

#----------------------------------------------------
ConvertEDLGui_defaults <- function() {
	tclvalue(edl_name) <- ""
#	tclvalue(edl_dir) <- ""
	tclvalue(edl_rtdf_name) <- ""

	tclvalue(edl_count) <- 1
	tclvalue(edl_index) <- 1
	tclvalue(multi_edl_names[[1]]) <- tclVar("")
	tclvalue(multi_edl_rtdfs[[1]]) <- tclVar("")

	tclvalue(auto_93k) <- 1
	tclvalue(use_Pins_used) <- 1
}

#----------------------------------------------------
run_ConvertEDL <-function(done=FALSE,...) {
	my_value <- as.integer(tclObj(edl_index))
	tclvalue(multi_edl_names[[my_value]]) <- tclObj(edl_name)
	tclvalue(multi_edl_rtdfs[[my_value]]) <- tclObj(edl_rtdf_name)

	#edl_name_ <- as.character(tclObj(edl_name))
	#rtdf_name_ <- as.character(tclObj(edl_rtdf_name))
	auto_93k_ <- as.logical(tclObj(auto_93k))
	use_Pins_used_ <- as.logical(tclObj(use_Pins_used))
	edl_dir_ <- paste(tclObj(edl_dir),sep="",collapse=" ")
	output_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")

	full_path = output_dir
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	setwd(full_path)

	count <- as.integer(tclObj(edl_count))
	for (j in 1:count) {
		edl_name_ <- paste(tclObj(multi_edl_names[[j]]),sep="",collapse=" ")
		rtdf_name_ <- paste(tclObj(multi_edl_rtdfs[[j]]),sep="",collapse=" ")

		my_expr = substitute(
			ConvertEDL(edl_name=edl_name_,rtdf_name=rtdf_name_,
						auto_93k=auto_93k_,use_Pins_used=use_Pins_used_,
						edl_dir=edl_dir_)
		)
		tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
		tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
		my_command = sprintf("%s\n",deparse(my_expr))
		my_cmnd = "ConvertEDL(...)\n"
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
		convertedl_win <- get("convertedl_win",envir=.TkRadar.wins)
		tkdestroy(convertedl_win)
	}
}

#----------------------------------------------------
ConvertEDLGui <- function() {

	ConvertEDLGui_defaults()		# initialize variables...
	convertedl_win <- tktoplevel()
	assign("convertedl_win",convertedl_win,envir=.TkRadar.wins)
	tkwm.title(convertedl_win, "ConvertEDL")
		
	bottom_row <- tkframe(convertedl_win)
	default_button <- tkbutton(bottom_row,
						text="DEFAULTS",
						#anchor="w",
						width=12,
						command=ConvertEDLGui_defaults)
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
						text="RUN & QUIT",
						#anchor="w",
						width=12,
						command=function() run_ConvertEDL(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
						text="QUIT",
						#anchor="w",
						width=12,
						command=function()tkdestroy(convertedl_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
						text="RUN",
						#anchor="w",
						width=12,
						command=function() run_ConvertEDL(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	dir_entry_frame <- tkframe(convertedl_win)
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

	edl_dir_entry_frame <- tkframe(convertedl_win)
	edl_dir_entry_label <- tklabel(edl_dir_entry_frame,
						width=10,
						text="edl_dir")
	tkpack(edl_dir_entry_label,side="left")
	edl_dir_entry <- tklabel(edl_dir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=edl_dir)
	tkpack(edl_dir_entry,side="left",fill="x",expand=1)
	edl_dir_browse <- tkbutton(edl_dir_entry_frame,
						text="Browse",
						command=function() dir_browser(edl_dir))
	tkpack(edl_dir_browse,side="right")
	tkpack(edl_dir_entry_frame,side="top",anchor="w",fill="x")

	multiple_frame <- tkframe(convertedl_win)
	multiple_label <- tklabel(multiple_frame,
						#width=10,
						text="file")
	tkpack(multiple_label,side="left")
	index_value <- tklabel(multiple_frame,
						width=3,
						relief="sunken",
						textvariable=edl_index)
	tkpack(index_value,side="left")
	multiple_label2 <- tklabel(multiple_frame,
						#width=10,
						text="of")
	tkpack(multiple_label2,side="left")
	count_value <- tklabel(multiple_frame,
						width=3,
						relief="sunken",
						textvariable=edl_count)
	tkpack(count_value,side="left")
	index_minus <- tkbutton(multiple_frame,
						text="-",
						command=dec_edl_index)
	tkpack(index_minus,side="left")
	index_plus <- tkbutton(multiple_frame,
						text="+",
						command=inc_edl_index)
	tkpack(index_plus,side="left")
	tkpack(multiple_frame,side="top",anchor="w",fill="x")

	edl_entry_frame <- tkframe(convertedl_win)
	edl_entry_label <- tklabel(edl_entry_frame,
						width=10,
						text="edl_name")
	tkpack(edl_entry_label,side="left")
	edl_entry <- tkentry(edl_entry_frame,
						width=50,
						textvariable=edl_name)
	tkpack(edl_entry,side="left",fill="x",expand=1)
	edl_browse <- tkbutton(edl_entry_frame,
						text="Browse",
						command=edl_browser)
	tkpack(edl_browse,side="right")
	tkpack(edl_entry_frame,side="top",anchor="w",fill="x")

	rtdf_entry_frame <- tkframe(convertedl_win)
	rtdf_entry_label <- tklabel(rtdf_entry_frame,
						width=10,
						text="rtdf_name")
	tkpack(rtdf_entry_label,side="left")
	rtdf_entry <- tkentry(rtdf_entry_frame,
						width=20,
						textvariable=edl_rtdf_name)
	tkpack(rtdf_entry,side="left",fill="x",expand=1)
	rtdf_browse <- tkbutton(rtdf_entry_frame,
						text="Browse",
						command=convertedl_rtdf_browser)
	tkpack(rtdf_browse,side="right")
	tkpack(rtdf_entry_frame,side="top",anchor="w",fill="x")

	auto93_button <- tkcheckbutton(convertedl_win,
						text="auto_93k",
						variable=auto_93k)
	tkpack(auto93_button,side="top",anchor="w")

	use_Pins_used_button <- tkcheckbutton(convertedl_win,
						text="use_Pins_used",
						variable=use_Pins_used)
	tkpack(use_Pins_used_button,side="top",anchor="w")

}


