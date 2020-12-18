# ExpandMPRsGui.R
#
# $Id: ExpandMPRsGui.R,v 1.8 2020/12/18 01:17:27 david Exp $
#
# Tk/Tcl GUI wrapper for calling ExpandMPRs.R
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
# ExpandMPRsGui specific variables
#-----------------------------------------
if(!exists(".TkRadar.env")) .TkRadar.env <- new.env()

#stdf_name <- tclVar("")	# shared with ConvertStdfGui
#stdf_dir <- tclVar("")		# shared with ConvertStdfGui
#new_stdf <- tclVar("")		# moved to ConvertStdfGui

inc_tnums <- tclVar(50)
out_stdf_dir <- tclVar("")

#stdf_count <- tclVar(1)	# shared with ConvertStdfGui
#stdf_index <- tclVar(1)	# shared with ConvertStdfGui
#multi_stdf_names <- list()		# shared with ConvertStdfGui
#multi_stdf_stdfs <- list()		# shared with ConvertStdfGui
#multi_stdf_names[[1]] <- tclVar("")	# shared with ConvertStdfGui
#multi_stdf_stdfs[[1]] <- tclVar("")	# shared with ConvertStdfGui

#----------------------------------------------------
ExpandMPRsGui_defaults <- function() {
	tclvalue(stdf_name) <- ""
#	tclvalue(stdf_dir) <- ""
	tclvalue(out_stdf_dir) <- tclObj(Output_dir)
	tclvalue(inc_tnums) <- 50
	tclvalue(new_stdf) <- ""

	tclvalue(stdf_count) <- 1
	tclvalue(stdf_index) <- 1
	tclvalue(multi_stdf_names[[1]]) <- tclVar("")
	tclvalue(multi_stdf_stdfs[[1]]) <- tclVar("")
}


#----------------------------------------------------
out_stdf_browser <-function(...) {

	my_dir <- paste(tclObj(out_stdf_dir),sep="",collapse=" ")
	if (nchar(my_dir)<1) {
		my_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(my_dir)<1) {
		my_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	if(as.numeric(tclObj(Bad_Vista))>0) {
		stdf_str = "{{All files} *} {{STDF Files} {.stdf .std .stdf.gz .std.gz}}"
	} else {
		stdf_str = "{{STDF Files} {.stdf .std .stdf.gz .std.gz}} {{All files} *}"
	}
	if (nchar(my_dir)>0) {
		name <- tclvalue(tkgetOpenFile(filetypes=stdf_str,
				initialdir=my_dir))
	} else {
		name <- tclvalue(tkgetOpenFile(filetypes=stdf_str))
	}

	if (nchar(name)>0) {
		my_filepath = sub("/[^/]*$","",name)	# strip off filename, just path left
		name=sub("^.*/","",name)
		tclvalue(out_stdf) <- name
		index <- as.integer(tclObj(stdf_index))
		tclvalue(multi_stdf_stdfs[[index]]) <- name

		# if we changed directory... update paths
		out_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
		if (nchar(out_dir)<1) {
			out_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
		}
		if(my_filepath != out_dir) {
			tclvalue(out_stdf_dir) <- my_filepath
		} else {
			tclvalue(out_stdf_dir) <- ""
		}
	}
}


#----------------------------------------------------
run_ExpandMPRs <-function(done=FALSE,...) {
	my_value <- as.integer(tclObj(stdf_index))
	tclvalue(multi_stdf_names[[my_value]]) <- tclObj(stdf_name)
	tclvalue(multi_stdf_stdfs[[my_value]]) <- tclObj(new_stdf)

	#stdf_name_ <- paste(tclObj(stdf_name),sep="",collapse=" ")
	#new_stdf_ <- paste(tclObj(new_stdf),sep="",collapse=" ")
	inc_tnums_ <- as.numeric(tclObj(inc_tnums))
	in_dir_ <- paste(tclObj(stdf_dir),sep="",collapse=" ")
	output_dir <- paste(tclObj(out_stdf_dir),sep="",collapse=" ")
	full_path = output_dir
	if (nchar(full_path)<1)  full_path <- paste(tclObj(Output_dir),sep="",collapse=" ")
	if (nchar(full_path)<1)  full_path <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	setwd(full_path)

	count <- as.integer(tclObj(stdf_count))
	for (j in 1:count) {
		stdf_name_ <- paste(tclObj(multi_stdf_names[[j]]),sep="",collapse=" ")
		new_stdf_ <- paste(tclObj(multi_stdf_stdfs[[j]]),sep="",collapse=" ")
		my_expr = substitute(
				ExpandMPRs(stdf_name=stdf_name_,new_stdf=new_stdf_,
							  inc_tnums=inc_tnums_,in_dir=in_dir_)
			)
		tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
		tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
		my_command = sprintf("%s\n",deparse(my_expr))
		my_cmnd = "ExpandMPRs(...)\n"
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

	# now update stdf_name and stdf_dir...
	tclvalue(stdf_name) <- new_stdf_
	tclvalue(stdf_dir) <- output_dir

	if(done>0) {
		expandmprs_win <- get("expandmprs_win",envir=.TkRadar.wins)
		tkdestroy(expandmprs_win)
	}
}


#----------------------------------------------------
ExpandMPRsGui <- function() {

	ExpandMPRsGui_defaults()		# initialize variables...
	expandmprs_win <- tktoplevel()
	assign("expandmprs_win",expandmprs_win,envir=.TkRadar.wins)
	tkwm.title(expandmprs_win, "ExpandMPRs")
		
	bottom_row <- tkframe(expandmprs_win)
	default_button <- tkbutton(bottom_row,
						text="DEFAULTS",
						#anchor="w",
						width=12,
						command=ExpandMPRsGui_defaults)
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
						text="RUN & QUIT",
						#anchor="w",
						width=12,
						command=function() run_ExpandMPRs(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
						text="QUIT",
						#anchor="w",
						width=12,
						command=function()tkdestroy(expandmprs_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
						text="RUN",
						#anchor="w",
						width=12,
						command=function() run_ExpandMPRs(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	dir_entry_frame <- tkframe(expandmprs_win)
	dir_entry_label <- tklabel(dir_entry_frame,
						width=10,
						text="out_stdf_dir")
	tkpack(dir_entry_label,side="left")
	dir_entry <- tklabel(dir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=out_stdf_dir)
	tkpack(dir_entry,side="left",fill="x",expand=1)
	dir_browse <- tkbutton(dir_entry_frame,
						text="Browse",
						command=function() dir_browser(out_stdf_dir))
	tkpack(dir_browse,side="right")
	tkpack(dir_entry_frame,side="top",anchor="w",fill="x")

	in_dir_entry_frame <- tkframe(expandmprs_win)
	in_dir_entry_label <- tklabel(in_dir_entry_frame,
						width=10,
						text="in_dir")
	tkpack(in_dir_entry_label,side="left")
	in_dir_entry <- tklabel(in_dir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=stdf_dir)
	tkpack(in_dir_entry,side="left",fill="x",expand=1)
	in_dir_browse <- tkbutton(in_dir_entry_frame,
						text="Browse",
						command=function() dir_browser(stdf_dir))
	tkpack(in_dir_browse,side="right")
	tkpack(in_dir_entry_frame,side="top",anchor="w",fill="x")

	multiple_frame <- tkframe(expandmprs_win)
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

	in_entry_frame <- tkframe(expandmprs_win)
	in_entry_label <- tklabel(in_entry_frame,
						width=10,
						text="stdf_name")
	tkpack(in_entry_label,side="left")
	in_entry <- tkentry(in_entry_frame,
						width=50,
						textvariable=stdf_name)
	tkpack(in_entry,side="left",fill="x",expand=1)
	in_browse <- tkbutton(in_entry_frame,
						text="Browse",
						command=stdf_browser)
	tkpack(in_browse,side="right")
	tkpack(in_entry_frame,side="top",anchor="w",fill="x")

	tnums_frame <- tkframe(expandmprs_win)
	tnums_label <- tklabel(tnums_frame,
						width=10,
						text="inc_tnums")
	tkpack(tnums_label,side="left")
	tnums_entry <- tklabel(tnums_frame,
						width=20,
						relief="sunken",
						textvariable=inc_tnums)
	tkpack(tnums_entry,side="left",fill="x",expand=1)
	tnums_browse <- tkbutton(tnums_frame,
						text="Edit",
						command=function() integer_entry(inc_tnums))
	tkpack(tnums_browse,side="right")
	tkpack(tnums_frame,side="top",anchor="w",fill="x")

	out_entry_frame <- tkframe(expandmprs_win)
	out_entry_label <- tklabel(out_entry_frame,
						width=10,
						text="new_stdf")
	tkpack(out_entry_label,side="left")
	out_entry <- tkentry(out_entry_frame,
						width=20,
						textvariable=new_stdf)
	tkpack(out_entry,side="left",fill="x",expand=1)
	out_browse <- tkbutton(out_entry_frame,
						text="Browse",
						command=out_stdf_browser)
	tkpack(out_browse,side="right")
	tkpack(out_entry_frame,side="top",anchor="w",fill="x")

}


