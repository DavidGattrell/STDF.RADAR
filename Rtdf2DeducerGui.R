# Rtdf2DeducerGui.R
#
# $Id: Rtdf2DeducerGui.R,v 1.1 2012/10/21 20:00:57 david Exp $
#
# Tk/Tcl GUI wrapper for calling Rtdf2Deducer.R
# called by TkRadar.R
#
# Copyright (C) 2012 David Gattrell
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
# Rtdf2DeducerGui specific variables
#-----------------------------------------
if(!exists(".TkRadar.env")) .TkRadar.env <- new.env()

#in_name <- tclVar("")
#in_dir <- tclVar("")
assign("deduce_out_name",tclVar(""),envir=.TkRadar.env)


#----------------------------------------------------
Rtdf2DeducerGui_defaults <- function() {
	tclvalue(deduce_out_name) <- ""
#	tclvalue(in_dir) <- ""
#	tclvalue(in_name) <- as.character(tclObj(Rtdf_name))
}


#----------------------------------------------------
deduce_out_rdata_browser <-function(...) {
	orig_name = paste(tclObj(deduce_out_name),sep="",collapse=" ")
	if (nchar(orig_name)<1)  orig_name="new.rdata"

	init_dir = paste(tclObj(Output_dir),sep="",collapse=" ")
	if (nchar(init_dir)<1) {
		init_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	if(as.numeric(tclObj(Bad_Vista))>0) {
		rdata_str = "{{All files} *} {{Rdata Files} {.rdata .Rdata}}"
	} else {
		rdata_str = "{{Rdata Files} {.rdata .Rdata}} {{All files} *}"
	}
	if (nchar(init_dir)>0) {
		name <- tclvalue(tkgetSaveFile(
				initialdir=init_dir,
				initialfile=orig_name,
				filetypes=rdata_str))
	} else {
		name <- tclvalue(tkgetSaveFile(
				initialfile=orig_name,
				filetypes=rdata_str))
	}

	if (nchar(name)>0) {
		my_filepath = sub("/[^/]*$","",name)	# strip off filename, just path left
		name=sub("^.*/","",name)
		tclvalue(deduce_out_name) <- name

		# if we changed directory... update paths
		out_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
		if(my_filepath != out_dir) {
			change_Output_dir(my_filepath)
			#tclvalue(Output_dir) <- my_filepath
		}
	}
}


#----------------------------------------------------
run_Rtdf2Deducer <-function(done=FALSE,...) {

	in_file_ <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
	in_dir_ <- paste(tclObj(Rtdf_dir),sep="",collapse=" ")
	if (nchar(in_dir_)<1) {
		in_dir_ <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(in_dir_)<1) {
		in_dir_ <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}

	out_file_ <- paste(tclObj(save_out_name),sep="",collapse=" ")
	output_dir_ <- paste(tclObj(Output_dir),sep="",collapse=" ")
	if (nchar(output_dir_)<1) {
		output_dir_ <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}

	setwd(output_dir_)

	my_expr = substitute(
		Rtdf2Deducer(rtdf_name=in_file_,rdata_name=out_file_,
						in_dir=in_dir_,output_dir=output_dir_)
	)
	tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
	tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
	my_command = sprintf("%s\n",deparse(my_expr))
	my_cmnd = "Rtdf2Deducer(...)\n"
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
	cat("Finished!\n")


	if(done>0) {
		rtdf2deducer_win <- get("rtdf2deducer_win",envir=.TkRadar.wins)
		tkdestroy(rtdf2deducer_win)
	}
}


#----------------------------------------------------
Rtdf2DeducerGui <- function() {

	Rtdf2DeducerGui_defaults()		# initialize variables...
	rtdf2deducer_win <- tktoplevel()
	assign("rtdf2deducer_win",rtdf2deducer_win,envir=.TkRadar.wins)
	tkwm.title(rtdf2deducer_win, "Rtdf2Deducer")
		
	bottom_row <- tkframe(rtdf2deducer_win)
	default_button <- tkbutton(bottom_row,
						text="DEFAULTS",
						#anchor="w",
						width=12,
						command=SaveRtdfGui_defaults)
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
						text="RUN & QUIT",
						#anchor="w",
						width=12,
						command=function() run_Rtdf2Deducer(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
						text="QUIT",
						#anchor="w",
						width=12,
						command=function()tkdestroy(rtdf2deducer_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
						text="RUN",
						#anchor="w",
						width=12,
						command=function() run_Rtdf2Deducer(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	in_dir_entry_frame <- tkframe(rtdf2deducer_win)
	in_dir_entry_label <- tklabel(in_dir_entry_frame,
						width=10,
						text="in_dir")
	tkpack(in_dir_entry_label,side="left")
	in_dir_entry <- tklabel(in_dir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=Rtdf_dir)
	tkpack(in_dir_entry,side="left",fill="x",expand=1)
	in_dir_browse <- tkbutton(in_dir_entry_frame,
						text="Browse",
						command=function() dir_browser(Rtdf_dir))
	tkpack(in_dir_browse,side="right")
	tkpack(in_dir_entry_frame,side="top",anchor="w",fill="x")

	dir_entry_frame <- tkframe(rtdf2deducer_win)
	dir_entry_label <- tklabel(dir_entry_frame,
						width=10,
						text="output_dir")
	tkpack(dir_entry_label,side="left")
	dir_entry <- tklabel(dir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=Output_dir)
	tkpack(dir_entry,side="left",fill="x",expand=1)
	dir_browse <- tkbutton(dir_entry_frame,
						text="Browse",
						command=function() dir_browser(Output_dir))
	tkpack(dir_browse,side="right")
	tkpack(dir_entry_frame,side="top",anchor="w",fill="x")

	in_entry_frame <- tkframe(rtdf2deducer_win)
	in_entry_label <- tklabel(in_entry_frame,
						width=10,
						text="rtdf_name")
	tkpack(in_entry_label,side="left")
	in_entry <- tkentry(in_entry_frame,
						width=50,
						textvariable=Rtdf_name)
	tkpack(in_entry,side="left",fill="x",expand=1)
	in_browse <- tkbutton(in_entry_frame,
						text="Browse",
						command=function() rtdf_browser(Rtdf_name,Rtdf_dir))
	tkpack(in_browse,side="right")
	tkpack(in_entry_frame,side="top",anchor="w",fill="x")

	out_entry_frame <- tkframe(rtdf2deducer_win)
	out_entry_label <- tklabel(out_entry_frame,
						width=10,
						text="rdata_name")
	tkpack(out_entry_label,side="left")
	out_entry <- tkentry(out_entry_frame,
						width=20,
						textvariable=deduce_out_name)
	tkpack(out_entry,side="left",fill="x",expand=1)
	out_browse <- tkbutton(out_entry_frame,
						text="Browse",
						command=deduce_out_rdata_browser)
	tkpack(out_browse,side="right")
	tkpack(out_entry_frame,side="top",anchor="w",fill="x")

}


