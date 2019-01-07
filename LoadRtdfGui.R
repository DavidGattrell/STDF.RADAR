# LoadRtdfGui.R
#
# $Id: LoadRtdfGui.R,v 1.4 2009/12/29 20:14:08 David Exp $
#
# Tk/Tcl GUI wrapper for calling LoadRtdf.R
# called by Radar.R
#
# Copyright (C) 2008-2009 David Gattrell
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
# LoadRtdfGui specific variables
#-----------------------------------------
if(!exists(".TkRadar.env")) .TkRadar.env <- new.env()


#----------------------------------------------------
LoadRtdfGui_defaults <- function() {
#	tclvalue(in_dir) <- as.character(tclObj(Rtdf_dir))
#	tclvalue(in_name) <- as.character(tclObj(Rtdf_name))
}



#----------------------------------------------------
run_LoadRtdf <-function(done=FALSE,...) {

	in_file_ <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
	in_dir_ <- paste(tclObj(Rtdf_dir),sep="",collapse=" ")

	if (nchar(in_dir_)<1) {
		in_dir_ <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(in_dir_)<1) {
		in_dir_ <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}

	setwd(in_dir_)

	my_expr = substitute(
		LoadRtdf(rtdf_name=in_file_,in_dir=in_dir_)
	)
	tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
	tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
	my_command = sprintf("%s\n",deparse(my_expr))
	my_cmnd = "LoadRtdf(...)\n"
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
	eval(my_expr)
	cat("Finished!\n")

	if(done>0) {
		loadrtdf_win <- get("loadrtdf_win",envir=.TkRadar.wins)
		tkdestroy(loadrtdf_win)
	}
}


#----------------------------------------------------
LoadRtdfGui <- function() {

	LoadRtdfGui_defaults()		# initialize variables...
	loadrtdf_win <- tktoplevel()
	assign("loadrtdf_win",loadrtdf_win,envir=.TkRadar.wins)
	tkwm.title(loadrtdf_win, "LoadRtdf")
		
	bottom_row <- tkframe(loadrtdf_win)
	default_button <- tkbutton(bottom_row,
						text="DEFAULTS",
						#anchor="w",
						width=12,
						command=LoadRtdfGui_defaults)
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
						text="RUN & QUIT",
						#anchor="w",
						width=12,
						command=function() run_LoadRtdf(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
						text="QUIT",
						#anchor="w",
						width=12,
						command=function()tkdestroy(loadrtdf_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
						text="RUN",
						#anchor="w",
						width=12,
						command=function() run_LoadRtdf(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	in_dir_entry_frame <- tkframe(loadrtdf_win)
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

	in_entry_frame <- tkframe(loadrtdf_win)
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

}


