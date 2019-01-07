# ShrinkRetestsGui.R
#
# $Id: ShrinkRetestsGui.R,v 1.6 2010/11/24 01:49:40 David Exp $
#
# Tk/Tcl GUI wrapper for calling ShrinkRetests.R
# called by TkRadar.R
#
# Copyright (C) 2008-2010 David Gattrell
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
# ShrinkRetestsGui specific variables
#-----------------------------------------
if(!exists(".TkRadar.env")) .TkRadar.env <- new.env()

#in_name <- tclVar("")
#in_dir <- tclVar("")
assign("shrink_out_file",tclVar(""),envir=.TkRadar.env)
assign("use_xy_coords",tclVar(0),envir=.TkRadar.env)

# these defaults can be controlled in the .Rprofile file:
default_use_xy_coords <- tclVar(0)		

#----------------------------------------------------
ShrinkRetestsGui_defaults <- function() {
	tclvalue(shrink_out_file) <- "shrunk.rtdf"
#	tclvalue(in_dir) <- ""
#	tclvalue(in_name) <- as.character(tclObj(Rtdf_name))
	tclvalue(use_xy_coords) <- tclObj(default_use_xy_coords)
}


#----------------------------------------------------
shrinkretest_rtdf_browser <-function(...) {
	orig_name = paste(tclObj(shrink_out_file),sep="",collapse=" ")
	if (nchar(orig_name)<1)  orig_name="shrunk.rtdf"

	init_dir = paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
	if (nchar(init_dir)<1) {
		init_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(init_dir)<1) {
		init_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	if(as.numeric(tclObj(Bad_Vista))>0) {
		rtdf_str = "{{All files} *} {{RTDF Files} {.rtdf .Rtdf .Rdata}}"
	} else {
		rtdf_str = "{{RTDF Files} {.rtdf .Rtdf .Rdata}} {{All files} *}"
	}
	if (nchar(init_dir)>0) {
		name <- tclvalue(tkgetSaveFile(
				initialdir=init_dir,
				initialfile=orig_name,
				filetypes=rtdf_str))
	} else {
		name <- tclvalue(tkgetSaveFile(
				initialfile=orig_name,
				filetypes=rtdf_str))
	}

	if (nchar(name)>0) {
		my_filepath = sub("/[^/]*$","",name)	# strip off filename, just path left
		name=sub("^.*/","",name)
		tclvalue(shrink_out_file) <- name

		# if we changed directory... update paths
		out_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
		if(my_filepath != out_dir) {
			change_Rtdfs_dir(my_filepath)
		}
	}
}


#----------------------------------------------------
run_ShrinkRetests <-function(done=FALSE,...) {

	in_file_ <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
	out_file_ <- paste(tclObj(shrink_out_file),sep="",collapse=" ")
	in_dir_ <- paste(tclObj(Rtdf_dir),sep="",collapse=" ")
	output_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
	use_xy <- as.logical(tclObj(use_xy_coords))

	full_path = output_dir
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	setwd(full_path)

	my_expr = substitute(
		ShrinkRetests(in_file=in_file_,out_file=out_file_,
						  in_dir=in_dir_,use_xy_coords=use_xy)
	)
	tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
	tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
	my_command = sprintf("%s\n",deparse(my_expr))
	my_cmnd = "ShrinkRetests(...)\n"
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

	# now update Rtdf_name and Rtdf_dir...
	tclvalue(Rtdf_name) <- out_file_
	tclvalue(Rtdf_dir) <- output_dir

	if(done>0) {
		shrinkretests_win <- get("shrinkretests_win",envir=.TkRadar.wins)
		tkdestroy(shrinkretests_win)
	}
}


#----------------------------------------------------
ShrinkRetestsGui <- function() {

	ShrinkRetestsGui_defaults()		# initialize variables...
	shrinkretests_win <- tktoplevel()
	assign("shrinkretests_win",shrinkretests_win,envir=.TkRadar.wins)
	tkwm.title(shrinkretests_win, "ShrinkRetests")
		
	bottom_row <- tkframe(shrinkretests_win)
	default_button <- tkbutton(bottom_row,
						text="DEFAULTS",
						#anchor="w",
						width=12,
						command=ShrinkRetestsGui_defaults)
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
						text="RUN & QUIT",
						#anchor="w",
						width=12,
						command=function() run_ShrinkRetests(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
						text="QUIT",
						#anchor="w",
						width=12,
						command=function()tkdestroy(shrinkretests_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
						text="RUN",
						#anchor="w",
						width=12,
						command=function() run_ShrinkRetests(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	dir_entry_frame <- tkframe(shrinkretests_win)
	dir_entry_label <- tklabel(dir_entry_frame,
						width=10,
						text="directory")
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

	in_dir_entry_frame <- tkframe(shrinkretests_win)
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
						command=function() dir_browser(in_dir))
	tkpack(in_dir_browse,side="right")
	tkpack(in_dir_entry_frame,side="top",anchor="w",fill="x")

	in_entry_frame <- tkframe(shrinkretests_win)
	in_entry_label <- tklabel(in_entry_frame,
						width=10,
						text="in_file")
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

	out_entry_frame <- tkframe(shrinkretests_win)
	out_entry_label <- tklabel(out_entry_frame,
						width=10,
						text="out_file")
	tkpack(out_entry_label,side="left")
	out_entry <- tkentry(out_entry_frame,
						width=20,
						textvariable=shrink_out_file)
	tkpack(out_entry,side="left",fill="x",expand=1)
	out_browse <- tkbutton(out_entry_frame,
						text="Browse",
						command=shrinkretest_rtdf_browser)
	tkpack(out_browse,side="right")
	tkpack(out_entry_frame,side="top",anchor="w",fill="x")

	use_xy_button <- tkcheckbutton(shrinkretests_win,
						text="use_xy_coords",
						variable=use_xy_coords)
	tkpack(use_xy_button,side="top",anchor="w")
}



