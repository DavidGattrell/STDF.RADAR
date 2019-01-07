# RemoveDevicesAtIndicesGui.R
#
# $Id: RemoveDevicesAtIndicesGui.R,v 1.5 2010/11/24 01:43:18 David Exp $
#
# Tk/Tcl GUI wrapper for calling RemoveDevicesAtIndices.R
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
# RemoveDevicesAtIndicesGui.R specific variables
#-----------------------------------------
if(!exists(".TkRadar.env")) .TkRadar.env <- new.env()

assign("rdai_out_file",tclVar("screened.rtdf"),envir=.TkRadar.env)
assign("rem_indices",tclVar(0),envir=.TkRadar.env)
rdai_in_name <- tclVar("")
rdai_in_dir <- tclVar("")


#----------------------------------------------------
RemoveDevicesAtIndicesGui_defaults <- function() {
	tclvalue(rem_indices) <- 0
	tclvalue(rdai_out_file) <- "screened.rtdf"
	in_dir <- paste(tclObj(Rtdf_dir),sep="",collapse=" ")
	in_name <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
	if(nchar(in_name)>0) {
		tclvalue(rdai_in_dir) <- tclObj(Rtdf_dir)
		tclvalue(rdai_in_name) <- tclObj(Rtdf_name)
	} else {
		if(nchar(in_dir)>0) {
			tclvalue(rdai_in_dir) <- tclObj(Rtdf_dir)
			tclvalue(rdai_in_name) <- tclObj(Rtdf_name)
		} else {
			tclvalue(rdai_in_dir) <- tclObj(Rtdfs_dir)
			tclvalue(rdai_in_name) <- tclObj(Rtdf_name)
		}
	}
}


#----------------------------------------------------
rdai_out_rtdf_browser <-function(...) {

	orig_name = paste(tclObj(rdai_out_file),sep="",collapse=" ")

	init_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
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
		tclvalue(rdai_out_file) <- name

		# if we changed directory... update paths
		out_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
		if(my_filepath != out_dir) {
			change_Rtdfs_dir(my_filepath)
		}
	}
}


#----------------------------------------------------
run_RemoveDevicesAtIndices <-function(done=FALSE,...) {

	in_file_ <- paste(tclObj(rdai_in_name),sep="",collapse=" ")
	indices_ <- as.numeric(tclObj(rem_indices))
	out_file_ <- paste(tclObj(rdai_out_file),sep="",collapse=" ")
	in_dir_ <- paste(tclObj(rdai_in_dir),sep="",collapse=" ")
	output_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")

	if(length(indices_)==1 && indices_==0)  indices_=NaN

	full_path = output_dir
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	setwd(full_path)

	my_expr = substitute(
		RemoveDevicesAtIndices(in_file=in_file_,indices=indices_,
					out_file=out_file_,in_dir=in_dir_)
	)
	tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
	tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
	my_command = sprintf("%s\n",deparse(my_expr))
	my_cmnd = "RemoveDevicesAtIndices(...)\n"
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
		remdevatindx_win <- get("remdevatindx_win",envir=.TkRadar.wins)
		tkdestroy(remdevatindx_win)
	}
}


#----------------------------------------------------
 RemoveDevicesAtIndicesGui <- function() {

	RemoveDevicesAtIndicesGui_defaults()		# initialize variables...
	remdevatindx_win <- tktoplevel()
	assign("remdevatindx_win",remdevatindx_win,envir=.TkRadar.wins)
	tkwm.title(remdevatindx_win, "RemoveDevicesAtIndices")
		
	bottom_row <- tkframe(remdevatindx_win)
	default_button <- tkbutton(bottom_row,
						text="DEFAULTS",
						#anchor="w",
						width=12,
						command=RemoveDevicesAtIndicesGui_defaults)
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
						text="RUN & QUIT",
						#anchor="w",
						width=12,
						command=function() run_RemoveDevicesAtIndices(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
						text="QUIT",
						#anchor="w",
						width=12,
						command=function()tkdestroy(remdevatindx_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
						text="RUN",
						#anchor="w",
						width=12,
						command=function() run_RemoveDevicesAtIndices(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	dir_entry_frame <- tkframe(remdevatindx_win)
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

	in_dir_entry_frame <- tkframe(remdevatindx_win)
	in_dir_entry_label <- tklabel(in_dir_entry_frame,
						width=10,
						text="in_dir")
	tkpack(in_dir_entry_label,side="left")
	in_dir_entry <- tklabel(in_dir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=rdai_in_dir)
	tkpack(in_dir_entry,side="left",fill="x",expand=1)
	in_dir_browse <- tkbutton(in_dir_entry_frame,
						text="Browse",
						command=function() dir_browser(rdai_in_dir))
	tkpack(in_dir_browse,side="right")
	tkpack(in_dir_entry_frame,side="top",anchor="w",fill="x")

	in_entry_frame <- tkframe(remdevatindx_win)
	in_entry_label <- tklabel(in_entry_frame,
						width=10,
						text="in_file")
	tkpack(in_entry_label,side="left")
	in_entry <- tkentry(in_entry_frame,
						width=50,
						textvariable=rdai_in_name)
	tkpack(in_entry,side="left",fill="x",expand=1)
	in_browse <- tkbutton(in_entry_frame,
						text="Browse",
						command=function() rtdf_browser(rdai_in_name,rdai_in_dir))
	tkpack(in_browse,side="right")
	tkpack(in_entry_frame,side="top",anchor="w",fill="x")

	# - - - - - - - - - - - - - - - -

	num_entry_frame <- tkframe(remdevatindx_win)
	num_entry_label <- tklabel(num_entry_frame,
						width=10,
						text="indices")
	tkpack(num_entry_label,side="left")
	num_entry <- tklabel(num_entry_frame,
						width=20,
						relief="sunken",
						textvariable=rem_indices)
	tkpack(num_entry,side="left",fill="x",expand=1)
	num_browse <- tkbutton(num_entry_frame,
						text="Edit",
						command=function() indices_entry(rem_indices))
	tkpack(num_browse,side="right")
	tkpack(num_entry_frame,side="top",anchor="w",fill="x")

	# - - - - - - - - - - - - - - - -

	out_entry_frame <- tkframe(remdevatindx_win)
	out_entry_label <- tklabel(out_entry_frame,
						width=10,
						text="out_file")
	tkpack(out_entry_label,side="left")
	out_entry <- tkentry(out_entry_frame,
						width=20,
						textvariable=rdai_out_file)
	tkpack(out_entry,side="left",fill="x",expand=1)
	out_browse <- tkbutton(out_entry_frame,
						text="Browse",
						command=rdai_out_rtdf_browser)
	tkpack(out_browse,side="right")
	tkpack(out_entry_frame,side="top",anchor="w",fill="x")

}


