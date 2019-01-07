# FilterByBinningGui.R
#
# $Id: FilterByBinningGui.R,v 1.4 2009/12/29 20:12:24 David Exp $
#
# Tk/Tcl GUI wrapper for calling FilterByBinning.R
# called by TkRadar.R
#
# Copyright (C) 2009 David Gattrell
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
if(!exists(".TkRadar.env"))  .TkRadar.env <- new.env()

# FilterByResult gui specific variables
#-----------------------------------------
filtbin_action <- tclVar("keep")
filtbin_type <- tclVar("hbin")
filtbin_bins <- tclVar(1)
filtbin_outfile <- tclVar("filteredbybin.rtdf")


#-----------------------------------------
FilterByBinningGui_defaults <- function(...) {
	tclvalue(filtbin_outfile) <- "filteredbybin.rtdf"

	tclvalue(filtbin_action) <- "keep"
	tclvalue(filtbin_type) <- "hbin"
	tclvalue(filtbin_bins) <- 1
	
}


#-----------------------------------------
run_FilterByBinning <-function(done=FALSE,..) {
	in_rtdf <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
	action_ <- as.character(tclObj(filtbin_action))
	filter_ <- as.character(tclObj(filtbin_type))
	bins_ <- as.numeric(tclObj(filtbin_bins))
	out_rtdf <- paste(tclObj(filtbin_outfile),sep="",collapse=" ")

	rtdf_dir_ <- paste(tclObj(Rtdf_dir),sep="",collapse=" ")
	output_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")

	if(rtdf_dir_ != output_dir) {
		if (nchar(rtdf_dir_)<1)  rtdf_dir_ <- paste(tclObj(Output_dir),sep="",collapse=" ")
		if (nchar(rtdf_dir_)<1)  rtdf_dir_ <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}

	# go to output directory...
	full_path = output_dir
	if (nchar(full_path)<1)  full_path <- paste(tclObj(Output_dir),sep="",collapse=" ")
	if (nchar(full_path)<1)  full_path <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	setwd(full_path)

	my_expr = substitute(
		FilterByBinning(in_file=in_rtdf,action=action_,bin_type=filter_,
				bins=bins_,out_file=out_rtdf,in_dir=rtdf_dir_)
	)
	tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
	tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
	my_command = sprintf("%s\n",deparse(my_expr))
	my_cmnd = "FilterByBinning(...)\n"
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
	tclvalue(Rtdf_name) <- out_rtdf
	tclvalue(Rtdf_dir) <- output_dir

	if(done>0) {
		filterbybinning_win <- get("filterbybinning_win",envir=.TkRadar.wins)
		tkdestroy(filterbybinning_win)
	}

}

#-----------------------------------------
FilterByBinningGui <- function(...) {

	FilterByBinningGui_defaults()		# initialize variables...
	filterbybinning_win <- tktoplevel()
	assign("filterbybinning_win",filterbybinning_win,envir=.TkRadar.wins)
	tkwm.title(filterbybinning_win, "FilterByBinning")
	
	bottom_row <- tkframe(filterbybinning_win)
	default_button <- tkbutton(bottom_row,
					text="DEFAULTS",
					#anchor="w",
					width=12,
					command=FilterByBinningGui_defaults)
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
					text="RUN & QUIT",
					#anchor="w",
					width=12,
					command=function() run_FilterByBinning(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
					text="QUIT",
					#anchor="w",
					width=12,
					command=function()tkdestroy(filterbybinning_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
					text="RUN",
					#anchor="w",
					width=12,
					command=function() run_FilterByBinning(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	rtdf_dir_entry_frame <- tkframe(filterbybinning_win)
	rtdf_dir_entry_label <- tklabel(rtdf_dir_entry_frame,
						width=10,
						text="in_dir")
	tkpack(rtdf_dir_entry_label,side="left")
	rtdf_dir_entry <- tklabel(rtdf_dir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=Rtdf_dir)
	tkpack(rtdf_dir_entry,side="left",fill="x",expand=1)
	rtdf_dir_browse <- tkbutton(rtdf_dir_entry_frame,
						text="Browse",
						command=function() dir_browser(Rtdf_dir))
	tkpack(rtdf_dir_browse,side="right")
	tkpack(rtdf_dir_entry_frame,side="top",anchor="w",fill="x")

	rtdf_entry_frame <- tkframe(filterbybinning_win)
	rtdf_entry_label <- tklabel(rtdf_entry_frame,
						width=10,
						text="rtdf_file")
	tkpack(rtdf_entry_label,side="left")
	rtdf_entry <- tkentry(rtdf_entry_frame,
						width=50,
						textvariable=Rtdf_name)
	tkpack(rtdf_entry,side="left",fill="x",expand=1)
	rtdf_browse <- tkbutton(rtdf_entry_frame,
						text="Browse",
						command=function() rtdf_browser(Rtdf_name,Rtdf_dir))
	tkpack(rtdf_browse,side="right")
	tkpack(rtdf_entry_frame,side="top",anchor="w",fill="x")

	outfile_entry_frame <- tkframe(filterbybinning_win)
	outfile_entry_label <- tklabel(outfile_entry_frame,
						width=10,
						text="out_file")
	tkpack(outfile_entry_label,side="left")
	outfile_entry <- tkentry(outfile_entry_frame,
						width=50,
						textvariable=filtbin_outfile)
	tkpack(outfile_entry,side="left",fill="x",expand=1)
	outfile_browse <- tkbutton(outfile_entry_frame,
						text="Browse",
						command=function() rtdf_browser(filtbin_outfile,Rtdfs_dir,output=TRUE))
	tkpack(outfile_browse,side="right")
	tkpack(outfile_entry_frame,side="top",anchor="w",fill="x")

	action_frame <- tkframe(filterbybinning_win)
	action_label <- tklabel(action_frame, text="action")
	tkpack(action_label,side="left")
	action_rem <- tkradiobutton(action_frame,
						text="remove",
						value="remove",
						#command=function() parameter_entry(0),  REVISIT
						variable=filtbin_action)
	tkpack(action_rem,side="left")
	action_keep <- tkradiobutton(action_frame,
						text="keep  ",
						value="keep",
						#command=function() parameter_entry(0),  REVISIT
						variable=filtbin_action)
	tkpack(action_keep,side="left")
	action_rep <- tkradiobutton(action_frame,
						text="report",
						value="report",
						#command=function() parameter_entry(0),  REVISIT
						variable=filtbin_action)
	tkpack(action_rep,side="left")
	tkpack(action_frame,side="top",anchor="w")

	filt_frame <- tkframe(filterbybinning_win)
	filt_label <- tklabel(filt_frame, text="bin_type")
	tkpack(filt_label,side="left")
	filt_hbin <- tkradiobutton(filt_frame,
						text="hbin",
						value="hbin",
						#command=function() parameter_entry(0),  REVISIT
						variable=filtbin_type)
	tkpack(filt_hbin,side="left")
	filt_sbin <- tkradiobutton(filt_frame,
						text="sbin",
						value="sbin",
						#command=function() parameter_entry(0),  REVISIT
						variable=filtbin_type)
	tkpack(filt_sbin,side="left")
	tkpack(filt_frame,side="top",anchor="w")


	num_entry_frame <- tkframe(filterbybinning_win)
	num_entry_label <- tklabel(num_entry_frame,
						width=10,
						text="bins")
	tkpack(num_entry_label,side="left")
	num_entry <- tklabel(num_entry_frame,
						width=20,
						relief="sunken",
						textvariable=filtbin_bins)
	tkpack(num_entry,side="left",fill="x",expand=1)
	num_browse <- tkbutton(num_entry_frame,
						text="Edit",
						command=function() indices_entry(filtbin_bins))
	tkpack(num_browse,side="right")
	tkpack(num_entry_frame,side="top",anchor="w",fill="x")


}



