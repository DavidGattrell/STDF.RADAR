# MergeNewTestsGui.R
#
# $Id: MergeNewTestsGui.R,v 1.1 2020/12/18 01:20:58 david Exp $
#
# Tk/Tcl GUI wrapper for calling MergeNewTests.R
# called by TkRadar.R
#
# Copyright (C) 2020 David Gattrell
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
# MergeNewTests gui specific variables
#--------------------------------
if(!exists(".TkRadar.env")) .TkRadar.env <- new.env()

mnts_infile1 <- tclVar("")
mnts_infile2 <- tclVar("")
mnts_outfile <- tclVar("mergedtests.rtdf")
mnts_use_xy_coords <- tclVar(0)

mnts_in1_dir <- tclVar("")
mnts_in2_dir <- tclVar("")

# these defaults can be controlled in the .Rprofile file:
default_mnts_use_xy_coords <- tclVar(0)


#-----------------------------------------
MergeNewTestsGui_defaults <- function(...) {

	tclvalue(mnts_use_xy_coords) <- tclObj(default_mnts_use_xy_coords)

	tclvalue(mnts_in1_dir) <- tclObj(Rtdfs_dir)
	tclvalue(mnts_infile1) <- ""

	tclvalue(mnts_in2_dir) <- tclObj(Rtdf_dir)
	in_name <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
	tclvalue(mnts_infile2) <- in_name

	tclvalue(mnts_outfile) <- "mergedtests.rtdf"

}


#-----------------------------------------------------
mnts_out_rtdf_browser <-function(...) {
	orig_name = paste(tclObj(mnts_outfile),sep="",collapse=" ")
	if (nchar(orig_name)<1)  orig_name="mergedtests.rtdf"

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
		tclvalue(mnts_outfile) <- name

		# if we changed directory... update paths
		out_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
		if(my_filepath != out_dir) {
			change_Rtdfs_dir(my_filepath)
		}
	}
}


#-----------------------------------------------------
run_MergeNewTests <-function(done=FALSE,...) {

	in_file1_ <- paste(tclObj(mnts_infile1),sep="",collapse=" ")
	in_file2_ <- paste(tclObj(mnts_infile2),sep="",collapse=" ")
	in_dir1_ <- paste(tclObj(mnts_in1_dir),sep="",collapse=" ")
	in_dir2_ <- paste(tclObj(mnts_in2_dir),sep="",collapse=" ")
	out_file_ <- paste(tclObj(mnts_outfile),sep="",collapse=" ")
	output_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
	use_xy_coords_ <- as.logical(tclObj(mnts_use_xy_coords))

	full_path = output_dir
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	setwd(full_path)

	my_expr = substitute(
		MergeNewTests(	in_file1=in_file1_,
					in_file2=in_file2_,
					out_file=out_file_,
					in_dir1=in_dir1_,
					in_dir2=in_dir2_,
					use_xy_coords=use_xy_coords_)
	)
	tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
	tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
	my_command = sprintf("%s\n",deparse(my_expr))
	my_cmnd = "MergeNewTests(...)\n"
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
		mergetests_win <- get("mergetests_win",envir=.TkRadar.wins)
		tkdestroy(mergetests_win)
	}
}


#-----------------------------------------
MergeNewTestsGui <- function(...) {

	MergeNewTestsGui_defaults()		# initialize variables...
	mergetests_win <- tktoplevel()
	assign("mergetests_win",mergetests_win,envir=.TkRadar.wins)
	tkwm.title(mergetests_win, "MergeNewTests")

	bottom_row <- tkframe(mergetests_win)
	default_button <- tkbutton(bottom_row,
						text="DEFAULTS",
						#anchor="w",
						width=12,
						command=MergeNewTestsGui_defaults)
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
						text="RUN & QUIT",
						#anchor="w",
						width=12,
						command=function() run_MergeNewTests(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
						text="QUIT",
						#anchor="w",
						width=12,
						command=function() tkdestroy(mergetests_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
						text="RUN",
						#anchor="w",
						width=12,
						command=function() run_MergeNewTests(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	dir_entry_frame <- tkframe(mergetests_win)
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

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	indir1_entry_frame <- tkframe(mergetests_win)
	indir1_entry_label <- tklabel(indir1_entry_frame,
						width=10,
						text="in1 rtdf dir")
	tkpack(indir1_entry_label,side="left")
	indir1_entry <- tklabel(indir1_entry_frame,
						width=50,
						relief="sunken",
						textvariable=mnts_in1_dir)
	tkpack(indir1_entry,side="left",fill="x",expand=1)
	indir1_browse <- tkbutton(indir1_entry_frame,
						text="Browse",
						command=function() dir_browser(mnts_in1_dir))
	tkpack(indir1_browse,side="right")
	tkpack(indir1_entry_frame,side="top",anchor="w",fill="x")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	indir2_entry_frame <- tkframe(mergetests_win)
	indir2_entry_label <- tklabel(indir2_entry_frame,
						width=10,
						text="in2 rtdf dir")
	tkpack(indir2_entry_label,side="left")
	indir2_entry <- tklabel(indir2_entry_frame,
						width=50,
						relief="sunken",
						textvariable=mnts_in2_dir)
	tkpack(indir2_entry,side="left",fill="x",expand=1)
	indir2_browse <- tkbutton(indir2_entry_frame,
						text="Browse",
						command=function() dir_browser(mnts_in2_dir))
	tkpack(indir2_browse,side="right")
	tkpack(indir2_entry_frame,side="top",anchor="w",fill="x")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mnts_xy_button <- tkcheckbutton(mergetests_win,
						text="use_xy_coords",
						variable=mnts_use_xy_coords)
	tkpack(mnts_xy_button,side="top",anchor="w")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	in1_entry_frame <- tkframe(mergetests_win)
	in1_entry_label <- tklabel(in1_entry_frame,
						width=10,
						text="in_file1")
	tkpack(in1_entry_label,side="left")
	in1_entry <- tkentry(in1_entry_frame,
						width=50,
						textvariable=mnts_infile1)
	tkpack(in1_entry,side="left",fill="x",expand=1)
	in1_browse <- tkbutton(in1_entry_frame,
						text="Browse",
						command=function() rtdf_browser(mnts_infile1,mnts_in1_dir))
	tkpack(in1_browse,side="right")
	tkpack(in1_entry_frame,side="top",anchor="w",fill="x")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	in2_entry_frame <- tkframe(mergetests_win)
	in2_entry_label <- tklabel(in2_entry_frame,
						width=10,
						text="in_file2")
	tkpack(in2_entry_label,side="left")
	in2_entry <- tkentry(in2_entry_frame,
						width=50,
						textvariable=mnts_infile2)
	tkpack(in2_entry,side="left",fill="x",expand=1)
	in2_browse <- tkbutton(in2_entry_frame,
						text="Browse",
						command=function() rtdf_browser(mnts_infile2,mnts_in2_dir))
	tkpack(in2_browse,side="right")
	tkpack(in2_entry_frame,side="top",anchor="w",fill="x")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mn_out_entry_frame <- tkframe(mergetests_win)
	mn_out_entry_label <- tklabel(mn_out_entry_frame,
						width=10,
						text="out_file")
	tkpack(mn_out_entry_label,side="left")
	mn_out_entry <- tkentry(mn_out_entry_frame,
						width=20,
						textvariable=mnts_outfile)
	tkpack(mn_out_entry,side="left",fill="x",expand=1)
	mn_out_browse <- tkbutton(mn_out_entry_frame,
						text="Browse",
						command=mnts_out_rtdf_browser)
	tkpack(mn_out_browse,side="right")
	tkpack(mn_out_entry_frame,side="top",anchor="w",fill="x")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

}
