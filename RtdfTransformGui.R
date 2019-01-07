# RtdfTransformGui.R
#
# $Id: RtdfTransformGui.R,v 1.5 2010/11/24 01:46:18 David Exp $
#
# Tk/Tcl GUI wrapper for calling RtdfTransform.R
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
if(!exists(".TkRadar.env"))  .TkRadar.env <- new.env()

# RtdfTransform gui specific variables
#-----------------------------------------
trans_csvfile <- tclVar("transform.csv")
trans_csv_dir <- tclVar("")
trans_outfile <- tclVar("transformed.rtdf")


#-----------------------------------------
RtdfTransformGui_defaults <- function(...) {
	tclvalue(trans_csvfile) <- "transform.csv"
	tclvalue(trans_csv_dir) <- ""
	tclvalue(trans_outfile) <- "transformed.rtdf"
	
}


#-----------------------------------------
run_RtdfTransform <-function(done=FALSE,..) {
	in_rtdf <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
	my_csv <- paste(tclObj(trans_csvfile),sep="",collapse=" ")
	out_rtdf <- paste(tclObj(trans_outfile),sep="",collapse=" ")

	rtdf_dir_ <- paste(tclObj(Rtdf_dir),sep="",collapse=" ")
	csv_dir_ <- paste(tclObj(trans_csv_dir),sep="",collapse=" ")
	output_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")

	full_path = output_dir
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	setwd(full_path)

	my_expr = substitute(
		RtdfTransform(in_file=in_rtdf,transform_csv=my_csv,
				out_file=out_rtdf,csv_dir=csv_dir_,in_dir=rtdf_dir_)
	)
	tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
	tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
	my_command = sprintf("%s\n",deparse(my_expr))
	my_cmnd = "RtdfTransform(...)\n"
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
		rtdftransform_win <- get("rtdftransform_win",envir=.TkRadar.wins)
		tkdestroy(rtdftransform_win)
	}

}

#----------------------------------------------------
trans_csv_browser <-function() {

	my_dir <- paste(tclObj(trans_csv_dir),sep="",collapse=" ")
	if(as.numeric(tclObj(Bad_Vista))>0) {
		csv_str = "{{All files} *} {{CSV Files} {.csv .csv.gz}}"
	} else {
		csv_str = "{{CSV Files} {.csv .csv.gz}} {{All files} *}"
	}
	if (nchar(my_dir)>0) {
		name <- tclvalue(tkgetOpenFile(filetypes=csv_str,
				initialdir=my_dir))
	} else {
		name <- tclvalue(tkgetOpenFile(filetypes=csv_str))
	}
  
	# the tkgetOpenFile returns the full path!
	if (nchar(name)>0) {
		# separate filename and path
		my_filepath = sub("/[^/]*$","",name)	
		name=sub("^.*/","",name)

		# update name and path variables
		tclvalue(trans_csvfile) <- name
		tclvalue(trans_csv_dir) <- my_filepath
	}
}


#-----------------------------------------
RtdfTransformGui <- function(...) {

	RtdfTransformGui_defaults()		# initialize variables...
	rtdftransform_win <- tktoplevel()
	assign("rtdftransform_win",rtdftransform_win,envir=.TkRadar.wins)
	tkwm.title(rtdftransform_win, "RtdfTransform")
	
	bottom_row <- tkframe(rtdftransform_win)
	default_button <- tkbutton(bottom_row,
					text="DEFAULTS",
					#anchor="w",
					width=12,
					command=RtdfTransformGui_defaults)
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
					text="RUN & QUIT",
					#anchor="w",
					width=12,
					command=function() run_RtdfTransform(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
					text="QUIT",
					#anchor="w",
					width=12,
					command=function()tkdestroy(rtdftransform_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
					text="RUN",
					#anchor="w",
					width=12,
					command=function() run_RtdfTransform(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	rtdf_dir_entry_frame <- tkframe(rtdftransform_win)
	rtdf_dir_entry_label <- tklabel(rtdf_dir_entry_frame,
						width=15,
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

	rtdf_entry_frame <- tkframe(rtdftransform_win)
	rtdf_entry_label <- tklabel(rtdf_entry_frame,
						width=15,
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


	csv_dir_entry_frame <- tkframe(rtdftransform_win)
	csv_dir_entry_label <- tklabel(csv_dir_entry_frame,
						width=15,
						text="csv_dir")
	tkpack(csv_dir_entry_label,side="left")
	csv_dir_entry <- tklabel(csv_dir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=trans_csv_dir)
	tkpack(csv_dir_entry,side="left",fill="x",expand=1)
	csv_dir_browse <- tkbutton(csv_dir_entry_frame,
						text="Browse",
						command=function() dir_browser(trans_csv_dir))
	tkpack(csv_dir_browse,side="right")
	tkpack(csv_dir_entry_frame,side="top",anchor="w",fill="x")

	csv_entry_frame <- tkframe(rtdftransform_win)
	csv_entry_label <- tklabel(csv_entry_frame,
						width=15,
						text="transform_csv")
	tkpack(csv_entry_label,side="left")
	csv_entry <- tkentry(csv_entry_frame,
						width=50,
						textvariable=trans_csvfile)
	tkpack(csv_entry,side="left",fill="x",expand=1)
	csv_browse <- tkbutton(csv_entry_frame,
						text="Browse",
						command=trans_csv_browser)
	tkpack(csv_browse,side="right")
	tkpack(csv_entry_frame,side="top",anchor="w",fill="x")


	outfile_entry_frame <- tkframe(rtdftransform_win)
	outfile_entry_label <- tklabel(outfile_entry_frame,
						width=15,
						text="out_file")
	tkpack(outfile_entry_label,side="left")
	outfile_entry <- tkentry(outfile_entry_frame,
						width=50,
						textvariable=trans_outfile)
	tkpack(outfile_entry,side="left",fill="x",expand=1)
	outfile_browse <- tkbutton(outfile_entry_frame,
						text="Browse",
						command=function() rtdf_browser(trans_outfile,Rtdfs_dir,output=TRUE))
	tkpack(outfile_browse,side="right")
	tkpack(outfile_entry_frame,side="top",anchor="w",fill="x")


}



