# ReplaceTestnamesGui.R
#
# $Id: ReplaceTestnamesGui.R,v 1.3 2009/12/29 20:28:41 David Exp $
#
# Tk/Tcl GUI wrapper for calling ReplaceTestnames.R
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

# ReplaceTestnames gui specific variables
#-----------------------------------------
repl_ref_file <- tclVar("ref.rtdf")
repl_ref_dir <- tclVar("")
repl_outfile <- tclVar("")
only_matches <- tclVar(1)


#-----------------------------------------
ReplaceTestnames_defaults <- function(...) {
	tclvalue(repl_ref_file) <- "ref.rtdf"
	tclvalue(repl_ref_dir) <- ""
	tclvalue(repl_outfile) <- ""
	tclvalue(only_matches) <- 1
	
}


#-----------------------------------------
run_ReplaceTestnames <-function(done=FALSE,..) {
	in_rtdf <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
	ref_rtdf <- paste(tclObj(repl_ref_file),sep="",collapse=" ")
	out_rtdf <- paste(tclObj(repl_outfile),sep="",collapse=" ")
	only_matches_ <- as.logical(tclObj(only_matches))

	rtdf_dir_ <- paste(tclObj(Rtdf_dir),sep="",collapse=" ")
	ref_dir_ <- paste(tclObj(repl_ref_dir),sep="",collapse=" ")
	output_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")

	if (out_rtdf == "") {
		out_rtdf = as.character(strsplit(in_rtdf,"[.][rR]tdf$"))
		out_rtdf = paste(out_rtdf,"_repl.rtdf",sep="")
		tclvalue(repl_outfile) <- out_rtdf
	}

	full_path = output_dir
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	setwd(full_path)

	my_expr = substitute(
		ReplaceTestnames(in_file=in_rtdf,reference_file=ref_rtdf,
				out_file=out_rtdf,only_matches=only_matches_,
				ref_dir=ref_dir_,in_dir=rtdf_dir_)
	)
	tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
	tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
	my_command = sprintf("%s\n",deparse(my_expr))
	my_cmnd = "ReplaceTestnames(...)\n"
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
		replacetestnames_win <- get("replacetestnames_win",envir=.TkRadar.wins)
		tkdestroy(replacetestnames_win)
	}

}


#-----------------------------------------
ReplaceTestnamesGui <- function(...) {

	ReplaceTestnames_defaults()		# initialize variables...
	replacetestnames_win <- tktoplevel()
	assign("replacetestnames_win",replacetestnames_win,envir=.TkRadar.wins)
	tkwm.title(replacetestnames_win, "ReplaceTestnames")
	
	bottom_row <- tkframe(replacetestnames_win)
	default_button <- tkbutton(bottom_row,
					text="DEFAULTS",
					#anchor="w",
					width=12,
					command=ReplaceTestnames_defaults)
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
					text="RUN & QUIT",
					#anchor="w",
					width=12,
					command=function() run_ReplaceTestnames(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
					text="QUIT",
					#anchor="w",
					width=12,
					command=function()tkdestroy(replacetestnames_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
					text="RUN",
					#anchor="w",
					width=12,
					command=function() run_ReplaceTestnames(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	rtdf_dir_entry_frame <- tkframe(replacetestnames_win)
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

	rtdf_entry_frame <- tkframe(replacetestnames_win)
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

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	ref_dir_entry_frame <- tkframe(replacetestnames_win)
	ref_dir_entry_label <- tklabel(ref_dir_entry_frame,
						width=15,
						text="ref_dir")
	tkpack(ref_dir_entry_label,side="left")
	ref_dir_entry <- tklabel(ref_dir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=repl_ref_dir)
	tkpack(ref_dir_entry,side="left",fill="x",expand=1)
	ref_dir_browse <- tkbutton(ref_dir_entry_frame,
						text="Browse",
						command=function() dir_browser(repl_ref_dir))
	tkpack(ref_dir_browse,side="right")
	tkpack(ref_dir_entry_frame,side="top",anchor="w",fill="x")

	ref_entry_frame <- tkframe(replacetestnames_win)
	ref_entry_label <- tklabel(ref_entry_frame,
						width=15,
						text="reference_file")
	tkpack(ref_entry_label,side="left")
	ref_entry <- tkentry(ref_entry_frame,
						width=50,
						textvariable=repl_ref_file)
	tkpack(ref_entry,side="left",fill="x",expand=1)
	ref_browse <- tkbutton(ref_entry_frame,
						text="Browse",
						command=function() rtdf_browser(repl_ref_file,repl_ref_dir))
	tkpack(ref_browse,side="right")
	tkpack(ref_entry_frame,side="top",anchor="w",fill="x")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	only_match_button <- tkcheckbutton(replacetestnames_win,
						text="only_matches",
						variable=only_matches)
	tkpack(only_match_button,side="bottom",anchor="w")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	outfile_entry_frame <- tkframe(replacetestnames_win)
	outfile_entry_label <- tklabel(outfile_entry_frame,
						width=15,
						text="out_file")
	tkpack(outfile_entry_label,side="left")
	outfile_entry <- tkentry(outfile_entry_frame,
						width=50,
						textvariable=repl_outfile)
	tkpack(outfile_entry,side="left",fill="x",expand=1)
	outfile_browse <- tkbutton(outfile_entry_frame,
						text="Browse",
						command=function() rtdf_browser(repl_outfile,Rtdfs_dir,output=TRUE))
	tkpack(outfile_browse,side="right")
	tkpack(outfile_entry_frame,side="top",anchor="w",fill="x")


}



