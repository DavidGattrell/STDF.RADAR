# FilterByIndicesGui.R
#
# $Id: FilterByIndicesGui.R,v 1.2 2015/04/18 01:51:32 david Exp $
#
# Tk/Tcl GUI wrapper for calling FilterByIndices.R
# called by TkRadar.R
#
# Copyright (C) 2013 David Gattrell
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
# FilterByIndicesGui.R specific variables
#-----------------------------------------
if(!exists(".TkRadar.env")) .TkRadar.env <- new.env()

fbi_out_file <- tclVar("screened.rtdf")
fbi_action <- tclVar("remove")
fbi_noaction <- tclVar(0)	# 1 = report only, no action taken
fbi_indices <- tclVar(0)
fbi_logic <- tclVar(0)		# 1 = indices is boolean, 0 is integer
fbi_in_name <- tclVar("")
fbi_in_dir <- tclVar("")


#----------------------------------------------------
FilterByIndicesGui_defaults <- function() {
	tclvalue(fbi_action) <- "remove"
	tclvalue(fbi_noaction) <- 0
	tclvalue(fbi_indices) <- 0
	tclvalue(fbi_logic) <- 0	# if 1, then indices is logical, not numeric
	
	tclvalue(fbi_out_file) <- "screened.rtdf"
	in_dir <- paste(tclObj(Rtdf_dir),sep="",collapse=" ")
	in_name <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
	if(nchar(in_name)>0) {
		tclvalue(fbi_in_dir) <- tclObj(Rtdf_dir)
		tclvalue(fbi_in_name) <- tclObj(Rtdf_name)
	} else {
		if(nchar(in_dir)>0) {
			tclvalue(fbi_in_dir) <- tclObj(Rtdf_dir)
			tclvalue(fbi_in_name) <- tclObj(Rtdf_name)
		} else {
			tclvalue(fbi_in_dir) <- tclObj(Rtdfs_dir)
			tclvalue(fbi_in_name) <- tclObj(Rtdf_name)
		}
	}
}


#----------------------------------------------------
fbi_out_rtdf_browser <-function(...) {

	orig_name = paste(tclObj(fbi_out_file),sep="",collapse=" ")

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
		tclvalue(fbi_out_file) <- name

		# if we changed directory... update paths
		out_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
		if(my_filepath != out_dir) {
			change_Rtdfs_dir(my_filepath)
		}
	}
}


#----------------------------------------------------
run_FilterByIndices <-function(done=FALSE,...) {

	in_file_ <- paste(tclObj(fbi_in_name),sep="",collapse=" ")
	action_ <- as.character(tclObj(fbi_action))
	no_action <- as.logical(tclObj(fbi_noaction))
	if(no_action)  action_ = "report"
	logic <- as.logical(tclObj(fbi_logic))
	#cmd <- as.character(tclObj(fbi_indices))
	cmd <- paste(tclObj(fbi_indices),sep="",collapse=" ")
	tmp <- try(eval(parse(text=cmd)),silent=TRUE)
	if(class(tmp) == "try-error") {
		# check if legacy RemoveDevicesAtIndices.R syntax... 
		my_entry = gsub('[[:blank:]]+',",",cmd)
		my_entry = gsub(",{2,}",",",my_entry)
		my_entry = paste("c(",my_entry,")",sep="")
		tmp = try(eval(parse(text=my_entry)),silent=TRUE)
	}
	if(class(tmp) != "try-error") {
		if(logic) {
			indices_ <- as.logical(tmp)
		} else {
			indices_ <- as.numeric(tmp)
		}
	} else {
		indices_ = NaN
	}
	out_file_ <- paste(tclObj(fbi_out_file),sep="",collapse=" ")
	in_dir_ <- paste(tclObj(fbi_in_dir),sep="",collapse=" ")
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
		FilterByIndices(in_file=in_file_,indices=indices_,
					action=action_,out_file=out_file_,in_dir=in_dir_)
	)
	tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
	tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
	my_command = sprintf("%s\n",deparse(my_expr))
	my_cmnd = "FilterByIndices(...)\n"
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

	if ((action_=="keep")||(action_=="remove")) {
		# now update Rtdf_name and Rtdf_dir...
		tclvalue(Rtdf_name) <- out_file_
		tclvalue(Rtdf_dir) <- output_dir
	}
	if(done>0) {
		filtbyindex_win <- get("filtbyindex_win",envir=.TkRadar.wins)
		tkdestroy(filtbyindex_win)
	}
}


#----------------------------------------------------
FilterByIndicesGui <- function() {

	FilterByIndicesGui_defaults()		# initialize variables...
	filtbyindex_win <- tktoplevel()
	assign("filtbyindex_win",filtbyindex_win,envir=.TkRadar.wins)
	tkwm.title(filtbyindex_win, "FilterByIndices")
		
	# these get reset by Default button, so need to be defined first...
	num_entry_frame <- tkframe(filtbyindex_win)
	num_entry <- tkentry(num_entry_frame,
						width=20,
						background="white",
						textvariable=fbi_indices)


	bottom_row <- tkframe(filtbyindex_win)
	default_button <- tkbutton(bottom_row,
						text="DEFAULTS",
						#anchor="w",
						width=12,
						command=function() {
							FilterByIndicesGui_defaults()
							tkconfigure(num_entry,background="white")
						})
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
						text="RUN & QUIT",
						#anchor="w",
						width=12,
						command=function() run_FilterByIndices(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
						text="QUIT",
						#anchor="w",
						width=12,
						command=function()tkdestroy(filtbyindex_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
						text="RUN",
						#anchor="w",
						width=12,
						command=function() run_FilterByIndices(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	dir_entry_frame <- tkframe(filtbyindex_win)
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

	in_dir_entry_frame <- tkframe(filtbyindex_win)
	in_dir_entry_label <- tklabel(in_dir_entry_frame,
						width=10,
						text="in_dir")
	tkpack(in_dir_entry_label,side="left")
	in_dir_entry <- tklabel(in_dir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=fbi_in_dir)
	tkpack(in_dir_entry,side="left",fill="x",expand=1)
	in_dir_browse <- tkbutton(in_dir_entry_frame,
						text="Browse",
						command=function() dir_browser(fbi_in_dir))
	tkpack(in_dir_browse,side="right")
	tkpack(in_dir_entry_frame,side="top",anchor="w",fill="x")

	in_entry_frame <- tkframe(filtbyindex_win)
	in_entry_label <- tklabel(in_entry_frame,
						width=10,
						text="in_file")
	tkpack(in_entry_label,side="left")
	in_entry <- tkentry(in_entry_frame,
						width=50,
						textvariable=fbi_in_name)
	tkpack(in_entry,side="left",fill="x",expand=1)
	in_browse <- tkbutton(in_entry_frame,
						text="Browse",
						command=function() rtdf_browser(fbi_in_name,fbi_in_dir))
	tkpack(in_browse,side="right")
	tkpack(in_entry_frame,side="top",anchor="w",fill="x")

	# - - - - - - - - - - - - - - - -

	action_frame <- tkframe(filtbyindex_win)
	action_label <- tklabel(action_frame, text="action")
	tkpack(action_label,side="left")
	action_rem <- tkradiobutton(action_frame,
						text="remove",
						value="remove",
						variable=fbi_action)
	tkpack(action_rem,side="left")
	action_keep <- tkradiobutton(action_frame,
						text="keep  ",
						value="keep",
						variable=fbi_action)
	tkpack(action_keep,side="left")
	no_action <- tkcheckbutton(action_frame,
						text="report only (no action)",
						variable=fbi_noaction)
	tkpack(no_action,side="left")
	tkpack(action_frame,side="top",anchor="w")

	num_entry_label <- tklabel(num_entry_frame,
						width=10,
						text="indices")
	tkpack(num_entry_label,side="left")
	tkpack(num_entry,side="left",fill="x",expand=1)
	tkbind(num_entry,"<KeyRelease>",function() {
					#cmd <- as.character(tclObj(fbi_indices))  # erroneously takes "1 2 3" and sets cmd to "3"
					cmd <- paste(tclObj(fbi_indices),sep="",collapse=" ")
					#cat(sprintf("entry...>>%s<< \n",cmd))
					tmp <- try(eval(parse(text=cmd)),silent=TRUE)
					if(class(tmp) == "try-error") {
						# check if legacy RemoveDevicesAtIndices.R syntax... 
						# if so, we'll handle in run_FilterByIndices
						my_entry = gsub('[[:blank:]]+',",",cmd)
						my_entry = gsub(",{2,}",",",my_entry)
						my_entry = paste("c(",my_entry,")",sep="")
						tmp = try(eval(parse(text=my_entry)),silent=TRUE)
						if(class(tmp) != "try-error") {
							tkconfigure(num_entry,background="white")
							tclvalue(fbi_logic) <- 0
						} else {
							tkconfigure(num_entry,background="yellow")
						}
					} else {
						if( is.logical(tmp) ) {
							tkconfigure(num_entry,background="white")
							tclvalue(fbi_logic) <- 1
						} else if( is.numeric(tmp) ) {
								tkconfigure(num_entry,background="white")
								tclvalue(fbi_logic) <- 0
						} else {
							tkconfigure(num_entry,background="yellow")
						}
					}
				})
	tkpack(num_entry_frame,side="top",anchor="w",fill="x")

	# - - - - - - - - - - - - - - - -

	out_entry_frame <- tkframe(filtbyindex_win)
	out_entry_label <- tklabel(out_entry_frame,
						width=10,
						text="out_file")
	tkpack(out_entry_label,side="left")
	out_entry <- tkentry(out_entry_frame,
						width=20,
						textvariable=fbi_out_file)
	tkpack(out_entry,side="left",fill="x",expand=1)
	out_browse <- tkbutton(out_entry_frame,
						text="Browse",
						command=fbi_out_rtdf_browser)
	tkpack(out_browse,side="right")
	tkpack(out_entry_frame,side="top",anchor="w",fill="x")

}


