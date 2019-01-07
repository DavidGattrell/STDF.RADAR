# FilterByResultGui.R
#
# $Id: FilterByResultGui.R,v 1.7 2013/09/01 03:32:35 david Exp $
#
# Tk/Tcl GUI wrapper for calling FilterByResult.R
# called by TkRadar.R
#
# Copyright (C) 2008-2013 David Gattrell
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
filtres_action <- tclVar("remove")
filtres_type <- tclVar("result")
filtres_testname <- tclVar("")
filtres_cond <- tclVar(">")
filtres_value <- tclVar(0.0)
filtres_scaler <- tclVar("")
filtres_outfile <- tclVar("filtered.rtdf")


#-----------------------------------------
FilterByResultGui_defaults <- function(...) {
	tclvalue(filtres_outfile) <- "filtered.rtdf"

	tclvalue(filtres_value) <- 0.0
}


#-----------------------------------------
run_FilterByResult <-function(done=FALSE,..) {
	in_rtdf <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
	action_ <- as.character(tclObj(filtres_action))
	filter_ <- as.character(tclObj(filtres_type))
	testname_ <- paste(tclObj(filtres_testname),sep="",collapse=" ")
	gt_lt_ <- as.character(tclObj(filtres_cond))
	value_ <- as.numeric(tclObj(filtres_value))
	if( length(value_)<1)  value_ <- 0
	scaler_ <- as.character(tclObj(filtres_scaler))
	if(length(scaler_)<1) scaler_ <- ""
	out_rtdf <- paste(tclObj(filtres_outfile),sep="",collapse=" ")

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
		FilterByResult(in_file=in_rtdf,action=action_,filter=filter_,
				testname=testname_,condition=gt_lt_,value=value_,
				scaler=scaler_,out_file=out_rtdf,in_dir=rtdf_dir_)
	)
	tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
	tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
	my_command = sprintf("%s\n",deparse(my_expr))
	my_cmnd = "FilterByResult(...)\n"
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
		tclvalue(Rtdf_name) <- out_rtdf
		tclvalue(Rtdf_dir) <- output_dir
	}
	if(done>0) {
		filterbyresult_win <- get("filterbyresult_win",envir=.TkRadar.wins)
		tkdestroy(filterbyresult_win)
	}

}

#-----------------------------------------
FilterByResultGui <- function(...) {

	FilterByResultGui_defaults()		# initialize variables...
	filterbyresult_win <- tktoplevel()
	assign("filterbyresult_win",filterbyresult_win,envir=.TkRadar.wins)
	tkwm.title(filterbyresult_win, "FilterByResult")
	
	num_entry_frame <- tkframe(filterbyresult_win)
	num_entry <- tkentry(num_entry_frame,
						width=30,
						background="white",
						textvariable=filtres_value)

	bottom_row <- tkframe(filterbyresult_win)
	default_button <- tkbutton(bottom_row,
					text="DEFAULTS",
					#anchor="w",
					width=12,
					command=function() {
						FilterByResultGui_defaults()
						tkconfigure(num_entry,background="white")
					})
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
					text="RUN & QUIT",
					#anchor="w",
					width=12,
					command=function() run_FilterByResult(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
					text="QUIT",
					#anchor="w",
					width=12,
					command=function()tkdestroy(filterbyresult_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
					text="RUN",
					#anchor="w",
					width=12,
					command=function() run_FilterByResult(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	rtdf_dir_entry_frame <- tkframe(filterbyresult_win)
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

	rtdf_entry_frame <- tkframe(filterbyresult_win)
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

	outfile_entry_frame <- tkframe(filterbyresult_win)
	outfile_entry_label <- tklabel(outfile_entry_frame,
						width=10,
						text="out_file")
	tkpack(outfile_entry_label,side="left")
	outfile_entry <- tkentry(outfile_entry_frame,
						width=50,
						textvariable=filtres_outfile)
	tkpack(outfile_entry,side="left",fill="x",expand=1)
	outfile_browse <- tkbutton(outfile_entry_frame,
						text="Browse",
						command=function() rtdf_browser(filtres_outfile,Rtdfs_dir,output=TRUE))
	tkpack(outfile_browse,side="right")
	tkpack(outfile_entry_frame,side="top",anchor="w",fill="x")

	param_entry_frame <- tkframe(filterbyresult_win)
	param_entry_label <- tklabel(param_entry_frame,
						width=10,
						text="testname")
	tkpack(param_entry_label,side="left")
	param_entry <- tkentry(param_entry_frame,
						width=20,
						textvariable=filtres_testname)
	tkpack(param_entry,side="left",fill="x",expand=1)
	param_browse <- tkbutton(param_entry_frame,
						text="Browse",
						command=function() param_browser(filtres_testname,
											Rtdf_name,Rtdf_dir)
						)
	tkpack(param_browse,side="right")
	tkpack(param_entry_frame,side="top",anchor="w",fill="x")
	
	action_frame <- tkframe(filterbyresult_win)
	action_label <- tklabel(action_frame, text="action")
	tkpack(action_label,side="left")
	action_rem <- tkradiobutton(action_frame,
						text="remove",
						value="remove",
						#command=function() parameter_entry(0),  REVISIT
						variable=filtres_action)
	tkpack(action_rem,side="left")
	action_keep <- tkradiobutton(action_frame,
						text="keep  ",
						value="keep",
						#command=function() parameter_entry(0),  REVISIT
						variable=filtres_action)
	tkpack(action_keep,side="left")
	action_rep <- tkradiobutton(action_frame,
						text="report",
						value="report",
						#command=function() parameter_entry(0),  REVISIT
						variable=filtres_action)
	tkpack(action_rep,side="left")
	tkpack(action_frame,side="top",anchor="w")

	filt_frame <- tkframe(filterbyresult_win)
	filt_label <- tklabel(filt_frame, text="filter")
	tkpack(filt_label,side="left")
	filt_res <- tkradiobutton(filt_frame,
						text="result",
						value="result",
						#command=function() parameter_entry(0),  REVISIT
						variable=filtres_type)
	tkpack(filt_res,side="left")
	filt_dev <- tkradiobutton(filt_frame,
						text="device",
						value="device",
						#command=function() parameter_entry(0),  REVISIT
						variable=filtres_type)
	tkpack(filt_dev,side="left")
	tkpack(filt_frame,side="top",anchor="w")

	type_frame <- tkframe(filterbyresult_win)
	type_label <- tklabel(type_frame, text="type")
	tkpack(type_label,side="left")
	type_lt <- tkradiobutton(type_frame,
						text="<     ",
						value="<",
						#command=function() parameter_entry(0),  REVISIT
						variable=filtres_cond)
	tkpack(type_lt,side="left")
	type_lteq <- tkradiobutton(type_frame,
						text="<=    ",
						value="<=",
						#command=function() parameter_entry(0),  REVISIT
						variable=filtres_cond)
	tkpack(type_lteq,side="left")
	type_gteq <- tkradiobutton(type_frame,
						text=">=    ",
						value=">=",
						#command=function() parameter_entry(1),  REVISIT
						variable=filtres_cond)
	tkpack(type_gteq,side="left")
	type_gt <- tkradiobutton(type_frame,
						text=">     ",
						value=">",
						#command=function() parameter_entry(1),  REVISIT
						variable=filtres_cond)
	tkpack(type_gt,side="left")
	tkpack(type_frame,side="top",anchor="w")

	#num_entry_frame <- tkframe(filterbyresult_win) -- moved to prior to DEFAULTS button
	num_entry_label <- tklabel(num_entry_frame,
						width=10,
						text="value")
	tkpack(num_entry_label,side="left")
	#num_entry <- tkentry(num_entry_frame,  -- moved to prior to DEFAULTS button
	#					width=30,
	#					background="white",
	#					textvariable=filtres_value)
	tkpack(num_entry,side="left",anchor="n")
	tkbind(num_entry,"<KeyRelease>",function() {
					tmp <- as.numeric(tclObj(filtres_value))
					if( (length(tmp)>0) && is.finite(tmp)) {
						tkconfigure(num_entry,background="white")
					} else {
						tkconfigure(num_entry,background="yellow")
					}
					tcl('update')
				})
	tkpack(num_entry_frame,side="top",anchor="w",fill="x")

	# tk menu of the various scale letters...
	
	scaler_frame <- tkframe(filterbyresult_win)
	scaler_label <- tklabel(scaler_frame,
						width=10,
						text="scaler")
	tkpack(scaler_label,side="left")
	scaler_entry <- tklabel(scaler_frame,
						width=3,
						relief="sunken",
						textvariable=filtres_scaler)
	#tkpack(scaler_entry,side="left",fill="x",expand=1)
	tkpack(scaler_entry,side="left")

	scale_menu_button <- tkmenubutton(scaler_frame,
						text="Menu",
						relief="raised")
	tkpack(scale_menu_button,side="left")
	scale_menu <- tkmenu(scale_menu_button,tearoff=FALSE)
	tkconfigure(scale_menu_button,	menu=scale_menu)
	tkadd(scale_menu,"command",label="T",
			command=function() tclvalue(filtres_scaler)<-"T")
	tkadd(scale_menu,"command",label="G",
			command=function() tclvalue(filtres_scaler)<-"G")
	tkadd(scale_menu,"command",label="M",
			command=function() tclvalue(filtres_scaler)<-"M")
	tkadd(scale_menu,"command",label="K",
			command=function() tclvalue(filtres_scaler)<-"K")
	tkadd(scale_menu,"command",label=" ",
			command=function() tclvalue(filtres_scaler)<-" ")
	tkadd(scale_menu,"command",label="m",
			command=function() tclvalue(filtres_scaler)<-"m")
	tkadd(scale_menu,"command",label="u",
			command=function() tclvalue(filtres_scaler)<-"u")
	tkadd(scale_menu,"command",label="n",
			command=function() tclvalue(filtres_scaler)<-"n")
	tkadd(scale_menu,"command",label="p",
			command=function() tclvalue(filtres_scaler)<-"p")
	tkadd(scale_menu,"command",label="f",
			command=function() tclvalue(filtres_scaler)<-"f")

	tkpack(scaler_frame,side="top",anchor="w",fill="x")

}



