# StackedWaferMapGui.R
#
# $Id$
#
# Tk/Tcl GUI wrapper for calling WaferMap.R
# called by TkRadar.R
#
# Copyright (C) 2018 David Gattrell
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
# StackedWaferMap gui specific variables
#--------------------------------
if(!exists(".TkRadar.env")) .TkRadar.env <- new.env()

swmap_type <- tclVar("sbin")
swmap_xleft <- tclVar(0)
swmap_ydown <- tclVar(0)
sx_coord_alpha <- tclVar(0)
swmap_panel <- tclVar(0)
swmap_notch <- tclVar("x")
swmap_autoopen <- tclVar(0)

swmap_rotate_ccw <- tclVar(0)
swmap_x_rev_polarity <- tclVar(0)
swmap_y_rev_polarity <- tclVar(0)
swmap_x_shift <- tclVar(0)
swmap_x_shift_shadow <- tclVar(0)
swmap_y_shift <- tclVar(0)
swmap_y_shift_shadow <- tclVar(0)

swmap_param_col_start <- tclVar(0.03)
swmap_param_col_start_shadow <- tclVar(0.03)
swmap_param_col_end <- tclVar(0.24)
swmap_param_col_end_shadow <- tclVar(0.24)
swmap_param_col_rev <- tclVar(0)		# false

swmap_borders_off <- tclVar(7000)
swmap_borders_off_shadow <- tclVar(7000)

swmap_pdf_name <- tclVar("stacked_wafer_map.pdf")
swmap_xform_pdf_name <- tclVar("xform_stacked_wafer_map.pdf")

REVISIT: as far as here!!!!!

# these defaults can be controlled in the .Rprofile file:
default_wmap_xleft <- tclVar(0)		# per user customizing
default_wmap_ydown <- tclVar(0)		# per user customizing
default_x_coord_alpha <- tclVar(0)	# per user customizing
default_panel <- tclVar(0)			# per user customizing
default_wmap_notch <- tclVar("x")	# per user customizing
default_wmap_autoopen <- tclVar(1)	# per user customizing

default_wmap_param_col_start <- tclVar(0.03)
default_wmap_param_col_end <- tclVar(0.24)
default_wmap_param_col_rev <- tclVar(0)		# false

default_wmap_bin_vs_col_name <- tclVar("")
default_wmap_bin_vs_col_path <- tclVar("")
default_wmap_gen_bins_csv <- tclVar(0)

default_wmap_borders_off <- tclVar(7000)

default_wmap_rotate_ccw <- tclVar(0)		# per user customizing
default_wmap_x_rev_polarity <- tclVar(0)	# per user customizing
default_wmap_y_rev_polarity <- tclVar(0)	# per user customizing
default_wmap_x_shift <- tclVar(0)			# per user customizing
default_wmap_y_shift <- tclVar(0)			# per user customizing

#----------------------------------------------------
WaferMapGui_defaults <- function(...) {
	#tclvalue(rtdf_name) <- ""
	tclvalue(wmap_pdf_name) <- "wafer_map.pdf"
	tclvalue(wmap_xform_pdf_name) <- "xform_wafer_map.pdf"
	tclvalue(wmap_type) <- "sbin"
	tclvalue(x_coord_alpha) <- tclObj(default_x_coord_alpha)
	tclvalue(panel) <- tclObj(default_panel)
	tclvalue(wmap_xleft) <- tclObj(default_wmap_xleft)
	tclvalue(wmap_ydown) <- tclObj(default_wmap_ydown)
	tclvalue(wmap_notch) <- tclObj(default_wmap_notch)
	tclvalue(wmap_autoopen) <- tclObj(default_wmap_autoopen)
	tclvalue(wmap_parameter) <- ""

	tclvalue(wmap_param_col_start) <- tclObj(default_wmap_param_col_start)
	tclvalue(wmap_param_col_start_shadow) <- tclObj(default_wmap_param_col_start)
	tclvalue(wmap_param_col_end) <- tclObj(default_wmap_param_col_end)
	tclvalue(wmap_param_col_end_shadow) <- tclObj(default_wmap_param_col_end)
	tclvalue(wmap_param_col_rev) <- tclObj(default_wmap_param_col_rev)

	tclvalue(wmap_bin_vs_col_name) <- tclObj(default_wmap_bin_vs_col_name)
	tclvalue(wmap_bin_vs_col_path) <- tclObj(default_wmap_bin_vs_col_path)
	tclvalue(wmap_gen_bins_csv) <- tclObj(default_wmap_gen_bins_csv)

	tclvalue(wmap_borders_off) <- tclObj(default_wmap_borders_off)
	tclvalue(wmap_borders_off_shadow) <- tclObj(default_wmap_borders_off)

	tclvalue(wmap_rotate_ccw) <- tclObj(default_wmap_rotate_ccw)
	tclvalue(wmap_x_rev_polarity) <- tclObj(default_wmap_x_rev_polarity)
	tclvalue(wmap_y_rev_polarity) <- tclObj(default_wmap_y_rev_polarity)
	tclvalue(wmap_x_shift) <- tclObj(default_wmap_x_shift)
	tclvalue(wmap_x_shift_shadow) <- tclObj(default_wmap_x_shift)
	tclvalue(wmap_y_shift) <- tclObj(default_wmap_y_shift)
	tclvalue(wmap_y_shift_shadow) <- tclObj(default_wmap_y_shift)
}


#----------------------------------------------------
run_WaferMap <-function(done=FALSE,...) {
	wmap_rtdf <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
	wmap_pdf_ <- paste(tclObj(wmap_pdf_name),sep="",collapse=" ")
	wmap_type_ <- as.character(tclObj(wmap_type))
	x_coord_alpha_ <- as.logical(tclObj(x_coord_alpha))
	panel_ <- as.logical(tclObj(panel))
	x_left_ <- as.logical(tclObj(wmap_xleft))
	y_down_ <- as.logical(tclObj(wmap_ydown))
	wmap_param <- paste(tclObj(wmap_parameter),sep="",collapse=" ")
	wmap_notch_ <- as.character(tclObj(wmap_notch))

	param_col_start_ <- as.numeric(tclObj(wmap_param_col_start))
	param_col_end_ <- as.numeric(tclObj(wmap_param_col_end))
	param_col_rev_flag_ <- as.logical(tclObj(wmap_param_col_rev))

	bin_vs_colors_ <- paste(tclObj(wmap_bin_vs_col_name),sep="",collapse=" ")
	bin_vs_colors_path_ <- paste(tclObj(wmap_bin_vs_col_path),sep="",collapse=" ")
	generate_bins_file_ <- as.logical(tclObj(wmap_gen_bins_csv))

	borders_off_ <- as.numeric(tclObj(wmap_borders_off))

	wmap_autoopen_ <- as.logical(tclObj(wmap_autoopen))
	if(wmap_notch_=="x")  wmap_notch_ = ""

	rtdf_dir_ <- paste(tclObj(Rtdf_dir),sep="",collapse=" ")
	output_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")

	full_path = output_dir
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	setwd(full_path)

	if(wmap_type_ == "parameter") {
		my_expr = substitute(
			WaferMap(rtdf_name=wmap_rtdf,pdf_name=wmap_pdf_,
					type=wmap_type_,x_coord_alpha=x_coord_alpha_,
					panel=panel_,x_left=x_left_,y_down=y_down_,
					parameter=wmap_param,rtdf_dir=rtdf_dir_,
					notch=wmap_notch_,
					param_col_start=param_col_start_,param_col_end=param_col_end_,
					param_col_rev_flag=param_col_rev_flag_,
					borders_off=borders_off_)
		)
	} else {
		my_expr = substitute(
			WaferMap(rtdf_name=wmap_rtdf,pdf_name=wmap_pdf_,
					type=wmap_type_,x_coord_alpha=x_coord_alpha_,
					panel=panel_,x_left=x_left_,y_down=y_down_,
					rtdf_dir=rtdf_dir_,notch=wmap_notch_,
					bin_vs_colors=bin_vs_colors_,bin_vs_colors_path=bin_vs_colors_path_,
					generate_bins_file=generate_bins_file_,
					borders_off=borders_off_)
		)
	}
	tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
	tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
	my_command = sprintf("%s\n",deparse(my_expr))
	my_cmnd = "WaferMap(...)\n"
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
	if(wmap_autoopen_) {
		if (as.character(Sys.info()["sysname"])=="Windows") {
			os_com = "cmd /c start"
		} else if (as.character(Sys.info()["sysname"])=="Darwin") {  # aka Mac
			os_com = "open"
		} else {  # linux...
			os_com = "xdg-open"
		}
		command_str = paste(os_com,wmap_pdf_)
		system(command_str)
	}
	cat("Finished!\n")

	if(done>0) {
		wafermap_win <- get("wafermap_win",envir=.TkRadar.wins)
		tkdestroy(wafermap_win)
	}

}

#----------------------------------------------------
run_XformWaferMap <-function(done=FALSE,...) {
	wmap_rtdf <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
	wmap_pdf_ <- paste(tclObj(wmap_xform_pdf_name),sep="",collapse=" ")
	wmap_type_ <- as.character(tclObj(wmap_type))
	x_coord_alpha_ <- as.logical(tclObj(x_coord_alpha))
	panel_ <- as.logical(tclObj(panel))
	x_left_ <- as.logical(tclObj(wmap_xleft))
	y_down_ <- as.logical(tclObj(wmap_ydown))
	wmap_param <- paste(tclObj(wmap_parameter),sep="",collapse=" ")
	wmap_notch_ <- as.character(tclObj(wmap_notch))

	param_col_start_ <- as.numeric(tclObj(wmap_param_col_start))
	param_col_end_ <- as.numeric(tclObj(wmap_param_col_end))
	param_col_rev_flag_ <- as.logical(tclObj(wmap_param_col_rev))

	bin_vs_colors_ <- paste(tclObj(wmap_bin_vs_col_name),sep="",collapse=" ")
	bin_vs_colors_path_ <- paste(tclObj(wmap_bin_vs_col_path),sep="",collapse=" ")
	generate_bins_file_ <- as.logical(tclObj(wmap_gen_bins_csv))

	borders_off_ <- as.numeric(tclObj(wmap_borders_off))

	wmap_autoopen_ <- as.logical(tclObj(wmap_autoopen))
	if(wmap_notch_=="x")  wmap_notch_ = ""

	rotate_ccw_ <- as.integer(tclObj(wmap_rotate_ccw))
	x_rev_polarity_ <- as.logical(tclObj(wmap_x_rev_polarity))
	y_rev_polarity_ <- as.logical(tclObj(wmap_y_rev_polarity))
	x_shift_ <- as.integer(tclObj(wmap_x_shift))
	y_shift_ <- as.integer(tclObj(wmap_y_shift))

	rtdf_dir_ <- paste(tclObj(Rtdf_dir),sep="",collapse=" ")
	output_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")

	full_path = output_dir
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	setwd(full_path)

	if(wmap_type_ == "parameter") {
		my_expr = substitute(
			WaferMap(rtdf_name=wmap_rtdf,pdf_name=wmap_pdf_,
					type=wmap_type_,x_coord_alpha=x_coord_alpha_,
					panel=panel_,x_left=x_left_,y_down=y_down_,
					parameter=wmap_param,rtdf_dir=rtdf_dir_,
					notch=wmap_notch_,
					rotate_ccw=rotate_ccw_,
					x_reverse_polarity=x_rev_polarity_,
					y_reverse_polarity=y_rev_polarity_,
					x_shift=x_shift_,y_shift=y_shift_,
					param_col_start=param_col_start_,param_col_end=param_col_end_,
					param_col_rev_flag=param_col_rev_flag_,
					borders_off=borders_off_)
		)
	} else {
		my_expr = substitute(
			WaferMap(rtdf_name=wmap_rtdf,pdf_name=wmap_pdf_,
					type=wmap_type_,x_coord_alpha=x_coord_alpha_,
					panel=panel_,x_left=x_left_,y_down=y_down_,
					rtdf_dir=rtdf_dir_,notch=wmap_notch_,
					rotate_ccw=rotate_ccw_,
					x_reverse_polarity=x_rev_polarity_,
					y_reverse_polarity=y_rev_polarity_,
					x_shift=x_shift_,y_shift=y_shift_,
					param_col_start=param_col_start_,param_col_end=param_col_end_,
					param_col_rev_flag=param_col_rev_flag_,
					bin_vs_colors=bin_vs_colors_,bin_vs_colors_path=bin_vs_colors_path_,
					generate_bins_file=generate_bins_file_,
					borders_off=borders_off_)
		)
	}
	tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
	tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
	my_command = sprintf("%s\n",deparse(my_expr))
	my_cmnd = "WaferMap(...)\n"
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
	if(wmap_autoopen_) {
		if (as.character(Sys.info()["sysname"])=="Windows") {
			os_com = "cmd /c start"
		} else if (as.character(Sys.info()["sysname"])=="Darwin") {  # aka Mac
			os_com = "open"
		} else {  # linux...
			os_com = "xdg-open"
		}
		command_str = paste(os_com,wmap_pdf_)
		system(command_str)
	}
	cat("Finished!\n")

	if(done>0) {
		xformwafermap_win <- get("xformwafermap_win",envir=.TkRadar.wins)
		tkdestroy(xformwafermap_win)
	}

}


#----------------------------------------------------
csv_name_path_browser <-function(csv_name,csv_dir) {

	my_dir <- paste(tclObj(csv_dir),sep="",collapse=" ")
	if (nchar(my_dir)<1) {
		my_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if(as.numeric(tclObj(Bad_Vista))>0) {
		my_str = "{{All files} *} {{CSV Files} {.csv}}"
	} else {
		my_str = "{{CSV Files} {.csv}} {{All files} *}"
	}
	if (nchar(my_dir)>0) {
		name <- tclvalue(tkgetOpenFile(filetypes=my_str,
				initialdir=my_dir,multiple=FALSE))
	} else {
		name <- tclvalue(tkgetOpenFile(filetypes=my_str,
				multiple=FALSE))
	}

	# the tkgetOpenFile returns the full path!
	if (nchar(name)>0) {
		# separate filename and path
		my_filepath = sub("/[^/]*$","",name)	
		name=sub("^.*/","",name)

		# update name and path variables
		tclvalue(csv_name) <- name
		out_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
		if(my_filepath != out_dir) {
			tclvalue(csv_dir) <- my_filepath
		} else {
			tclvalue(csv_dir) <- ""
		}
	}
}

#-----------------------------------------------------
WaferMapGui <-function(...) {

	WaferMapGui_defaults()		# initialize variables...
	wafermap_win <- tktoplevel()
	assign("wafermap_win",wafermap_win,envir=.TkRadar.wins)
	tkwm.title(wafermap_win, "WaferMap")


	# need to define white/yellow background objects first...
	param_col_frame <- tkframe(wafermap_win)
	col_start_entry <- tkentry(param_col_frame,
						width=7,
						background="white",
						textvariable=wmap_param_col_start_shadow)
	col_end_entry <- tkentry(param_col_frame,
						width=7,
						background="white",
						textvariable=wmap_param_col_end_shadow)
	border_off_frame <- tkframe(wafermap_win)
	border_off_entry <- tkentry(border_off_frame,
						width=7,
						background="white",
						textvariable=wmap_borders_off_shadow)

	bottom_row <- tkframe(wafermap_win)
	default_button <- tkbutton(bottom_row,
					text="DEFAULTS",
					#anchor="w",
					width=12,
					command=function() {
						WaferMapGui_defaults()
						tkconfigure(col_start_entry,background="white")
						tkconfigure(col_end_entry,background="white")
						tkconfigure(border_off_entry,background="white")
					}
					)
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
					text="RUN & QUIT",
					#anchor="w",
					width=12,
					command=function() run_WaferMap(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
					text="QUIT",
					#anchor="w",
					width=12,
					command=function()tkdestroy(wafermap_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
					text="RUN",
					#anchor="w",
					width=12,
					command=function() run_WaferMap(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	dir_entry_frame <- tkframe(wafermap_win)
	dir_entry_label <- tklabel(dir_entry_frame,
						width=10,
						text="directory")
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

	rtdf_entry_frame <- tkframe(wafermap_win)
	rtdf_entry_label <- tklabel(rtdf_entry_frame,
						width=10,
						text="rtdf_name")
	tkpack(rtdf_entry_label,side="left")
		rtdf_entry <- tkentry(rtdf_entry_frame,
						width=20,
						textvariable=Rtdf_name)
	tkpack(rtdf_entry,side="left",fill="x",expand=1)
	rtdf_browse <- tkbutton(rtdf_entry_frame,
						text="Browse",
						command=function() rtdf_browser(Rtdf_name,Rtdf_dir))
	tkpack(rtdf_browse,side="right")
	tkpack(rtdf_entry_frame,side="top",anchor="w",fill="x")

	pdf_entry_frame <- tkframe(wafermap_win)
	pdf_entry_label <- tklabel(pdf_entry_frame,
						width=10,
						text="pdf_name ")
	tkpack(pdf_entry_label,side="left")
	pdf_entry <- tkentry(pdf_entry_frame,
						width=20,
						textvariable=wmap_pdf_name)
	tkpack(pdf_entry,side="left",fill="x",expand=1)
	pdf_browse <- tkbutton(pdf_entry_frame,
						text="Browse",
						command=function() pdf_browser(wmap_pdf_name))
	tkpack(pdf_browse,side="right")
	tkpack(pdf_entry_frame,side="top",anchor="w",fill="x")

	type_frame <- tkframe(wafermap_win)
	type_label <- tklabel(type_frame, text="type")
	tkpack(type_label,side="left")
	type_sbin <- tkradiobutton(type_frame,
						text="sbin",
						value="sbin",
						#command=function() parameter_entry(0),  REVISIT
						variable=wmap_type)
	tkpack(type_sbin,side="left")
	type_hbin <- tkradiobutton(type_frame,
						text="hbin",
						value="hbin",
						#command=function() parameter_entry(0),  REVISIT
						variable=wmap_type)
	tkpack(type_hbin,side="left")
	type_param <- tkradiobutton(type_frame,
						text="parameter",
						value="parameter",
						#command=function() parameter_entry(1),  REVISIT
						# if set, check if parameter is empty, if so set
						# its background to yellow,
						#    tkconfigure(param_entry,background="yellow")
						# also set run_disable to true... grey out
						# run and run&quit buttons
						variable=wmap_type)
	tkpack(type_param,side="left")
	tkpack(type_frame,side="top",anchor="w")

	xalpha_button <- tkcheckbutton(wafermap_win,
						text="x_coord_alpha",
						variable=x_coord_alpha)
	tkpack(xalpha_button,side="top",anchor="w")

	pnl_button <- tkcheckbutton(wafermap_win,
						text="panel",
						variable=panel)
	tkpack(pnl_button,side="top",anchor="w")

	ydown_button <- tkcheckbutton(wafermap_win,
						text="y_down",
						variable=wmap_ydown)
	tkpack(ydown_button,side="top",anchor="w")

	xleft_button <- tkcheckbutton(wafermap_win,
						text="x_left",
						variable=wmap_xleft)
	tkpack(xleft_button,side="top",anchor="w")

	notch_frame <- tkframe(wafermap_win)
	notch_label <- tklabel(notch_frame, text="notch")
	tkpack(notch_label,side="left")
	notch_none <- tkradiobutton(notch_frame,
						text="hide",
						value="x",
						variable=wmap_notch)
	tkpack(notch_none,side="left")
	notch_n <- tkradiobutton(notch_frame,
						text="north",
						value="n",
						variable=wmap_notch)
	tkpack(notch_n,side="left")
	notch_s <- tkradiobutton(notch_frame,
						text="south",
						value="s",
						variable=wmap_notch)
	tkpack(notch_s,side="left")
	notch_e <- tkradiobutton(notch_frame,
						text="east",
						value="e",
						variable=wmap_notch)
	tkpack(notch_e,side="left")
	notch_w <- tkradiobutton(notch_frame,
						text="west",
						value="w",
						variable=wmap_notch)
	tkpack(notch_w,side="left")
	tkpack(notch_frame,side="top",anchor="w")

	param_entry_frame <- tkframe(wafermap_win)
	param_entry_label <- tklabel(param_entry_frame,
						width=10,
						text="parameter")
	tkpack(param_entry_label,side="left")
	param_entry <- tkentry(param_entry_frame,
						width=20,
						textvariable=wmap_parameter)
	tkpack(param_entry,side="left",fill="x",expand=1)
	param_browse <- tkbutton(param_entry_frame,
						text="Browse",
						command=function() param_browser(wmap_parameter,
											Rtdf_name,Rtdf_dir)
						)
	tkpack(param_browse,side="right")
	tkpack(param_entry_frame,side="top",anchor="w",fill="x")

	#param_col_frame <- tkframe(wafermap_win) -- moved to prior to DEFAULTS button
	col_start_label <- tklabel(param_col_frame,
						width=15,
						text="param_col_start")
	tkpack(col_start_label,side="left")
	#col_start_entry <- tkentry(param_col_frame,  -- moved to prior to DEFAULTS button
	#					width=20,
	#					background="white",
	#					textvariable=wmap_param_col_start)
	tkpack(col_start_entry,side="left",anchor="n")
	tkbind(col_start_entry,"<KeyRelease>",function() {
					tmp <- as.numeric(tclObj(wmap_param_col_start_shadow))
					if( (length(tmp)>0) && is.finite(tmp) &&
							(tmp>=0.0)  && (tmp<=1.0) ) {
						tkconfigure(col_start_entry,background="white")
						tclvalue(wmap_param_col_start) <- tmp
					} else {
						tkconfigure(col_start_entry,background="yellow")
					}
					tcl('update')
				})
	col_end_label <- tklabel(param_col_frame,
						width=15,
						text="param_col_end")
	tkpack(col_end_label,side="left")
	#col_end_entry <- tkentry(param_col_frame,  -- moved to prior to DEFAULTS button
	#					width=20,
	#					background="white",
	#					textvariable=wmap_param_col_end)
	tkpack(col_end_entry,side="left",anchor="n")
	tkbind(col_end_entry,"<KeyRelease>",function() {
					tmp <- as.numeric(tclObj(wmap_param_col_end_shadow))
					if( (length(tmp)>0) && is.finite(tmp) &&
							(tmp>=0.0)  && (tmp<=1.0) ) {
						tkconfigure(col_end_entry,background="white")
						tclvalue(wmap_param_col_end) <- tmp
					} else {
						tkconfigure(col_end_entry,background="yellow")
					}
					tcl('update')
				})
	col_rev_button <- tkcheckbutton(param_col_frame,
						text="param_color_reverse",
						variable=wmap_param_col_rev)
	tkpack(col_rev_button,side="top",anchor="w")
	tkpack(param_col_frame,side="top",anchor="w")

	#border_off_frame <- tkframe(wafermap_win) -- moved to prior to DEFAULTS button
	border_off_label <- tklabel(border_off_frame,
						width=15,
						text="borders_off")
	tkpack(border_off_label,side="left")
	#border_off_entry <- tkentry(border_off_frame,  -- moved to prior to DEFAULTS button
	#					width=7,
	#					background="white",
	#					textvariable=wmap_borders_off)
	tkpack(border_off_entry,side="left",anchor="n")
	tkbind(border_off_entry,"<KeyRelease>",function() {
					tmp <- as.integer(tclObj(wmap_borders_off_shadow))
					if( (length(tmp)>0) && is.finite(tmp) &&
							(tmp>-1.5)  && (tmp<1e6) ) {
						tkconfigure(border_off_entry,background="white")
						tclvalue(wmap_borders_off) <- tmp
					} else {
						tkconfigure(border_off_entry,background="yellow")
					}
					tcl('update')
				})
	tkpack(border_off_frame,side="top",anchor="w")

	bin_vs_col_path_frame <- tkframe(wafermap_win)
	bin_vs_col_path_label <- tklabel(bin_vs_col_path_frame,
						width=20,
						text="bin_vs_colors_path")
	tkpack(bin_vs_col_path_label,side="left")
	bin_vs_col_path_entry <- tkentry(bin_vs_col_path_frame,
						width=20,
						textvariable=wmap_bin_vs_col_path)
	tkpack(bin_vs_col_path_entry,side="left",fill="x",expand=1)
	tkpack(bin_vs_col_path_frame,side="top",anchor="w",fill="x")
	bin_vs_col_name_frame <- tkframe(wafermap_win)
	bin_vs_col_name_label <- tklabel(bin_vs_col_name_frame,
						width=20,
						text="bin_vs_colors")
	tkpack(bin_vs_col_name_label,side="left")
	bin_vs_col_name_entry <- tkentry(bin_vs_col_name_frame,
						width=20,
						textvariable=wmap_bin_vs_col_name)
	tkpack(bin_vs_col_name_entry,side="left",fill="x",expand=1)
	bin_vs_col_browse <- tkbutton(bin_vs_col_name_frame,
						text="Browse",
						command=function() csv_name_path_browser(wmap_bin_vs_col_name,wmap_bin_vs_col_path))
	tkpack(bin_vs_col_browse,side="right")
	tkpack(bin_vs_col_name_frame,side="top",anchor="w",fill="x")

	gen_bins_csv_button <- tkcheckbutton(wafermap_win,
						text="generate_bins_file",
						variable=wmap_gen_bins_csv)
	tkpack(gen_bins_csv_button,side="top",anchor="w")

	autoopen_button <- tkcheckbutton(wafermap_win,
						text="auto_open_pdf",
						variable=wmap_autoopen)
	tkpack(autoopen_button,side="top",anchor="w")

}

#-----------------------------------------------------
XformWaferMapGui <-function(...) {

	WaferMapGui_defaults()		# initialize variables...
	xformwafermap_win <- tktoplevel()
	assign("xformwafermap_win",xformwafermap_win,envir=.TkRadar.wins)
	tkwm.title(xformwafermap_win, "XformWaferMap")
	
	# need to define white/yellow background objects first...
	param_col_frame <- tkframe(xformwafermap_win)
	col_start_entry <- tkentry(param_col_frame,
						width=7,
						background="white",
						textvariable=wmap_param_col_start_shadow)
	col_end_entry <- tkentry(param_col_frame,
						width=7,
						background="white",
						textvariable=wmap_param_col_end_shadow)
	border_off_frame <- tkframe(xformwafermap_win)
	border_off_entry <- tkentry(border_off_frame,
						width=7,
						background="white",
						textvariable=wmap_borders_off_shadow)
	xshift_entry_frame <- tkframe(xformwafermap_win)
	xshift_entry <- tkentry(xshift_entry_frame,
						width=10,
						background="white",
						textvariable=wmap_x_shift_shadow)
	yshift_entry_frame <- tkframe(xformwafermap_win)
	yshift_entry <- tkentry(yshift_entry_frame,
						width=10,
						background="white",
						textvariable=wmap_y_shift_shadow)
	
	bottom_row <- tkframe(xformwafermap_win)
	default_button <- tkbutton(bottom_row,
					text="DEFAULTS",
					#anchor="w",
					width=12,
					command=function() {
						WaferMapGui_defaults()
						tkconfigure(col_start_entry,background="white")
						tkconfigure(col_end_entry,background="white")
						tkconfigure(border_off_entry,background="white")
						tkconfigure(xshift_entry,background="white")
						tkconfigure(yshift_entry,background="white")
					}
					)
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
					text="RUN & QUIT",
					#anchor="w",
					width=12,
					command=function() run_XformWaferMap(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
					text="QUIT",
					#anchor="w",
					width=12,
					command=function()tkdestroy(xformwafermap_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
					text="RUN",
					#anchor="w",
					width=12,
					command=function() run_XformWaferMap(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	dir_entry_frame <- tkframe(xformwafermap_win)
	dir_entry_label <- tklabel(dir_entry_frame,
						width=10,
						text="directory")
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

	rtdf_entry_frame <- tkframe(xformwafermap_win)
	rtdf_entry_label <- tklabel(rtdf_entry_frame,
						width=10,
						text="rtdf_name")
	tkpack(rtdf_entry_label,side="left")
		rtdf_entry <- tkentry(rtdf_entry_frame,
						width=20,
						textvariable=Rtdf_name)
	tkpack(rtdf_entry,side="left",fill="x",expand=1)
	rtdf_browse <- tkbutton(rtdf_entry_frame,
						text="Browse",
						command=function() rtdf_browser(Rtdf_name,Rtdf_dir))
	tkpack(rtdf_browse,side="right")
	tkpack(rtdf_entry_frame,side="top",anchor="w",fill="x")

	pdf_entry_frame <- tkframe(xformwafermap_win)
	pdf_entry_label <- tklabel(pdf_entry_frame,
						width=10,
						text="pdf_name ")
	tkpack(pdf_entry_label,side="left")
	pdf_entry <- tkentry(pdf_entry_frame,
						width=20,
						textvariable=wmap_xform_pdf_name)
	tkpack(pdf_entry,side="left",fill="x",expand=1)
	pdf_browse <- tkbutton(pdf_entry_frame,
						text="Browse",
						command=function() pdf_browser(wmap_xform_pdf_name))
	tkpack(pdf_browse,side="right")
	tkpack(pdf_entry_frame,side="top",anchor="w",fill="x")

	type_frame <- tkframe(xformwafermap_win)
	type_label <- tklabel(type_frame, text="type")
	tkpack(type_label,side="left")
	type_sbin <- tkradiobutton(type_frame,
						text="sbin",
						value="sbin",
						#command=function() parameter_entry(0),  REVISIT
						variable=wmap_type)
	tkpack(type_sbin,side="left")
	type_hbin <- tkradiobutton(type_frame,
						text="hbin",
						value="hbin",
						#command=function() parameter_entry(0),  REVISIT
						variable=wmap_type)
	tkpack(type_hbin,side="left")
	type_param <- tkradiobutton(type_frame,
						text="parameter",
						value="parameter",
						#command=function() parameter_entry(1),  REVISIT
						# if set, check if parameter is empty, if so set
						# its background to yellow,
						#    tkconfigure(param_entry,background="yellow")
						# also set run_disable to true... grey out
						# run and run&quit buttons
						variable=wmap_type)
	tkpack(type_param,side="left")
	tkpack(type_frame,side="top",anchor="w")

	xalpha_button <- tkcheckbutton(xformwafermap_win,
						text="x_coord_alpha",
						variable=x_coord_alpha)
	tkpack(xalpha_button,side="top",anchor="w")

	pnl_button <- tkcheckbutton(xformwafermap_win,
						text="panel",
						variable=panel)
	tkpack(pnl_button,side="top",anchor="w")

	ydown_button <- tkcheckbutton(xformwafermap_win,
						text="y_down",
						variable=wmap_ydown)
	tkpack(ydown_button,side="top",anchor="w")

	xleft_button <- tkcheckbutton(xformwafermap_win,
						text="x_left",
						variable=wmap_xleft)
	tkpack(xleft_button,side="top",anchor="w")

	notch_frame <- tkframe(xformwafermap_win)
	notch_label <- tklabel(notch_frame, text="notch")
	tkpack(notch_label,side="left")
	notch_none <- tkradiobutton(notch_frame,
						text="hide",
						value="x",
						variable=wmap_notch)
	tkpack(notch_none,side="left")
	notch_n <- tkradiobutton(notch_frame,
						text="north",
						value="n",
						variable=wmap_notch)
	tkpack(notch_n,side="left")
	notch_s <- tkradiobutton(notch_frame,
						text="south",
						value="s",
						variable=wmap_notch)
	tkpack(notch_s,side="left")
	notch_e <- tkradiobutton(notch_frame,
						text="east",
						value="e",
						variable=wmap_notch)
	tkpack(notch_e,side="left")
	notch_w <- tkradiobutton(notch_frame,
						text="west",
						value="w",
						variable=wmap_notch)
	tkpack(notch_w,side="left")
	tkpack(notch_frame,side="top",anchor="w")

	param_entry_frame <- tkframe(xformwafermap_win)
	param_entry_label <- tklabel(param_entry_frame,
						width=10,
						text="parameter")
	tkpack(param_entry_label,side="left")
	param_entry <- tkentry(param_entry_frame,
						width=20,
						textvariable=wmap_parameter)
	tkpack(param_entry,side="left",fill="x",expand=1)
	param_browse <- tkbutton(param_entry_frame,
						text="Browse",
						command=function() param_browser(wmap_parameter,
											Rtdf_name,Rtdf_dir)
						)
	tkpack(param_browse,side="right")
	tkpack(param_entry_frame,side="top",anchor="w",fill="x")

	#param_col_frame <- tkframe(wafermap_win) -- moved to prior to DEFAULTS button
	col_start_label <- tklabel(param_col_frame,
						width=15,
						text="param_col_start")
	tkpack(col_start_label,side="left")
	#col_start_entry <- tkentry(param_col_frame,  -- moved to prior to DEFAULTS button
	#					width=20,
	#					background="white",
	#					textvariable=wmap_param_col_start)
	tkpack(col_start_entry,side="left",anchor="n")
	tkbind(col_start_entry,"<KeyRelease>",function() {
					tmp <- as.numeric(tclObj(wmap_param_col_start_shadow))
					if( (length(tmp)>0) && is.finite(tmp) &&
							(tmp>=0.0)  && (tmp<=1.0) ) {
						tkconfigure(col_start_entry,background="white")
						tclvalue(wmap_param_col_start) <- tmp
					} else {
						tkconfigure(col_start_entry,background="yellow")
					}
					tcl('update')
				})
	col_end_label <- tklabel(param_col_frame,
						width=15,
						text="param_col_end")
	tkpack(col_end_label,side="left")
	#col_end_entry <- tkentry(param_col_frame,  -- moved to prior to DEFAULTS button
	#					width=20,
	#					background="white",
	#					textvariable=wmap_param_col_end)
	tkpack(col_end_entry,side="left",anchor="n")
	tkbind(col_end_entry,"<KeyRelease>",function() {
					tmp <- as.numeric(tclObj(wmap_param_col_end_shadow))
					if( (length(tmp)>0) && is.finite(tmp) &&
							(tmp>=0.0)  && (tmp<=1.0) ) {
						tkconfigure(col_end_entry,background="white")
						tclvalue(wmap_param_col_end) <- tmp
					} else {
						tkconfigure(col_end_entry,background="yellow")
					}
					tcl('update')
				})
	col_rev_button <- tkcheckbutton(param_col_frame,
						text="param_color_reverse",
						variable=wmap_param_col_rev)
	tkpack(col_rev_button,side="top",anchor="w")
	tkpack(param_col_frame,side="top",anchor="w")

	#border_off_frame <- tkframe(wafermap_win) -- moved to prior to DEFAULTS button
	border_off_label <- tklabel(border_off_frame,
						width=15,
						text="borders_off")
	tkpack(border_off_label,side="left")
	#border_off_entry <- tkentry(border_off_frame,  -- moved to prior to DEFAULTS button
	#					width=7,
	#					background="white",
	#					textvariable=wmap_borders_off)
	tkpack(border_off_entry,side="left",anchor="n")
	tkbind(border_off_entry,"<KeyRelease>",function() {
					tmp <- as.numeric(tclObj(wmap_borders_off_shadow))
					if( (length(tmp)>0) && is.finite(tmp) &&
							(tmp>-1.5)  && (tmp<1e6) ) {
						tkconfigure(border_off_entry,background="white")
						tclvalue(wmap_borders_off) <- tmp
					} else {
						tkconfigure(border_off_entry,background="yellow")
					}
					tcl('update')
				})
	tkpack(border_off_frame,side="top",anchor="w")

	bin_vs_col_path_frame <- tkframe(xformwafermap_win)
	bin_vs_col_path_label <- tklabel(bin_vs_col_path_frame,
						width=20,
						text="bin_vs_colors_path")
	tkpack(bin_vs_col_path_label,side="left")
	bin_vs_col_path_entry <- tkentry(bin_vs_col_path_frame,
						width=20,
						textvariable=wmap_bin_vs_col_path)
	tkpack(bin_vs_col_path_entry,side="left",fill="x",expand=1)
	tkpack(bin_vs_col_path_frame,side="top",anchor="w",fill="x")
	bin_vs_col_name_frame <- tkframe(xformwafermap_win)
	bin_vs_col_name_label <- tklabel(bin_vs_col_name_frame,
						width=20,
						text="bin_vs_colors")
	tkpack(bin_vs_col_name_label,side="left")
	bin_vs_col_name_entry <- tkentry(bin_vs_col_name_frame,
						width=20,
						textvariable=wmap_bin_vs_col_name)
	tkpack(bin_vs_col_name_entry,side="left",fill="x",expand=1)
	bin_vs_col_browse <- tkbutton(bin_vs_col_name_frame,
						text="Browse",
						command=function() csv_name_path_browser(wmap_bin_vs_col_name,wmap_bin_vs_col_path))
	tkpack(bin_vs_col_browse,side="right")
	tkpack(bin_vs_col_name_frame,side="top",anchor="w",fill="x")

	gen_bins_csv_button <- tkcheckbutton(xformwafermap_win,
						text="generate_bins_file",
						variable=wmap_gen_bins_csv)
	tkpack(gen_bins_csv_button,side="top",anchor="w")

	rotate_frame <- tkframe(xformwafermap_win)
	rotate_label <- tklabel(rotate_frame, text="rotate_ccw")
	tkpack(rotate_label,side="left")
	rotate0 <- tkradiobutton(rotate_frame,
						text="0   ",
						value="0",
						variable=wmap_rotate_ccw)
	tkpack(rotate0,side="left")
	rotate90 <- tkradiobutton(rotate_frame,
						text="90  ",
						value="90",
						variable=wmap_rotate_ccw)
	tkpack(rotate90,side="left")
	rotate180 <- tkradiobutton(rotate_frame,
						text="180 ",
						value="180",
						variable=wmap_rotate_ccw)
	tkpack(rotate180,side="left")
	rotate270 <- tkradiobutton(rotate_frame,
						text="270 ",
						value="270",
						variable=wmap_rotate_ccw)
	tkpack(rotate270,side="left")
	tkpack(rotate_frame,side="top",anchor="w")

	x_polar_button <- tkcheckbutton(xformwafermap_win,
						text="x_reverse_polarity",
						variable=wmap_x_rev_polarity)
	tkpack(x_polar_button,side="top",anchor="w")

	y_polar_button <- tkcheckbutton(xformwafermap_win,
						text="y_reverse_polarity",
						variable=wmap_y_rev_polarity)
	tkpack(y_polar_button,side="top",anchor="w")

	#xshift_entry_frame <- tkframe(xformwafermap_win) ... earlier
	xshift_entry_label <- tklabel(xshift_entry_frame,
						width=10,
						text="x_shift")
	tkpack(xshift_entry_label,side="left")
	#xshift_entry <- tklabel(xshift_entry_frame,.. earlier
	tkpack(xshift_entry,side="left",anchor="n")
	tkbind(xshift_entry,"<KeyRelease>",function() {
					tmp <- as.integer(tclObj(wmap_x_shift_shadow))
					if( (length(tmp)>0) && is.finite(tmp) &&
							(tmp>-1e6)  && (tmp<1e6) ) {
						tkconfigure(xshift_entry,background="white")
						tclvalue(wmap_x_shift) <- tmp
					} else {
						tkconfigure(xshift_entry,background="yellow")
					}
					tcl('update')
				})
	tkpack(xshift_entry_frame,side="top",anchor="w")

	#yshift_entry_frame <- tkframe(xformwafermap_win)
	yshift_entry_label <- tklabel(yshift_entry_frame,
						width=10,
						text="y_shift")
	tkpack(yshift_entry_label,side="left")
	#yshift_entry <- tklabel(yshift_entry_frame, .. earlier now
	tkpack(yshift_entry,side="left",anchor="n")
	tkbind(yshift_entry,"<KeyRelease>",function() {
					tmp <- as.integer(tclObj(wmap_y_shift_shadow))
					if( (length(tmp)>0) && is.finite(tmp) &&
							(tmp>-1e6)  && (tmp<1e6) ) {
						tkconfigure(yshift_entry,background="white")
						tclvalue(wmap_y_shift) <- tmp
					} else {
						tkconfigure(yshift_entry,background="yellow")
					}
					tcl('update')
				})
	tkpack(yshift_entry_frame,side="top",anchor="w")

	autoopen_button <- tkcheckbutton(xformwafermap_win,
						text="auto_open_pdf",
						variable=wmap_autoopen)
	tkpack(autoopen_button,side="top",anchor="w")

}



