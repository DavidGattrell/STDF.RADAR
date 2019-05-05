# AsciiWaferMapGui.R
#
# $Id: AsciiWaferMapGui.R,v 1.5 2019/05/05 21:51:05 david Exp $
#
# Tk/Tcl GUI wrapper for calling AsciiWaferMap.R
# called by TkRadar.R
#
# Copyright (C) 2009-2010 David Gattrell
#				2018  David Gattrell
#				2019  David Gattrell
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
# AsciiWaferMap gui specific variables
#---------------------------------------
if(!exists(".TkRadar.env")) .TkRadar.env <- new.env()

assign("ascwmap_name",tclVar(""),envir=.TkRadar.env)
assign("ascwmap_type",tclVar("sbin"),envir=.TkRadar.env)
assign("ascwmap_xleft",tclVar(0),envir=.TkRadar.env)
assign("ascwmap_ydown",tclVar(0),envir=.TkRadar.env)
assign("ascwmap_notch",tclVar("S"),envir=.TkRadar.env)
assign("ascwmap_pass_bins",tclVar(-1),envir=.TkRadar.env)
assign("ascwmap_test_floor",tclVar(""),envir=.TkRadar.env)
assign("ascwmap_product_id",tclVar(""),envir=.TkRadar.env)
assign("ascwmap_lot_id",tclVar(""),envir=.TkRadar.env)
assign("ascwmap_wafer_id",tclVar(""),envir=.TkRadar.env)
assign("ascwmap_yield",tclVar(1),envir=.TkRadar.env)
assign("ascwmap_multi_bin",tclVar(0),envir=.TkRadar.env)
assign("ascwmap_multi_terse",tclVar(0),envir=.TkRadar.env)
assign("ascwmap_skip_die_minus",tclVar(1),envir=.TkRadar.env)
assign("ascwmap_mirror_die",tclVar(""),envir=.TkRadar.env)
assign("ascwmap_sinf_fmt",tclVar(0),envir=.TkRadar.env)
assign("ascwmap_x_step",tclVar(""),envir=.TkRadar.env)
assign("ascwmap_y_step",tclVar(""),envir=.TkRadar.env)

# these defaults can be controlled in the .Rprofile file:
default_ascwmap_type <- tclVar("sbin")		# per user customizing
default_ascwmap_xleft <- tclVar(0)			# per user customizing
default_ascwmap_ydown <- tclVar(0)			# per user customizing
default_ascwmap_notch <- tclVar("S")		# per user customizing
default_ascwmap_pass_bins <- tclVar(-1)		# per user customizing
default_ascwmap_test_floor <- tclVar("")	# per user customizing
default_ascwmap_product_id <- tclVar("")	# per user customizing
default_ascwmap_yield <- tclVar(1)			# per user customizing
default_ascwmap_multi_bin <- tclVar(0)		# per user customizing
default_ascwmap_multi_terse <- tclVar(0)	# per user customizing
default_ascwmap_skip_die_minus <- tclVar(1)	# per user customizing
default_ascwmap_mirror_die <- tclVar("")	# per user customizing
default_ascwmap_sinf_fmt <- tclVar(0)
default_ascwmap_x_step <- tclVar("")
default_ascwmap_y_step <- tclVar("")

#----------------------------------------------------
AsciiWaferMapGui_defaults <- function(...) {
	tclvalue(ascwmap_name) <- ""
	tclvalue(ascwmap_type) <- tclObj(default_ascwmap_type)
	tclvalue(ascwmap_xleft) <- tclObj(default_ascwmap_xleft)
	tclvalue(ascwmap_ydown) <- tclObj(default_ascwmap_ydown)
	tclvalue(ascwmap_notch) <- tclObj(default_ascwmap_notch)
	tclvalue(ascwmap_pass_bins) <- tclObj(default_ascwmap_pass_bins)
	tclvalue(ascwmap_test_floor) <- tclObj(default_ascwmap_test_floor)
	tclvalue(ascwmap_product_id) <- tclObj(default_ascwmap_product_id)
	tclvalue(ascwmap_lot_id) <- ""
	tclvalue(ascwmap_wafer_id) <- ""
	tclvalue(ascwmap_yield) <- tclObj(default_ascwmap_yield)
	tclvalue(ascwmap_multi_bin) <- tclObj(default_ascwmap_multi_bin)
	tclvalue(ascwmap_multi_terse) <- tclObj(default_ascwmap_multi_terse)
	tclvalue(ascwmap_skip_die_minus) <- tclObj(default_ascwmap_skip_die_minus)
	tclvalue(ascwmap_mirror_die) <- tclObj(default_ascwmap_mirror_die)
	tclvalue(ascwmap_sinf_fmt) <- tclObj(default_ascwmap_sinf_fmt)
	tclvalue(ascwmap_x_step) <- tclObj(default_ascwmap_x_step)
	tclvalue(ascwmap_y_step) <- tclObj(default_ascwmap_y_step)
}


#----------------------------------------------------
run_AsciiWaferMap <-function(done=FALSE,...) {
	wmap_rtdf <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
	wmap_name_ <- paste(tclObj(ascwmap_name),sep="",collapse=" ")
	wmap_type_ <- as.character(tclObj(ascwmap_type))
	x_left_ <- as.logical(tclObj(ascwmap_xleft))
	y_down_ <- as.logical(tclObj(ascwmap_ydown))
	notch_ <- as.character(tclObj(ascwmap_notch))
	pass_bins_ <- as.integer(tclObj(ascwmap_pass_bins))
	floor <- paste(tclObj(ascwmap_test_floor),sep="",collapse=" ")
	product <- paste(tclObj(ascwmap_product_id),sep="",collapse=" ")
	lot <- paste(tclObj(ascwmap_lot_id),sep="",collapse=" ")
	wafer <- paste(tclObj(ascwmap_wafer_id),sep="",collapse=" ")
	yield <- as.logical(tclObj(ascwmap_yield))
	multi_bin <- as.logical(tclObj(ascwmap_multi_bin))
	multi_terse <- as.logical(tclObj(ascwmap_multi_terse))
	skip_die_minus_ <- as.logical(tclObj(ascwmap_skip_die_minus))
	mirror_die_ <- paste(tclObj(ascwmap_mirror_die),sep="",collapse=" ")
	sinf_fmt_ <- as.logical(tclObj(ascwmap_sinf_fmt))
	x_step_ <- paste(tclObj(ascwmap_x_step),sep="",collapse=" ")
	y_step_ <- paste(tclObj(ascwmap_y_step),sep="",collapse=" ")


	rtdf_dir_ <- paste(tclObj(Rtdf_dir),sep="",collapse=" ")
	output_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")

	if (nchar(output_dir)>0)  setwd(output_dir)

	my_expr = substitute(
		AsciiWaferMap(rtdf_name=wmap_rtdf,wmap_name=wmap_name_,
					type=wmap_type_,x_left=x_left_,y_down=y_down_,
					notch=notch_,pass_bins=pass_bins_,rtdf_dir=rtdf_dir_,
					test_floor=floor,product_id=product,lot_id=lot,
					wafer_id=wafer,do_yield=yield,
					multi_binning=multi_bin,multi_bin_terse=multi_terse,
					skip_die_minus=skip_die_minus_,
					mirror_die=mirror_die_,sinf_fmt=sinf_fmt_,
					x_step=x_step_,y_step=y_step_)
	)
	tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
	tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
	my_command = sprintf("%s\n",deparse(my_expr))
	my_cmnd = "AsciiWaferMap(...)\n"
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

	if(done>0) {
		asciiwafermap_win <- get("asciiwafermap_win",envir=.TkRadar.wins)
		tkdestroy(asciiwafermap_win)
	}

}


#-----------------------------------------------------
AsciiWaferMapGui <-function(...) {

	AsciiWaferMapGui_defaults()		# initialize variables...
	asciiwafermap_win <- tktoplevel()
	assign("asciiwafermap_win",asciiwafermap_win,envir=.TkRadar.wins)
	tkwm.title(asciiwafermap_win, "AsciiWaferMap")
	
	bottom_row <- tkframe(asciiwafermap_win)
	default_button <- tkbutton(bottom_row,
					text="DEFAULTS",
					#anchor="w",
					width=12,
					command=AsciiWaferMapGui_defaults)
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
					text="RUN & QUIT",
					#anchor="w",
					width=12,
					command=function() run_AsciiWaferMap(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
					text="QUIT",
					#anchor="w",
					width=12,
					command=function()tkdestroy(asciiwafermap_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
					text="RUN",
					#anchor="w",
					width=12,
					command=function() run_AsciiWaferMap(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	dir_entry_frame <- tkframe(asciiwafermap_win)
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

	rtdf_entry_frame <- tkframe(asciiwafermap_win)
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

	wmap_entry_frame <- tkframe(asciiwafermap_win)
	wmap_entry_label <- tklabel(wmap_entry_frame,
						width=10,
						text="wmap_name ")
	tkpack(wmap_entry_label,side="left")
	wmap_entry <- tkentry(wmap_entry_frame,
						width=20,
						textvariable=ascwmap_name)
	tkpack(wmap_entry,side="left",fill="x",expand=1)
	wmap_browse <- tkbutton(wmap_entry_frame,
						text="Browse",
						command=function() wmap_browser(ascwmap_name))
	tkpack(wmap_browse,side="right")
	tkpack(wmap_entry_frame,side="top",anchor="w",fill="x")

	sinf_button <- tkcheckbutton(asciiwafermap_win,
						text="sinf_fmt",
						variable=ascwmap_sinf_fmt)
	tkpack(sinf_button,side="top",anchor="w")

	type_frame <- tkframe(asciiwafermap_win)
	type_label <- tklabel(type_frame, text="type")
	tkpack(type_label,side="left")
	type_sbin <- tkradiobutton(type_frame,
						text="sbin",
						value="sbin",
						variable=ascwmap_type)
	tkpack(type_sbin,side="left")
	type_hbin <- tkradiobutton(type_frame,
						text="hbin",
						value="hbin",
						variable=ascwmap_type)
	tkpack(type_hbin,side="left")
	tkpack(type_frame,side="top",anchor="w")

	ydown_button <- tkcheckbutton(asciiwafermap_win,
						text="y_down",
						variable=ascwmap_ydown)
	tkpack(ydown_button,side="top",anchor="w")

	xleft_button <- tkcheckbutton(asciiwafermap_win,
						text="x_left",
						variable=ascwmap_xleft)
	tkpack(xleft_button,side="top",anchor="w")

	notch_frame <- tkframe(asciiwafermap_win)
	notch_label <- tklabel(notch_frame, text="notch")
	tkpack(notch_label,side="left")
	notch_s <- tkradiobutton(notch_frame,
						text="South",
						value="S",
						variable=ascwmap_notch)
	tkpack(notch_s,side="left")
	notch_n <- tkradiobutton(notch_frame,
						text="North",
						value="N",
						variable=ascwmap_notch)
	tkpack(notch_n,side="left")
	notch_w <- tkradiobutton(notch_frame,
						text="West",
						value="W",
						variable=ascwmap_notch)
	tkpack(notch_w,side="left")
	notch_e <- tkradiobutton(notch_frame,
						text="East",
						value="E",
						variable=ascwmap_notch)
	tkpack(notch_e,side="left")
	tkpack(notch_frame,side="top",anchor="w")


	num_entry_frame <- tkframe(asciiwafermap_win)
	num_entry_label <- tklabel(num_entry_frame,
						width=10,
						text="pass_bins")
	tkpack(num_entry_label,side="left")
	num_entry <- tklabel(num_entry_frame,
						width=20,
						relief="sunken",
						textvariable=ascwmap_pass_bins)
	tkpack(num_entry,side="left",fill="x",expand=1)
	num_browse <- tkbutton(num_entry_frame,
						text="Edit",
						command=function() indices_entry(ascwmap_pass_bins))
	tkpack(num_browse,side="right")
	tkpack(num_entry_frame,side="top",anchor="w",fill="x")


	floor_frame <- tkframe(asciiwafermap_win)
	floor_label <- tklabel(floor_frame,
						width=10,
						text="test_floor")
	tkpack(floor_label,side="left")
	floor_entry <- tkentry(floor_frame,
						width=20,
						textvariable=ascwmap_test_floor)
	tkpack(floor_entry,side="left",fill="x",expand=1)
	tkpack(floor_frame,side="top",anchor="w",fill="x")


	product_frame <- tkframe(asciiwafermap_win)
	product_label <- tklabel(product_frame,
						width=10,
						text="product_id")
	tkpack(product_label,side="left")
	product_entry <- tkentry(product_frame,
						width=20,
						textvariable=ascwmap_product_id)
	tkpack(product_entry,side="left",fill="x",expand=1)
	tkpack(product_frame,side="top",anchor="w",fill="x")


	lotid_frame <- tkframe(asciiwafermap_win)
	lotid_label <- tklabel(lotid_frame,
						width=10,
						text="lot_id")
	tkpack(lotid_label,side="left")
	lotid_entry <- tkentry(lotid_frame,
						width=20,
						textvariable=ascwmap_lot_id)
	tkpack(lotid_entry,side="left",fill="x",expand=1)
	tkpack(lotid_frame,side="top",anchor="w",fill="x")


	waferid_frame <- tkframe(asciiwafermap_win)
	waferid_label <- tklabel(waferid_frame,
						width=10,
						text="wafer_id")
	tkpack(waferid_label,side="left")
	waferid_entry <- tkentry(waferid_frame,
						width=20,
						textvariable=ascwmap_wafer_id)
	tkpack(waferid_entry,side="left",fill="x",expand=1)
	tkpack(waferid_frame,side="top",anchor="w",fill="x")


	wmap_yield_button <- tkcheckbutton(asciiwafermap_win,
						text="do_yield",
						variable=ascwmap_yield)
	tkpack(wmap_yield_button,side="top",anchor="w")


	wmap_multi_bin_button <- tkcheckbutton(asciiwafermap_win,
						text="multi_binning",
						variable=ascwmap_multi_bin)
	tkpack(wmap_multi_bin_button,side="top",anchor="w")


	wmap_multi_terse_button <- tkcheckbutton(asciiwafermap_win,
						text="multi_bin_terse",
						variable=ascwmap_multi_terse)
	tkpack(wmap_multi_terse_button,side="top",anchor="w")

	wmap_skip_die_minus_button <- tkcheckbutton(asciiwafermap_win,
						text="skip_die_minus",
						variable=ascwmap_skip_die_minus)
	tkpack(wmap_skip_die_minus_button,side="top",anchor="w")


	mirror_frame <- tkframe(asciiwafermap_win)
	mirror_label <- tklabel(mirror_frame,
						width=10,
						text="mirror_die")
	tkpack(mirror_label,side="left")
	mirror_entry <- tkentry(mirror_frame,
						width=20,
						textvariable=ascwmap_mirror_die)
	tkpack(mirror_entry,side="left",fill="x",expand=1)
	tkpack(mirror_frame,side="top",anchor="w",fill="x")


	xstep_frame <- tkframe(asciiwafermap_win)
	xstep_label <- tklabel(xstep_frame,
						width=10,
						text="x_step")
	tkpack(xstep_label,side="left")
	xstep_entry <- tkentry(xstep_frame,
						width=20,
						textvariable=ascwmap_x_step)
	tkpack(xstep_entry,side="left",fill="x",expand=1)
	tkpack(xstep_frame,side="top",anchor="w",fill="x")


	ystep_frame <- tkframe(asciiwafermap_win)
	ystep_label <- tklabel(ystep_frame,
						width=10,
						text="y_step")
	tkpack(ystep_label,side="left")
	ystep_entry <- tkentry(ystep_frame,
						width=20,
						textvariable=ascwmap_y_step)
	tkpack(ystep_entry,side="left",fill="x",expand=1)
	tkpack(ystep_frame,side="top",anchor="w",fill="x")
}



