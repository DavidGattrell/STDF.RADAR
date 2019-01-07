# PlotTestvsTestGui.R
#
# $Id: PlotTestvsTestGui.R,v 1.6 2013/09/01 03:36:05 david Exp $
#
# Tk/Tcl GUI wrapper for calling PlotTestvsTest.R
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
# PlotTestvsTest gui specific variables
#--------------------------------------
if(!exists(".TkRadar.env")) .TkRadar.env <- new.env()

assign("testX",tclVar(""),envir=.TkRadar.env)
assign("testY",tclVar(""),envir=.TkRadar.env)
assign("tvst_png_file",tclVar(""),envir=.TkRadar.env)
show_part_ids <- tclVar(0)
tvst_png_flag <- tclVar(0)
tvst_xmin_flag <- tclVar(0)
tvst_xmax_flag <- tclVar(0)
tvst_ymin_flag <- tclVar(0)
tvst_ymax_flag <- tclVar(0)
tvst_xmin <- tclVar(0)
tvst_xmax <- tclVar(0)
tvst_ymin <- tclVar(0)
tvst_ymax <- tclVar(0)

#----------------------------------------------------
PlotTestvsTestGui_defaults <- function() {
	tclvalue(testX) <- ""
	tclvalue(testY) <- ""
	tclvalue(show_part_ids) <- 0
	tclvalue(tvst_png_flag) <- 0
	tclvalue(tvst_png_file) <- ""
	tclvalue(tvst_xmin_flag) <- 0
	tclvalue(tvst_xmax_flag) <- 0
	tclvalue(tvst_ymin_flag) <- 0
	tclvalue(tvst_ymax_flag) <- 0
	tclvalue(tvst_xmin) <- 0
	tclvalue(tvst_xmax) <- 0
	tclvalue(tvst_ymin) <- 0
	tclvalue(tvst_ymax) <- 0

}


#----------------------------------------------------
run_PlotTestvsTest <- function(done=FALSE,...) {
	test_rtdf <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
	testX_ <- paste(tclObj(testX),sep="",collapse=" ")
	testY_ <- paste(tclObj(testY),sep="",collapse=" ")
	show_part_ids_ <- as.logical(tclObj(show_part_ids))
	rtdf_dir_ <- paste(tclObj(Rtdf_dir),sep="",collapse=" ")
	if (nchar(rtdf_dir_)<1)  rtdf_dir_ <- paste(tclObj(Output_dir),sep="",collapse=" ")
	if (nchar(rtdf_dir_)<1)  rtdf_dir_ <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	png_flag_ <- as.logical(tclObj(tvst_png_flag))
	png_file_ <- paste(tclObj(tvst_png_file),sep="",collapse=" ")

	limits_flag_ = 0
	if( as.logical(tclObj(tvst_xmin_flag)) )  limits_flag_ = limits_flag_ + 1
	if( as.logical(tclObj(tvst_xmax_flag)) )  limits_flag_ = limits_flag_ + 2
	if( as.logical(tclObj(tvst_ymin_flag)) )  limits_flag_ = limits_flag_ + 4
	if( as.logical(tclObj(tvst_ymax_flag)) )  limits_flag_ = limits_flag_ + 8

	limits_ = as.numeric(tclObj(tvst_xmin))
	limits_[2] = as.numeric(tclObj(tvst_xmax))
	limits_[3] = as.numeric(tclObj(tvst_ymin))
	limits_[4] = as.numeric(tclObj(tvst_ymax))

	output_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	if (nchar(output_dir)<1)  output_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	setwd(output_dir)

	my_expr = substitute(
		PlotTestvsTest(rtdf_file=test_rtdf,
					testX=testX_,testY=testY_,
					show_part_ids=show_part_ids_,
					rtdf_dir=rtdf_dir_,png_flag=png_flag_,
					png_file=png_file_,
					limits_flag=limits_flag_,limits=limits_)
		)
	tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
	tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
	my_command = sprintf("%s\n",deparse(my_expr))
	my_cmnd = "PlotTestvsTest(...)\n"
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

	if(done>0)  tkdestroy(plottestvstest_win)
}


#----------------------------------------------------
PlotTestvsTestGui <-function(...) {

	PlotTestvsTestGui_defaults()
	plottestvstest_win <- tktoplevel()
	assign("plottestvstest_win",plottestvstest_win,envir=.TkRadar.wins)
	tkwm.title(plottestvstest_win, "PlotTestvsTest")

	# these get reset by Default button, so need to be defined first...
	xmin_frame <- tkframe(plottestvstest_win)
	xmin_entry <- tkentry(xmin_frame,
					width=12,
					background="white",
					disabledbackground="grey",
					state="disabled",
					textvariable=tvst_xmin
					)
	xmax_frame <- tkframe(plottestvstest_win)
	xmax_entry <- tkentry(xmax_frame,
					width=12,
					background="white",
					disabledbackground="grey",
					state="disabled",
					textvariable=tvst_xmax
					)
	ymin_frame <- tkframe(plottestvstest_win)
	ymin_entry <- tkentry(ymin_frame,
					width=12,
					background="white",
					disabledbackground="grey",
					state="disabled",
					textvariable=tvst_ymin
					)
	ymax_frame <- tkframe(plottestvstest_win)
	ymax_entry <- tkentry(ymax_frame,
					width=12,
					background="white",
					disabledbackground="grey",
					state="disabled",
					textvariable=tvst_ymax	
					)

	#... now typical bottom row definition, but with modified Defaults...
	bottom_row <- tkframe(plottestvstest_win)
	default_button <- tkbutton(bottom_row,
					text="DEFAULTS",
					#anchor="w",
					width=12,
					command=function() {
						PlotTestvsTestGui_defaults()
						tkconfigure(xmin_entry,state="disabled")
						tkconfigure(xmax_entry,state="disabled")
						tkconfigure(ymin_entry,state="disabled")
						tkconfigure(ymax_entry,state="disabled")
						tkconfigure(xmin_entry,background="white")		
						tkconfigure(xmax_entry,background="white")		
						tkconfigure(ymin_entry,background="white")		
						tkconfigure(ymax_entry,background="white")		
				})
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
					text="RUN & QUIT",
					#anchor="w",
					width=12,
					command=function() run_PlotTestvsTest(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
					text="QUIT",
					#anchor="w",
					width=12,
					command=function()tkdestroy(plottestvstest_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
					text="RUN",
					#anchor="w",
					width=12,
					command=function() run_PlotTestvsTest(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")


	rtdf_entry_frame <- tkframe(plottestvstest_win)
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


	testx_entry_frame <- tkframe(plottestvstest_win)
	testx_entry_label <- tklabel(testx_entry_frame,
						width=10,
						text="X parameter")
	tkpack(testx_entry_label,side="left")
	testx_entry <- tkentry(testx_entry_frame,
						width=20,
						textvariable=testX)
	tkpack(testx_entry,side="left",fill="x",expand=1)
	testx_browse <- tkbutton(testx_entry_frame,
						text="Browse",
						command=function() XYparam_browser(testX,testY,
											Rtdf_name,Rtdf_dir)
						)
	tkpack(testx_browse,side="right")
	tkpack(testx_entry_frame,side="top",anchor="w",fill="x")


	testy_entry_frame <- tkframe(plottestvstest_win)
	testy_entry_label <- tklabel(testy_entry_frame,
						width=10,
						text="Y parameter")
	tkpack(testy_entry_label,side="left")
	testy_entry <- tkentry(testy_entry_frame,
						width=20,
						textvariable=testY)
	tkpack(testy_entry,side="left",fill="x",expand=1)
	testy_browse <- tkbutton(testy_entry_frame,
						text="Browse",
						command=function() XYparam_browser(testX,testY,
											Rtdf_name,Rtdf_dir)
						)
	tkpack(testy_browse,side="right")
	tkpack(testy_entry_frame,side="top",anchor="w",fill="x")

	partids_button <- tkcheckbutton(plottestvstest_win,
						text="show_part_ids",
						variable=show_part_ids)
	tkpack(partids_button,side="top",anchor="w")

	do_png_button <- tkcheckbutton(plottestvstest_win,
						text="png_flag",
						variable=tvst_png_flag)
	tkpack(do_png_button,side="top",anchor="w")

	png_entry_frame <- tkframe(plottestvstest_win)
	png_entry_label <- tklabel(png_entry_frame,
						width=10,
						text="png_file")
	tkpack(png_entry_label,side="left")
	png_entry <- tkentry(png_entry_frame,
						width=20,
						textvariable=tvst_png_file)
	tkpack(png_entry,side="left",fill="x",expand=1)
	tkpack(png_entry_frame,side="top",anchor="w",fill="x")

	xmin_button <- tkcheckbutton(xmin_frame,
					width=10,
					text="Force Xmin",
					variable=tvst_xmin_flag,
					command=function() {
						ok <- as.logical(tclObj(tvst_xmin_flag))
						if(ok) {
							tkconfigure(xmin_entry,state="normal")
						} else {
							tkconfigure(xmin_entry,state="disabled")
						}
					}
					)
	tkpack(xmin_button,side="left",anchor="n")
	tkpack(xmin_entry,side="left",anchor="n")
	tkbind(xmin_entry,"<KeyRelease>",function() {
					tmp <- as.numeric(tclObj(tvst_xmin))
					if( (length(tmp)>0) && is.finite(tmp)) {
						tkconfigure(xmin_entry,background="white")
					} else {
						tkconfigure(xmin_entry,background="yellow")
					}
					tcl('update')
				})
	tkpack(xmin_frame,side="top",anchor="w",fill="x")

	xmax_button <- tkcheckbutton(xmax_frame,
					width=10,
					text="Force Xmax",
					variable=tvst_xmax_flag,
					command=function() {
						ok <- as.logical(tclObj(tvst_xmax_flag))
						if(ok) {
							tkconfigure(xmax_entry,state="normal")
						} else {
							tkconfigure(xmax_entry,state="disabled")
						}
					}
					)
	tkpack(xmax_button,side="left",anchor="n")
	tkpack(xmax_entry,side="left",anchor="n")
	tkbind(xmax_entry,"<KeyRelease>",function() {
					tmp <- as.numeric(tclObj(tvst_xmax))
					if( (length(tmp)>0) && is.finite(tmp)) {
						tkconfigure(xmax_entry,background="white")
					} else {
						tkconfigure(xmax_entry,background="yellow")
					}
					tcl('update')
				})
	tkpack(xmax_frame,side="top",anchor="w",fill="x")

	ymin_button <- tkcheckbutton(ymin_frame,
					width=10,
					text="Force Ymin",
					variable=tvst_ymin_flag,
					command=function() {
						ok <- as.logical(tclObj(tvst_ymin_flag))
						if(ok) {
							tkconfigure(ymin_entry,state="normal")
						} else {
							tkconfigure(ymin_entry,state="disabled")
						}
					}
					)
	tkpack(ymin_button,side="left",anchor="n")
	tkpack(ymin_entry,side="left",anchor="n")
	tkbind(ymin_entry,"<KeyRelease>",function() {
					tmp <- as.numeric(tclObj(tvst_ymin))
					if( (length(tmp)>0) && is.finite(tmp)) {
						tkconfigure(ymin_entry,background="white")
					} else {
						tkconfigure(ymin_entry,background="yellow")
					}
					tcl('update')
				})
	tkpack(ymin_frame,side="top",anchor="w",fill="x")

	ymax_button <- tkcheckbutton(ymax_frame,
					width=10,
					text="Force Ymax",
					variable=tvst_ymax_flag,
					command=function() {
						ok <- as.logical(tclObj(tvst_ymax_flag))
						if(ok) {
							tkconfigure(ymax_entry,state="normal")
						} else {
							tkconfigure(ymax_entry,state="disabled")
						}
					}
					)
	tkpack(ymax_button,side="left",anchor="n")
	tkpack(ymax_entry,side="left",anchor="n")
	tkbind(ymax_entry,"<KeyRelease>",function() {
					tmp <- as.numeric(tclObj(tvst_ymax))
					if( (length(tmp)>0) && is.finite(tmp)) {
						tkconfigure(ymax_entry,background="white")
					} else {
						tkconfigure(ymax_entry,background="yellow")
					}
					tcl('update')
				})
	tkpack(ymax_frame,side="top",anchor="w",fill="x")

}



