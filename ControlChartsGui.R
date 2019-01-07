# ControlChartsGui.R
#
# $Id: ControlChartsGui.R,v 1.5 2010/11/22 03:14:21 David Exp $
#
# Tk/Tcl GUI wrapper for calling ControlCharts.R
# called by TkRadar.R
#
# Copyright (C) 2009-10 David Gattrell
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
# ControlCharts gui specific variables
#--------------------------------
if(!exists(".TkRadar.env")) .TkRadar.env <- new.env()

# param_name <- tclVar("")	# uses PlotRtdfGui variable
controlcharts_title <- tclVar("ControlCharts.R output")
do_western_electric <- tclVar(0)
control_start_n <- tclVar(1)
control_count_n <- tclVar(50)
control_charts_per_page <- tclVar(7)
control_landscape <- tclVar(0)
# param_name_dir <- tclVar("")	# uses PlotRtdfGui variable
controlcharts_pdf_name <- tclVar("my_controlcharts.pdf")
control_autoopen <- tclVar(0)

# these defaults can be controlled in the .Rprofile file:
default_do_western_electric <- tclVar(0)
default_control_start_n <- tclVar(1)
default_control_count_n <- tclVar(50)
default_control_charts_per_page <- tclVar(7)
default_control_landscape <- tclVar(0)
default_control_autoopen <- tclVar(1)



#-----------------------------------------------------
ControlChartsGui_defaults <- function(...) {
	tclvalue(controlcharts_pdf_name) <- "my_controlcharts.pdf"
#	tclvalue(param_name) <- ""
	tclvalue(controlcharts_title) <- "ControlCharts.R output"
	tclvalue(do_western_electric) <- tclObj(default_do_western_electric)
	tclvalue(control_start_n) <- tclObj(default_control_start_n)
	tclvalue(control_count_n) <- tclObj(default_control_count_n)
	tclvalue(control_charts_per_page) <- tclObj(default_control_charts_per_page)
	tclvalue(control_landscape) <- tclObj(default_control_landscape)
	tclvalue(control_autoopen) <- tclObj(default_control_autoopen)
#	tclvalue(param_name_dir) <- ""
}

#-----------------------------------------------------
inc_charts_p_page <- function() {

	my_value <- as.integer(tclObj(control_charts_per_page))
	if (my_value<20)  my_value <- my_value + 1
	tclvalue(control_charts_per_page) <- my_value
}

#-----------------------------------------------------
dec_charts_p_page <- function() {

	my_value <- as.integer(tclObj(control_charts_per_page))
	if (my_value>1)  my_value <- my_value - 1
	tclvalue(control_charts_per_page) <- my_value
}


#-----------------------------------------------------
run_ControlCharts <-function(done=FALSE,...) {

	rtdf_name_ <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
	pdf_name_ <- paste(tclObj(controlcharts_pdf_name),sep="",collapse=" ")
	param_name_ <- paste(tclObj(param_name),sep="",collapse=" ")
	title_ <- paste(tclObj(controlcharts_title),sep="",collapse=" ")
	do_western_electric_ <- as.logical(tclObj(do_western_electric))
	start_n_ <- as.integer(tclObj(control_start_n))
	count_n_ <- as.integer(tclObj(control_count_n))
	charts_per_page_ <- as.integer(tclObj(control_charts_per_page))
	do_landscape_ <- as.logical(tclObj(control_landscape))
	rtdf_dir_ <- paste(tclObj(Rtdf_dir),sep="",collapse=" ")
	param_name_dir_ <- paste(tclObj(param_name_dir),sep="",collapse=" ")
	control_autoopen_ <- as.logical(tclObj(control_autoopen))

	output_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	if (nchar(output_dir)<1) {
		output_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}

	if (nchar(output_dir)>0)  setwd(output_dir)

	my_expr = substitute(
		ControlCharts(rtdf_name=rtdf_name_,pdf_name=pdf_name_,
					param_name=param_name_,title=title_,
					do_western_electric=do_western_electric_,
					start_n=start_n_,count_n=count_n_,
					charts_per_page=charts_per_page_,
					do_landscape=do_landscape_,rtdf_dir=rtdf_dir_,
					param_dir=param_name_dir_)
	)
	tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
	tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
	my_command = sprintf("%s\n",deparse(my_expr))
	my_cmnd = "ControlCharts(...)\n"
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
	if(control_autoopen_) {
		if (as.character(Sys.info()["sysname"])=="Windows") {
			os_com = "cmd /c start"
		} else if (as.character(Sys.info()["sysname"])=="Darwin") { # aka Mac
			os_com = "open"
		} else {	# linux ...
			os_com = "xdg-open"
		}
		command_str = paste(os_com,pdf_name_)
		system(command_str)
	}

	if(done>0) {
		controlcharts_win <- get("controlcharts_win",envir=.TkRadar.wins)
		tkdestroy(controlcharts_win)
	}
}


#-----------------------------------------------------
ControlChartsGui <-function() {

	ControlChartsGui_defaults()
	controlcharts_win <- tktoplevel()
	assign("controlcharts_win",controlcharts_win,envir=.TkRadar.wins)
	tkwm.title(controlcharts_win, "ControlCharts")

	bottom_row <- tkframe(controlcharts_win)
	default_button <- tkbutton(bottom_row,
						text="DEFAULTS",
						#anchor="w",
						width=12,
						command=ControlChartsGui_defaults)
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
						text="RUN & QUIT",
						#anchor="w",
						width=12,
						command=function() run_ControlCharts(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
						text="QUIT",
						#anchor="w",
						width=12,
						command=function() tkdestroy(controlcharts_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
						text="RUN",
						#anchor="w",
						width=12,
						command=function() run_ControlCharts(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	dir_entry_frame <- tkframe(controlcharts_win)
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

	rtdf_entry_frame <- tkframe(controlcharts_win)
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

	pdf_entry_frame <- tkframe(controlcharts_win)
	pdf_entry_label <- tklabel(pdf_entry_frame,
						width=10,
						text="pdf_name ")
	tkpack(pdf_entry_label,side="left")
	pdf_entry <- tkentry(pdf_entry_frame,
						width=20,
						textvariable=controlcharts_pdf_name)
	tkpack(pdf_entry,side="left",fill="x",expand=1)
	pdf_browse <- tkbutton(pdf_entry_frame,
						text="Browse",
						command=function() pdf_browser(controlcharts_pdf_name))
	tkpack(pdf_browse,side="right")
	tkpack(pdf_entry_frame,side="top",anchor="w",fill="x")

	param_entry_frame <- tkframe(controlcharts_win)
	param_entry_label <- tklabel(param_entry_frame,
						width=10,
						text="param_name")
	tkpack(param_entry_label,side="left")
	param_entry <- tkentry(param_entry_frame,
						width=20,
						textvariable=param_name)
	tkpack(param_entry,side="left",fill="x",expand=1)
	param_browse <- tkbutton(param_entry_frame,
						text="Browse",
						command=function() rtdf_browser(param_name,param_name_dir))
	tkpack(param_browse,side="right")
	tkpack(param_entry_frame,side="top",anchor="w",fill="x")

	title_frame <- tkframe(controlcharts_win)
	title_label <- tklabel(title_frame,
						width=10,
						text="title")
	tkpack(title_label,side="left")
	title_entry <- tkentry(title_frame,
						width=20,
						textvariable=controlcharts_title)
	tkpack(title_entry,side="left",fill="x",expand=1)
	tkpack(title_frame,side="top",anchor="w",fill="x")

	wer_button <- tkcheckbutton(controlcharts_win,
						text="do_western_electric",
						variable=do_western_electric)
	tkpack(wer_button,side="top",anchor="w")


	ctrl_start_frame <- tkframe(controlcharts_win)
	ctrl_start_label <- tklabel(ctrl_start_frame,
						width=10,
						text="start_n")
	tkpack(ctrl_start_label,side="left")
	ctrl_start_entry <- tklabel(ctrl_start_frame,
						width=10,
						relief="sunken",
						textvariable=control_start_n)
	tkpack(ctrl_start_entry,side="left",fill="x",expand=1)
	ctrl_start_browse <- tkbutton(ctrl_start_frame,
						text="Edit",
						command=function() index_entry(control_start_n))
	tkpack(ctrl_start_browse,side="right")
	tkpack(ctrl_start_frame,side="top",anchor="w")

	ctrl_count_frame <- tkframe(controlcharts_win)
	ctrl_count_label <- tklabel(ctrl_count_frame,
						width=10,
						text="count_n")
	tkpack(ctrl_count_label,side="left")
	ctrl_count_entry <- tklabel(ctrl_count_frame,
						width=10,
						relief="sunken",
						textvariable=control_count_n)
	tkpack(ctrl_count_entry,side="left",fill="x",expand=1)
	ctrl_count_browse <- tkbutton(ctrl_count_frame,
						text="Edit",
						command=function() index_entry(control_count_n))
	tkpack(ctrl_count_browse,side="right")
	tkpack(ctrl_count_frame,side="top",anchor="w")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	charts_p_page_frame <- tkframe(controlcharts_win)
	charts_pp_label <- tklabel(charts_p_page_frame,
						#width=10,
						text="charts per page")
	tkpack(charts_pp_label,side="left")
	charts_pp_value <- tklabel(charts_p_page_frame,
						width=3,
						relief="sunken",
						textvariable=control_charts_per_page)
	tkpack(charts_pp_value,side="left")
	charts_pp_plus <- tkbutton(charts_p_page_frame,
						text="+",
						command=inc_charts_p_page)
	tkpack(charts_pp_plus,side="left")
	charts_pp_minus <- tkbutton(charts_p_page_frame,
						text="-",
						command=dec_charts_p_page)
	tkpack(charts_pp_minus,side="left")
	tkpack(charts_p_page_frame,side="top",anchor="w")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	chart_land_button <- tkcheckbutton(controlcharts_win,
						text="do_landscape",
						variable=control_landscape)
	tkpack(chart_land_button,side="top",anchor="w")

	autoopen_button <- tkcheckbutton(controlcharts_win,
						text="auto_open_pdf",
						variable=control_autoopen)
	tkpack(autoopen_button,side="bottom",anchor="w")

}
