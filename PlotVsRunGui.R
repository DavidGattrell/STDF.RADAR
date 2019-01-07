# PlotVsRunGui.R
#
# $Id: PlotVsRunGui.R,v 1.2 2013/02/19 02:16:51 david Exp $
#
# Tk/Tcl GUI wrapper for calling PlotVrRun.R
# called by TkRadar.R
#
# Copyright (C) 2012-2013 David Gattrell
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
# PlotVsRun gui specific variables
#--------------------------------
if(!exists(".TkRadar.env")) .TkRadar.env <- new.env()

pvr_datasets <- tclVar(1)
pvr_param_name <- tclVar("")
pvr_title <- tclVar("PlotVsRun output")
pvr_auto_scale <- tclVar(1)
pvr_do_robust_stats <- tclVar(0)
pvr_alt_limits <- tclVar("")
pvr_min_plots_per_page <- tclVar(4)
pvr_plot_using_limits_int <- tclVar(3)
pvr_plot_using_limits_frac <- tclVar("0.2")
pvr_collapse_MPRs <- tclVar(0)
pvr_pdf_name <- tclVar("my_data_vs_run.pdf")
pvr_landscape <- tclVar(0)
pvr_autoopen <- tclVar(0)
pvr_to_png <- tclVar(0)

pvr_dataset_frames <- list()	# populated when window packed...
pvr_rtdf_names <- list()
pvr_rtdf_dirs <- list()
pvr_dataset_names <- list()
pvr_use_alt_limits <- list()
for (j in 1:20) {
	pvr_rtdf_names[[j]] <- tclVar("")
	pvr_rtdf_dirs[[j]] <- tclVar("")
	pvr_dataset_names[[j]] <- tclVar("")
	pvr_use_alt_limits[[j]] <- tclVar(0)
}

pvr_param_name_dir <- tclVar("")
pvr_alt_limits_dir <- tclVar("")

# these defaults can be controlled in the .Rprofile file:
default_pvr_min_plots_per_page <- tclVar(4)		# per user customizing
default_pvr_plot_using_limits <- tclVar("3.2")	# per user customizing
default_pvr_do_robust_stats <- tclVar(0)		# per user customizing
default_pvr_landscape <- tclVar(1)				# per user customizing
default_pvr_autoopen <- tclVar(1)				# per user customizing
default_pvr_to_png <- tclVar(0)

rm(j)

#-----------------------------------------------------
PlotVsRunGui_defaults <- function(...) {
	tclvalue(pvr_pdf_name) <- "my_data_vs_run.pdf"
	tclvalue(pvr_param_name) <- ""
	tclvalue(pvr_title) <- "PlotVsRun output"
	tclvalue(pvr_auto_scale) <- 1
	tclvalue(pvr_do_robust_stats) <- tclObj(default_pvr_do_robust_stats)
	tclvalue(pvr_alt_limits) <- ""
	tclvalue(pvr_min_plots_per_page) <- tclObj(default_pvr_min_plots_per_page)
	pvr_plot_using_limits <- as.numeric(tclObj(default_pvr_plot_using_limits))
	tclvalue(pvr_plot_using_limits_int) <- as.integer(pvr_plot_using_limits)
	limits_frac = pvr_plot_using_limits - as.integer(pvr_plot_using_limits)
	if(limits_frac<0.05) {
		limits_frac = "0.0"
	} else if(limits_frac<0.15) {
		limits_frac = "0.1"
	} else if(limits_frac<0.25) {
		limits_frac = "0.2"
	} else { 
		limits_frac = "0.3"
	}
	tclvalue(pvr_plot_using_limits_frac) <- limits_frac
	tclvalue(pvr_collapse_MPRs) <- 0
	tclvalue(pvr_landscape) <- tclObj(default_pvr_landscape)
	tclvalue(pvr_autoopen) <- tclObj(default_pvr_autoopen)
	tclvalue(pvr_to_png) <- tclObj(default_pvr_to_png)


	for (j in 1:20) {
		if (j<2) {
			pvr_rtdf_name <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
			pvr_rtdf_dir <- paste(tclObj(Rtdf_dir),sep="",collapse=" ")
			tclvalue(pvr_rtdf_names[[j]]) <- pvr_rtdf_name
			tclvalue(pvr_rtdf_dirs[[j]]) <- pvr_rtdf_dir
		} else {
			tclvalue(pvr_rtdf_names[[j]]) <- ""
			tclvalue(pvr_rtdf_dirs[[j]]) <- ""
		}
		tclvalue(pvr_dataset_names[[j]]) <- ""
		tclvalue(pvr_use_alt_limits[[j]]) <- 0
	}

	tclvalue(pvr_param_name_dir) <- ""
	tclvalue(pvr_alt_limits_dir) <- ""

	my_value <- as.integer(tclObj(pvr_datasets))
	if(my_value>1) {
		for (j in my_value:2) {
			tkpack.forget(pvr_dataset_frames[[j]])
		}
	}
	tclvalue(pvr_datasets) <- 1

}

#-----------------------------------------------------
pvr_inc_datasets <- function() {

	my_value <- as.integer(tclObj(pvr_datasets))
	if (my_value<20) {
		my_value <- my_value + 1
		tkpack(pvr_dataset_frames[[my_value]],side="top",anchor="w",fill="x")
	}
	tclvalue(pvr_datasets) <- my_value
}

#-----------------------------------------------------
pvr_dec_datasets <- function() {

	my_value <- as.integer(tclObj(pvr_datasets))
	if (my_value>1) {
		tkpack.forget(pvr_dataset_frames[[my_value]])
		my_value <- my_value - 1
	}
	tclvalue(pvr_datasets) <- my_value
}


#-----------------------------------------------------
pvr_inc_plots_p_page <- function() {

	my_value <- as.integer(tclObj(pvr_min_plots_per_page))
	if (my_value<20)  my_value <- my_value + 1
	tclvalue(pvr_min_plots_per_page) <- my_value
}

#-----------------------------------------------------
pvr_dec_plots_p_page <- function() {

	my_value <- as.integer(tclObj(pvr_min_plots_per_page))
	if (my_value>1)  my_value <- my_value - 1
	tclvalue(pvr_min_plots_per_page) <- my_value
}


#-----------------------------------------------------
run_PlotVsRun <-function(done=FALSE,...) {

	pdf_name_ <- paste(tclObj(pvr_pdf_name),sep="",collapse=" ")
	param_name_ <- paste(tclObj(pvr_param_name),sep="",collapse=" ")
	alt_limits_ <- paste(tclObj(pvr_alt_limits),sep="",collapse=" ")
	pvr_title_ <- paste(tclObj(pvr_title),sep="",collapse=" ")
	param_name_dir_ <- paste(tclObj(pvr_param_name_dir),sep="",collapse=" ")
	alt_limits_dir_ <- paste(tclObj(pvr_alt_limits_dir),sep="",collapse=" ")

	auto_scale_ <- as.logical(tclObj(pvr_auto_scale))
	do_robust_stats_ <- as.integer(tclObj(pvr_do_robust_stats))
	plot_using_limits_ <- as.integer(tclObj(pvr_plot_using_limits_int)) +
						as.numeric(tclObj(pvr_plot_using_limits_frac))
	collapse_MPRs_ <- as.logical(tclObj(pvr_collapse_MPRs))
	do_landscape_ <- as.logical(tclObj(pvr_landscape))
	pvr_autoopen_ <- as.logical(tclObj(pvr_autoopen))
	to_png_ <- as.logical(tclObj(pvr_to_png))

	min_plots_per_page_ <- as.integer(tclObj(pvr_min_plots_per_page))
	
	# vector stuff...
	# datasets, use_alt_limits, ...
	datasets_ <- as.integer(tclObj(pvr_datasets))
	rtdf_names_ <- vector()
	rtdf_dirs_ <- vector()
	dataset_names_ <- vector()
	use_alt_limits_ <- vector()
	for (j in 1:datasets_) {
		rtdf_names_[j] <- paste(tclObj(pvr_rtdf_names[[j]]),sep="",collapse=" ")
		rtdf_dirs_[j] <- paste(tclObj(pvr_rtdf_dirs[[j]]),sep="",collapse=" ")
		dataset_names_[j] <- paste(tclObj(pvr_dataset_names[[j]]),sep="",collapse=" ")
		use_alt_limits_[j] <- as.logical(tclObj(pvr_use_alt_limits[[j]]))
	}

	output_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	full_path = output_dir
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	setwd(full_path)


	my_expr = substitute(
		PlotVsRun(rtdf_name=rtdf_names_,pdf_name=pdf_name_,
					param_name=param_name_,dataset_name=dataset_names_,
					title=pvr_title_,auto_scale=auto_scale_,
					do_robust_stats=do_robust_stats_,
					alt_limits=alt_limits_,use_alt_lims=use_alt_limits_,
					min_plots_per_page=min_plots_per_page_,
					plot_using_limits=plot_using_limits_,
					collapse_MPRs=collapse_MPRs_,rtdf_dirs=rtdf_dirs_,
					param_dir=param_name_dir_,alt_lim_dir=alt_limits_dir_,
					do_landscape=do_landscape_,to_png=to_png_)
	)
	tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
	tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
	my_command = sprintf("%s\n",deparse(my_expr))
	my_cmnd = "PlotVsRun(...)\n"
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
	if(pvr_autoopen_ && !to_png_) {
		if (as.character(Sys.info()["sysname"])=="Windows") {
			os_com = "cmd /c start"
		} else if (as.character(Sys.info()["sysname"])=="Darwin") { # aka Mac
			os_com = "open"
		} else {  # linux, etc...
			os_com = "xdg-open"
		}
		command_str = paste(os_com,pdf_name_)
		system(command_str)
	}

	if(done>0) {
		plotvsrun_win <- get("plotvsrun_win",envir=.TkRadar.wins)
		tkdestroy(plotvsrun_win)
	}

}



#-----------------------------------------------------
PlotVsRunGui <-function() {
	
	PlotVsRunGui_defaults()
	plotvsrun_win <- tktoplevel()
	assign("plotvsrun_win",plotvsrun_win,envir=.TkRadar.wins)
	tkwm.title(plotvsrun_win, "PlotVsRun")

	bottom_row <- tkframe(plotvsrun_win)
	default_button <- tkbutton(bottom_row,
						text="DEFAULTS",
						#anchor="w",
						width=12,
						command=PlotVsRunGui_defaults)
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
						text="RUN & QUIT",
						#anchor="w",
						width=12,
						command=function() run_PlotVsRun(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
						text="QUIT",
						#anchor="w",
						width=12,
						command=function() tkdestroy(plotvsrun_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
						text="RUN",
						#anchor="w",
						width=12,
						command=function() run_PlotVsRun(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	dir_entry_frame <- tkframe(plotvsrun_win)
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

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	indir_entry_frame <- tkframe(plotvsrun_win)
	indir_entry_label <- tklabel(indir_entry_frame,
						width=10,
						text="in rtdf dir")
	tkpack(indir_entry_label,side="left")
	indir_entry <- tklabel(indir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=Rtdfs_dir)
	tkpack(indir_entry,side="left",fill="x",expand=1)
	indir_browse <- tkbutton(indir_entry_frame,
						text="Browse",
						command=function() dir_browser(Rtdfs_dir))
	tkpack(indir_browse,side="right")
	tkpack(indir_entry_frame,side="top",anchor="w",fill="x")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	dataset_frame <- tkframe(plotvsrun_win)
	dataset_label <- tklabel(dataset_frame,
						#width=10,
						text="number of datasets")
	tkpack(dataset_label,side="left")
	datasets_value <- tklabel(dataset_frame,
						width=3,
						relief="sunken",
						textvariable=pvr_datasets)
	tkpack(datasets_value,side="left")
	dataset_plus <- tkbutton(dataset_frame,
						text="+",
						command=pvr_inc_datasets)
	tkpack(dataset_plus,side="left")
	dataset_minus <- tkbutton(dataset_frame,
						text="-",
						command=pvr_dec_datasets)
	tkpack(dataset_minus,side="left")
	tkpack(dataset_frame,side="top",anchor="w",fill="x")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	plots_p_page_frame <- tkframe(plotvsrun_win)
	plots_pp_label <- tklabel(plots_p_page_frame,
						#width=10,
						text="min plots per page")
	tkpack(plots_pp_label,side="left")
	plots_pp_value <- tklabel(plots_p_page_frame,
						width=3,
						relief="sunken",
						textvariable=pvr_min_plots_per_page)
	tkpack(plots_pp_value,side="left")
	plots_pp_plus <- tkbutton(plots_p_page_frame,
						text="+",
						command=pvr_inc_plots_p_page)
	tkpack(plots_pp_plus,side="left")
	plots_pp_minus <- tkbutton(plots_p_page_frame,
						text="-",
						command=pvr_dec_plots_p_page)
	tkpack(plots_pp_minus,side="left")
	tkpack(plots_p_page_frame,side="bottom",anchor="w")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	checkboxes_frame <- tkframe(plotvsrun_win)

	left_frame <- tkframe(checkboxes_frame)
	right_frame <- tkframe(checkboxes_frame)


	to_png_button <- tkcheckbutton(right_frame,
						text="to_png  (disables pdf output!)",
						variable=pvr_to_png)
	tkpack(to_png_button,side="bottom",anchor="w")

	autoopen_button <- tkcheckbutton(right_frame,
						text="auto_open_pdf",
						variable=pvr_autoopen)
	tkpack(autoopen_button,side="bottom",anchor="w")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mprs_button <- tkcheckbutton(left_frame,
						text="collapse_MPRs",
						variable=pvr_collapse_MPRs)
	tkpack(mprs_button,side="bottom",anchor="w")

	do_autoscale_button <- tkcheckbutton(left_frame,
						text="auto_scale",
						variable=pvr_auto_scale)
	tkpack(do_autoscale_button,side="bottom",anchor="w")

	do_landscape_button <- tkcheckbutton(left_frame,
						text="do_landscape          ",
						variable=pvr_landscape)
	tkpack(do_landscape_button,side="bottom",anchor="w")

	tkpack(left_frame,side="left",anchor="n")
	tkpack(right_frame,side="left",anchor="n")
	tkpack(checkboxes_frame,side="bottom",anchor="w")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	limits_int_radio_frame <- tkframe(plotvsrun_win)
	limits_int_label <- tklabel(limits_int_radio_frame, text="plot to:")
	tkpack(limits_int_label,side="left")

	lmt_int_radio0 <- tkradiobutton(limits_int_radio_frame,
						text="pop min/max",
						value="0",
						variable=pvr_plot_using_limits_int)
	tkpack(lmt_int_radio0,side="left")
	
	lmt_int_radio1 <- tkradiobutton(limits_int_radio_frame,
						text="limits",
						value="1",
						variable=pvr_plot_using_limits_int)
	tkpack(lmt_int_radio1,side="left")
	
	lmt_int_radio2 <- tkradiobutton(limits_int_radio_frame,
						text="tightest pop or limit",
						value="2",
						variable=pvr_plot_using_limits_int)
	tkpack(lmt_int_radio2,side="left")
	
	lmt_int_radio3 <- tkradiobutton(limits_int_radio_frame,
						text="limit or limit+%",
						value="3",
						variable=pvr_plot_using_limits_int)
	tkpack(lmt_int_radio3,side="left")
	tkpack(limits_int_radio_frame,side="bottom",anchor="w",fill="x")


	limits_frac_radio_frame <- tkframe(plotvsrun_win)
	limits_frac_label <- tklabel(limits_frac_radio_frame, text="plot range +:")
	tkpack(limits_frac_label,side="left")

	lmt_frac_radio0 <- tkradiobutton(limits_frac_radio_frame,
						text="0%",
						value="0.0",
						variable=pvr_plot_using_limits_frac)
	tkpack(lmt_frac_radio0,side="left")
	
	lmt_frac_radio1 <- tkradiobutton(limits_frac_radio_frame,
						text="10%",
						value="0.1",
						variable=pvr_plot_using_limits_frac)
	tkpack(lmt_frac_radio1,side="left")
	
	lmt_frac_radio2 <- tkradiobutton(limits_frac_radio_frame,
						text="20%",
						value="0.2",
						variable=pvr_plot_using_limits_frac)
	tkpack(lmt_frac_radio2,side="left")

	lmt_frac_radio3 <- tkradiobutton(limits_frac_radio_frame,
						text="30%",
						value="0.3",
						variable=pvr_plot_using_limits_frac)
	tkpack(lmt_frac_radio3,side="left")
	tkpack(limits_frac_radio_frame,side="bottom",anchor="w",fill="x")
	
	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	radio_buttons_frame <- tkframe(plotvsrun_win)
	
	radio0 <- tkradiobutton(radio_buttons_frame,
						text="use mean/sdev",
						value="0",
						variable=pvr_do_robust_stats)
	tkpack(radio0,side="left")
	radio1 <- tkradiobutton(radio_buttons_frame,
						text="use robust mean/sdev",
						value="1",
						variable=pvr_do_robust_stats)
	tkpack(radio1,side="left")
	radio2 <- tkradiobutton(radio_buttons_frame,
						text="use 2 sided robust mean/sdev",
						value="2",
						variable=pvr_do_robust_stats)
	tkpack(radio2,side="left")
	tkpack(radio_buttons_frame,side="bottom",anchor="w",fill="x")
	

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	title_frame <- tkframe(plotvsrun_win)
	title_label <- tklabel(title_frame,
						width=10,
						text="title")
	tkpack(title_label,side="left")
	title_entry <- tkentry(title_frame,
						width=20,
						textvariable=pvr_title)
	tkpack(title_entry,side="left",fill="x",expand=1)
	tkpack(title_frame,side="bottom",anchor="w",fill="x")

	alt_limit_frame <- tkframe(plotvsrun_win)
	alt_limit_label <- tklabel(alt_limit_frame,
						width=10,
						text="alt_limits")
	tkpack(alt_limit_label,side="left")
	alt_limit_entry <- tkentry(alt_limit_frame,
						width=20,
						textvariable=pvr_alt_limits)
	tkpack(alt_limit_entry,side="left",fill="x",expand=1)
	alt_limit_browse <- tkbutton(alt_limit_frame,
						text="Browse",
						command=function() rtdf_browser(pvr_alt_limits,pvr_alt_limits_dir))
	tkpack(alt_limit_browse,side="right")
	tkpack(alt_limit_frame,side="bottom",anchor="w",fill="x")

	param_entry_frame <- tkframe(plotvsrun_win)
	param_entry_label <- tklabel(param_entry_frame,
						width=10,
						text="param_name")
	tkpack(param_entry_label,side="left")
	param_entry <- tkentry(param_entry_frame,
						width=20,
						textvariable=pvr_param_name)
	tkpack(param_entry,side="left",fill="x",expand=1)
	param_browse <- tkbutton(param_entry_frame,
						text="Browse",
						command=function() rtdf_browser(pvr_param_name,pvr_param_name_dir))
	tkpack(param_browse,side="right")
	tkpack(param_entry_frame,side="bottom",anchor="w",fill="x")

	pdf_entry_frame <- tkframe(plotvsrun_win)
	pdf_entry_label <- tklabel(pdf_entry_frame,
						width=10,
						text="pdf_name ")
	tkpack(pdf_entry_label,side="left")
	pdf_entry <- tkentry(pdf_entry_frame,
						width=20,
						textvariable=pvr_pdf_name)
	tkpack(pdf_entry,side="left",fill="x",expand=1)
	pdf_browse <- tkbutton(pdf_entry_frame,
						text="Browse",
						command=function() pdf_browser(pvr_pdf_name))
	tkpack(pdf_browse,side="right")
	tkpack(pdf_entry_frame,side="bottom",anchor="w",fill="x")	

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	for (j in 1:20) {
		dataset_frame <- tkframe(plotvsrun_win)
		my_text = as.character(j)
		rtdf_entry_label <- tklabel(dataset_frame,
						width=5,
						anchor="e",
						text=my_text)
		tkpack(rtdf_entry_label,side="left")
		rtdf_entry <- tkentry(dataset_frame,
						textvariable=pvr_rtdf_names[[j]])
		tkpack(rtdf_entry,side="left",fill="x",expand=1)
		if (j==1) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(pvr_rtdf_names[[1]],pvr_rtdf_dirs[[1]]))
		} else if (j==2) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(pvr_rtdf_names[[2]],pvr_rtdf_dirs[[2]]))
		} else if (j==3) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(pvr_rtdf_names[[3]],pvr_rtdf_dirs[[3]]))
		} else if (j==4) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(pvr_rtdf_names[[4]],pvr_rtdf_dirs[[4]]))
		} else if (j==5) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(pvr_rtdf_names[[5]],pvr_rtdf_dirs[[5]]))
		} else if (j==6) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(pvr_rtdf_names[[6]],pvr_rtdf_dirs[[6]]))
		} else if (j==7) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(pvr_rtdf_names[[7]],pvr_rtdf_dirs[[7]]))
		} else if (j==8) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(pvr_rtdf_names[[8]],pvr_rtdf_dirs[[8]]))
		} else if (j==9) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(pvr_rtdf_names[[9]],pvr_rtdf_dirs[[9]]))
		} else if (j==10) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(pvr_rtdf_names[[10]],pvr_rtdf_dirs[[10]]))
		} else if (j==11) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(pvr_rtdf_names[[11]],pvr_rtdf_dirs[[11]]))
		} else if (j==12) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(pvr_rtdf_names[[12]],pvr_rtdf_dirs[[12]]))
		} else if (j==13) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(pvr_rtdf_names[[13]],pvr_rtdf_dirs[[13]]))
		} else if (j==14) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(pvr_rtdf_names[[14]],pvr_rtdf_dirs[[14]]))
		} else if (j==15) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(pvr_rtdf_names[[15]],pvr_rtdf_dirs[[15]]))
		} else if (j==16) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(pvr_rtdf_names[[16]],pvr_rtdf_dirs[[16]]))
		} else if (j==17) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(pvr_rtdf_names[[17]],pvr_rtdf_dirs[[17]]))
		} else if (j==18) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(pvr_rtdf_names[[18]],pvr_rtdf_dirs[[18]]))
		} else if (j==19) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(pvr_rtdf_names[[19]],pvr_rtdf_dirs[[19]]))
		} else if (j==20) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(pvr_rtdf_names[[20]],pvr_rtdf_dirs[[20]]))
		} 
		tkpack(rtdf_browse,side="left")

		name_entry <- tkentry(dataset_frame,
						width=20,
						textvariable=pvr_dataset_names[[j]])
		tkpack(name_entry,side="left")

		altlim_ckbox <- tkcheckbutton(dataset_frame,
						text="use_alt_lim",
						variable=use_alt_limits[[j]])
		tkpack(altlim_ckbox,side="left")

		pvr_dataset_frames[[j]] <<- dataset_frame
	}

	my_sets <- as.integer(tclObj(pvr_datasets))

	for (j in 1:my_sets)  tkpack(pvr_dataset_frames[[j]],side="top",anchor="w",fill="x")


}


