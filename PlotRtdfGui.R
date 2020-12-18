# PlotRtdfGui.R
#
# $Id: PlotRtdfGui.R,v 1.17 2020/12/18 01:23:41 david Exp $
#
# Tk/Tcl GUI wrapper for calling PlotRtdf.R
# called by TkRadar.R
#
# Copyright (C) 2008-2016 David Gattrell
#               2010 Vincent Horng
#				2016 Cristian Miortescu
#				2019 David Gattrell
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
# PlotRtdf gui specific variables
#--------------------------------
if(!exists(".TkRadar.env")) .TkRadar.env <- new.env()

datasets <- tclVar(1)
param_name <- tclVar("")
do_xy_plots <- tclVar(0)
do_hist_and_xy <- tclVar(0)
plotrtdf_title <- tclVar("PlotRtdf output")
auto_scale <- tclVar(1)
save_workspace_to <- tclVar("")
do_csv <- tclVar(1)
do_robust_stats <- tclVar(0)
alt_limits <- tclVar("")
alt2_limits <- tclVar("")
min_plots_per_page <- tclVar(6)
do_guardbands <- tclVar(0)
use_csv_formulas <- tclVar(1)
use_OOCalc_csv <- tclVar(1)
add_normal_curve <- tclVar(0)
plot_using_test_limits <- tclVar(1)
plot_widest_limits_or_values <- tclVar(0)
collapse_MPRs <- tclVar(0)
plotrtdf_pdf_name <- tclVar("my_histograms.pdf")
plotrtdf_autoopen <- tclVar(0)
superimpose_hist <- tclVar(0)
just_superimposed_histo <- tclVar(0)
do_norm_prob_plots <- tclVar(0)
plotrtdf_to_png <- tclVar(0)
plot_max_tests <- tclVar(2000)
plot_max_tests_shadow <- tclVar(2000)
plot_limits_plus_10pct <- tclVar(0)
outside_limits_count <- tclVar(0)

dataset_frames <- list()	# populated when window packed...
rtdf_names <- list()
rtdf_dirs <- list()
dataset_names <- list()
use_alt_limits <- list()
for (j in 1:20) {
	rtdf_names[[j]] <- tclVar("")
	rtdf_dirs[[j]] <- tclVar("")
	dataset_names[[j]] <- tclVar("")
	use_alt_limits[[j]] <- tclVar(0)
}

param_name_dir <- tclVar("")
alt_limits_dir <- tclVar("")

# these defaults can be controlled in the .Rprofile file:
default_min_plots_per_page <- tclVar(6)		# per user customizing
default_use_csv_formulas <- tclVar(1)		# per user customizing
default_use_OOCalc_csv <- tclVar(1)			# per user customizing
default_add_normal_curve <- tclVar(0)		# per user customizing
default_do_robust_stats <- tclVar(0)		# per user customizing
default_plotrtdf_autoopen <- tclVar(1)		# per user customizing
default_superimpose_hist <- tclVar(0)		# per user customizing
default_just_superimposed_histo <- tclVar(0)	# per user customizing
default_do_norm_prob_plots <- tclVar(0)
default_plotrtdf_to_png <- tclVar(0)
default_plot_max_tests <- tclVar(2000)
default_plot_limits_plus_10pct <- tclVar(0)
default_outside_limits_count <- tclVar(0)

rm(j)

#-----------------------------------------------------
PlotRtdfGui_defaults <- function(...) {
#	tclvalue(plotrtdf_pdf_name) <- "my_histograms.pdf"
	tclvalue(param_name) <- ""
	tclvalue(do_xy_plots) <- 0
	tclvalue(do_hist_and_xy) <- 0
	tclvalue(plotrtdf_title) <- "PlotRtdf output"
	tclvalue(auto_scale) <- 1
	tclvalue(save_workspace_to) <- ""
	tclvalue(do_csv) <- 1
	tclvalue(do_robust_stats) <- tclObj(default_do_robust_stats)
	tclvalue(alt_limits) <- ""
	tclvalue(alt2_limits) <- ""
	tclvalue(min_plots_per_page) <- tclObj(default_min_plots_per_page)
	tclvalue(do_guardbands) <- 0
	tclvalue(use_csv_formulas) <- tclObj(default_use_csv_formulas)
	tclvalue(use_OOCalc_csv) <- tclObj(default_use_OOCalc_csv)
	tclvalue(add_normal_curve) <- tclObj(default_add_normal_curve)
	tclvalue(plot_using_test_limits) <- 1
	tclvalue(plot_widest_limits_or_values) <- 0
	tclvalue(collapse_MPRs) <- 0
	tclvalue(plotrtdf_autoopen) <- tclObj(default_plotrtdf_autoopen)
	tclvalue(superimpose_hist) <- tclObj(default_superimpose_hist)
	tclvalue(just_superimposed_histo) <- tclObj(default_just_superimposed_histo)
	tclvalue(do_norm_prob_plots) <- tclObj(default_do_norm_prob_plots)
	tclvalue(plotrtdf_to_png) <- tclObj(default_plotrtdf_to_png)
	tclvalue(plot_max_tests) <- tclObj(default_plot_max_tests)
	tclvalue(plot_max_tests_shadow) <- tclObj(default_plot_max_tests)
	tclvalue(plot_limits_plus_10pct) <- tclObj(default_plot_limits_plus_10pct)
	tclvalue(outside_limits_count) <- tclObj(default_outside_limits_count)



	for (j in 1:20) {
		if (j<2) {
			rtdf_name <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
			rtdf_dir <- paste(tclObj(Rtdf_dir),sep="",collapse=" ")
			tclvalue(rtdf_names[[j]]) <- rtdf_name
			tclvalue(rtdf_dirs[[j]]) <- rtdf_dir
		} else {
			tclvalue(rtdf_names[[j]]) <- ""
			tclvalue(rtdf_dirs[[j]]) <- ""
		}
		tclvalue(dataset_names[[j]]) <- ""
		tclvalue(use_alt_limits[[j]]) <- 0
	}

	tclvalue(param_name_dir) <- ""
	tclvalue(alt_limits_dir) <- ""

	my_value <- as.integer(tclObj(datasets))
	if(my_value>1) {
		for (j in my_value:2) {
			tkpack.forget(dataset_frames[[j]])
		}
	}
	tclvalue(datasets) <- 1

	if( rtdf_name=="") {
		tclvalue(plotrtdf_pdf_name) <- "my_histograms.pdf"
	} else {
		tclvalue(plotrtdf_pdf_name) <- sub(".rtdf?$",".pdf",rtdf_name)
	}

}

#-----------------------------------------------------
workspace_browser <- function() {
	# use directory from pdf_name...
	# don't allow directory to be changed
	# file type is .Rdata
	
}

#-----------------------------------------------------
inc_datasets <- function() {

	my_value <- as.integer(tclObj(datasets))
	if (my_value<20) {
		my_value <- my_value + 1
		tkpack(dataset_frames[[my_value]],side="top",anchor="w",fill="x")
	}
	tclvalue(datasets) <- my_value

		tclvalue(plotrtdf_pdf_name) <- "my_multiple_files_histograms.pdf"
}

#-----------------------------------------------------
dec_datasets <- function() {

	my_value <- as.integer(tclObj(datasets))
	if (my_value>1) {
		tkpack.forget(dataset_frames[[my_value]])
		my_value <- my_value - 1
	}
	if (my_value==1) {
		rtdf_name <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
		if( rtdf_name=="") {
			tclvalue(plotrtdf_pdf_name) <- "my_histograms.pdf"
		} else {
			tclvalue(plotrtdf_pdf_name) <- sub(".rtdf?$",".pdf",rtdf_name)
		}
	}
	tclvalue(datasets) <- my_value
}


#-----------------------------------------------------
inc_plots_p_page <- function() {

	my_value <- as.integer(tclObj(min_plots_per_page))
	if (my_value<20)  my_value <- my_value + 1
	tclvalue(min_plots_per_page) <- my_value
}

#-----------------------------------------------------
dec_plots_p_page <- function() {

	my_value <- as.integer(tclObj(min_plots_per_page))
	if (my_value>1)  my_value <- my_value - 1
	tclvalue(min_plots_per_page) <- my_value
}


#-----------------------------------------------------
run_PlotRtdf <-function(done=FALSE,...) {

	pdf_name_ <- paste(tclObj(plotrtdf_pdf_name),sep="",collapse=" ")
	param_name_ <- paste(tclObj(param_name),sep="",collapse=" ")
	alt_limits_ <- paste(tclObj(alt_limits),sep="",collapse=" ")
	alt2_limits_ <- paste(tclObj(alt2_limits),sep="",collapse=" ")
	if( (alt_limits_ != "") && (alt2_limits_ != "") ) {
		alt_limits_[2] = alt2_limits_
	}
	plotrtdf_title_ <- paste(tclObj(plotrtdf_title),sep="",collapse=" ")
	save_workspace_to_ <- paste(tclObj(save_workspace_to),sep="",collapse=" ")
	param_name_dir_ <- paste(tclObj(param_name_dir),sep="",collapse=" ")
	alt_limits_dir_ <- paste(tclObj(alt_limits_dir),sep="",collapse=" ")

	do_xy_plots_ <- as.logical(tclObj(do_xy_plots))
	do_hist_and_xy_ <- as.logical(tclObj(do_hist_and_xy))
	auto_scale_ <- as.logical(tclObj(auto_scale))
	do_csv_ <- as.logical(tclObj(do_csv))
	do_robust_stats_ <- as.integer(tclObj(do_robust_stats))
	do_guardbands_ <- as.logical(tclObj(do_guardbands))
	use_csv_formulas_ <- as.logical(tclObj(use_csv_formulas))
	use_OOCalc_csv_ <- as.logical(tclObj(use_OOCalc_csv))
	add_normal_curve_ <- as.logical(tclObj(add_normal_curve))
	plot_using_test_limits_ <- as.logical(tclObj(plot_using_test_limits))
	plot_widest_limits_or_values_ <- as.logical(tclObj(plot_widest_limits_or_values))
	collapse_MPRs_ <- as.logical(tclObj(collapse_MPRs))
	plotrtdf_autoopen_ <- as.logical(tclObj(plotrtdf_autoopen))
	superimpose_hist_ <- as.logical(tclObj(superimpose_hist))
	just_superimposed_histo_ <- as.logical(tclObj(just_superimposed_histo))
	do_norm_prob_plots_ <- as.logical(tclObj(do_norm_prob_plots))
	to_png_ <- as.logical(tclObj(plotrtdf_to_png))
	max_tests_ <- as.integer(tclObj(plot_max_tests))
	plot_limits_plus_10pct_ <- as.logical(tclObj(plot_limits_plus_10pct))
	outside_limits_count_ <- as.logical(tclObj(outside_limits_count))

	min_plots_per_page_ <- as.integer(tclObj(min_plots_per_page))
	
	# vector stuff...
	# datasets, use_alt_limits, ...
	datasets_ <- as.integer(tclObj(datasets))
	rtdf_names_ <- vector()
	rtdf_dirs_ <- vector()
	dataset_names_ <- vector()
	use_alt_limits_ <- vector()
	for (j in 1:datasets_) {
		rtdf_names_[j] <- paste(tclObj(rtdf_names[[j]]),sep="",collapse=" ")
		rtdf_dirs_[j] <- paste(tclObj(rtdf_dirs[[j]]),sep="",collapse=" ")
		dataset_names_[j] <- paste(tclObj(dataset_names[[j]]),sep="",collapse=" ")
		use_alt_limits_[j] <- as.integer(tclObj(use_alt_limits[[j]]))
	}

	output_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	full_path = output_dir
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	setwd(full_path)


	my_expr = substitute(
		PlotRtdf(rtdf_name=rtdf_names_,pdf_name=pdf_name_,
					param_name=param_name_,dataset_name=dataset_names_,
					do_xy_plots=do_xy_plots_,do_hist_and_xy=do_hist_and_xy_,
					title=plotrtdf_title_,auto_scale=auto_scale_,
					save_workspace_to=save_workspace_to_,
					do_csv=do_csv_,do_robust_stats=do_robust_stats_,
					alt_limits=alt_limits_,use_alt_lims=use_alt_limits_,
					min_plots_per_page=min_plots_per_page_,
					do_guardbands=do_guardbands_,use_csv_formulas=use_csv_formulas_,
					use_OOCalc_csv=use_OOCalc_csv_,add_normal_curve=add_normal_curve_,
					plot_using_test_limits=plot_using_test_limits_,
					collapse_MPRs=collapse_MPRs_,rtdf_dirs=rtdf_dirs_,
					param_dir=param_name_dir_,alt_lim_dir=alt_limits_dir_,
					plot_widest_limits_or_values=plot_widest_limits_or_values_,
					superimpose_hist=superimpose_hist_,
					just_superimposed_histo=just_superimposed_histo_,
					do_norm_prob_plots=do_norm_prob_plots_,
					to_png=to_png_,max_tests=max_tests_,
					plot_limits_plus_10pct=plot_limits_plus_10pct_,
					outside_limits_count=outside_limits_count_)
	)
	tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
	tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
	my_command = sprintf("%s\n",deparse(my_expr))
	my_cmnd = "PlotRtdf(...)\n"
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
	if(plotrtdf_autoopen_ && !to_png_) {
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
		plotrtdf_win <- get("plotrtdf_win",envir=.TkRadar.wins)
		tkdestroy(plotrtdf_win)
	}

}



#-----------------------------------------------------
PlotRtdfGui <-function() {
	
	PlotRtdfGui_defaults()
	plotrtdf_win <- tktoplevel()
	assign("plotrtdf_win",plotrtdf_win,envir=.TkRadar.wins)
	tkwm.title(plotrtdf_win, "PlotRtdf")

	# these get reset by Default button, so need to be defined first...
	max_tests_frame <- tkframe(plotrtdf_win)
	max_tests_entry <- tkentry(max_tests_frame,
						width=10,
						background="white",
						textvariable=plot_max_tests_shadow)


	bottom_row <- tkframe(plotrtdf_win)
	default_button <- tkbutton(bottom_row,
						text="DEFAULTS",
						#anchor="w",
						width=12,
						command=PlotRtdfGui_defaults)
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
						text="RUN & QUIT",
						#anchor="w",
						width=12,
						command=function() run_PlotRtdf(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
						text="QUIT",
						#anchor="w",
						width=12,
						command=function() tkdestroy(plotrtdf_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
						text="RUN",
						#anchor="w",
						width=12,
						command=function() run_PlotRtdf(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	dir_entry_frame <- tkframe(plotrtdf_win)
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

	indir_entry_frame <- tkframe(plotrtdf_win)
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

	dataset_frame <- tkframe(plotrtdf_win)
	dataset_label <- tklabel(dataset_frame,
						#width=10,
						text="number of datasets")
	tkpack(dataset_label,side="left")
	datasets_value <- tklabel(dataset_frame,
						width=3,
						relief="sunken",
						textvariable=datasets)
	tkpack(datasets_value,side="left")
	dataset_plus <- tkbutton(dataset_frame,
						text="+",
						command=inc_datasets)
	tkpack(dataset_plus,side="left")
	dataset_minus <- tkbutton(dataset_frame,
						text="-",
						command=dec_datasets)
	tkpack(dataset_minus,side="left")
	tkpack(dataset_frame,side="top",anchor="w",fill="x")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	plots_p_page_frame <- tkframe(plotrtdf_win)
	plots_pp_label <- tklabel(plots_p_page_frame,
						#width=10,
						text="min plots per page")
	tkpack(plots_pp_label,side="left")
	plots_pp_value <- tklabel(plots_p_page_frame,
						width=3,
						relief="sunken",
						textvariable=min_plots_per_page)
	tkpack(plots_pp_value,side="left")
	plots_pp_plus <- tkbutton(plots_p_page_frame,
						text="+",
						command=inc_plots_p_page)
	tkpack(plots_pp_plus,side="left")
	plots_pp_minus <- tkbutton(plots_p_page_frame,
						text="-",
						command=dec_plots_p_page)
	tkpack(plots_pp_minus,side="left")
	tkpack(plots_p_page_frame,side="bottom",anchor="w")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	# defined earlier: max_tests_frame <- tkframe(plotrtdf_win)
	max_tests_label <- tklabel(max_tests_frame,
						text="max_tests")
	tkpack(max_tests_label,side="left")
	tkpack(max_tests_entry,side="left")
	tkbind(max_tests_entry,"<KeyRelease>",function() {
					tmp <- as.integer(tclObj(plot_max_tests_shadow))
					if( (length(tmp)>0) && is.finite(tmp)) {
						tkconfigure(max_tests_entry,background="white")
						tclvalue(plot_max_tests) <- tmp
					} else {
						tkconfigure(max_tests_entry,background="yellow")
					}
				})
	tkpack(max_tests_frame,side="bottom",anchor="w")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	checkboxes_frame <- tkframe(plotrtdf_win)

	left_frame <- tkframe(checkboxes_frame)
	middle_frame <- tkframe(checkboxes_frame)
	right_frame <- tkframe(checkboxes_frame)

	donormplot_button <- tkcheckbutton(left_frame,
						text="do_norm_prob_plots          ",
						variable=do_norm_prob_plots)
	tkpack(donormplot_button,side="top",anchor="w")

	do_limits_10pct_button <- tkcheckbutton(left_frame,
						text="plot_limits_plus_10pct",
						variable=plot_limits_plus_10pct)
	tkpack(do_limits_10pct_button,side="top",anchor="w")

	do_csv_button <- tkcheckbutton(left_frame,
						text="do_csv",
						variable=do_csv)
	tkpack(do_csv_button,side="top",anchor="w")

	normal_button <- tkcheckbutton(left_frame,
						text="add_normal_curve",
						variable=add_normal_curve)
	tkpack(normal_button,side="top",anchor="w")

	mprs_button <- tkcheckbutton(left_frame,
						text="collapse_MPRs",
						variable=collapse_MPRs)
	tkpack(mprs_button,side="top",anchor="w")

	autoopen_button <- tkcheckbutton(left_frame,
						text="auto_open_pdf",
						variable=plotrtdf_autoopen)
	tkpack(autoopen_button,side="top",anchor="w")



	doxy_button <- tkcheckbutton(middle_frame,
						text="do_xy_plots",
						variable=do_xy_plots)
	tkpack(doxy_button,side="top",anchor="w")

	testlimit_button <- tkcheckbutton(middle_frame,
						text="plot_using_test_limits",
						variable=plot_using_test_limits)
	tkpack(testlimit_button,side="top",anchor="w")

	do_csvform_button <- tkcheckbutton(middle_frame,
						text="use_csv_formulas",
						variable=use_csv_formulas)
	tkpack(do_csvform_button,side="top",anchor="w")

	super_button <- tkcheckbutton(middle_frame,
						text="superimpose_hist",
						variable=superimpose_hist)
	tkpack(super_button,side="top",anchor="w")

	guardbands_button <- tkcheckbutton(middle_frame,
						text="do_guardbands",
						variable=do_guardbands)
	tkpack(guardbands_button,side="top",anchor="w")

	to_png_button <- tkcheckbutton(middle_frame,
						text="to_png  (disables pdf output!)",
						variable=plotrtdf_to_png)
	tkpack(to_png_button,side="top",anchor="w")


	dohistxy_button <- tkcheckbutton(right_frame,
						text="do_hist_and_xy",
						variable=do_hist_and_xy)
	tkpack(dohistxy_button,side="top",anchor="w")

	testlimit2_button <- tkcheckbutton(right_frame,
						text="plot_widest_limits_or_values",
						variable=plot_widest_limits_or_values)
	tkpack(testlimit2_button,side="top",anchor="w")

	OOCalc_csv_button <- tkcheckbutton(right_frame,
						text="use_OOCalc_csv",
						variable=use_OOCalc_csv)
	tkpack(OOCalc_csv_button,side="top",anchor="w")

	just_super_button <- tkcheckbutton(right_frame,
						text="just_superimposed_histo",
						variable=just_superimposed_histo)
	tkpack(just_super_button,side="top",anchor="w")

	do_fails_cnt_button <- tkcheckbutton(right_frame,
						text="outside_limits_count",
						variable=outside_limits_count)
	tkpack(do_fails_cnt_button,side="top",anchor="w")

	do_autoscale_button <- tkcheckbutton(right_frame,
						text="auto_scale",
						variable=auto_scale)
	tkpack(do_autoscale_button,side="top",anchor="w")


	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	radio_buttons_frame <- tkframe(plotrtdf_win)
	
	radio0 <- tkradiobutton(radio_buttons_frame,
						text="use mean/sdev",
						value="0",
						variable=do_robust_stats)
	tkpack(radio0,side="left")
	radio1 <- tkradiobutton(radio_buttons_frame,
						text="use robust mean/sdev",
						value="1",
						variable=do_robust_stats)
	tkpack(radio1,side="left")
	radio2 <- tkradiobutton(radio_buttons_frame,
						text="use 2 sided robust mean/sdev",
						value="2",
						variable=do_robust_stats)
	tkpack(radio2,side="left")
	tkpack(radio_buttons_frame,side="bottom",anchor="w",fill="x")
	

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	tkpack(left_frame,side="left",anchor="n")
	tkpack(middle_frame,side="left",anchor="n")
	tkpack(right_frame,side="left",anchor="n")
	tkpack(checkboxes_frame,side="bottom",anchor="w")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	workspace_frame <- tkframe(plotrtdf_win)
	workspace_label <- tklabel(workspace_frame,
						#width=10,
						text="save workspace to ")
	tkpack(workspace_label,side="left")
	workspace_entry <- tkentry(workspace_frame,
						width=20,
						textvariable=save_workspace_to)
	tkpack(workspace_entry,side="left",fill="x",expand=1)
#	workspace_browse <- tkbutton(workspace_frame,
#						text="Browse",
#						command=workspace_browser)
#	tkpack(workspace_browse,side="right")
	tkpack(workspace_frame,side="bottom",anchor="w",fill="x")	

	title_frame <- tkframe(plotrtdf_win)
	title_label <- tklabel(title_frame,
						width=10,
						text="title")
	tkpack(title_label,side="left")
	title_entry <- tkentry(title_frame,
						width=20,
						textvariable=plotrtdf_title)
	tkpack(title_entry,side="left",fill="x",expand=1)
	tkpack(title_frame,side="bottom",anchor="w",fill="x")

	alt2_limit_frame <- tkframe(plotrtdf_win)
	alt2_limit_label <- tklabel(alt2_limit_frame,
						width=10,
						text="alt2_limits")
	tkpack(alt2_limit_label,side="left")
	alt2_limit_entry <- tkentry(alt2_limit_frame,
						width=20,
						textvariable=alt2_limits)
	tkpack(alt2_limit_entry,side="left",fill="x",expand=1)
	alt2_limit_browse <- tkbutton(alt2_limit_frame,
						text="Browse",
						command=function() rtdf_browser(alt2_limits,alt_limits_dir))
	tkpack(alt2_limit_browse,side="right")
	tkpack(alt2_limit_frame,side="bottom",anchor="w",fill="x")

	alt_limit_frame <- tkframe(plotrtdf_win)
	alt_limit_label <- tklabel(alt_limit_frame,
						width=10,
						text="alt_limits")
	tkpack(alt_limit_label,side="left")
	alt_limit_entry <- tkentry(alt_limit_frame,
						width=20,
						textvariable=alt_limits)
	tkpack(alt_limit_entry,side="left",fill="x",expand=1)
	alt_limit_browse <- tkbutton(alt_limit_frame,
						text="Browse",
						command=function() rtdf_browser(alt_limits,alt_limits_dir))
	tkpack(alt_limit_browse,side="right")
	tkpack(alt_limit_frame,side="bottom",anchor="w",fill="x")

	param_entry_frame <- tkframe(plotrtdf_win)
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
	tkpack(param_entry_frame,side="bottom",anchor="w",fill="x")

	pdf_entry_frame <- tkframe(plotrtdf_win)
	pdf_entry_label <- tklabel(pdf_entry_frame,
						width=10,
						text="pdf_name ")
	tkpack(pdf_entry_label,side="left")
	pdf_entry <- tkentry(pdf_entry_frame,
						width=20,
						textvariable=plotrtdf_pdf_name)
	tkpack(pdf_entry,side="left",fill="x",expand=1)
	pdf_browse <- tkbutton(pdf_entry_frame,
						text="Browse",
						command=function() pdf_browser(plotrtdf_pdf_name))
	tkpack(pdf_browse,side="right")
	tkpack(pdf_entry_frame,side="bottom",anchor="w",fill="x")	

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	for (j in 1:20) {
		dataset_frame <- tkframe(plotrtdf_win)
		my_text = as.character(j)
		rtdf_entry_label <- tklabel(dataset_frame,
						width=5,
						anchor="e",
						text=my_text)
		tkpack(rtdf_entry_label,side="left")
		rtdf_entry <- tkentry(dataset_frame,
						textvariable=rtdf_names[[j]])
		tkpack(rtdf_entry,side="left",fill="x",expand=1)
		if (j==1) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(rtdf_names[[1]],rtdf_dirs[[1]]))
		} else if (j==2) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(rtdf_names[[2]],rtdf_dirs[[2]]))
		} else if (j==3) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(rtdf_names[[3]],rtdf_dirs[[3]]))
		} else if (j==4) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(rtdf_names[[4]],rtdf_dirs[[4]]))
		} else if (j==5) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(rtdf_names[[5]],rtdf_dirs[[5]]))
		} else if (j==6) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(rtdf_names[[6]],rtdf_dirs[[6]]))
		} else if (j==7) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(rtdf_names[[7]],rtdf_dirs[[7]]))
		} else if (j==8) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(rtdf_names[[8]],rtdf_dirs[[8]]))
		} else if (j==9) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(rtdf_names[[9]],rtdf_dirs[[9]]))
		} else if (j==10) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(rtdf_names[[10]],rtdf_dirs[[10]]))
		} else if (j==11) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(rtdf_names[[11]],rtdf_dirs[[11]]))
		} else if (j==12) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(rtdf_names[[12]],rtdf_dirs[[12]]))
		} else if (j==13) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(rtdf_names[[13]],rtdf_dirs[[13]]))
		} else if (j==14) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(rtdf_names[[14]],rtdf_dirs[[14]]))
		} else if (j==15) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(rtdf_names[[15]],rtdf_dirs[[15]]))
		} else if (j==16) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(rtdf_names[[16]],rtdf_dirs[[16]]))
		} else if (j==17) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(rtdf_names[[17]],rtdf_dirs[[17]]))
		} else if (j==18) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(rtdf_names[[18]],rtdf_dirs[[18]]))
		} else if (j==19) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(rtdf_names[[19]],rtdf_dirs[[19]]))
		} else if (j==20) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() rtdf_browser(rtdf_names[[20]],rtdf_dirs[[20]]))
		} 
		tkpack(rtdf_browse,side="left")

		name_entry <- tkentry(dataset_frame,
						width=20,
						textvariable=dataset_names[[j]])
		tkpack(name_entry,side="left")

		lim_radio <- tkradiobutton(dataset_frame,
						text="lim",
						value=0,
						variable=use_alt_limits[[j]])
		tkpack(lim_radio,side="left")

		alt_lim_radio <- tkradiobutton(dataset_frame,
						text="alt_lim",
						value=1,
						variable=use_alt_limits[[j]])
		tkpack(alt_lim_radio,side="left")

		alt2_lim_radio <- tkradiobutton(dataset_frame,
						text="alt2_lim",
						value=2,
						variable=use_alt_limits[[j]])
		tkpack(alt2_lim_radio,side="left")

#		altlim_ckbox <- tkcheckbutton(dataset_frame,
#						text="use_alt_lim",
#						variable=use_alt_limits[[j]])
#		tkpack(altlim_ckbox,side="left")

		dataset_frames[[j]] <<- dataset_frame
	}

	my_sets <- as.integer(tclObj(datasets))

	for (j in 1:my_sets)  tkpack(dataset_frames[[j]],side="top",anchor="w",fill="x")


}


