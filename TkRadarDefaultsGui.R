# TkRadarDefaultsGui.R
#
# $Id: TkRadarDefaultsGui.R,v 1.27 2020/06/22 23:59:27 david Exp $
#
# Tk/Tcl GUI wrapper for loading and saving .TkRadar files which
# define default directories and settings for TkRadar sessions
#
# Copyright (C) 2009-14 David Gattrell
#               2019-20
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
if(!exists(".TkRadar.env")) .TkRadar.env <- new.env()

defaults_file_name <- tclVar("")

# TkRadar
defaults_set_TkRadar_logfile <- tclVar(0)
defaults_set_TkRadar_verbose <- tclVar(0)
# AsciiWaferMap
defaults_set_default_ascwmap_type <- tclVar(0)		
defaults_set_default_ascwmap_xleft <- tclVar(0)			
defaults_set_default_ascwmap_ydown <- tclVar(0)			
defaults_set_default_ascwmap_notch <- tclVar(0)		
defaults_set_default_ascwmap_pass_bins <- tclVar(0)		
defaults_set_default_ascwmap_test_floor <- tclVar(0)	
defaults_set_default_ascwmap_product_id <- tclVar(0)	
defaults_set_default_ascwmap_yield <- tclVar(0)			
defaults_set_default_ascwmap_multi_bin <- tclVar(0)			
defaults_set_default_ascwmap_multi_terse <- tclVar(0)			
defaults_set_default_ascwmap_skip_die_minus <- tclVar(0)			
defaults_set_default_ascwmap_mirror_die <- tclVar(0)			
defaults_set_default_ascwmap_sinf_fmt <- tclVar(0)			
defaults_set_default_ascwmap_x_step <- tclVar(0)			
defaults_set_default_ascwmap_y_step <- tclVar(0)			
# ControlCharts
defaults_set_default_do_western_electric <- tclVar(0)
defaults_set_default_control_start_n <- tclVar(0)
defaults_set_default_control_count_n <- tclVar(0)
defaults_set_default_control_charts_per_page <- tclVar(0)
defaults_set_default_control_landscape <- tclVar(0)
defaults_set_default_control_autoopen <- tclVar(0)
# ConvertCsv
defaults_set_default_convcsv_type <- tclVar(0)
defaults_set_default_csv_posix_time <- tclVar(0)
defaults_set_default_csv_lim_eq_pass_flags <- tclVar(0)

csv_in_dir <- tclVar("")	# old var, renamed csv_out_csv_dir for clarity
							# leave a copy here so that older defaults files
							# can still be read without error.

# ConvertEagleCSV
defaults_set_default_csvETS_do_summary <- tclVar(0)
defaults_set_default_csvETS_just_fail_tests_summary <- tclVar(0)
defaults_set_default_csvETS_duplicate_testnames <- tclVar(0)
# ConvertStdf
defaults_set_default_do_summary <- tclVar(0)
defaults_set_default_just_fail_tests_summary <- tclVar(0)
defaults_set_default_do_conditions <- tclVar(0)
defaults_set_default_do_dtrs <- tclVar(0)
defaults_set_default_duplicate_testnames <- tclVar(0)
defaults_set_default_use_MPR_invalid_pf_data <- tclVar(0)
defaults_set_default_ltx_ignore_testname_objects <- tclVar(0)
defaults_set_default_do_testflag_matrix <- tclVar(0)
defaults_set_default_keep_alarmed_values <- tclVar(0)
defaults_set_default_do_raw_tsrs <- tclVar(0)
defaults_set_default_do_FTR_fail_cycle <- tclVar(0)
defaults_set_default_use_testorder <- tclVar(0)
defaults_set_default_save_testorder <- tclVar(0)
defaults_set_default_mult_limits <- tclVar(0)
default_mult_limits_shadow <- tclVar(0)

# MergeRtdf
defaults_set_default_merge_union_of_tests <- tclVar(0)

# PlotRtdf
defaults_set_default_min_plots_per_page <- tclVar(0)
defaults_set_default_use_csv_formulas <- tclVar(0)
defaults_set_default_use_OOCalc_csv <- tclVar(0)
defaults_set_default_add_normal_curve <- tclVar(0)
defaults_set_default_do_robust_stats <- tclVar(0)
defaults_set_default_plotrtdf_autoopen <- tclVar(0)
defaults_set_default_superimpose_hist <- tclVar(0)
defaults_set_default_plot_max_tests <- tclVar(0)
default_max_tests_shadow <- tclVar(0)
defaults_set_default_plot_limits_plus_10pct <- tclVar(0)
defaults_set_default_outside_limits_count <- tclVar(0)

# ProbeVsReprobe
defaults_set_default_pvsrp_site_pct <- tclVar(0)
defaults_set_default_pvsrp_type <- tclVar(0)

# ShrinkRetests
defaults_set_default_use_xy_coords <- tclVar(0)

# WaferMap
defaults_set_default_wmap_xleft <- tclVar(0)
defaults_set_default_wmap_ydown <- tclVar(0)
defaults_set_default_x_coord_alpha <- tclVar(0)
defaults_set_default_panel <- tclVar(0)
defaults_set_default_wmap_notch <- tclVar(0)
defaults_set_default_wmap_autoopen <- tclVar(0)
defaults_set_default_wmap_param_col_start <- tclVar(0)
defaults_set_default_wmap_param_col_end <- tclVar(0)
defaults_set_default_wmap_param_col_rev <- tclVar(0)	
defaults_set_default_wmap_bin_vs_col_name <- tclVar(0)
defaults_set_default_wmap_bin_vs_col_path <- tclVar(0)
defaults_set_default_wmap_gen_bins_csv <- tclVar(0)
defaults_set_default_wmap_borders_off <- tclVar(0)

default_wmap_param_col_start_shadow <- tclVar(0)
default_wmap_param_col_end_shadow <- tclVar(0)

# WaferMap .. XformWaferMap
defaults_set_default_wmap_rotate_ccw <- tclVar(0)
defaults_set_default_wmap_x_rev_polarity <- tclVar(0)
defaults_set_default_wmap_y_rev_polarity <- tclVar(0)
defaults_set_default_wmap_x_shift <- tclVar(0)
defaults_set_default_wmap_y_shift <- tclVar(0)
# XYWid2Partid
defaults_set_default_xyw2partid_save_prev_partid <- tclVar(0)
defaults_set_default_xyw2partid_xcoord_substr <- tclVar(0)
defaults_set_default_xyw2partid_ycoord_substr <- tclVar(0)
defaults_set_default_xyw2partid_waferid_substr <- tclVar(0)


#-----------------------------------------------------
inc_verbose <- function() {

	my_value <- as.integer(tclObj(TkRadar_verbose))
	if (my_value<5) {
		my_value <- my_value + 1
	}
	tclvalue(TkRadar_verbose) <- my_value
}

#-----------------------------------------------------
dec_verbose <- function() {

	my_value <- as.integer(tclObj(TkRadar_verbose))
	if (my_value>-1) {
		my_value <- my_value - 1
	}
	tclvalue(TkRadar_verbose) <- my_value
}

#-----------------------------------------------------
inc_def_plots_p_page <- function() {

	my_value <- as.integer(tclObj(default_min_plots_per_page))
	if (my_value<20)  my_value <- my_value + 1
	tclvalue(default_min_plots_per_page) <- my_value
}

#-----------------------------------------------------
dec_def_plots_p_page <- function() {

	my_value <- as.integer(tclObj(default_min_plots_per_page))
	if (my_value>1)  my_value <- my_value - 1
	tclvalue(default_min_plots_per_page) <- my_value
}

#-----------------------------------------------------
inc_def_charts_p_page <- function() {

	my_value <- as.integer(tclObj(default_control_charts_per_page))
	if (my_value<20)  my_value <- my_value + 1
	tclvalue(default_control_charts_per_page) <- my_value
}

#-----------------------------------------------------
dec_def_charts_p_page <- function() {

	my_value <- as.integer(tclObj(default_control_charts_per_page))
	if (my_value>1)  my_value <- my_value - 1
	tclvalue(default_control_charts_per_page) <- my_value
}

#----------------------------------------------------
load_default_settings_file <- function() {

	# initialize all default_set_ variables to 0
	tclvalue(defaults_set_TkRadar_logfile) <- 0
	tclvalue(defaults_set_TkRadar_verbose) <- 0

	tclvalue(defaults_set_default_ascwmap_type) <- 0
	tclvalue(defaults_set_default_ascwmap_xleft) <- 0
	tclvalue(defaults_set_default_ascwmap_ydown) <- 0
	tclvalue(defaults_set_default_ascwmap_notch) <- 0
	tclvalue(defaults_set_default_ascwmap_pass_bins) <- 0
	tclvalue(defaults_set_default_ascwmap_test_floor) <- 0
	tclvalue(defaults_set_default_ascwmap_product_id) <- 0
	tclvalue(defaults_set_default_ascwmap_yield) <- 0
	tclvalue(defaults_set_default_ascwmap_multi_bin) <- 0
	tclvalue(defaults_set_default_ascwmap_multi_terse) <- 0
	tclvalue(defaults_set_default_ascwmap_skip_die_minus) <- 0
	tclvalue(defaults_set_default_ascwmap_mirror_die) <- 0
	tclvalue(defaults_set_default_ascwmap_sinf_fmt) <- 0
	tclvalue(defaults_set_default_ascwmap_x_step) <- 0
	tclvalue(defaults_set_default_ascwmap_y_step) <- 0

	tclvalue(defaults_set_default_do_western_electric) <- 0
	tclvalue(defaults_set_default_control_start_n) <- 0
	tclvalue(defaults_set_default_control_count_n) <- 0
	tclvalue(defaults_set_default_control_charts_per_page) <- 0
	tclvalue(defaults_set_default_control_landscape) <- 0
	tclvalue(defaults_set_default_control_autoopen) <- 0

	tclvalue(defaults_set_default_convcsv_type) <- 0
	tclvalue(defaults_set_default_csv_posix_time) <- 0
	tclvalue(defaults_set_default_csv_lim_eq_pass_flags) <- 0

	tclvalue(defaults_set_default_csvETS_do_summary) <- 0
	tclvalue(defaults_set_default_csvETS_just_fail_tests_summary) <- 0
	tclvalue(defaults_set_default_csvETS_duplicate_testnames) <- 0

	tclvalue(defaults_set_default_do_summary) <- 0
	tclvalue(defaults_set_default_just_fail_tests_summary) <- 0
	tclvalue(defaults_set_default_do_conditions) <- 0	
	tclvalue(defaults_set_default_do_dtrs) <- 0
	tclvalue(defaults_set_default_duplicate_testnames) <- 0
	tclvalue(defaults_set_default_use_MPR_invalid_pf_data) <- 0
	tclvalue(defaults_set_default_ltx_ignore_testname_objects) <- 0
	tclvalue(defaults_set_default_do_testflag_matrix) <- 0	
	tclvalue(defaults_set_default_keep_alarmed_values) <- 0	
	tclvalue(defaults_set_default_do_raw_tsrs) <- 0	
	tclvalue(defaults_set_default_do_FTR_fail_cycle) <- 0	
	tclvalue(defaults_set_default_use_testorder) <- 0	
	tclvalue(defaults_set_default_save_testorder) <- 0	
	tclvalue(defaults_set_default_mult_limits) <- 0	

	tclvalue(defaults_set_default_merge_union_of_tests) <- 0

	tclvalue(defaults_set_default_min_plots_per_page) <- 0
	tclvalue(defaults_set_default_use_csv_formulas) <- 0
	tclvalue(defaults_set_default_use_OOCalc_csv) <- 0
	tclvalue(defaults_set_default_add_normal_curve) <- 0
	tclvalue(defaults_set_default_do_robust_stats) <- 0
	tclvalue(defaults_set_default_plotrtdf_autoopen) <- 0
	tclvalue(defaults_set_default_superimpose_hist) <- 0
	tclvalue(defaults_set_default_plot_max_tests) <- 0
	tclvalue(defaults_set_default_plot_limits_plus_10pct) <- 0
	tclvalue(defaults_set_default_outside_limits_count) <- 0

	tclvalue(defaults_set_default_pvsrp_site_pct) <- 0
	tclvalue(defaults_set_default_pvsrp_type) <- 0

	tclvalue(defaults_set_default_use_xy_coords) <- 0
	
	tclvalue(defaults_set_default_wmap_xleft) <- 0
	tclvalue(defaults_set_default_wmap_ydown) <- 0
	tclvalue(defaults_set_default_x_coord_alpha) <- 0
	tclvalue(defaults_set_default_panel) <- 0
	tclvalue(defaults_set_default_wmap_notch) <- 0
	tclvalue(defaults_set_default_wmap_autoopen) <- 0
	tclvalue(defaults_set_default_wmap_param_col_start) <- 0
	tclvalue(defaults_set_default_wmap_param_col_end) <- 0
	tclvalue(defaults_set_default_wmap_param_col_rev) <- 0
	tclvalue(defaults_set_default_wmap_bin_vs_col_name) <- 0
	tclvalue(defaults_set_default_wmap_bin_vs_col_path) <- 0
	tclvalue(defaults_set_default_wmap_gen_bins_csv) <- 0
	tclvalue(defaults_set_default_wmap_borders_off) <- 0
	# WaferMap .. XformWaferMap
	tclvalue(defaults_set_default_wmap_rotate_ccw) <- 0
	tclvalue(defaults_set_default_wmap_x_rev_polarity) <- 0
	tclvalue(defaults_set_default_wmap_y_rev_polarity) <- 0
	tclvalue(defaults_set_default_wmap_x_shift) <- 0
	tclvalue(defaults_set_default_wmap_y_shift) <- 0

	tclvalue(defaults_set_default_xyw2partid_save_prev_partid) <- 0
	tclvalue(defaults_set_default_xyw2partid_xcoord_substr) <- 0
	tclvalue(defaults_set_default_xyw2partid_ycoord_substr) <- 0
	tclvalue(defaults_set_default_xyw2partid_waferid_substr) <- 0

	# file should already be in the correct
	# syntax to source...
	filename_ <- paste(tclObj(defaults_file_name),sep="",collapse=" ")
	filename_ = paste(filename_,".TkRadar",sep="")

	my_dir = getwd()
	orig_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	if (nchar(orig_dir)>0)  setwd(orig_dir)
	sys.source(filename_,envir=.TkRadar.env)
	setwd(my_dir)

	# if defaults file contained older csv_in_dir, then transfer contents
	# to new csv_out_csv_dir
	temp_dir = paste(tclObj(csv_in_dir),sep="",collapse=" ")
	if (nchar(temp_dir)>0) {
		tclvalue(csv_out_csv_dir) <- temp_dir
	}

	# update 'shadow' variables
	tclvalue(default_mult_limits_shadow) <- tclObj(default_mult_limits)
	tclvalue(default_max_tests_shadow) <- tclObj(default_plot_max_tests)
}


#----------------------------------------------------
save_default_settings_file <- function() {

	filename_ <- paste(tclObj(defaults_file_name),sep="",collapse=" ")
	filename_ = paste(filename_,".TkRadar",sep="")

	# open file to write to...
	#---------------------------
	orig_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	setwd(orig_dir)
	out_conn = file(filename_,"w")

	# dump directory defaults...
	#---------------------------
	my_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	the_string = sprintf("tclvalue(Output_dir) <- \"%s\" \n",my_dir)
	cat(the_string,file=out_conn)

	my_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
	the_string = sprintf("tclvalue(Rtdfs_dir) <- \"%s\" \n",my_dir)
	cat(the_string,file=out_conn)

	my_dir <- paste(tclObj(stdf_dir),sep="",collapse=" ")
	the_string = sprintf("tclvalue(stdf_dir) <- \"%s\" \n",my_dir)
	cat(the_string,file=out_conn)

	my_dir <- paste(tclObj(a5xx_dir),sep="",collapse=" ")
	the_string = sprintf("tclvalue(a5xx_dir) <- \"%s\" \n",my_dir)
	cat(the_string,file=out_conn)
	
	my_dir <- paste(tclObj(edl_dir),sep="",collapse=" ")
	the_string = sprintf("tclvalue(edl_dir) <- \"%s\" \n",my_dir)
	cat(the_string,file=out_conn)
	
	my_dir <- paste(tclObj(frug_dir),sep="",collapse=" ")
	the_string = sprintf("tclvalue(frug_dir) <- \"%s\" \n",my_dir)
	cat(the_string,file=out_conn)
	
	my_dir <- paste(tclObj(HP9490_dir),sep="",collapse=" ")
	the_string = sprintf("tclvalue(HP9490_dir) <- \"%s\" \n",my_dir)
	cat(the_string,file=out_conn)
	
	my_dir <- paste(tclObj(j9_dir),sep="",collapse=" ")
	the_string = sprintf("tclvalue(j9_dir) <- \"%s\" \n",my_dir)
	cat(the_string,file=out_conn)
	
	my_dir <- paste(tclObj(kdf_dir),sep="",collapse=" ")
	the_string = sprintf("tclvalue(kdf_dir) <- \"%s\" \n",my_dir)
	cat(the_string,file=out_conn)
	
	my_dir <- paste(tclObj(klf_dir),sep="",collapse=" ")
	the_string = sprintf("tclvalue(klf_dir) <- \"%s\" \n",my_dir)
	cat(the_string,file=out_conn)
	
	my_dir <- paste(tclObj(csv_out_csv_dir),sep="",collapse=" ")
	the_string = sprintf("tclvalue(csv_out_csv_dir) <- \"%s\" \n",my_dir)
	cat(the_string,file=out_conn)
	
	my_dir <- paste(tclObj(csv94_dir),sep="",collapse=" ")
	the_string = sprintf("tclvalue(csv94_dir) <- \"%s\" \n",my_dir)
	cat(the_string,file=out_conn)
	
	my_dir <- paste(tclObj(ets_dir),sep="",collapse=" ")
	the_string = sprintf("tclvalue(ets_dir) <- \"%s\" \n",my_dir)
	cat(the_string,file=out_conn)

	my_dir <- paste(tclObj(csvETS_dir),sep="",collapse=" ")
	the_string = sprintf("tclvalue(csvETS_dir) <- \"%s\" \n",my_dir)
	cat(the_string,file=out_conn)


	# dump variable defaults...
	#---------------------------
	if (as.logical(tclObj(defaults_set_TkRadar_logfile))) {
		the_string = "tclvalue(defaults_set_TkRadar_logfile) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
		the_string = sprintf("tclvalue(TkRadar_logfile) <- \"%s\" \n",my_value)
		cat(the_string,file=out_conn)
	}

	if (as.logical(tclObj(defaults_set_TkRadar_verbose))) {
		the_string = "tclvalue(defaults_set_TkRadar_verbose) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(TkRadar_verbose))
		the_string = sprintf("tclvalue(TkRadar_verbose) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}

	# AsciiWaferMap
	if (as.logical(tclObj(defaults_set_default_ascwmap_type))) {
		the_string = "tclvalue(defaults_set_default_ascwmap_type) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.character(tclObj(default_ascwmap_type))
		the_string = sprintf("tclvalue(default_ascwmap_type) <- \"%s\" \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_ascwmap_xleft))) {
		the_string = "tclvalue(defaults_set_default_ascwmap_xleft) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_ascwmap_xleft))
		the_string = sprintf("tclvalue(default_ascwmap_xleft) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_ascwmap_ydown))) {
		the_string = "tclvalue(defaults_set_default_ascwmap_ydown) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_ascwmap_ydown))
		the_string = sprintf("tclvalue(default_ascwmap_ydown) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_ascwmap_notch))) {
		the_string = "tclvalue(defaults_set_default_ascwmap_notch) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.character(tclObj(default_ascwmap_notch))
		the_string = sprintf("tclvalue(default_ascwmap_notch) <- \"%s\" \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_ascwmap_pass_bins))) {
		the_string = "tclvalue(defaults_set_default_ascwmap_pass_bins) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_ascwmap_pass_bins))
		my_value <- paste(my_value,sep="",collapse=",")
		the_string = sprintf("tclvalue(default_ascwmap_pass_bins) <- c(\"%s\") \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_ascwmap_test_floor))) {
		the_string = "tclvalue(defaults_set_default_ascwmap_test_floor) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- paste(tclObj(default_ascwmap_test_floor),sep="",collapse=" ")
		the_string = sprintf("tclvalue(default_ascwmap_test_floor) <- \"%s\" \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_ascwmap_product_id))) {
		the_string = "tclvalue(defaults_set_default_ascwmap_product_id) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- paste(tclObj(default_ascwmap_product_id),sep="",collapse=" ")
		the_string = sprintf("tclvalue(default_ascwmap_product_id) <- \"%s\" \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_ascwmap_yield))) {
		the_string = "tclvalue(defaults_set_default_ascwmap_yield) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_ascwmap_yield))
		the_string = sprintf("tclvalue(default_ascwmap_yield) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_ascwmap_multi_bin))) {
		the_string = "tclvalue(defaults_set_default_ascwmap_multi_bin) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_ascwmap_multi_bin))
		the_string = sprintf("tclvalue(default_ascwmap_multi_bin) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_ascwmap_multi_terse))) {
		the_string = "tclvalue(defaults_set_default_ascwmap_multi_terse) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_ascwmap_multi_terse))
		the_string = sprintf("tclvalue(default_ascwmap_multi_terse) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_ascwmap_skip_die_minus))) {
		the_string = "tclvalue(defaults_set_default_ascwmap_skip_die_minus) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_ascwmap_skip_die_minus))
		the_string = sprintf("tclvalue(default_ascwmap_skip_die_minus) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_ascwmap_mirror_die))) {
		the_string = "tclvalue(defaults_set_default_ascwmap_mirror_die) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- paste(tclObj(default_ascwmap_mirror_die),sep="",collapse=" ")
		the_string = sprintf("tclvalue(default_ascwmap_mirror_die) <- \"%s\" \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_ascwmap_sinf_fmt))) {
		the_string = "tclvalue(defaults_set_default_ascwmap_sinf_fmt) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_ascwmap_sinf_fmt))
		the_string = sprintf("tclvalue(default_ascwmap_sinf_fmt) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_ascwmap_x_step))) {
		the_string = "tclvalue(defaults_set_default_ascwmap_x_step) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- paste(tclObj(default_ascwmap_x_step),sep="",collapse=" ")
		the_string = sprintf("tclvalue(default_ascwmap_x_step) <- \"%s\" \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_ascwmap_y_step))) {
		the_string = "tclvalue(defaults_set_default_ascwmap_y_step) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- paste(tclObj(default_ascwmap_y_step),sep="",collapse=" ")
		the_string = sprintf("tclvalue(default_ascwmap_y_step) <- \"%s\" \n",my_value)
		cat(the_string,file=out_conn)
	}
	
	# ControlCharts
	if (as.logical(tclObj(defaults_set_default_do_western_electric))) {
		the_string = "tclvalue(defaults_set_default_do_western_electric) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_do_western_electric))
		the_string = sprintf("tclvalue(default_do_western_electric) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_control_start_n))) {
		the_string = "tclvalue(defaults_set_default_control_start_n) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_control_start_n))
		the_string = sprintf("tclvalue(default_control_start_n) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_control_count_n))) {
		the_string = "tclvalue(defaults_set_default_control_count_n) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_control_count_n))
		the_string = sprintf("tclvalue(default_control_count_n) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_control_charts_per_page))) {
		the_string = "tclvalue(defaults_set_default_control_charts_per_page) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_control_charts_per_page))
		the_string = sprintf("tclvalue(default_control_charts_per_page) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_control_landscape))) {
		the_string = "tclvalue(defaults_set_default_control_landscape) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_control_landscape))
		the_string = sprintf("tclvalue(default_control_landscape) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_control_autoopen))) {
		the_string = "tclvalue(defaults_set_default_control_autoopen) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_control_autoopen))
		the_string = sprintf("tclvalue(default_control_autoopen) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}


	# ConvertCsv
	if (as.logical(tclObj(defaults_set_default_convcsv_type))) {
		the_string = "tclvalue(defaults_set_default_convcsv_type) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- paste(tclObj(default_convcsv_type),sep="",collapse=" ")
		the_string = sprintf("tclvalue(default_convcsv_type) <- \"%s\" \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_csv_posix_time))) {
		the_string = "tclvalue(defaults_set_default_csv_posix_time) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_csv_posix_time))
		the_string = sprintf("tclvalue(default_csv_posix_time) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_csv_lim_eq_pass_flags))) {
		the_string = "tclvalue(defaults_set_default_csv_lim_eq_pass_flags) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_csv_lim_eq_pass_flags))
		the_string = sprintf("tclvalue(default_csv_lim_eq_pass_flags) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}


	# ConvertEagleCSV
	if (as.logical(tclObj(defaults_set_default_csvETS_do_summary))) {
		the_string = "tclvalue(defaults_set_default_csvETS_do_summary) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_csvETS_do_summary))
		the_string = sprintf("tclvalue(default_csvETS_do_summary) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_csvETS_just_fail_tests_summary))) {
		the_string = "tclvalue(defaults_set_default_csvETS_just_fail_tests_summary) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_csvETS_just_fail_tests_summary))
		the_string = sprintf("tclvalue(default_csvETS_just_fail_tests_summary) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_csvETS_duplicate_testnames))) {
		the_string = "tclvalue(defaults_set_default_csvETS_duplicate_testnames) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_csvETS_duplicate_testnames))
		the_string = sprintf("tclvalue(default_csvETS_duplicate_testnames) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}


	# ConvertStdf
	if (as.logical(tclObj(defaults_set_default_do_summary))) {
		the_string = "tclvalue(defaults_set_default_do_summary) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_do_summary))
		the_string = sprintf("tclvalue(default_do_summary) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_just_fail_tests_summary))) {
		the_string = "tclvalue(defaults_set_default_just_fail_tests_summary) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_just_fail_tests_summary))
		the_string = sprintf("tclvalue(default_just_fail_tests_summary) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_do_conditions))) {
		the_string = "tclvalue(defaults_set_default_do_conditions) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_do_conditions))
		the_string = sprintf("tclvalue(default_do_conditions) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_do_dtrs))) {
		the_string = "tclvalue(defaults_set_default_do_dtrs) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_do_dtrs))
		the_string = sprintf("tclvalue(default_do_dtrs) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_duplicate_testnames))) {
		the_string = "tclvalue(defaults_set_default_duplicate_testnames) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_duplicate_testnames))
		the_string = sprintf("tclvalue(default_duplicate_testnames) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_use_MPR_invalid_pf_data))) {
		the_string = "tclvalue(defaults_set_default_use_MPR_invalid_pf_data) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_use_MPR_invalid_pf_data))
		the_string = sprintf("tclvalue(default_use_MPR_invalid_pf_data) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_ltx_ignore_testname_objects))) {
		the_string = "tclvalue(defaults_set_default_ltx_ignore_testname_objects) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_ltx_ignore_testname_objects))
		the_string = sprintf("tclvalue(default_ltx_ignore_testname_objects) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_do_testflag_matrix))) {
		the_string = "tclvalue(defaults_set_default_do_testflag_matrix) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_do_testflag_matrix))
		the_string = sprintf("tclvalue(default_do_testflag_matrix) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_keep_alarmed_values))) {
		the_string = "tclvalue(defaults_set_default_keep_alarmed_values) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_keep_alarmed_values))
		the_string = sprintf("tclvalue(default_keep_alarmed_values) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}

	if (as.logical(tclObj(defaults_set_default_do_raw_tsrs))) {
		the_string = "tclvalue(defaults_set_default_do_raw_tsrs) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_do_raw_tsrs))
		the_string = sprintf("tclvalue(default_do_raw_tsrs) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}

	if (as.logical(tclObj(defaults_set_default_do_FTR_fail_cycle))) {
		the_string = "tclvalue(defaults_set_default_do_FTR_fail_cycle) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_do_FTR_fail_cycle))
		the_string = sprintf("tclvalue(default_do_FTR_fail_cycle) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}

	if (as.logical(tclObj(defaults_set_default_use_testorder))) {
		the_string = "tclvalue(defaults_set_default_use_testorder) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_use_testorder))
		the_string = sprintf("tclvalue(default_use_testorder) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	
	if (as.logical(tclObj(defaults_set_default_save_testorder))) {
		the_string = "tclvalue(defaults_set_default_save_testorder) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_save_testorder))
		the_string = sprintf("tclvalue(default_save_testorder) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}

	if (as.logical(tclObj(defaults_set_default_mult_limits))) {
		the_string = "tclvalue(defaults_set_default_mult_limits) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_mult_limits))
		the_string = sprintf("tclvalue(default_mult_limits) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}

	# MergeRtdf
	if (as.logical(tclObj(defaults_set_default_merge_union_of_tests))) {
		the_string = "tclvalue(defaults_set_default_merge_union_of_tests) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_merge_union_of_tests))
		the_string = sprintf("tclvalue(default_merge_union_of_tests) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}

	# PlotRtdf
	if (as.logical(tclObj(defaults_set_default_min_plots_per_page))) {
		the_string = "tclvalue(defaults_set_default_min_plots_per_page) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_min_plots_per_page))
		the_string = sprintf("tclvalue(default_min_plots_per_page) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_use_csv_formulas))) {
		the_string = "tclvalue(defaults_set_default_use_csv_formulas) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_use_csv_formulas))
		the_string = sprintf("tclvalue(default_use_csv_formulas) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_use_OOCalc_csv))) {
		the_string = "tclvalue(defaults_set_default_use_OOCalc_csv) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_use_OOCalc_csv))
		the_string = sprintf("tclvalue(default_use_OOCalc_csv) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_add_normal_curve))) {
		the_string = "tclvalue(defaults_set_default_add_normal_curve) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_add_normal_curve))
		the_string = sprintf("tclvalue(default_add_normal_curve) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_do_robust_stats))) {
		the_string = "tclvalue(defaults_set_default_do_robust_stats) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_do_robust_stats))
		the_string = sprintf("tclvalue(default_do_robust_stats) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_plotrtdf_autoopen))) {
		the_string = "tclvalue(defaults_set_default_plotrtdf_autoopen) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_plotrtdf_autoopen))
		the_string = sprintf("tclvalue(default_plotrtdf_autoopen) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_superimpose_hist))) {
		the_string = "tclvalue(defaults_set_default_superimpose_hist) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_superimpose_hist))
		the_string = sprintf("tclvalue(default_superimpose_hist) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_plot_max_tests))) {
		the_string = "tclvalue(defaults_set_default_plot_max_tests) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_plot_max_tests))
		the_string = sprintf("tclvalue(default_plot_max_tests) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_plot_limits_plus_10pct))) {
		the_string = "tclvalue(defaults_set_default_plot_limits_plus_10pct) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_plot_limits_plus_10pct))
		the_string = sprintf("tclvalue(default_plot_limits_plus_10pct) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_outside_limits_count))) {
		the_string = "tclvalue(defaults_set_default_outside_limits_count) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_outside_limits_count))
		the_string = sprintf("tclvalue(default_outside_limits_count) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}

	# ProbeVsReprobe
	if (as.logical(tclObj(defaults_set_default_pvsrp_site_pct))) {
		the_string = "tclvalue(defaults_set_default_pvsrp_site_pct) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_pvsrp_site_pct))
		the_string = sprintf("tclvalue(default_pvsrp_site_pct) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_pvsrp_type))) {
		the_string = "tclvalue(defaults_set_default_pvsrp_type) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- paste(tclObj(default_pvsrp_type),sep="",collapse=" ")
		the_string = sprintf("tclvalue(default_pvsrp_type) <- \"%s\" \n",my_value)
		cat(the_string,file=out_conn)
	}

	# ShrinkRetests
	if (as.logical(tclObj(defaults_set_default_use_xy_coords))) {
		the_string = "tclvalue(defaults_set_default_use_xy_coords) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_use_xy_coords))
		the_string = sprintf("tclvalue(default_use_xy_coords) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}

	# WaferMap
	if (as.logical(tclObj(defaults_set_default_wmap_xleft))) {
		the_string = "tclvalue(defaults_set_default_wmap_xleft) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_wmap_xleft))
		the_string = sprintf("tclvalue(default_wmap_xleft) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_wmap_ydown))) {
		the_string = "tclvalue(defaults_set_default_wmap_ydown) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_wmap_ydown))
		the_string = sprintf("tclvalue(default_wmap_ydown) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_x_coord_alpha))) {
		the_string = "tclvalue(defaults_set_default_x_coord_alpha) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_x_coord_alpha))
		the_string = sprintf("tclvalue(default_x_coord_alpha) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_panel))) {
		the_string = "tclvalue(defaults_set_default_panel) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_panel))
		the_string = sprintf("tclvalue(default_panel) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_wmap_notch))) {
		the_string = "tclvalue(defaults_set_default_wmap_notch) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.character(tclObj(default_wmap_notch))
		the_string = sprintf("tclvalue(default_wmap_notch) <- \"%s\" \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_wmap_autoopen))) {
		the_string = "tclvalue(defaults_set_default_wmap_autoopen) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_wmap_autoopen))
		the_string = sprintf("tclvalue(default_wmap_autoopen) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_wmap_param_col_start))) {
		the_string = "tclvalue(defaults_set_default_wmap_param_col_start) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.numeric(tclObj(default_wmap_param_col_start))
		the_string = sprintf("tclvalue(default_wmap_param_col_start) <- %f \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_wmap_param_col_end))) {
		the_string = "tclvalue(defaults_set_default_wmap_param_col_end) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.numeric(tclObj(default_wmap_param_col_end))
		the_string = sprintf("tclvalue(default_wmap_param_col_end) <- %f \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_wmap_param_col_rev))) {
		the_string = "tclvalue(defaults_set_default_wmap_param_col_rev) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_wmap_param_col_rev))
		the_string = sprintf("tclvalue(default_wmap_param_col_rev) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_wmap_bin_vs_col_name))) {
		the_string = "tclvalue(defaults_set_default_wmap_bin_vs_col_name) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.character(tclObj(default_wmap_bin_vs_col_name))
		the_string = sprintf("tclvalue(default_wmap_bin_vs_col_name) <- \"%s\" \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_wmap_bin_vs_col_path))) {
		the_string = "tclvalue(defaults_set_default_wmap_bin_vs_col_path) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.character(tclObj(default_wmap_bin_vs_col_path))
		the_string = sprintf("tclvalue(default_wmap_bin_vs_col_path) <- \"%s\" \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_wmap_gen_bins_csv))) {
		the_string = "tclvalue(defaults_set_default_wmap_gen_bins_csv) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_wmap_gen_bins_csv))
		the_string = sprintf("tclvalue(default_wmap_gen_bins_csv) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_wmap_borders_off))) {
		the_string = "tclvalue(defaults_set_default_wmap_borders_off) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_wmap_borders_off))
		the_string = sprintf("tclvalue(default_wmap_borders_off) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	# WaferMap .. XformWaferMap
	if (as.logical(tclObj(defaults_set_default_wmap_rotate_ccw))) {
		the_string = "tclvalue(defaults_set_default_wmap_rotate_ccw) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_wmap_rotate_ccw))
		the_string = sprintf("tclvalue(default_wmap_rotate_ccw) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_wmap_x_rev_polarity))) {
		the_string = "tclvalue(defaults_set_default_wmap_x_rev_polarity) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_wmap_x_rev_polarity))
		the_string = sprintf("tclvalue(default_wmap_x_rev_polarity) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_wmap_y_rev_polarity))) {
		the_string = "tclvalue(defaults_set_default_wmap_y_rev_polarity) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_wmap_y_rev_polarity))
		the_string = sprintf("tclvalue(default_wmap_y_rev_polarity) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_wmap_x_shift))) {
		the_string = "tclvalue(defaults_set_default_wmap_x_shift) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_wmap_x_shift))
		the_string = sprintf("tclvalue(default_wmap_x_shift) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_wmap_y_shift))) {
		the_string = "tclvalue(defaults_set_default_wmap_y_shift) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.integer(tclObj(default_wmap_y_shift))
		the_string = sprintf("tclvalue(default_wmap_y_shift) <- %d \n",my_value)
		cat(the_string,file=out_conn)
	}

	# XYWid2Partid
	if (as.logical(tclObj(defaults_set_default_xyw2partid_save_prev_partid))) {
		the_string = "tclvalue(defaults_set_default_xyw2partid_save_prev_partid) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.character(tclObj(default_xyw2partid_save_prev_partid))
		the_string = sprintf("tclvalue(default_xyw2partid_save_prev_partid) <- \"%s\" \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_xyw2partid_xcoord_substr))) {
		the_string = "tclvalue(defaults_set_default_xyw2partid_xcoord_substr) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.character(tclObj(default_xyw2partid_xcoord_substr))
		the_string = sprintf("tclvalue(default_xyw2partid_xcoord_substr) <- \"%s\" \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_xyw2partid_ycoord_substr))) {
		the_string = "tclvalue(defaults_set_default_xyw2partid_ycoord_substr) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.character(tclObj(default_xyw2partid_ycoord_substr))
		the_string = sprintf("tclvalue(default_xyw2partid_ycoord_substr) <- \"%s\" \n",my_value)
		cat(the_string,file=out_conn)
	}
	if (as.logical(tclObj(defaults_set_default_xyw2partid_waferid_substr))) {
		the_string = "tclvalue(defaults_set_default_xyw2partid_waferid_substr) <- 1 \n"
		cat(the_string,file=out_conn)
		my_value <- as.character(tclObj(default_xyw2partid_waferid_substr))
		the_string = sprintf("tclvalue(default_xyw2partid_waferid_substr) <- \"%s\" \n",my_value)
		cat(the_string,file=out_conn)
	}

	close(out_conn)

}


#----------------------------------------------------
# used in default_settings_browser() below to pull strings from tklistbox listvariable
get_tklist_string <- function(my_tkobj,my_index) {
	raw_string <- tclvalue(my_tkobj)

	# tclvalue(def_list) -> "a b {c d} d"
	# a bit of a chore to parse...
	# assume we have no nested {}'s...

	raw_chars = strsplit(raw_string,split="")[[1]]
	my_strings = ""
	sep = " "
	string_count = 1
	i=1
	while(i <= length(raw_chars)) {
		if(raw_chars[i]=="}") {
			sep = " "
		} else if (raw_chars[i]=="{") {
			sep = "}"
		} else if (raw_chars[i]==sep) {
			string_count = string_count + 1
			my_strings[string_count] = ""
		} else {
			my_strings[string_count] = paste(my_strings[string_count],raw_chars[i],sep="")
		}
		i = i+1
	}

	if(my_index<=string_count) {
		tklist_string = my_strings[my_index]
	} else {
		tklist_string = ""
	}

	tklist_string
}


#----------------------------------------------------
default_settings_browser <- function(tk_settingsfile) {

	my_dir <- getwd()
	orig_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	setwd(orig_dir)
	filenames = list.files(pattern="[.]TkRadar$")

	def_list <- tclVar()

	if(length(filenames)>0) {
		
		my_list <- sub("[.]TkRadar$","",filenames)
		tclObj(def_list) <- my_list

		if (exists("defaults_win",envir=.TkRadar.wins,inherits=FALSE)) {
			defaults_win <- get("defaults_win",envir=.TkRadar.wins)
		}
		if (exists("defaults_win") && as.logical(tkwinfo("exists",defaults_win)))  tkdestroy(param_win)
		defaults_win <- tktoplevel()
		assign("defaults_win",defaults_win,envir=.TkRadar.wins)
		tkwm.title(defaults_win, "Settings Browser")
		
		listbox_frame <- tkframe(defaults_win)
		settings_listbox <- tklistbox(listbox_frame,
							selectmode="single",
							exportselection=FALSE,
#							width = max(sapply(my_list,nchar)),
							listvariable = def_list,
							height=10)
		settings_scroll <- tkscrollbar(listbox_frame,
							orient="vertical",
							command=function(...) tkyview(settings_listbox,...))
		tkconfigure(settings_listbox,
					yscrollcommand=function(...) tkset(settings_scroll,...))
#		lapply(my_list,function(my_item) tkinsert(settings_listbox,"end",my_item))

		tkpack(settings_listbox,side="left",anchor="n",fill="both",expand=1)
		tkpack(settings_scroll,side="right",anchor="n",fill="y")
		tkpack(listbox_frame,side="top",anchor="w",fill="both",expand=1)

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

		bottom_row <- tkframe(defaults_win)
		delete_button <- tkbutton(bottom_row,
						text="DELETE",
						#anchor="w",
						width=8,
						command=function() {
							my_index <- as.numeric(tclvalue(tkcurselection(settings_listbox)))
							if(is.finite(my_index)) {
								nickname <- get_tklist_string(def_list,my_index+1)
								#nickname <- as.character(my_list[my_index+1])
								filename_ = paste(nickname,".TkRadar",sep="")
								file.remove(filename_)

								filenames = list.files(pattern="[.]TkRadar$")
								if(length(filenames)>0) {
									my_list <- sub("[.]TkRadar$","",filenames)
								} else {
									my_list <- character()
								}
#								tkdelete(settings_listbox,"0","end")
#								lapply(my_list,function(my_item) tkinsert(settings_listbox,
#												"end",my_item))
								tclObj(def_list) <- my_list
							}
						})						
		tkpack(delete_button,side="right")
		cancel_button <- tkbutton(bottom_row,
						text="QUIT",
						#anchor="w",
						width=8,
						command=function() tkdestroy(defaults_win))
		tkpack(cancel_button,side="right")
	
		apply_button <- tkbutton(bottom_row,
						text="LOAD",
						#anchor="w",
						width=8,
						command=function() {
							my_index <- as.numeric(tclvalue(tkcurselection(settings_listbox)))
							if(is.finite(my_index)) {
#								tclvalue(tk_settingsfile) <- as.character(my_list[my_index+1])
								tclvalue(tk_settingsfile) <- get_tklist_string(def_list,my_index+1)
								load_default_settings_file()
							}
							tkdestroy(defaults_win)
						})
		tkpack(apply_button,side="right")
		tkpack(bottom_row,side="bottom",anchor="w")
	} else {
		tkmessageBox(message="No Settings Files found.")
	}

	setwd(my_dir)
}


#----------------------------------------------------
TkRadarDefaultsGui <- function() {

	tkradardefaults_win <- tktoplevel()
	assign("tkradardefaults_win",tkradardefaults_win,envir=.TkRadar.wins)
	tkwm.title(tkradardefaults_win, "TkRadar Defaults Settings")

	bottom_row <- tkframe(tkradardefaults_win)
	load_button <- tkbutton(bottom_row,
						text="LOAD",
						width=12,
						command= function() default_settings_browser(defaults_file_name))
	tkpack(load_button,side="left")
	save_button <- tkbutton(bottom_row,
						text="SAVE AS",
						width=12,
						command=save_default_settings_file)
	tkpack(save_button,side="left")
	def_file_entry <- tkentry(bottom_row,
						width=20,
						textvariable=defaults_file_name)
	tkpack(def_file_entry,side="left",fill="x",expand=1)
	cancel_button <- tkbutton(bottom_row,
						text="DONE",
						width=12,
						command=function()tkdestroy(tkradardefaults_win))
	tkpack(cancel_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w",fill="x")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	tabs_list <- tclVar()
	tclObj(tabs_list) <- c("Directories",
							"Log file",
							"AsciiWaferMap",
							"ControlCharts",
							"ConvertCsv",
							"ConvertEagleCSV",
							"ConvertStdf",
							"MergeRtdf",
							"PlotRtdf",
							"ProbeVsReprobe",
							"ShrinkRetests",
							"WaferMap",
							"XYWi2Partid")

	# now put a listbox for the tab names on the left side...
	tabs_listbox_frame <- tkframe(tkradardefaults_win)
	tabs_listbox <- tklistbox(tabs_listbox_frame,
						selectmode="single",
						exportselection=FALSE,
						listvariable = tabs_list,
						height=10)
	tabs_scroll <- tkscrollbar(tabs_listbox_frame,
						orient="vertical",
						command=function(...) tkyview(tabs_listbox,...))
	tkconfigure(tabs_listbox,
						yscrollcommand=function(...) tkset(tabs_scroll,...))

	tkpack(tabs_listbox,side="left",anchor="n",fill="both",expand=1)
	tkpack(tabs_scroll,side="right",anchor="n",fill="y")
	tkpack(tabs_listbox_frame,side="left",anchor="n",fill="both",expand=1)

	tkselection.set(tabs_listbox,0)	# set an initial selection

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	tabs <- ttknotebook(tkradardefaults_win)
	tab1 <- ttkframe(tabs)		# Directories
	tab2 <- ttkframe(tabs)		# Log file
	tab8 <- ttkframe(tabs)		# AsciiWaferMap
	tab3 <- ttkframe(tabs)		# ControlCharts
	tab11 <- ttkframe(tabs)		# ConvertCsv
	tab10 <- ttkframe(tabs)		# ConvertEagleCSV
	tab4 <- ttkframe(tabs)		# ConvertStdf
	tab7 <- ttkframe(tabs)		# MergeRtdf
	tab5 <- ttkframe(tabs)		# PlotRtdf
	tab12 <- ttkframe(tabs)		# ProbeVsReprobe
	tab9 <- ttkframe(tabs)		# ShrinkRetests
	tab6 <- ttkframe(tabs)		# WaferMap
	tab13 <- ttkframe(tabs)		# XYWid2Partid

	# === Directories tab ===
	outdir_frame <- tkframe(tab1)
	outdir_label <- tklabel(outdir_frame,
						width=12,
						text="Output_dir")
	tkpack(outdir_label,side="left")
	outdir_entry <- tklabel(outdir_frame,
						width=80,
						relief="sunken",
						textvariable=Output_dir)
	tkpack(outdir_entry,side="left",fill="x",expand=1)
	outdir_browse <- tkbutton(outdir_frame,
						text="Browse",
						command=function() dir_browser(Output_dir))
	tkpack(outdir_browse,side="right")
	tkpack(outdir_frame,side="top",anchor="w",fill="x")

	rtdfdir_frame <- tkframe(tab1)
	rtdfdir_label <- tklabel(rtdfdir_frame,
						width=12,
						text="Rtdfs_dir")
	tkpack(rtdfdir_label,side="left")
	rtdfdir_entry <- tklabel(rtdfdir_frame,
						width=80,
						relief="sunken",
						textvariable=Rtdfs_dir)
	tkpack(rtdfdir_entry,side="left",fill="x",expand=1)
	rtdfdir_browse <- tkbutton(rtdfdir_frame,
						text="Browse",
						command=function() dir_browser(Rtdfs_dir))
	tkpack(rtdfdir_browse,side="right")
	rtdfdir_reset <- tkbutton(rtdfdir_frame,
						text="Clear",
						command=function() tclvalue(Rtdfs_dir)<-"")
	tkpack(rtdfdir_reset,side="right")
	tkpack(rtdfdir_frame,side="top",anchor="w",fill="x")

	stdfdir_frame <- tkframe(tab1)
	stdfdir_label <- tklabel(stdfdir_frame,
						width=12,
						text="stdf_dir")
	tkpack(stdfdir_label,side="left")
	stdfdir_entry <- tklabel(stdfdir_frame,
						width=80,
						relief="sunken",
						textvariable=stdf_dir)
	tkpack(stdfdir_entry,side="left",fill="x",expand=1)
	stdfdir_browse <- tkbutton(stdfdir_frame,
						text="Browse",
						command=function() dir_browser(stdf_dir))
	tkpack(stdfdir_browse,side="right")
	stdfdir_reset <- tkbutton(stdfdir_frame,
						text="Clear",
						command=function() tclvalue(stdf_dir)<-"")
	tkpack(stdfdir_reset,side="right")
	tkpack(stdfdir_frame,side="top",anchor="w",fill="x")

	a5xxdir_frame <- tkframe(tab1)
	a5xxdir_label <- tklabel(a5xxdir_frame,
						width=12,
						text="a5xx_dir")
	tkpack(a5xxdir_label,side="left")
	a5xxdir_entry <- tklabel(a5xxdir_frame,
						width=80,
						relief="sunken",
						textvariable=a5xx_dir)
	tkpack(a5xxdir_entry,side="left",fill="x",expand=1)
	a5xxdir_browse <- tkbutton(a5xxdir_frame,
						text="Browse",
						command=function() dir_browser(a5xx_dir))
	tkpack(a5xxdir_browse,side="right")
	a5xxdir_reset <- tkbutton(a5xxdir_frame,
						text="Clear",
						command=function() tclvalue(a5xx_dir)<-"")
	tkpack(a5xxdir_reset,side="right")
	tkpack(a5xxdir_frame,side="top",anchor="w",fill="x")

	edl_dir_frame <- tkframe(tab1)
	edl_dir_label <- tklabel(edl_dir_frame,
						width=12,
						text="edl_dir")
	tkpack(edl_dir_label,side="left")
	edl_dir_entry <- tklabel(edl_dir_frame,
						width=80,
						relief="sunken",
						textvariable=edl_dir)
	tkpack(edl_dir_entry,side="left",fill="x",expand=1)
	edl_dir_browse <- tkbutton(edl_dir_frame,
						text="Browse",
						command=function() dir_browser(edl_dir))
	tkpack(edl_dir_browse,side="right")
	edl_dir_reset <- tkbutton(edl_dir_frame,
						text="Clear",
						command=function() tclvalue(edl_dir)<-"")
	tkpack(edl_dir_reset,side="right")
	tkpack(edl_dir_frame,side="top",anchor="w",fill="x")

	frug_dir_frame <- tkframe(tab1)
	frug_dir_label <- tklabel(frug_dir_frame,
						width=12,
						text="frug_dir")
	tkpack(frug_dir_label,side="left")
	frug_dir_entry <- tklabel(frug_dir_frame,
						width=80,
						relief="sunken",
						textvariable=frug_dir)
	tkpack(frug_dir_entry,side="left",fill="x",expand=1)
	frug_dir_browse <- tkbutton(frug_dir_frame,
						text="Browse",
						command=function() dir_browser(frug_dir))
	tkpack(frug_dir_browse,side="right")
	frug_dir_reset <- tkbutton(frug_dir_frame,
						text="Clear",
						command=function() tclvalue(frug_dir)<-"")
	tkpack(frug_dir_reset,side="right")
	tkpack(frug_dir_frame,side="top",anchor="w",fill="x")

	hp94_dir_frame <- tkframe(tab1)
	hp94_dir_label <- tklabel(hp94_dir_frame,
						width=12,
						text="HP9490_dir")
	tkpack(hp94_dir_label,side="left")
	hp94_dir_entry <- tklabel(hp94_dir_frame,
						width=80,
						relief="sunken",
						textvariable=HP9490_dir)
	tkpack(hp94_dir_entry,side="left",fill="x",expand=1)
	hp94_dir_browse <- tkbutton(hp94_dir_frame,
						text="Browse",
						command=function() dir_browser(HP9490_dir))
	tkpack(hp94_dir_browse,side="right")
	hp94_dir_reset <- tkbutton(hp94_dir_frame,
						text="Clear",
						command=function() tclvalue(HP9490_dir)<-"")
	tkpack(hp94_dir_reset,side="right")
	tkpack(hp94_dir_frame,side="top",anchor="w",fill="x")

	j9_dir_frame <- tkframe(tab1)
	j9_dir_label <- tklabel(j9_dir_frame,
						width=12,
						text="j9_dir")
	tkpack(j9_dir_label,side="left")
	j9_dir_entry <- tklabel(j9_dir_frame,
						width=80,
						relief="sunken",
						textvariable=j9_dir)
	tkpack(j9_dir_entry,side="left",fill="x",expand=1)
	j9_dir_browse <- tkbutton(j9_dir_frame,
						text="Browse",
						command=function() dir_browser(j9_dir))
	tkpack(j9_dir_browse,side="right")
	j9_dir_reset <- tkbutton(j9_dir_frame,
						text="Clear",
						command=function() tclvalue(j9_dir)<-"")
	tkpack(j9_dir_reset,side="right")
	tkpack(j9_dir_frame,side="top",anchor="w",fill="x")

	kdf_dir_frame <- tkframe(tab1)
	kdf_dir_label <- tklabel(kdf_dir_frame,
						width=12,
						text="kdf_dir")
	tkpack(kdf_dir_label,side="left")
	kdf_dir_entry <- tklabel(kdf_dir_frame,
						width=80,
						relief="sunken",
						textvariable=kdf_dir)
	tkpack(kdf_dir_entry,side="left",fill="x",expand=1)
	kdf_dir_browse <- tkbutton(kdf_dir_frame,
						text="Browse",
						command=function() dir_browser(kdf_dir))
	tkpack(kdf_dir_browse,side="right")
	kdf_dir_reset <- tkbutton(kdf_dir_frame,
						text="Clear",
						command=function() tclvalue(kdf_dir)<-"")
	tkpack(kdf_dir_reset,side="right")
	tkpack(kdf_dir_frame,side="top",anchor="w",fill="x")

	klf_dir_frame <- tkframe(tab1)
	klf_dir_label <- tklabel(klf_dir_frame,
						width=12,
						text="klf_dir")
	tkpack(klf_dir_label,side="left")
	klf_dir_entry <- tklabel(klf_dir_frame,
						width=80,
						relief="sunken",
						textvariable=klf_dir)
	tkpack(klf_dir_entry,side="left",fill="x",expand=1)
	klf_dir_browse <- tkbutton(klf_dir_frame,
						text="Browse",
						command=function() dir_browser(klf_dir))
	tkpack(klf_dir_browse,side="right")
	klf_dir_reset <- tkbutton(klf_dir_frame,
						text="Clear",
						command=function() tclvalue(klf_dir)<-"")
	tkpack(klf_dir_reset,side="right")
	tkpack(klf_dir_frame,side="top",anchor="w",fill="x")

	csv_dir_frame <- tkframe(tab1)
	csv_dir_label <- tklabel(csv_dir_frame,
						width=12,
						text="csv_out_csv_dir")
	tkpack(csv_dir_label,side="left")
	csv_dir_entry <- tklabel(csv_dir_frame,
						width=80,
						relief="sunken",
						textvariable=csv_out_csv_dir)
	tkpack(csv_dir_entry,side="left",fill="x",expand=1)
	csv_dir_browse <- tkbutton(csv_dir_frame,
						text="Browse",
						command=function() dir_browser(csv_out_csv_dir))
	tkpack(csv_dir_browse,side="right")
	csv_dir_reset <- tkbutton(csv_dir_frame,
						text="Clear",
						command=function() tclvalue(csv_out_csv_dir)<-"")
	tkpack(csv_dir_reset,side="right")
	tkpack(csv_dir_frame,side="top",anchor="w",fill="x")

	csv94_dir_frame <- tkframe(tab1)
	csv94_dir_label <- tklabel(csv94_dir_frame,
						width=12,
						text="csv94_dir")
	tkpack(csv94_dir_label,side="left")
	csv94_dir_entry <- tklabel(csv94_dir_frame,
						width=80,
						relief="sunken",
						textvariable=csv94_dir)
	tkpack(csv94_dir_entry,side="left",fill="x",expand=1)
	csv94_dir_browse <- tkbutton(csv94_dir_frame,
						text="Browse",
						command=function() dir_browser(csv94_dir))
	tkpack(csv94_dir_browse,side="right")
	csv94_dir_reset <- tkbutton(csv94_dir_frame,
						text="Clear",
						command=function() tclvalue(csv94_dir)<-"")
	tkpack(csv94_dir_reset,side="right")
	tkpack(csv94_dir_frame,side="top",anchor="w",fill="x")

	ets_dir_frame <- tkframe(tab1)
	ets_dir_label <- tklabel(ets_dir_frame,
						width=12,
						text="ets_dir")
	tkpack(ets_dir_label,side="left")
	ets_dir_entry <- tklabel(ets_dir_frame,
						width=80,
						relief="sunken",
						textvariable=ets_dir)
	tkpack(ets_dir_entry,side="left",fill="x",expand=1)
	ets_dir_browse <- tkbutton(ets_dir_frame,
						text="Browse",
						command=function() dir_browser(ets_dir))
	tkpack(ets_dir_browse,side="right")
	ets_dir_reset <- tkbutton(ets_dir_frame,
						text="Clear",
						command=function() tclvalue(ets_dir)<-"")
	tkpack(ets_dir_reset,side="right")
	tkpack(ets_dir_frame,side="top",anchor="w",fill="x")

	csvETS_dir_frame <- tkframe(tab1)
	csvETS_dir_label <- tklabel(csvETS_dir_frame,
						width=12,
						text="csvETS_dir")
	tkpack(csvETS_dir_label,side="left")
	csvETS_dir_entry <- tklabel(csvETS_dir_frame,
						width=80,
						relief="sunken",
						textvariable=csvETS_dir)
	tkpack(csvETS_dir_entry,side="left",fill="x",expand=1)
	csvETS_dir_browse <- tkbutton(csvETS_dir_frame,
						text="Browse",
						command=function() dir_browser(csvETS_dir))
	tkpack(csvETS_dir_browse,side="right")
	csvETS_dir_reset <- tkbutton(csvETS_dir_frame,
						text="Clear",
						command=function() tclvalue(csvETS_dir)<-"")
	tkpack(csvETS_dir_reset,side="right")
	tkpack(csvETS_dir_frame,side="top",anchor="w",fill="x")

	# === Log file tab ===
	logname_frame <- tkframe(tab2)
	set_logname_button <- tkcheckbutton(logname_frame,
						text="Set",
						variable=defaults_set_TkRadar_logfile)
	tkpack(set_logname_button,side="left",anchor="n")
	logname_label <- tklabel(logname_frame,
						#width=10,
						text="Log File Name")
	tkpack(logname_label,side="left")
	logname_entry <- tkentry(logname_frame,
						width=20,
						textvariable=TkRadar_logfile)
	tkpack(logname_entry,side="left")
	tkpack(logname_frame,side="top",anchor="w",fill="x")
	logverbose_frame <- tkframe(tab2)
	set_logverbose_button <- tkcheckbutton(logverbose_frame,
						text="Set",
						variable=defaults_set_TkRadar_verbose)
	tkpack(set_logverbose_button,side="left",anchor="n")
	logverbose_label <- tklabel(logverbose_frame,
						text="Verbose lines")
	tkpack(logverbose_label,side="left")
	logverbose_value <- tklabel(logverbose_frame,
						width=3,
						relief="sunken",
						textvariable=TkRadar_verbose)
	tkpack(logverbose_value,side="left")
	verbose_plus <- tkbutton(logverbose_frame,
						text="+",
						command=inc_verbose)
	tkpack(verbose_plus,side="left")
	verbose_minus <- tkbutton(logverbose_frame,
						text="-",
						command=dec_verbose)
	tkpack(verbose_minus,side="left")
	tkpack(logverbose_frame,side="top",anchor="w",fill="x")

	# === ControlCharts tab ===
	WECO_frame <- tkframe(tab3)
	set_WECO_button <- tkcheckbutton(WECO_frame,
						text="Set",
						variable=defaults_set_default_do_western_electric)
	tkpack(set_WECO_button,side="left",anchor="n")
	WECO_button <- tkcheckbutton(WECO_frame,
						text="default do_western_electric",
						variable=default_do_western_electric)
	tkpack(WECO_button,side="left",anchor="n")
	tkpack(WECO_frame,side="top",anchor="w",fill="x")
	ctrl_start_frame <- tkframe(tab3)
	set_ctrl_start_button <- tkcheckbutton(ctrl_start_frame,
						text="Set",
						variable=defaults_set_default_control_start_n)
	tkpack(set_ctrl_start_button,side="left",anchor="n")
	ctrl_start_label <- tklabel(ctrl_start_frame,
						width=10,
						text="start_n")
	tkpack(ctrl_start_label,side="left")
	ctrl_start_entry <- tklabel(ctrl_start_frame,
						width=8,
						relief="sunken",
						textvariable=default_control_start_n)
	tkpack(ctrl_start_entry,side="left")
	ctrl_start_browse <- tkbutton(ctrl_start_frame,
						text="Edit",
						command=function() index_entry(default_control_start_n))
	tkpack(ctrl_start_browse,side="left")
	tkpack(ctrl_start_frame,side="top",anchor="w",fill="x")
	ctrl_count_frame <- tkframe(tab3)
	set_ctrl_count_button <- tkcheckbutton(ctrl_count_frame,
						text="Set",
						variable=defaults_set_default_control_count_n)
	tkpack(set_ctrl_count_button,side="left",anchor="n")
	ctrl_count_label <- tklabel(ctrl_count_frame,
						width=10,
						text="count_n")
	tkpack(ctrl_count_label,side="left")
	ctrl_count_entry <- tklabel(ctrl_count_frame,
						width=8,
						relief="sunken",
						textvariable=default_control_count_n)
	tkpack(ctrl_count_entry,side="left")
	ctrl_count_browse <- tkbutton(ctrl_count_frame,
						text="Edit",
						command=function() index_entry(default_control_count_n))
	tkpack(ctrl_count_browse,side="left")
	tkpack(ctrl_count_frame,side="top",anchor="w",fill="x")
	charts_p_page_frame <- tkframe(tab3)
	set_charts_p_page_button <- tkcheckbutton(charts_p_page_frame,
						text="Set",
						variable=defaults_set_default_control_charts_per_page)
	tkpack(set_charts_p_page_button,side="left",anchor="n")
	charts_pp_label <- tklabel(charts_p_page_frame,
						#width=10,
						text="charts per page")
	tkpack(charts_pp_label,side="left")
	charts_pp_value <- tklabel(charts_p_page_frame,
						width=3,
						relief="sunken",
						textvariable=default_control_charts_per_page)
	tkpack(charts_pp_value,side="left")
	charts_pp_plus <- tkbutton(charts_p_page_frame,
						text="+",
						command=inc_def_charts_p_page)
	tkpack(charts_pp_plus,side="left")
	charts_pp_minus <- tkbutton(charts_p_page_frame,
						text="-",
						command=dec_def_charts_p_page)
	tkpack(charts_pp_minus,side="left")
	tkpack(charts_p_page_frame,side="top",anchor="w",fill="x")
	#... 3 more variables to add here...
	cont_land_frame <- tkframe(tab3)
	set_cont_land_button <- tkcheckbutton(cont_land_frame,
						text="Set",
						variable=defaults_set_default_control_landscape)
	tkpack(set_cont_land_button,side="left",anchor="n")
	cont_land_button <- tkcheckbutton(cont_land_frame,
						text="default control_landscape",
						variable=default_control_landscape)
	tkpack(cont_land_button,side="left",anchor="n")
	tkpack(cont_land_frame,side="top",anchor="w",fill="x")

	cont_autoopen_frame <- tkframe(tab3)
	set_cont_autoopen_button <- tkcheckbutton(cont_autoopen_frame,
						text="Set",
						variable=defaults_set_default_control_autoopen)
	tkpack(set_cont_autoopen_button,side="left",anchor="n")
	cont_autoopen_button <- tkcheckbutton(cont_autoopen_frame,
						text="default control_autoopen",
						variable=default_control_autoopen)
	tkpack(cont_autoopen_button,side="left",anchor="n")
	tkpack(cont_autoopen_frame,side="top",anchor="w",fill="x")

	# === ConvertStdf tab ===
	do_summary_frame <- tkframe(tab4)
	set_summary_button <- tkcheckbutton(do_summary_frame,
						text="Set",
						variable=defaults_set_default_do_summary)
	tkpack(set_summary_button,side="left",anchor="n")
	summary_button <- tkcheckbutton(do_summary_frame,
						text="default do_summary",
						variable=default_do_summary)
	tkpack(summary_button,side="left",anchor="n")
	tkpack(do_summary_frame,side="top",anchor="w",fill="x")
	just_fails_summ_frame <- tkframe(tab4)
	set_just_fails_summ_button <- tkcheckbutton(just_fails_summ_frame,
						text="Set",
						variable=defaults_set_default_just_fail_tests_summary)
	tkpack(set_just_fails_summ_button,side="left",anchor="n")
	just_fails_summ_button <- tkcheckbutton(just_fails_summ_frame,
						text="default just_fail_tests_summary",
						variable=default_just_fail_tests_summary)
	tkpack(just_fails_summ_button,side="left",anchor="n")
	tkpack(just_fails_summ_frame,side="top",anchor="w",fill="x")
	do_conditions_frame <- tkframe(tab4)
	set_conditions_button <- tkcheckbutton(do_conditions_frame,
						text="Set",
						variable=defaults_set_default_do_conditions)
	tkpack(set_conditions_button,side="left",anchor="n")
	conditions_button <- tkcheckbutton(do_conditions_frame,
						text="default do_conditions",
						variable=default_do_conditions)
	tkpack(conditions_button,side="left",anchor="n")
	tkpack(do_conditions_frame,side="top",anchor="w",fill="x")
	do_dtrs_frame <- tkframe(tab4)
	set_dtrs_button <- tkcheckbutton(do_dtrs_frame,
						text="Set",
						variable=defaults_set_default_do_dtrs)
	tkpack(set_dtrs_button,side="left",anchor="n")
	dtrs_button <- tkcheckbutton(do_dtrs_frame,
						text="default do_dtrs",
						variable=default_do_dtrs)
	tkpack(dtrs_button,side="left",anchor="n")
	tkpack(do_dtrs_frame,side="top",anchor="w",fill="x")
	duplicate_testnames_frame <- tkframe(tab4)
	set_duplicate_testnames_button <- tkcheckbutton(duplicate_testnames_frame,
						text="Set",
						variable=defaults_set_default_duplicate_testnames)
	tkpack(set_duplicate_testnames_button,side="left",anchor="n")
	duplicate_testnames_button <- tkcheckbutton(duplicate_testnames_frame,
						text="default duplicate_testnames",
						variable=default_duplicate_testnames)
	tkpack(duplicate_testnames_button,side="left",anchor="n")
	tkpack(duplicate_testnames_frame,side="top",anchor="w",fill="x")
	use_MPR_invalid_pf_data_frame <- tkframe(tab4)
	set_use_MPR_invalid_pf_data_button <- tkcheckbutton(use_MPR_invalid_pf_data_frame,
						text="Set",
						variable=defaults_set_default_use_MPR_invalid_pf_data)
	tkpack(set_use_MPR_invalid_pf_data_button,side="left",anchor="n")
	use_MPR_invalid_pf_data_button <- tkcheckbutton(use_MPR_invalid_pf_data_frame,
						text="default use_MPR_invalid_pf_data",
						variable=default_use_MPR_invalid_pf_data)
	tkpack(use_MPR_invalid_pf_data_button,side="left",anchor="n")
	tkpack(use_MPR_invalid_pf_data_frame,side="top",anchor="w",fill="x")
	ltx_ignore_tname_frame <- tkframe(tab4)
	set_ltx_ignore_tname_button <- tkcheckbutton(ltx_ignore_tname_frame,
						text="Set",
						variable=defaults_set_default_ltx_ignore_testname_objects)
	tkpack(set_ltx_ignore_tname_button,side="left",anchor="n")
	ltx_ignore_tname_button <- tkcheckbutton(ltx_ignore_tname_frame,
						text="default ltx_ignore_testname_objects",
						variable=default_ltx_ignore_testname_objects)
	tkpack(ltx_ignore_tname_button,side="left",anchor="n")
	tkpack(ltx_ignore_tname_frame,side="top",anchor="w",fill="x")
	do_testflag_frame <- tkframe(tab4)
	set_testflag_button <- tkcheckbutton(do_testflag_frame,
						text="Set",
						variable=defaults_set_default_do_testflag_matrix)
	tkpack(set_testflag_button,side="left",anchor="n")
	testflag_button <- tkcheckbutton(do_testflag_frame,
						text="default do_testflag_matrix",
						variable=default_do_testflag_matrix)
	tkpack(testflag_button,side="left",anchor="n")
	tkpack(do_testflag_frame,side="top",anchor="w",fill="x")
	alarmed_frame <- tkframe(tab4)
	set_alarmed_button <- tkcheckbutton(alarmed_frame,
						text="Set",
						variable=defaults_set_default_keep_alarmed_values)
	tkpack(set_alarmed_button,side="left",anchor="n")
	alarmed_button <- tkcheckbutton(alarmed_frame,
						text="default keep_alarmed_values",
						variable=default_keep_alarmed_values)
	tkpack(alarmed_button,side="left",anchor="n")
	tkpack(alarmed_frame,side="top",anchor="w",fill="x")
	raw_tsrs_frame <- tkframe(tab4)
	set_raw_tsrs_button <- tkcheckbutton(raw_tsrs_frame,
						text="Set",
						variable=defaults_set_default_do_raw_tsrs)
	tkpack(set_raw_tsrs_button,side="left",anchor="n")
	do_raw_tsrs_button <- tkcheckbutton(raw_tsrs_frame,
						text="default do_raw_tsrs",
						variable=default_do_raw_tsrs)
	tkpack(do_raw_tsrs_button,side="left",anchor="n")
	tkpack(raw_tsrs_frame,side="top",anchor="w",fill="x")
	ftr_fcycle_frame <- tkframe(tab4)
	set_ftr_fcycle_button <- tkcheckbutton(ftr_fcycle_frame,
						text="Set",
						variable=defaults_set_default_do_FTR_fail_cycle)
	tkpack(set_ftr_fcycle_button,side="left",anchor="n")
	do_ftr_fcycle_button <- tkcheckbutton(ftr_fcycle_frame,
						text="default do_FTR_fail_cycle",
						variable=default_do_FTR_fail_cycle)
	tkpack(do_ftr_fcycle_button,side="left",anchor="n")
	tkpack(ftr_fcycle_frame,side="top",anchor="w",fill="x")
	use_testorder_frame <- tkframe(tab4)
	set_use_testorder_button <- tkcheckbutton(use_testorder_frame,
						text="Set",
						variable=defaults_set_default_use_testorder)
	tkpack(set_use_testorder_button,side="left",anchor="n")
	do_use_testorder_button <- tkcheckbutton(use_testorder_frame,
						text="default use_testorder",
						variable=default_use_testorder)
	tkpack(do_use_testorder_button,side="left",anchor="n")
	tkpack(use_testorder_frame,side="top",anchor="w",fill="x")
	save_testorder_frame <- tkframe(tab4)
	set_save_testorder_button <- tkcheckbutton(save_testorder_frame,
						text="Set",
						variable=defaults_set_default_save_testorder)
	tkpack(set_save_testorder_button,side="left",anchor="n")
	do_save_testorder_button <- tkcheckbutton(save_testorder_frame,
						text="default save_testorder",
						variable=default_save_testorder)
	tkpack(do_save_testorder_button,side="left",anchor="n")
	tkpack(save_testorder_frame,side="top",anchor="w",fill="x")
	mult_limits_frame <- tkframe(tab4)
	set_mult_limits_button <- tkcheckbutton(mult_limits_frame,
						text="Set",
						variable=defaults_set_default_mult_limits)
	tkpack(set_mult_limits_button,side="left",anchor="n")
	mult_limits_label <- tklabel(mult_limits_frame,
						#width=10,
						text="default mult_limits")
	tkpack(mult_limits_label,side="left")
	tclvalue(default_mult_limits_shadow) <- tclObj(default_mult_limits)
	mult_limits_entry <- tkentry(mult_limits_frame,
						width=10,
						background="white",
						textvariable=default_mult_limits_shadow)
	tkpack(mult_limits_entry,side="left")
	tkbind(mult_limits_entry,"<KeyRelease>",function() {
					tmp <- as.integer(tclObj(default_mult_limits_shadow))
					if( (length(tmp)>0) && is.finite(tmp)) {
						tkconfigure(mult_limits_entry,background="white")
						tclvalue(default_mult_limits) <- tmp
					} else {
						tkconfigure(mult_limits_entry,background="yellow")
					}
				})
	tkpack(mult_limits_frame,side="top",anchor="w",fill="x")

	# === PlotRtdf tab ===
	min_plots_ppage_frame <- tkframe(tab5)
	set_min_plots_ppage_button <- tkcheckbutton(min_plots_ppage_frame,
						text="Set",
						variable=defaults_set_default_min_plots_per_page)
	tkpack(set_min_plots_ppage_button,side="left",anchor="n")
	plots_pp_label <- tklabel(min_plots_ppage_frame,
						#width=10,
						text="default min plots per page")
	tkpack(plots_pp_label,side="left")
	plots_pp_value <- tklabel(min_plots_ppage_frame,
						width=3,
						relief="sunken",
						textvariable=default_min_plots_per_page)
	tkpack(plots_pp_value,side="left")
	plots_pp_plus <- tkbutton(min_plots_ppage_frame,
						text="+",
						command=inc_def_plots_p_page)
	tkpack(plots_pp_plus,side="left")
	plots_pp_minus <- tkbutton(min_plots_ppage_frame,
						text="-",
						command=dec_def_plots_p_page)
	tkpack(plots_pp_minus,side="left")
	tkpack(min_plots_ppage_frame,side="top",anchor="w",fill="x")
	use_csv_formulas_frame <- tkframe(tab5)
	set_use_csv_formulas_button <- tkcheckbutton(use_csv_formulas_frame,
						text="Set",
						variable=defaults_set_default_use_csv_formulas)
	tkpack(set_use_csv_formulas_button,side="left",anchor="n")
	use_csv_formulas_button <- tkcheckbutton(use_csv_formulas_frame,
						text="default use_csv_formulas",
						variable=default_use_csv_formulas)
	tkpack(use_csv_formulas_button,side="left",anchor="n")
	tkpack(use_csv_formulas_frame,side="top",anchor="w",fill="x")
	use_OOCalc_csv_frame <- tkframe(tab5)
	set_use_OOCalc_csv_button <- tkcheckbutton(use_OOCalc_csv_frame,
						text="Set",
						variable=defaults_set_default_use_OOCalc_csv)
	tkpack(set_use_OOCalc_csv_button,side="left",anchor="n")
	use_OOCalc_csv_button <- tkcheckbutton(use_OOCalc_csv_frame,
						text="default use_OOCalc_csv",
						variable=default_use_OOCalc_csv)
	tkpack(use_OOCalc_csv_button,side="left",anchor="n")
	tkpack(use_OOCalc_csv_frame,side="top",anchor="w",fill="x")
	add_normal_curve_frame <- tkframe(tab5)
	set_add_normal_curve_button <- tkcheckbutton(add_normal_curve_frame,
						text="Set",
						variable=defaults_set_default_add_normal_curve)
	tkpack(set_add_normal_curve_button,side="left",anchor="n")
	add_normal_curve_button <- tkcheckbutton(add_normal_curve_frame,
						text="default add_normal_curve",
						variable=default_add_normal_curve)
	tkpack(add_normal_curve_button,side="left",anchor="n")
	tkpack(add_normal_curve_frame,side="top",anchor="w",fill="x")

	do_robust_stats_frame <- tkframe(tab5)
	set_do_robust_stats_button <- tkcheckbutton(do_robust_stats_frame,
						text="Set",
						variable=defaults_set_default_do_robust_stats)
	tkpack(set_do_robust_stats_button,side="left",anchor="n")
	robust_stats_label <- tklabel(do_robust_stats_frame, text="default do_robust_stats")
	tkpack(robust_stats_label,side="left")
	robust0 <- tkradiobutton(do_robust_stats_frame,
						text="use mean/sdev",
						value="0",
						variable=default_do_robust_stats)
	tkpack(robust0,side="left")
	robust1 <- tkradiobutton(do_robust_stats_frame,
						text="use robust mean/sdev",
						value="1",
						variable=default_do_robust_stats)
	tkpack(robust1,side="left")
	robust2 <- tkradiobutton(do_robust_stats_frame,
						text="use 2 sided robust mean/sdev",
						value="2",
						variable=default_do_robust_stats)
	tkpack(robust2,side="left")
	tkpack(do_robust_stats_frame,side="top",anchor="w",fill="x")

	superimpose_frame <- tkframe(tab5)
	set_superimpose_button <- tkcheckbutton(superimpose_frame,
						text="Set",
						variable=defaults_set_default_superimpose_hist)
	tkpack(set_superimpose_button,side="left",anchor="n")
	superimpose_button <- tkcheckbutton(superimpose_frame,
						text="default superimpose_hist",
						variable=default_superimpose_hist)
	tkpack(superimpose_button,side="left",anchor="n")
	tkpack(superimpose_frame,side="top",anchor="w",fill="x")

	plotrtdf_autoopen_frame <- tkframe(tab5)
	set_plotrtdf_autoopen_button <- tkcheckbutton(plotrtdf_autoopen_frame,
						text="Set",
						variable=defaults_set_default_plotrtdf_autoopen)
	tkpack(set_plotrtdf_autoopen_button,side="left",anchor="n")
	plotrtdf_autoopen_button <- tkcheckbutton(plotrtdf_autoopen_frame,
						text="default plotrtdf_autoopen",
						variable=default_plotrtdf_autoopen)
	tkpack(plotrtdf_autoopen_button,side="left",anchor="n")
	tkpack(plotrtdf_autoopen_frame,side="top",anchor="w",fill="x")

	max_tests_frame <- tkframe(tab5)
	set_max_tests_button <- tkcheckbutton(max_tests_frame,
						text="Set",
						variable=defaults_set_default_plot_max_tests)
	tkpack(set_max_tests_button,side="left",anchor="n")
	max_tests_label <- tklabel(max_tests_frame,
						#width=10,
						text="default max_tests")
	tkpack(max_tests_label,side="left")
	tclvalue(default_max_tests_shadow) <- tclObj(default_plot_max_tests)
	max_tests_entry <- tkentry(max_tests_frame,
						width=10,
						background="white",
						textvariable=default_max_tests_shadow)
	tkpack(max_tests_entry,side="left")
	tkbind(max_tests_entry,"<KeyRelease>",function() {
					tmp <- as.integer(tclObj(default_max_tests_shadow))
					if( (length(tmp)>0) && is.finite(tmp)) {
						tkconfigure(max_tests_entry,background="white")
						tclvalue(default_plot_max_tests) <- tmp
					} else {
						tkconfigure(max_tests_entry,background="yellow")
					}
				})
	tkpack(max_tests_frame,side="top",anchor="w",fill="x")
	limit_plus_10pct_frame <- tkframe(tab5)
	set_limit_plus_10pct_button <- tkcheckbutton(limit_plus_10pct_frame,
						text="Set",
						variable=defaults_set_default_plot_limits_plus_10pct)
	tkpack(set_limit_plus_10pct_button,side="left",anchor="n")
	limit_plus_10pct_button <- tkcheckbutton(limit_plus_10pct_frame,
						text="default plot_limits_plus_10pct",
						variable=default_plot_limits_plus_10pct)
	tkpack(limit_plus_10pct_button,side="left",anchor="n")
	tkpack(limit_plus_10pct_frame,side="top",anchor="w",fill="x")
	outside_limits_frame <- tkframe(tab5)
	set_outside_limits_button <- tkcheckbutton(outside_limits_frame,
						text="Set",
						variable=defaults_set_default_outside_limits_count)
	tkpack(set_outside_limits_button,side="left",anchor="n")
	outside_limits_button <- tkcheckbutton(outside_limits_frame,
						text="default outside_limits_count",
						variable=default_outside_limits_count)
	tkpack(outside_limits_button,side="left",anchor="n")
	tkpack(outside_limits_frame,side="top",anchor="w",fill="x")

	# === WaferMap tab ===
	wmap_xleft_frame <- tkframe(tab6)
	wmap_xleft_button <- tkcheckbutton(wmap_xleft_frame,
						text="default x_left",
						variable=default_wmap_xleft)
	set_wmap_xleft_button <- tkcheckbutton(wmap_xleft_frame,
						text="Set",
						variable=defaults_set_default_wmap_xleft)
	tkpack(set_wmap_xleft_button,side="left",anchor="n")
	tkpack(wmap_xleft_button,side="left",anchor="n")
	tkpack(wmap_xleft_frame,side="top",anchor="w",fill="x")
	wmap_ydown_frame <- tkframe(tab6)
	#flag = as.logical(tclObj(defaults_set_default_wmap_ydown))
	#if(flag)  my_state = "normal"
	#else      my_state = "disable"
	wmap_ydown_button <- tkcheckbutton(wmap_ydown_frame,
						text="default y_down",
						#state=my_state,    
						variable=default_wmap_ydown)
	set_wmap_ydown_button <- tkcheckbutton(wmap_ydown_frame,
						text="Set",
						variable=defaults_set_default_wmap_ydown #,
						#command=function() {
						#	flag = as.logical(tclObj(defaults_set_default_wmap_ydown))
						#	if (flag)  tkconfigure(wmap_ydown_button,state="normal")
						#	else  tkconfigure(wmap_ydown_button,state="disable")
						#}
						)
	tkpack(set_wmap_ydown_button,side="left",anchor="n")
	tkpack(wmap_ydown_button,side="left",anchor="n")
	tkpack(wmap_ydown_frame,side="top",anchor="w",fill="x")
	x_coord_alpha_frame <- tkframe(tab6)
	x_coord_alpha_button <- tkcheckbutton(x_coord_alpha_frame,
						text="default x_coord_alpha",
						variable=default_x_coord_alpha)
	set_x_coord_alpha_button <- tkcheckbutton(x_coord_alpha_frame,
						text="Set",
						variable=defaults_set_default_x_coord_alpha)
	tkpack(set_x_coord_alpha_button,side="left",anchor="n")
	tkpack(x_coord_alpha_button,side="left",anchor="n")
	tkpack(x_coord_alpha_frame,side="top",anchor="w",fill="x")
	panel_frame <- tkframe(tab6)
	panel_button <- tkcheckbutton(panel_frame,
						text="default panel",
						variable=default_panel)
	set_panel_button <- tkcheckbutton(panel_frame,
						text="Set",
						variable=defaults_set_default_panel)
	tkpack(set_panel_button,side="left",anchor="n")
	tkpack(panel_button,side="left",anchor="n")
	tkpack(panel_frame,side="top",anchor="w",fill="x")

	wmap_notch_frame <- tkframe(tab6)
	set_wmap_notch_button <- tkcheckbutton(wmap_notch_frame,
						text="Set",
						variable=defaults_set_default_wmap_notch)
	tkpack(set_wmap_notch_button,side="left",anchor="n")
	wmap_notch_label <- tklabel(wmap_notch_frame, text="default wmap_notch")
	tkpack(wmap_notch_label,side="left")
	wmap_notch_x <- tkradiobutton(wmap_notch_frame,
						text="No label",
						value="x",
						variable=default_wmap_notch)
	tkpack(wmap_notch_x,side="left")
	wmap_notch_s <- tkradiobutton(wmap_notch_frame,
						text="South",
						value="s",
						variable=default_wmap_notch)
	tkpack(wmap_notch_s,side="left")
	wmap_notch_n <- tkradiobutton(wmap_notch_frame,
						text="North",
						value="n",
						variable=default_wmap_notch)
	tkpack(wmap_notch_n,side="left")
	wmap_notch_w <- tkradiobutton(wmap_notch_frame,
						text="West",
						value="w",
						variable=default_wmap_notch)
	tkpack(wmap_notch_w,side="left")
	wmap_notch_e <- tkradiobutton(wmap_notch_frame,
						text="East",
						value="e",
						variable=default_wmap_notch)
	tkpack(wmap_notch_e,side="left")
	tkpack(wmap_notch_frame,side="top",anchor="w",fill="x")

	wmap_autoopen_frame <- tkframe(tab6)
	wmap_autoopen_button <- tkcheckbutton(wmap_autoopen_frame,
						text="default wmap_autoopen",
						variable=default_wmap_autoopen)
	set_wmap_autoopen_button <- tkcheckbutton(wmap_autoopen_frame,
						text="Set",
						variable=defaults_set_default_wmap_autoopen)
	tkpack(set_wmap_autoopen_button,side="left",anchor="n")
	tkpack(wmap_autoopen_button,side="left",anchor="n")
	tkpack(wmap_autoopen_frame,side="top",anchor="w",fill="x")

	wmap_borders_frame <- tkframe(tab6)
	set_wmap_borders_button <- tkcheckbutton(wmap_borders_frame,
						text="Set",
						variable=defaults_set_default_wmap_borders_off)
	tkpack(set_wmap_borders_button,side="left",anchor="n")
	wmap_borders_label <- tklabel(wmap_borders_frame,
						width=10,
						text="borders_off")
	tkpack(wmap_borders_label,side="left")
	wmap_borders_entry <- tklabel(wmap_borders_frame,
						width=8,
						relief="sunken",
						textvariable=default_wmap_borders_off)
	tkpack(wmap_borders_entry,side="left")
	wmap_borders_browse <- tkbutton(wmap_borders_frame,
						text="Edit",
						command=function() index_entry(default_wmap_borders_off))
	tkpack(wmap_borders_browse,side="left")
	tkpack(wmap_borders_frame,side="top",anchor="w",fill="x")

	# ------------------

	wmap_param_col_frame <- tkframe(tab6)
	wmap_param_col_label <- tklabel(wmap_param_col_frame,
						text="-------- parametric wafermap color settings --------")
	tkpack(wmap_param_col_label,side="left")
	tkpack(wmap_param_col_frame,side="top",anchor="w",fill="x")

	# param_col_start REVISIT.. no error checking for floating # yet!
	tclvalue(default_wmap_param_col_start_shadow) <- tclObj(default_wmap_param_col_start)
	param_col_start_frame <- tkframe(tab6)
	set_param_col_start_button <- tkcheckbutton(param_col_start_frame,
						text="Set",
						variable=defaults_set_default_wmap_param_col_start)
	tkpack(set_param_col_start_button,side="left",anchor="n")
	param_col_start_label <- tklabel(param_col_start_frame,
						width=18,
						text="default param_col_start")
	tkpack(param_col_start_label,side="left")
	param_col_start_entry <- tkentry(param_col_start_frame,
						width=10,
						background="white",
						textvariable=default_wmap_param_col_start_shadow)
	tkpack(param_col_start_entry,side="left")
	tkbind(param_col_start_entry,"<KeyRelease>",function() {
					tmp <- as.numeric(tclObj(default_wmap_param_col_start_shadow))
					if( (length(tmp)>0) && is.finite(tmp) &&
							(tmp>=0.0)  && (tmp<=1.0) ) {
						tkconfigure(param_col_start_entry,background="white")
						tclvalue(default_wmap_param_col_start) <- tmp
					} else {
						tkconfigure(param_col_start_entry,background="yellow")
					}
					tcl('update')
				})
	tkpack(param_col_start_frame,side="top",anchor="w",fill="x")

	# param_col_end
	tclvalue(default_wmap_param_col_end_shadow) <- tclObj(default_wmap_param_col_end)
	param_col_end_frame <- tkframe(tab6)
	set_param_col_end_button <- tkcheckbutton(param_col_end_frame,
						text="Set",
						variable=defaults_set_default_wmap_param_col_end)
	tkpack(set_param_col_end_button,side="left",anchor="n")
	param_col_end_label <- tklabel(param_col_end_frame,
						width=18,
						text="default param_col_end")
	tkpack(param_col_end_label,side="left")
	param_col_end_entry <- tkentry(param_col_end_frame,
						width=10,
						background="white",
						textvariable=default_wmap_param_col_end_shadow)
	tkpack(param_col_end_entry,side="left")
	tkbind(param_col_end_entry,"<KeyRelease>",function() {
					tmp <- as.numeric(tclObj(default_wmap_param_col_end_shadow))
					if( (length(tmp)>0) && is.finite(tmp) &&
							(tmp>=0.0)  && (tmp<=1.0) ) {
						tkconfigure(param_col_end_entry,background="white")
						tclvalue(default_wmap_param_col_end) <- tmp
					} else {
						tkconfigure(param_col_end_entry,background="yellow")
					}
					tcl('update')
				})
	tkpack(param_col_end_frame,side="top",anchor="w",fill="x")
	
	wmap_param_col_rev_frame <- tkframe(tab6)
	wmap_param_col_rev_button <- tkcheckbutton(wmap_param_col_rev_frame,
						text="default wmap_param_col_rev",
						variable=default_wmap_param_col_rev)
	set_wmap_param_col_rev_button <- tkcheckbutton(wmap_param_col_rev_frame,
						text="Set",
						variable=defaults_set_default_wmap_param_col_rev)
	tkpack(set_wmap_param_col_rev_button,side="left",anchor="n")
	tkpack(wmap_param_col_rev_button,side="left",anchor="n")
	tkpack(wmap_param_col_rev_frame,side="top",anchor="w",fill="x")

	# ------------------

	wmap_bin_vs_col_frame <- tkframe(tab6)
	wmap_bin_vs_col_label <- tklabel(wmap_bin_vs_col_frame,
						text="-------- sbin/hbin wafermap color settings --------")
	tkpack(wmap_bin_vs_col_label,side="left")
	tkpack(wmap_bin_vs_col_frame,side="top",anchor="w",fill="x")

	bin_vs_col_name_frame <- tkframe(tab6)
	set_bin_vs_col_name_button <- tkcheckbutton(bin_vs_col_name_frame,
						text="Set",
						variable=defaults_set_default_wmap_bin_vs_col_name)
	tkpack(set_bin_vs_col_name_button,side="left",anchor="n")
	bin_vs_col_name_label <- tklabel(bin_vs_col_name_frame,
						width=18,
						text="default bin_vs_colors")
	tkpack(bin_vs_col_name_label,side="left")
	bin_vs_col_name_entry <- tkentry(bin_vs_col_name_frame,
						width=20,
						textvariable=default_wmap_bin_vs_col_name)
	tkpack(bin_vs_col_name_entry,side="left")
	tkpack(bin_vs_col_name_frame,side="top",anchor="w",fill="x")

	# bin_vs_col_path
	bin_vs_col_path_frame <- tkframe(tab6)
	set_bin_vs_col_path_button <- tkcheckbutton(bin_vs_col_path_frame,
						text="Set",
						variable=defaults_set_default_wmap_bin_vs_col_path)
	tkpack(set_bin_vs_col_path_button,side="left",anchor="n")
	bin_vs_col_path_label <- tklabel(bin_vs_col_path_frame,
						width=18,
						text="default bin_vs_colors")
	tkpack(bin_vs_col_path_label,side="left")
	bin_vs_col_path_entry <- tkentry(bin_vs_col_path_frame,
						width=20,
						textvariable=default_wmap_bin_vs_col_path)
	tkpack(bin_vs_col_path_entry,side="left")
	tkpack(bin_vs_col_path_frame,side="top",anchor="w",fill="x")

	wmap_gen_bins_csv_frame <- tkframe(tab6)
	wmap_gen_bins_csv_button <- tkcheckbutton(wmap_gen_bins_csv_frame,
						text="default wmap_gen_bins_csv",
						variable=default_wmap_gen_bins_csv)
	set_wmap_gen_bins_csv_button <- tkcheckbutton(wmap_gen_bins_csv_frame,
						text="Set",
						variable=defaults_set_default_wmap_gen_bins_csv)
	tkpack(set_wmap_gen_bins_csv_button,side="left",anchor="n")
	tkpack(wmap_gen_bins_csv_button,side="left",anchor="n")
	tkpack(wmap_gen_bins_csv_frame,side="top",anchor="w",fill="x")

	# ------------------

	wmap_xform_frame <- tkframe(tab6)
	wmap_xform_label <- tklabel(wmap_xform_frame,
						text="-------- XformWaferMap settings --------")
	tkpack(wmap_xform_label,side="left")
	tkpack(wmap_xform_frame,side="top",anchor="w",fill="x")

	wmap_rotate_frame <- tkframe(tab6)
	set_wmap_rotate_button <- tkcheckbutton(wmap_rotate_frame,
						text="Set",
						variable=defaults_set_default_wmap_rotate_ccw)
	tkpack(set_wmap_rotate_button,side="left",anchor="n")
	wmap_rotate_label <- tklabel(wmap_rotate_frame, text=" default wmap_rotate_ccw")
	tkpack(wmap_rotate_label,side="left")
	wmap_rotate0 <- tkradiobutton(wmap_rotate_frame,
						text="0    ",
						value="0",
						variable=default_wmap_rotate_ccw)
	tkpack(wmap_rotate0,side="left")
	wmap_rotate90 <- tkradiobutton(wmap_rotate_frame,
						text="90   ",
						value="90",
						variable=default_wmap_rotate_ccw)
	tkpack(wmap_rotate90,side="left")
	wmap_rotate180 <- tkradiobutton(wmap_rotate_frame,
						text="180  ",
						value="180",
						variable=default_wmap_rotate_ccw)
	tkpack(wmap_rotate180,side="left")
	wmap_rotate270 <- tkradiobutton(wmap_rotate_frame,
						text="270  ",
						value="270",
						variable=default_wmap_rotate_ccw)
	tkpack(wmap_rotate270,side="left")
	tkpack(wmap_rotate_frame,side="top",anchor="w",fill="x")

	wmap_x_polar_frame <- tkframe(tab6)
	wmap_x_polar_button <- tkcheckbutton(wmap_x_polar_frame,
						text="default x_reverse_polarity",
						variable=default_wmap_x_rev_polarity)
	set_wmap_x_polar_button <- tkcheckbutton(wmap_x_polar_frame,
						text="Set",
						variable=defaults_set_default_wmap_x_rev_polarity)
	tkpack(set_wmap_x_polar_button,side="left",anchor="n")
	tkpack(wmap_x_polar_button,side="left",anchor="n")
	tkpack(wmap_x_polar_frame,side="top",anchor="w",fill="x")

	wmap_y_polar_frame <- tkframe(tab6)
	wmap_y_polar_button <- tkcheckbutton(wmap_y_polar_frame,
						text="default y_reverse_polarity",
						variable=default_wmap_y_rev_polarity)
	set_wmap_y_polar_button <- tkcheckbutton(wmap_y_polar_frame,
						text="Set",
						variable=defaults_set_default_wmap_y_rev_polarity)
	tkpack(set_wmap_y_polar_button,side="left",anchor="n")
	tkpack(wmap_y_polar_button,side="left",anchor="n")
	tkpack(wmap_y_polar_frame,side="top",anchor="w",fill="x")

	wmap_xshift_frame <- tkframe(tab6)
	set_wmap_xshift_button <- tkcheckbutton(wmap_xshift_frame,
						text="Set",
						variable=defaults_set_default_wmap_x_shift)
	tkpack(set_wmap_xshift_button,side="left",anchor="n")
	wmap_xshift_label <- tklabel(wmap_xshift_frame,
						width=10,
						text="x_shift")
	tkpack(wmap_xshift_label,side="left")
	wmap_xshift_entry <- tklabel(wmap_xshift_frame,
						width=10,
						relief="sunken",
						textvariable=default_wmap_x_shift)
	tkpack(wmap_xshift_entry,side="left")
	wmap_xshift_browse <- tkbutton(wmap_xshift_frame,
						text="Edit",
						command=function() integer_entry(default_wmap_x_shift))
	tkpack(wmap_xshift_browse,side="left")
	tkpack(wmap_xshift_frame,side="top",anchor="w",fill="x")

	wmap_yshift_frame <- tkframe(tab6)
	set_wmap_yshift_button <- tkcheckbutton(wmap_yshift_frame,
						text="Set",
						variable=defaults_set_default_wmap_y_shift)
	tkpack(set_wmap_yshift_button,side="left",anchor="n")
	wmap_yshift_label <- tklabel(wmap_yshift_frame,
						width=10,
						text="y_shift")
	tkpack(wmap_yshift_label,side="left")
	wmap_yshift_entry <- tklabel(wmap_yshift_frame,
						width=10,
						relief="sunken",
						textvariable=default_wmap_y_shift)
	tkpack(wmap_yshift_entry,side="left")
	wmap_yshift_browse <- tkbutton(wmap_yshift_frame,
						text="Edit",
						command=function() integer_entry(default_wmap_y_shift))
	tkpack(wmap_yshift_browse,side="left")
	tkpack(wmap_yshift_frame,side="top",anchor="w",fill="x")


	# === MergeRtdf tab ===
	union_frame <- tkframe(tab7)
	set_union_button <- tkcheckbutton(union_frame,
						text="Set",
						variable=defaults_set_default_merge_union_of_tests)
	tkpack(set_union_button,side="left",anchor="n")
	union_button <- tkcheckbutton(union_frame,
						text="default union_of_tests",
						variable=default_merge_union_of_tests)
	tkpack(union_button,side="left",anchor="n")
	tkpack(union_frame,side="top",anchor="w",fill="x")

	# === AsciiWaferMap tab ===
	awmap_sinf_frame <- tkframe(tab8)
	set_awmap_sinf_button <- tkcheckbutton(awmap_sinf_frame,
						text="Set",
						variable=defaults_set_default_ascwmap_sinf_fmt 
						)
	tkpack(set_awmap_sinf_button,side="left",anchor="n")
	awmap_sinf_button <- tkcheckbutton(awmap_sinf_frame,
						text="default sinf_fmt",
						variable=default_ascwmap_sinf_fmt)
	tkpack(awmap_sinf_button,side="left",anchor="n")
	tkpack(awmap_sinf_frame,side="top",anchor="w",fill="x")

	awmap_type_frame <- tkframe(tab8)
	set_awmap_type_button <- tkcheckbutton(awmap_type_frame,
						text="Set",
						variable=defaults_set_default_ascwmap_type 
						)
	tkpack(set_awmap_type_button,side="left",anchor="n")
	type_label <- tklabel(awmap_type_frame, text="default type")
	tkpack(type_label,side="left")
	awmap_type_sbin <- tkradiobutton(awmap_type_frame,
						text="sbin",
						value="sbin",
						variable=default_ascwmap_type)
	tkpack(awmap_type_sbin,side="left")
	awmap_type_hbin <- tkradiobutton(awmap_type_frame,
						text="hbin",
						value="hbin",
						variable=default_ascwmap_type)
	tkpack(awmap_type_hbin,side="left")
	tkpack(awmap_type_frame,side="top",anchor="w",fill="x")

	awmap_ydown_frame <- tkframe(tab8)
	set_awmap_ydown_button <- tkcheckbutton(awmap_ydown_frame,
						text="Set",
						variable=defaults_set_default_ascwmap_ydown 
						)
	tkpack(set_awmap_ydown_button,side="left",anchor="n")
	awmap_ydown_button <- tkcheckbutton(awmap_ydown_frame,
						text="default y_down",
						variable=default_ascwmap_ydown)
	tkpack(awmap_ydown_button,side="left",anchor="n")
	tkpack(awmap_ydown_frame,side="top",anchor="w",fill="x")

	awmap_xleft_frame <- tkframe(tab8)
	awmap_xleft_button <- tkcheckbutton(awmap_xleft_frame,
						text="default x_left",
						variable=default_ascwmap_xleft)
	set_awmap_xleft_button <- tkcheckbutton(awmap_xleft_frame,
						text="Set",
						variable=defaults_set_default_ascwmap_xleft)
	tkpack(set_awmap_xleft_button,side="left",anchor="n")
	tkpack(awmap_xleft_button,side="left",anchor="n")
	tkpack(awmap_xleft_frame,side="top",anchor="w",fill="x")

	notch_frame <- tkframe(tab8)
	set_notch_button <- tkcheckbutton(notch_frame,
						text="Set",
						variable=defaults_set_default_ascwmap_notch)
	tkpack(set_notch_button,side="left",anchor="n")
	notch_label <- tklabel(notch_frame, text="default notch")
	tkpack(notch_label,side="left")
	notch_s <- tkradiobutton(notch_frame,
						text="South",
						value="S",
						variable=default_ascwmap_notch)
	tkpack(notch_s,side="left")
	notch_n <- tkradiobutton(notch_frame,
						text="North",
						value="N",
						variable=default_ascwmap_notch)
	tkpack(notch_n,side="left")
	notch_w <- tkradiobutton(notch_frame,
						text="West",
						value="W",
						variable=default_ascwmap_notch)
	tkpack(notch_w,side="left")
	notch_e <- tkradiobutton(notch_frame,
						text="East",
						value="E",
						variable=default_ascwmap_notch)
	tkpack(notch_e,side="left")
	tkpack(notch_frame,side="top",anchor="w",fill="x")

	awmap_pass_frame <- tkframe(tab8)
	set_awmap_pass_button <- tkcheckbutton(awmap_pass_frame,
						text="Set",
						variable=defaults_set_default_ascwmap_pass_bins)
	tkpack(set_awmap_pass_button,side="left",anchor="n")
	awmap_pass_label <- tklabel(awmap_pass_frame,
						width=18,
						text="default pass_bins")
	tkpack(awmap_pass_label,side="left")
	awmap_pass_entry <- tklabel(awmap_pass_frame,
						width=20,
						relief="sunken",
						textvariable=default_ascwmap_pass_bins)
	tkpack(awmap_pass_entry,side="left",fill="x",expand=1)
	awmap_pass_browse <- tkbutton(awmap_pass_frame,
						text="Edit",
						command=function() indices_entry(default_ascwmap_pass_bins))
	tkpack(awmap_pass_browse,side="right")
	tkpack(awmap_pass_frame,side="top",anchor="w",fill="x")

	floor_frame <- tkframe(tab8)
	set_floor_button <- tkcheckbutton(floor_frame,
						text="Set",
						variable=defaults_set_default_ascwmap_test_floor)
	tkpack(set_floor_button,side="left",anchor="n")
	floor_label <- tklabel(floor_frame,
						width=18,
						text="default test_floor")
	tkpack(floor_label,side="left")
	floor_entry <- tkentry(floor_frame,
						width=20,
						textvariable=default_ascwmap_test_floor)
	tkpack(floor_entry,side="left")
	tkpack(floor_frame,side="top",anchor="w",fill="x")

	prod_id_frame <- tkframe(tab8)
	set_prod_id_button <- tkcheckbutton(prod_id_frame,
						text="Set",
						variable=defaults_set_default_ascwmap_product_id)
	tkpack(set_prod_id_button,side="left",anchor="n")
	prod_id_label <- tklabel(prod_id_frame,
						width=18,
						text="default product_id")
	tkpack(prod_id_label,side="left")
	prod_id_entry <- tkentry(prod_id_frame,
						width=20,
						textvariable=default_ascwmap_product_id)
	tkpack(prod_id_entry,side="left")
	tkpack(prod_id_frame,side="top",anchor="w",fill="x")

	awmap_yield_frame <- tkframe(tab8)
	awmap_yield_button <- tkcheckbutton(awmap_yield_frame,
						text="default do_yield",
						variable=default_ascwmap_yield)
	set_awmap_yield_button <- tkcheckbutton(awmap_yield_frame,
						text="Set",
						variable=defaults_set_default_ascwmap_yield)
	tkpack(set_awmap_yield_button,side="left",anchor="n")
	tkpack(awmap_yield_button,side="left",anchor="n")
	tkpack(awmap_yield_frame,side="top",anchor="w",fill="x")

	awmap_multi_bin_frame <- tkframe(tab8)
	awmap_multi_bin_button <- tkcheckbutton(awmap_multi_bin_frame,
						text="default multi_binning",
						variable=default_ascwmap_multi_bin)
	set_awmap_multi_bin_button <- tkcheckbutton(awmap_multi_bin_frame,
						text="Set",
						variable=defaults_set_default_ascwmap_multi_bin)
	tkpack(set_awmap_multi_bin_button,side="left",anchor="n")
	tkpack(awmap_multi_bin_button,side="left",anchor="n")
	tkpack(awmap_multi_bin_frame,side="top",anchor="w",fill="x")

	awmap_multi_terse_frame <- tkframe(tab8)
	awmap_multi_terse_button <- tkcheckbutton(awmap_multi_terse_frame,
						text="default multi_bin_terse",
						variable=default_ascwmap_multi_terse)
	set_awmap_multi_terse_button <- tkcheckbutton(awmap_multi_terse_frame,
						text="Set",
						variable=defaults_set_default_ascwmap_multi_terse)
	tkpack(set_awmap_multi_terse_button,side="left",anchor="n")
	tkpack(awmap_multi_terse_button,side="left",anchor="n")
	tkpack(awmap_multi_terse_frame,side="top",anchor="w",fill="x")

	awmap_skip_die_minus_frame <- tkframe(tab8)
	awmap_skip_die_minus_button <- tkcheckbutton(awmap_skip_die_minus_frame,
						text="default skip_die_minus",
						variable=default_ascwmap_skip_die_minus)
	set_awmap_skip_die_minus_button <- tkcheckbutton(awmap_skip_die_minus_frame,
						text="Set",
						variable=defaults_set_default_ascwmap_skip_die_minus)
	tkpack(set_awmap_skip_die_minus_button,side="left",anchor="n")
	tkpack(awmap_skip_die_minus_button,side="left",anchor="n")
	tkpack(awmap_skip_die_minus_frame,side="top",anchor="w",fill="x")

	mirror_frame <- tkframe(tab8)
	set_mirror_button <- tkcheckbutton(mirror_frame,
						text="Set",
						variable=defaults_set_default_ascwmap_mirror_die)
	tkpack(set_mirror_button,side="left",anchor="n")
	mirror_label <- tklabel(mirror_frame,
						width=18,
						text="default mirror_die")
	tkpack(mirror_label,side="left")
	mirror_entry <- tkentry(mirror_frame,
						width=20,
						textvariable=default_ascwmap_mirror_die)
	tkpack(mirror_entry,side="left",fill="x",expand=1)
	tkpack(mirror_frame,side="top",anchor="w",fill="x")

	xstep_frame <- tkframe(tab8)
	set_xstep_button <- tkcheckbutton(xstep_frame,
						text="Set",
						variable=defaults_set_default_ascwmap_x_step)
	tkpack(set_xstep_button,side="left",anchor="n")
	xstep_label <- tklabel(xstep_frame,
						width=18,
						text="default x_step")
	tkpack(xstep_label,side="left")
	xstep_entry <- tkentry(xstep_frame,
						width=40,
						textvariable=default_ascwmap_x_step)
	tkpack(xstep_entry,side="left")
	tkpack(xstep_frame,side="top",anchor="w",fill="x")

	ystep_frame <- tkframe(tab8)
	set_ystep_button <- tkcheckbutton(ystep_frame,
						text="Set",
						variable=defaults_set_default_ascwmap_y_step)
	tkpack(set_ystep_button,side="left",anchor="n")
	ystep_label <- tklabel(ystep_frame,
						width=18,
						text="default y_step")
	tkpack(ystep_label,side="left")
	ystep_entry <- tkentry(ystep_frame,
						width=40,
						textvariable=default_ascwmap_y_step)
	tkpack(ystep_entry,side="left")
	tkpack(ystep_frame,side="top",anchor="w",fill="x")


	# === ShrinkRetests tab ===
	use_xy_coords_frame <- tkframe(tab9)
	set_use_xy_coords_button <- tkcheckbutton(use_xy_coords_frame,
						text="Set",
						variable=defaults_set_default_use_xy_coords)
	tkpack(set_use_xy_coords_button,side="left",anchor="n")
	use_xy_coords_button <- tkcheckbutton(use_xy_coords_frame,
						text="default use_xy_coords",
						variable=default_use_xy_coords)
	tkpack(use_xy_coords_button,side="left",anchor="n")
	tkpack(use_xy_coords_frame,side="top",anchor="w",fill="x")

	# === ConvertEagleCSV tab ===
	do_etscsv_summary_frame <- tkframe(tab10)
	set_etscsv_summary_button <- tkcheckbutton(do_etscsv_summary_frame,
						text="Set",
						variable=defaults_set_default_csvETS_do_summary)
	tkpack(set_etscsv_summary_button,side="left",anchor="n")
	etscsv_summary_button <- tkcheckbutton(do_etscsv_summary_frame,
						text="default do_summary",
						variable=default_csvETS_do_summary)
	tkpack(etscsv_summary_button,side="left",anchor="n")
	tkpack(do_etscsv_summary_frame,side="top",anchor="w",fill="x")
	etscsv_just_fails_summ_frame <- tkframe(tab10)
	etscsv_set_just_fails_summ_button <- tkcheckbutton(etscsv_just_fails_summ_frame,
						text="Set",
						variable=defaults_set_default_csvETS_just_fail_tests_summary)
	tkpack(etscsv_set_just_fails_summ_button,side="left",anchor="n")
	etscsv_just_fails_summ_button <- tkcheckbutton(etscsv_just_fails_summ_frame,
						text="default just_fail_tests_summary",
						variable=default_csvETS_just_fail_tests_summary)
	tkpack(etscsv_just_fails_summ_button,side="left",anchor="n")
	tkpack(etscsv_just_fails_summ_frame,side="top",anchor="w",fill="x")
	etscsv_duplicate_testnames_frame <- tkframe(tab10)
	etscsv_set_duplicate_testnames_button <- tkcheckbutton(etscsv_duplicate_testnames_frame,
						text="Set",
						variable=defaults_set_default_csvETS_duplicate_testnames)
	tkpack(etscsv_set_duplicate_testnames_button,side="left",anchor="n")
	etscsv_duplicate_testnames_button <- tkcheckbutton(etscsv_duplicate_testnames_frame,
						text="default duplicate_testnames",
						variable=default_csvETS_duplicate_testnames)
	tkpack(etscsv_duplicate_testnames_button,side="left",anchor="n")
	tkpack(etscsv_duplicate_testnames_frame,side="top",anchor="w",fill="x")

	# === ConvertCsv tab ===
	do_convcsv_type_frame <- tkframe(tab11)
	convcsv_set_type_button <- tkcheckbutton(do_convcsv_type_frame,
						text="Set",
						variable=defaults_set_default_convcsv_type)
	tkpack(convcsv_set_type_button,side="left",anchor="n")
	convcsv_type_label <- tklabel(do_convcsv_type_frame, text="default ")
	tkpack(convcsv_type_label,side="left")
	convcsv_to_rows_eq_tests <- tkradiobutton(do_convcsv_type_frame,
						text="rtdf->csv rows=tests",
						value="r2c",
						variable=default_convcsv_type)
	tkpack(convcsv_to_rows_eq_tests,side="left")
	convcsv_to_cols_eq_tests <- tkradiobutton(do_convcsv_type_frame,
						text="rtdf->csv cols=tests",
						value="r2ct",
						variable=default_convcsv_type)
	tkpack(convcsv_to_cols_eq_tests,side="left")
	convcsv_to_rtdf <- tkradiobutton(do_convcsv_type_frame,
						text="csv->rtdf",
						value="c2r",
						variable=default_convcsv_type)
	tkpack(convcsv_to_rtdf,side="left")
	tkpack(do_convcsv_type_frame,side="top",anchor="w",fill="x")

	do_convcsv_posix_frame <- tkframe(tab11)
	convcsv_set_posix_button <- tkcheckbutton(do_convcsv_posix_frame,
						text="Set",
						variable=defaults_set_default_csv_posix_time)
	tkpack(convcsv_set_posix_button,side="left",anchor="n")
	convcsv_posix_time_button <- tkcheckbutton(do_convcsv_posix_frame,
						text="default posix_time",
						variable=default_csv_posix_time)
	tkpack(convcsv_posix_time_button,side="left",anchor="n")
	tkpack(do_convcsv_posix_frame,side="top",anchor="w",fill="x")

	do_convcsv_eq_lims_frame <- tkframe(tab11)
	convcsv_set_eq_flags_button <- tkcheckbutton(do_convcsv_eq_lims_frame,
						text="Set",
						variable=defaults_set_default_csv_lim_eq_pass_flags)
	tkpack(convcsv_set_eq_flags_button,side="left",anchor="n")
	convcsv_eq_flags_button <- tkcheckbutton(do_convcsv_eq_lims_frame,
						text="default lim_eq_flags",
						variable=default_csv_lim_eq_pass_flags)
	tkpack(convcsv_eq_flags_button,side="left",anchor="n")
	tkpack(do_convcsv_eq_lims_frame,side="top",anchor="w",fill="x")


	# === ProbeVsReprob tab ===
	do_pvsrp_type_frame <- tkframe(tab12)
	pvsrp_set_type_button <- tkcheckbutton(do_pvsrp_type_frame,
						text="Set",
						variable=defaults_set_default_pvsrp_type)
	tkpack(pvsrp_set_type_button,side="left",anchor="n")
	pvsrp_type_label <- tklabel(do_pvsrp_type_frame, text="default ")
	tkpack(pvsrp_type_label,side="left")
	pvsrp_to_sbins <- tkradiobutton(do_pvsrp_type_frame,
						text="use soft bins",
						value="sbin",
						variable=default_pvsrp_type)
	tkpack(pvsrp_to_sbins,side="left")
	pvsrp_to_hbins <- tkradiobutton(do_pvsrp_type_frame,
						text="use hard bins",
						value="hbin",
						variable=default_pvsrp_type)
	tkpack(pvsrp_to_hbins,side="left")
	tkpack(do_pvsrp_type_frame,side="top",anchor="w",fill="x")

	do_pvsrp_site_pct_frame <- tkframe(tab12)
	pvsrp_set_site_pct_button <- tkcheckbutton(do_pvsrp_site_pct_frame,
						text="Set",
						variable=defaults_set_default_pvsrp_site_pct)
	tkpack(pvsrp_set_site_pct_button,side="left",anchor="n")
	pvsrp_site_pct_button <- tkcheckbutton(do_pvsrp_site_pct_frame,
						text="default site_pct_vs_site",
						variable=default_pvsrp_site_pct)
	tkpack(pvsrp_site_pct_button,side="left",anchor="n")
	tkpack(do_pvsrp_site_pct_frame,side="top",anchor="w",fill="x")

	# === XYWid2Partid tab ===
	xyw2p_partid_frame <- tkframe(tab13)
	xyw2p_set_partid_button <- tkcheckbutton(xyw2p_partid_frame,
						text="Set",
						variable=defaults_set_default_xyw2partid_save_prev_partid)
	tkpack(xyw2p_set_partid_button,side="left",anchor="n")
	xyw2p_partid_label <- tklabel(xyw2p_partid_frame,
						width=26,
						text="default save_prev_part_id")
	tkpack(xyw2p_partid_label,side="left")
	xyw2p_partid_entry <- tkentry(xyw2p_partid_frame,
						width=20,
						textvariable=default_xyw2partid_save_prev_partid)
	tkpack(xyw2p_partid_entry,side="left")
	tkpack(xyw2p_partid_frame,side="top",anchor="w",fill="x")

	xyw2p_xcoord_frame <- tkframe(tab13)
	xyw2p_set_xcoord_button <- tkcheckbutton(xyw2p_xcoord_frame,
						text="Set",
						variable=defaults_set_default_xyw2partid_xcoord_substr)
	tkpack(xyw2p_set_xcoord_button,side="left",anchor="n")
	xyw2p_xcoord_label <- tklabel(xyw2p_xcoord_frame,
						width=26,
						text="default x_testname")
	tkpack(xyw2p_xcoord_label,side="left")
	xyw2p_xcoord_entry <- tkentry(xyw2p_xcoord_frame,
						width=20,
						textvariable=default_xyw2partid_xcoord_substr)
	tkpack(xyw2p_xcoord_entry,side="left")
	tkpack(xyw2p_xcoord_frame,side="top",anchor="w",fill="x")

	xyw2p_ycoord_frame <- tkframe(tab13)
	xyw2p_set_ycoord_button <- tkcheckbutton(xyw2p_ycoord_frame,
						text="Set",
						variable=defaults_set_default_xyw2partid_ycoord_substr)
	tkpack(xyw2p_set_ycoord_button,side="left",anchor="n")
	xyw2p_ycoord_label <- tklabel(xyw2p_ycoord_frame,
						width=26,
						text="default y_testname")
	tkpack(xyw2p_ycoord_label,side="left")
	xyw2p_ycoord_entry <- tkentry(xyw2p_ycoord_frame,
						width=20,
						textvariable=default_xyw2partid_ycoord_substr)
	tkpack(xyw2p_ycoord_entry,side="left")
	tkpack(xyw2p_ycoord_frame,side="top",anchor="w",fill="x")

	xyw2p_waferid_frame <- tkframe(tab13)
	xyw2p_set_waferid_button <- tkcheckbutton(xyw2p_waferid_frame,
						text="Set",
						variable=defaults_set_default_xyw2partid_waferid_substr)
	tkpack(xyw2p_set_waferid_button,side="left",anchor="n")
	xyw2p_waferid_label <- tklabel(xyw2p_waferid_frame,
						width=26,
						text="default w_testname")
	tkpack(xyw2p_waferid_label,side="left")
	xyw2p_waferid_entry <- tkentry(xyw2p_waferid_frame,
						width=20,
						textvariable=default_xyw2partid_waferid_substr)
	tkpack(xyw2p_waferid_entry,side="left")
	tkpack(xyw2p_waferid_frame,side="top",anchor="w",fill="x")

# add tabs to notebook
# getting too noisy, so make hidden, use listbox on left side instead of tabs
# across top

	tkadd(tabs,tab1,text=" ",state="hidden",text="Directories")
	tkadd(tabs,tab2,text=" ",state="hidden",text="Log file")
	tkadd(tabs,tab8,text=" ",state="hidden",text="AsciiWaferMap")
	tkadd(tabs,tab3,text=" ",state="hidden",text="ControlCharts")
	tkadd(tabs,tab11,text=" ",state="hidden",text="ConvertCsv")
	tkadd(tabs,tab10,text=" ",state="hidden",text="ConvertEagleCSV")
	tkadd(tabs,tab4,text=" ",state="hidden",text="ConvertStdf")
	tkadd(tabs,tab7,text=" ",state="hidden",text="MergeRtdf")
	tkadd(tabs,tab5,text=" ",state="hidden",text="PlotRtdf")
	tkadd(tabs,tab12,text=" ",state="hidden",text="ProbeVsReprobe")
	tkadd(tabs,tab9,text=" ",state="hidden",text="ShrinkRetests")
	tkadd(tabs,tab6,text=" ",state="hidden",text="WaferMap")
	tkadd(tabs,tab13,text=" ",state="hidden",text="XYWid2Partid")

	tkpack(tabs,side="top",anchor="w",fill="x")

	#tcl(tabs,"select",0)	# select first tab
	tkbind(tabs_listbox,"<<ListboxSelect>>",function() {
			my_index <- as.numeric(tclvalue(tkcurselection(tabs_listbox)))
			if(is.finite(my_index)) {
				tcl(tabs,"hide","current")
				tcl(tabs,"select",my_index)
			}
			tcl('update')
		})
			
	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -


}

