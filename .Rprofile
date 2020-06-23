#########################################################
#
# .Rprofile for RADAR version 0.6.9  10mar2019
#
#########################################################
my_dir <- getwd()
#setwd("C:/Documents and Settings/All Users/Documents/RADAR Packages/RADAR_package_0v6p4")
setwd("/home/david/RADAR/RADAR_package_0v6xxdev")

source("Radar.R")

tclvalue(.Radar$.TkRadar.env$Orig_dir) <- my_dir
setwd(my_dir)
rm(my_dir)


# to change default values, uncomment and adjust the values accordingly:
# =======================================================================
# -- TkRadar_logfile: if "", no log file written,
# -- else write timestamp and RADAR script call to filename specified
# tclvalue(.Radar$.TkRadar.env$TkRadar_logfile) <- "TkRadar.log"

# -- TkRadar_verbose:
# -- if 0, just print RADAR script name to console window
# -- if >0, print up to the first n lines of command to console window
# -- if <0, print whole command, (can be quite long) to console window
tclvalue(.Radar$.TkRadar.env$TkRadar_verbose) <- -1		# default is 1

# AsciiWaferMapGui defaults
# -------------------------
#tclvalue(.Radar$.TkRadar.env$default_ascwmap_type) <- "sbin"
#tclvalue(.Radar$.TkRadar.env$default_ascwmap_xleft) <- 0
#tclvalue(.Radar$.TkRadar.env$default_ascwmap_ydown) <- 0
#tclvalue(.Radar$.TkRadar.env$default_ascwmap_notch) <- "S"
#tclvalue(.Radar$.TkRadar.env$default_ascwmap_pass_bins) <- -1
#tclvalue(.Radar$.TkRadar.env$default_ascwmap_test_floor) <- ""
#tclvalue(.Radar$.TkRadar.env$default_ascwmap_product_id) <- ""
#tclvalue(.Radar$.TkRadar.env$default_ascwmap_yield) <- 1
#tclvalue(.Radar$.TkRadar.env$default_ascwmap_multi_bin) <- 0
#tclvalue(.Radar$.TkRadar.env$default_ascwmap_multi_terse) <- 0
#tclvalue(.Radar$.TkRadar.env$default_ascwmap_skip_die_minus) <- 1

# ControlChartsGui defaults
# -------------------------
#tclvalue(.Radar$.TkRadar.env$default_do_western_electric) <- 0
#tclvalue(.Radar$.TkRadar.env$default_control_start_n) <- 1
#tclvalue(.Radar$.TkRadar.env$default_control_count_n) <- 50
#tclvalue(.Radar$.TkRadar.env$default_control_plots_per_page) <- 7
#tclvalue(.Radar$.TkRadar.env$default_control_landscape) <- 0
#tclvalue(.Radar$.TkRadar.env$default_control_autoopen) <- 1

# ConvertCsvGui defaults
#-----------------------
#tclvalue(.Radar$.TkRadar.env$default_convcsv_type) <- "r2c"		# "r2c" or "r2ct"
#tclvalue(.Radar$.TkRadar.env$default_csv_posix_time) <- 0
#tclvalue(.Radar$.TkRadar.env$default_csv_lim_eq_pass_flags) <- 0

# ConvertStdfGui defaults
# -----------------------
#tclvalue(.Radar$.TkRadar.env$default_do_summary) <- 1
#tclvalue(.Radar$.TkRadar.env$default_just_fail_tests_summary) <- 1
#tclvalue(.Radar$.TkRadar.env$default_do_conditions) <- 0
#tclvalue(.Radar$.TkRadar.env$default_do_dtrs) <- 0
#tclvalue(.Radar$.TkRadar.env$default_duplicate_testnames) <- 0
#tclvalue(.Radar$.TkRadar.env$default_use_MPR_invalid_pf_data) <- 0
#tclvalue(.Radar$.TkRadar.env$default_ltx_ignore_testname_objects) <- 1
tclvalue(.Radar$.TkRadar.env$default_do_testflag_matrix) <- 1	# default is 0
#tclvalue(.Radar$.TkRadar.env$default_keep_alarmed_values) <- 0
#tclvalue(.Radar$.TkRadar.env$default_do_raw_tsrs) <- 0
#tclvalue(.Radar$.TkRadar.env$default_do_FTR_fail_cycle) <- 1
#tclvalue(.Radar$.TkRadar.env$default_use_testorder) <- 0
#tclvalue(.Radar$.TkRadar.env$default_save_testorder) <- 0
#tclvalue(.Radar$.TkRadar.env$default_mult_limits) <- 0

# ConvertXlsxGui defaults
# -----------------------
#tclvalue(.Radar$.TkRadar.env$python_exists <- 1	# maybe do this?

# MergeRtdfGui defaults
# ---------------------
#tclvalue(.Radar$.TkRadar.env$default_merge_union_of_tests) <- 0

# PlotRtdfGui defaults
# --------------------
tclvalue(.Radar$.TkRadar.env$default_min_plots_per_page) <- 8	# default is 6
#tclvalue(.Radar$.TkRadar.env$default_use_csv_formulas) <- 1
#tclvalue(.Radar$.TkRadar.env$default_use_OOCalc_csv) <- 1
tclvalue(.Radar$.TkRadar.env$default_add_normal_curve) <- 1		# default is 0
#tclvalue(.Radar$.TkRadar.env$default_do_robust_stats) <- 0
#tclvalue(.Radar$.TkRadar.env$default_plotrtdf_autoopen) <- 1
#tclvalue(.Radar$.TkRadar.env$default_superimpose_hist) <- 0
#tclvalue(.Radar$.TkRadar.env$default_do_norm_prob_plots) <- 0
#tclvalue(.Radar$.TkRadar.env$default_plotrtdf_to_png) <- 0
tclvalue(.Radar$.TkRadar.env$default_plot_max_tests) <- -1	# default is 2000

# ProbeVsReprobe defaults
# -------------------------
#tclvalue(.Radar$.TkRadar.env$default_pvsrp_type) <- "sbin"
#tclvalue(.Radar$.TkRadar.env$default_pvsrp_site_pct) <- 1

# ShrinkRetestsGui defaults
# -------------------------
#tclvalue(.Radar$.TkRadar.env$default_use_xy_coords) <- 0

# WaferMapGui defaults
# --------------------
#tclvalue(.Radar$.TkRadar.env$default_wmap_xleft) <- 0
#tclvalue(.Radar$.TkRadar.env$default_wmap_ydown) <- 0
#tclvalue(.Radar$.TkRadar.env$default_x_coord_alpha) <- 0
#tclvalue(.Radar$.TkRadar.env$default_panel) <- 0
#tclvalue(.Radar$.TkRadar.env$default_wmap_notch) <- "x"
#tclvalue(.Radar$.TkRadar.env$default_wmap_autoopen) <- 1

# XYWid2PartidGui defaults
# ------------------------
#tclvalue(.Radar$.TkRadar.env$default_xyw2partid_save_prev_partid) <- "orig_partid"
#tclvalue(.Radar$.TkRadar.env$default_xyw2partid_xcoord_substr) <- "X_COORD"
#tclvalue(.Radar$.TkRadar.env$default_xyw2partid_ycoord_substr) <- "Y_COORD"
#tclvalue(.Radar$.TkRadar.env$default_xyw2partid_waferid_substr) <- "WAFER_ID"

TkRadar()

