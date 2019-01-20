# ProbeVsReprobeGui.R
#
# $Id: ProbeVsReprobeGui.R,v 1.1 2015/05/07 00:51:05 david Exp $
#
# Tk/Tcl GUI wrapper for calling ProbeVsReprobe.R
# called by TkRadar.R
#
# Copyright (C) 2015 David Gattrell
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
# ProbeVsReprobe gui specific variables
#---------------------------------------
if(!exists(".TkRadar.env")) .TkRadar.env <- new.env()

pvsrp_name <- tclVar("per_site_bin_summary.csv")
pvsrp_type <- tclVar("sbin")
pvsrp_site_pct <- tclVar(1)

# these defaults can be controlled in the .Rprofile file:
default_pvsrp_type <- tclVar("sbin")		# per user customizing
default_pvsrp_site_pct <- tclVar(1)		# per user customizing

#----------------------------------------------------
ProbeVsReprobeGui_defaults <- function(...) {
	tclvalue(pvsrp_name) <- "per_site_bin_summary.csv"
	tclvalue(pvsrp_type) <- tclObj(default_pvsrp_type)
	tclvalue(pvsrp_site_pct) <- tclObj(default_pvsrp_site_pct)
}


#----------------------------------------------------
run_ProbeVsReprobe <-function(done=FALSE,...) {
	pvsrp_rtdf <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
	pvsrp_name_ <- paste(tclObj(pvsrp_name),sep="",collapse=" ")
	pvsrp_type_ <- as.character(tclObj(pvsrp_type))
	pvsrp_site_pct_ <- as.logical(tclObj(pvsrp_site_pct))

	rtdf_dir_ <- paste(tclObj(Rtdf_dir),sep="",collapse=" ")
	output_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")

	if (nchar(output_dir)>0)  setwd(output_dir)

	my_expr = substitute(
		ProbeVsReprobe(rtdf_name=pvsrp_rtdf,out_name=pvsrp_name_,
					type=pvsrp_type_,rtdf_dir=rtdf_dir_,
					site_pct_vs_site=pvsrp_site_pct_)
	)
	tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
	tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
	my_command = sprintf("%s\n",deparse(my_expr))
	my_cmnd = "ProbeVsReprobe(...)\n"
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
		probevsreprobe_win <- get("probevsreprobe_win",envir=.TkRadar.wins)
		tkdestroy(probevsreprobe_win)
	}

}


#-----------------------------------------------------
ProbeVsReprobeGui <-function(...) {

	ProbeVsReprobeGui_defaults()		# initialize variables...
	probevsreprobe_win <- tktoplevel()
	assign("probevsreprobe_win",probevsreprobe_win,envir=.TkRadar.wins)
	tkwm.title(probevsreprobe_win, "ProbeVsReprobe")
	
	bottom_row <- tkframe(probevsreprobe_win)
	default_button <- tkbutton(bottom_row,
					text="DEFAULTS",
					#anchor="w",
					width=12,
					command=ProbeVsReprobeGui_defaults)
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
					text="RUN & QUIT",
					#anchor="w",
					width=12,
					command=function() run_ProbeVsReprobe(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
					text="QUIT",
					#anchor="w",
					width=12,
					command=function()tkdestroy(probevsreprobe_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
					text="RUN",
					#anchor="w",
					width=12,
					command=function() run_ProbeVsReprobe(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	dir_entry_frame <- tkframe(probevsreprobe_win)
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

	rtdf_entry_frame <- tkframe(probevsreprobe_win)
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

	pvsrp_entry_frame <- tkframe(probevsreprobe_win)
	pvsrp_entry_label <- tklabel(pvsrp_entry_frame,
						width=10,
						text="pvsrp_name ")
	tkpack(pvsrp_entry_label,side="left")
	pvsrp_entry <- tkentry(pvsrp_entry_frame,
						width=20,
						textvariable=pvsrp_name)
	tkpack(pvsrp_entry,side="left",fill="x",expand=1)
	pvsrp_browse <- tkbutton(pvsrp_entry_frame,
						text="Browse",
						command=function() csv_browser(pvsrp_name))
	tkpack(pvsrp_browse,side="right")
	tkpack(pvsrp_entry_frame,side="top",anchor="w",fill="x")

	type_frame <- tkframe(probevsreprobe_win)
	type_label <- tklabel(type_frame, text="type")
	tkpack(type_label,side="left")
	type_sbin <- tkradiobutton(type_frame,
						text="sbin",
						value="sbin",
						variable=pvsrp_type)
	tkpack(type_sbin,side="left")
	type_hbin <- tkradiobutton(type_frame,
						text="hbin",
						value="hbin",
						variable=pvsrp_type)
	tkpack(type_hbin,side="left")
	tkpack(type_frame,side="top",anchor="w")

	site_button <- tkcheckbutton(probevsreprobe_win,
						text="site_pct_vs_site",
						variable=pvsrp_site_pct)
	tkpack(site_button,side="top",anchor="w")

}



