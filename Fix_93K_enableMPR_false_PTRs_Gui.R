# Fix_93K_enableMPR_false_PTRs_Gui.R
#
# $Id: Fix_93K_enableMPR_false_PTRs_Gui.R,v 1.1 2022/08/24 01:00:29 david Exp $
#
# Tk/Tcl GUI wrapper for calling ShrinkRetests.R
# called by TkRadar.R
#
# Copyright (C) 2022 David Gattrell
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
# Fix_93K_enableMPR_false_PTRs_Gui specific variables
#-----------------------------------------
if(!exists(".TkRadar.env")) .TkRadar.env <- new.env()

f93k_in_name <- tclVar("")			
f93k_out_name <- tclVar("")	

f93k_append_mpr <- tclVar(1)	# auto create output names as "..._mpr.rtdf"

f93k_file_count <- tclVar(1)
f93k_file_index <- tclVar(1)
f93k_in_names <- list()
f93k_out_names <- list()
f93k_in_names[[1]] <- tclVar("")
f93k_out_names[[1]] <- tclVar("")
f93k_in_dir <- tclVar("")

# these defaults can be controlled in the settings file or the .Rprofile file:
default_f93k_append_mpr <- tclVar(1)				# per user customizing

#----------------------------------------------------
Fix_93K_enableMPR_false_PTRs_Gui_defaults <- function() {

	in_name <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
	tclvalue(f93k_in_name) <- in_name

	append_mpr_flag <- as.logical(tclObj(default_f93k_append_mpr))
	tclvalue(f93k_append_mpr) <- append_mpr_flag

	out_name = ""
	if(append_mpr_flag) {
		if(nchar(in_name)>0) {
			out_name = sub("(.rtdf)?(.Rtdf)?$","_mpr.rtdf",in_name)
		} 
	}
	tclvalue(f93k_out_name) <- out_name
	tclvalue(f93k_file_count) <- 1
	tclvalue(f93k_file_index) <- 1
	tclvalue(f93k_in_names[[1]]) <- tclVar("")
	tclvalue(f93k_out_names[[1]]) <- tclVar("")

	tclvalue(f93k_in_dir) <- tclObj(Rtdfs_dir)
}


#----------------------------------------------------
f93k_inc_index <- function() {
	my_value <- as.integer(tclObj(f93k_file_index))
	tclvalue(f93k_in_names[[my_value]]) <- tclObj(f93k_in_name)
	tclvalue(f93k_out_names[[my_value]]) <- tclObj(f93k_out_name)

	my_count <- as.integer(tclObj(f93k_file_count))
	if (my_value<my_count) {
		my_value <- my_value + 1
		tclvalue(f93k_in_name) <- tclObj(f93k_in_names[[my_value]])
		tclvalue(f93k_out_name) <- tclObj(f93k_out_names[[my_value]])
	} 
	tclvalue(f93k_file_index) <- my_value
}

#-----------------------------------------------------
f93k_dec_index <- function() {
	my_value <- as.integer(tclObj(f93k_file_index))
	tclvalue(f93k_in_names[[my_value]]) <- tclObj(f93k_in_name)
	tclvalue(f93k_out_names[[my_value]]) <- tclObj(f93k_out_name)

	if (my_value>1) {
		my_value <- my_value - 1
		tclvalue(f93k_in_name) <- tclObj(f93k_in_names[[my_value]])
		tclvalue(f93k_out_name) <- tclObj(f93k_out_names[[my_value]])
	}
	tclvalue(f93k_file_index) <- my_value
}

#----------------------------------------------------
f93k_in_rtdf_browser <-function() {
	init_dir = paste(tclObj(f93k_in_dir),sep="",collapse=" ")
	if (nchar(init_dir)<1) {
		init_dir = paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
	}
	if (nchar(init_dir)<1) {
		init_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(init_dir)<1) {
		init_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}

	if(as.numeric(tclObj(Bad_Vista))>0) {
		my_str = "{{All files} *} {{RTDF Files} {.rtdf .Rtdf .Rdata}}"
	} else {
		my_str = "{{RTDF Files} {.rtdf .Rtdf .Rdata}} {{All files} *}"
	}
	if (nchar(init_dir)>0) {
		name <- tclvalue(tkgetOpenFile(filetypes=my_str,
				initialdir=init_dir,multiple=TRUE))
	} else {
		name <- tclvalue(tkgetOpenFile(filetypes=my_str,
				multiple=TRUE))
	}
	names = split_multiple_filenames(name)

	if(length(names)>0) {
		for (j in 1:length(names)) {
			name = names[j]
			if (nchar(name)>0) {
				tclvalue(f93k_file_count) <- length(names)

				# the tkgetOpenFile returns the full path!
				# separate filename and path
				my_filepath = sub("/[^/]*$","",name)	
				name=sub("^.*/","",name)

				# update name and path variables
				if (length(f93k_in_names)<j)  f93k_in_names[[j]] <<- tclVar(name)
				else  tclvalue(f93k_in_names[[j]]) <- name

				if (j==1) {
					index <- as.integer(tclObj(f93k_file_index))
					if (index>length(names))  tclvalue(f93k_file_index) <- 1

					tclvalue(f93k_in_dir) <- my_filepath
				}

				do_append_mpr = as.logical(tclObj(f93k_append_mpr))	
				if(do_append_mpr) {
					out_name <- sub("(.rtdf)?(.Rtdf)?$","_mpr.rtdf",name)
				}else {
					out_name <- ""
				}

				if (length(f93k_out_names)<j)  f93k_out_names[[j]] <<- tclVar(out_name)
				else  tclvalue(f93k_out_names[[j]]) <- out_name

			}
		}
		index <- as.integer(tclObj(f93k_file_index))
		tclvalue(f93k_in_name) <- tclObj(f93k_in_names[[index]])
		tclvalue(f93k_out_name) <- tclObj(f93k_out_names[[index]])
	}
}

#----------------------------------------------------
f93k_out_rtdf_browser <-function(...) {
	init_dir = paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
	if (nchar(init_dir)<1) {
		init_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(init_dir)<1) {
		init_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	if(as.numeric(tclObj(Bad_Vista))>0) {
		my_str = "{{All files} *} {{RTDF Files} {.rtdf .Rtdf .Rdata}}"
	} else {
		my_str = "{{RTDF Files} {.rtdf .Rtdf .Rdata}} {{All files} *}"
	}
	if (nchar(init_dir)>0) {
		name <- tclvalue(tkgetSaveFile(
				initialdir=init_dir,
				initialfile=orig_name,
				filetypes=my_str))
	} else {
		name <- tclvalue(tkgetSaveFile(
				initialfile=orig_name,
				filetypes=my_str))
	}
	# the tkgetSaveFile returns the full path!
	if (nchar(name)>0) {
		# separate filename and path
		my_filepath = sub("/[^/]*$","",name)	
		name=sub("^.*/","",name)

		# update name and path variables
		tclvalue(f93k_out_name) <- name
		index <- as.integer(tclObj(f93k_file_index))
		tclvalue(f93k_out_names[[index]]) <- name

		rtdfs_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
		if(my_filepath != rtdfs_dir) {
			change_Rtdfs_dir(my_filepath)
		}
	}
}

#-----------------------------------------------------
run_Fix_93K_enableMPR_false_PTRs <-function(done=FALSE,...) {

	# make sure vector is up-to-date with gui entry boxes
	my_value <- as.integer(tclObj(f93k_file_index))
	tclvalue(f93k_in_names[[my_value]]) <- tclObj(f93k_in_name)
	tclvalue(f93k_out_names[[my_value]]) <- tclObj(f93k_out_name)

	output_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
	in_dir_ <- paste(tclObj(f93k_in_dir),sep="",collapse=" ")

	# go to output directory...
	full_path = output_dir
	if (nchar(full_path)<1)  full_path <- paste(tclObj(Output_dir),sep="",collapse=" ")
	if (nchar(full_path)<1)  full_path <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	setwd(full_path)

	count <- as.integer(tclObj(f93k_file_count))
	for (j in 1:count) {
		in_file_ <- paste(tclObj(f93k_in_names[[j]]),sep="",collapse=" ")
		out_file_ <- paste(tclObj(f93k_out_names[[j]]),sep="",collapse=" ")

		my_expr = substitute(
			Fix_93K_enableMPR_false_PTRs(in_file=in_file_,out_file=out_file_,
						in_dir=in_dir_)
		)
		tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
		tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
		my_command = sprintf("%s\n",deparse(my_expr))
		my_cmnd = "Fix_93K_enableMPR_false_PTRs(...)\n"

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
		cat("...Finished!\n")
	}


	# now update Rtdf_name and Rtdf_dir...
	tclvalue(Rtdf_name) <- out_file_
	tclvalue(Rtdf_dir) <- output_dir

	if(done>0) {
		fix_93k_mpr_win <- get("fix_93k_mpr_win",envir=.TkRadar.wins)
		tkdestroy(fix_93k_mpr_win)
	}
}


#----------------------------------------------------
Fix_93K_enableMPR_false_PTRs_Gui <- function() {

	Fix_93K_enableMPR_false_PTRs_Gui_defaults()		# initialize variables...
	fix_93k_mpr_win <- tktoplevel()
	assign("fix_93k_mpr_win",fix_93k_mpr_win,envir=.TkRadar.wins)
	tkwm.title(fix_93k_mpr_win, "Fix_93K_enableMPR_false_PTRs")
		
	bottom_row <- tkframe(fix_93k_mpr_win)
	default_button <- tkbutton(bottom_row,
						text="DEFAULTS",
						#anchor="w",
						width=12,
						command=Fix_93K_enableMPR_false_PTRs_Gui_defaults)
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
						text="RUN & QUIT",
						#anchor="w",
						width=12,
						command=function() run_Fix_93K_enableMPR_false_PTRs(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
						text="QUIT",
						#anchor="w",
						width=12,
						command=function()tkdestroy(fix_93k_mpr_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
						text="RUN",
						#anchor="w",
						width=12,
						command=function() run_Fix_93K_enableMPR_false_PTRs(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	# - - - 

	dir_entry_frame <- tkframe(fix_93k_mpr_win)
	dir_entry_label <- tklabel(dir_entry_frame,
						width=10,
						text="Rtdfs_dir")
	tkpack(dir_entry_label,side="left")
	dir_entry <- tklabel(dir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=Rtdfs_dir)
	tkpack(dir_entry,side="left",fill="x",expand=1)
	dir_browse <- tkbutton(dir_entry_frame,
						text="Browse",
						command=function() dir_browser(Rtdfs_dir))
	tkpack(dir_browse,side="right")
	tkpack(dir_entry_frame,side="top",anchor="w",fill="x")

	in_dir_entry_frame <- tkframe(fix_93k_mpr_win)
	in_dir_entry_label <- tklabel(in_dir_entry_frame,
						width=10,
						text="in_dir")
	tkpack(in_dir_entry_label,side="left")
	in_dir_entry <- tklabel(in_dir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=f93k_in_dir)
	tkpack(in_dir_entry,side="left",fill="x",expand=1)
	in_dir_browse <- tkbutton(in_dir_entry_frame,
						text="Browse",
						command=function() dir_browser(f93k_in_dir))
	tkpack(in_dir_browse,side="right")
	tkpack(in_dir_entry_frame,side="top",anchor="w",fill="x")

	multiple_frame <- tkframe(fix_93k_mpr_win)
	multiple_label <- tklabel(multiple_frame,
						#width=10,
						text="file")
	tkpack(multiple_label,side="left")
	index_value <- tklabel(multiple_frame,
						width=3,
						relief="sunken",
						textvariable=f93k_file_index)
	tkpack(index_value,side="left")
	multiple_label2 <- tklabel(multiple_frame,
						#width=10,
						text="of")
	tkpack(multiple_label2,side="left")
	count_value <- tklabel(multiple_frame,
						width=3,
						relief="sunken",
						textvariable=f93k_file_count)
	tkpack(count_value,side="left")
	index_minus <- tkbutton(multiple_frame,
						text="-",
						command=function() ff_dec_index())
	tkpack(index_minus,side="left")
	index_plus <- tkbutton(multiple_frame,
						text="+",
						command=function() ff_inc_index())
	tkpack(index_plus,side="left")
	tkpack(multiple_frame,side="top",anchor="w",fill="x")

	in_entry_frame <- tkframe(fix_93k_mpr_win)
	in_entry_label <- tklabel(in_entry_frame,
						width=10,
						text="in_file")
	tkpack(in_entry_label,side="left")
	in_entry <- tkentry(in_entry_frame,
						width=50,
						textvariable=f93k_in_name)
	tkpack(in_entry,side="left",fill="x",expand=1)
	in_browse <- tkbutton(in_entry_frame,
						text="Browse",
						command=f93k_in_rtdf_browser)
	tkpack(in_browse,side="right")
	tkpack(in_entry_frame,side="top",anchor="w",fill="x")

	out_entry_frame <- tkframe(fix_93k_mpr_win)
	out_entry_label <- tklabel(out_entry_frame,
						width=10,
						text="out_file")
	tkpack(out_entry_label,side="left")
	out_entry <- tkentry(out_entry_frame,
						width=20,
						textvariable=f93k_out_name)
	tkpack(out_entry,side="left",fill="x",expand=1)
	out_browse <- tkbutton(out_entry_frame,
						text="Browse",
						command=f93k_out_rtdf_browser)
	tkpack(out_browse,side="right")
	tkpack(out_entry_frame,side="top",anchor="w",fill="x")

	append_mpr_button <- tkcheckbutton(fix_93k_mpr_win,
						text="append_mpr",
						variable=f93k_append_mpr)
	tkpack(append_mpr_button,side="top",anchor="w")

}



