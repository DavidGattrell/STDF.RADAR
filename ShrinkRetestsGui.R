# ShrinkRetestsGui.R
#
# $Id: ShrinkRetestsGui.R,v 1.7 2023/12/20 01:55:48 david Exp $
#
# Tk/Tcl GUI wrapper for calling ShrinkRetests.R
# called by TkRadar.R
#
# Copyright (C) 2008-2010 David Gattrell
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
# ShrinkRetestsGui specific variables
#-----------------------------------------
if(!exists(".TkRadar.env")) .TkRadar.env <- new.env()

shr_in_name <- tclVar("")
shr_out_name <- tclVar("")
use_xy_coords <- tclVar(0)

shr_append_shrunk <- tclVar(1)	# auto create output names as "..._shrunk.rtdf"

shr_file_count <- tclVar(1)
shr_file_index <- tclVar(1)
shr_in_names <- list()
shr_out_names <- list()
shr_in_names[[1]] <- tclVar("")
shr_out_names[[1]] <- tclVar("")
shr_in_dir <- tclVar("")

# these defaults can be controlled in the .Rprofile file:
default_use_xy_coords <- tclVar(0)		

#----------------------------------------------------
ShrinkRetestsGui_defaults <- function() {

	tclvalue(use_xy_coords) <- tclObj(default_use_xy_coords)
	append_shrunk_flag <- 1
	tclvalue(shr_append_shrunk) <- append_shrunk_flag

	in_name <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
	tclvalue(shr_in_name) <- in_name

	out_name = "shrunk.rtdf"
	if(append_shrunk_flag) {
		if(nchar(in_name)>0) {
			out_name = sub("(.rtdf)?(.Rtdf)?$","_shrunk.rtdf",in_name)
		}
	}
	tclvalue(shr_out_name) <- out_name
	tclvalue(shr_file_count) <- 1
	tclvalue(shr_file_index) <- 1
	tclvalue(shr_in_names[[1]]) <- tclVar("")
	tclvalue(shr_out_names[[1]]) <- tclVar("")
	tclvalue(shr_in_dir) <- tclObj(Rtdfs_dir)
}


#-----------------------------------------------------
shr_inc_index <- function() {
	my_value <- as.integer(tclObj(shr_file_index))
	tclvalue(shr_in_names[[my_value]]) <- tclObj(shr_in_name)
	tclvalue(shr_out_names[[my_value]]) <- tclObj(shr_out_name)

	my_count <- as.integer(tclObj(shr_file_count))
	if (my_value<my_count) {
		my_value <- my_value + 1
		tclvalue(shr_in_name) <- tclObj(shr_in_names[[my_value]])
		tclvalue(shr_out_name) <- tclObj(shr_out_names[[my_value]])
	} 
	tclvalue(shr_file_index) <- my_value
}

#-----------------------------------------------------
shr_dec_index <- function() {
	my_value <- as.integer(tclObj(shr_file_index))
	tclvalue(shr_in_names[[my_value]]) <- tclObj(shr_in_name)
	tclvalue(shr_out_names[[my_value]]) <- tclObj(shr_out_name)

	if (my_value>1) {
		my_value <- my_value - 1
		tclvalue(shr_in_name) <- tclObj(shr_in_names[[my_value]])
		tclvalue(shr_out_name) <- tclObj(shr_out_names[[my_value]])
	}
	tclvalue(shr_file_index) <- my_value
}

#----------------------------------------------------
shr_in_rtdf_browser <-function() {

	init_dir = paste(tclObj(shr_in_dir),sep="",collapse=" ")
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
				tclvalue(shr_file_count) <- length(names)

				# the tkgetOpenFile returns the full path!
				# separate filename and path
				my_filepath = sub("/[^/]*$","",name)	
				name=sub("^.*/","",name)

				# update name and path variables
				if (length(shr_in_names)<j)  shr_in_names[[j]] <<- tclVar(name)
				else  tclvalue(shr_in_names[[j]]) <- name

				if (j==1) {
					index <- as.integer(tclObj(shr_file_index))
					if (index>length(names))  tclvalue(shr_file_index) <- 1

					tclvalue(shr_in_dir) <- my_filepath
				}

				do_append_shrunk = as.logical(tclObj(shr_append_shrunk))	
				if(do_append_shrunk) {
					out_name <- sub("(.rtdf)?(.Rtdf)?$","_shrunk.rtdf",name)
				}else {
					out_name <- ""
				}
				if (length(shr_out_names)<j)  shr_out_names[[j]] <<- tclVar(out_name)
				else  tclvalue(shr_out_names[[j]]) <- out_name

			}
		}
		index <- as.integer(tclObj(shr_file_index))
		tclvalue(shr_in_name) <- tclObj(shr_in_names[[index]])
		tclvalue(shr_out_name) <- tclObj(shr_out_names[[index]])
	}
}


#----------------------------------------------------
shr_out_rtdf_browser <-function(...) {
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
				#initialfile=orig_name,
				filetypes=my_str))
	} else {
		name <- tclvalue(tkgetSaveFile(
				#initialfile=orig_name,
				filetypes=my_str))
	}
	# the tkgetSaveFile returns the full path!
	if (nchar(name)>0) {
		# separate filename and path
		my_filepath = sub("/[^/]*$","",name)	
		name=sub("^.*/","",name)

		# update name and path variables
		tclvalue(shr_out_name) <- name
		index <- as.integer(tclObj(shr_file_index))
		tclvalue(shr_out_names[[index]]) <- name

		rtdfs_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
		if(my_filepath != rtdfs_dir) {
			change_Rtdfs_dir(my_filepath)
		}
	}
}

#----------------------------------------------------
run_ShrinkRetests <-function(done=FALSE,...) {

	# make sure vector is up-to-date with gui entry boxes
	my_value <- as.integer(tclObj(shr_file_index))
	tclvalue(shr_in_names[[my_value]]) <- tclObj(shr_in_name)
	tclvalue(shr_out_names[[my_value]]) <- tclObj(shr_out_name)

	use_xy <- as.logical(tclObj(use_xy_coords))

	output_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
	in_dir_ <- paste(tclObj(shr_in_dir),sep="",collapse=" ")

	full_path = output_dir
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	setwd(full_path)
	
	count <- as.integer(tclObj(shr_file_count))
	for (j in 1:count) {
		in_file_ <- paste(tclObj(shr_in_names[[j]]),sep="",collapse=" ")
		out_file_ <- paste(tclObj(shr_out_names[[j]]),sep="",collapse=" ")

		my_expr = substitute(
			ShrinkRetests(in_file=in_file_,out_file=out_file_,
							  in_dir=in_dir_,use_xy_coords=use_xy)
		)
		tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
		tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
		my_command = sprintf("%s\n",deparse(my_expr))
		my_cmnd = "ShrinkRetests(...)\n"
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
		shrinkretests_win <- get("shrinkretests_win",envir=.TkRadar.wins)
		tkdestroy(shrinkretests_win)
	}
}


#----------------------------------------------------
ShrinkRetestsGui <- function() {

	ShrinkRetestsGui_defaults()		# initialize variables...
	shrinkretests_win <- tktoplevel()
	assign("shrinkretests_win",shrinkretests_win,envir=.TkRadar.wins)
	tkwm.title(shrinkretests_win, "ShrinkRetests")
		
	bottom_row <- tkframe(shrinkretests_win)
	default_button <- tkbutton(bottom_row,
						text="DEFAULTS",
						#anchor="w",
						width=12,
						command=ShrinkRetestsGui_defaults)
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
						text="RUN & QUIT",
						#anchor="w",
						width=12,
						command=function() run_ShrinkRetests(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
						text="QUIT",
						#anchor="w",
						width=12,
						command=function()tkdestroy(shrinkretests_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
						text="RUN",
						#anchor="w",
						width=12,
						command=function() run_ShrinkRetests(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	# - - - 

	dir_entry_frame <- tkframe(shrinkretests_win)
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

	in_dir_entry_frame <- tkframe(shrinkretests_win)
	in_dir_entry_label <- tklabel(in_dir_entry_frame,
						width=10,
						text="in_dir")
	tkpack(in_dir_entry_label,side="left")
	in_dir_entry <- tklabel(in_dir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=shr_in_dir)
	tkpack(in_dir_entry,side="left",fill="x",expand=1)
	in_dir_browse <- tkbutton(in_dir_entry_frame,
						text="Browse",
						command=function() dir_browser(shr_in_dir))
	tkpack(in_dir_browse,side="right")
	tkpack(in_dir_entry_frame,side="top",anchor="w",fill="x")

	multiple_frame <- tkframe(shrinkretests_win)
	multiple_label <- tklabel(multiple_frame,
						#width=10,
						text="file")
	tkpack(multiple_label,side="left")
	index_value <- tklabel(multiple_frame,
						width=3,
						relief="sunken",
						textvariable=shr_file_index)
	tkpack(index_value,side="left")
	multiple_label2 <- tklabel(multiple_frame,
						#width=10,
						text="of")
	tkpack(multiple_label2,side="left")
	count_value <- tklabel(multiple_frame,
						width=3,
						relief="sunken",
						textvariable=shr_file_count)
	tkpack(count_value,side="left")
	index_minus <- tkbutton(multiple_frame,
						text="-",
						command=function() shr_dec_index())
	tkpack(index_minus,side="left")
	index_plus <- tkbutton(multiple_frame,
						text="+",
						command=function() shr_inc_index())
	tkpack(index_plus,side="left")
	tkpack(multiple_frame,side="top",anchor="w",fill="x")

	in_entry_frame <- tkframe(shrinkretests_win)
	in_entry_label <- tklabel(in_entry_frame,
						width=10,
						text="in_file")
	tkpack(in_entry_label,side="left")
	in_entry <- tkentry(in_entry_frame,
						width=50,
						textvariable=shr_in_name)
	tkpack(in_entry,side="left",fill="x",expand=1)
	in_browse <- tkbutton(in_entry_frame,
						text="Browse",
						command=shr_in_rtdf_browser)
	tkpack(in_browse,side="right")
	tkpack(in_entry_frame,side="top",anchor="w",fill="x")

	out_entry_frame <- tkframe(shrinkretests_win)
	out_entry_label <- tklabel(out_entry_frame,
						width=10,
						text="out_file")
	tkpack(out_entry_label,side="left")
	out_entry <- tkentry(out_entry_frame,
						width=20,
						textvariable=shr_out_name)
	tkpack(out_entry,side="left",fill="x",expand=1)
	out_browse <- tkbutton(out_entry_frame,
						text="Browse",
						command=shr_out_rtdf_browser)
	tkpack(out_browse,side="right")
	tkpack(out_entry_frame,side="top",anchor="w",fill="x")

	shr_append_button <- tkcheckbutton(shrinkretests_win,
						text="append_shrunk",
						variable=shr_append_shrunk)
	tkpack(shr_append_button,side="top",anchor="w")

	use_xy_button <- tkcheckbutton(shrinkretests_win,
						text="use_xy_coords",
						variable=use_xy_coords)
	tkpack(use_xy_button,side="top",anchor="w")
}



