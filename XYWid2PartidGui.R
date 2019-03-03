# XYWid2PartidGui.R
#
# $Id: XYWid2PartidGui.R,v 1.1 2017/10/17 02:19:19 david Exp $
#
# Tk/Tcl GUI wrapper for calling XYWid2Partid.R
# called by TkRadar.R
#
# Copyright (C) 2017 David Gattrell
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
# XYWid2Partid gui specific variables
#--------------------------------------
if(!exists(".TkRadar.env")) .TkRadar.env <- new.env()

xyw2partid_rtdf_count <- tclVar(1)		# number of rtdf files to process
xyw2partid_rtdf_index <- tclVar(1)		# the one displayed in GUI
xyw2partid_rtdf_name <- tclVar("")		# the one displayed in GUI
xyw2partid_rtdf_names <- list()		# all the rtdf names
xyw2partid_rtdf_dirs <- list()
xyw2partid_rtdf_names[[1]] <- tclVar("")
xyw2partid_rtdf_dirs[[1]] <- tclVar("")

xyw2partid_outfile <- tclVar("")
xyw2partid_outfiles <- list()
xyw2partid_outfiles[[1]] <- tclVar("")
xyw2partid_in_dir <- tclVar("")

xyw2partid_from_params <- tclVar(0)
xyw2partid_save_prev_partid <- tclVar("")
xyw2partid_xcoord_substr <- tclVar("")
xyw2partid_ycoord_substr <- tclVar("")
xyw2partid_waferid_substr <- tclVar("")

# these defaults can be controlled in the .Rprofile file,
# or in a settings file ... per user customize
default_xyw2partid_save_prev_partid <- tclVar("orig_partid")
default_xyw2partid_xcoord_substr <- tclVar("X_COORD")
default_xyw2partid_ycoord_substr <- tclVar("Y_COORD")
default_xyw2partid_waferid_substr <- tclVar("WAFER_ID")


#-----------------------------------------
XYWid2PartidGui_defaults <- function(...) {

	tclvalue(xyw2partid_outfile) <- ""
	tclvalue(xyw2partid_outfiles[[1]]) <- ""
	tclvalue(xyw2partid_in_dir) <- tclObj(Rtdfs_dir)

	rtdf_name <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
	rtdf_dir <- paste(tclObj(Rtdf_dir),sep="",collapse=" ")
	tclvalue(xyw2partid_rtdf_count) <- 1
	tclvalue(xyw2partid_rtdf_index) <- 1
	tclvalue(xyw2partid_rtdf_names[[1]]) <- rtdf_name
	tclvalue(xyw2partid_rtdf_name) <- rtdf_name
	tclvalue(xyw2partid_rtdf_dirs[[1]]) <- rtdf_dir

	tclvalue(xyw2partid_from_params) <- 0
	tclvalue(xyw2partid_save_prev_partid) <- tclObj(default_xyw2partid_save_prev_partid)
	tclvalue(xyw2partid_xcoord_substr) <- tclObj(default_xyw2partid_xcoord_substr)
	tclvalue(xyw2partid_ycoord_substr) <- tclObj(default_xyw2partid_ycoord_substr)
	tclvalue(xyw2partid_waferid_substr) <- tclObj(default_xyw2partid_waferid_substr)

}

#-----------------------------------------------------
xyw2partid_rtdf_browser <- function() {

	if(as.numeric(tclObj(Bad_Vista))>0) {
		type_str = "{{All files} *} {{RTDF Files} {.rtdf .Rtdf .Rdata}}"
	} else {
		type_str="{{RTDF Files} {.rtdf .Rtdf .Rdata}} {{All files} *}"
	}
	my_dir <- paste(tclObj(xyw2partid_in_dir),sep="",collapse=" ")

	if (nchar(my_dir)<1) {
		my_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
	}
	if (nchar(my_dir)<1) {
		my_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(my_dir)<1) {
		my_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	name <- tclvalue(tkgetOpenFile(
			filetypes=type_str,
			initialdir=my_dir,multiple=TRUE))

	names = split_multiple_filenames(name)

	if(length(names)>0) {
		for (j in 1:length(names)) {
			name = names[j]
			if (nchar(name)>0) {
				tclvalue(xyw2partid_rtdf_count) <- length(names)
				
				# separate filename and path
				my_filepath = sub("/[^/]*$","",name)	
				name=sub("^.*/","",name)
				
				if (length(xyw2partid_rtdf_names)<j)  xyw2partid_rtdf_names[[j]] <<- tclVar(name)
				else  tclvalue(xyw2partid_rtdf_names[[j]]) <- name

				if (j==1) {
					index <- as.integer(tclObj(xyw2partid_rtdf_index))
					if (index>length(names))  tclvalue(xyw2partid_rtdf_index) <- 1

					first_path = my_filepath
					out_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
					if (nchar(out_dir)<1) {
						out_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
					}
					if (nchar(out_dir)<1) {
						out_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
					}
					if(my_filepath == out_dir)  tclvalue(xyw2partid_in_dir) <- ""
					else  tclvalue(xyw2partid_in_dir) <- my_filepath
				} else if (!(first_path %in% my_filepath)) {
					# nasty message... shouldn't get here...
				}

				my_rtdf <- ""
				if (length(xyw2partid_outfiles)<j)  xyw2partid_outfiles[[j]] <<- tclVar(my_rtdf)
				else  tclvalue(xyw2partid_outfiles[[j]]) <- my_rtdf
			}
		}
		index <- as.integer(tclObj(xyw2partid_rtdf_index))
		tclvalue(xyw2partid_rtdf_name) <- tclObj(xyw2partid_rtdf_names[[index]])
		tclvalue(xyw2partid_outfile) <- tclObj(xyw2partid_outfiles[[index]])
	}
}

#-----------------------------------------------------
inc_xyw2partid_index <- function() {

	my_value <- as.integer(tclObj(xyw2partid_rtdf_index))
	tclvalue(xyw2partid_rtdf_names[[my_value]]) <- tclObj(xyw2partid_rtdf_name)
	tclvalue(xyw2partid_outfiles[[my_value]]) <- tclObj(xyw2partid_outfile)

	my_count <- as.integer(tclObj(xyw2partid_rtdf_count))
	if (my_value<my_count) {
		my_value <- my_value + 1
		tclvalue(xyw2partid_rtdf_name) <- tclObj(xyw2partid_rtdf_names[[my_value]])
		tclvalue(xyw2partid_outfile) <- tclObj(xyw2partid_outfiles[[my_value]])
	} 
	tclvalue(xyw2partid_rtdf_index) <- my_value
}


#-----------------------------------------------------
dec_xyw2partid_index <- function() {

	my_value <- as.integer(tclObj(xyw2partid_rtdf_index))
	tclvalue(xyw2partid_rtdf_names[[my_value]]) <- tclObj(xyw2partid_rtdf_name)
	tclvalue(xyw2partid_outfiles[[my_value]]) <- tclObj(xyw2partid_outfile)

	if (my_value>1) {
		my_value <- my_value - 1
		tclvalue(xyw2partid_rtdf_name) <- tclObj(xyw2partid_rtdf_names[[my_value]])
		tclvalue(xyw2partid_outfile) <- tclObj(xyw2partid_outfiles[[my_value]])
	}
	tclvalue(xyw2partid_rtdf_index) <- my_value
}



#-----------------------------------------------------
run_XYWid2Partid <-function(done=FALSE,...) {

	my_value <- as.integer(tclObj(xyw2partid_rtdf_index))
	tclvalue(xyw2partid_rtdf_names[[my_value]]) <- tclObj(xyw2partid_rtdf_name)
	tclvalue(xyw2partid_outfiles[[my_value]]) <- tclObj(xyw2partid_outfile)

	rtdf_count <- as.integer(tclObj(xyw2partid_rtdf_count))

	from_params_ <- as.logical(tclObj(xyw2partid_from_params))
	prev_part_id_ <- paste(tclObj(xyw2partid_save_prev_partid),sep="",collapse=" ")
	x_test_substr_ <- paste(tclObj(xyw2partid_xcoord_substr),sep="",collapse=" ")
	y_test_substr_ <- paste(tclObj(xyw2partid_ycoord_substr),sep="",collapse=" ")
	w_test_substr_ <- paste(tclObj(xyw2partid_waferid_substr),sep="",collapse=" ")

	output_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
	full_path = output_dir
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	setwd(full_path)

	in_dir_ <- paste(tclObj(xyw2partid_rtdf_dirs[[1]]),sep="",collapse=" ")

	# now loop the call to XYWid2Partid for each RTDF file...
	#----------------------------------------------------------
	for (k in 1:rtdf_count) {
		in_file_ <- paste(tclObj(xyw2partid_rtdf_names[[k]]),sep="",collapse=" ")

		out_rtdf <- paste(tclObj(xyw2partid_outfiles[[k]]),sep="",collapse=" ")


		my_expr = substitute(
			XYWid2Partid(	in_rtdf=in_file_,
						out_rtdf=out_rtdf,
						in_dir=in_dir_,
						from_params=from_params_,
						save_prev_part_id=prev_part_id_,
						x_test_substr=x_test_substr_,
						y_test_substr=y_test_substr_,
						w_test_substr=w_test_substr_)
		)
		tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
		tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
		my_command = sprintf("%s\n",deparse(my_expr))
		my_cmnd = "XYWid2Partid(...)\n"
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
	}
	#cat("Finished!\n")

	tclvalue(Rtdf_name) <- out_rtdf
	tclvalue(Rtdf_dir) <- output_dir

	if(done>0) {
		xyw2partid_win <- get("xyw2partid_win",envir=.TkRadar.wins)
		tkdestroy(xyw2partid_win)
	}
}


#-----------------------------------------
XYWid2PartidGui <- function(...) {

	XYWid2PartidGui_defaults()		# initialize variables...
	xyw2partid_win <- tktoplevel()
	assign("xyw2partid_win",xyw2partid_win,envir=.TkRadar.wins)
	tkwm.title(xyw2partid_win, "XYWid2Partid")

	# these get reset by Default button, so need to be defined first...
	xname_frame <- tkframe(xyw2partid_win)
	xname_entry <- tkentry(xname_frame,
						width=24,
						background="white",
						disabledbackground="grey",
						state="disabled",
						textvariable=xyw2partid_xcoord_substr
						)
	yname_frame <- tkframe(xyw2partid_win)
	yname_entry <- tkentry(yname_frame,
						width=24,
						background="white",
						disabledbackground="grey",
						state="disabled",
						textvariable=xyw2partid_ycoord_substr
						)
	wname_frame <- tkframe(xyw2partid_win)
	wname_entry <- tkentry(wname_frame,
						width=24,
						background="white",
						disabledbackground="grey",
						state="disabled",
						textvariable=xyw2partid_waferid_substr
						)

	#... now typical bottom row definition, but with modified Defaults...
	bottom_row <- tkframe(xyw2partid_win)
	default_button <- tkbutton(bottom_row,
						text="DEFAULTS",
						#anchor="w",
						width=12,
						command=function() {
							XYWid2PartidGui_defaults()
							tkconfigure(xname_entry,state="disabled")
							tkconfigure(yname_entry,state="disabled")
							tkconfigure(wname_entry,state="disabled")
							tkconfigure(xname_entry,background="white")
							tkconfigure(yname_entry,background="white")
							tkconfigure(wname_entry,background="white")
						})
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
						text="RUN & QUIT",
						#anchor="w",
						width=12,
						command=function() run_XYWid2Partid(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
						text="QUIT",
						#anchor="w",
						width=12,
						command=function() tkdestroy(xyw2partid_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
						text="RUN",
						#anchor="w",
						width=12,
						command=function() run_XYWid2Partid(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	dir_entry_frame <- tkframe(xyw2partid_win)
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

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	indir_entry_frame <- tkframe(xyw2partid_win)
	indir_entry_label <- tklabel(indir_entry_frame,
						width=10,
						text="in rtdf dir")
	tkpack(indir_entry_label,side="left")
	indir_entry <- tklabel(indir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=xyw2partid_in_dir)
	tkpack(indir_entry,side="left",fill="x",expand=1)
	indir_browse <- tkbutton(indir_entry_frame,
						text="Browse",
						command=function() dir_browser(xyw2partid_in_dir))
	tkpack(indir_browse,side="right")
	tkpack(indir_entry_frame,side="top",anchor="w",fill="x")

	multiple_frame <- tkframe(xyw2partid_win)
	multiple_label <- tklabel(multiple_frame,
						#width=10,
						text="file")
	tkpack(multiple_label,side="left")
	index_value <- tklabel(multiple_frame,
						width=3,
						relief="sunken",
						textvariable=xyw2partid_rtdf_index)
	tkpack(index_value,side="left")
	multiple_label2 <- tklabel(multiple_frame,
						#width=10,
						text="of")
	tkpack(multiple_label2,side="left")
	count_value <- tklabel(multiple_frame,
						width=3,
						relief="sunken",
						textvariable=xyw2partid_rtdf_count)
	tkpack(count_value,side="left")
	index_minus <- tkbutton(multiple_frame,
						text="-",
						command=function() dec_xyw2partid_index())
	tkpack(index_minus,side="left")
	index_plus <- tkbutton(multiple_frame,
						text="+",
						command=function() inc_xyw2partid_index())
	tkpack(index_plus,side="left")
	tkpack(multiple_frame,side="top",anchor="w",fill="x")

	in_entry_frame <- tkframe(xyw2partid_win)
	in_entry_label <- tklabel(in_entry_frame,
						width=10,
						text="in_file")
	tkpack(in_entry_label,side="left")
	in_entry <- tkentry(in_entry_frame,
						width=50,
						textvariable=xyw2partid_rtdf_name)
	tkpack(in_entry,side="left",fill="x",expand=1)
	in_browse <- tkbutton(in_entry_frame,
						text="Browse",
						command=function() xyw2partid_rtdf_browser())
	tkpack(in_browse,side="right")
	tkpack(in_entry_frame,side="top",anchor="w",fill="x")

	outfile_entry_frame <- tkframe(xyw2partid_win)
	outfile_entry_label <- tklabel(outfile_entry_frame,
						width=10,
						text="out_file")
	tkpack(outfile_entry_label,side="left")
	outfile_entry <- tkentry(outfile_entry_frame,
						width=50,
						textvariable=xyw2partid_outfile)
	tkpack(outfile_entry,side="left",fill="x",expand=1)
	outfile_browse <- tkbutton(outfile_entry_frame,
						text="Browse",
						command=function() rtdf_browser(xyw2partid_outfile,Rtdfs_dir,output=TRUE))
	tkpack(outfile_browse,side="right")
	tkpack(outfile_entry_frame,side="top",anchor="w",fill="x")
	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	prev_partid_frame <- tkframe(xyw2partid_win)
	prev_partid_label <- tklabel(prev_partid_frame,
						width=18,
						text="save_prev_part_id")
	prev_partid_entry <- tkentry(prev_partid_frame,
						width=24,
						textvariable=xyw2partid_save_prev_partid
						)
	tkpack(prev_partid_label,side="left",anchor="n")
	tkpack(prev_partid_entry,side="left",anchor="n")
	tkpack(prev_partid_frame,side="top",anchor="w",fill="x")

	use_params_button <- tkcheckbutton(xyw2partid_win,
						text="from_params",
						variable=xyw2partid_from_params,
						command=function() {
							ok <- as.logical(tclObj(xyw2partid_from_params))
							if(ok) {
								tkconfigure(xname_entry,state="normal")
								tkconfigure(yname_entry,state="normal")
								tkconfigure(wname_entry,state="normal")
							} else {
								tkconfigure(xname_entry,state="disabled")
								tkconfigure(yname_entry,state="disabled")
								tkconfigure(wname_entry,state="disabled")
							}
						})
	tkpack(use_params_button,side="top",anchor="w")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	xname_label <- tklabel(xname_frame,
					width=12,
					text="x_test_substr")
	tkpack(xname_label,side="left",anchor="n")
	tkpack(xname_entry,side="left",anchor="w")
	tkpack(xname_frame,side="top",anchor="w",fill="x")

	yname_label <- tklabel(yname_frame,
					width=12,
					text="y_test_substr")
	tkpack(yname_label,side="left",anchor="n")
	tkpack(yname_entry,side="left",anchor="w")
	tkpack(yname_frame,side="top",anchor="w",fill="x")

	wname_label <- tklabel(wname_frame,
					width=12,
					text="w_test_substr")
	tkpack(wname_label,side="left",anchor="n")
	tkpack(wname_entry,side="left",anchor="w")
	tkpack(wname_frame,side="top",anchor="w",fill="x")



}


