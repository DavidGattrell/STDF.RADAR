# SplitBySubstrGui.R
#
# $Id: SplitBySubstrGui.R,v 1.1 2010/11/24 01:51:51 David Exp $
#
# Tk/Tcl GUI wrapper for calling SplitBySubstr.R
# called by TkRadar.R
#
# Copyright (C) 2010 David Gattrell
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
# SplitBySubstr gui specific variables
#--------------------------------
if(!exists(".TkRadar.env")) .TkRadar.env <- new.env()

splitsubstr_count <- tclVar(1)
splitsubstr_names <- list()
splitsubstr_frames <- list()	# populated when window packed...

splitsubstr_rtdf_count <- tclVar(1)		# number of rtdf files to process
splitsubstr_rtdf_index <- tclVar(1)		# the one displayed in GUI
splitsubstr_rtdf_name <- tclVar("")		# the one displayed in GUI
splitsubstr_rtdf_names <- list()		# all the rtdf names
splitsubstr_rtdf_dirs <- list()
splitsubstr_rtdf_names[[1]] <- tclVar("")
splitsubstr_rtdf_dirs[[1]] <- tclVar("")

splitsubstr_outfile <- tclVar("")
splitsubstr_outfiles <- list()
splitsubstr_outfiles[[1]] <- tclVar("")
splitsubstr_in_dir <- tclVar("")

for (j in 1:30) {
	splitsubstr_names[[j]] <- tclVar("")
}
rm(j)



#-----------------------------------------
SplitBySubstrGui_defaults <- function(...) {

	tclvalue(splitsubstr_outfile) <- ""
	tclvalue(splitsubstr_outfiles[[1]]) <- ""
	tclvalue(splitsubstr_in_dir) <- tclObj(Rtdfs_dir)

	rtdf_name <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
	rtdf_dir <- paste(tclObj(Rtdf_dir),sep="",collapse=" ")
	tclvalue(splitsubstr_rtdf_count) <- 1
	tclvalue(splitsubstr_rtdf_index) <- 1
	tclvalue(splitsubstr_rtdf_names[[1]]) <- rtdf_name
	tclvalue(splitsubstr_rtdf_name) <- rtdf_name
	tclvalue(splitsubstr_rtdf_dirs[[1]]) <- rtdf_dir

	for (j in 1:30) {
		tclvalue(splitsubstr_names[[j]]) <- ""
	}

	my_value <- as.integer(tclObj(splitsubstr_count))
	if(my_value>1) {
		for (j in my_value:2) {
			tkpack.forget(splitsubstr_frames[[j]])
		}
	}
	tclvalue(splitsubstr_count) <- 1


}

#-----------------------------------------------------
splitsubstr_rtdf_browser <- function() {

	if(as.numeric(tclObj(Bad_Vista))>0) {
		type_str = "{{All files} *} {{RTDF Files} {.rtdf .Rtdf .Rdata}}"
	} else {
		type_str="{{RTDF Files} {.rtdf .Rtdf .Rdata}} {{All files} *}"
	}
	my_dir <- paste(tclObj(splitsubstr_in_dir),sep="",collapse=" ")

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
				tclvalue(splitsubstr_rtdf_count) <- length(names)
				
				# separate filename and path
				my_filepath = sub("/[^/]*$","",name)	
				name=sub("^.*/","",name)
				
				if (length(splitsubstr_rtdf_names)<j)  splitsubstr_rtdf_names[[j]] <<- tclVar(name)
				else  tclvalue(splitsubstr_rtdf_names[[j]]) <- name

				if (j==1) {
					index <- as.integer(tclObj(splitsubstr_rtdf_index))
					if (index>length(names))  tclvalue(splitsubstr_rtdf_index) <- 1

					first_path = my_filepath
					out_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
					if (nchar(out_dir)<1) {
						out_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
					}
					if (nchar(out_dir)<1) {
						out_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
					}
					if(my_filepath == out_dir)  tclvalue(splitsubstr_in_dir) <- ""
					else  tclvalue(splitsubstr_in_dir) <- my_filepath
				} else if (!(first_path %in% my_filepath)) {
					# nasty message... shouldn't get here...
				}

				my_rtdf <- ""
				if (length(splitsubstr_outfiles)<j)  splitsubstr_outfiles[[j]] <<- tclVar(my_rtdf)
				else  tclvalue(splitsubstr_outfiles[[j]]) <- my_rtdf
			}
		}
		index <- as.integer(tclObj(splitsubstr_rtdf_index))
		tclvalue(splitsubstr_rtdf_name) <- tclObj(splitsubstr_rtdf_names[[index]])
		tclvalue(splitsubstr_outfile) <- tclObj(splitsubstr_outfiles[[index]])
	}
}

#-----------------------------------------------------
inc_splitsubstr_index <- function() {

	my_value <- as.integer(tclObj(splitsubstr_rtdf_index))
	tclvalue(splitsubstr_rtdf_names[[my_value]]) <- tclObj(splitsubstr_rtdf_name)
	tclvalue(splitsubstr_outfiles[[my_value]]) <- tclObj(splitsubstr_outfile)

	my_count <- as.integer(tclObj(splitsubstr_rtdf_count))
	if (my_value<my_count) {
		my_value <- my_value + 1
		tclvalue(splitsubstr_rtdf_name) <- tclObj(splitsubstr_rtdf_names[[my_value]])
		tclvalue(splitsubstr_outfile) <- tclObj(splitsubstr_outfiles[[my_value]])
	} 
	tclvalue(splitsubstr_rtdf_index) <- my_value
}


#-----------------------------------------------------
dec_splitsubstr_index <- function() {

	my_value <- as.integer(tclObj(splitsubstr_rtdf_index))
	tclvalue(splitsubstr_rtdf_names[[my_value]]) <- tclObj(splitsubstr_rtdf_name)
	tclvalue(splitsubstr_outfiles[[my_value]]) <- tclObj(splitsubstr_outfile)

	if (my_value>1) {
		my_value <- my_value - 1
		tclvalue(splitsubstr_rtdf_name) <- tclObj(splitsubstr_rtdf_names[[my_value]])
		tclvalue(splitsubstr_outfile) <- tclObj(splitsubstr_outfiles[[my_value]])
	}
	tclvalue(splitsubstr_rtdf_index) <- my_value
}



#-----------------------------------------------------
inc_splitsubstr_names <- function() {

	my_value <- as.integer(tclObj(splitsubstr_count))
	if (my_value<30) {
		my_value <- my_value + 1
		tkpack(splitsubstr_frames[[my_value]],side="top",anchor="w",fill="x")
	}
	tclvalue(splitsubstr_count) <- my_value
}


#-----------------------------------------------------
dec_splitsubstr_names <- function() {

	my_value <- as.integer(tclObj(splitsubstr_count))
	if (my_value>1) {
		tkpack.forget(splitsubstr_frames[[my_value]])
		my_value <- my_value - 1
	}
	tclvalue(splitsubstr_count) <- my_value
}


#-----------------------------------------------------
run_SplitBySubstr <-function(done=FALSE,...) {

	my_value <- as.integer(tclObj(splitsubstr_rtdf_index))
	tclvalue(splitsubstr_rtdf_names[[my_value]]) <- tclObj(splitsubstr_rtdf_name)
	tclvalue(splitsubstr_outfiles[[my_value]]) <- tclObj(splitsubstr_outfile)

	rtdf_count <- as.integer(tclObj(splitsubstr_rtdf_count))

	count_ <- as.integer(tclObj(splitsubstr_count))
	substrs_ <- vector()
	for (j in 1:count_) {
		substrs_[j] <- paste(tclObj(splitsubstr_names[[j]]),sep="",collapse=" ")
	}
	output_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
	full_path = output_dir
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	setwd(full_path)

	in_dir_ <- paste(tclObj(splitsubstr_rtdf_dirs[[1]]),sep="",collapse=" ")

	# now loop the call to SplitSubstr for each RTDF file...
	#----------------------------------------------------------
	for (k in 1:rtdf_count) {
		in_file_ <- paste(tclObj(splitsubstr_rtdf_names[[k]]),sep="",collapse=" ")

		out_rtdf <- paste(tclObj(splitsubstr_outfiles[[k]]),sep="",collapse=" ")


		my_expr = substitute(
			SplitBySubstr(	in_file=in_file_,
						substrs=substrs_,
						out_file=out_rtdf,
						in_dir=in_dir_)
		)
		tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
		tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
		my_command = sprintf("%s\n",deparse(my_expr))
		my_cmnd = "SplitBySubstr(...)\n"
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
	}
	cat("Finished!\n")

	if(done>0) {
		splitbysubstr_win <- get("splitbysubstr_win",envir=.TkRadar.wins)
		tkdestroy(splitbysubstr_win)
	}
}


#-----------------------------------------
SplitBySubstrGui <- function(...) {

	SplitBySubstrGui_defaults()		# initialize variables...
	splitbysubstr_win <- tktoplevel()
	assign("splitbysubstr_win",splitbysubstr_win,envir=.TkRadar.wins)
	tkwm.title(splitbysubstr_win, "SplitBySubstr")

	bottom_row <- tkframe(splitbysubstr_win)
	default_button <- tkbutton(bottom_row,
						text="DEFAULTS",
						#anchor="w",
						width=12,
						command=SplitBySubstrGui_defaults)
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
						text="RUN & QUIT",
						#anchor="w",
						width=12,
						command=function() run_SplitBySubstr(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
						text="QUIT",
						#anchor="w",
						width=12,
						command=function() tkdestroy(splitbysubstr_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
						text="RUN",
						#anchor="w",
						width=12,
						command=function() run_SplitBySubstr(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	dir_entry_frame <- tkframe(splitbysubstr_win)
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

	indir_entry_frame <- tkframe(splitbysubstr_win)
	indir_entry_label <- tklabel(indir_entry_frame,
						width=10,
						text="in rtdf dir")
	tkpack(indir_entry_label,side="left")
	indir_entry <- tklabel(indir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=splitsubstr_in_dir)
	tkpack(indir_entry,side="left",fill="x",expand=1)
	indir_browse <- tkbutton(indir_entry_frame,
						text="Browse",
						command=function() dir_browser(splitsubstr_in_dir))
	tkpack(indir_browse,side="right")
	tkpack(indir_entry_frame,side="top",anchor="w",fill="x")

	multiple_frame <- tkframe(splitbysubstr_win)
	multiple_label <- tklabel(multiple_frame,
						#width=10,
						text="file")
	tkpack(multiple_label,side="left")
	index_value <- tklabel(multiple_frame,
						width=3,
						relief="sunken",
						textvariable=splitsubstr_rtdf_index)
	tkpack(index_value,side="left")
	multiple_label2 <- tklabel(multiple_frame,
						#width=10,
						text="of")
	tkpack(multiple_label2,side="left")
	count_value <- tklabel(multiple_frame,
						width=3,
						relief="sunken",
						textvariable=splitsubstr_rtdf_count)
	tkpack(count_value,side="left")
	index_minus <- tkbutton(multiple_frame,
						text="-",
						command=function() dec_splitsubstr_index())
	tkpack(index_minus,side="left")
	index_plus <- tkbutton(multiple_frame,
						text="+",
						command=function() inc_splitsubstr_index())
	tkpack(index_plus,side="left")
	tkpack(multiple_frame,side="top",anchor="w",fill="x")

	in_entry_frame <- tkframe(splitbysubstr_win)
	in_entry_label <- tklabel(in_entry_frame,
						width=10,
						text="in_file")
	tkpack(in_entry_label,side="left")
	in_entry <- tkentry(in_entry_frame,
						width=50,
						textvariable=splitsubstr_rtdf_name)
	tkpack(in_entry,side="left",fill="x",expand=1)
	in_browse <- tkbutton(in_entry_frame,
						text="Browse",
						command=function() splitsubstr_rtdf_browser())
	tkpack(in_browse,side="right")
	tkpack(in_entry_frame,side="top",anchor="w",fill="x")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	substr_frame <- tkframe(splitbysubstr_win)
	substr_label <- tklabel(substr_frame,
						#width=10,
						text="number of strings")
	tkpack(substr_label,side="left")
	substr_value <- tklabel(substr_frame,
						width=3,
						relief="sunken",
						textvariable=splitsubstr_count)
	tkpack(substr_value,side="left")
	substr_plus <- tkbutton(substr_frame,
						text="+",
						command=inc_splitsubstr_names)
	tkpack(substr_plus,side="left")
	substr_minus <- tkbutton(substr_frame,
						text="-",
						command=dec_splitsubstr_names)
	tkpack(substr_minus,side="left")
	tkpack(substr_frame,side="top",anchor="w",fill="x")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	outfile_entry_frame <- tkframe(splitbysubstr_win)
	outfile_entry_label <- tklabel(outfile_entry_frame,
						width=10,
						text="out_file")
	tkpack(outfile_entry_label,side="left")
	outfile_entry <- tkentry(outfile_entry_frame,
						width=50,
						textvariable=splitsubstr_outfile)
	tkpack(outfile_entry,side="left",fill="x",expand=1)
	outfile_browse <- tkbutton(outfile_entry_frame,
						text="Browse",
						command=function() rtdf_browser(splitsubstr_outfile,Rtdfs_dir,output=TRUE))
	tkpack(outfile_browse,side="right")
	tkpack(outfile_entry_frame,side="bottom",anchor="w",fill="x")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	for (j in 1:30) {
		substr_name_frame <- tkframe(splitbysubstr_win)
		my_text = as.character(j)
		substr_entry_label <- tklabel(substr_name_frame,
						width=5,
						anchor="e",
						text=my_text)
		tkpack(substr_entry_label,side="left")
		substr_entry <- tkentry(substr_name_frame,
						width=20,
						textvariable=splitsubstr_names[[j]])
		tkpack(substr_entry,side="left",fill="x")

		splitsubstr_frames[[j]] <<- substr_name_frame
	}

	my_sets <- as.integer(tclObj(splitsubstr_count))

	for (j in 1:my_sets)  tkpack(splitsubstr_frames[[j]],side="top",anchor="w",fill="x")

}


