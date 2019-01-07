# MergeRtdfGui.R
#
# $Id: MergeRtdfGui.R,v 1.11 2010/11/24 01:41:40 David Exp $
#
# Tk/Tcl GUI wrapper for calling MergeRtdf.R
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
# MergeRtdf gui specific variables
#--------------------------------
if(!exists(".TkRadar.env")) .TkRadar.env <- new.env()

merge_datasets <- tclVar(1)
merge_dataset_frames <- list()	# populated when window packed...
merge_rtdf_names <- list()
merge_rtdf_dirs <- list()
merge_outfile <- tclVar("merged.rtdf")
merge_union_of_tests <- tclVar(0)

in_rtdf_dir <- tclVar("")

for (j in 1:30) {
	merge_rtdf_names[[j]] <- tclVar("")
	merge_rtdf_dirs[[j]] <- tclVar("")
}

rm(j)

# these defaults can be controlled in the .Rprofile file:
default_merge_union_of_tests <- tclVar(0)



#-----------------------------------------
MergeRtdfGui_defaults <- function(...) {

	tclvalue(merge_outfile) <- "merged.rtdf"
	tclvalue(in_rtdf_dir) <- tclObj(Rtdfs_dir)

	for (j in 1:30) {
		if (j<2) {
			rtdf_name <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
			rtdf_dir <- paste(tclObj(Rtdf_dir),sep="",collapse=" ")
			tclvalue(merge_rtdf_names[[j]]) <- rtdf_name
			tclvalue(merge_rtdf_dirs[[j]]) <- rtdf_dir
		} else {
			rtdf_dir <- paste(tclObj(in_rtdf_dir),sep="",collapse=" ")
			tclvalue(merge_rtdf_names[[j]]) <- ""
			tclvalue(merge_rtdf_dirs[[j]]) <- rtdf_dir
		}
	}

	my_value <- as.integer(tclObj(merge_datasets))
	if(my_value>1) {
		for (j in my_value:2) {
			tkpack.forget(merge_dataset_frames[[j]])
		}
	}
	tclvalue(merge_datasets) <- 1

	tclvalue(merge_union_of_tests) <- tclObj(default_merge_union_of_tests)
}


#-----------------------------------------------------
merge_rtdf_browser <- function(index) {

	if(as.numeric(tclObj(Bad_Vista))>0) {
		type_str="{{All files} *} {{RTDF Files} {.rtdf .Rtdf .Rdata}}"
	} else {
		type_str="{{RTDF Files} {.rtdf .Rtdf .Rdata}} {{All files} *}"
	}
	my_dir <- paste(tclObj(in_rtdf_dir),sep="",collapse=" ")

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

	do_warning=FALSE
	idx=0
	if(length(names)>0) {
		for (j in 1:length(names)) {
			name = names[j]
			if (nchar(name)>0) {
				# separate filename and path
				my_filepath = sub("/[^/]*$","",name)	
				name=sub("^.*/","",name)
				
				idx = j+index-1
				if (idx<=30) {
					tclvalue(merge_rtdf_dirs[[idx]]) <- my_filepath
					tclvalue(merge_rtdf_names[[idx]]) <- name

					my_value <- as.integer(tclObj(merge_datasets))
					if (my_value<idx) {
						my_value <- my_value + 1
						tkpack(merge_dataset_frames[[my_value]],side="top",anchor="w",fill="x")
					}
					tclvalue(merge_datasets) <- my_value
				} else   do_warning=TRUE
			}
		}
		if (do_warning)  tkmessageBox(message="WARNING: Only the first 30 files will be used!")
	}

}


#-----------------------------------------------------
inc_merge_datasets <- function() {

	my_value <- as.integer(tclObj(merge_datasets))
	if (my_value<30) {
		my_value <- my_value + 1
		tkpack(merge_dataset_frames[[my_value]],side="top",anchor="w",fill="x")
	}
	tclvalue(merge_datasets) <- my_value
}


#-----------------------------------------------------
dec_merge_datasets <- function() {

	my_value <- as.integer(tclObj(merge_datasets))
	if (my_value>1) {
		tkpack.forget(merge_dataset_frames[[my_value]])
		my_value <- my_value - 1
	}
	tclvalue(merge_datasets) <- my_value
}


#-----------------------------------------------------
run_MergeRtdf <-function(done=FALSE,...) {

	datasets_ <- as.integer(tclObj(merge_datasets))
	rtdf_names_ <- vector()
	rtdf_dirs_ <- vector()
	for (j in 1:datasets_) {
		rtdf_names_[j] <- paste(tclObj(merge_rtdf_names[[j]]),sep="",collapse=" ")
		rtdf_dirs_[j] <- paste(tclObj(merge_rtdf_dirs[[j]]),sep="",collapse=" ")
	}
	out_rtdf <- paste(tclObj(merge_outfile),sep="",collapse=" ")
	output_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
	union_of_tests_ <- as.logical(tclObj(merge_union_of_tests))

	full_path = output_dir
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	setwd(full_path)

	my_expr = substitute(
		MergeRtdf(	in_files=rtdf_names_,
					out_file=out_rtdf,
					in_dirs=rtdf_dirs_,
					union_of_tests=union_of_tests_)
	)
	tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
	tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
	my_command = sprintf("%s\n",deparse(my_expr))
	my_cmnd = "MergeRtdf(...)\n"
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

	# now update Rtdf_name and Rtdf_dir...
	tclvalue(Rtdf_name) <- out_rtdf
	tclvalue(Rtdf_dir) <- output_dir

	if(done>0) {
		mergertdf_win <- get("mergertdf_win",envir=.TkRadar.wins)
		tkdestroy(mergertdf_win)
	}
}


#-----------------------------------------
MergeRtdfGui <- function(...) {

	MergeRtdfGui_defaults()		# initialize variables...
	mergertdf_win <- tktoplevel()
	assign("mergertdf_win",mergertdf_win,envir=.TkRadar.wins)
	tkwm.title(mergertdf_win, "MergeRtdf")

	bottom_row <- tkframe(mergertdf_win)
	default_button <- tkbutton(bottom_row,
						text="DEFAULTS",
						#anchor="w",
						width=12,
						command=MergeRtdfGui_defaults)
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
						text="RUN & QUIT",
						#anchor="w",
						width=12,
						command=function() run_MergeRtdf(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
						text="QUIT",
						#anchor="w",
						width=12,
						command=function() tkdestroy(mergertdf_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
						text="RUN",
						#anchor="w",
						width=12,
						command=function() run_MergeRtdf(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	dir_entry_frame <- tkframe(mergertdf_win)
	dir_entry_label <- tklabel(dir_entry_frame,
						width=10,
						text="directory")
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

	indir_entry_frame <- tkframe(mergertdf_win)
	indir_entry_label <- tklabel(indir_entry_frame,
						width=10,
						text="in rtdf dir")
	tkpack(indir_entry_label,side="left")
	indir_entry <- tklabel(indir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=in_rtdf_dir)
	tkpack(indir_entry,side="left",fill="x",expand=1)
	indir_browse <- tkbutton(indir_entry_frame,
						text="Browse",
						command=function() dir_browser(in_rtdf_dir))
	tkpack(indir_browse,side="right")
	tkpack(indir_entry_frame,side="top",anchor="w",fill="x")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	union_button <- tkcheckbutton(mergertdf_win,
						text="union_of_tests",
						variable=merge_union_of_tests)
	tkpack(union_button,side="top",anchor="w")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	dataset_frame <- tkframe(mergertdf_win)
	dataset_label <- tklabel(dataset_frame,
						#width=10,
						text="number of datasets")
	tkpack(dataset_label,side="left")
	datasets_value <- tklabel(dataset_frame,
						width=3,
						relief="sunken",
						textvariable=merge_datasets)
	tkpack(datasets_value,side="left")
	dataset_plus <- tkbutton(dataset_frame,
						text="+",
						command=inc_merge_datasets)
	tkpack(dataset_plus,side="left")
	dataset_minus <- tkbutton(dataset_frame,
						text="-",
						command=dec_merge_datasets)
	tkpack(dataset_minus,side="left")
	tkpack(dataset_frame,side="top",anchor="w",fill="x")

	# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	outfile_entry_frame <- tkframe(mergertdf_win)
	outfile_entry_label <- tklabel(outfile_entry_frame,
						width=10,
						text="out_file")
	tkpack(outfile_entry_label,side="left")
	outfile_entry <- tkentry(outfile_entry_frame,
						width=50,
						textvariable=merge_outfile)
	tkpack(outfile_entry,side="left",fill="x",expand=1)
	outfile_browse <- tkbutton(outfile_entry_frame,
						text="Browse",
						command=function() rtdf_browser(merge_outfile,Rtdfs_dir,output=TRUE))
	tkpack(outfile_browse,side="right")
	tkpack(outfile_entry_frame,side="bottom",anchor="w",fill="x")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	for (j in 1:30) {
		dataset_frame <- tkframe(mergertdf_win)
		my_text = as.character(j)
		rtdf_entry_label <- tklabel(dataset_frame,
						width=5,
						anchor="e",
						text=my_text)
		tkpack(rtdf_entry_label,side="left")
		rtdf_entry <- tkentry(dataset_frame,
						textvariable=merge_rtdf_names[[j]])
		tkpack(rtdf_entry,side="left",fill="x",expand=1)
		if (j==1) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(1))
		} else if (j==2) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(2))
		} else if (j==3) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(3))
		} else if (j==4) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(4))
		} else if (j==5) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(5))
		} else if (j==6) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(6))
		} else if (j==7) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(7))
		} else if (j==8) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(8))
		} else if (j==9) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(9))
		} else if (j==10) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(10))
		} else if (j==11) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(11))
		} else if (j==12) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(12))
		} else if (j==13) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(13))
		} else if (j==14) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(14))
		} else if (j==15) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(15))
		} else if (j==16) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(16))
		} else if (j==17) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(17))
		} else if (j==18) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(18))
		} else if (j==19) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(19))
		} else if (j==20) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(20))
		} else if (j==21) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(21))
		} else if (j==22) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(22))
		} else if (j==23) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(23))
		} else if (j==24) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(24))
		} else if (j==25) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(25))
		} else if (j==26) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(26))
		} else if (j==27) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(27))
		} else if (j==28) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(28))
		} else if (j==29) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(29))
		} else if (j==30) {
			rtdf_browse <- tkbutton(dataset_frame,
						text="Browse",
						command=function() merge_rtdf_browser(30))
		} 
		tkpack(rtdf_browse,side="left")


		merge_dataset_frames[[j]] <<- dataset_frame
	}

	my_sets <- as.integer(tclObj(merge_datasets))

	for (j in 1:my_sets)  tkpack(merge_dataset_frames[[j]],side="top",anchor="w",fill="x")

}


