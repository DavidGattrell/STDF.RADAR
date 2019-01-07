# ConvertKDFGui.R
#
# $Id: ConvertKDFGui.R,v 1.2 2011/02/18 02:17:44 David Exp $
#
# Tk/Tcl GUI wrapper for calling ConvertKDF.R
# called by TkRadar.R
#
# Copyright (C) 2011 David Gattrell
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

# ConvertKDFGui specific variables
#-----------------------------------
kdf_name <- tclVar("")
kdf_dir <- tclVar("")
kdf_rtdf_name <- tclVar("")

klf_name <- tclVar("")
klf_dir <- tclVar("")

kdf_count <- tclVar(1)
kdf_index <- tclVar(1)
multi_kdf_names <- list()
multi_kdf_rtdfs <- list()
multi_kdf_names[[1]] <- tclVar("")
multi_kdf_rtdfs[[1]] <- tclVar("")

#----------------------------------------------------
kdf_browser <-function() {

	my_dir <- paste(tclObj(kdf_dir),sep="",collapse=" ")
	if (nchar(my_dir)<1) {
		my_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if(as.numeric(tclObj(Bad_Vista))>0) {
		my_str = "{{All files} *} {{KDF Files} {.kdf .kdf.gz}}"
	} else {
		my_str = "{{KDF Files} {.kdf .kdf.gz}} {{All files} *}"
	}
	if (nchar(my_dir)>0) {
		name <- tclvalue(tkgetOpenFile(filetypes=my_str,
				initialdir=my_dir,multiple=TRUE))
	} else {
		name <- tclvalue(tkgetOpenFile(filetypes=my_str,
				multiple=TRUE))
	}
	names = split_multiple_filenames(name)

	if(length(names)>0) {
		for (j in 1:length(names)) {
			name = names[j]
		  
			if (nchar(name)>0) {
				tclvalue(kdf_count) <- length(names)

				# the tkgetOpenFile returns the full path!
				# separate filename and path
				my_filepath = sub("/[^/]*$","",name)	
				name=sub("^.*/","",name)

				# update name and path variables
				if (length(multi_kdf_names)<j)  multi_kdf_names[[j]] <<- tclVar(name)
				else  tclvalue(multi_kdf_names[[j]]) <- name

				if (j==1) {
					index <- as.integer(tclObj(kdf_index))
					if (index>length(names))  tclvalue(kdf_index) <- 1

					first_path = my_filepath
					out_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
					if(my_filepath == out_dir)  tclvalue(kdf_dir) <- ""
					else  tclvalue(kdf_dir) <- my_filepath
				} else if (!(first_path %in% my_filepath)) {
					# nasty message... shouldn't get here...
				}

				my_rtdf <- sub("([.]kdf)?([.]gz)?$",".rtdf",name)
				if (length(multi_kdf_rtdfs)<j)  multi_kdf_rtdfs[[j]] <<- tclVar(my_rtdf)
				else  tclvalue(multi_kdf_rtdfs[[j]]) <- my_rtdf
			}
		}
		index <- as.integer(tclObj(kdf_index))
		tclvalue(kdf_name) <- tclObj(multi_kdf_names[[index]])
		tclvalue(kdf_rtdf_name) <- tclObj(multi_kdf_rtdfs[[index]])
	}
}

#----------------------------------------------------
convertkdf_rtdf_browser <-function(...) {
	orig_name = paste(tclObj(kdf_rtdf_name),sep="",collapse=" ")
	if (nchar(orig_name)<1)  orig_name="converted.rtdf"

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
		tclvalue(kdf_rtdf_name) <- name
		index <- as.integer(tclObj(kdf_index))
		tclvalue(multi_kdf_rtdfs[[index]]) <- name
		out_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
		if(my_filepath != out_dir) {
			tclvalue(Rtdf_dir) <- my_filepath
			change_Rtdfs_dir(my_filepath)
		} else {
			tclvalue(Rtdf_dir) <- ""
		}
	}
}

#----------------------------------------------------
klf_browser <-function(klf_name,klf_dir) {

	my_dir <- paste(tclObj(klf_dir),sep="",collapse=" ")
	if (nchar(my_dir)<1) {
		my_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if(as.numeric(tclObj(Bad_Vista))>0) {
		my_str = "{{All files} *} {{KLF Files} {.klf .klf.gz}}"
	} else {
		my_str = "{{KLF Files} {.klf .klf.gz}} {{All files} *}"
	}
	if (nchar(my_dir)>0) {
		name <- tclvalue(tkgetOpenFile(filetypes=my_str,
				initialdir=my_dir,multiple=FALSE))
	} else {
		name <- tclvalue(tkgetOpenFile(filetypes=my_str,
				multiple=FALSE))
	}

	# the tkgetOpenFile returns the full path!
	if (nchar(name)>0) {
		# separate filename and path
		my_filepath = sub("/[^/]*$","",name)	
		name=sub("^.*/","",name)

		# update name and path variables
		tclvalue(klf_name) <- name
		out_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
		if(my_filepath != out_dir) {
			tclvalue(klf_dir) <- my_filepath
		} else {
			tclvalue(klf_dir) <- ""
		}
	}
}

#-----------------------------------------------------
inc_kdf_index <- function() {
	my_value <- as.integer(tclObj(kdf_index))
	tclvalue(multi_kdf_names[[my_value]]) <- tclObj(kdf_name)
	tclvalue(multi_kdf_rtdfs[[my_value]]) <- tclObj(kdf_rtdf_name)

	my_count <- as.integer(tclObj(a5_count))
	if (my_value<my_count) {
		my_value <- my_value + 1
		tclvalue(kdf_name) <- tclObj(multi_kdf_names[[my_value]])
		tclvalue(kdf_rtdf_name) <- tclObj(multi_kdf_rtdfs[[my_value]])
	} 
	tclvalue(kdf_index) <- my_value
}

#-----------------------------------------------------
dec_kdf_index <- function() {
	my_value <- as.integer(tclObj(kdf_index))
	tclvalue(multi_kdf_names[[my_value]]) <- tclObj(kdf_name)
	tclvalue(multi_kdf_rtdfs[[my_value]]) <- tclObj(kdf_rtdf_name)

	if (my_value>1) {
		my_value <- my_value - 1
		tclvalue(kdf_name) <- tclObj(multi_kdf_names[[my_value]])
		tclvalue(kdf_rtdf_name) <- tclObj(multi_kdf_rtdfs[[my_value]])
	}
	tclvalue(kdf_index) <- my_value
}

#----------------------------------------------------
ConvertKDFGui_defaults <- function() {
	tclvalue(kdf_name) <- ""
#	tclvalue(kdf_dir) <- ""
	tclvalue(kdf_rtdf_name) <- ""
	tclvalue(klf_name) <- ""
#	tclvalue(klf_dir) <- ""

	tclvalue(kdf_count) <- 1
	tclvalue(kdf_index) <- 1
	tclvalue(multi_kdf_names[[1]]) <- tclVar("")
	tclvalue(multi_kdf_rtdfs[[1]]) <- tclVar("")

}

#----------------------------------------------------
run_ConvertKDF <-function(done=FALSE,...) {
	my_value <- as.integer(tclObj(kdf_index))
	tclvalue(multi_kdf_names[[my_value]]) <- tclObj(kdf_name)
	tclvalue(multi_kdf_rtdfs[[my_value]]) <- tclObj(kdf_rtdf_name)

	klf_name_ <- paste(tclObj(klf_name),sep="",collapse=" ")

	kdf_dir_ <- paste(tclObj(kdf_dir),sep="",collapse=" ")
	klf_dir_ <- paste(tclObj(klf_dir),sep="",collapse=" ")
	output_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")

	full_path = output_dir
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	setwd(full_path)

	count <- as.integer(tclObj(kdf_count))
	for (j in 1:count) {
		kdf_name_ <- paste(tclObj(multi_kdf_names[[j]]),sep="",collapse=" ")
		rtdf_name_ <- paste(tclObj(multi_kdf_rtdfs[[j]]),sep="",collapse=" ")

		my_expr = substitute(
			ConvertKDF(kdf_name=kdf_name_,rtdf_name=rtdf_name_,
						kdf_dir=kdf_dir_,klf_name=klf_name_,
						klf_dir=klf_dir_)
		)
		tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
		tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
		my_command = sprintf("%s\n",deparse(my_expr))
		my_cmnd = "ConvertKDF(...)\n"
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

	tclvalue(Rtdf_name) <- rtdf_name_
	tclvalue(Rtdf_dir) <- output_dir
		
	if(done>0) {
		convertkdf_win <- get("convertkdf_win",envir=.TkRadar.wins)
		tkdestroy(convertkdf_win)
	}
}

#----------------------------------------------------
ConvertKDFGui <- function() {

	ConvertKDFGui_defaults()		# initialize variables...
	convertkdf_win <- tktoplevel()
	assign("convertkdf_win",convertkdf_win,envir=.TkRadar.wins)
	tkwm.title(convertkdf_win, "ConvertKDF")
		
	bottom_row <- tkframe(convertkdf_win)
	default_button <- tkbutton(bottom_row,
						text="DEFAULTS",
						#anchor="w",
						width=12,
						command=ConvertKDFGui_defaults)
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
						text="RUN & QUIT",
						#anchor="w",
						width=12,
						command=function() run_ConvertKDF(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
						text="QUIT",
						#anchor="w",
						width=12,
						command=function()tkdestroy(convertkdf_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
						text="RUN",
						#anchor="w",
						width=12,
						command=function() run_ConvertKDF(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	dir_entry_frame <- tkframe(convertkdf_win)
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

	kdf_dir_entry_frame <- tkframe(convertkdf_win)
	kdf_dir_entry_label <- tklabel(kdf_dir_entry_frame,
						width=10,
						text="kdf_dir")
	tkpack(kdf_dir_entry_label,side="left")
	kdf_dir_entry <- tklabel(kdf_dir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=kdf_dir)
	tkpack(kdf_dir_entry,side="left",fill="x",expand=1)
	kdf_dir_browse <- tkbutton(kdf_dir_entry_frame,
						text="Browse",
						command=function() dir_browser(kdf_dir))
	tkpack(kdf_dir_browse,side="right")
	tkpack(kdf_dir_entry_frame,side="top",anchor="w",fill="x")

	klf_dir_entry_frame <- tkframe(convertkdf_win)
	klf_dir_entry_label <- tklabel(klf_dir_entry_frame,
						width=10,
						text="klf_dir")
	tkpack(klf_dir_entry_label,side="left")
	klf_dir_entry <- tklabel(klf_dir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=klf_dir)
	tkpack(klf_dir_entry,side="left",fill="x",expand=1)
	klf_dir_browse <- tkbutton(klf_dir_entry_frame,
						text="Browse",
						command=function() dir_browser(klf_dir))
	tkpack(klf_dir_browse,side="right")
	tkpack(klf_dir_entry_frame,side="top",anchor="w",fill="x")

	klf_entry_frame <- tkframe(convertkdf_win)
	klf_entry_label <- tklabel(klf_entry_frame,
						width=10,
						text="klf_name")
	tkpack(klf_entry_label,side="left")
		klf_entry <- tkentry(klf_entry_frame,
						width=20,
						textvariable=klf_name)
	tkpack(klf_entry,side="left",fill="x",expand=1)
	klf_browse <- tkbutton(klf_entry_frame,
						text="Browse",
						command=function() klf_browser(klf_name,klf_dir))
	tkpack(klf_browse,side="right")
	tkpack(klf_entry_frame,side="top",anchor="w",fill="x")


	multiple_frame <- tkframe(convertkdf_win)
	multiple_label <- tklabel(multiple_frame,
						#width=10,
						text="file")
	tkpack(multiple_label,side="left")
	index_value <- tklabel(multiple_frame,
						width=3,
						relief="sunken",
						textvariable=kdf_index)
	tkpack(index_value,side="left")
	multiple_label2 <- tklabel(multiple_frame,
						#width=10,
						text="of")
	tkpack(multiple_label2,side="left")
	count_value <- tklabel(multiple_frame,
						width=3,
						relief="sunken",
						textvariable=kdf_count)
	tkpack(count_value,side="left")
	index_minus <- tkbutton(multiple_frame,
						text="-",
						command=dec_kdf_index)
	tkpack(index_minus,side="left")
	index_plus <- tkbutton(multiple_frame,
						text="+",
						command=inc_kdf_index)
	tkpack(index_plus,side="left")
	tkpack(multiple_frame,side="top",anchor="w",fill="x")

	kdf_entry_frame <- tkframe(convertkdf_win)
	kdf_entry_label <- tklabel(kdf_entry_frame,
						width=10,
						text="kdf_name")
	tkpack(kdf_entry_label,side="left")
	kdf_entry <- tkentry(kdf_entry_frame,
						width=50,
						textvariable=kdf_name)
	tkpack(kdf_entry,side="left",fill="x",expand=1)
	kdf_browse <- tkbutton(kdf_entry_frame,
						text="Browse",
						command=kdf_browser)
	tkpack(kdf_browse,side="right")
	tkpack(kdf_entry_frame,side="top",anchor="w",fill="x")

	rtdf_entry_frame <- tkframe(convertkdf_win)
	rtdf_entry_label <- tklabel(rtdf_entry_frame,
						width=10,
						text="rtdf_name")
	tkpack(rtdf_entry_label,side="left")
	rtdf_entry <- tkentry(rtdf_entry_frame,
						width=20,
						textvariable=kdf_rtdf_name)
	tkpack(rtdf_entry,side="left",fill="x",expand=1)
	rtdf_browse <- tkbutton(rtdf_entry_frame,
						text="Browse",
						command=convertkdf_rtdf_browser)
	tkpack(rtdf_browse,side="right")
	tkpack(rtdf_entry_frame,side="top",anchor="w",fill="x")

}


