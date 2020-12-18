# NonGating2GatingGui.R
#
# $Id: NonGating2GatingGui.R,v 1.2 2020/12/18 01:22:43 david Exp $
#
# Tk/Tcl GUI wrapper for calling NonGating2Gating.R
# called by TkRadar.R
#
# Copyright (C) 2019 David Gattrell
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

# NonGating2GatingGui specific variables
#--------------------------------------
if(!exists(".TkRadar.env")) .TkRadar.env <- new.env()

ng2g_in_name <- tclVar("")			
ng2g_out_name <- tclVar("")	
ng2g_testname <- tclVar("")
ng2g_sbin_num <- tclVar(13)
ng2g_sbin_num_shadow <- tclVar(13)
ng2g_sbin_name <- tclVar("")
ng2g_hbin_num <- tclVar(3)
ng2g_hbin_num_shadow <- tclVar(3)
ng2g_hbin_name <- tclVar("")
ng2g_force_lims <- tclVar(0)


ng2g_file_count <- tclVar(1)
ng2g_file_index <- tclVar(1)
ng2g_in_names <- list()
ng2g_out_names <- list()
ng2g_in_names[[1]] <- tclVar("")
ng2g_out_names[[1]] <- tclVar("")
ng2g_in_dir <- tclVar("")	


#----------------------------------------------------
NonGating2GatingGui_defaults <- function() {

	tclvalue(ng2g_testname) <- ""
	tclvalue(ng2g_sbin_num) <-13 
	tclvalue(ng2g_sbin_num_shadow) <- 13
	tclvalue(ng2g_sbin_name) <- ""
	tclvalue(ng2g_hbin_num) <- 3
	tclvalue(ng2g_hbin_num_shadow) <- 3
	tclvalue(ng2g_hbin_name) <- ""
	tclvalue(ng2g_force_lims) <- 0

	in_name <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
	tclvalue(ng2g_in_name) <- in_name

	out_name = ""
	tclvalue(ng2g_out_name) <- out_name
	tclvalue(ng2g_file_count) <- 1
	tclvalue(ng2g_file_index) <- 1
	tclvalue(ng2g_in_names[[1]]) <- tclVar("")
	tclvalue(ng2g_out_names[[1]]) <- tclVar("")

	tclvalue(ng2g_in_dir) <- tclObj(Rtdfs_dir)
}

#-----------------------------------------------------
ng_inc_index <- function() {
	my_value <- as.integer(tclObj(ng2g_file_index))
	tclvalue(ng2g_in_names[[my_value]]) <- tclObj(ng2g_in_name)
	tclvalue(ng2g_out_names[[my_value]]) <- tclObj(ng2g_out_name)

	my_count <- as.integer(tclObj(ng2g_file_count))
	if (my_value<my_count) {
		my_value <- my_value + 1
		tclvalue(ng2g_in_name) <- tclObj(ng2g_in_names[[my_value]])
		tclvalue(ng2g_out_name) <- tclObj(ng2g_out_names[[my_value]])
	} 
	tclvalue(ng2g_file_index) <- my_value
}

#-----------------------------------------------------
ng_dec_index <- function() {
	my_value <- as.integer(tclObj(ng2g_file_index))
	tclvalue(ng2g_in_names[[my_value]]) <- tclObj(ng2g_in_name)
	tclvalue(ng2g_out_names[[my_value]]) <- tclObj(ng2g_out_name)

	if (my_value>1) {
		my_value <- my_value - 1
		tclvalue(ng2g_in_name) <- tclObj(ng2g_in_names[[my_value]])
		tclvalue(ng2g_out_name) <- tclObj(ng2g_out_names[[my_value]])
	}
	tclvalue(ng2g_file_index) <- my_value
}

#----------------------------------------------------
ng2g_in_rtdf_browser <-function() {
	init_dir = paste(tclObj(ng2g_in_dir),sep="",collapse=" ")
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
				tclvalue(ng2g_file_count) <- length(names)

				# the tkgetOpenFile returns the full path!
				# separate filename and path
				my_filepath = sub("/[^/]*$","",name)	
				name=sub("^.*/","",name)

				# update name and path variables
				if (length(ng2g_in_names)<j)  ng2g_in_names[[j]] <<- tclVar(name)
				else  tclvalue(ng2g_in_names[[j]]) <- name

				if (j==1) {
					index <- as.integer(tclObj(ng2g_file_index))
					if (index>length(names))  tclvalue(ng2g_file_index) <- 1

					tclvalue(ng2g_in_dir) <- my_filepath
				}

				out_name <- ""
				if (length(ng2g_out_names)<j)  ng2g_out_names[[j]] <<- tclVar(out_name)
				else  tclvalue(ng2g_out_names[[j]]) <- out_name

			}
		}
		index <- as.integer(tclObj(ng2g_file_index))
		tclvalue(ng2g_in_name) <- tclObj(ng2g_in_names[[index]])
		tclvalue(ng2g_out_name) <- tclObj(ng2g_out_names[[index]])
	}
}

#----------------------------------------------------
ng2g_out_rtdf_browser <-function(...) {
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
		tclvalue(ng2g_out_name) <- name
		index <- as.integer(tclObj(ng2g_file_index))
		tclvalue(ng2g_out_names[[index]]) <- name

		rtdfs_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
		if(my_filepath != rtdfs_dir) {
			change_Rtdfs_dir(my_filepath)
		}
	}
}

#----------------------------------------------------
run_NonGating2Gating <-function(done=FALSE,...) {
	# make sure vector is up-to-date with gui entry boxes
	my_value <- as.integer(tclObj(ng2g_file_index))
	tclvalue(ng2g_in_names[[my_value]]) <- tclObj(ng2g_in_name)
	tclvalue(ng2g_out_names[[my_value]]) <- tclObj(ng2g_out_name)

	testname_ <- paste(tclObj(ng2g_testname),sep="",collapse=" ")

	sbin_num_ <- as.integer(tclObj(ng2g_sbin_num))
	sbin_name_ <- paste(tclObj(ng2g_sbin_name),sep="",collapse=" ")
	hbin_num_ <- as.integer(tclObj(ng2g_hbin_num))
	hbin_name_ <- paste(tclObj(ng2g_hbin_name),sep="",collapse=" ")
	force_use_limits_ <- as.logical(tclObj(ng2g_force_lims))

	output_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
	in_dir_ <- paste(tclObj(ng2g_in_dir),sep="",collapse=" ")

	# go to output directory...
	full_path = output_dir
	if (nchar(full_path)<1)  full_path <- paste(tclObj(Output_dir),sep="",collapse=" ")
	if (nchar(full_path)<1)  full_path <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	setwd(full_path)

	count <- as.integer(tclObj(ng2g_file_count))
	for (j in 1:count) {
		in_file_ <- paste(tclObj(ng2g_in_names[[j]]),sep="",collapse=" ")
		out_file_ <- paste(tclObj(ng2g_out_names[[j]]),sep="",collapse=" ")

		my_expr = substitute(
			NonGating2Gating(in_file=in_file_,out_file=out_file_,
						testname=testname_,
						sbin_num=sbin_num_,sbin_name=sbin_name_,
						hbin_num=hbin_num_,hbin_name=hbin_name_,
						force_use_limits=force_use_limits_,
						in_dir=in_dir_)
		)
		tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
		tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
		my_command = sprintf("%s\n",deparse(my_expr))
		my_cmnd = "NonGating2Gating(...)\n"

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

	#cat("Finished!\n")
	
	if(out_file_ == "") {
		out_file_ = sub("(.rtdf)?(.Rtdf)?$","_ng2g.rtdf",in_file_)
	}
	tclvalue(Rtdf_name) <- out_file_
	tclvalue(Rtdf_dir) <- output_dir
		
	if(done>0) {
		nongate2gate_win <- get("nongate2gate_win",envir=.TkRadar.wins)
		tkdestroy(nongate2gate_win)
	}
}

#----------------------------------------------------
NonGating2GatingGui <- function() {

	NonGating2GatingGui_defaults()		# initialize variables...
	nongate2gate_win <- tktoplevel()
	assign("nongate2gate_win",nongate2gate_win,envir=.TkRadar.wins)
	tkwm.title(nongate2gate_win, "NonGating2Gating")
	
	
	# need to define white/yellow background objects first before
	# being 'reset' by DEFAULTS button
	sbin_num_frame <- tkframe(nongate2gate_win)
	sbin_num_entry <- tkentry(sbin_num_frame,
					  width=7,
					  background="white",
					  textvariable=ng2g_sbin_num_shadow)
	hbin_num_frame <- tkframe(nongate2gate_win)
	hbin_num_entry <- tkentry(hbin_num_frame,
					  width=7,
					  background="white",
					  textvariable=ng2g_hbin_num_shadow)


	
	bottom_row <- tkframe(nongate2gate_win)
	default_button <- tkbutton(bottom_row,
						text="DEFAULTS",
						#anchor="w",
						width=12,
						command=function() {
							NonGating2GatingGui_defaults()
							tkconfigure(sbin_num_entry,background="white")
							tkconfigure(hbin_num_entry,background="white")
						})
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
						text="RUN & QUIT",
						#anchor="w",
						width=12,
						command=function() run_NonGating2Gating(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
						text="QUIT",
						#anchor="w",
						width=12,
						command=function()tkdestroy(nongate2gate_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
						text="RUN",
						#anchor="w",
						width=12,
						command=function() run_NonGating2Gating(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	# - - - 

	dir_entry_frame <- tkframe(nongate2gate_win)
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

	in_dir_entry_frame <- tkframe(nongate2gate_win)
	in_dir_entry_label <- tklabel(in_dir_entry_frame,
						width=10,
						text="in_dir")
	tkpack(in_dir_entry_label,side="left")
	in_dir_entry <- tklabel(in_dir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=ng2g_in_dir)
	tkpack(in_dir_entry,side="left",fill="x",expand=1)
	in_dir_browse <- tkbutton(in_dir_entry_frame,
						text="Browse",
						command=function() dir_browser(ng2g_in_dir))
	tkpack(in_dir_browse,side="right")
	tkpack(in_dir_entry_frame,side="top",anchor="w",fill="x")

	multiple_frame <- tkframe(nongate2gate_win)
	multiple_label <- tklabel(multiple_frame,
						#width=10,
						text="file")
	tkpack(multiple_label,side="left")
	index_value <- tklabel(multiple_frame,
						width=3,
						relief="sunken",
						textvariable=ng2g_file_index)
	tkpack(index_value,side="left")
	multiple_label2 <- tklabel(multiple_frame,
						#width=10,
						text="of")
	tkpack(multiple_label2,side="left")
	count_value <- tklabel(multiple_frame,
						width=3,
						relief="sunken",
						textvariable=ng2g_file_count)
	tkpack(count_value,side="left")
	index_minus <- tkbutton(multiple_frame,
						text="-",
						command=function() ng_dec_index())
	tkpack(index_minus,side="left")
	index_plus <- tkbutton(multiple_frame,
						text="+",
						command=function() ng_inc_index())
	tkpack(index_plus,side="left")
	tkpack(multiple_frame,side="top",anchor="w",fill="x")

	in_entry_frame <- tkframe(nongate2gate_win)
	in_entry_label <- tklabel(in_entry_frame,
						width=10,
						text="in_file")
	tkpack(in_entry_label,side="left")
	in_entry <- tkentry(in_entry_frame,
						width=50,
						textvariable=ng2g_in_name)
	tkpack(in_entry,side="left",fill="x",expand=1)
	in_browse <- tkbutton(in_entry_frame,
						text="Browse",
						command=ng2g_in_rtdf_browser)
	tkpack(in_browse,side="right")
	tkpack(in_entry_frame,side="top",anchor="w",fill="x")

	out_entry_frame <- tkframe(nongate2gate_win)
	out_entry_label <- tklabel(out_entry_frame,
						width=10,
						text="out_file")
	tkpack(out_entry_label,side="left")
	out_entry <- tkentry(out_entry_frame,
						width=20,
						textvariable=ng2g_out_name)
	tkpack(out_entry,side="left",fill="x",expand=1)
	out_browse <- tkbutton(out_entry_frame,
						text="Browse",
						command=ng2g_out_rtdf_browser)
	tkpack(out_browse,side="right")
	tkpack(out_entry_frame,side="top",anchor="w",fill="x")

	testname_frame <- tkframe(nongate2gate_win)
	testname_label <- tklabel(testname_frame,
						width=10,
						text="testname")
	tkpack(testname_label,side="left")
	testname_entry <- tkentry(testname_frame,
						width=20,
						textvariable=ng2g_testname)
	tkpack(testname_entry,side="left",fill="x",expand=1)
	testname_browse <- tkbutton(testname_frame,
						text="Browse",
						command=function() param_browser(ng2g_testname,
								ng2g_in_name,ng2g_in_dir)
						)
	tkpack(testname_browse,side="right")
	tkpack(testname_frame,side="top",anchor="w",fill="x")

	# sbin_num
	sbin_num_label <- tklabel(sbin_num_frame,
						width=10,
						text="sbin_num")
	tkpack(sbin_num_label,side="left")
	# sbin_num_entry defined earlier due to background color stuff
	tkpack(sbin_num_entry,side="left",anchor="n")
	tkbind(sbin_num_entry,"<KeyRelease>",function() {
					tmp <- as.integer(tclObj(ng2g_sbin_num_shadow))
					if( (length(tmp)>0) && is.finite(tmp) &&
						(tmp>0.5) && (tmp<1023.5) ) {
						tkconfigure(sbin_num_entry,background="white")
						tclvalue(ng2g_sbin_num) <- tmp
					} else {
						tkconfigure(sbin_num_entry,background="yellow")
					}
					tcl('update')
				})
	tkpack(sbin_num_frame,side="top",anchor="w")

	sbinname_frame <- tkframe(nongate2gate_win)
	sbinname_label <- tklabel(sbinname_frame,
						width=10,
						text="sbin_name")
	tkpack(sbinname_label,side="left")
	sbinname_entry <- tkentry(sbinname_frame,
						width=20,
						textvariable=ng2g_sbin_name)
	tkpack(sbinname_entry,side="left",fill="x",expand=1)
	tkpack(sbinname_frame,side="top",anchor="w",fill="x")

	# hbin_num
	hbin_num_label <- tklabel(hbin_num_frame,
						width=10,
						text="hbin_num")
	tkpack(hbin_num_label,side="left")
	# sbin_num_entry defined earlier due to background color stuff
	tkpack(hbin_num_entry,side="left",anchor="n")
	tkbind(hbin_num_entry,"<KeyRelease>",function() {
					tmp <- as.integer(tclObj(ng2g_hbin_num_shadow))
					if( (length(tmp)>0) && is.finite(tmp) &&
						(tmp>0.5) && (tmp<1023.5) ) {
						tkconfigure(hbin_num_entry,background="white")
						tclvalue(ng2g_hbin_num) <- tmp
					} else {
						tkconfigure(hbin_num_entry,background="yellow")
					}
					tcl('update')
				})
	tkpack(hbin_num_frame,side="top",anchor="w")

	hbinname_frame <- tkframe(nongate2gate_win)
	hbinname_label <- tklabel(hbinname_frame,
						width=10,
						text="hbin_name")
	tkpack(hbinname_label,side="left")
	hbinname_entry <- tkentry(hbinname_frame,
						width=20,
						textvariable=ng2g_hbin_name)
	tkpack(hbinname_entry,side="left",fill="x",expand=1)
	tkpack(hbinname_frame,side="top",anchor="w",fill="x")

	limits_button <- tkcheckbutton(nongate2gate_win,
						text="force_use_limits",
						variable=ng2g_force_lims)
	tkpack(limits_button,side="top",anchor="w")

}


