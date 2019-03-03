# FingerprintGui.R
#
# $Id: FingerprintGui.R,v 1.2 2019/01/28 00:46:12 david Exp $
#
# Tk/Tcl GUI wrapper for calling Fingerprint.R
# called by TkRadar.R
#
# Copyright (C) 2013 David Gattrell
#               2018 David Gattrell
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
# FingerprintGui.R specific variables
#-----------------------------------------
if(!exists(".TkRadar.env")) .TkRadar.env <- new.env()

fp_out_file <- tclVar("fingerprinted.rtdf")
fp_testlist <- list()
fp_testlist_count <- tclVar(0)
for (j in 1:30) {	# limit testlist to <=30, really 7-10 is ideal.
	fp_testlist[[j]] <- tclVar("")
}
fp_top <- tclVar(7)		
fp_top_shadow <- tclVar(7)		
fp_in_name <- tclVar("")
fp_in_dir <- tclVar("")
fp_ref_dir <- tclVar("")
fp_ref1_name <- tclVar("")
fp_ref2_name <- tclVar("")


#----------------------------------------------------
FingerprintGui_defaults <- function() {
	tclvalue(fp_testlist_count) <- 0
	for (j in 1:30) {
		tclvalue(fp_testlist[[j]]) <- ""
	}
	tclvalue(fp_top) <- 7	
	tclvalue(fp_top_shadow) <- 7	
	
	tclvalue(fp_out_file) <- "fingerprinted.rtdf"
	in_dir <- paste(tclObj(Rtdf_dir),sep="",collapse=" ")
	in_name <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
	if(nchar(in_name)>0) {
		tclvalue(fp_in_dir) <- tclObj(Rtdf_dir)
		tclvalue(fp_in_name) <- tclObj(Rtdf_name)
	} else {
		if(nchar(in_dir)>0) {
			tclvalue(fp_in_dir) <- tclObj(Rtdf_dir)
			tclvalue(fp_in_name) <- tclObj(Rtdf_name)
		} else {
			tclvalue(fp_in_dir) <- tclObj(Rtdfs_dir)
			tclvalue(fp_in_name) <- tclObj(Rtdf_name)
		}
	}
	tclvalue(fp_ref_dir) <- tclObj(Rtdf_dir)
	tclvalue(fp_ref1_name) <- ""
	tclvalue(fp_ref2_name) <- ""
}

#----------------------------------------------------
testlist_browser <-function(...) {

	# REVISIT
	# file search for my_testlist.R
	#
	# and then execute file.. and check for creation
	# of my_testlist
	#
	# 
	orig_name <- "my_testlist.R"
	init_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	if (nchar(init_dir)<1) {
		init_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	if(as.numeric(tclObj(Bad_Vista))>0) {
		r_str = "{{All files} *} {{R Files} {.r .R}}"
	} else {
		r_str = "{{R Files} {.r .R}} {{All files} *}"
	}
	if (nchar(init_dir)>0) {
		name <- tclvalue(tkgetOpenFile(
				initialdir=init_dir,
				initialfile=orig_name,
				filetypes=r_str))
	} else {
		name <- tclvalue(tkgetOpenFile(
				initialfile=orig_name,
				filetypes=r_str))
	}

	if (nchar(name)>0) {
		source(name)

		if(exists("my_testlist")) {
			new_count = length(my_testlist)
			testlist_end <- as.numeric(tclObj(fp_testlist_count))
			if(new_count>(30-testlist_end)) {
				new_count = 30-testlist_end
			}
			if(new_count>0) {
				#cat(sprintf("testlist was %d long \n",testlist_end))
				#cat(sprintf("adding %d tests...\n",new_count))
				for (j in 1:new_count) {
					tclvalue(fp_testlist[[j+testlist_end]]) <- as.character(my_testlist[j])
				}
				tclvalue(fp_testlist_count) <- j+testlist_end
			}
		} else {
			# REVISIT...
			# print message that file didn't contain expected variable definition!
		}
	} else {
		# print message that no file was selected
	}

}
#----------------------------------------------------
paramframe_browser <-function(my_listbox) {
	my_file <- paste(tclObj(fp_ref1_name),sep="",collapse=" ")	
	my_dir <- paste(tclObj(fp_ref_dir),sep="",collapse=" ")
	if (nchar(my_dir)<1) {
		my_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(my_dir)<1) {
		my_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}

	if (nchar(my_file)>0) {

		# load ParametersFrame from rtdf file
		#------------------------------------
		if (my_dir != "") {
			orig_dir <- getwd()
			setwd(my_dir)
		}
		load(my_file)
		if (my_dir != "")  setwd(orig_dir)

		param_count <- dim(ParametersFrame)[1]

		my_list <- ParametersFrame[["testname"]]
	

		# create popup list window
		#-------------------------
		if (exists("FPparam_win",envir=.TkRadar.wins,inherits=FALSE)) {
			FPparam_win <- get("FPparam_win",envir=.TkRadar.wins)
		}
		if (exists("FPparam_win") && as.logical(tkwinfo("exists",FPparam_win)))  tkdestroy(FPparam_win)
		FPparam_win <- tktoplevel()
		assign("FPparam_win",FPparam_win,envir=.TkRadar.wins)
		tkwm.title(FPparam_win, "FingerPrint Parameter Browser")

		#   35 row scrollable window for parameters
		if(param_count<35)  my_height <- param_count
		else  my_height <- 35

		listbox_frame <- tkframe(FPparam_win)
		param_listbox <- tklistbox(listbox_frame,
							selectmode="extended",
							exportselection=FALSE,
							width = max(sapply(my_list,nchar)),
							height=my_height)
		param_scroll <- tkscrollbar(listbox_frame,
							orient="vertical",
							command=function(...) tkyview(param_listbox,...))
		tkconfigure(param_listbox,
					yscrollcommand=function(...) tkset(param_scroll,...))
		lapply(my_list,function(my_item) tkinsert(param_listbox,"end",my_item))

		tkpack(param_listbox,side="left",anchor="n",fill="both",expand=1)
		tkpack(param_scroll,side="right",anchor="n",fill="y")
		tkpack(listbox_frame,side="top",anchor="w",fill="both",expand=1)
		
		# ok or quit buttons at bottom 
		bottom_row <- tkframe(FPparam_win)
		cancel_button <- tkbutton(bottom_row,
						text="QUIT",
						#anchor="w",
						width=12,
						command=function() tkdestroy(FPparam_win))
		tkpack(cancel_button,side="right")
	
		apply_button <- tkbutton(bottom_row,
						text="OK",
						#anchor="w",
						width=12,
						command=function() {
							tmp = as.character(tclvalue(tkcurselection(param_listbox)))
							my_indices <- as.numeric(unlist(strsplit(tmp," ")))
							new_count = length(my_indices)
							if(new_count>0) {
								testlist_end = as.numeric(tclObj(fp_testlist_count))
								if(new_count>(30-testlist_end)) {
									new_count = 30-testlist_end
									# REVISIT..
									# need popup to remind that only new_count of tests added
								}
								
								for (j in 1:new_count) {
									idx = my_indices[j] + 1 	# idx starts at 0, my_list starts at 1
									tclvalue(fp_testlist[[j+testlist_end]]) <- as.character(
														my_list[idx])
								}
								tclvalue(fp_testlist_count) <- j+testlist_end

							}
							tkdestroy(FPparam_win)
							# update my_listbox
							testlist_end <- as.numeric(tclObj(fp_testlist_count))
							#cat(sprintf("testlist_end is %d \n",testlist_end))
							if(testlist_end>0) {
								my_list <- vector()
								for(j in 1:testlist_end) {
									my_list[j] = paste(tclObj(fp_testlist[[j]]),sep="",collapse=" ")
								}
							} else {
								my_list = ""
							}
							tkdelete(my_listbox,0,"end")
							lapply(my_list,function(my_item) tkinsert(my_listbox,"end",my_item))
						})
		tkpack(apply_button,side="right")
		tkpack(bottom_row,side="bottom",anchor="w")

	}

}


#----------------------------------------------------
fp_out_rtdf_browser <-function(...) {

	orig_name = paste(tclObj(fp_out_file),sep="",collapse=" ")

	init_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
	if (nchar(init_dir)<1) {
		init_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(init_dir)<1) {
		init_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	
	if(as.numeric(tclObj(Bad_Vista))>0) {
		rtdf_str = "{{All files} *} {{RTDF Files} {.rtdf .Rtdf .Rdata}}"
	} else {
		rtdf_str = "{{RTDF Files} {.rtdf .Rtdf .Rdata}} {{All files} *}"
	}
	if (nchar(init_dir)>0) {
		name <- tclvalue(tkgetSaveFile(
				initialdir=init_dir,
				initialfile=orig_name,
				filetypes=rtdf_str))
	} else {
		name <- tclvalue(tkgetSaveFile(
				initialfile=orig_name,
				filetypes=rtdf_str))
	}

	if (nchar(name)>0) {
		my_filepath = sub("/[^/]*$","",name)	# strip off filename, just path left
		name=sub("^.*/","",name)
		tclvalue(fp_out_file) <- name

		# if we changed directory... update paths
		out_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
		if(my_filepath != out_dir) {
			change_Rtdfs_dir(my_filepath)
		}
	}
}


#----------------------------------------------------
run_Fingerprint <-function(done=FALSE,...) {

	rtdf_name_ <- paste(tclObj(fp_in_name),sep="",collapse=" ")
	ref_rtdf_name_ <- paste(tclObj(fp_ref1_name),sep="",collapse=" ")
	testlist_end <- as.numeric(tclObj(fp_testlist_count))
	if(testlist_end>0) {
		if(testlist_end>30)  testlist_end = 30
		testlist_ <- vector()
		for (j in 1:testlist_end) {
			testlist_[j] <- paste(tclObj(fp_testlist[[j]]),sep="",collapse=" ")
		}
	} else {
		testlist_ = ""
	}
	out_rtdf_name_ <-  paste(tclObj(fp_out_file),sep="",collapse=" ")
	ref2_rtdf_name_ <- paste(tclObj(fp_ref2_name),sep="",collapse=" ")
	top_ <- as.numeric(tclObj(fp_top))
	in_dir_ <- paste(tclObj(fp_in_dir),sep="",collapse=" ")
	ref_dir_ <- paste(tclObj(fp_ref_dir),sep="",collapse=" ")
	output_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")


	full_path = output_dir
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(full_path)<1) {
		full_path <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	setwd(full_path)

	my_expr = substitute(
		Fingerprint(rtdf_name=rtdf_name_,ref_rtdf_name=ref_rtdf_name_,
					testlist=testlist_,out_rtdf_name=out_rtdf_name_,
					ref2_rtdf_name=ref2_rtdf_name_,top=top_,
					in_dir=in_dir_,ref_dir=ref_dir_)
	)
	tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
	tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
	my_command = sprintf("%s\n",deparse(my_expr))
	my_cmnd = "Fingerprint(...)\n"
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

	if (rtdf_name_!="") {
		# now update Rtdf_name and Rtdf_dir...
		tclvalue(Rtdf_name) <- out_rtdf_name_
		tclvalue(Rtdf_dir) <- output_dir
	}
	if(done>0) {
		fingerprint_win <- get("fingerprint_win",envir=.TkRadar.wins)
		tkdestroy(fingerprint_win)
	}
}


#----------------------------------------------------
FingerprintGui <- function() {

	FingerprintGui_defaults()		# initialize variables...
	fingerprint_win <- tktoplevel()
	assign("fingerprint_win",fingerprint_win,envir=.TkRadar.wins)
	tkwm.title(fingerprint_win, "Fingerprint")
		
#REVISIT: as far as here... part II
	# need to do something special for testlist
	# .. display in scroll window
	# option to load my_testlist.R
	# option to select tests from ref_rtdf testlist from ParametersFrame
	# option to manually cut/paste in tests
	# option to delete tests

	# these get reset by Default button, so need to be defined first...
	num_entry_frame <- tkframe(fingerprint_win)
	num_entry <- tkentry(num_entry_frame,
						width=10,
						background="white",
						textvariable=fp_top_shadow)


	bottom_row <- tkframe(fingerprint_win)
	default_button <- tkbutton(bottom_row,
						text="DEFAULTS",
						#anchor="w",
						width=12,
						command=function() {
							FingerprintGui_defaults()
							tkconfigure(num_entry,background="white")
						})
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
						text="RUN & QUIT",
						#anchor="w",
						width=12,
						command=function() run_Fingerprint(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
						text="QUIT",
						#anchor="w",
						width=12,
						command=function()tkdestroy(fingerprint_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
						text="RUN",
						#anchor="w",
						width=12,
						command=function() run_Fingerprint(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	dir_entry_frame <- tkframe(fingerprint_win)
	dir_entry_label <- tklabel(dir_entry_frame,
						width=15,
						text="directory")
	tkpack(dir_entry_label,side="left")
	dir_entry <- tklabel(dir_entry_frame,
						width=40,
						relief="sunken",
						textvariable=Rtdfs_dir)
	tkpack(dir_entry,side="left",fill="x",expand=1)
	dir_browse <- tkbutton(dir_entry_frame,
						text="Browse",
						command=function() dir_browser(Rtdfs_dir))
	tkpack(dir_browse,side="right")
	tkpack(dir_entry_frame,side="top",anchor="w",fill="x")


	in_dir_entry_frame <- tkframe(fingerprint_win)
	in_dir_entry_label <- tklabel(in_dir_entry_frame,
						width=15,
						text="in_dir")
	tkpack(in_dir_entry_label,side="left")
	in_dir_entry <- tklabel(in_dir_entry_frame,
						width=40,
						relief="sunken",
						textvariable=fp_in_dir)
	tkpack(in_dir_entry,side="left",fill="x",expand=1)
	in_dir_browse <- tkbutton(in_dir_entry_frame,
						text="Browse",
						command=function() dir_browser(fp_in_dir))
	tkpack(in_dir_browse,side="right")
	tkpack(in_dir_entry_frame,side="top",anchor="w",fill="x")

	in_entry_frame <- tkframe(fingerprint_win)
	in_entry_label <- tklabel(in_entry_frame,
						width=15,
						text="rtdf_name")
	tkpack(in_entry_label,side="left")
	in_entry <- tkentry(in_entry_frame,
						width=40,
						textvariable=fp_in_name)
	tkpack(in_entry,side="left",fill="x",expand=1)
	in_browse <- tkbutton(in_entry_frame,
						text="Browse",
						command=function() rtdf_browser(fp_in_name,fp_in_dir))
	tkpack(in_browse,side="right")
	tkpack(in_entry_frame,side="top",anchor="w",fill="x")


	ref_dir_entry_frame <- tkframe(fingerprint_win)
	ref_dir_entry_label <- tklabel(ref_dir_entry_frame,
						width=15,
						text="ref_dir")
	tkpack(ref_dir_entry_label,side="left")
	ref_dir_entry <- tklabel(ref_dir_entry_frame,
						width=40,
						relief="sunken",
						textvariable=fp_ref_dir)
	tkpack(ref_dir_entry,side="left",fill="x",expand=1)
	ref_dir_browse <- tkbutton(ref_dir_entry_frame,
						text="Browse",
						command=function() dir_browser(fp_ref_dir))
	tkpack(ref_dir_browse,side="right")
	tkpack(ref_dir_entry_frame,side="top",anchor="w",fill="x")

	ref_entry_frame <- tkframe(fingerprint_win)
	ref_entry_label <- tklabel(ref_entry_frame,
						width=15,
						text="ref_rtdf_name")
	tkpack(ref_entry_label,side="left")
	ref_entry <- tkentry(ref_entry_frame,
						width=40,
						textvariable=fp_ref1_name)
	tkpack(ref_entry,side="left",fill="x",expand=1)
	ref_browse <- tkbutton(ref_entry_frame,
						text="Browse",
						command=function() rtdf_browser(fp_ref1_name,fp_ref_dir))
	tkpack(ref_browse,side="right")
	tkpack(ref_entry_frame,side="top",anchor="w",fill="x")

	ref2_entry_frame <- tkframe(fingerprint_win)
	ref2_entry_label <- tklabel(ref2_entry_frame,
						width=15,
						text="ref2_rtdf_name")
	tkpack(ref2_entry_label,side="left")
	ref2_entry <- tkentry(ref2_entry_frame,
						width=40,
						textvariable=fp_ref2_name)
	tkpack(ref2_entry,side="left",fill="x",expand=1)
	ref2_browse <- tkbutton(ref2_entry_frame,
						text="Browse",
						command=function() rtdf_browser(fp_ref2_name,fp_ref_dir))
	tkpack(ref2_browse,side="right")
	tkpack(ref2_entry_frame,side="top",anchor="w",fill="x")

	# REVISIT... sharing same fp_ref_dir, no sanity check if using same
	# directory or not...
	# ..should fix this at some point.

	out_rtdf_entry_frame <- tkframe(fingerprint_win)
	out_rtdf_entry_label <- tklabel(out_rtdf_entry_frame,
						width=15,
						text="out_rtdf_name")
	tkpack(out_rtdf_entry_label,side="left")
	out_rtdf_entry <- tkentry(out_rtdf_entry_frame,
						width=40,
						textvariable=fp_out_file)
	tkpack(out_rtdf_entry,side="left",fill="x",expand=1)
	out_rtdf_browse <- tkbutton(out_rtdf_entry_frame,
						text="Browse",
						command=fp_out_rtdf_browser)
	tkpack(out_rtdf_browse,side="right")
	tkpack(out_rtdf_entry_frame,side="top",anchor="w",fill="x")


	# - - - - - - - - - - - - - - - -

	num_entry_label <- tklabel(num_entry_frame,
						width=10,
						text="top")
	tkpack(num_entry_label,side="left")
	tkpack(num_entry,side="left")	#,fill="x",expand=1)
	tkbind(num_entry,"<KeyRelease>",function() {
					tmp <- as.integer(tclObj(fp_top_shadow))
					if( (length(tmp)>0) && is.finite(tmp)) {
						tkconfigure(num_entry,background="white")
						tclvalue(fp_top) <- tmp
					} else {
						tkconfigure(num_entry,background="yellow")
					}
				})
	tkpack(num_entry_frame,side="top",anchor="w",fill="x")

	
	# - - - - - - - - - - - - - - - -
	testlist_end <- as.numeric(tclObj(fp_testlist_count))
	if(testlist_end>0) {
		my_list <- vector()
		for(j in 1:testlist_end) {
			my_list[j] = paste(tclObj(fp_testlist[[j]]),sep="",collapse=" ")
		}
	} else {
		my_list = ""
	}
	longest_name = max(sapply(my_list,nchar))
	if(longest_name < 30)  longest_name = 30

	testlist_frame <- tkframe(fingerprint_win)

	listbox_frame <- tkframe(testlist_frame)
	my_listbox <- tklistbox(listbox_frame,
					selectmode="extended",
					width = longest_name,
					height = 10)
	my_list_scroll <- tkscrollbar(listbox_frame,
					orient="vertical",
					command=function(...) tkyview(my_listbox,...))
	tkconfigure(my_listbox,
				yscrollcommand=function(...) tkset(my_list_scroll,...))
	lapply(my_list,function(my_item) tkinsert(my_listbox,"end",my_item))
	tkpack(my_listbox,side="left",anchor="n",fill="both",expand=1)
	tkpack(my_list_scroll,side="right",anchor="n",fill="y")
	tkpack(listbox_frame,side="left",anchor="n",fill="both",expand=1)

	options_frame <- tkframe(testlist_frame)
	testlist_browse <- tkbutton(options_frame,
						text="Load my_testlist.R",
						width=14,
						command=function() {
							testlist_browser()
							# update my_listbox
							testlist_end <- as.numeric(tclObj(fp_testlist_count))
							#cat(sprintf("testlist_end is %d \n",testlist_end))
							if(testlist_end>0) {
								my_list <- vector()
								for(j in 1:testlist_end) {
									my_list[j] = paste(tclObj(fp_testlist[[j]]),sep="",collapse=" ")
								}
							} else {
								my_list = ""
							}
							tkdelete(my_listbox,0,"end")
							lapply(my_list,function(my_item) tkinsert(my_listbox,"end",my_item))
						})  
	tkpack(testlist_browse,side="top")
	ref_browse <- tkbutton(options_frame,
					text="Browse ref_rtdf",
					width=14,
					command=function() {
						# REVISIT.. need code here!
						paramframe_browser(my_listbox)
					})
	tkpack(ref_browse,side="top")
	delete_button <- tkbutton(options_frame,
					text="Delete",
					width=14,
					command=function() {
						tmp = as.character(tclvalue(tkcurselection(my_listbox)))
						#cat(tmp)
						my_indices = as.numeric(unlist(strsplit(tmp," ")))
						if( length(my_indices)>0 ) {
							testlist_end = as.numeric(tclObj(fp_testlist_count))
							# start at back of list, so indices stay intact as deleting
							for (j in length(my_indices):1) {
								idx = my_indices[j]
								tkdelete(my_listbox,idx,idx)
								if((idx+1)<testlist_end) {
									for(k in (idx+1):(testlist_end-1)) {
										tclvalue(fp_testlist[[k]]) <- paste(
											tclObj(fp_testlist[[k+1]]),sep="",collapse=" ")
									}
								}
								testlist_end = testlist_end - 1
							}
						}
					})
	tkpack(delete_button,side="top")
	clear_all_button <- tkbutton(options_frame,
					text="Clear All",
					width=14,
					command=function() {
						tclvalue(fp_testlist_count) <- 0 
						for (j in 1:30) {
							tclvalue(fp_testlist[[j]]) <- ""
						}
						tkdelete(my_listbox,0,"end")
					})
	tkpack(clear_all_button,side="top")
	# buttons go here...
	# ok - clear all
	# ok - delete.. or will list be live, can do in list itself
	# - add element
	# ok - read from testlist
	# - browse param list.. brings up popup window
	tkpack(options_frame,side="left",anchor="n")

	tkpack(testlist_frame,side="top",anchor="w",fill="both",expand=1)

# REVISIT
# ... add test list display scroll window + testlist.R browse button +
#     ref_rtdf param list browse button + delete button + insert entry +
#     up/down list button ?

}


