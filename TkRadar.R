# TkRadar.R
#
# $Id: TkRadar.R,v 1.55 2020/12/18 01:30:31 david Exp $
#
# top level Tk/Tcl GUI wrapper for calling Radar.R scripts
# calls various xxxxxGui.R Tk gui wrappers
#
# Copyright (C) 2008-2020 David Gattrell
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
# this script defines TkRadar() function and
# sources the related gui subfiles 
#

# namespace for the function... main function Global variables and local functions
#-------------------------------------------------------
if(is.finite(match(".TkRadar.env",search())))  detach(.TkRadar.env)
if(exists(".TkRadar.env")) rm(.TkRadar.env) 
.TkRadar.env <- new.env()

# separate environment that we don't attach, use for Tk windows...
#-----------------------------------------------------------------
if(exists(".TkRadar.wins")) rm(.TkRadar.wins) 
.TkRadar.wins <- new.env()



require(tcltk)
# require(Radar) ...when it is finally a proper package

# TkRadar namespace
assign("Rtdf_name",tclVar(""),envir=.TkRadar.env)
assign("Rtdf_dir",tclVar(""),envir=.TkRadar.env)
assign("Rtdfs_dir",tclVar(""),envir=.TkRadar.env)
assign("Pdf_name",tclVar(""),envir=.TkRadar.env)

assign("Output_dir",tclVar(""),envir=.TkRadar.env)
assign("Orig_dir",tclVar(""),envir=.TkRadar.env)
assign("Source_dir",tclVar(getwd()),envir=.TkRadar.env)

assign("My_entry",tclVar(""),envir=.TkRadar.env)
assign("My_index",tclVar(""),envir=.TkRadar.env)
assign("My_indices_entry",tclVar(""),envir=.TkRadar.env)

assign("TkRadar_logfile",tclVar("TkRadar.log"),envir=.TkRadar.env)
assign("TkRadar_verbose",tclVar(1),envir=.TkRadar.env)

assign("Bad_Vista",tclVar(0),envir=.TkRadar.env)			# Microsoft Vista/Windows7 
assign("Use_MS_dir_browser",tclVar(0),envir=.TkRadar.env)	# Microsoft Vista/Windows7 
assign("Filetype_list_reversed",tclVar(0),envir=.TkRadar.env)	# Microsoft Vista/Windows7 

sys.source("Convert9470CSVGui.R",envir=.TkRadar.env)
sys.source("ConvertA5xxGui.R",envir=.TkRadar.env)
sys.source("ConvertCsvGui.R",envir=.TkRadar.env)
sys.source("ConvertEagleCSVGui.R",envir=.TkRadar.env)
sys.source("ConvertEDLGui.R",envir=.TkRadar.env)
sys.source("ConvertETSGui.R",envir=.TkRadar.env)
sys.source("ConvertFrugalGui.R",envir=.TkRadar.env)
sys.source("ConvertHP9490Gui.R",envir=.TkRadar.env)
sys.source("ConvertJ9Gui.R",envir=.TkRadar.env)
sys.source("ConvertKDFGui.R",envir=.TkRadar.env)
sys.source("ConvertStdfGui.R",envir=.TkRadar.env)
sys.source("ExpandMPRsGui.R",envir=.TkRadar.env)
sys.source("ConvertParametersGui.R",envir=.TkRadar.env)
sys.source("Rtdf2DeducerGui.R",envir=.TkRadar.env)

sys.source("FilterByBinningGui.R",envir=.TkRadar.env)
sys.source("FilterByIndicesGui.R",envir=.TkRadar.env)
sys.source("FilterByResultGui.R",envir=.TkRadar.env)
#source("FilterDevicesGui.R") obsoleted by FilterByResult
sys.source("FindFirstFailsGui.R",envir=.TkRadar.env)
sys.source("FingerprintGui.R",envir=.TkRadar.env)
#sys.source("JustBin1sGui.R",envir=.TkRadar.env) obsoleted by FilterByBinning
sys.source("MergeNewTestsGui.R",envir=.TkRadar.env)
sys.source("MergeRtdfGui.R",envir=.TkRadar.env)
sys.source("NonGating2GatingGui.R",envir=.TkRadar.env)
sys.source("RemoveAtXYGui.R",envir=.TkRadar.env)
#sys.source("RemoveDevicesAtIndicesGui.R",envir=.TkRadar.env) obsoleted by FilterByIndices
sys.source("ReplaceTestnamesGui.R",envir=.TkRadar.env)
sys.source("RobustFilterGui.R",envir=.TkRadar.env)
sys.source("RtdfTransformGui.R",envir=.TkRadar.env)
sys.source("ShrinkRetestsGui.R",envir=.TkRadar.env)
sys.source("SplitBySubstrGui.R",envir=.TkRadar.env)
sys.source("SplitConditionsGui.R",envir=.TkRadar.env)
sys.source("SplitSitesGui.R",envir=.TkRadar.env)
sys.source("SplitWafersGui.R",envir=.TkRadar.env)
sys.source("XYWid2PartidGui.R",envir=.TkRadar.env)

sys.source("AsciiWaferMapGui.R",envir=.TkRadar.env)
sys.source("ControlChartsGui.R",envir=.TkRadar.env)
sys.source("PlotRtdfGui.R",envir=.TkRadar.env)
sys.source("PlotTestvsTestGui.R",envir=.TkRadar.env)
sys.source("PlotVsRunGui.R",envir=.TkRadar.env)
sys.source("ProbeVsReprobeGui.R",envir=.TkRadar.env)
sys.source("WaferMapGui.R",envir=.TkRadar.env)

sys.source("LoadRtdfGui.R",envir=.TkRadar.env)
sys.source("SaveRtdfGui.R",envir=.TkRadar.env)

sys.source("TkRadarDefaultsGui.R",envir=.TkRadar.env)


#tclvalue(.Radar$.TkRadar.env$Source_dir) <- getwd()

#----------------------------------------------------
numeric_entry <-function(my_var) {
	# pop up window with entry field
	# and ok and quit buttons...
	# when hit ok, checks if entry is numeric,
	# if so, set my_var
	# else set entry background to yellow,
	# pop up that says bad value!

	tclvalue(My_entry) <- as.numeric(tclObj(my_var))

	num_entry_win <- tktoplevel()
	tkwm.title(num_entry_win, "Numeric Entry")

	num_entry <- tkentry(num_entry_win,
						width=30,
						textvariable=My_entry)
	tkpack(num_entry,side="top",fill="x",expand=1)


	bottom_row <- tkframe(num_entry_win)
	cancel_button <- tkbutton(bottom_row,
					text="QUIT",
					#anchor="w",
					width=12,
					command=function()tkdestroy(num_entry_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
					text="OK",
					#anchor="w",
					width=12,
					command=function() {
						my_entry <- as.numeric(tclObj(My_entry))
						if(is.finite(my_entry)) {
							tclvalue(my_var) <- my_entry
							tkdestroy(num_entry_win)
						} else {
							tkmessageBox(message="Entry not a valid number,\n please try again")
						}
					})
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

}

#----------------------------------------------------
integer_entry <-function(my_var) {
	# pop up window with entry field
	# and ok and quit buttons...
	# when hit ok, checks if entry is numeric,
	# if so, set my_var
	# else set entry background to yellow,
	# pop up that says bad value!

	tclvalue(My_entry) <- as.numeric(tclObj(my_var))

	num_entry_win <- tktoplevel()
	tkwm.title(num_entry_win, "Integer Entry")

	num_entry <- tkentry(num_entry_win,
						width=30,
						textvariable=My_entry)
	tkpack(num_entry,side="top",fill="x",expand=1)


	bottom_row <- tkframe(num_entry_win)
	cancel_button <- tkbutton(bottom_row,
					text="QUIT",
					#anchor="w",
					width=12,
					command=function()tkdestroy(num_entry_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
					text="OK",
					#anchor="w",
					width=12,
					command=function() {
						my_entry <- as.numeric(tclObj(My_entry))
						if(is.finite(my_entry)) {
							tclvalue(my_var) <- as.integer(my_entry)
							tkdestroy(num_entry_win)
						} else {
							tkmessageBox(message="Entry not a valid number,\n please try again")
						}
					})
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

}
#----------------------------------------------------
index_entry <-function(my_var) {
	# pop up window with entry field
	# and ok and quit buttons...
	# when hit ok, checks if entry is whole number,
	# if so, set my_var
	# else set entry background to yellow,
	# pop up that says bad value!

	tclvalue(My_index) <- as.numeric(tclObj(my_var))

	index_entry_win <- tktoplevel()
	tkwm.title(index_entry_win, "Index Entry")

	index_entry <- tkentry(index_entry_win,
						width=30,
						textvariable=My_index)
	tkpack(index_entry,side="top",fill="x",expand=1)


	bottom_row <- tkframe(index_entry_win)
	cancel_button <- tkbutton(bottom_row,
					text="QUIT",
					#anchor="w",
					width=12,
					command=function()tkdestroy(index_entry_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
					text="OK",
					#anchor="w",
					width=12,
					command=function() {
						my_entry <- as.integer(tclObj(My_index))
						if(is.finite(my_entry) && my_entry>0) {
							tclvalue(my_var) <- as.integer(my_entry)
							tkdestroy(index_entry_win)
						} else {
							tkmessageBox(message="Entry not a valid whole number,\n please try again")
						}
					})
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

}


#----------------------------------------------------
indices_entry <-function(my_var) {
	# pop up window with entry field
	# and ok and quit buttons...
	# when hit ok, checks if entry is list of whole numbers,
	# if so, set my_var
	# else set entry background to yellow,... haven't done any colour stuff yet.
	# pop up that says bad value!

	tclvalue(My_indices_entry) <- as.numeric(tclObj(my_var))

	indx_entry_win <- tktoplevel()
	tkwm.title(indx_entry_win, "Indices Entry")
	#current_geometry = tkwm.geometry(indx_entry_win)
	#tkwm.geometry(indx_entry_win, "300x200-5+40")	# width.height+/-x+/-y

	num_entry <- tkentry(indx_entry_win,
						width=30,
						textvariable=My_indices_entry)
	tkpack(num_entry,side="top",fill="x",expand=1)

	bottom_row <- tkframe(indx_entry_win)
	cancel_button <- tkbutton(bottom_row,
					text="QUIT",
					#anchor="w",
					width=12,
					command=function()tkdestroy(indx_entry_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
					text="OK",
					#anchor="w",
					width=12,
					command=function() {
						my_entry = paste(tclObj(My_indices_entry),sep="",collapse=" ")
						my_entry = gsub('[[:blank:]]+',",",my_entry)
						my_entry = gsub(",{2,}",",",my_entry)
						my_entry = paste("c(",my_entry,")",sep="")
						my_entry = eval(parse(text=my_entry))
						if(is.numeric(my_entry)) {
							tclvalue(my_var) <- as.integer(my_entry)
							tkdestroy(indx_entry_win)
						} else {
							tkmessageBox(message="Entry not a valid index list,\n please try again")
						}
					})
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")
}


#----------------------------------------------------
split_multiple_filenames <- function(in_string) {

	# if paths/names contain spaces, then string will start with "{"
	# and we need to split by curly brackets and remove "{", "} {", "}"
	# else split by spaces into names...

	if(length(grep("^[{]",in_string))>0) {
		names = strsplit(in_string,"}")[[1]]
		names = sub(" *[{]","",names)
	} else {
		names = strsplit(in_string," ")[[1]]
	}
	
	return( names )
}


#----------------------------------------------------
change_Output_dir <-function(new_dir) {

	# is new dir really a change?
	output_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")

	#browser()
	if(output_dir != new_dir) {
		tclvalue(Output_dir) <- new_dir
		if (nchar(new_dir)>0)  setwd(new_dir)

		# if changing, then need to update input directories
		# since they can be relative if they are ""

		# stdf_dir (ConvertStdfGui)
		#---------------------------
		my_dir <- paste(tclObj(stdf_dir),sep="",collapse=" ")
		if(nchar(my_dir)>0) {
			if(my_dir == new_dir)  tclvalue(stdf_dir) <- ""
		} else {
			my_name <- paste(tclObj(stdf_name),sep="",collapse=" ")
			if(nchar(my_name)>0) {
				tclvalue(stdf_dir) <- output_dir
			}
		}
		
		# in_dir (ConvertParametersGui)
		#------------------------------

		# csv_in_dir (ConvertCsvGui)
		#------------------------------
		my_dir <- paste(tclObj(csv_in_dir),sep="",collapse=" ")
		if(nchar(my_dir)>0) {
			if(my_dir == new_dir)  tclvalue(csv_in_dir) <- ""
		} else {
			my_name <- paste(tclObj(csv_in_name),sep="",collapse=" ")
			if(nchar(my_name)>0) {
				tclvalue(csv_in_dir) <- output_dir
			}
		}


		# Rtdf_dir (Lots of them...)
		#----------------------------
		my_dir <- paste(tclObj(Rtdf_dir),sep="",collapse=" ")
		if(nchar(my_dir)>0) {
			if(my_dir == new_dir)  tclvalue(Rtdf_dir) <- ""
		} else {
			my_name <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
			if(nchar(my_name)>0) {
				tclvalue(Rtdf_dir) <- output_dir
			}
		}

		# Rtdfs_dir (see below...)
		#----------------------------
		my_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
		if(nchar(my_dir)>0) {
			if(my_dir == new_dir)  tclvalue(Rtdfs_dir) <- ""
		}

		# rtdf_dirs (PlotRtdfGui)
		#------------------------
		# these are relative to Rtdfs_dir...

		# merge_rtdf_dirs (MergeRtdfGui)
		#-------------------------------
		# these are relative to Rtdfs_dir...
	}
}

#----------------------------------------------------
change_Rtdfs_dir <-function(new_dir) {

	# is new dir really a change?
	orig_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")

	if(orig_dir != new_dir) {
		output_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
		if(new_dir==output_dir)  tclvalue(Rtdfs_dir) <- ""
		else  tclvalue(Rtdfs_dir) <- new_dir

		# if changing, then need to update input rtdf directories
		# since they can be relative if they are ""

		# rtdf_dirs (PlotRtdfGui)
		#------------------------
		for (j in 1:20) {
			my_name <- paste(tclObj(rtdf_names[[j]]),sep="",collapse=" ")
			my_dir <- paste(tclObj(rtdf_dirs[[j]]),sep="",collapse=" ")

			if(nchar(my_dir)>0) {
				if(my_dir == new_dir)  tclvalue(rtdf_dirs[[j]]) <- ""
			} else {
				if(nchar(my_name)>0)  tclvalue(rtdf_dirs[[j]]) <- orig_dir
			}
		}


		# merge_rtdf_dirs (MergeRtdfGui)
		#-------------------------------
		for (j in 1:30) {
			my_name <- paste(tclObj(merge_rtdf_names[[j]]),sep="",collapse=" ")
			my_dir <- paste(tclObj(merge_rtdf_dirs[[j]]),sep="",collapse=" ")

			if(nchar(my_dir)>0) {
				if(my_dir == new_dir)  tclvalue(merge_rtdf_dirs[[j]]) <- ""
			} else {
				if(nchar(my_name)>0)  tclvalue(merge_rtdf_dirs[[j]]) <- orig_dir
			}
		}
	}
}


#----------------------------------------------------
dir_browser <-function(my_dir) {

	init_dir <- paste(tclObj(my_dir),sep="",collapse=" ")
	if (nchar(init_dir)<1) {
		init_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(init_dir)<1) {
		init_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	if ( (as.character(Sys.info()["sysname"])=="Windows") && (as.numeric(tclObj(Use_MS_dir_browser))==1) ) {
#	if ( (as.character(Sys.info()["sysname"])=="Windows") &&
#			(substr(as.character(Sys.info()["release"]),1,2)=="XP") ) {
		win_init_dir <- gsub("/","\\\\",init_dir)
		win_name <- choose.dir(default=win_init_dir)	# broke in Vista and newer...
		if(is.na(win_name))  win_name=""
		name <- gsub("\\","/",win_name,fixed=TRUE)
	} else {
		name <- tclvalue(tkchooseDirectory(initialdir=init_dir))
	}
	# name <- choose.dir(default=init_dir)
	# and then need to replace \\ with /

	#browser()
	if (nchar(name)>0) {
		# if my_dir is Output_dir, then use special command
		if (as.character(my_dir)==as.character(Output_dir)) {
			change_Output_dir(name)
		} else if (as.character(my_dir)==as.character(Rtdfs_dir)) {
			change_Rtdfs_dir(name)
		} else {
			# if same as Output_dir, set it to ""
			out_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
			if(name == out_dir)  tclvalue(my_dir) <- ""
			else  tclvalue(my_dir) <- name
		}
	}
}


#----------------------------------------------------
rtdf_browser <-function(my_name,my_dir,output=FALSE) {

	if (as.character(my_dir)==as.character(Output_dir)) {
		init_dir = paste(tclObj(Output_dir),sep="",collapse=" ")
	} else if (as.character(my_dir)==as.character(Rtdf_dir)) {
		#if (nchar(paste(tclObj(my_name),sep="",collapse=" "))>0)   init_dir = paste(tclObj(Rtdf_dir),sep="",collapse=" ")
		#else  init_dir = paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
		init_dir = paste(tclObj(Rtdf_dir),sep="",collapse=" ")
	} else {
		init_dir = paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
	}
	if (nchar(init_dir)<1) {
		init_dir <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
	}
	if (nchar(init_dir)<1) {
		init_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(init_dir)<1) {
		init_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	if(as.numeric(tclObj(Filetype_list_reversed))>0) {
		my_str = "{{All files} *} {{RTDF Files} {.rtdf .Rtdf .Rdata}}"
	} else {
		my_str = "{{RTDF Files} {.rtdf .Rtdf .Rdata}} {{All files} *}"
	}
	if (nchar(init_dir)>0) {
		if (output) {
			name <- tclvalue(tkgetSaveFile(filetypes=my_str,
					initialdir=init_dir))
		} else {
			name <- tclvalue(tkgetOpenFile(filetypes=my_str,
					initialdir=init_dir))
		}
	} else {
		if (output) {
			name <- tclvalue(tkgetSaveFile(filetypes=my_str))
		} else {
			name <- tclvalue(tkgetOpenFile(filetypes=my_str))
		}
	}
	
	if (nchar(name)>0) {
		my_filepath = sub("/[^/]*$","",name)	# strip off filename, just path left
		name=sub("^.*/","",name)
		tclvalue(my_name) <- name

		# if we're not in default directory... set my_dir accordingly
		my_pwd = paste(tclObj(Output_dir),sep="",collapse=" ")
		if(my_filepath == my_pwd) {
			tclvalue(my_dir) <- ""
		} else {
			# if my_dir was Output_dir, then use special command to change it
			if (as.character(my_dir)==as.character(Output_dir)) {
				change_Output_dir(my_filepath)
			} else if(as.character(my_dir)==as.character(Rtdfs_dir)) {
				change_Rtdfs_dir(my_filepath)
			} else {
				tclvalue(my_dir) <- my_filepath
			}
		}
	}
}


#----------------------------------------------------
pdf_browser <-function(my_name) {
	orig_name = as.character(tclObj(my_name))
	if ((length(orig_name)<1) || (nchar(orig_name)<1))  orig_name="output.pdf"

	output_dir = paste(tclObj(Output_dir),sep="",collapse=" ")
	if (nchar(output_dir)<1) {
		output_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	if(as.numeric(tclObj(Filetype_list_reversed))>0) {
		my_str = "{{All files} *} {{PDF Files} {.pdf .PDF}}"
	} else {
		my_str = "{{PDF Files} {.pdf .PDF}} {{All files} *}"
	}
	if (nchar(output_dir)>0) {
		name <- tclvalue(tkgetSaveFile(
				initialfile=orig_name,
				initialdir=output_dir,
				filetypes=my_str))
	} else {
		name <- tclvalue(tkgetSaveFile(
				initialfile=orig_name,
				filetypes=my_str))
	}

	if (nchar(name)>0) {
		my_filepath = sub("/[^/]*$","",name)	# strip off filename, just path left
		name=sub("^.*/","",name)
		tclvalue(my_name) <- name

		# if we changed directory... update paths
		my_pwd = paste(tclObj(Output_dir),sep="",collapse=" ")
		if(my_filepath != my_pwd) {
			change_Output_dir(my_filepath)
		}
	}
}


#----------------------------------------------------
csv_browser <-function(my_name) {
	orig_name = as.character(tclObj(my_name))
	if ((length(orig_name)<1) || (nchar(orig_name)<1))  orig_name="output.csv"

	output_dir = paste(tclObj(Output_dir),sep="",collapse=" ")
	if (nchar(output_dir)<1) {
		output_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	if(as.numeric(tclObj(Filetype_list_reversed))>0) {
		my_str = "{{All files} *} {{CSV Files} {.csv .CSV}}"
	} else {
		my_str = "{{CSV Files} {.csv .CSV}} {{All files} *}"
	}
	if (nchar(output_dir)>0) {
		name <- tclvalue(tkgetSaveFile(
				initialfile=orig_name,
				initialdir=output_dir,
				filetypes=my_str))
	} else {
		name <- tclvalue(tkgetSaveFile(
				initialfile=orig_name,
				filetypes=my_str))
	}

	if (nchar(name)>0) {
		my_filepath = sub("/[^/]*$","",name)	# strip off filename, just path left
		name=sub("^.*/","",name)
		tclvalue(my_name) <- name

		# if we changed directory... update paths
		my_pwd = paste(tclObj(Output_dir),sep="",collapse=" ")
		if(my_filepath != my_pwd) {
			change_Output_dir(my_filepath)
		}
	}
}


#----------------------------------------------------
wmap_browser <-function(wmap_name) {
	orig_name = paste(tclObj(wmap_name),sep="",collapse=" ")
	if ((length(orig_name)<1) || (nchar(orig_name)<1))  orig_name="ascii.wmap"

	output_dir = paste(tclObj(Output_dir),sep="",collapse=" ")
	if (nchar(output_dir)<1) {
		output_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	if(as.numeric(tclObj(Filetype_list_reversed))>0) {
		my_str = "{{All files} *} {{wmap Files} {.wmap .WMAP}}"
	} else {
		my_str = "{{wmap Files} {.wmap .WMAP}} {{All files} *}"
	}
	if (nchar(output_dir)>0) {
		name <- tclvalue(tkgetSaveFile(
				initialfile=orig_name,
				initialdir=output_dir,
				filetypes=my_str))
	} else {
		name <- tclvalue(tkgetSaveFile(
				initialfile=orig_name,
				filetypes=my_str))
	}

	if (nchar(name)>0) {
		my_filepath = sub("/[^/]*$","",name)	# strip off filename, just path left
		name=sub("^.*/","",name)
		tclvalue(wmap_name) <- name

		# if we changed directory... update paths
		my_pwd = paste(tclObj(Output_dir),sep="",collapse=" ")
		if(my_filepath != my_pwd) {
			change_Output_dir(my_filepath)
		}
	}
}

#-----------------------------------------------------
#param_browser <-function(tk_param,tk_file,tk_dir,param_win) {
param_browser <-function(tk_param,tk_file,tk_dir) {

	my_param <- paste(tclObj(tk_param),sep="",collapse=" ")
	my_file <- paste(tclObj(tk_file),sep="",collapse=" ")
	my_dir <- paste(tclObj(tk_dir),sep="",collapse=" ")
	if (nchar(my_dir)<1) {
		my_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(my_dir)<1) {
		my_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}


	if (nchar(my_file)>0) {

		sort_sort <- tclVar("exec")

		# load ParametersFrame from rtdf file
		if (my_dir != "") {
			orig_dir <- getwd()
			setwd(my_dir)
		}
		load(my_file)
		if (my_dir != "")  setwd(orig_dir)


		param_count <- dim(ParametersFrame)[1]

		my_list <- ParametersFrame[["testname"]]

		if (exists("param_win",envir=.TkRadar.wins,inherits=FALSE)) {
			param_win <- get("param_win",envir=.TkRadar.wins)
		}
		if (exists("param_win") && as.logical(tkwinfo("exists",param_win)))  tkdestroy(param_win)
		param_win <- tktoplevel()
		assign("param_win",param_win,envir=.TkRadar.wins)
		tkwm.title(param_win, "Parameter Browser")

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

		# label saying how many parameters
		my_text <- sprintf("There are %d parameters",param_count)
		count_label <- tklabel(param_win,
						text=my_text)
		tkpack(count_label,side="top",anchor="w")

		# radio buttons to select list order...
		#   execution
		#   test number
		#   alphabetic
#		exec_sort_button <- tkradiobutton(param_win,
#							text="sorted by execution order",
#							value="exec",
#							#command=update_sort,
#							variable=sort_sort)
#		tkpack(exec_sort_button,side="top",anchor="w")
#		num_sort_button <- tkradiobutton(param_win,
#							text="sorted by test number",
#							value="num",
#							#command=update_sort,
#							variable=sort_sort)
#		tkpack(num_sort_button,side="top",anchor="w")
#		alpha_sort_button <- tkradiobutton(param_win,
#							text="sorted alphanumerically",
#							value="alpha",
#							#command=update_sort,
#							variable=sort_sort)
#		tkpack(alpha_sort_button,side="top",anchor="w")
#
		# entry box for testname starts with

		# entry box for testname contains

		#   35 row scrollable window for parameters
		if(param_count<35)  my_height <- param_count
		else  my_height <- 35

		listbox_frame <- tkframe(param_win)
		param_listbox <- tklistbox(listbox_frame,
							selectmode="single",
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
		# tkpack(listbox_frame ...  move to after tkpack(bottom row ...

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

		bottom_row <- tkframe(param_win)
		cancel_button <- tkbutton(bottom_row,
						text="QUIT",
						#anchor="w",
						width=12,
						command=function() tkdestroy(param_win))
		tkpack(cancel_button,side="right")
	
		apply_button <- tkbutton(bottom_row,
						text="OK",
						#anchor="w",
						width=12,
						command=function() {
							my_index <- as.numeric(tclvalue(tkcurselection(param_listbox)))
							if(is.finite(my_index)) {
								tclvalue(tk_param) <- as.character(my_list[my_index+1])
								#cat(sprintf(">>%s<<\n",as.character(my_list[my_index+1])))	# debugging
								#debug_var = as.character(my_list[my_index+1])
								#cat(sprintf(">>%s<<\n",debug_var))		# trailing whitespace still there
								#debug_var = paste(tclObj(tk_param),sep="",collapse=" ")
								#cat(sprintf(">>%s<<\n",debug_var))		# trailing whitespace trimmed!
							}
							tkdestroy(param_win)
						})
		tkpack(apply_button,side="right")
		tkpack(bottom_row,side="bottom",anchor="w")

		# pack the listbox_frame last, so it is the one that gets squeezed when the overall window shrinks
		tkpack(listbox_frame,side="top",anchor="w",fill="both",expand=1)
	}
}



#-----------------------------------------------------
XYparam_browser <-function(tk_Xparam,tk_Yparam,tk_file,tk_dir) {

#	my_Xparam <- paste(tclObj(tk_Xparam),sep="",collapse=" ")
#	my_Yparam <- paste(tclObj(tk_Yparam),sep="",collapse=" ")
	my_file <- paste(tclObj(tk_file),sep="",collapse=" ")
	my_dir <- paste(tclObj(tk_dir),sep="",collapse=" ")
	if (nchar(my_dir)<1) {
		my_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(my_dir)<1) {
		my_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}


	if (nchar(my_file)>0) {

		sort_sort <- tclVar("exec")

		# load ParametersFrame from rtdf file
		if (my_dir != "") {
			orig_dir <- getwd()
			setwd(my_dir)
		}
		load(my_file)
		if (my_dir != "")  setwd(orig_dir)


		param_count <- dim(ParametersFrame)[1]

		my_list <- ParametersFrame[["testname"]]

		if (exists("XYparam_win",envir=.TkRadar.wins,inherits=FALSE)) {
			XYparam_win <- get("XYparam_win",envir=.TkRadar.wins)
		}
		if (exists("XYparam_win") && as.logical(tkwinfo("exists",XYparam_win)))  tkdestroy(XYparam_win)
		XYparam_win <- tktoplevel()
		assign("XYparam_win",XYparam_win,envir=.TkRadar.wins)
		tkwm.title(XYparam_win, "XY Parameter Browser")

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

		bottom_row <- tkframe(XYparam_win)
		cancel_button <- tkbutton(bottom_row,
						text="QUIT",
						#anchor="w",
						width=8,
						command=function() tkdestroy(XYparam_win))
		tkpack(cancel_button,side="right")
	
		Yapply_button <- tkbutton(bottom_row,
						text="-> Y",
						#anchor="w",
						width=8,
						command=function() {
							my_index <- as.numeric(tclvalue(tkcurselection(param_listbox)))
							if(is.finite(my_index)) {
								tclvalue(tk_Yparam) <- as.character(my_list[my_index+1])
							}
							#tkdestroy(XYparam_win)
						})
		tkpack(Yapply_button,side="right")
		Xapply_button <- tkbutton(bottom_row,
						text="-> X",
						#anchor="w",
						width=8,
						command=function() {
							my_index <- as.numeric(tclvalue(tkcurselection(param_listbox)))
							if(is.finite(my_index)) {
								tclvalue(tk_Xparam) <- as.character(my_list[my_index+1])
							}
							#tkdestroy(XYparam_win)
						})
		tkpack(Xapply_button,side="right")
		tkpack(bottom_row,side="bottom",anchor="w")

		# - - - - - - - - - - - - - - - - - - - - - - - - - - - -

		# label saying how many parameters
		my_text <- sprintf("There are %d parameters",param_count)
		count_label <- tklabel(XYparam_win,
						text=my_text)
		tkpack(count_label,side="top",anchor="w")


		#   35 row scrollable window for parameters
		if(param_count<35)  my_height <- param_count
		else  my_height <- 35

		listbox_frame <- tkframe(XYparam_win)
		param_listbox <- tklistbox(listbox_frame,
							selectmode="single",
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

		# pack the listbox_frame last, so it is the one that shrinks if the overall window shrinks
		tkpack(listbox_frame,side="top",anchor="w",fill="both",expand=1)

	}
}


#----------------------------------------------------
# A function to mimic a tooltip using tcltk only
# text - the content of the tool tip
# targetWidget - the widget to which the tooltip is going to be associated
# width - the width of the tooltip measured as pixels.

tooltip <- function(text, targetWidget, width = 350){

    end <- function(){
        tkdestroy(base)
    }

    tipX <- as.numeric(tkwinfo("rootx", targetWidget)) +
            as.numeric(tkwinfo("width", targetWidget))
    tipY <- as.numeric(tkwinfo("rooty", targetWidget))

    # Takes out the frame and title bar
    tkwm.overrideredirect(base <- tktoplevel(), TRUE)
    on.exit(tkdestroy(base))
    # Put the TW in the right place
    tkwm.geometry(base, paste("+", tipX, "+", tipY, sep = ""))
    tip <- tklabel(base, text = text, background = "white",
                   wraplength = width)
    tkpack(tip)

    tkbind(targetWidget, "<Leave>", end)

    tkwait.window(base)

    return(invisible())
}


#----------------------------------------------------
TkRadar <- function() {


#	attach(.TkRadar.env)


	tclvalue(Output_dir) <- getwd()


	my_main_win <- tktoplevel()
	tkwm.title(my_main_win, "RADAR 0v6p9dev GUI 16Feb2020")

	# if user has been stuck with Vista or Windows7 (aka Vista with lipstick),
	# set flag for alternate behaviour...
	# ... ok now there is windows 8, in various flavours...
	# We'll set the Bad_Vista flag for all Windows except if the first 2 letters of the
	# release are "XP" or "NT"... hopefully that does what we want, and will work 
	# with future versions of Windows
	tclvalue(Bad_Vista) <- 0
	if (as.character(Sys.info()["sysname"])=="Windows") {
		the_release = as.character(Sys.info()["release"])
#		if ( (the_release=="Vista") || (the_release=="7") ||
#				(the_release=="7 x64") ) {
		if ( (substr(the_release,1,2)!="XP") && (substr(the_release,1,2)!="NT") ) {
			tclvalue(Bad_Vista) <- 1
		}
	}
	

	# Settings section
	#---------------------
	settings_frame <- tkframe(my_main_win)
	label0 <- tklabel(settings_frame,
						#font="helvetica",
						justify="left",
						text="Settings File:")
	tkpack(label0, side="left")
	setname <- tklabel(settings_frame,
						#font="helvetica",
						justify="left",
						width=15,
						relief="sunken",
						textvariable=defaults_file_name)
	tkpack(setname, side="left",fill="x")
	tkpack(settings_frame, side="top",anchor="w")
	settings_frame2 <- tkframe(my_main_win)
	load_button <- tkbutton(settings_frame2,
						text="Load",
						width=8,
						command= function() default_settings_browser(defaults_file_name))
	tkpack(load_button,padx="5m",side="left")
	edit_button <- tkbutton(settings_frame2,
						text="Edit",
						width=8,
						command= function() {
							if (exists("tkradardefaults_win",envir=.TkRadar.wins,inherits=FALSE)) {
								tkradardefaults_win <- get("tkradardefaults_win",envir=.TkRadar.wins)
							}
							if (exists("tkradardefaults_win") && 
								as.logical(tkwinfo("exists",tkradardefaults_win)))  tkraise(tkradardefaults_win)
							else {
								TkRadarDefaultsGui()
								tkradardefaults_win <- get("tkradardefaults_win",envir=.TkRadar.wins)
							}
							tkfocus(tkradardefaults_win)
						})
	tkpack(edit_button,side="left")
	tkpack(settings_frame2, side="top",anchor="w")

	# Converting section
	#---------------------
	label1 <- tklabel(my_main_win,
						font="helvetica",
						justify="left",
						text="Converting")
	tkpack(label1, side="top",anchor="w")

	convstdf_button <- tkbutton(my_main_win,
						text="ConvertStdf",
						anchor="w",
						command=function() {
							if (exists("convertstdf_win",envir=.TkRadar.wins,inherits=FALSE)) {
								convertstdf_win <- get("convertstdf_win",envir=.TkRadar.wins)
							}
							if (exists("convertstdf_win") && 
									as.logical(tkwinfo("exists",convertstdf_win))) {
								tkraise(convertstdf_win)
							} else {
								ConvertStdfGui()
								convertstdf_win <- get("convertstdf_win",envir=.TkRadar.wins)
							}
							tkfocus(convertstdf_win)
						})
	tkpack(convstdf_button,side="top",padx="10m",fill="x")

	convcsv_button <- tkbutton(my_main_win,
						text="ConvertCsv",
						anchor="w",
						command=function() {
							if (exists("convertcsv_win",envir=.TkRadar.wins,inherits=FALSE)) {
								convertcsv_win <- get("convertcsv_win",envir=.TkRadar.wins)
							}
							if (exists("convertcsv_win") && 
									as.logical(tkwinfo("exists",convertcsv_win))) {
								tkraise(convertcsv_win)
							} else {
								ConvertCsvGui()
								convertcsv_win <- get("convertcsv_win",envir=.TkRadar.wins)
							}
							tkfocus(convertcsv_win)
						})
	tkpack(convcsv_button,side="top",padx="10m",fill="x")

	conv_menu_butt <- tkmenubutton(my_main_win,
						text="Full Converting Menu",
						anchor="w",
						relief="raised")
	tkpack(conv_menu_butt,side="top",padx="10m",fill="x")
	conv_menu <- tkmenu(conv_menu_butt,tearoff=TRUE)
	tkconfigure(conv_menu_butt,	menu=conv_menu)
	#tkadd(conv_menu,"separator",label="To Radar...",
	tkadd(conv_menu,"command",label="Convert9470CSV",
			command=function() {
				if (exists("convertcsv94_win",envir=.TkRadar.wins,inherits=FALSE)) {
					convertcsv94_win <- get("convertcsv94_win",envir=.TkRadar.wins)
				}
				if (exists("convertcsv94_win") && 
						as.logical(tkwinfo("exists",convertcsv94_win))) {
					tkraise(convertcsv94_win)
				} else {
					Convert9470CSVGui()
					convertcsv94_win <- get("convertcsv94_win",envir=.TkRadar.wins)
				}
				tkfocus(convertcsv94_win)
			}
		)
	tkadd(conv_menu,"command",label="ConvertA5xx",
			command=function() {
				if (exists("converta5xx_win",envir=.TkRadar.wins,inherits=FALSE)) {
					converta5xx_win <- get("converta5xx_win",envir=.TkRadar.wins)
				}
				if (exists("converta5xx_win") && 
						as.logical(tkwinfo("exists",converta5xx_win))) {
					tkraise(converta5xx_win)
				} else {
					ConvertA5xxGui()
					converta5xx_win <- get("converta5xx_win",envir=.TkRadar.wins)
				}
				tkfocus(converta5xx_win)
			}
		)
	tkadd(conv_menu,"command",label="ConvertCsv",
			command=function() {
				if (exists("convertcsv_win",envir=.TkRadar.wins,inherits=FALSE)) {
					convertcsv_win <- get("convertcsv_win",envir=.TkRadar.wins)
				}
				if (exists("convertcsv_win") && 
						as.logical(tkwinfo("exists",convertcsv_win))) {
					tkraise(convertcsv_win)
				} else {
					ConvertCsvGui()
					convertcsv_win <- get("convertcsv_win",envir=.TkRadar.wins)
				}
				tkfocus(convertcsv_win)
			}
		)
	tkadd(conv_menu,"command",label="ConvertEagleCSV",
			command=function() {
				if (exists("convertcsvETS_win",envir=.TkRadar.wins,inherits=FALSE)) {
					convertcsvETS_win <- get("convertcsvETS_win",envir=.TkRadar.wins)
				}
				if (exists("convertcsvETS_win") && 
						as.logical(tkwinfo("exists",convertcsvETS_win))) {
					tkraise(convertcsvETS_win)
				} else {
					ConvertEagleCSVGui()
					convertcsvETS_win <- get("convertcsvETS_win",envir=.TkRadar.wins)
				}
				tkfocus(convertcsvETS_win)
			}
		)
	tkadd(conv_menu,"command",label="ConvertEDL",
			command=function() {
				if (exists("convertedl_win",envir=.TkRadar.wins,inherits=FALSE)) {
					convertedl_win <- get("convertedl_win",envir=.TkRadar.wins)
				}
				if (exists("convertedl_win") && 
						as.logical(tkwinfo("exists",convertedl_win))) {
					tkraise(convertedl_win)
				} else {
					ConvertEDLGui()
					convertedl_win <- get("convertedl_win",envir=.TkRadar.wins)
				}
				tkfocus(convertedl_win)
			}
		)
	tkadd(conv_menu,"command",label="ConvertETS",
			command=function() {
				if (exists("convertets_win",envir=.TkRadar.wins,inherits=FALSE)) {
					convertets_win <- get("convertets_win",envir=.TkRadar.wins)
				}
				if (exists("convertets_win") && 
						as.logical(tkwinfo("exists",convertets_win))) {
					tkraise(convertets_win)
				} else {
					ConvertETSGui()
					convertets_win <- get("convertets_win",envir=.TkRadar.wins)
				}
				tkfocus(convertets_win)
			}
		)
	tkadd(conv_menu,"command",label="ConvertFrugal",
			command=function() {
				if (exists("convertfrug_win",envir=.TkRadar.wins,inherits=FALSE)) {
					convertfrug_win <- get("convertfrug_win",envir=.TkRadar.wins)
				}
				if (exists("convertfrug_win") && 
						as.logical(tkwinfo("exists",convertfrug_win))) {
					tkraise(convertfrug_win)
				} else {
					ConvertFrugalGui()
					convertfrug_win <- get("convertfrug_win",envir=.TkRadar.wins)
				}
				tkfocus(convertfrug_win)
			}
		)
	tkadd(conv_menu,"command",label="ConvertHP9490",
			command=function() {
				if (exists("convertHP9490_win",envir=.TkRadar.wins,inherits=FALSE)) {
					convertHP9490_win <- get("convertHP9490_win",envir=.TkRadar.wins)
				}
				if (exists("convertHP9490_win") && 
						as.logical(tkwinfo("exists",convertHP9490_win))) {
					tkraise(convertHP9490_win)
				} else {
					ConvertHP9490Gui()
					convertHP9490_win <- get("convertHP9490_win",envir=.TkRadar.wins)
				}
				tkfocus(convertHP9490_win)
			}
		)
	tkadd(conv_menu,"command",label="ConvertJ9",
			command=function() {
				if (exists("convertj9_win",envir=.TkRadar.wins,inherits=FALSE)) {
					convertj9_win <- get("convertj9_win",envir=.TkRadar.wins)
				}
				if (exists("convertj9_win") && 
						as.logical(tkwinfo("exists",convertj9_win))) {
					tkraise(convertj9_win)
				} else {
					ConvertJ9Gui()
					convertj9_win <- get("convertj9_win",envir=.TkRadar.wins)
				}
				tkfocus(convertj9_win)
			}
		)
	tkadd(conv_menu,"command",label="ConvertKDF",
			command=function() {
				if (exists("convertkdf_win",envir=.TkRadar.wins,inherits=FALSE)) {
					convertkdf_win <- get("convertkdf_win",envir=.TkRadar.wins)
				}
				if (exists("convertkdf_win") && 
						as.logical(tkwinfo("exists",convertkdf_win))) {
					tkraise(convertkdf_win)
				} else {
					ConvertKDFGui()
					convertkdf_win <- get("convertkdf_win",envir=.TkRadar.wins)
				}
				tkfocus(convertkdf_win)
			}
		)
	tkadd(conv_menu,"command",label="ConvertParameters",
			command=function() {
				if (exists("convertparameters_win",envir=.TkRadar.wins,inherits=FALSE)) {
					convertparameters_win <- get("convertparameters_win",envir=.TkRadar.wins)
				}
				if (exists("convertparameters_win") && 
						as.logical(tkwinfo("exists",convertparameters_win))) {
					tkraise(convertparameters_win)
				} else {
					ConvertParametersGui()
					convertparameters_win <- get("convertparameters_win",envir=.TkRadar.wins)
				}
				tkfocus(convertparameters_win)
			}
		)
	tkadd(conv_menu,"command",label="ConvertStdf",	 font="bold",
			command=function() {
				if (exists("convertstdf_win",envir=.TkRadar.wins,inherits=FALSE)) {
					convertstdf_win <- get("convertstdf_win",envir=.TkRadar.wins)
				}
				if (exists("convertstdf_win") && 
						as.logical(tkwinfo("exists",convertstdf_win))) {
					tkraise(convertstdf_win)
				} else {
					ConvertStdfGui()
					convertstdf_win <- get("convertstdf_win",envir=.TkRadar.wins)
				}
				tkfocus(convertstdf_win)
			}
		)
	tkadd(conv_menu,"command",label="ExpandMPRs",
			command=function() {
				if (exists("expandmprs_win",envir=.TkRadar.wins,inherits=FALSE)) {
					expandmprs_win <- get("expandmprs_win",envir=.TkRadar.wins)
				}
				if (exists("expandmprs_win") && 
						as.logical(tkwinfo("exists",expandmprs_win))) {
					tkraise(expandmprs_win)
				} else {
					ExpandMPRsGui()
					expandmprs_win <- get("expandmprs_win",envir=.TkRadar.wins)
				}
				tkfocus(expandmprs_win)
			}
		)
	tkadd(conv_menu,"command",label="Rtdf2Deducer",
			command=function() {
				if (exists("rtdf2deducer_win",envir=.TkRadar.wins,inherits=FALSE)) {
					rtdf2deducer_win <- get("rtdf2deducer_win",envir=.TkRadar.wins)
				}
				if (exists("rtdf2deducer_win") && 
						as.logical(tkwinfo("exists",rtdf2deducer_win))) {
					tkraise(rtdf2deducer_win)
				} else {
					Rtdf2DeducerGui()
					rtdf2deducer_win <- get("rtdf2deducer_win",envir=.TkRadar.wins)
				}
				tkfocus(rtdf2deducer_win)
			}
		)

	
	# Manipulating section
	#---------------------
	label2 <- tklabel(my_main_win,
						font="helvetica",
						justify="left",
						text="Manipulating")
	tkpack(label2, side="top",anchor="w")

	manip_menu_butt <- tkmenubutton(my_main_win,
						text="Manipulating Menu",
						anchor="w",
						relief="raised")
	tkpack(manip_menu_butt,side="top",padx="10m",fill="x")
	manip_menu <- tkmenu(manip_menu_butt,tearoff=TRUE)
	tkconfigure(manip_menu_butt,	menu=manip_menu)
	tkadd(manip_menu,"command",label="FilterByBinning",
			command=function() {
				if (exists("filterbybinning_win",envir=.TkRadar.wins,inherits=FALSE)) {
					filterbybinning_win <- get("filterbybinning_win",envir=.TkRadar.wins)
				}
				if (exists("filterbybinning_win") && 
					as.logical(tkwinfo("exists",filterbybinning_win)))  tkraise(filterbybinning_win)
				else {
					FilterByBinningGui()
					filterbybinning_win <- get("filterbybinning_win",envir=.TkRadar.wins)
				}
				tkfocus(filterbybinning_win)
			}
		)
	tkadd(manip_menu,"command",label="FilterByIndices",
			command=function() {
				if (exists("filtbyindex_win",envir=.TkRadar.wins,inherits=FALSE)) {
					filtbyindex_win <- get("filtbyindex_win",envir=.TkRadar.wins)
				}
				if (exists("filtbyindex_win") && 
					as.logical(tkwinfo("exists",filtbyindex_win)))  tkraise(filtbyindex_win)
				else {
					FilterByIndicesGui()
					filtbyindex_win <- get("filtbyindex_win",envir=.TkRadar.wins)
				}
				tkfocus(filtbyindex_win)
			}
		)
	tkadd(manip_menu,"command",label="FilterByResult",
			command=function() {
				if (exists("filterbyresult_win",envir=.TkRadar.wins,inherits=FALSE)) {
					filterbyresult_win <- get("filterbyresult_win",envir=.TkRadar.wins)
				}
				if (exists("filterbyresult_win") && 
					as.logical(tkwinfo("exists",filterbyresult_win)))  tkraise(filterbyresult_win)
				else {
					FilterByResultGui()
					filterbyresult_win <- get("filterbyresult_win",envir=.TkRadar.wins)
				}
				tkfocus(filterbyresult_win)
			}
		)
	tkadd(manip_menu,"command",label="FindFirstFails",
			command=function() {
				if (exists("findfirstfails_win",envir=.TkRadar.wins,inherits=FALSE)) {
					findfirstfails_win <- get("findfirstfails_win",envir=.TkRadar.wins)
				}
				if (exists("findfirstfails_win") && 
					as.logical(tkwinfo("exists",findfirstfails_win)))  tkraise(findfirstfails_win)
				else {
					FindFirstFailsGui()
					findfirstfails_win <- get("findfirstfails_win",envir=.TkRadar.wins)
				}
				tkfocus(findfirstfails_win)
			}
		)
	tkadd(manip_menu,"command",label="Fingerprint",
			command=function() {
				if (exists("fingerprint_win",envir=.TkRadar.wins,inherits=FALSE)) {
					fingerprint_win <- get("fingerprint_win",envir=.TkRadar.wins)
				}
				if (exists("fingerprint_win") && 
					as.logical(tkwinfo("exists",fingerprint_win)))  tkraise(fingerprint_win)
				else {
					FingerprintGui()
					fingerprint_win <- get("fingerprint_win",envir=.TkRadar.wins)
				}
				tkfocus(fingerprint_win)
			}
		)
	tkadd(manip_menu,"command",label="MergeNewTests",
			command=function() {
				if (exists("mergetests_win",envir=.TkRadar.wins,inherits=FALSE)) {
					mergetests_win <- get("mergetests_win",envir=.TkRadar.wins)
				}
				if (exists("mergetests_win") && 
					as.logical(tkwinfo("exists",mergetests_win)))  tkraise(mergetests_win)
				else {
					MergeNewTestsGui()
					mergetests_win <- get("mergetests_win",envir=.TkRadar.wins)
				}
				tkfocus(mergetests_win)
			}
		)
	tkadd(manip_menu,"command",label="MergeRtdf",
			command=function() {
				if (exists("mergertdf_win",envir=.TkRadar.wins,inherits=FALSE)) {
					mergertdf_win <- get("mergertdf_win",envir=.TkRadar.wins)
				}
				if (exists("mergertdf_win") && 
					as.logical(tkwinfo("exists",mergertdf_win)))  tkraise(mergertdf_win)
				else {
					MergeRtdfGui()
					mergertdf_win <- get("mergertdf_win",envir=.TkRadar.wins)
				}
				tkfocus(mergertdf_win)
			}
		)
	tkadd(manip_menu,"command",label="NonGating2Gating",
			command=function() {
				if (exists("nongate2gate_win",envir=.TkRadar.wins,inherits=FALSE)) {
					nongate2gate_win <- get("nongate2gate_win",envir=.TkRadar.wins)
				}
				if (exists("nongate2gate_win") && 
					as.logical(tkwinfo("exists",nongate2gate_win)))  tkraise(nongate2gate_win)
				else {
					NonGating2GatingGui()
					nongate2gate_win <- get("nongate2gate_win",envir=.TkRadar.wins)
				}
				tkfocus(nongate2gate_win)
			}
		)
	tkadd(manip_menu,"command",label="RemoveAtXY",
			command=function() {
				if (exists("removeatxy_win",envir=.TkRadar.wins,inherits=FALSE)) {
					removeatxy_win <- get("removeatxy_win",envir=.TkRadar.wins)
				}
				if (exists("removeatxy_win") && 
					as.logical(tkwinfo("exists",removeatxy_win)))  tkraise(removeatxy_win)
				else {
					RemoveAtXYGui()
					removeatxy_win <- get("removeatxy_win",envir=.TkRadar.wins)
				}
				tkfocus(removeatxy_win)
			}
		)
	tkadd(manip_menu,"command",label="ReplaceTestnames",
			command=function() {
				if (exists("replacetestnames_win",envir=.TkRadar.wins,inherits=FALSE)) {
					replacetestnames_win <- get("replacetestnames_win",envir=.TkRadar.wins)
				}
				if (exists("replacetestnames_win") && 
					as.logical(tkwinfo("exists",replacetestnames_win)))  tkraise(replacetestnames_win)
				else {
					ReplaceTestnamesGui()
					replacetestnames_win <- get("replacetestnames_win",envir=.TkRadar.wins)
				}
				tkfocus(replacetestnames_win)
			}
		)
	tkadd(manip_menu,"command",label="RobustFilter",
			command=function() {
				if (exists("robustfilter_win",envir=.TkRadar.wins,inherits=FALSE)) {
					robustfilter_win <- get("robustfilter_win",envir=.TkRadar.wins)
				}
				if (exists("robustfilter_win") && 
					as.logical(tkwinfo("exists",robustfilter_win)))  tkraise(robustfilter_win)
				else {
					RobustFilterGui()
					robustfilter_win <- get("robustfilter_win",envir=.TkRadar.wins)
				}
				tkfocus(robustfilter_win)
			}
		)
	tkadd(manip_menu,"command",label="RtdfTransform",
			command=function() {
				if (exists("rtdftransform_win",envir=.TkRadar.wins,inherits=FALSE)) {
					rtdftransform_win <- get("rtdftransform_win",envir=.TkRadar.wins)
				}
				if (exists("rtdftransform_win") && 
					as.logical(tkwinfo("exists",rtdftransform_win)))  tkraise(rtdftransform_win)
				else {
					RtdfTransformGui()
					rtdftransform_win <- get("rtdftransform_win",envir=.TkRadar.wins)
				}
				tkfocus(rtdftransform_win)
			}
		)
	tkadd(manip_menu,"command",label="ShrinkRetests",
			command=function() {
				if (exists("shrinkretests_win",envir=.TkRadar.wins,inherits=FALSE)) {
					shrinkretests_win <- get("shrinkretests_win",envir=.TkRadar.wins)
				}
				if (exists("shrinkretests_win") && 
					as.logical(tkwinfo("exists",shrinkretests_win)))  tkraise(shrinkretests_win)
				else {
					ShrinkRetestsGui()
					shrinkretests_win <- get("shrinkretests_win",envir=.TkRadar.wins)
				}
				tkfocus(shrinkretests_win)
			}
		)
	tkadd(manip_menu,"command",label="SplitBySubstr",
			command=function() {
				if (exists("splitbysubstr_win",envir=.TkRadar.wins,inherits=FALSE)) {
					splitbysubstr_win <- get("splitbysubstr_win",envir=.TkRadar.wins)
				}
				if (exists("splitbysubstr_win") && 
					as.logical(tkwinfo("exists",splitbysubstr_win)))  tkraise(splitbysubstr_win)
				else {
					SplitBySubstrGui()
					splitbysubstr_win <- get("splitbysubstr_win",envir=.TkRadar.wins)
				}
				tkfocus(splitbysubstr_win)
			}
		)
	tkadd(manip_menu,"command",label="SplitConditions",
			command=function() {
				if (exists("splitconds_win",envir=.TkRadar.wins,inherits=FALSE)) {
					splitconds_win <- get("splitconds_win",envir=.TkRadar.wins)
				}
				if (exists("splitconds_win") && 
					as.logical(tkwinfo("exists",splitconds_win)))  tkraise(splitconds_win)
				else {
					SplitConditionsGui()
					splitconds_win <- get("splitconds_win",envir=.TkRadar.wins)
				}
				tkfocus(splitconds_win)
			}
		)
	tkadd(manip_menu,"command",label="SplitSites",
			command=function() {
				if (exists("splitsites_win",envir=.TkRadar.wins,inherits=FALSE)) {
					splitsites_win <- get("splitsites_win",envir=.TkRadar.wins)
				}
				if (exists("splitsites_win") && 
					as.logical(tkwinfo("exists",splitsites_win)))  tkraise(splitsites_win)
				else {
					SplitSitesGui()
					splitsites_win <- get("splitsites_win",envir=.TkRadar.wins)
				}
				tkfocus(splitsites_win)
			}
		)
	tkadd(manip_menu,"command",label="SplitWafers",
			command=function() {
				if (exists("splitwafers_win",envir=.TkRadar.wins,inherits=FALSE)) {
					splitwafers_win <- get("splitwafers_win",envir=.TkRadar.wins)
				}
				if (exists("splitwafers_win") && 
					as.logical(tkwinfo("exists",splitwafers_win)))  tkraise(splitwafers_win)
				else {
					SplitWafersGui()
					splitwafers_win <- get("splitwafers_win",envir=.TkRadar.wins)
				}
				tkfocus(splitwafers_win)
			}
		)
	tkadd(manip_menu,"command",label="XYWid2Partid",
			command=function() {
				if (exists("xyw2partid_win",envir=.TkRadar.wins,inherits=FALSE)) {
					xyw2partid_win <- get("xyw2partid_win",envir=.TkRadar.wins)
				}
				if (exists("xyw2partid_win") && 
					as.logical(tkwinfo("exists",xyw2partid_win)))  tkraise(xyw2partid_win)
				else {
					XYWid2PartidGui()
					xyw2partid_win <- get("xyw2partid_win",envir=.TkRadar.wins)
				}
				tkfocus(xyw2partid_win)
			}
		)


	# Plotting section
	#---------------------
	label3 <- tklabel(my_main_win,
						font="helvetica",
						justify="left",
						text="Plots and Statistics")
	tkpack(label3, side="top",anchor="w")

	histo_button <- tkbutton(my_main_win,
						text="PlotRtdf",
						anchor="w",
						command=function() {
							if (exists("plotrtdf_win",envir=.TkRadar.wins,inherits=FALSE)) {
								plotrtdf_win <- get("plotrtdf_win",envir=.TkRadar.wins)
							}
							if (exists("plotrtdf_win") && 
								as.logical(tkwinfo("exists",plotrtdf_win)))  tkraise(plotrtdf_win)
							else {
								PlotRtdfGui()
								plotrtdf_win <- get("plotrtdf_win",envir=.TkRadar.wins)
							}
							tkfocus(plotrtdf_win)
						})
	tkpack(histo_button,side="top",padx="10m",fill="x")
	wmap_button <- tkbutton(my_main_win,
						text="WaferMap",
						anchor="w",
						command=function() {
							if (exists("wafermap_win",envir=.TkRadar.wins,inherits=FALSE)) {
								wafermap_win <- get("wafermap_win",envir=.TkRadar.wins)
							}
							if (exists("wafermap_win") && 
								as.logical(tkwinfo("exists",wafermap_win)))  tkraise(wafermap_win)
							else {
								WaferMapGui()
								wafermap_win <- get("wafermap_win",envir=.TkRadar.wins)
							}
							tkfocus(wafermap_win)
						})
	tkpack(wmap_button,side="top",padx="10m",fill="x")

	plot_menu_butt <- tkmenubutton(my_main_win,
						text="Full Plots & Stats Menu",
						anchor="w",
						relief="raised")
	tkpack(plot_menu_butt,side="top",padx="10m",fill="x")
	plot_menu <- tkmenu(plot_menu_butt,tearoff=TRUE)
	tkconfigure(plot_menu_butt, menu=plot_menu)
	tkadd(plot_menu,"command",label="AsciiWaferMap",
			command=function() {
				if (exists("asciiwafermap_win",envir=.TkRadar.wins,inherits=FALSE)) {
					asciiwafermap_win <- get("asciiwafermap_win",envir=.TkRadar.wins)
				}
				if (exists("asciiwafermap_win") && 
					as.logical(tkwinfo("exists",asciiwafermap_win)))  tkraise(asciiwafermap_win)
				else {
					AsciiWaferMapGui()
					asciiwafermap_win <- get("asciiwafermap_win",envir=.TkRadar.wins)
				}
				tkfocus(asciiwafermap_win)
			}
		)
	tkadd(plot_menu,"command",label="ControlCharts",
			command=function() {
				if (exists("controlcharts_win",envir=.TkRadar.wins,inherits=FALSE)) {
					controlcharts_win <- get("controlcharts_win",envir=.TkRadar.wins)
				}
				if (exists("controlcharts_win") && 
					as.logical(tkwinfo("exists",controlcharts_win)))  tkraise(controlcharts_win)
				else {
					ControlChartsGui()
					controlcharts_win <- get("controlcharts_win",envir=.TkRadar.wins)
				}
			}
		)
	tkadd(plot_menu,"command",label="PlotRtdf",
			command=function() {
				if (exists("plotrtdf_win",envir=.TkRadar.wins,inherits=FALSE)) {
					plotrtdf_win <- get("plotrtdf_win",envir=.TkRadar.wins)
				}
				if (exists("plotrtdf_win") && 
					as.logical(tkwinfo("exists",plotrtdf_win)))  tkraise(plotrtdf_win)
				else {
					PlotRtdfGui()
					plotrtdf_win <- get("plotrtdf_win",envir=.TkRadar.wins)
				}
				tkfocus(plotrtdf_win)
			}
		)
	tkadd(plot_menu,"command",label="PlotTestvsTest",
			command=function() {
				if (exists("plottestvstest_win",envir=.TkRadar.wins,inherits=FALSE)) {
					plottestvstest_win <- get("plottestvstest_win",envir=.TkRadar.wins)
				}
				if (exists("plottestvstest_win") && 
					as.logical(tkwinfo("exists",plottestvstest_win))) {
						tkraise(plottestvstest_win)
				} else {
					PlotTestvsTestGui()
					plottestvstest_win <- get("plottestvstest_win",envir=.TkRadar.wins)
				}
				tkfocus(plottestvstest_win)
			}
		)
	tkadd(plot_menu,"command",label="PlotVsRun",
			command=function() {
				if (exists("plotvsrun_win",envir=.TkRadar.wins,inherits=FALSE)) {
					plotvsrun_win <- get("plotvsrun_win",envir=.TkRadar.wins)
				}
				if (exists("plotvsrun_win") && 
					as.logical(tkwinfo("exists",plotvsrun_win))) {
						tkraise(plotvsrun_win)
				} else {
					PlotVsRunGui()
					plotvsrun_win <- get("plotvsrun_win",envir=.TkRadar.wins)
				}
				tkfocus(plotvsrun_win)
			}
		)
	tkadd(plot_menu,"command",label="ProbeVsReprobe",
			command=function() {
				if (exists("probevsreprobe_win",envir=.TkRadar.wins,inherits=FALSE)) {
					probevsreprobe_win <- get("probevsreprobe_win",envir=.TkRadar.wins)
				}
				if (exists("probevsreprobe_win") && 
					as.logical(tkwinfo("exists",probevsreprobe_win)))  tkraise(probevsreprobe_win)
				else {
					ProbeVsReprobeGui()
					probevsreprobe_win <- get("probevsreprobe_win",envir=.TkRadar.wins)
				}
				tkfocus(probevsreprobe_win)
			}
		)
	tkadd(plot_menu,"command",label="WaferMap",
			command=function() {
				if (exists("wafermap_win",envir=.TkRadar.wins,inherits=FALSE)) {
					wafermap_win <- get("wafermap_win",envir=.TkRadar.wins)
				}
				if (exists("wafermap_win") && 
					as.logical(tkwinfo("exists",wafermap_win)))  tkraise(wafermap_win)
				else {
					WaferMapGui()
					wafermap_win <- get("wafermap_win",envir=.TkRadar.wins)
				}
				tkfocus(wafermap_win)
			}
		)
	tkadd(plot_menu,"command",label="XformWaferMap",
			command=function() {
				if (exists("xformwafermap_win",envir=.TkRadar.wins,inherits=FALSE)) {
					xformwafermap_win <- get("xformwafermap_win",envir=.TkRadar.wins)
				}
				if (exists("xformwafermap_win") && 
					as.logical(tkwinfo("exists",xformwafermap_win)))  tkraise(xformwafermap_win)
				else {
					XformWaferMapGui()
					xformwafermap_win <- get("xformwafermap_win",envir=.TkRadar.wins)
				}
				tkfocus(xformwafermap_win)
			}
		)
						

	# Manual Edits section
	#---------------------
	label3b <- tklabel(my_main_win,
						font="helvetica",
						justify="left",
						text="Manual Edits")
	tkpack(label3b, side="top",anchor="w")

	loadrtdf_button <- tkbutton(my_main_win,
						text="LoadRtdf",
						anchor="w",
						command=function() {
							if (exists("loadrtdf_win",envir=.TkRadar.wins,inherits=FALSE)) {
								loadrtdf_win <- get("loadrtdf_win",envir=.TkRadar.wins)
							}
							if (exists("loadrtdf_win") && 
								as.logical(tkwinfo("exists",loadrtdf_win)))  tkraise(loadrtdf_win)
							else {
								LoadRtdfGui()
								loadrtdf_win <- get("loadrtdf_win",envir=.TkRadar.wins)
							}
							tkfocus(loadrtdf_win)
						})
	tkpack(loadrtdf_button,side="top",padx="10m",fill="x")

	savertdf_button <- tkbutton(my_main_win,
						text="SaveRtdf",
						anchor="w",
						command=function() {
							if (exists("savertdf_win",envir=.TkRadar.wins,inherits=FALSE)) {
								savertdf_win <- get("savertdf_win",envir=.TkRadar.wins)
							}
							if (exists("savertdf_win") && 
								as.logical(tkwinfo("exists",savertdf_win))) {
									tkraise(savertdf_win)
							} else {
								SaveRtdfGui()
								savertdf_win <- get("savertdf_win",envir=.TkRadar.wins)
							}
							tkfocus(savertdf_win)
						})
	tkpack(savertdf_button,side="top",padx="10m",fill="x")


	# Help section
	#--------------
	label3p5 <- tklabel(	my_main_win,
						font="helvetica",
						justify="left",
						text="Help")
	tkpack(label3p5, side="top",anchor="w")
	help_button <- tkbutton(my_main_win,
						text="PDF Manual",
						anchor="w",
						command=function() {
							my_dir = getwd()
							src_dir <- paste(tclObj(Source_dir),sep="",collapse=" ")
							setwd(src_dir)
							# system("ls RADAR_documentation_*.pdf",intern=TRUE)
							my_pdf = "RADAR_documentation_0v6p9.pdf"
							if (as.character(Sys.info()["sysname"])=="Windows") {
								os_com = "cmd /c start"
							} else if (as.character(Sys.info()["sysname"])=="Darwin") { # aka Mac
								os_com = "open"
							} else {  # linux...
								os_com = "xdg-open"
							}
							command_str = paste(os_com,my_pdf)
							system(command_str)
							setwd(my_dir)
						})
	tkpack(help_button,side="top",padx="10m",fill="x")


	# Quiting section
	#------------------
	label4 <- tklabel(	my_main_win,
						font="helvetica",
						justify="left",
						text="Done")
	tkpack(label4, side="top",anchor="w")

	quit_button <- tkbutton(my_main_win,
						text="QUIT",
						anchor="w",
						command=function() {
							#setwd(paste(tclObj(Orig_dir),sep="",collapse=" "))
							if (exists("tkradardefaults_win",envir=.TkRadar.wins,inherits=FALSE)) {
								tkradardefaults_win <- get("tkradardefaults_win",envir=.TkRadar.wins)
								tkdestroy(tkradardefaults_win)
							}
							if (exists("convertcsv94_win",envir=.TkRadar.wins,inherits=FALSE)) {
								convertcsv94_win <- get("convertcsv94_win",envir=.TkRadar.wins)
								tkdestroy(convertcsv94_win)
							}
							if (exists("converta5xx_win",envir=.TkRadar.wins,inherits=FALSE)) {
								converta5xx_win <- get("converta5xx_win",envir=.TkRadar.wins)
								tkdestroy(converta5xx_win)
							}
							if (exists("convertcsv_win",envir=.TkRadar.wins,inherits=FALSE)) {
								convertcsv_win <- get("convertcsv_win",envir=.TkRadar.wins)
								tkdestroy(convertcsv_win)
							}
							if (exists("convertcsvETS_win",envir=.TkRadar.wins,inherits=FALSE)) {
								convertcsvETS_win <- get("convertcsvETS_win",envir=.TkRadar.wins)
								tkdestroy(convertcsvETS_win)
							}
							if (exists("convertedl_win",envir=.TkRadar.wins,inherits=FALSE)) {
								convertedl_win <- get("convertedl_win",envir=.TkRadar.wins)
								tkdestroy(convertedl_win)
							}
							if (exists("convertets_win",envir=.TkRadar.wins,inherits=FALSE)) {
								convertets_win <- get("convertets_win",envir=.TkRadar.wins)
								tkdestroy(convertets_win)
							}
							if (exists("convertfrug_win",envir=.TkRadar.wins,inherits=FALSE)) {
								convertfrug_win <- get("convertfrug_win",envir=.TkRadar.wins)
								tkdestroy(convertfrug_win)
							}
							if (exists("convertHP9490_win",envir=.TkRadar.wins,inherits=FALSE)) {
								convertHP9490_win <- get("convertHP9490_win",envir=.TkRadar.wins)
								tkdestroy(convertHP9490_win)
							}
							if (exists("convertj9_win",envir=.TkRadar.wins,inherits=FALSE)) {
								convertj9_win <- get("convertj9_win",envir=.TkRadar.wins)
								tkdestroy(convertj9_win)
							}
							if (exists("convertkdf_win",envir=.TkRadar.wins,inherits=FALSE)) {
								convertkdf_win <- get("convertkdf_win",envir=.TkRadar.wins)
								tkdestroy(convertkdf_win)
							}
							if (exists("convertstdf_win",envir=.TkRadar.wins,inherits=FALSE)) {
								convertstdf_win <- get("convertstdf_win",envir=.TkRadar.wins)
								tkdestroy(convertstdf_win)
							}
							if (exists("convertparameters_win",envir=.TkRadar.wins,inherits=FALSE)) {
								convertparameters_win <- get("convertparameters_win",envir=.TkRadar.wins)
								tkdestroy(convertparameters_win)
							}
							if (exists("expandmprs_win",envir=.TkRadar.wins,inherits=FALSE)) {
								expandmprs_win <- get("expandmprs_win",envir=.TkRadar.wins)
								tkdestroy(expandmprs_win)
							}
							if (exists("rtdf2deducer_win",envir=.TkRadar.wins,inherits=FALSE)) {
								rtdf2deducer_win <- get("rtdf2deducer_win",envir=.TkRadar.wins)
								tkdestroy(rtdf2deducer_win)
							}
							if (exists("filterbybinning_win",envir=.TkRadar.wins,inherits=FALSE)) {
								filterbybinning_win <- get("filterbybinning_win",envir=.TkRadar.wins)
								tkdestroy(filterbybinning_win)
							}
							if (exists("filtbyindex_win",envir=.TkRadar.wins,inherits=FALSE)) {
								filtbyindex_win <- get("filtbyindex_win",envir=.TkRadar.wins)
								tkdestroy(filtbyindex_win)
							}
							if (exists("filterbyresult_win",envir=.TkRadar.wins,inherits=FALSE)) {
								filterbyresult_win <- get("filterbyresult_win",envir=.TkRadar.wins)
								tkdestroy(filterbyresult_win)
							}
							if (exists("findfirstfails_win",envir=.TkRadar.wins,inherits=FALSE)) {
								findfirstfails_win <- get("findfirstfails_win",envir=.TkRadar.wins)
								tkdestroy(findfirstfails_win)
							}
							if (exists("fingerprint_win",envir=.TkRadar.wins,inherits=FALSE)) {
								fingerprint_win <- get("fingerprint_win",envir=.TkRadar.wins)
								tkdestroy(fingerprint_win)
							}
							if (exists("mergetests_win",envir=.TkRadar.wins,inherits=FALSE)) {
								mergetests_win <- get("mergetests_win",envir=.TkRadar.wins)
								tkdestroy(mergetests_win)
							}
							if (exists("mergertdf_win",envir=.TkRadar.wins,inherits=FALSE)) {
								mergertdf_win <- get("mergertdf_win",envir=.TkRadar.wins)
								tkdestroy(mergertdf_win)
							}
							if (exists("removeatxy_win",envir=.TkRadar.wins,inherits=FALSE)) {
								removeatxy_win <- get("removeatxy_win",envir=.TkRadar.wins)
								tkdestroy(removeatxy_win)
							}
							if (exists("replacetestnames_win",envir=.TkRadar.wins,inherits=FALSE)) {
								replacetestnames_win <- get("replacetestnames_win",envir=.TkRadar.wins)
								tkdestroy(replacetestnames_win)
							}
							if (exists("robustfilter_win",envir=.TkRadar.wins,inherits=FALSE)) {
								robustfilter_win <- get("robustfilter_win",envir=.TkRadar.wins)
								tkdestroy(robustfilter_win)
							}
							if (exists("rtdftransform_win",envir=.TkRadar.wins,inherits=FALSE)) {
								rtdftransform_win <- get("rtdftransform_win",envir=.TkRadar.wins)
								tkdestroy(rtdftransform_win)
							}
							if (exists("shrinkretests_win",envir=.TkRadar.wins,inherits=FALSE)) {
								shrinkretests_win <- get("shrinkretests_win",envir=.TkRadar.wins)
								tkdestroy(shrinkretests_win)
							}
							if (exists("splitbysubstr_win",envir=.TkRadar.wins,inherits=FALSE)) {
								splitbysubstr_win <- get("splitbysubstr_win",envir=.TkRadar.wins)
								tkdestroy(splitbysubstr_win)
							}
							if (exists("splitconds_win",envir=.TkRadar.wins,inherits=FALSE)) {
								splitconds_win <- get("splitconds_win",envir=.TkRadar.wins)
								tkdestroy(splitconds_win)
							}
							if (exists("splitsites_win",envir=.TkRadar.wins,inherits=FALSE)) {
								splitsites_win <- get("splitsites_win",envir=.TkRadar.wins)
								tkdestroy(splitsites_win)
							}
							if (exists("splitwafers_win",envir=.TkRadar.wins,inherits=FALSE)) {
								splitwafers_win <- get("splitwafers_win",envir=.TkRadar.wins)
								tkdestroy(splitwafers_win)
							}
							if (exists("asciiwafermap_win",envir=.TkRadar.wins,inherits=FALSE)) {
								asciiwafermap_win <- get("asciiwafermap_win",envir=.TkRadar.wins)
								tkdestroy(asciiwafermap_win)
							}
							if (exists("controlcharts_win",envir=.TkRadar.wins,inherits=FALSE)) {
								controlcharts_win <- get("controlcharts_win",envir=.TkRadar.wins)
								tkdestroy(controlcharts_win)
							}
							if (exists("plotrtdf_win",envir=.TkRadar.wins,inherits=FALSE)) {
								plotrtdf_win <- get("plotrtdf_win",envir=.TkRadar.wins)
								tkdestroy(plotrtdf_win)
							}
							if (exists("plottestvstest_win",envir=.TkRadar.wins,inherits=FALSE)) {
								plottestvstest_win <- get("plottestvstest_win",envir=.TkRadar.wins)
								tkdestroy(plottestvstest_win)
							}
							if (exists("plotvsrun_win",envir=.TkRadar.wins,inherits=FALSE)) {
								plotvsrun_win <- get("plotvsrun_win",envir=.TkRadar.wins)
								tkdestroy(plotvsrun_win)
							}
							if (exists("probevsreprobe_win",envir=.TkRadar.wins,inherits=FALSE)) {
								probevsreprobe_win <- get("probevsreprobe_win",envir=.TkRadar.wins)
								tkdestroy(probevsreprobe_win)
							}
							if (exists("wafermap_win",envir=.TkRadar.wins,inherits=FALSE)) {
								wafermap_win <- get("wafermap_win",envir=.TkRadar.wins)
								tkdestroy(wafermap_win)
							}
							if (exists("xformwafermap_win",envir=.TkRadar.wins,inherits=FALSE)) {
								xformwafermap_win <- get("xformwafermap_win",envir=.TkRadar.wins)
								tkdestroy(xformwafermap_win)
							}
							if (exists("loadrtdf_win",envir=.TkRadar.wins,inherits=FALSE)) {
								loadrtdf_win <- get("loadrtdf_win",envir=.TkRadar.wins)
								tkdestroy(loadrtdf_win)
							}
							if (exists("savertdf_win",envir=.TkRadar.wins,inherits=FALSE)) {
								savertdf_win <- get("savertdf_win",envir=.TkRadar.wins)
								tkdestroy(savertdf_win)
							}
							if (exists("param_win",envir=.TkRadar.wins,inherits=FALSE)) {
								param_win <- get("param_win",envir=.TkRadar.wins)
								tkdestroy(param_win)
							}
							tkdestroy(my_main_win)
							#detach(.TkRadar.env)
						})
	tkpack(quit_button,side="top",padx="10m",fill="x")

# maybe just a help button would be better than below:
#	tkbind(conv_into_menu_butt,"<Enter>",expression(tooltip("various functions to take data in various formats and convert them to RTDF, the format used by this tool",conv_into_menu_butt)))
#	tkbind(conv_tofrom_menu_butt,"<Enter>",expression(tooltip("functions that can both import/export RTDF data",conv_tofrom_menu_butt)))

}


#############################################################################
#  copy local functions to the .TkRadar.env and remove them
#  from the global environment
#############################################################################
environment(numeric_entry)<-.TkRadar.env
assign("numeric_entry",numeric_entry,envir=.TkRadar.env)
rm(numeric_entry)

environment(integer_entry)<-.TkRadar.env
assign("integer_entry",integer_entry,envir=.TkRadar.env)
rm(integer_entry)

environment(index_entry)<-.TkRadar.env
assign("index_entry",index_entry,envir=.TkRadar.env)
rm(index_entry)

environment(indices_entry)<-.TkRadar.env
assign("indices_entry",indices_entry,envir=.TkRadar.env)
rm(indices_entry)

environment(split_multiple_filenames)<-.TkRadar.env
assign("split_multiple_filenames",split_multiple_filenames,envir=.TkRadar.env)
rm(split_multiple_filenames)

environment(change_Output_dir)<-.TkRadar.env
assign("change_Output_dir",change_Output_dir,envir=.TkRadar.env)
rm(change_Output_dir)

environment(change_Rtdfs_dir)<-.TkRadar.env
assign("change_Rtdfs_dir",change_Rtdfs_dir,envir=.TkRadar.env)
rm(change_Rtdfs_dir)

environment(dir_browser)<-.TkRadar.env
assign("dir_browser",dir_browser,envir=.TkRadar.env)
rm(dir_browser)

environment(rtdf_browser)<-.TkRadar.env
assign("rtdf_browser",rtdf_browser,envir=.TkRadar.env)
rm(rtdf_browser)

environment(pdf_browser)<-.TkRadar.env
assign("pdf_browser",pdf_browser,envir=.TkRadar.env)
rm(pdf_browser)

environment(csv_browser)<-.TkRadar.env
assign("csv_browser",csv_browser,envir=.TkRadar.env)
rm(csv_browser)

environment(wmap_browser)<-.TkRadar.env
assign("wmap_browser",wmap_browser,envir=.TkRadar.env)
rm(wmap_browser)

environment(param_browser)<-.TkRadar.env
assign("param_browser",param_browser,envir=.TkRadar.env)
rm(param_browser)

environment(XYparam_browser)<-.TkRadar.env
assign("XYparam_browser",XYparam_browser,envir=.TkRadar.env)
rm(XYparam_browser)

environment(tooltip)<-.TkRadar.env
assign("tooltip",tooltip,envir=.TkRadar.env)
rm(tooltip)

environment(TkRadar)<-.TkRadar.env

