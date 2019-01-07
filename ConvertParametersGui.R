# ConvertParametersGui.R
#
# $Id: ConvertParametersGui.R,v 1.7 2010/11/24 01:38:28 David Exp $
#
# Tk/Tcl GUI wrapper for calling ConvertParameters.R
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
# ConvertParametersGui specific variables
#-----------------------------------------
if(!exists(".TkRadar.env")) .TkRadar.env <- new.env()

in_name <- tclVar("")
convparam_rtdf_dir <- tclVar("")
convparam_out_name <- tclVar("")

convert_type <- tclVar("r2c")


#----------------------------------------------------
ConvertParametersGui_defaults <- function(...) {
	tclvalue(convparam_out_name) <- "parameters.csv"
	tclvalue(convert_type) <- "r2c"
	tclvalue(convparam_rtdf_dir) <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
	convparam_type()

}


#----------------------------------------------------
convparam_browser <- function(out=FALSE) {
	# based on convert_type and out_flag, browse
	# for rtdf, txt or csv files

	my_type <- as.character(tclObj(convert_type))
	my_dir = ""
	if(as.numeric(tclObj(Bad_Vista))>0) {
		rtdf_str = "{{All files} *} {{RTDF Files} {.rtdf .Rtdf .Rdata}}"
		csv_str = "{{All files} *} {{CSV Files} {.csv}}"
		txt_str = "{{All files} *} {{Text Files} {.txt .text}}}"
	} else {
		rtdf_str = "{{RTDF Files} {.rtdf .Rtdf .Rdata}} {{All files} *}"
		csv_str = "{{CSV Files} {.csv}} {{All files} *}"
		txt_str = "{{Text Files} {.txt .text}} {{All files} *}"
	}
	if (my_type=="r2t") {
		if (out) {
			rtdf_flag = FALSE
			my_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
			type_str=txt_str
		} else {
			rtdf_flag = TRUE
			my_dir <- paste(tclObj(convparam_rtdf_dir),sep="",collapse=" ")
			type_str=rtdf_str
		}
	} else if (my_type=="r2c") {
		if (out) {
			rtdf_flag = FALSE
			my_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
			type_str=csv_str
		} else {
			rtdf_flag = TRUE
			my_dir <- paste(tclObj(convparam_rtdf_dir),sep="",collapse=" ")
			type_str=rtdf_str
		}
	} else if (my_type=="t2r") {
		if (out) {
			my_dir <- paste(tclObj(convparam_rtdf_dir),sep="",collapse=" ")
			type_str=rtdf_str
		} else {
			my_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
			type_str=txt_str
		}
	} else if (my_type=="c2r") {
		if (out) {
			rtdf_flag = TRUE
			my_dir <- paste(tclObj(convparam_rtdf_dir),sep="",collapse=" ")
			type_str=rtdf_str
		} else {
			rtdf_flag = FALSE
			my_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
			type_str=csv_str
		}
	} else {
		type_str="{{All files} *}"
	}
	if (nchar(my_dir)<1) {
		my_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(my_dir)<1) {
		my_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}


	if (out) {
		name <- tclvalue(tkgetSaveFile(
			#initialfile=orig_name,
			filetypes=type_str,
			initialdir=my_dir))
	} else {
		name <- tclvalue(tkgetOpenFile(
			#initialfile=orig_name,
			filetypes=type_str,
			initialdir=my_dir))
	}

	if (nchar(name)>0) {
		# separate filename and path
		my_filepath = sub("/[^/]*$","",name)	
		name=sub("^.*/","",name)

		if(rtdf_flag) {
			#change_Rtdfs_dir(my_filepath)
			output_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")
			if (nchar(output_dir)<1) {
				output_dir <- paste(tclObj(Orig_dir),sep="",collapse=" ")
			}

			if(my_filepath != output_dir)  tclvalue(convparam_rtdf_dir) <- my_filepath
			else  tclvalue(convparam_rtdf_dir) <- ""
		} else {
			if(my_filepath != my_dir)  change_Output_dir(my_filepath)
		}
		if (out) {
			tclvalue(convparam_out_name) <- name
		} else {
			tclvalue(in_name) <- name
		}
	}
}


#----------------------------------------------------
# when radiobutton clicked, update in_file and out_file
# names...
convparam_type <- function() {

	my_type <- as.character(tclObj(convert_type))
	in_file <- paste(tclObj(in_name),sep="",collapse=" ")
	rtdf_dir_ <- paste(tclObj(convparam_rtdf_dir),sep="",collapse=" ")
	out_file <- paste(tclObj(convparam_out_name),sep="",collapse=" ")

	if (my_type=="r2t") {
		# if in_file is empty or ends in .txt or .csv
		if ( (in_file=="") || (regexpr(".txt$",in_file)>0) ||
				(regexpr(".csv$",in_file)>0) ) {
			in_file <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
			tclvalue(in_name) <- in_file
			if (in_file!="") {
				rtdf_dir_ <- paste(tclObj(Rtdf_dir),sep="",collapse=" ")
				if (rtdf_dir_ == "") {
					rtdf_dir_ <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
				}
				tclvalue(convparam_rtdf_dir) <- rtdf_dir_
			}
		}
		# if out_file doesn't end in .txt
		if (regexpr(".txt$",out_file)<1) {
			tclvalue(convparam_out_name) <- "parameters.txt"
		}
	} else if (my_type=="r2c") {
		# if in_file is empty or ends in .txt or .csv
		if ( (in_file=="") || (regexpr(".txt$",in_file)>0) ||
				(regexpr(".csv$",in_file)>0) ) {
			in_file <- paste(tclObj(Rtdf_name),sep="",collapse=" ")
			tclvalue(in_name) <- in_file
			if (in_file!="") {
				rtdf_dir_ <- paste(tclObj(Rtdf_dir),sep="",collapse=" ")
				if (rtdf_dir_ == "") {
					rtdf_dir_ <- paste(tclObj(Rtdfs_dir),sep="",collapse=" ")
				}
				tclvalue(convparam_rtdf_dir) <- rtdf_dir_
			}
		}
		# if out_file doesn't end in .csv
		if (regexpr(".csv$",out_file)<1) {
			tclvalue(convparam_out_name) <- "parameters.csv"
		}
	} else if (my_type=="t2r") {
		# if in_file ends in .rtdf or .csv
		if ( (regexpr(".rtdf$",in_file)>0) ||
				(regexpr(".csv$",in_file)>0) ) {
			tclvalue(in_name) <- ""
		}
		# if out_file doesn't end in .rtdf
		if (regexpr(".rtdf$",out_file)<1) {
			tclvalue(convparam_out_name) <- "parameters.rtdf"
		}
	} else if (my_type=="c2r") {
		# if in_file ends in .rtdf or .txt
		if ( (regexpr(".rtdf$",in_file)>0) ||
				(regexpr(".txt$",in_file)>0) ) {
			tclvalue(in_name) <- ""
		}
		# if out_file doesn't end in .rtdf
		if (regexpr(".rtdf$",out_file)<1) {
			tclvalue(convparam_out_name) <- "parameters.rtdf"
		}
	}
}


#----------------------------------------------------
run_ConvertParameters <-function(done=FALSE,...) {

	my_type <- as.character(tclObj(convert_type))
	in_file_ <- paste(tclObj(in_name),sep="",collapse=" ")
	out_file_ <- paste(tclObj(convparam_out_name),sep="",collapse=" ")
	rtdfs_dir <- paste(tclObj(convparam_rtdf_dir),sep="",collapse=" ")
	output_dir <- paste(tclObj(Output_dir),sep="",collapse=" ")

	if (my_type=="r2t" || my_type=="r2c") {
		output_dir_ = output_dir
		in_dir_ = rtdfs_dir
	} else {
		output_dir_ = rtdfs_dir
		in_dir_ = output_dir
		if (nchar(rtdfs_dir)>0) {
			if (nchar(in_dir_)<1) {
				in_dir_ <- paste(tclObj(Orig_dir),sep="",collapse=" ")
			}
		}
	}
	if (nchar(output_dir_)<1) {
		output_dir_ <- paste(tclObj(Output_dir),sep="",collapse=" ")
	}
	if (nchar(output_dir_)<1) {
		output_dir_ <- paste(tclObj(Orig_dir),sep="",collapse=" ")
	}
	setwd(output_dir)
	
	my_expr = substitute(
		ConvertParameters(in_file=in_file_,out_file=out_file_,
						  in_dir=in_dir_)
	)
	tkradar_logfile <- paste(tclObj(TkRadar_logfile),sep="",collapse=" ")
	tkradar_verbose <- as.integer(tclObj(TkRadar_verbose))
	my_command = sprintf("%s\n",deparse(my_expr))
	my_cmnd = "ConvertParameters(...)\n"
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
		convertparameters_win <- get("convertparameters_win",envir=.TkRadar.wins)
		tkdestroy(convertparameters_win)
	}
}



#-----------------------------------------------------
ConvertParametersGui <-function(...) {

	ConvertParametersGui_defaults()		# initialize variables...
	convertparameters_win <- tktoplevel()
	assign("convertparameters_win",convertparameters_win,envir=.TkRadar.wins)
	tkwm.title(convertparameters_win, "ConvertParameters")
	
	bottom_row <- tkframe(convertparameters_win)
	default_button <- tkbutton(bottom_row,
					text="DEFAULTS",
					#anchor="w",
					width=12,
					command=ConvertParametersGui_defaults)
	tkpack(default_button,side="right")

	ok_button <- tkbutton(bottom_row,
					text="RUN & QUIT",
					#anchor="w",
					width=12,
					command=function() run_ConvertParameters(done=TRUE))
	tkpack(ok_button,side="right")

	cancel_button <- tkbutton(bottom_row,
					text="QUIT",
					#anchor="w",
					width=12,
					command=function()tkdestroy(convertparameters_win))
	tkpack(cancel_button,side="right")

	apply_button <- tkbutton(bottom_row,
					text="RUN",
					#anchor="w",
					width=12,
					command=function() run_ConvertParameters(done=FALSE))
	tkpack(apply_button,side="right")
	tkpack(bottom_row,side="bottom",anchor="w")

	dir_entry_frame <- tkframe(convertparameters_win)
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


	type_frame <- tkframe(convertparameters_win)
	type_r2t <- tkradiobutton(type_frame,
						text="rtdf->txt",
						value="r2t",
						command=convparam_type,
						variable=convert_type)
	tkpack(type_r2t,side="left")
	type_r2c <- tkradiobutton(type_frame,
						text="rtdf->csv",
						value="r2c",
						command=convparam_type,
						variable=convert_type)
	tkpack(type_r2c,side="left")
	type_t2r <- tkradiobutton(type_frame,
						text="txt->rtdf",
						value="t2r",
						command=convparam_type,
						variable=convert_type)
	tkpack(type_t2r,side="left")
	type_c2r <- tkradiobutton(type_frame,
						text="csv->rtdf",
						value="c2r",
						command=convparam_type,
						variable=convert_type)
	tkpack(type_c2r,side="left")
	tkpack(type_frame,side="top",anchor="w",fill="x")

	in_dir_entry_frame <- tkframe(convertparameters_win)
	in_dir_entry_label <- tklabel(in_dir_entry_frame,
						width=10,
						text="rtdf_dir")
	tkpack(in_dir_entry_label,side="left")
	in_dir_entry <- tklabel(in_dir_entry_frame,
						width=50,
						relief="sunken",
						textvariable=convparam_rtdf_dir)
	tkpack(in_dir_entry,side="left",fill="x",expand=1)
	in_dir_browse <- tkbutton(in_dir_entry_frame,
						text="Browse",
						command=function() dir_browser(convparam_rtdf_dir))
	tkpack(in_dir_browse,side="right")
	tkpack(in_dir_entry_frame,side="top",anchor="w",fill="x")

	in_entry_frame <- tkframe(convertparameters_win)
	in_entry_label <- tklabel(in_entry_frame,
						width=10,
						text="in_file")
	tkpack(in_entry_label,side="left")
	in_entry <- tkentry(in_entry_frame,
						width=50,
						textvariable=in_name)
	tkpack(in_entry,side="left",fill="x",expand=1)
	in_browse <- tkbutton(in_entry_frame,
						text="Browse",
						command=function() convparam_browser(out=FALSE))
	tkpack(in_browse,side="right")
	tkpack(in_entry_frame,side="top",anchor="w",fill="x")

	out_entry_frame <- tkframe(convertparameters_win)
	out_entry_label <- tklabel(out_entry_frame,
						width=10,
						text="out_file")
	tkpack(out_entry_label,side="left")
	out_entry <- tkentry(out_entry_frame,
						width=20,
						textvariable=convparam_out_name)
	tkpack(out_entry,side="left",fill="x",expand=1)
	out_browse <- tkbutton(out_entry_frame,
						text="Browse",
						command=function() convparam_browser(out=TRUE))
	tkpack(out_browse,side="right")
	tkpack(out_entry_frame,side="top",anchor="w",fill="x")

}


