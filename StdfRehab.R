# StdfRehab.R
#
# $Id: StdfRehab.R,v 1.2 2007/10/08 18:08:54 David Exp $
#
# R script to parse through stdf file and try to repair
# FTP damage.
#
# for Offshore#1, it looks like if byte is 10, it added an
# an extra byte of 13 prior to it, not too bad to undo
#
# for Offshore#2, it looks like 13's were changed to 10's,
#   a bit nastier to fix, leave for another day
#**********************************************************
StdfRehab <- function(stdf_name="",new_name="") {
    
    timestamp0 = proc.time()
    timestamp0 = timestamp0[3]  # start time

    if (stdf_name == "") {
	stdf_name <- readline("Enter the name of the STDF file to read: ")
    }

    if (new_name == "") {
	new_name <- readline("Enter the name of the new STDF file to write: ")
    }

    STDF <- gzfile(stdf_name,"rb")
    NEW <- file(new_name,"wb")


    # determine size of file... won't work if .gz
    #-----------------------------------------------------
    my_com = paste("ls -s ",stdf_name,sep="")
    my_str = system(my_com,intern=TRUE)
    blocks = as.integer(strsplit(my_str[1]," ")[[1]][1])
    in_bytes = blocks*1024


    # then suck it all in with a single readBin() command
    #-----------------------------------------------------
    my_bytes = readBin(STDF,raw(),n=in_bytes)
    end = length(my_bytes)
    ptr = 1
    close(STDF)


    # create output vector to write to
    #-----------------------------------------------------
    #new_bytes = raw()
    #length(new_bytes)<-in_bytes
    #ptr2=1

    bytes = 0
    bytes_fixed = 0

    byte_13s = 0
    byte_13_10s = 0
    byte_10s = 0


    # now find bytes==13 and bytes==10
    #-------------------------------------
    indices_13 = which(my_bytes==13)
    indices_next = indices_13+1
    indices_10 = which(my_bytes==10)
    indices_13_10s = intersect(indices_10,indices_next) - 1
   
    byte_13s = length(indices_13)
    byte_10s = length(indices_10)
    byte_13_10s = length(indices_13_10s)
    
    new_bytes = my_bytes[-(indices_13_10s)]
    bytes=length(new_bytes)


    
    writeBin(new_bytes,NEW)
    close(NEW)

    cat(sprintf("DONE: %d extraneous bytes removed, %d bytes remaining\n",byte_13_10s,bytes))

    cat(sprintf(" byte=13     %d times\n",byte_13s))
    cat(sprintf(" byte=13,10  %d times\n",byte_13_10s))
    cat(sprintf(" byte=10     %d times\n\n",byte_10s))

    timestamp9 = proc.time()
    timestamp9 = (timestamp9[3] - timestamp0)/60.0
    cat(sprintf("ran in %.2f minutes\n",timestamp9))
    
}
