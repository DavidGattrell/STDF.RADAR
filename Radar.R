# - - - - - - - - - - - - - - - - - - - - - 
# Radar.R
# - - - - - - - - - - - - - - - - - - - - -
#  R - based
#  ATE
#  Data
#  Analysis
#  Resources
#
# $Id: Radar.R,v 1.27 2020/12/18 01:25:01 david Exp $
#
# This script loads the various RADAR command scripts until such
# time as it gets put into a proper R package.
#
# Copyright (C) 2006-2020 David Gattrell
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
#-----------------------------------------------
if(is.finite(match(".Radar",search())))  detach(.Radar)
if(exists(".Radar")) rm(.Radar) 
.Radar <- new.env()

sys.source("Convert9470CSV.R",envir=.Radar)
sys.source("ConvertA5xx.R",envir=.Radar)
sys.source("ConvertCsv.R",envir=.Radar)
sys.source("ConvertEagleCSV.R",envir=.Radar)
sys.source("ConvertEDL.R",envir=.Radar)
sys.source("ConvertETS.R",envir=.Radar)
sys.source("ConvertFrugal.R",envir=.Radar)
sys.source("ConvertHP9490.R",envir=.Radar)
sys.source("ConvertJ9.R",envir=.Radar)
sys.source("ConvertKDF.R",envir=.Radar)
sys.source("ConvertStdf.R",envir=.Radar)
sys.source("ExpandMPRs.R",envir=.Radar)
sys.source("PlotRtdf.R",envir=.Radar)	
sys.source("PlotRtdf_2.R",envir=.Radar)	# alternate version
#sys.source("JustBin1s.R",envir=.Radar)	# obsoleted, use FilterByBinning
sys.source("ConvertParameters.R",envir=.Radar)
sys.source("WaferMap.R",envir=.Radar)
sys.source("MergeNewTests.R",envir=.Radar)
sys.source("MergeRtdf.R",envir=.Radar)
sys.source("ShrinkRetests.R",envir=.Radar)
sys.source("LoadRtdf.R",envir=.Radar)
sys.source("SaveRtdf.R",envir=.Radar)
#sys.source("FindDevices.R",envir=.Radar)	# obsoleted, use FilterByResult
sys.source("RemoveAtXY.R",envir=.Radar)
#sys.source("RemoveDevicesAtIndices.R",envir=.Radar) # obsoleted, use FilterByIndices
#sys.source("RemoveDevices.R",envir=.Radar)	# obsoleted, use FilterByResult
#sys.source("RemoveResults.R",envir=.Radar)	# obsoleted, use FilterByResult
#sys.source("FilterDevices.R",envir=.Radar)	# obsoleted, use FilterByResult
sys.source("FilterByResult.R",envir=.Radar)
sys.source("FilterByBinning.R",envir=.Radar)
sys.source("FilterByIndices.R",envir=.Radar)
sys.source("FindFirstFails.R",envir=.Radar)
sys.source("Fingerprint.R",envir=.Radar)
sys.source("RobustFilter.R",envir=.Radar)
sys.source("Rtdf2Deducer.R",envir=.Radar)
sys.source("SplitConditions.R",envir=.Radar)
sys.source("SplitSites.R",envir=.Radar)
sys.source("SplitWafers.R",envir=.Radar)
sys.source("PlotTestvsTest.R",envir=.Radar)
sys.source("PlotVsRun.R",envir=.Radar)
sys.source("ProbeVsReprobe.R",envir=.Radar)
sys.source("RtdfTransform.R",envir=.Radar)
sys.source("ReplaceTestnames.R",envir=.Radar)
sys.source("ControlCharts.R",envir=.Radar)
sys.source("No_Vlo_Vhi.R",envir=.Radar)		# sort of custom...
sys.source("AsciiWaferMap.R",envir=.Radar)
sys.source("SplitBySubstr.R",envir=.Radar)
sys.source("XYWid2Partid.R",envir=.Radar)
sys.source("StackedWaferMap.R",envir=.Radar)
sys.source("NonGating2Gating.R",envir=.Radar)

# source("Pareto.R")		# on the to-do list...
# source("StdfRehab.R")		# on the to-do list...


sys.source("TkRadar.R",envir=.Radar)


attach(.Radar)



