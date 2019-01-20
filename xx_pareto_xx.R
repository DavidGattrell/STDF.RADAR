# build pareto from FindFirstFails
#  - testlist based pareto instead of SBIN pareto


# building pareto from DeviceFrame...
#-------------------------------------
all_sbins = as.integer(DevicesFrame[,"soft_bin"])
sbin_summary = sort(table(all_sbins),decreasing=TRUE)
pct_sbin_summary = 100.0*sbin_summary/sum(sbin_summary)

# set bins to red except pass bins
cols = rep("red",length(pct_sbin_summary))
pass_bins = 1
pass_idxs = which(names(sbin_summary) %in% pass_bins)
cols[pass_idxs] = "green"

barplot(pct_sbin_summary,col=cols,ylim=c(0,100))


# stacked by site?


# building from SbinInfoFrame...
#---------------------------------
table_sbins = as.integer(SbinInfoFrame[,"sbin_cnt"])
sbin_nums = as.integer(SbinInfoFrame[,"sbin_num"])
names(table_sbins) = sbin_nums

sbin_summary = sort(table_sbins,decreasing=TRUE)
pct_sbin_summary = 100.0*sbin_summary/sum(sbin_summary)

# set bins to red except pass bins
cols = rep("red",length(pct_sbin_summary))
pass_bins = which(as.character(SbinInfoFrame[,"sbin_pf"])=="P")
pass_idxs = which(names(sbin_summary) %in% pass_bins)
cols[pass_idxs] = "green"

barplot(pct_sbin_summary,col=cols,ylim=c(0,100))



# building from multisite SiteSbinInfoFrame/SiteSbinCountMatrix/...
#------------------------------------------------------------------
table_sbins = as.integer(SbinInfoFrame[,"sbin_cnt"])
sort_idxs = sort(table_sbins,decreasing=TRUE,index.return=TRUE)

rownames(SiteSbinCountMatrix) = SiteSbinSiteVector

sbin_nums = as.integer(SiteSbinInfoFrame[,"sbin_num"])
colnames(SiteSbinCountMatrix) = sbin_nums

sorted_matrix = SiteSbinCountMatrix[,sort_idxs$ix]
sorted_matrix[which(!is.finite(sorted_matrix))] = 0
sorted_matrix = 100.0*sorted_matrix/sum(table_sbins)

barplot(sorted_matrix,ylim=c(0,100),
	xlab = "Soft Bin",ylab = "percent",
	legend.text = as.character(SiteSbinSiteVector))
# col=c("red","orange","yellow") for 3 site example
# border=NA when using colour is nicer

