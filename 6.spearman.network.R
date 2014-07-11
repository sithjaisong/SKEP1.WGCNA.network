###############################################################################
#'title         : Building the Spearman's correlation based Network 
#'date          : July, 2014
#'purpose       : Visual the survey data to network model
#'writed by     : Sith Jaisong (s.jaisong@irri.org)
#'contact       : International Rice Research Institute
#'input         : data frame named ds from the previous step (2.cleaning the data.R)
#'output        : Spearman's correlation based network 
###############################################################################

library(caret) # find the high correlation
library(corrplot) # visualize the correlation plot
library(fdrtool) # Local False Discovery Rates and Higher Criticism
library(qgraph) # Network construct

######----Correlation matrix-----######
# creat the correlation matrix

cor.test <- cor(selected.data, use = "everything", method = "spearman")

corrplot(cor.test)
corrplot(cor.test, p.mat = pvalu, sig.level = 0.01)

# set the correlation threshold from the estimation from FDR package 

highlyCor <- findCorrelation(cor.test, cutoff = 0.6) # cutoff at fdr$param[1]

dat.filted <- selected.data[,-highlyCor]

cor.filed <- cor(dat.filted, use = "everything", method = "spearman")

# Plot corrplot
corrplot(cor.filed, order = "hclust", addrect = 5)

###
qgraph(cor.filed, 
       layout = "spring",
       title = "Spearman's correlation based Network")
