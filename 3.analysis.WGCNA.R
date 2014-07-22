###############################################################################
#'title         : Network Analysis of SKEP Phase I
#'date          : July, 2014
#'purpose       : Network Construction and Analysis 
#'writed by     : Sith Jaisong (s.jaisong@irri.org)
#'contact       : International Rice Research Institute
#'input         : data from the 2.cleaning the data
#'output        : data frame and RData 
###############################################################################
#Load data saved from the 2.cleaning the data
lnames <- load("selected.data.RData")
lnames
#####-- Load Package --#####
library(WGCNA)
options(stringsAsFactors = FALSE) # data frame created after executing that line will not auto-convert to factors
######-- Step 1: Loading Data --#####

data <- selected.data

#####-- Step 1.2 Clustering and d identication of outlier --#####

# Analyse the cluster analysis by using package flashClust what the differnt between clust and falshClust ???
clustaltree <- flashClust(dist(data), method = "average")

## Check the outline with cluter analysis  

plot(clustaltree, 
     main = "Clustering to detect outliers",
     sub="",
     xlab="",
     cex.lab = 1.5,
     cex.axis = 1.5,
     cex.main = 2
     )

### combine the cluster of production situation, insect injuires and disease 
### with the prodcution (yield) 

yield.data <- subset(selected.data, select = yield)

yieldColors <- numbers2colors(yield.data, signed = F)

plotDendroAndColors(clustaltree,
                    yieldColors,
                    groupLabels = names(yield.data),
                    main = "Dendrogram of PS and IP with yield heatmap"
                    )


#####----- Network Construction and Analysis-----#####
# This package is used for weighted network construction
# Call the network topology analysis function
###############################################################################
# WGCNA provide the automatic network construction 1 step                    
# Constructing a weighted gene network entails the choice of the soft thresholding 
# power β to which co-expression or co occureance 
# similarity is raised to calculate adjacency 
# Soft Thresold is the value to cutoff the correlation coeffienct and  #
# reduce spares correaltion 
# function pickSoftThreshold that performs the analysis of network topology and aids the
# user in choosing a proper soft-thresholding power. 
###############################################################################
sft <- pickSoftThreshold(t(data), dataIsExpr = TRUE,
                         RsquaredCut = 0.85, 
                         powerVector = c(seq(1, 10, by = 1), seq(12, 20, by = 2)), 
                         removeFirst = FALSE, nBreaks = 10, blockSize = NULL, 
                         corFnc = cor, 
                         corOptions = list(use = 'p'), 
                         networkType = "unsigned",
                         moreNetworkConcepts = FALSE,
                         verbose = 0, 
                         indent = 0)

powers = c(c(1:10), seq(from = 12, to=20, by=2))

#Plot the result
sizeGrWindow(9,5)
par(mfrow = c(1,2))
cex1 = 0.9

# Scale-Free topology fit index as a function pf tje soft-thresholding power

plot(sft$fitIndices[,1],
     sft$fitIndices[,2],
     xlab="Soft Threshold (power)",
     ylab="Scale Free Topology Model Fit,signed R^2",
     type="n",
     main = paste("Scale independence"))

text(sft$fitIndices[,1],
     sft$fitIndices[,2],
     labels=powers,cex=cex1,col="red")

# this line corresponds to using an R^2 cut-off of h
# Mean connectivity as a function of the soft-thresholding power

plot(sft$fitIndices[,1], 
     sft$fitIndices[,5],
     xlab="Soft Threshold (power)",
     ylab="Mean Connectivity",
     type="n",
     main = paste("Mean connectivity"))

text(sft$fitIndices[,1],
     sft$fitIndices[,5],
     labels=powers,
     cex=cex1,
     col="red")

######-----One-step network construction and module detection-----#####
net.data <- blockwiseModules(data, 
                                      power = 4, 
                                      TOMType = "unsigned", 
                                      minModuleSize = 1, 
                                      reassignThreshold = 0, 
                                      mergeCutHeight = 0.25, 
                                      numericLabels = TRUE, 
                                      pamRespectsDendro = FALSE, 
                                      saveTOMs = TRUE, 
                                      saveTOMFileBase = "data.surveyTOM", 
                                      verbose = 3)

##

mergedColors <- labels2colors(net.data$colors) # How many module the data have

# Plot the dendrogram and the module colors underneath

plotDendroAndColors(net.data$dendrograms[[1]], 
                    mergedColors[net.data$blockGenes[[1]]], 
                    "Module colors", 
                    dendroLabels = FALSE, 
                    hang = 0.03, 
                    addGuide = TRUE, 
                    guideHang = 0.05)


moduleLabels <- net.data$colors # colors represent module
moduleColors <- labels2colors(net.data$colors)

# Define numbers of variables and samples
nVars <- ncol(data)
nFields <- nrow(data)
# Recalculate MEs with color labels
MEs0 <- moduleEigengenes(data, moduleColors)$eigengenes
MEs <- orderMEs(MEs0)

###
moduleVariablesCor <- cor(MEs, yield.data, use = "p")
moduleVariablesPvalue <- corPvalueStudent(moduleVariablesCor, nFields)

###
sizeGrWindow(10,6)

# Will display correlations and their p-values

textMatrix <- paste(signif(moduleVariablesCor, 2), "\n(",
                   signif(moduleVariablesPvalue, 1), ")", sep = "");

dim(textMatrix) <- dim(moduleVariablesCor)

par(mar = c(6, 8.5, 3, 3))
# Display the correlation values within a heatmap plot

labeledHeatmap(Matrix = moduleVariablesCor, 
               xLabels = names(yield.data), 
               yLabels = names(MEs), 
               ySymbols = names(MEs),
               colorLabels = NULL,  
               colors = NULL, 
               textMatrix = textMatrix, 
               setStdMargins = FALSE, 
               cex.text = NULL,  
               main = paste("Module-trait relationships"))

#### 
modNames <- substring(names(MEs), 3)

VarModuleMembership <- as.data.frame(cor(data, MEs, use = "p"))

MMPvalue <- as.data.frame(corPvalueStudent(as.matrix(VarModuleMembership), nFields))

names(VarModuleMembership) <- paste("MM", modNames, sep="")

names(MMPvalue) <- paste("p.MM", modNames, sep="")

VarYieldSignificance <- as.data.frame(cor(data, yield.data, use = "p"))

GSPvalue <- as.data.frame(corPvalueStudent(as.matrix(VarYieldSignificance), nFields))

names(VarYieldSignificance) <- paste("GS.", names(yield), sep="")

names(GSPvalue) <- paste("p.GS.", names(yield), sep="")

#####-- Step 3 : Intramodular analysis: identifying variables within Module and yields

module <- "blue" # grey, yellow, blue, brown turquoise
column <- match(module, modNames)

moduleGenes <- moduleColors == module

verboseScatterplot(abs(VarModuleMembership[moduleGenes, column]),
                   abs(VarYieldSignificance[moduleGenes, 1]),
                   xlab = paste("Module Membership in", module, "module"), 
                   ylab = "Injuries profiles and production situation significance for yield",
                   main = paste("Module membership vs. PS IP significance\n"),
                   cex.main = 1.2, 
                   cex.lab = 1.2, 
                   cex.axis = 1.2, 
                   col = "red")
#eos
cor.mat <- cor(data)
Sur.Net.mat <- TOMsimilarityFromExpr(datExpr = data,
                                 corType = "p")
