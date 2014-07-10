#####-- Load Package --#####
library(WGCNA)
options(stringsAsFactors = FALSE)

######-- Step 1: Loading Data --#####

load("selected.data.RData")

data <- selected.data


#####-- Step 1.2 Clustering and d identication of outlier --#####

# Analyse the cluster analysis by using package flashClust what the differnt between clust and falshClust ???
sampleTree <- flashClust(dist(data), method = "average")


## Have the cluster file plot the cluster and decorate the plot with the main title and the  
plot(sampleTree, main = "Sample clustering to detect outliers", sub="", xlab="", cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)

yield.data <- subset(selected.data, select = yield)

yieldColors <- numbers2colors(yield.data, signed = F)

plotDendroAndColors(sampleTree, yieldColors, groupLabels = names(yield.data), main = "Sample dendrogram and yield heatmap")




net.inj.datsurvey <- blockwiseModules(data, 
                                      power = 6, 
                                      TOMType = "unsigned", 
                                      minModuleSize = 1, 
                                      reassignThreshold = 0, 
                                      mergeCutHeight = 0.25, 
                                      numericLabels = TRUE, 
                                      pamRespectsDendro = FALSE, 
                                      saveTOMs = TRUE, 
                                      saveTOMFileBase = "inj.datsurveyTOM", 
                                      verbose = 3)

##

mergedColors <- labels2colors(net.inj.datsurvey$colors) # How many module the data have

# Plot the dendrogram and the module colors underneath

plotDendroAndColors(net.inj.datsurvey$dendrograms[[1]], 
                    mergedColors[net.inj.datsurvey$blockGenes[[1]]], 
                    "Module colors", 
                    dendroLabels = FALSE, 
                    hang = 0.03, 
                    addGuide = TRUE, 
                    guideHang = 0.05)
powers = c(c(1:10), seq(from = 12, to=20, by=2))

# Call the network topology analysis function

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

# Plot the results:

sizeGrWindow(9, 5)

par(mfrow = c(1,2))

cex1 = 0.9

# Scale-free topology fit index as a function of the soft-thresholding power

plot(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2], xlab="Soft Threshold (power)",ylab="Scale Free Topology Model Fit,signed R^2",type="n", main = paste("Scale independence"))

text(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2], labels=powers,cex=cex1,col="red")

# this line corresponds to using an R^2 cut-off of h

abline(h=0.50,col="red")

# Mean connectivity as a function of the soft-thresholding power

plot(sft$fitIndices[,1], sft$fitIndices[,5], xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n", main = paste("Mean connectivity"))

text(sft$fitIndices[,1], sft$fitIndices[,5], labels=powers, cex=cex1,col="red")


##### Stpe 2.2 : One-step network construction and module detection --#####

net.inj.datsurvey <- blockwiseModules(data, 
                                      power = 1, 
                                      TOMType = "unsigned", 
                                      minModuleSize = 1, 
                                      reassignThreshold = 0, 
                                      mergeCutHeight = 0.25, 
                                      numericLabels = TRUE, 
                                      pamRespectsDendro = FALSE, 
                                      saveTOMs = TRUE, 
                                      saveTOMFileBase = "inj.datsurveyTOM", 
                                      verbose = 3)

##

mergedColors <- labels2colors(net.inj.datsurvey$colors) # How many module the data have

# Plot the dendrogram and the module colors underneath

plotDendroAndColors(net.inj.datsurvey$dendrograms[[1]], 
                    mergedColors[net.inj.datsurvey$blockGenes[[1]]], 
                    "Module colors", 
                    dendroLabels = FALSE, 
                    hang = 0.03, 
                    addGuide = TRUE, 
                    guideHang = 0.05)

#### Note: Module color = grey ####
####

moduleLabels <- net.inj.datsurvey$colors
moduleColors <- labels2colors(net.inj.datsurvey$colors)

# Define numbers of variables and samples
nGenes <- ncol(data)
nSamples <- nrow(data)
# Recalculate MEs with color labels
MEs0 <- moduleEigengenes(data, moduleColors)$eigengenes
MEs <- orderMEs(MEs0)

###
moduleVariablesCor <- cor(MEs, yield.data, use = "p")
moduleVariablesPvalue <- corPvalueStudent(moduleVariablesCor, nSamples)

###
sizeGrWindow(10,6)

# Will display correlations and their p-values

textMatrix = paste(signif(moduleVariablesCor, 2), "\n(",
                   signif(moduleVariablesPvalue, 1), ")", sep = "");

dim(textMatrix) = dim(moduleVariablesCor)

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
#yield = as.data.frame(datprodsit$Yield)

#names(yield) = "yield"

modNames = substring(names(MEs), 3)

VarModuleMembership <- as.data.frame(cor(data, MEs, use = "p"))

MMPvalue <- as.data.frame(corPvalueStudent(as.matrix(VarModuleMembership), nSamples))

names(VarModuleMembership) = paste("MM", modNames, sep="")

names(MMPvalue) = paste("p.MM", modNames, sep="")

VarYieldSignificance = as.data.frame(cor(data, yield.data, use = "p"))

GSPvalue <- as.data.frame(corPvalueStudent(as.matrix(VarYieldSignificance), nSamples))

names(VarYieldSignificance) = paste("GS.", names(yield), sep="")

names(GSPvalue) = paste("p.GS.", names(yield), sep="")

#####-- Step 3 : Intramodular analysis: identifying variables within Module and yields

module = "turquoise"
column = match(module, modNames)
moduleGenes = moduleColors==module
#sizeGrWindow(7, 7)
#par(mfrow = c(1,1))

verboseScatterplot(abs(VarModuleMembership[moduleGenes, column]),
                   abs(VarYieldSignificance[moduleGenes, 1]),
                   xlab = paste("Module Membership in", module, "module"), 
                   ylab = "Injuries profiles and production situation significance for yield",
                   main = paste("Module membership vs. PS IP significance\n"),
                   cex.main = 1.2, 
                   cex.lab = 1.2, 
                   cex.axis = 1.2, 
                   col = "red")
