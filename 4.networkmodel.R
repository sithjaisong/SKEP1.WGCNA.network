###############################################################################
#'title         : Network modelon SKEP Phase I
#'date          : July, 2014
#'purpose       : Network Construction and Analysis 
#'writed by     : Sith Jaisong (s.jaisong@irri.org)
#'contact       : International Rice Research Institute
#'input         : data from the 3.analysis.WGCNA.R
#'output        : network model 
###############################################################################
cluster0 <- subset(cluster, color == 0) # Module grey
cluster1 <- subset(cluster, color == 1) # Module 
cluster2 <- subset(cluster, color == 2) # Module blue

data.cluster0 <- data[cluster0$vars] # data of module grey
data.cluster1 <- data[cluster1$vars] # data of module
data.cluster2 <- data[cluster2$vars] # data of module

softPower <- 4
adjacency0 <- adjacency(data.cluster0, power = softPower)
adjacency1 <- adjacency(data.cluster1, power = softPower)
adjacency2 <- adjacency(data.cluster2, power = softPower)

######## Cal TOM
TOM2 <- TOMsimilarityFromExpr(data.cluster2, power = 4)

TOM0 <- TOMsimilarity(adjacency0)
TOM1 <- TOMsimilarity(adjacency1)
TOM2 <- TOMsimilarity(adjacency2)

library("qgraph")

q0 <- qgraph(TOM0,layout = "spring")
q1 <- qgraph(TOM1,layout = "spring")
q2 <- qgraph(TOM2,layout = "spring")
,
             minimum=0.3,
             maximum = 0.9,
             vsize=3,
             cut = 0.3,
             vsize = 3,
             layout = "spring",
             overlay = F)
