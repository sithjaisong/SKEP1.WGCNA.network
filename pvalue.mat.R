###############################################################################
#'title         : the function for create the p value matrix
#'date          : July, 2014
#'purpose       : p value matrix for  
#'                format
#'writed by     : Sith Jaisong (s.jaisong@irri.org)
#'contact       : International Rice Research Institute
#'input         : import excel file from the shared files and delete the 
#'                samples with NA and set the class of varibles of data set in 
#'                the right class of varible
#'output        : data frame and RData 
###############################################################################

pval.mat <- function(mat, conf.level = 0.95, method = "pearson") {
        mat <- as.matrix(mat)
        n <- ncol(mat)
        p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
        diag(p.mat) <- 0
        for (i in 1:(n - 1)) {
                for (j in (i + 1):n) {
                        tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level, method = method)
                        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
                }
        }
        return(p.mat)
}
