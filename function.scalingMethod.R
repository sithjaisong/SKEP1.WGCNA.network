##' @title scalingMethods
##' @param data a data matrix ([data.frame object] row: molecules,
##' col: samples or replicates)
##' @param methods the chosen methods.
##' @return the resulting data frame (or scaled data matrix)
##' @examples
##' scalingMethods(iris[,1:4], "level")
##' @author Atsushi Fukushima
##' @export
scalingMethods <- function(data, 
                           methods = c("auto", "range", "pareto", "vast", "level", "power")) {
        methods <- match.arg(methods)
        if(ncol(data) > 1) {
                switch(methods,
                       auto = {
                               res <- apply(data, 1,
                                            function(x) (x-mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE))
                               return(data.frame(t(res), check.names=FALSE))
                       },
                       range = {
                               res <- apply(data, 1,
                                            function(x)
                                                    (x - mean(x, na.rm=TRUE))/(range(x)[2]-range(x)[1]))
                               return(data.frame(t(res), check.names=FALSE))
                       },
                       pareto = {
                               res <- apply(data, 1,
                                            function(x)
                                                    (x - mean(x, na.rm=TRUE))/sqrt(sd(x, na.rm=TRUE)))
                               return(data.frame(t(res), check.names=FALSE))
                       },
                       vast = {
                               res <- apply(data, 1,
                                            function(x) mean(x, na.rm=TRUE) *
                                                    (x - mean(x, na.rm=TRUE)) / (sd(x, na.rm=TRUE)**2))
                               return(data.frame(t(res), check.names=FALSE))
                       },
                       level = {
                               res <- apply(data, 1,
                                            function(x) (x - mean(x, na.rm=TRUE)) / mean(x, na.rm=TRUE))
                               return(data.frame(t(res), check.names=FALSE))
                       },
                       power = {
                               res <- apply(data, 1,
                                            function(x) sqrt(x) - mean(sqrt(x)))
                               return(data.frame(t(res), check.names=FALSE))
                       })
        }
}