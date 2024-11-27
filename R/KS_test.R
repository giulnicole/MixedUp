#' @title KStestCpG
#' @description
#' \code{} internal function which helps in adjusting methylated matrix and unmethylated matrix when filtering the CpGs according to the already filtered coverage
#'
#' @importFrom dplyr bind_rows
#' @importFrom graphics legend
#' @importFrom stats ks.test
#' @importFrom stats ecdf
#'
#' @param origmat filtered matrix from the SummarizedExperiment object but without missing
#' @param masked_data.mat1 matrix which has been imputed
#' @param plots cumulative distribution of CpGs
#'
#' @name KStestCpG
#'
#' @return
#' a data frame with 2 elements per each iteration:
#'  \item{KS.statistics}{KS test statistics value per each CpG}
#'  \item{Pval}{P value per each test.}
#'
#'
#' @examples
#'
#'  \dontrun{
#'
#'  KS_test <- KStestCpG(origmat1, masked_data.mat1)
#'
#'  }
#'
#'
#' @noRd
KStestCpG <- function(origmat1, masked_data.mat1, plots=FALSE){
  
  
  KStest.results <- list()
  df <- data.frame()
  cdf1 <- NULL
  cdf2 <- NULL
  
  for (i in 1:dim(origmat1)[1]) {
    
    
    imp <- as.matrix(masked_data.mat1[i,])
    ori <- as.matrix(origmat1[i,])
    c <- ks.test(imp, ori, alternative = "two.sided",  simulate.p.value = TRUE, B = 5000)
    
    
    if (plots==T) {
      
      a <- ecdf(imp)
      b <- ecdf(ori)
      
      cdf1[[i]] <- a
      cdf2[[i]] <- b
      
      print(i)
      
      
      plot(a, verticals=TRUE, do.points=FALSE, col="green",
           main="Cumulative density functions")
      plot(b, verticals=TRUE, do.points=FALSE, col="black", add=TRUE)
      
      legend("topleft", legend=c("Simulated CDF", "Original CDF"),
             col=c("green", "black"), lty=1, cex=0.8)
      
    } # if plots
    
    
    pvalue<- c$p.value
    statistics <- c$statistic
    
    
    # Create a data frame for Accuracy
    data <- data.frame(
      KS.statistics = statistics,
      Pval = pvalue)
    
    KStest.results[[i]] <- data
    
    # Combine data with df
    df <- bind_rows(df, data)
    
  }  # for
  
  return(df)
  
  
}