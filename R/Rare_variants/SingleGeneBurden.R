#' @title SingleGeneBurden
#' @description
#' \code{}SingleGeneBurden calculates the coefficient of gene's level burden on rare variants per each subject (glm model); cases vs controls adjusted for explicit covaiates
#'
#'  @author DavideFer98
#'  @author giulnicole
#'
#'
#' @param y Sample_group
#' @param data.genes must contain the output from burden test for rare variants 
#' @param data.covar must contain the covariates thathas to be added to test gene's level burden 
#' @param sign significance level
    
    
SingleGeneBurden<- function(y, data.genes, data.covar, sign=0.05){
  

   df <- data.frame(
    Gene = character(),       
    Effect = numeric(),      
    Pvalue = numeric())  
  
  
    nrows <- nrow(data.genes)
    ncols <- ncol(data.genes)


   for (i in 1:ncol(data.genes)) {
  
  
   gene<- colnames(data.genes)[i]
   vars <- as.data.frame(cbind(y, data.genes[, i], data.covar))
   colnames(vars)[2] <- gene
  
   model <- glm(y ~ ., data = vars, family = binomial)
  

   model_summary <- summary(model)
  
   effect <- model_summary$coefficients[2, 1]
   pvalue <- model_summary$coefficients[2, 4]
   
   
   
   # significance condition
   if (pvalue < sign) {
   
     df <- rbind(df, data.frame(
         Gene = gene,
         Effect = effect,
         Pvalue = pvalue))
       
            }
     
    }
    
    
    return(df)
    
          

}


