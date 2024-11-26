#' @title TotalBurden
#' @description
#' \code{}TotalBurden calculates the coefficient of cumulative burden on rare variants per each subject (glm model); cases vs controls
#'  @author DavideFer98
#' 
#'
#'
#' @param dataset rows subjects, columns (from the 2nd on) total burden of genes; must contain the ID of subject as first column; the name of the genes in all the other columns with the sum of rare variants per gene in each subjects
#' @param samplesheet must contain one column with sample IDs; one with: Cases/Controls (categorical); one with Sex (categorical)
#' 

TotalBurden <- function(dataset, samplesheet) {
  
  library(tidyr)
  
  cat("Dataset must contain the ID of subject as first column; the name of the genes in all the other columns with the sum of rare variants per gene in each subjects")
  cat("Samplesheet must contain one column with sample IDs; one with: Cases/Controls (categorical); one with Sex (categorical)")  
  colnames(dataset)[1] <- "Gene.refGene"
  colnames(samplesheet)[1] <- "SAMPLE"
  burden <- dataset %>% separate(Gene.refGene, into = c("SAMPLE","ELSE"), sep="_")
  burden <- burden[, -2]
  burden$SAMPLE <- toupper(burden$SAMPLE)
  samplesheet$SAMPLE <- toupper(samplesheet$SAMPLE)
  
  data <-merge(burden, samplesheet, by= "SAMPLE")
  
  nrows <- nrow(data)
  ncols <- ncol(data)
  
  data$Sum_total <- rowSums(data[2:(ncols-2)])
  data$Sample_Group_class <- as.factor(data$Sample_Group)
  data$Sex <- as.factor(data$Sex)
  data$Sample_Group <- NULL
  
  plot_sum <- plot(data$Sum_total~ data$Sample_Group_class)
  
  data$Sample_Group_class <- relevel(data$Sample_Group_class, ref = "Controls")
  model<-glm(data$Sample_Group_class ~ data$Sum_total + data$Sex,family = "binomial")
  summary_mod <- summary(model)
  
  res <- list(Model_tot_burden=summary_mod)
  print(plot_sum)
  
  return(res)
  
  
  
}
