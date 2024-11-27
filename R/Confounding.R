

#  #  #

#  PCA  #

#  #  #

# Calculating PCs for accounting for confounding

library(factoextra)
library(FactoMineR)

# Perform logistic regression for each principal component
# Extracting coordinates of PCs


# dataset is the input data on which we want to perform the PCA

result <- prcomp(dataset)

binary_trait <- variabili$centro
pca_models <- list()
pca_scores <- as.data.frame(result$x)

for (i in 1:ncol(pca_scores)) {
  formula <- as.formula(paste("binary_trait ~ PC", i, sep = ""))
  model <- glm(formula, data = pca_scores, family = binomial(link = "logit"))
  pca_models[[i]] <- summary(model)
}

p_values <- sapply(pca_models, function(model) model$coefficients[2, "Pr(>|z|)"])
coefficients<- sapply(pca_models, function(model) model$coefficients[2, "Estimate"])

# Combine results into a data frame for easy viewing
pca_results <- data.frame(PC = paste0("PC", 1:ncol(pca_scores)), Coefficient = coefficients, P_value = p_values)
print(pca_results)

explained_variance <- (result$sdev^2 / sum(result$sdev^2))*100

# Cumulative variance explained
cumulative_variance <- cumsum(explained_variance)

barplot(explained_variance,
        main = "Variance Explained by Each Principal Component",
        xlab = "Principal Components",
        ylab = "Proportion of Variance Explained",
        col = "lightblue")


