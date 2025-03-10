
# Perform PCA with imputation
imputed_data <- imputePCA(df, ncp = ncp)$completeObs
pca_result <- prcomp(imputed_data, scale. = TRUE)


fviz_pca_ind(pca_result, col.ind = df$classes, 
             # gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = F)



fviz_contrib(pca_result, choice = "var", axes = 1, top = 10)
fviz_contrib(pca_result, choice = "var", axes = 2, top = 10)
fviz_contrib(pca_result, choice = "var", axes = 3, top = 10)
