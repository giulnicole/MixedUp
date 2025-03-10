
# df is the dataset from which we start

dt <- data.table(df)

result <- dt[classes %in% c("Group1", "Group2"),
              lapply(.SD, function(x) list(
                mean = mean(x, na.rm = TRUE),
                sd = sd(x, na.rm = TRUE),
                median = median(x, na.rm = TRUE),
                Q1 = quantile(x, 0.25, na.rm = TRUE),
                Q3 = quantile(x, 0.75, na.rm = TRUE))),
              .SDcols = var_names]

result <- do.call(rbind, result)
result <- as.data.frame(result)
result <- apply(result, 2, as.numeric)
colnames(result) <- c("Mean", "SD", "Median", "Q1", "Q3") 
