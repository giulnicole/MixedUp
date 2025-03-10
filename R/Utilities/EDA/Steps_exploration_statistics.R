#' @title exploratory_data_analysis
#' @description
#' \code{} Steps for data exploration
#'
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @import car
#' @import nortest
#' @import corrplot
#'
#' @param data Data frame
#' 
#' 
#' @name exploratory_data_analysis
#
exploratory_data_analysis <- function(data, output_file = NULL) {
  # Initialize results lists
  results <- list()
  results$numeric_summary <- NULL
  results$normality_tests <- NULL
  results$categorical_summary <- NULL
  results$chi_square_tests <- NULL
  results$group_comparisons <- NULL
  results$correlation_matrices <- NULL
  
  # Check data structure
  cat("Data Structure:\n")
  str(data)
  cat("\n")
  
  # Convert character variables to factors
  data <- data %>% mutate(across(where(is.character), as.factor))
  cat("Character variables converted to factors\n\n")
  
  # Data summary
  cat("Data Summary:\n")
  summary(data)
  cat("\n")
  
  # Missing values analysis
  missing_values <- colSums(is.na(data))
  total_missing <- sum(missing_values)
  
  cat("Missing Values Analysis:\n")
  cat("Total missing values in dataset:", total_missing, "\n")
  
  if(total_missing > 0) {
    cat("Missing values by variable:\n")
    missing_percent <- round((missing_values/nrow(data))*100, 2)
    missing_df <- data.frame(
      Variable = names(missing_values),
      Missing_Count = missing_values,
      Missing_Percent = missing_percent
    )
    print(missing_df[missing_df$Missing_Count > 0, ])
  } else {
    cat("No missing values found in the dataset.\n")
  }
  cat("\n")
  
  # Separate numeric and categorical variables
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  categorical_vars <- names(data)[sapply(data, is.factor)]
  
  cat("Numeric variables:", paste(numeric_vars, collapse=", "), "\n")
  cat("Categorical variables:", paste(categorical_vars, collapse=", "), "\n\n")
  
  # Analyze numeric variables
  if (length(numeric_vars) > 0) {
    cat("Analysis of Numeric Variables:\n")
    cat("--------------------------\n\n")
    
    # Basic statistics for numeric variables (excluding missing values)
    num_stats <- data %>%
      select(all_of(numeric_vars)) %>%
      summarise(across(everything(), 
                       list(
                         n_valid = ~ sum(!is.na(.)),
                         n_missing = ~ sum(is.na(.)),
                         min = ~ min(., na.rm = TRUE),
                         q1 = ~ quantile(., 0.25, na.rm = TRUE),
                         median = ~ median(., na.rm = TRUE),
                         mean = ~ mean(., na.rm = TRUE),
                         q3 = ~ quantile(., 0.75, na.rm = TRUE),
                         max = ~ max(., na.rm = TRUE),
                         sd = ~ sd(., na.rm = TRUE),
                         iqr = ~ IQR(., na.rm = TRUE),
                         skew = ~ e1071::skewness(., na.rm = TRUE),
                         kurt = ~ e1071::kurtosis(., na.rm = TRUE)
                       ), 
                       .names = "{.col}_{.fn}"))
    
    # Transform to a cleaner format for results
    num_summary <- data.frame()
    for (var in numeric_vars) {
      var_cols <- grep(paste0("^", var, "_"), names(num_stats), value = TRUE)
      var_data <- num_stats[, var_cols]
      names(var_data) <- gsub(paste0("^", var, "_"), "", names(var_data))
      var_data <- data.frame(Variable = var, t(var_data))
      names(var_data)[2] <- "Value"
      num_summary <- rbind(num_summary, var_data)
    }
    
    # Reshape to wide format for easier reading
    num_summary_wide <- reshape(
      num_summary, 
      idvar = "Variable", 
      timevar = "row.names", 
      direction = "wide"
    )
    rownames(num_summary_wide) <- num_summary_wide$Variable
    num_summary_wide$Variable <- NULL
    colnames(num_summary_wide) <- gsub("Value\\.", "", colnames(num_summary_wide))
    
    # Store in results
    results$numeric_summary <- num_summary_wide
    
    print(num_summary_wide)
    cat("\n")
    
    # Normality tests for each numeric variable
    cat("Normality Tests:\n")
    normality_results <- data.frame(
      Variable = character(),
      Valid_Obs = integer(),
      Shapiro_W = numeric(),
      Shapiro_p = numeric(),
      AD_A = numeric(),
      AD_p = numeric(),
      KS_D = numeric(),
      KS_p = numeric(),
      Is_Normal = character(),
      Recommended_Test = character(),
      stringsAsFactors = FALSE
    )
    
    for (var in numeric_vars) {
      cat("\nVariable:", var, "\n")
      
      # Count valid observations
      valid_obs <- sum(!is.na(data[[var]]))
      unique_vals <- length(unique(na.omit(data[[var]])))
      
      cat("Valid observations:", valid_obs, "\n")
      cat("Unique values:", unique_vals, "\n")
      
      # Initialize test results
      shapiro_w <- NA
      shapiro_p <- NA
      ad_a <- NA
      ad_p <- NA
      ks_d <- NA
      ks_p <- NA
      is_normal <- "Unknown"
      recommended_test <- "Unknown"
      
      if (valid_obs > 3 && unique_vals > 5) {
        # Shapiro-Wilk Test
        if (valid_obs <= 5000) {  # Shapiro limited to 5000 observations
          tryCatch({
            shapiro_test <- shapiro.test(na.omit(data[[var]]))
            shapiro_w <- shapiro_test$statistic
            shapiro_p <- shapiro_test$p.value
            cat("Shapiro-Wilk Test: W =", round(shapiro_w, 4), 
                ", p-value =", round(shapiro_p, 4), "\n")
          }, error = function(e) {
            cat("Shapiro-Wilk Test: Error -", e$message, "\n")
          })
        } else {
          cat("Shapiro-Wilk Test: Skipped (too many observations)\n")
        }
        
        # Anderson-Darling Test
        tryCatch({
          ad_test <- ad.test(na.omit(data[[var]]))
          ad_a <- ad_test$statistic
          ad_p <- ad_test$p.value
          cat("Anderson-Darling Test: A =", round(ad_a, 4), 
              ", p-value =", round(ad_p, 4), "\n")
        }, error = function(e) {
          cat("Anderson-Darling Test: Error -", e$message, "\n")
        })
        
        # Kolmogorov-Smirnov Test
        tryCatch({
          ks_test <- ks.test(na.omit(data[[var]]), "pnorm", 
                             mean=mean(data[[var]], na.rm=TRUE), 
                             sd=sd(data[[var]], na.rm=TRUE))
          ks_d <- ks_test$statistic
          ks_p <- ks_test$p.value
          cat("Kolmogorov-Smirnov Test: D =", round(ks_d, 4), 
              ", p-value =", round(ks_p, 4), "\n")
        }, error = function(e) {
          cat("Kolmogorov-Smirnov Test: Error -", e$message, "\n")
        })
        
        # Determine normality based on test results (prefer Anderson-Darling)
        is_normal_ad <- !is.na(ad_p) && ad_p >= 0.05
        is_normal_shapiro <- !is.na(shapiro_p) && shapiro_p >= 0.05
        is_normal_ks <- !is.na(ks_p) && ks_p >= 0.05
        
        # Decision logic for normality
        if (!is.na(ad_p)) {
          is_normal <- ifelse(is_normal_ad, "Yes", "No")
        } else if (!is.na(shapiro_p)) {
          is_normal <- ifelse(is_normal_shapiro, "Yes", "No") 
        } else if (!is.na(ks_p)) {
          is_normal <- ifelse(is_normal_ks, "Yes", "No")
        }
        
        # Recommended statistical test based on normality
        if (is_normal == "Yes") {
          recommended_test <- "Parametric (t-test, ANOVA, Pearson correlation)"
        } else if (is_normal == "No") {
          recommended_test <- "Non-parametric (Mann-Whitney, Kruskal-Wallis, Spearman correlation)"
        }
        
        cat("Conclusion: Data", ifelse(is_normal == "Yes", "may", "likely doesn't"), 
            "follow normal distribution\n")
        cat("Recommended tests:", recommended_test, "\n")
      } else {
        cat("Skipping normality tests - too few valid or unique values\n")
        recommended_test <- "Non-parametric due to insufficient data"
      }
      
      # Add to results dataframe
      normality_results <- rbind(normality_results, data.frame(
        Variable = var,
        Valid_Obs = valid_obs,
        Shapiro_W = shapiro_w,
        Shapiro_p = shapiro_p,
        AD_A = ad_a,
        AD_p = ad_p,
        KS_D = ks_d,
        KS_p = ks_p,
        Is_Normal = is_normal,
        Recommended_Test = recommended_test,
        stringsAsFactors = FALSE
      ))
    }
    
    # Store in results
    results$normality_tests <- normality_results
  }
  
  # Analyze categorical variables
  if (length(categorical_vars) > 0) {
    cat("\nAnalysis of Categorical Variables:\n")
    cat("-------------------------------\n\n")
    
    categorical_summary <- data.frame()
    
    # Frequency tables for categorical variables
    for (var in categorical_vars) {
      cat("\nVariable:", var, "\n")
      # Count missing values
      n_missing <- sum(is.na(data[[var]]))
      n_valid <- nrow(data) - n_missing
      
      # Get frequency table excluding NA values
      freq_table <- table(data[[var]], useNA = "no")
      prop_table <- prop.table(freq_table) * 100
      
      result <- data.frame(
        Variable = var,
        Category = names(freq_table),
        Frequency = as.vector(freq_table),
        Percentage = as.vector(prop_table)
      )
      
      # Report missing values separately
      cat("Valid observations:", n_valid, "\n")
      if (n_missing > 0) {
        cat("Missing values:", n_missing, "(", round(n_missing/nrow(data)*100, 2), "%)\n")
        # Add missing values row
        result <- rbind(result, data.frame(
          Variable = var,
          Category = "Missing",
          Frequency = n_missing,
          Percentage = (n_missing/nrow(data))*100
        ))
      }
      
      categorical_summary <- rbind(categorical_summary, result)
      print(result)
    }
    
    # Store in results
    results$categorical_summary <- categorical_summary
    
    # Chi-square tests for independence between categorical variables
    if (length(categorical_vars) > 1) {
      cat("\nChi-square Tests for Independence:\n")
      cat("----------------------------------\n\n")
      
      chi_square_results <- data.frame(
        Variable1 = character(),
        Variable2 = character(),
        Complete_Cases = integer(),
        Test_Type = character(),
        Statistic = numeric(),
        DF = numeric(),
        P_Value = numeric(),
        Is_Dependent = character(),
        stringsAsFactors = FALSE
      )
      
      for (i in 1:(length(categorical_vars)-1)) {
        for (j in (i+1):length(categorical_vars)) {
          var1 <- categorical_vars[i]
          var2 <- categorical_vars[j]
          
          cat("Variables:", var1, "vs", var2, "\n")
          
          # Count complete cases for these two variables
          complete_cases <- sum(!is.na(data[[var1]]) & !is.na(data[[var2]]))
          missing_cases <- nrow(data) - complete_cases
          
          cat("Complete cases:", complete_cases, "\n")
          if (missing_cases > 0) {
            cat("Missing cases:", missing_cases, "(", round(missing_cases/nrow(data)*100, 2), "%)\n")
          }
          
          # Initialize test results
          test_type <- "Skipped"
          statistic <- NA
          df <- NA
          p_value <- NA
          is_dependent <- "Unknown"
          
          if (complete_cases >= 30) {  # Minimum sample size for chi-square
            # Create contingency table excluding NAs
            cont_table <- table(data[[var1]], data[[var2]])
            
            # Check if we have enough observations in each cell
            expected <- chisq.test(cont_table)$expected
            min_expected <- min(expected)
            
            if (min_expected >= 5) {
              # Chi-square test
              tryCatch({
                chi_test <- chisq.test(cont_table, simulate.p.value = FALSE)
                test_type <- "Chi-square Test"
                statistic <- chi_test$statistic
                df <- chi_test$parameter
                p_value <- chi_test$p.value
              }, error = function(e) {
                cat("Chi-square Test: Error -", e$message, "\n")
              })
            } else {
              # Fisher's exact test for small samples
              tryCatch({
                chi_test <- fisher.test(cont_table, simulate.p.value = TRUE, B = 2000)
                test_type <- "Fisher's Exact Test"
                p_value <- chi_test$p.value
              }, error = function(e) {
                cat("Fisher's Exact Test: Error -", e$message, "\n")
              })
            }
            
            # Display test results
            if (test_type != "Skipped") {
              if (test_type == "Chi-square Test") {
                cat(test_type, ": X² =", round(statistic, 4), 
                    ", df =", df, ", p-value =", round(p_value, 4), "\n")
              } else {
                cat(test_type, ": p-value =", round(p_value, 4), "\n")
              }
              
              # Determine dependency
              is_dependent <- ifelse(!is.na(p_value) && p_value < 0.05, "Yes", "No")
              
              # Interpretation
              cat("Conclusion: Variables are", ifelse(is_dependent == "Yes", 
                                                      "likely dependent", 
                                                      "not significantly dependent"), "\n\n")
            } else {
              cat("Skipping test - insufficient data for analysis\n\n")
            }
          } else {
            cat("Skipping test - insufficient complete cases\n\n")
          }
          
          # Add to results dataframe
          chi_square_results <- rbind(chi_square_results, data.frame(
            Variable1 = var1,
            Variable2 = var2,
            Complete_Cases = complete_cases,
            Test_Type = test_type,
            Statistic = statistic,
            DF = df,
            P_Value = p_value,
            Is_Dependent = is_dependent,
            stringsAsFactors = FALSE
          ))
        }
      }
      
      # Store in results
      results$chi_square_tests <- chi_square_results
    }
  }
  
  # Relationships between numeric and categorical variables
  if (length(numeric_vars) > 0 && length(categorical_vars) > 0) {
    cat("\nRelationships between Numeric and Categorical Variables:\n")
    cat("------------------------------------------------------\n\n")
    
    group_comparison_results <- data.frame(
      Numeric_Var = character(),
      Categorical_Var = character(),
      Complete_Cases = integer(),
      Test_Type = character(),
      Statistic = numeric(),
      DF = numeric(),
      P_Value = numeric(),
      Groups_Differ = character(),
      stringsAsFactors = FALSE
    )
    
    for (num_var in numeric_vars) {
      for (cat_var in categorical_vars) {
        cat("\nNumeric Variable:", num_var, ", Categorical Variable:", cat_var, "\n")
        
        # Count complete cases
        complete_cases <- sum(!is.na(data[[num_var]]) & !is.na(data[[cat_var]]))
        missing_cases <- nrow(data) - complete_cases
        
        cat("Complete cases:", complete_cases, "\n")
        if (missing_cases > 0) {
          cat("Missing cases:", missing_cases, "(", round(missing_cases/nrow(data)*100, 2), "%)\n")
        }
        
        # Initialize test results
        test_type <- "Skipped"
        statistic <- NA
        df <- NA
        p_value <- NA
        groups_differ <- "Unknown"
        
        if (complete_cases > 0) {
          # Group statistics
          group_stats <- data %>%
            filter(!is.na(!!sym(num_var)) & !is.na(!!sym(cat_var))) %>%
            group_by(across(all_of(cat_var))) %>%
            summarise(
              count = n(),
              mean = mean(!!sym(num_var), na.rm = TRUE),
              median = median(!!sym(num_var), na.rm = TRUE),
              sd = sd(!!sym(num_var), na.rm = TRUE),
              .groups = "drop"
            )
          print(group_stats)
          
          # Determine appropriate test based on normality and number of groups
          groups <- unique(na.omit(data[[cat_var]]))
          
          # Get normality result if available
          is_normal <- "Unknown"
          if (!is.null(results$normality_tests)) {
            norm_result <- results$normality_tests[results$normality_tests$Variable == num_var, "Is_Normal"]
            if (length(norm_result) > 0) {
              is_normal <- norm_result
            }
          }
          
          if (length(groups) >= 2 && complete_cases >= 10) {
            if (length(groups) == 2) {
              # Decision logic for two groups
              if (is_normal == "Yes" && all(group_stats$count >= 30)) {
                # t-test for normal data with large samples
                tryCatch({
                  t_test <- t.test(
                    formula = as.formula(paste(num_var, "~", cat_var)),
                    data = data,
                    var.equal = TRUE,
                    na.action = na.omit
                  )
                  test_type <- "t-test (equal variance)"
                  statistic <- t_test$statistic
                  df <- t_test$parameter
                  p_value <- t_test$p.value
                }, error = function(e) {
                  cat("t-test: Error -", e$message, "\n")
                })
              } else {
                # Mann-Whitney U test (non-parametric)
                tryCatch({
                  wilcox_test <- wilcox.test(
                    formula = as.formula(paste(num_var, "~", cat_var)),
                    data = data,
                    exact = FALSE,
                    na.action = na.omit
                  )
                  test_type <- "Mann-Whitney U Test"
                  statistic <- wilcox_test$statistic
                  p_value <- wilcox_test$p.value
                }, error = function(e) {
                  cat("Mann-Whitney U Test: Error -", e$message, "\n")
                })
              }
            } else if (length(groups) > 2) {
              # Decision logic for more than two groups
              if (is_normal == "Yes" && all(group_stats$count >= 30)) {
                # ANOVA for normal data with large samples
                tryCatch({
                  anova_test <- aov(
                    formula = as.formula(paste(num_var, "~", cat_var)),
                    data = data,
                    na.action = na.omit
                  )
                  anova_summary <- summary(anova_test)[[1]]
                  test_type <- "ANOVA"
                  statistic <- anova_summary$`F value`[1]
                  df <- paste(anova_summary$Df[1], anova_summary$Df[2], sep=",")
                  p_value <- anova_summary$`Pr(>F)`[1]
                }, error = function(e) {
                  cat("ANOVA: Error -", e$message, "\n")
                })
              } else {
                # Kruskal-Wallis test (non-parametric)
                tryCatch({
                  kw_test <- kruskal.test(
                    formula = as.formula(paste(num_var, "~", cat_var)),
                    data = data,
                    na.action = na.omit
                  )
                  test_type <- "Kruskal-Wallis Test"
                  statistic <- kw_test$statistic
                  df <- kw_test$parameter
                  p_value <- kw_test$p.value
                }, error = function(e) {
                  cat("Kruskal-Wallis Test: Error -", e$message, "\n")
                })
              }
            }
            
            # Display test results
            if (test_type != "Skipped") {
              stat_name <- switch(test_type,
                                  "t-test (equal variance)" = "t",
                                  "Mann-Whitney U Test" = "W",
                                  "ANOVA" = "F",
                                  "Kruskal-Wallis Test" = "chi-squared")
              
              cat(test_type, ": ", stat_name, " = ", round(statistic, 4), 
                  ifelse(!is.na(df), paste(", df =", df), ""),
                  ", p-value =", round(p_value, 4), "\n")
              
              # Determine if groups differ
              groups_differ <- ifelse(!is.na(p_value) && p_value < 0.05, "Yes", "No")
              
              # Interpretation
              cat("Conclusion: Groups", ifelse(groups_differ == "Yes", 
                                               "show significant differences", 
                                               "do not show significant differences"), "\n")
            } else {
              cat("Skipping test - insufficient data for analysis\n")
            }
          } else {
            cat("Skipping tests - insufficient groups or observations\n")
          }
        } else {
          cat("Skipping analysis - no complete cases available\n")
        }
        
        # Add to results dataframe
        group_comparison_results <- rbind(group_comparison_results, data.frame(
          Numeric_Var = num_var,
          Categorical_Var = cat_var,
          Complete_Cases = complete_cases,
          Test_Type = test_type,
          Statistic = statistic,
          DF = df,
          P_Value = p_value,
          Groups_Differ = groups_differ,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # Store in results
    results$group_comparisons <- group_comparison_results
  }
  
  # Correlation analysis for numeric variables
  if (length(numeric_vars) > 1) {
    cat("\nCorrelation Analysis for Numeric Variables:\n")
    cat("----------------------------------------\n\n")
    
    # Count complete cases for numeric variables
    complete_matrix <- data[numeric_vars]
    complete_cases_all <- sum(complete.cases(complete_matrix))
    missing_cases_all <- nrow(data) - complete_cases_all
    
    cat("Complete cases for all numeric variables:", complete_cases_all, "\n")
    if (missing_cases_all > 0) {
      cat("Cases with at least one missing value:", missing_cases_all, 
          "(", round(missing_cases_all/nrow(data)*100, 2), "%)\n\n")
    }
    
    # Store correlation matrices
    results$correlation_matrices <- list()
    
    # Pearson correlation (parametric)
    cat("Pearson Correlation Matrix (using pairwise complete observations):\n")
    pearson_cor <- cor(data[numeric_vars], use = "pairwise.complete.obs", method = "pearson")
    print(round(pearson_cor, 3))
    results$correlation_matrices$pearson <- pearson_cor
    
    # Spearman correlation (non-parametric)
    cat("\nSpearman Correlation Matrix (more appropriate for non-normal data):\n")
    spearman_cor <- cor(data[numeric_vars], use = "pairwise.complete.obs", method = "spearman")
    print(round(spearman_cor, 3))
    results$correlation_matrices$spearman <- spearman_cor
    
    # Pairwise completion information
    cat("\nPairwise complete observations for each variable pair:\n")
    pairwise_complete <- matrix(NA, nrow = length(numeric_vars), ncol = length(numeric_vars))
    rownames(pairwise_complete) <- numeric_vars
    colnames(pairwise_complete) <- numeric_vars
    
    for (i in 1:length(numeric_vars)) {
      for (j in 1:length(numeric_vars)) {
        pairwise_complete[i, j] <- sum(!is.na(data[[numeric_vars[i]]]) & !is.na(data[[numeric_vars[j]]]))
      }
    }
    print(pairwise_complete)
    results$correlation_matrices$pairwise_counts <- pairwise_complete
  }
  
  # Save results to CSV files if an output file is specified
  if (!is.null(output_file)) {
    base_name <- tools::file_path_sans_ext(output_file)
    
    # Save numeric summary
    if (!is.null(results$numeric_summary)) {
      write.csv(results$numeric_summary, paste0(base_name, "_numeric_summary.csv"))
    }
    
    # Save normality tests
    if (!is.null(results$normality_tests)) {
      write.csv(results$normality_tests, paste0(base_name, "_normality_tests.csv"))
    }
    
    # Save categorical summary
    if (!is.null(results$categorical_summary)) {
      write.csv(results$categorical_summary, paste0(base_name, "_categorical_summary.csv"))
    }
    
    # Save chi-square tests
    if (!is.null(results$chi_square_tests)) {
      write.csv(results$chi_square_tests, paste0(base_name, "_chi_square_tests.csv"))
    }
    
    # Save group comparisons
    if (!is.null(results$group_comparisons)) {
      write.csv(results$group_comparisons, paste0(base_name, "_group_comparisons.csv"))
    }
    
    # Save correlation matrices
    if (!is.null(results$correlation_matrices)) {
      if (!is.null(results$correlation_matrices$pearson)) {
        write.csv(results$correlation_matrices$pearson, paste0(base_name, "_pearson_correlation.csv"))
      }
      if (!is.null(results$correlation_matrices$spearman)) {
        write.csv(results$correlation_matrices$spearman, paste0(base_name, "_spearman_correlation.csv"))
      }
      if (!is.null(results$correlation_matrices$pairwise_counts)) {
        write.csv(results$correlation_matrices$pairwise_counts, paste0(base_name, "_pairwise_counts.csv"))
      }
    }
    
    cat("\nResults saved to CSV files with base name:", base_name, "\n")
  }
  
  # Return the modified dataframe with character columns converted to factors and the results
  return(list(data = data, results = results))
}


# Load data
data <- read.csv("data.csv")

# Run the analysis with CSV output
analysis <- exploratory_data_analysis(data, output_file = "eda_results.csv")

# Access the processed data
processed_data <- analysis$data

# Access the results tables
results <- analysis$results
numeric_stats <- results$numeric_summary
normality_results <- results$normality_tests
categorical_stats <- results$categorical_summary
chi_square_results <- results$chi_square_tests
group_comparison_results <- results$group_comparisons
correlation_matrices <- results$correlation_matrices


