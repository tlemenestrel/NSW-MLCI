library(boot)
library(MatchIt)
library(randomForest)
library(nnet)
library(optmatch)
library(ggplot2)

calculate_ace <- function(treated_outcomes, control_outcomes) {
    # Extracting numerical values from the columns if they are data frames or tibbles
    if (is.data.frame(treated_outcomes)) {
        treated_outcomes <- treated_outcomes[[1]]
    }
    if (is.data.frame(control_outcomes)) {
        control_outcomes <- control_outcomes[[1]]
    }

    # Calculate and return the ACE
    mean(treated_outcomes, na.rm = TRUE) - mean(control_outcomes, na.rm = TRUE)
}

bootstrap_ace_ci <- function(data, treated_var, outcome_var, nboot = 1, conf_level = 0.99) {
  # Bootstrap function
  boot_func <- function(data, indices) {
    resampled_data <- data[indices, ]
    treated_outcomes <- resampled_data[resampled_data[[treated_var]] == 1, outcome_var, drop = FALSE]
    control_outcomes <- resampled_data[resampled_data[[treated_var]] == 0, outcome_var, drop = FALSE]
    calculate_ace(treated_outcomes, control_outcomes)
  }
  
  # Perform bootstrap
  boot_results <- boot::boot(data, boot_func, R = nboot)

  # Calculate confidence intervals
  boot_ci <- boot::boot.ci(boot_results, type = "perc", conf = conf_level)

    # Extract CI
  ci_lower <- boot_ci$percent[4]
  ci_upper <- boot_ci$percent[5]

  # Return ACE estimate and CI
  list(ace = mean(boot_results$t), ci_lower = ci_lower, ci_upper = ci_upper)
}

# Function to perform matching and plot covariate balance, and print sum of covariate balance
plot_covariate_balance <- function(data, treatment_var, outcome_var, propensity_score_var, covariates, method) {
  
  distance_matrix <- match_on(as.formula(paste(treatment_var, "~", propensity_score_var)), data = data, method = "mahalanobis")
  pairwise_matching <- pairmatch(distance_matrix, data = data)
  formula_pre <- as.formula(paste(treatment_var, "~", paste(covariates, collapse = " + ")))
  
  pre_balance <- xBalance(formula_pre, data = data, report = "std.diffs")
  post_balance <- xBalance(formula_pre, data = data, strata = pairwise_matching, report = "std.diffs")
  
  sum_pre_balance  <- sum(abs(pre_balance$results[, 1, ]))
  sum_post_balance <- sum(abs(post_balance$results[, 1, ]))
  
  cat("Sum of Standardized Differences Before Matching:", sum_pre_balance, "\n")
  cat("Sum of Standardized Differences After Matching:", sum_post_balance, "\n")

  pre_balance_df <- data.frame(vars =dimnames(pre_balance$results)[1]$vars, std.diff = pre_balance$results, Period = "Before")
  post_balance_df <- data.frame(vars = dimnames(post_balance$results)[1]$vars, std.diff = post_balance$results, Period = "After")
  names(post_balance_df) <- c("vars", "std.diff.std.diff.unstrat", "std.diff.p.unstrat", "Period")
  
  balance_data <- rbind(pre_balance_df, post_balance_df)

  # Plotting
  p <- ggplot(balance_data, aes(x = vars, y = std.diff.std.diff.unstrat, fill = Period)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
    geom_text(aes(label = round(std.diff.std.diff.unstrat, 2)), 
              position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +
    theme_minimal() +
    labs(x = "Covariate", y = "Standardized Difference", 
         title = "Covariate Balance Before and After Matching") +
    scale_fill_brewer(palette = "Set1") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
  
  # Save the plot
  ggsave(paste0("/Users/thomaslemenestrel/Documents/university/stanford/y2/stats209/covariate_balance_plot_", method, ".png"), plot = p, width = 10, height = 8, dpi = 300)
  # After matching, get the matched data
  matched_data <- data.frame(data, match = as.character(pairwise_matching))
  
  # Calculate ACE and CI for pre-matching
  ace_ci_pre <- bootstrap_ace_ci(data, treatment_var, outcome_var, nboot=1000000)
  
  # Calculate ACE and CI for post-matching
  ace_ci_post <- bootstrap_ace_ci(matched_data, treatment_var, outcome_var, nboot=1000000)

  # Output the results to a table
  results_table <- data.frame(
    Model = c("Pre-Matching", "Post-Matching"),
    ACE = c(ace_ci_pre$ace, ace_ci_post$ace),
    CI_Lower = c(ace_ci_pre$ci_lower, ace_ci_post$ci_lower),
    CI_Upper = c(ace_ci_pre$ci_upper, ace_ci_post$ci_upper),
    Range    = c(ace_ci_post$ci_upper - ace_ci_post$ci_lower, ace_ci_pre$ci_upper - ace_ci_pre$ci_lower),
    Covariate_Balance = c(sum_pre_balance, sum_post_balance)
  )
  
  # Return the results table
  return(results_table)
}
