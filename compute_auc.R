library(randomForest)
library(nnet)
library(pROC)

estimate_ps_and_auc_lr <- function(data) {
  # Fit a logistic regression model for propensity score estimation
  model <- glm(treated ~ age + educ + ethnicity + married + nodegree + re74 + re75,
               family = binomial, data = data)

  # Obtain the predicted probabilities of the treatment
  predicted_probabilities <- predict(model, type = "response")

  # Calculate ROC and AUC for the logistic regression model
  roc_model <- roc(response = data$treated, predictor = predicted_probabilities)
  
  auc_model <- auc(roc_model)

  # Print the AUC for the logistic regression model
  print(paste("AUC for Logistic Regression Model:", auc_model))
  
  return(list(model = model, auc = auc_model, predicted_probabilities = predicted_probabilities))
}

estimate_ps_and_auc <- function(data) {
  # Fit a Random Forest model for propensity score estimation
  model <- randomForest(as.factor(treated) ~ age + educ + ethnicity + married + nodegree + re74 + re75,
                        data = data, ntree = 100)

  # Obtain the predicted probabilities of the treatment
  predicted_probabilities <- predict(model, type = "prob")[,2]  # assuming 'treated' is the second level in the factor

  # Calculate ROC and AUC for the Random Forest model
  roc_rf <- roc(data$treated, predicted_probabilities)
  auc_rf <- auc(roc_rf)

  # Print the AUC for the Random Forest model
  print(paste("AUC for Random Forest Model:", auc_rf))

  # Assuming auc_null is predefined or calculated earlier in the script
  # If not, you will need to calculate auc_null within this function or pass it as a parameter
  if (exists("auc_null")) {
    print(paste("AUC for Null Model:", auc_null))
  } else {
    warning("Variable 'auc_null' is not defined.")
  }
  
  return(list(auc_rf = auc_rf, predicted_probabilities = predicted_probabilities))
}

estimate_ps_and_auc_nn <- function(data) {
  # Fit a Neural Network model for propensity score estimation
  nn_model <- nnet::multinom(treated ~ age + educ + ethnicity + married + nodegree + re74 + re75 + dwincl + early_ra + sample,
                             data = data, trace = FALSE)

  # Obtain the predicted probabilities of the treatment
  predicted_probabilities_nn <- predict(nn_model, type = "probs")

  # Calculate ROC and AUC for the Neural Network model
  roc_nn <- roc(response = data$treated, predictor = predicted_probabilities_nn[,2])  # assuming 'treated' is the second level in the factor
  
  auc_nn <- auc(roc_nn)

  # Print the AUC for the Neural Network model
  print(paste("AUC for Neural Network Model:", auc_nn))
  
  return(list(model = nn_model, auc = auc_nn, predicted_probabilities = predicted_probabilities_nn))
}
