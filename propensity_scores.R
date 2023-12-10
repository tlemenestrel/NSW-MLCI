
library(nnet)
library(randomForest)

add_glm_prop_scores <- function (data) {

	propensity_model <- glm(
		treated ~ age + educ + ethnicity + married + nodegree + re74 + re75,
		family = binomial, 
		data = data)

	data$lo_propensity_score <- predict(propensity_model, type = "response")

	return(data)

}

add_rf_prop_scores <- function (data) {

	rf_model <- randomForest(
		as.factor(treated) ~ age + educ + ethnicity + married + nodegree + re74 + re75,
		data = data, 
		ntree = 100)

	data$rf_propensity_score <- predict(rf_model, newdata = data, type = "prob")[, 2]

	return (data)

}

add_nn_prop_scores <- function (data) {

	nn_model <- nnet::multinom(
		treated ~ age + educ + ethnicity + married + nodegree + re74 + re75,
		data = data, 
		trace = FALSE)

	data$nn_propensity_score <- predict(nn_model, newdata = data, type = "probs")

	return (data)

}
