library(dplyr)
library(haven)
library(RItools)

read_and_prepare_dataset <- function() {

	data <- read_dta('data/nsw.dta')
	data <- na.omit(data, cols = "treated")
	data$ethnicity <- ifelse(data$black == 1, 2,
	                          ifelse(data$hisp == 1, 3, 1))
	data$black <- NULL
	data$hisp <- NULL

	return (data)

}

print_balance <- function (data) {

	balance <- xBalance(treated ~ ., data=data, report="all")
	print(balance)
}