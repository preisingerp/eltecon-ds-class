library(data.table)
library(ggplot2)
library(caret)
library(glmnet)

data <- fread("data/OnlineNewsPopularity_mod.csv")

summary(data$shares)

data[, ln_shares := log(shares)]

colSums(is.na(data))

var.names <- names(data)
x.words <- var.names[c(2:6,11)]
x.links <- var.names[c(7:8,28:30)]
x.dig <- var.names[9:10]
x.key <- var.names[c(12,19:27)]
x.channel <- var.names[13:18]
x.NLP <- var.names[31:51]

# Define test data

test_data = data[seq(1, nrow(data), 10), ]
train_data = data[!(seq(1, nrow(data), 10))]

# Models

M1 <- "constant"
M2 <- paste0(x.key, collapse = " + ")
M3 <- paste0(c(x.key, x.dig), collapese = " + ")
M4 <- paste0(c(x.key, x.dig, x.words), collapse = " + ")
M5 <- paste0(c(x.key, x.dig, x.words, x.NLP), collapse = " + ")
M6 <- paste0(c(x.key, x.dig, x.words, x.NLP, x.links), collapse = " + ")

models <- c(M1, M2, M3, M4, M5, M6)

# MSE

MSE <- function(y, pred) {
  mean((y - pred)**2)
}

mse_train <- list()
mse_test <- list()
for (model in models) {
  
  model_formula <- as.formula(paste0("ln_shares ~", get(model)))     
  model_fit <- lm(model_formula, data = data_train)
  
  colname <- paste0("pred",model)
  data_train <- data_train[, eval(colname):=predict(model_fit, newdata = data_train)]
  mse_train[[colname]] <-  MSE(data_train[, ln_shares], data_train[, get(colname)])
  data_test <- data_test[, eval(colname):=predict(model_fit, newdata = data_test)]
  mse_test[[colname]] <-  MSE(data_test[, ln_shares], data_test[, get(colname)])
}

