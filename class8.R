library(data.table)
library(purrr)

n <- 50

set.seed(1337)
x <- runif(n, min = 0, max = 1)
e <- rnorm(n, 0, 0.1)
y <- sin(2*pi*x) + e

data <- data.table(x = x, y = y)

fold <- 5

cv_split <- split(sample(1:n), 1:fold)

formula <- as.formula("y ~ x + I(x^2)")
cv_errors <- imap(cv_split, ~{
  model <- lm(formula = formula, data = data[-.x,])
  p <- predict(model, data_new = data[.x,])
  mean((p - data[.x, y])^2)
})

mean(unlist(cv_errors))

#######################################

library(caret)
train_control <- trainControl(method = "cv", number = 5)
set.seed(1337)
model <- train(form = formula, data = data, trControl = train_control, method = "lm")
model

model$resample
model$results$RMSE**2
