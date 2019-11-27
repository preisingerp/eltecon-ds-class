library(data.table)

data <- fread("data/returning_buyers.csv")
summary(data)

set.seed(20191127)
training_indices <- sample(seq(1000), 800)

data[, returning_buyer := as.factor(returning_buyer)]

data_train <- data[training_indices]
data_test <- data[-training_indices]

# Standard model prediction

formula <- as.formula("returning_buyer ~ .")
model <- glm(formula = formula, family = "binomial", data = data_train)
prediction <- predict(model, newdata = data_test, type = "response")
prediction <- round(prediction)

# prediction <- predict(model, newdata = data_train, type = "response")
# prediction <- round(prediction)
# mean(prediction == data_train$returning_buyer)

mean(prediction == data_test$returning_buyer)
pred_table <- table(prediction, data_test$returning_buyer)

tpr <- 63/93  # True Positive Rate, visszatértet becsült ÉS tényleg visszatért arány
fpr <- 16/107 # False Positive Rate, visszatértet becsült ÉS nem tért vissza arány
              # ha növelni akarom ezeket, akkor csökkentem a cutoff szintet (alap cél: tpr max, fpr min)

# Tree model predicition

library(rpart)
library(rpart.plot)

tree_model <- rpart(formula = formula, data = data_train)
rpart.plot(tree_model)

prediction_tree <- predict(tree_model, data_test)[, 2]
mean(round(prediction_tree) == data_test$returning_buyer)

# full_tree_model <- rpart(
#   formula = formula, data = data_train,
#   control = rpart.control(minsplit = 2, minbucket = 1, cp = 0)
# )
# rpart.plot(prune(full_tree_model, cp = 0.005))


# Random Forest Xtra-bit

library(randomForest)

tree_randomforest <- randomForest(formula = formula, data = data_train)
pred_randfor <- predict(tree_randomforest, data_test)
mean(pred_randfor == data_test$returning_buyer)
