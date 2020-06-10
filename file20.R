library(caret)
library(dslabs)
set.seed(1) # use `set.seed(1, sample.kind = "Rounding")` in R 3.6 or later
data("mnist_27")

models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 
names(fits) <- models

pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))
dim(pred)

model_accuracies <- colMeans(pred == mnist_27$test$y)
model_accuracies
mean(model_accuracies)

votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

ind <- model_accuracies > mean(y_hat == mnist_27$test$y)
sum(ind)
models[ind]

acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_hat)

ind <- acc_hat >= 0.8
votes <- rowMeans(pred[,ind] == "7")
y_hat <- ifelse(votes>=0.5, 7, 2)
mean(y_hat == mnist_27$test$y)
