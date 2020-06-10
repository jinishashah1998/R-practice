library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(tidyr)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

set.seed(42)
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
test_set <- titanic_clean[test_index, ]
train_set <- titanic_clean[-test_index, ]

#set.seed(3)
set.seed(3, sample.kind = "Rounding")
# guess with equal probability of survival
guess <- sample(c(0,1), nrow(test_set), replace = TRUE)
mean(guess == test_set$Survived)

train_set %>%
  group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1))

test_set %>%
  summarize( (sum(Sex == 'female' & Survived == 1) + sum(Sex == 'male' & Survived == 0)) / n())
#or
sex_model <- ifelse(test_set$Sex == "female", 1, 0)    # predict Survived=1 if female, 0 if male
mean(sex_model == test_set$Survived)    # calculate accuracy


train_set %>%
  group_by(Pclass) %>%
  summarize(Survived = mean(Survived == 1))

test_set %>%
  inner_join(survival_class, by='Pclass') %>%
  summarize(PredictingSurvival = mean(Survived == PredictingSurvival))
#or
class_model <- ifelse(test_set$Pclass == 1, 1, 0)    # predict survival only if first class
mean(class_model == test_set$Survived)    # calculate accuracy

train_set %>%
  group_by(Sex, Pclass) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Survived > 0.5)

test_set %>%
  inner_join(survival_class, by=c('Sex', 'Pclass')) %>%
  summarize(PredictingSurvival = mean(Survived == PredictingSurvival))

sex_model <- train_set %>%
  group_by(Sex) %>%
  summarize(Survived_predict = ifelse(mean(Survived == 1) > 0.5, 1, 0))
test_set1 <- test_set %>%
  inner_join(sex_model, by = 'Sex')
cm1 <- confusionMatrix(data = factor(test_set1$Survived_predict), reference = factor(test_set1$Survived))
print(cm1)

class_model <- train_set %>%
  group_by(Pclass) %>%
  summarize(Survived_predict = ifelse(mean(Survived == 1) > 0.5, 1, 0))
test_set2 <- test_set %>%
  inner_join(class_model, by = 'Pclass')

sex_class_model <- train_set %>%
  group_by(Sex, Pclass) %>%
  summarize(Survived_predict = ifelse(mean(Survived == 1) > 0.5, 1, 0))
test_set3 <- test_set %>%
  inner_join(sex_class_model, by=c('Sex', 'Pclass'))

F_meas(data=factor(test_set1$Survived), reference = factor(test_set1$Survived_predict))
F_meas(data=factor(test_set2$Survived), reference = factor(test_set2$Survived_predict))
F_meas(data=factor(test_set3$Survived), reference = factor(test_set3$Survived_predict))


fit_lda <- train(Survived ~ Fare, data = train_set, method = 'lda')
Survived_hat <- predict(fit_lda, test_set)
mean(test_set$Survived == Survived_hat)

fit_qda <- train(Survived ~ Fare, data = train_set, method = 'qda')
Survived_hat <- predict(fit_qda, test_set)
mean(test_set$Survived == Survived_hat)


fit_logreg_a <- glm(Survived ~ Age, data = train_set, family = 'binomial')
survived_hat_a <- ifelse(predict(fit_logreg_a, test_set) >= 0, 1, 0)
mean(survived_hat_a == test_set$Survived)

fit_logreg_b <- glm(Survived ~ Sex + Pclass + Fare + Age, data = train_set, family = 'binomial')
survived_hat_b <- ifelse(predict(fit_logreg_b, test_set) >= 0, 1, 0)
mean(survived_hat_b == test_set$Survived)

str(train_set)
fit_logreg_c <- glm(Survived ~ ., data = train_set, family = 'binomial')
survived_hat_c <- ifelse(predict(fit_logreg_c, test_set) >= 0, 1, 0)
mean(survived_hat_c == test_set$Survived)


set.seed(6, sample.kind = "Rounding")
# Method below doesn't give same result as EdX (though it is correct)
# ks <- seq(3,51,2)
# res_knn9a <- sapply(ks, function(k) {
#     fit_knn9a <- knn3(Survived ~ ., data = train_set, k = k)
#     survived_hat <- predict(fit_knn9a, train_set, type = "class") %>% factor(levels = levels(train_set$Survived))
#     cm_test <- confusionMatrix(data = survived_hat, reference = train_set$Survived)
#     cm_test$overall["Accuracy"]
# })
# ks[which.max(res_knn9a)]
# Other method using train function
k <- seq(3,51,2)
fit_knn9a <- train(Survived ~ ., data = train_set, method = "knn", tuneGrid = data.frame(k))
fit_knn9a$bestTune

ggplot(fit_knn9a)

survived_hat <- predict(fit_knn9a, test_set) %>% factor(levels = levels(test_set$Survived))
cm_test <- confusionMatrix(data = survived_hat, reference = test_set$Survived)
cm_test$overall["Accuracy"]


set.seed(8, sample.kind = "Rounding")
fit_knn10 <- train(Survived ~ ., 
                   data=train_set, 
                   method = "knn",
                   tuneGrid = data.frame(k = seq(3, 51, 2)),
                   trControl = trainControl(method = "cv", number=10, p=0.9))
fit_knn10
survived_hat <- predict(fit_knn10, test_set)
cm_test <- confusionMatrix(data = survived_hat, reference = test_set$Survived)
cm_test$overall["Accuracy"]


set.seed(10, sample.kind = 'Rounding')
fit_rpart11 <- train(Survived ~ ., 
                     data=train_set, 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))
fit_rpart11$bestTune
plot(fit_rpart11)
survived_hat <- predict(fit_rpart11, test_set)
cm_test <- confusionMatrix(data = survived_hat, reference = test_set$Survived)
cm_test$overall["Accuracy"]


fit_rpart11$finalModel
plot(fit_rpart11$finalModel, margin=0.1)
text(fit_rpart11$finalModel, cex = 0.75)

set.seed(14, sample.kind = 'Rounding')
fit12_rf <- train(Survived ~., 
                  data = train_set,
                  method = "rf", 
                  tuneGrid = data.frame(mtry = seq(1, 7)), 
                  ntree = 100)
fit12_rf$bestTune
survived_hat <- predict(fit12_rf, test_set)
mean(survived_hat == test_set$Survived)
varImp(fit12_rf)
