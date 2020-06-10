library(tidyverse)
library(caret)
library(dslabs)

#set.seed(1, sample.kind="Rounding") if using R 3.6 or later
n <- c(100,500,1000,5000,10000)

my_func <- function(n) {
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata = test_set)
    sqrt(mean((y_hat-test_set$y)^2))
  })
  
  mean(rmse)
  #sd(rmse)
}

set.seed(1, sample.kind="Rounding")
sapply(n,my_func)