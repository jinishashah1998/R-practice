library(dslabs)
library(tidyverse)

set.seed(1986) #for R 3.5 or earlier
#if using R 3.6 or later, use `set.seed(1986, sample.kind="Rounding")` instead
n <- round(2^rnorm(1000, 8, 1))

set.seed(1) #for R 3.5 or earlier
#if using R 3.6 or later, use `set.seed(1, sample.kind="Rounding")` instead
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

schools %>% top_n(10, quality) %>% arrange(desc(quality))

set.seed(1) #for R 3.5 or earlier
#if using R 3.6 or later, use `set.seed(1, sample.kind="Rounding")` instead
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

schools %>% top_n(10, score) %>% arrange(desc(score)) %>% select(id, size, score)

schools %>% summarize(median(size))

schools %>% top_n(10, score) %>% summarize(median(size))

schools %>% top_n(-10, score) %>% summarize(median(size))

overall <- mean(sapply(scores, mean))
alpha <- 25
schools5 <- schools %>%
  mutate(score_dev = overall + (score - overall) * size / (size + alpha)) %>%
  arrange(desc(score_dev))
#    mutate(quality_new = score_dev-80)
schools5 %>%
  top_n(10, score_dev)


overall <- mean(sapply(scores, mean))
alpha <- 135
schools5 <- schools %>%
  mutate(score_dev = overall + (score - overall) * size / (size + alpha)) %>%
  arrange(desc(score_dev))
#    mutate(quality_new = score_dev-80)
schools5 %>%
  top_n(10, score_dev)