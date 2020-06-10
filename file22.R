library(dslabs)
library(tidyverse)
data("movielens")

movielens %>%
  group_by(movieId) %>%
  summarize(n = n_distinct(userId), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

res2 <- movielens %>%
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(avg_rating = mean(rating), n = n(), title=title[1], years=2018 - first(year)) %>%
  mutate(n_year = n / years) %>%
  top_n(25, n_year) %>%
  arrange(desc(n_year))

movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()