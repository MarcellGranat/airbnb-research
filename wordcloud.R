library(tidyverse)
library(tidytext)
load("C:/school/szem_8/TDK-airbnb/airbnb-research/dat_words.RData")
load('room_list.RData')

room_list <- room_list_total %>% 
  group_by(id) %>% 
  summarise(n_reviews = max(n_reviews, na.rm = T), assesment = mean(assesment, na.rm = T)) %>% 
  ungroup()

room_list %>% 
  filter(n_reviews >= 0) %>% 
  mutate(
    type = as.numeric(Hmisc::cut2(assesment, g = 20, levels.mean = T)),
    type = case_when(
      type == min(type) ~ 'Low',
      type == max(type) ~ 'High',
      T ~ 'Middle'
    ),
  ) %>% 
  merge(dat_words) %>% 
  filter(language %in% c('hungarian')) %>% 
  mutate(
    words = SnowballC::wordStem(words, language = 'hungarian')
  ) %>% 
  group_by(words, type) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  select(words, type, n) %>%
  bind_tf_idf(term = words, document = type, n = n) %>% 
  anti_join(data.frame(words = c(stopwords::stopwords(), "also", "can"))) %>%
  anti_join(data.frame(words = c(stopwords::stopwords(language = "hu")))) %>%
  filter(type != 'Middle' & n > 20 & !str_detect(words, '\\s')) %>%
  arrange(desc(tf_idf)) %>%
  group_by(type) %>%
  group_modify(~ head(mutate(.x, rank = row_number()), 50))  %>%
  mutate(
    rank = 51- rank
  ) %>% 
  reshape2::acast(words ~ type, value.var = "rank", fill = 0) %>%
  wordcloud::comparison.cloud(colors = viridis::viridis(2, direction = -1 , end = .7),
                              max.words = 50)