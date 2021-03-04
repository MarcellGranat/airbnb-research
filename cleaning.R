dat <- list()
for (i in 1:50) {
  load(paste0("C:/school/szem_8/TDK-kincs/airbnb-research/room_rawdata/raw_dat", i*50 ,".RData"))
  dat <- c(dat, raw_dat)
}
save(list = c("dat"), file = "C:/school/szem_8/TDK-kincs/airbnb-research/dat.RData")
dat_comment <- dat
# comments 1 ------------------------------------------------------------------------


dat_comment <- lapply(dat, function(x) {
  tryCatch({
  data.frame(assesment = x$assesment, comments = x$comments) %>% 
    tail(-2)
  }, error = function(e) NULL)
})

dat_comment <- reduce(Filter(f = Negate(is.null), dat_comment), rbind)

dat_comment <- dat_comment %>% 
  mutate(
    comments = str_replace_all(comments, '\n', ' '),
    english = case_when(
      str_detect(comments, 'very') ~ T,
      str_detect(comments, 'and') ~ T,
      str_detect(comments, 'house') ~ T,
      str_detect(comments, 'but') ~ T,
      str_detect(comments, 'during') ~ T,
      str_detect(comments, 'we ') ~ T,
      str_detect(comments, 'come') ~ T,
      str_detect(comments, 'location') ~ T,
      str_detect(comments, 'after') ~ T,
      str_detect(comments, 'from') ~ T,
      str_detect(comments, 'hogy') ~ T,
      str_detect(comments, '무') ~ NA,
      T ~ NA
    )
  ) %>% 
  select(-english) %>% 
  na.omit()

dat_comment <- dat_comment %>% 
  apply(1, function(x) {
  tidytext::unnest_tokens(data.frame(comments = x[2]), words, comments, to_lower = T) %>% 
    mutate(value = x[1])
  }) %>% 
  reduce(rbind) %>% 
  tibble() %>% 
  mutate(
    value = str_replace_all(value, ',', '.') %>% as.numeric()
  ) %>% 
  anti_join(data.frame(words = c(stopwords::stopwords(), "also", "can"))) %>% 
  filter(!str_detect(words, '[^a-z]')) %>% 
  group_by(words) %>% 
  summarise(value = mean(value, na.rm = T), n = n()) %>% 
  ungroup()

dat_comment %>% 
  arrange(desc(value)) %>% 
  {rbind(mutate(head(., 50), type = "High"), mutate(tail(., 50), type = "Low"))} %>% 
  reshape2::acast(words ~ type, value.var = "n", fill = 0) %>%
  wordcloud::comparison.cloud(colors = c("red4", "cyan4"),
                   max.words = 100)

# comments 2 ---------------------------------------



