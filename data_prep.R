library(tidyverse)
library(parallel)
load("C:/school/szem_8/TDK-airbnb/airbnb-research/hun_counties.RData")
load("C:/school/szem_8/TDK-airbnb/airbnb-research/dat_room_final.RData")

cl <- makeCluster(7)
clusterExport(cl, list("dat"), envir = environment())
clusterEvalQ(cl, library(tidyverse))

dat_rbnb <- parLapply(cl = cl, dat, function(x) {
  tibble(
    id = gsub('.*rooms/', '', x$url) %>% 
      gsub(pattern = '\\?.*', replacement = ''),
    url = x$url,
    title = x$title[1] %>% 
      {ifelse(is.null(.), NA, .)},
    subtitle = gsub(' hosted.*', '', x$subtitle),
    host = gsub('.*by', '', x$subtitle) %>% 
      str_sub(2) %>% 
      {ifelse(is.null(.), NA, .)},
    geo = x$geo %>% 
      {.[str_detect(., 'Hungary')]} %>% 
      gsub(pattern = ',.*', replacement = '') %>% 
      {ifelse(is.null(.), NA, .)} %>% 
      str_remove_all('\\d') %>% 
      trimws() %>% 
      {ifelse(str_detect(., 'istrict'), 'Budapest', .)} %>% 
      {ifelse(str_detect(., 'Pest'), 'Budapest', .)}
    ,
    price = x$price[1] %>% 
      str_remove_all('\\$') %>% 
      str_remove_all(',') %>% 
      as.numeric() %>% 
      {ifelse(is.null(.), NA, .)},
    n_reviews = gsub(pattern = '.*\\(', replacement = '', x = x$n_reviews) %>% 
      str_remove_all('\\)') %>% 
      .[1] %>% 
      as.numeric() %>% 
      {ifelse(is.null(.), NA, .)},
    assesment = sub(pattern = '\\(.*', replacement = '', x = x$n_reviews) %>% 
      str_remove_all('\\)') %>% 
      .[1] %>% 
      as.numeric() %>% 
      {ifelse(is.null(.), NA, .)},
    stars_cleanliness = x$stars %>% 
      {.[str_detect(., 'Cleanliness')]} %>% 
      str_remove_all('\\D') %>% 
      as.numeric() %>% 
      {./10} %>% 
      {ifelse(is.null(.), NA, .)},
    stars_accuracy = x$stars %>% 
      {.[str_detect(., 'Accuracy')]} %>% 
      str_remove_all('\\D') %>% 
      as.numeric() %>% 
      {./10} %>% 
      {ifelse(is.null(.), NA, .)},
    stars_communication = x$stars %>% 
      {.[str_detect(., 'Communication')]} %>% 
      str_remove_all('\\D') %>% 
      as.numeric() %>% 
      {./10} %>% 
      {ifelse(is.null(.), NA, .)},
    stars_location = x$stars %>% 
      {.[str_detect(., 'Location')]} %>% 
      str_remove_all('\\D') %>% 
      as.numeric() %>% 
      {./10} %>% 
      {ifelse(is.null(.), NA, .)},
    stars_checkin = x$stars %>% 
      {.[str_detect(., 'Check-in')]} %>% 
      str_remove_all('\\D') %>% 
      as.numeric() %>% 
      {./10} %>% 
      {ifelse(is.null(.), NA, .)},
    stars_value = x$stars %>% 
      {.[str_detect(., 'Value')]} %>% 
      str_remove_all('\\D') %>% 
      as.numeric() %>% 
      {./10} %>% 
      {ifelse(is.null(.), NA, .)},
    description = str_c(x$description, collapse = ' ') %>% 
      {ifelse(is.null(.), NA, .)},
  ) %>% 
    mutate(n_reviews = ifelse(is.na(n_reviews) & sum(str_detect(x$geo, 'review')) == 1, 
                              x$geo %>% 
                                {.[str_detect(., 'review')]} %>% 
                                str_remove_all('\\D') %>% 
                                as.numeric(),
                              n_reviews
    ) 
    )
}) %>% 
  reduce(rbind) %>% 
  filter(!is.na(geo)) 

dat_rbnb <- dat_rbnb %>% 
  mutate(
    price = ifelse(price > 1500, NA, price),
  )

dat_comments <- parLapply(cl = cl, dat, function(x) {
  tryCatch({
    x$comments[!(x$comments %in% x$description)] %>% 
      {tibble(text = .)} %>% 
      filter(!duplicated(str_sub(text, end = 20))) %>% 
      mutate(language = textcat::textcat(text)) %>% 
      mutate(
        language = ifelse(str_detect(text, 'nice'), 'english', language),
        language = ifelse(str_detect(text, 'place'), 'english', language),
        language = ifelse(str_detect(text, 'great'), 'english', language),
        language = ifelse(str_detect(text, 'perfect'), 'english', language),
        language = ifelse(str_detect(text, 'location'), 'english', language),
        language = ifelse(str_detect(text, 'only'), 'english', language),
        language = ifelse(str_detect(text, 'minden'), 'hungarian', language),
        language = ifelse(str_detect(text, 'Minden'), 'hungarian', language),
        language = ifelse(str_detect(text, 'nagyon'), 'hungarian', language),
        language = ifelse(str_detect(text, 'Nagyon'), 'hungarian', language),
        language = ifelse(str_detect(text, 'Hely'), 'hungarian', language),
        language = ifelse(str_detect(text, 'hely'), 'hungarian', language),
        language = ifelse(str_detect(text, 'zerintem'), 'hungarian', language),
        language = ifelse(str_detect(text, 'ökéletes'), 'hungarian', language),
        language = ifelse(str_detect(text, 'tudom'), 'hungarian', language),
        language = ifelse(str_detect(text, 'Tudom'), 'hungarian', language),
        language = ifelse(language == 'scots', 'english', language),
        language = ifelse(language == 'welsh', 'english', language),
        language = ifelse(str_detect(text, 'pour'), 'french', language),
        language = ifelse(str_detect(text, 'rès'), 'french', language),
        text = str_replace_all(text, '\n', ' ')
      ) %>% 
      mutate(id = gsub('.*rooms/', '', x$url) %>% 
               gsub(pattern = '\\?.*', replacement = ''), 
             r = row_number())
  }, error = function(e) NULL)
}) %>% 
  {reduce(Filter(f = Negate(is.null), .), rbind)} %>% 
  tibble() %>% 
  filter(id %in% dat_rbnb$id)

dat_comments <- dat_comments %>% 
  filter(duplicated(text) & str_length(text) > 30) %>% 
  select(text) %>% 
  {anti_join(dat_comments, .)} %>% 
  filter(!str_detect(text, 'mins by car without traffic')) %>% 
  filter(!str_detect(text, 'The neighborhood is trendy and lively'))
  
stopCluster(cl)

save(list = c('dat_rbnb', 'dat_comments', 'hun_counties'), file = 'C:/school/szem_8/TDK-airbnb/airbnb-research/dat_rbnb.RData')
