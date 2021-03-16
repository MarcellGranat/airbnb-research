library(tidyverse)
library(rvest)

URLs <- read_html('https://hu.wikipedia.org/wiki/Magyarorsz%C3%A1g_telep%C3%BCl%C3%A9sei:_A,_%C3%81') %>% 
  html_nodes('#toc a') %>% 
  html_attr('href') %>% 
  na.omit() %>% 
  {paste0('https://hu.wikipedia.org', .)} %>% 
  {c('https://hu.wikipedia.org/wiki/Magyarorsz%C3%A1g_telep%C3%BCl%C3%A9sei:_A,_%C3%81', .)} %>% 
  {ifelse(duplicated(.), NA, .)} %>% 
  na.omit()



hun_counties <- lapply(URLs, function(x) {
read_html(x) %>% 
  html_table(fill = T) %>% 
  .[[3]] %>% 
  tibble()
}) %>% 
  reduce(rbind) %>% 
  select(1, 3, 5) %>% 
  set_names('city', 'county', 'pop')

save(list = c("hun_counties"), file = "C:/school/szem_8/TDK-airbnb/airbnb-research/hun_counties.RData")
  
