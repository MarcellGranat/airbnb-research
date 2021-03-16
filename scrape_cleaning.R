library(rvest)
library(tidyverse)
library(RSelenium)

setwd('C:/school/szem_8/TDK-airbnb/airbnb-research/room_list')
room_list_total <- tibble()
for (i in list.files(pattern = "room_list")) {
  load(i)
  room_list <- room_list %>% 
    mutate(
      date = i
    )
  room_list_total <- rbind(room_list_total, room_list)
}

room_list_total <- room_list_total %>% 
  mutate(
    id = gsub('.*rooms/', '', gsub('\\?ch.*', '', URL))
  ) %>% 
  tibble() %>% 
  mutate(
    price = as.numeric(str_remove_all(price, '\\$')),
    n_reviews = gsub(pattern = '.*[(]', replacement = '', x = assesment) %>% 
      gsub(pattern = ' .*', replacement = '') %>% 
      as.numeric(),
    assesment = gsub('\\s.*', '', assesment) %>%
      as.numeric()
  ) %>% 
  select(id, n_reviews) %>% 
  group_by(id) %>% 
  summarise(n_reviews = max(n_reviews, na.rm = T)) %>% 
  ungroup()

load("C:/school/szem_8/TDK-airbnb/airbnb-research/dat_cleaned.RData")

room_interval <- dat %>% 
  lapply(function(x) {
    data.frame(id = x$source$id, comments = length(x$comments), z = ifelse(length(x$url_amenities) == 0 &&
          length(x$url_reviews) == 0 &&  
          length(x$host) == 0
            , T, F))
  }) %>% 
  reduce(rbind) %>% 
  tibble() %>% 
  merge(room_list_total, all = F, sort = F) %>% 
  mutate(y = ifelse((comments == 0 & n_reviews > 0) | z, row_number(), NA)) %>% 
  pull(y) %>% 
  na.omit()


rD <- rsDriver(verbose = TRUE, 
               port=48466L, 
               chromever = '88.0.4324.27',
               check = TRUE)

remDr <- rD$client
ctime <- as.numeric(format(Sys.time(), "%M")) %/% 10

for (i in room_interval) {
  remDr$navigate(pull(dat[[i]]$source, URL))
  
  url_descript <- character()
  url_amenities <- character()
  url_reviews <- character()
  host <- character()
  rules <- character()
  assesment <- character()
  bed <- character()
  comments <- character()
  amenities <- character()
  stars <- character()
  
  Sys.sleep(4)
  page_room <- remDr$getPageSource()[[1]] %>% 
    read_html() 
  
  page_room %>% 
    html_nodes("._13e0raay") %>% 
    html_attr("href") %>% 
    {paste0("https://www.airbnb.com", .)} %>% 
    {
      url_amenities <<- str_subset(., "amenities")
      url_reviews <<- str_subset(., "reviews")
    }
  
  host <- page_room %>% 
    html_nodes("._14i3z6h") %>% # host
    html_text()
  
  rules <- page_room %>% 
    html_nodes("._u827kd") %>% # rules
    html_text()
  
  bed <- page_room %>% 
    html_nodes("._1a5glfg") %>% # bed
    html_text()
  
  stars <- page_room %>% 
    html_nodes("._1s11ltsf") %>% 
    html_text()
  
  
  # reviews + amenities -------------------------------------------------------------------
  if (length(url_reviews) != 0 && url_reviews != 'https://www.airbnb.com') {
    
    remDr$navigate(url_reviews)
    Sys.sleep(6)     
    comments <- remDr$getPageSource()[[1]] %>% 
      read_html() %>% 
      html_nodes('._1xib9m0') %>% 
      html_text()
    
    remDr$navigate(url_amenities)
    Sys.sleep(3)     
    amenities <- remDr$getPageSource()[[1]] %>% 
      read_html() %>% 
      html_nodes("._vzrbjl") %>% 
      html_text()
  }
  
  dat[[i]] <- list(
    source = dat[[i]]$source,
    url_amenities = url_amenities,
    url_reviews = url_reviews,
    host = host,
    rules = rules,
    bed = bed,
    comments = comments,
    amenities = amenities,
    stars = stars
  )
  
  if (as.numeric(format(Sys.time(), "%M")) %/% 10 != ctime) {
    ctime <- as.numeric(format(Sys.time(), "%M")) %/% 10
    save(list = c("dat"), file = "C:/school/szem_8/TDK-airbnb/airbnb-research/dat_cleaned.RData")
  }
}
