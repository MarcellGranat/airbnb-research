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
  tibble()

room_list_total <- tibble(room_list_total) %>% 
  filter(!duplicated(id)) %>%
  filter(!is.na(assesment)) %>%
  filter(str_detect(assesment, '\\.')) %>%
  mutate(
    price = as.numeric(str_remove_all(price, '\\$')),
    n_reviews = gsub(pattern = '.*[(]', replacement = '', x = assesment) %>% 
      gsub(pattern = ' .*', replacement = '') %>% 
      as.numeric(),
    assesment = gsub('\\s.*', '', assesment) %>%
      as.numeric()
  )

room_interval <- 6900:nrow(room_list_total) 
# TODO MODIFY
raw_dat <- list()

rD <- rsDriver(verbose = TRUE, 
               port=48460L, 
               chromever = '88.0.4324.27',
               check = TRUE)

remDr <- rD$client

for (i in room_interval) {
remDr$navigate(pull(room_list_total[i, ], URL))
  
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
  
  raw_dat[[length(raw_dat) + 1]] <- list(
    source = room_list_total[i, ],
    url_amenities = url_amenities,
    url_reviews = url_reviews,
    host = host,
    rules = rules,
    bed = bed,
    comments = comments,
    amenities = amenities,
    stars = stars
  )
  
  if (i %% 50 == 0) {
    save(list = c("raw_dat"), file = 
           paste0("C:/school/szem_8/TDK-airbnb/airbnb-research/room_rawdata/raw_dat", 
                  i ,".RData"))
    raw_dat <- list()
  }
}
