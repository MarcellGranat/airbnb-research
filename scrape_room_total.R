library(rvest)
library(tidyverse)
library(RSelenium)

load("C:/school/szem_8/TDK-kincs/airbnb-research/room_list2.RData")

room_interval <- 2501:10000

URL <- room_list$URL

raw_dat <- list()
sleep_time <- 1

rD <- rsDriver(verbose = TRUE, 
               port=48304L, 
               chromever = '88.0.4324.27',
               check = TRUE)

remDr <- rD$client

for (i in room_interval) {
  
  remDr$navigate(pull(room_list[i, ], URL))
  run <- T
  j = 1
  url_amenities <- character()
  url_reviews <- character()
  url_descript <- character()
  title <- character()
  host <- character()
  rules <- character()
  assesment <- character()
  n_reviews <- character()
  bed <- character()
  price <- character()
  stars <- character()
  
  while (run) {
    Sys.sleep(sleep_time)
    page_room <- remDr$getPageSource()[[1]]
    page_room <- read_html(page_room) 
    page_room %>% 
      html_nodes("._13e0raay") %>% 
      html_attr("href") %>% 
      {paste0("https://www.airbnb.com", .)} %>% 
      {
        url_amenities <<- str_subset(., "amenities")
        url_reviews <<- str_subset(., "reviews")
      }
    
    if (length(url_reviews) == 0) {
      url_reviews <- page_room %>% 
        html_nodes(".a._13e0raay") %>%
        html_attr('href')
    }
    
    url_descript <- page_room %>% 
      html_nodes("._1xib9m0 ._gzog035") %>% 
      html_attr("href") %>% 
      {paste0("https://www.airbnb.com", .)}
    
    if (url_descript == 'https://www.airbnb.com') url_descript <- character()
    
    title<- page_room %>% 
      html_nodes("._mbmcsn ._14i3z6h") %>% # title
      html_text()
    
    host <- page_room %>% 
      html_nodes("._14i3z6h") %>% # host
      html_text()
    
    rules <- page_room %>% 
      html_nodes("._u827kd") %>% # rules
      html_text()
    
    assesment <- page_room %>% 
      html_nodes("._17204k6g") %>% # assesment
      html_text()
    
    if (length(assesment) == 0) {
      assesment <- page_room %>% 
        html_nodes("._1hhxz3f ._pgfqnw") %>% # assesment
        html_text()
    }
    
    n_reviews <- page_room %>% 
      html_nodes("._owhb5o") %>% # number of reviews
      html_text()
    
    bed <- page_room %>% 
      html_nodes("._1a5glfg") %>% # bed
      html_text()
    
    price <- page_room %>% 
      html_nodes("._ymq6as") %>% 
      html_text()
    
    stars <- page_room %>% 
      html_nodes("._1s11ltsf") %>% 
      html_text()
    
    base_descript <- page_room %>% 
      html_nodes("._1xib9m0") %>% 
      html_text()
    
    if ((length(url_reviews) != 0 & 
         length(url_amenities) != 0 &
         length(url_descript) != 0 &
         length(title) != 0 &
         length(host) != 0 &
         length(rules) != 0 &
         length(assesment) != 0 &
         length(n_reviews) != 0 &
         length(bed) != 0 &
         length(price) != 0 &
         length(stars) != 0
    ) | j == 7) run <- F
    j <- j + 1
  }
  
  # reviews ---------------------------------------------------------------------------
  
  remDr$navigate(url_reviews)
  run <- T
  j <- 1
  comments <- character()
  while(run) {
    Sys.sleep(sleep_time)
    comments <- remDr$getPageSource()[[1]] %>% 
      read_html() %>% 
      html_nodes('._1xib9m0') %>% 
      html_text()
    if (length(comments) != 0 | j == 7) run <- F
    j <- j + 1
  }
  
  
  
  remDr$navigate(url_descript)
  run <- T
  j <- 1
  description <- character()
  while(run) {
    Sys.sleep(sleep_time)
    if (i == 1) Sys.sleep(3)
    description <- remDr$getPageSource()[[1]] %>% 
      read_html() %>% 
      html_nodes("._1xib9m0") %>% 
      html_text()
    if (length(description) != 0 | j == 7) run <- F
    j <- j + 1
  }
  
  remDr$navigate(url_amenities)
  run <- T
  j <- 1
  amenities <- character()
  while(run) {
    Sys.sleep(sleep_time)
    amenities <- remDr$getPageSource()[[1]] %>% 
      read_html() %>% 
      html_nodes("._vzrbjl") %>% 
      html_text()
    if (length(amenities) != 0 | j == 7) run <- F
    j <- j + 1
  }
  
  l <- list(
    source = room_list[i, ],
    url_descript = url_descript,
    url_amenities = url_amenities,
    url_reviews = url_reviews,
    title = title,
    host = host,
    rules = rules,
    assesment = assesment,
    n_reviews = n_reviews,
    bed = bed,
    price = price,
    comments = comments,
    description = description,
    amenities = amenities,
    stars = stars,
    base_descript = base_descript
  )
  
  raw_dat[[length(raw_dat) + 1]] <- l
  if (i %% 50 == 0) {
    save(list = c("raw_dat"), file = paste0("C:/school/szem_8/TDK-kincs/airbnb-research/room_rawdata/raw_dat", i ,".RData"))
    raw_dat <- list()
  }
}
