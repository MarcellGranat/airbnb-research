library(rvest)
library(tidyverse)
library(RSelenium)

load("C:/school/szem_8/TDK-airbnb/airbnb-research/room_list2.RData")

room_interval <- 3101:10000
raw_dat <- list()

rD <- rsDriver(verbose = TRUE, 
               port=48330L, 
               chromever = '88.0.4324.27',
               check = TRUE)

remDr <- rD$client
sleep_time <- .5

for (i in room_interval) {
  run_total <- T
  while (run_total) {
    
  
  remDr$navigate(pull(room_list[i, ], URL))
  run <- T
  j <-  1
  
  url_descript <- character()
  url_amenities <- character()
  url_reviews <- character()
  title <- character()
  host <- character()
  rules <- character()
  assesment <- character()
  n_reviews <- character()
  bed <- character()
  price <- character()
  comments <- character()
  description <- character()
  amenities <- character()
  stars <- character()
  base_descript <- character()
  
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
    
    if ((length(url_reviews) != 0 &&
         length(assesment) != 0 &
         length(base_descript) != 0 & # the correct site is fully loaded
         length(title) != 0) &&
         title == pull(room_list[i, ], title)
        | j == 7) run <- F
    j <- j + 1
  }
  
  # reviews ---------------------------------------------------------------------------
  if (length(url_reviews) != 0 && url_reviews != 'https://www.airbnb.com') {
    remDr$navigate(url_reviews)
    run <- T
    j <- 1
    comments <- character()
    while(run) {
      if (j == 1) Sys.sleep(sleep_time)
      Sys.sleep(sleep_time)
      comments <- remDr$getPageSource()[[1]] %>% 
        read_html() %>% 
        html_nodes('._1xib9m0') %>% 
        html_text()
      if (length(comments) != 0 | j == 7) run <- F
      j <- j + 1
    }
  }
  
  if (length(url_descript) != 0 && url_descript != 'https://www.airbnb.com') {
    remDr$navigate(url_descript)
    run <- T
    j <- 1
    description <- character()
    while(run) {
      Sys.sleep(sleep_time)
      if (j == 1) Sys.sleep(3)
      description <- remDr$getPageSource()[[1]] %>% 
        read_html() %>% 
        html_nodes("._1xib9m0") %>% 
        html_text()
      if (length(description) != 0 | j == 7) run <- F
      j <- j + 1
    }
  }
    
    if (length(url_amenities) != 0 && url_amenities != 'https://www.airbnb.com') {
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
    }
  
  raw_dat[[length(raw_dat) + ifelse(sleep_time == .5, 1, 0)]] <- list(
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
  
  rm(list=setdiff(ls(), c('i', 'rD', 'room_interval', 'room_list', 'remDr', 'raw_dat', 'sleep_time', 'run_total')))
  if (length(raw_dat[[length(raw_dat)]]$title) == 0 & sleep_time < 9) {
  sleep_time <- sleep_time*2
  print(paste('run', i, 'again!'))
  } else {
    sleep_time <- .5
    run_total <- F
  }
  }
  if (i %% 50 == 0) {
    save(list = c("raw_dat"), file = paste0("C:/school/szem_8/TDK-airbnb/airbnb-research/room_rawdata/raw_dat", i ,".RData"))
    raw_dat <- list()
  }
}
