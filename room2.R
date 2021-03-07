library(rvest)
library(tidyverse)
library(RSelenium)

load("C:/school/szem_8/TDK-airbnb/airbnb-research/room_list2.RData")

room_interval <- 3101:3104
raw_dat <- list()

rD <- rsDriver(verbose = TRUE, 
               port=48409L, 
               chromever = '88.0.4324.27',
               check = TRUE)

remDr <- rD$client

for (i in room_interval) {
  print(pull(room_list[i, ], title))
  run <- 1
  while (run < 5) {
    if (run == 1 | run == 3) remDr$navigate(pull(room_list[i, ], URL))
    Sys.sleep(run^2-.5)
    
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
    
    url_descript <- page_room %>% 
      html_nodes("._1xib9m0 ._gzog035") %>% 
      html_attr("href") %>% 
      {paste0("https://www.airbnb.com", .)}
    
    if (length(url_descript) != 0 && url_descript == 'https://www.airbnb.com') url_descript <- character()
    if (length(url_reviews) != 0 && url_reviews == 'https://www.airbnb.com') url_descript <- character()
    if (length(url_amenities) != 0 && url_amenities == 'https://www.airbnb.com') url_descript <- character()
    
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
    
    n_reviews <- page_room %>% 
      html_nodes("._owhb5o") %>% # number of reviews
      html_text()
    
    bed <- page_room %>% 
      html_nodes("._1a5glfg") %>% # bed
      html_text()
    
    stars <- page_room %>% 
      html_nodes("._1s11ltsf") %>% 
      html_text()
    
    base_descript <- page_room %>% 
      html_nodes("._1xib9m0") %>% 
      html_text()
    
    if (length(title) == 0 | !(length(n_reviews) != 0 && (length(url_reviews) == 0 | length(assesment) == 0))) {
      run <- run + 1
    } else {
      run <- 5
    }
    print(run)
    print(title)
    print(length(title) == 0 | !(length(n_reviews) != 0 && (length(url_reviews) == 0 | length(assesment) == 0)))
  }
    
    # reviews ---------------------------------------------------------------------------
    run <- 5
    if (length(url_reviews) != 0 && url_reviews != 'https://www.airbnb.com') run <- 1
    while (run < 5) {
      if (run == 1 | run == 3) remDr$navigate(url_reviews)
      comments <- character()
      Sys.sleep(run^2-.5)     
      comments <- remDr$getPageSource()[[1]] %>% 
        read_html() %>% 
        html_nodes('._1xib9m0') %>% 
        html_text()
      if (length(comments) == 0) {
        run <- run + 1
      } else {
        run <- 5
      }
      print(run)
      print(length(comments) == 0)
    }
    
    run <- 5
    if (length(url_descript) != 0 && url_descript != 'https://www.airbnb.com') run <- 1
    while (run < 5) {
      if (run == 1 | run == 3) remDr$navigate(url_descript)
      description <- character()
      Sys.sleep(run^2-.5)     
      description <- remDr$getPageSource()[[1]] %>% 
        read_html() %>% 
        html_nodes("._1xib9m0") %>% 
        html_text()
      if (length(description) == 0) {
        run <- run + 1
      } else {
        run <- 5
      } 
      print(run)
      print(length(description) == 0)
    }
    
    run <- 5
    if (length(url_amenities) != 0 && url_amenities != 'https://www.airbnb.com') run <- 1
    while (run < 5) {
      if (run == 1 | run == 3) remDr$navigate(url_amenities)
      amenities <- character()
      Sys.sleep(run^2-.5)     
      amenities <- remDr$getPageSource()[[1]] %>% 
        read_html() %>% 
        html_nodes("._vzrbjl") %>% 
        html_text()
      if (length(amenities) == 0) {
        run <- run + 1
      } else {
        run <- 5
      } 
      print(run)
      print(length(amenities) == 0)
    }
    
    raw_dat[[length(raw_dat) + 1]] <- list(
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
  
  if (i %% 50 == 0) {
    save(list = c("raw_dat"), file = paste0("C:/school/szem_8/TDK-airbnb/airbnb-research/room_rawdata/raw_dat", i ,".RData"))
    raw_dat <- list()
  }
}

