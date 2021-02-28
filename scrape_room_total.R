library(rvest)
library(tidyverse)
library(RSelenium)

source_URL <- "https://www.airbnb.hu/s/budapest/homes?tab_id=home_tab&refinement_paths%5B%5D=%2Fhomes&flexible_trip_dates%5B%5D=february&flexible_trip_dates%5B%5D=march&flexible_trip_lengths%5B%5D=weekend_trip&date_picker_type=calendar&source=structured_search_input_header&search_type=search_query"

URL <- read_html(source_URL) %>% 
  html_nodes("._gjfol0") %>%
  html_attr("href") %>% 
  {paste0("https://www.airbnb.com", .)}

raw_dat <- list()
sleep_time <- 5


rD <- rsDriver(verbose = TRUE, 
               port=48116L, 
               chromever = '88.0.4324.27',
               check = TRUE)


remDr <- rD$client



for (i in 1:length(URL)) {
# for (i in 1:2) { # TODO

remDr$navigate(URL[i])
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

remDr$navigate(url_reviews)

url_descript <- page_room %>% 
  html_nodes("._1xib9m0 ._gzog035") %>% 
  html_attr("href") %>% 
  {paste0("https://www.airbnb.com", .)}

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

price <- page_room %>% 
  html_nodes("._ymq6as") %>% 
  html_text()

stars <- page_room %>% 
  html_nodes("._1s11ltsf") %>% 
  html_text()

# reviews
Sys.sleep(sleep_time)
comments <- remDr$getPageSource()[[1]] %>% 
  read_html() %>% 
  html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "_1xib9m0", " " ))]') %>% 
  html_text()

remDr$navigate(url_descript)
Sys.sleep(sleep_time)
description <- remDr$getPageSource()[[1]] %>% 
  read_html() %>% 
  html_nodes("._1xib9m0") %>% 
  html_text()

remDr$navigate(url_amenities)
Sys.sleep(sleep_time)
amenities <- remDr$getPageSource()[[1]] %>% 
  read_html() %>% 
  html_nodes("._vzrbjl") %>% 
  html_text()

l <- list(
  url = URL[i],
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
  amenities = amenities
)

raw_dat[[length(raw_dat) + 1]] <- l

}

save(list = c("raw_dat"), file = "raw_dat.RData")


