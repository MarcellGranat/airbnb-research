library(rvest)
library(tidyverse)
library(RSelenium)
library(seleniumPipes)

load("C:/school/szem_8/TDK-airbnb/airbnb-research/hun_counties.RData")
hun_map <- list()
rD <- rsDriver(verbose = TRUE, 
               port=48465L, 
               chromever = '88.0.4324.27',
               check = TRUE)

remDr <- rD$client
remDr$navigate('https://www.futas.net/hungary/gps-geokoordinatak.php')
Sys.sleep(4)

for (i in 1:nrow(hun_counties)) {


webElement <- remDr$findElement(using = 'css selector',"#address")
webElement$clearElement()
webElement$sendKeysToElement(list(paste0(pull(hun_counties, city)[i], ', Magyarország'), key="enter"))
web_button <- remDr$findElement(using = 'css selector',"#address+ input")
web_button$clickElement()
try(remDr$acceptAlert(), TRUE)
Sys.sleep(.5)

hun_map[[i]] <- list(
  source = hun_counties[i, ],
  city_coord = remDr$getPageSource()[[1]] %>% 
  read_html() %>% 
  html_table() %>% 
  .[[1]] %>% 
  tibble()
)
}

hun_map <- hun_map %>% 
  lapply(function(x) {
    cbind(x$source, tibble(lat = x$city_coord[[2]][2], lng = x$city_coord[[3]][2]))
  }) %>% 
  reduce(rbind) %>% 
  filter(!duplicated(lat))

save(list = c("hun_map"), file = 
      paste0("C:/school/szem_8/TDK-airbnb/airbnb-research/hun_map.RData"))