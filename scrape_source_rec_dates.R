library(rvest)
library(tidyverse)
library(parallel)

load("C:/school/szem_8/TDK-airbnb/airbnb-research/room_list/initial_list.RData")

start_dates <- c(seq(from = 1, to = 89, by = 7), seq(from = 5, to = 92, by = 7)) %>% 
  sort() 

for (i in start_dates) {
    cities_current <- cities %>% 
      mutate(
        url = str_replace_all(url, '2021-07-01', as.character(as.Date(i, origin = '2021-05-30'))),
        url = str_replace_all(url, '2021-07-04', as.character(as.Date(ifelse(i %% 7 == 1, i + 3, i + 2), origin = '2021-05-30')))
      )

cl <- makeCluster(7)
clusterExport(cl, list("cities"), envir = environment())
clusterEvalQ(cl, library(rvest))
clusterEvalQ(cl, library(tidyverse))

all_source <- parApply(cl = cl, cities_current, 1, function(x) {
  page <- read_html(x[2])
  n_rooms <- page %>% 
    html_nodes("._1snxcqc") %>%
    html_text() %>% 
    {gsub(" s.*", "", .)} %>% 
    as.numeric()
  
  df <- data.frame(city = x[1], url = x[2])
  
  if (!is.na(n_rooms) && n_rooms > 20) {
    
    df <- data.frame(
      city = x[1],
      url = html_nodes(page, xpath = '/html/body/div[4]/div/div/div/div[1]/main/div/div/div[1]/div[2]/div/div/div[1]/nav/div/a[1]') %>% 
        html_attr('href') %>% 
        {paste0('https://www.airbnb.com', .)},
      v = seq(from = 20, to = (n_rooms %/% 20)*20, by = 20)
    ) %>% 
      mutate(
        url = str_replace(url, 'items_offset=20', paste0('items_offset=', v))
      ) %>% 
      select(-v) %>% 
      rbind(df)
  }
  df
}) 

all_source <- reduce(all_source, rbind)

room_list <- parApply(cl = cl, all_source, 1, function(x) {
  tryCatch({
    page <- read_html(x[2]) 
    URL <- page %>%
      html_nodes('._gjfol0') %>%
      html_attr('href') %>% 
      {paste0('https://www.airbnb.com', .)}
    price <- page %>%
      html_nodes('span._olc9rf0') %>%
      html_text() %>% 
      .[1:length(URL)]
    place <- page %>%
      html_nodes('._b14dlit') %>%
      html_text() %>% 
      .[1:length(URL)]
    assesment <- page %>%
      html_nodes('._18khxk1') %>%
      html_text() %>% 
      .[1:length(URL)]
    data.frame(
      city = x[1],
      title = page %>%
        html_nodes('._gjfol0') %>%
        html_attr('aria-label'),
      URL, price, place, assesment
    )
  }, error = function(e) NULL)
})

stopCluster(cl)

room_list <- reduce(Filter(f = Negate(is.null), room_list), rbind)
tibble(room_list)

save(list = c("room_list"), file = paste0('C:/school/szem_8/TDK-airbnb/airbnb-research/room_list/room_list', i, '.RData'))
}

