library(rvest)
library(tidyverse)
library(parallel)

cities <- readxl::read_excel("cities.xlsx") %>% 
  mutate(
    URL = str_replace_all(URL, 'airbnb.hu', 'airbnb.com')
  )

# TODO dates

cities <- cities %>%
  apply(1, function(x) {
    n_rooms <- read_html(x[2]) %>%
      html_nodes("._1snxcqc") %>%
      html_text() %>%
      {gsub(' .*', '', .)}
    data.frame(city = x[1], url = x[2], n_rooms = n_rooms) 
  }
  ) %>%
  reduce(rbind)

RecPri_df <- cities %>%
  filter(n_rooms == 'Több' | n_rooms == '300+' | n_rooms == '300' )

for (i in 1:nrow(RecPri_df)) { # TODO parallel
  run <-  T
  df <- seq(from = 10, to = 1500, length.out = 5) %>%
    floor() %>%
    {na.omit(data.frame(p1 = ., p2 = lead(.)))} %>%
    mutate(
      url = paste0(RecPri_df[i, 2], '&price_max=', p2, '&price_min=', p1)
    ) 
  
  df$n_rooms <-  sapply(df$url, function(url) {
    read_html(url) %>%
      html_nodes("._1snxcqc") %>%
      html_text() 
  }
  )
  
  while (run) {
    
    df_still <- df %>% 
      filter(str_detect(n_rooms, "300"))
    
    df <- df %>% 
      filter(!str_detect(n_rooms, "300"))
    
    if (nrow(df_still) > 0) {
      
      
      df_still <- apply(df_still, 1, function(x) {
        seq(from = x[1], to = x[2], length.out = 3) %>%
          floor() %>%
          {na.omit(data.frame(p1 = ., p2 = lead(.)))} %>%
          mutate(
            url = paste0(RecPri_df[i, 2], '&price_max=', p2, '&price_min=', p1)
          ) 
      }
      ) %>% 
        reduce(rbind)
      
      df_still$n_rooms <-  sapply(df_still$url, function(url) {
        read_html(url) %>%
          html_nodes("._1snxcqc") %>%
          html_text() %>% 
          as.character()
      }
      )
      
      df <- rbind(df, df_still)
      print(df)
    } else {
      run <- F
      cities <- df %>% 
        mutate(city = RecPri_df[i, 1]) %>% 
        select(city, url, n_rooms) %>% 
        rbind(cities)
    }
  }
}

cities <- cities %>% 
  filter(!str_detect(n_rooms, 'Több') & !str_detect(n_rooms, '300'))

cl <- makeCluster(7)
clusterExport(cl, list("cities"), envir = environment())
clusterEvalQ(cl, library(rvest))
clusterEvalQ(cl, library(tidyverse))

all_source <- parApply(cl = cl,cities, 1, function(x) {
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
    URL = page %>%
      html_nodes('._gjfol0') %>%
      html_attr('href') %>% 
      {paste0('https://www.airbnb.com', .)}
    price = page %>%
      html_nodes('span._olc9rf0') %>%
      html_text() %>% 
      .[1:length(URL)]
    place = page %>%
      html_nodes('._b14dlit') %>%
      html_text() %>% 
      .[1:length(URL)]
    data.frame(
      city = x[1],
      title = page %>%
        html_nodes('._gjfol0') %>%
        html_attr('aria-label'),
      URL, price, place
    )
  }, error = function(e) NULL)
})

stopCluster(cl)

room_list <- reduce(Filter(f = Negate(is.null), room_list), rbind)
tibble(room_list)

save(list = c("room_list"), file = "room_list2.RData")