library(rvest)
library(tidyverse)
library(parallel)

cities <- readxl::read_excel("C:/school/szem_8/TDK-airbnb/airbnb-research/cities.xlsx") %>% 
  mutate(
    URL = str_replace_all(URL, 'airbnb.hu', 'airbnb.com')
  )

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

for (i in 1:nrow(RecPri_df)) {
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
        tryCatch(
        read_html(url) %>%
          html_nodes("._1snxcqc") %>%
          html_text() %>% 
          as.character(),
          error = function(e) NA)
      }
      )
      
      df <- rbind(df, na.omit(df_still))
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

save(list = c("cities"), file = "C:/school/szem_8/TDK-airbnb/airbnb-research/room_list/initial_list.RData")
