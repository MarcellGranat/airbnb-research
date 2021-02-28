cities <- readxl::read_excel("cities.xlsx")

cities <- cities %>% 
  .[1:5, ] %>% 
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
  filter(n_rooms == 'Több' | n_rooms == '300+' | n_rooms == '300')

for (i in 1:length(RecPri_df)) {
  run <-  T
  k <- 5
  while (run) {
    urls <- seq(from = 10, to = 1500, length.out = k) %>% 
      floor() %>% 
      {na.omit(data.frame(p1 = ., p2 = lead(.)))} %>% 
      mutate(
        url = paste0(RecPri_df[i, 2], '&price_max=', p2, '&price_min=', p1)
      ) %>% 
      pull(url)
    n_rooms <- urls %>% 
      sapply(function(url) {
        read_html(url) %>% 
          html_nodes("._1snxcqc") %>% 
          html_text() %>% 
          {gsub(' .*', '', .)}
      }
      )
    cat(RecPri_df[i, 1], '-', 'k =', k, '===>', n_rooms, '\n')
    if (!('Több' %in% n_rooms | '300' %in% n_rooms | '300+' %in% n_rooms)) {
      run <- F
      tibble(city = RecPri_df[i, 1], url = urls, n_rooms = n_rooms)
    }
  }
}


https://www.airbnb.hu/s/Budapest/homes?tab_id=home_tab&refinement_paths%5B%5D=%2Fhomes&date_picker_type=calendar&source=structured_search_input_header&search_type=filter_change&flexible_trip_dates%5B%5D=april&flexible_trip_dates%5B%5D=march&flexible_trip_lengths%5B%5D=weekend_trip&checkin=2021-07-01&checkout=2021-07-04&price_max=15&place_id=ChIJyc_U0TTDQUcRYBEeDCnEAAQ&price_min=11