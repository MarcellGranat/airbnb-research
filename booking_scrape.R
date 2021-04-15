library(tidyverse)
library(rvest)
library(parallel)

cl <- makeCluster(6)
clusterEvalQ(cl, library(rvest))
clusterEvalQ(cl, library(tidyverse))

price_min <- c(0,20, seq(from = 40, to = 120, by = 5), seq(from = 140, to = 280, by = 20))
price_max <- as.character(price_min[-1] - 1)
price_max[length(price_max) + 1] <- 'max'

source <- paste0('https://www.booking.com/searchresults.html?label=gen173nr-1FCAEoggI46AdIM1gEaGeIAQGYATG4ARfIAQzYAQHoAQH4AQKIAgGoAgO4AtrinIMGwAIB0gIkZmI5NWFjZWYtMDY5My00ZWQzLTljZmQtMmM5NDZiNzhiY2Jl2AIF4AIB&sid=cb2934811e3d4874f836f9b28d3a7f0e&sb=1&src=searchresults&src_elem=sb&error_url=https%3A%2F%2Fwww.booking.com%2Fsearchresults.html%3Flabel%3Dgen173nr-1FCAEoggI46AdIM1gEaGeIAQGYATG4ARfIAQzYAQHoAQH4AQKIAgGoAgO4AtrinIMGwAIB0gIkZmI5NWFjZWYtMDY5My00ZWQzLTljZmQtMmM5NDZiNzhiY2Jl2AIF4AIB%3Bsid%3Dcb2934811e3d4874f836f9b28d3a7f0e%3Btmpl%3Dsearchresults%3Bcheckin_month%3D6%3Bcheckin_monthday%3D1%3Bcheckin_year%3D2021%3Bcheckout_month%3D6%3Bcheckout_monthday%3D4%3Bcheckout_year%3D2021%3Bclass_interval%3D1%3Bdest_id%3D96%3Bdest_type%3Dcountry%3Bfrom_sf%3D1%3Bgroup_adults%3D1%3Bgroup_children%3D0%3Blabel_click%3Dundef%3Bno_rooms%3D1%3Boffset%3D0%3Braw_dest_type%3Dcountry%3Broom1%3DA%3Bsb_price_type%3Dtotal%3Bshw_aparth%3D1%3Bslp_r_match%3D0%3Bsrc%3Dsearchresults%3Bsrc_elem%3Dsb%3Bsrpvid%3D49fa6aba57b20053%3Bss%3DHungary%3Bssb%3Dempty%3Bssne%3DHungary%3Bssne_untouched%3DHungary%3Btop_ufis%3D1%26%3B&ss=Hungary&is_ski_area=0&ssne=Hungary&ssne_untouched=Hungary&dest_id=96&dest_type=country&checkin_year=2021&checkin_month=6&checkin_monthday=1&checkout_year=2021&checkout_month=6&checkout_monthday=2&group_adults=1&group_children=0&no_rooms=1&sb_with_filters=1&from_sf=1&nflt=price%3DUSD-', price_min, '-', price_max, '-1%3B&offset=')

URLs <- parLapply(cl = cl, source, function(x) {
read_html(x) %>% 
  html_nodes('#search_results_table > div.bui-pagination.results-paging_simplified.js-results-paging > nav > ul') %>% 
  html_text() %>% 
  str_replace_all('\\D', ' ') %>% 
  str_split(' ') %>% 
  .[[1]] %>% 
  as.numeric() %>% 
  max(na.rm = T) %>% 
  {paste0(x, c('', as.character(seq(.-1)*25)))}
}) %>% 
  reduce(c)

booking_prices <- parLapply(cl = cl, URLs, function(x) {
read_html(x) %>% 
  html_nodes('div.bui-price-display__value.prco-inline-block-maker-helper') %>% 
  html_text() %>% 
  str_remove_all('\\D') %>% 
  as.numeric()
}) %>% 
  reduce(c)

stopCluster(cl)

head(booking_prices)

save(list = c('booking_prices'), file = 'booking_prices.RData')

