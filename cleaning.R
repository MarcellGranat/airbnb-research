dat <- list()
for (i in 1:156) {
  load(paste0("C:/school/szem_8/TDK-airbnb/airbnb-research/room_rawdata/raw_dat", i*50 ,".RData"))
  dat <- c(dat, raw_dat)
}

save(list = c("dat"), file = "C:/school/szem_8/TDK-airbnb/airbnb-research/dat.RData")

sapply(dat, function(x) length(x$comments) == 0) %>% 
  mean()

load("C:/school/szem_8/TDK-airbnb/airbnb-research/dat_cleaned.RData")

library(parallel)

cl <- makeCluster(7)
clusterExport(cl, list("dat"), envir = environment())
clusterEvalQ(cl, library(rvest))
clusterEvalQ(cl, library(tidyverse))

dat_words <- parLapply(cl = cl, dat, function(x) { 
    tryCatch({
      tibble(comments = x$comments) %>% 
        mutate(language = textcat::textcat(comments)) %>% 
        tail(-2) %>% 
        tidytext::unnest_tokens(words, comments, to_lower = T) %>% 
        mutate(id = x[["source"]][["id"]])
    }, error = function(e) NULL)
  }
  ) %>% 
  {reduce(Filter(f = Negate(is.null), .), rbind)}

stopCluster(cl)

save(list = c("dat_words"), file = 'C:/school/szem_8/TDK-airbnb/airbnb-research/dat_words.RData')

