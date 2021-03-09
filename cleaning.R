dat <- list()
for (i in 1:138) {
  load(paste0("C:/school/szem_8/TDK-airbnb/airbnb-research/room_rawdata/raw_dat", i*50 ,".RData"))
  dat <- c(dat, raw_dat)
}

save(list = c("dat"), file = "C:/school/szem_8/TDK-airbnb/airbnb-research/dat.RData")

sapply(dat, function(x) length(x$comments) == 0) %>% 
  mean()

