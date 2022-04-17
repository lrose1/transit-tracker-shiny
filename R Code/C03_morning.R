# This takes the static data and creates random points within tracts every morning

library(mapsapi)
library(tidyverse)
library(sf)
library(readxl)
library(geodist)

my_tracts <- readRDS("/Users/Liam/OneDrive - Stanford/Misc/transit/Transit/Map data/testsample")
working3 <- readRDS("/Users/Liam/OneDrive - Stanford/Misc/transit/Transit/Map data/working3")


# sample points in the set
howmanytimes = 2
samples <- st_sample(my_tracts ,size=rep(howmanytimes,nrow(my_tracts)),type = "random",
                     by_polygon=T) %>% 
  st_sf() %>% 
  mutate(GEOID1 = rep(my_tracts$GEOID1, each=howmanytimes))
# plot(samples)
morning <- working3 %>% 
  left_join(as.data.frame(samples) %>% rename(geometry1 = geometry)) %>% 
  left_join(as.data.frame(samples) %>% rename(geometry2 = geometry), by = c("GEOID2" = "GEOID1")) 

saveRDS(morning, "/Users/Liam/OneDrive - Stanford/Misc/transit/Transit/Map data/morning")
# write_csv(morning, "/Users/Liam/OneDrive - Stanford/Misc/transit/Transit/Map data/morning.csv")

rm(list= ls()[! (ls() %in% c('morning'))])
gc()

