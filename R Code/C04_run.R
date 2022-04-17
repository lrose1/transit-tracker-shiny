# Take that days sample and split into samples to run every few minutes
library(mapsapi)
library(tidyverse)
library(sf)
library(readxl)
library(geodist)
library(later)



the_call <- function(){
  # small sample
  tiny_samp <- morning %>% sample_n(10)
  
  tes <- split(tiny_samp, seq(nrow(tiny_samp)))
  
  # Then plug that into google api
  key = "AIzaSyA42KMTo3MwHoIKPYibkwdGIgUtB90ffl4"
  go_points <- lapply(tes,function(x){
    doc = mp_directions(
      origin = x$geometry1[1],
      destination = x$geometry2[1],
      mode = c("driving"),
      alternatives = FALSE,
      key = key,
      quiet = FALSE,
      departure_time = Sys.time()
    )
    r = mp_get_routes(doc)
    
    doc2 = mp_directions(
      origin = x$geometry1[1],
      destination = x$geometry2[1],
      mode = c("transit"),
      alternatives = FALSE,
      key = key,
      quiet = TRUE
    )
    r2 = mp_get_routes(doc2)
    
    # put together
    test1 <- data.frame(r) %>%
      mutate(mode = "driving") %>% 
      bind_rows(r2 %>%   mutate(mode = "transit")) %>% 
      mutate(time = Sys.time()) %>% 
      mutate(origin = x$GEOID1[1]) %>% 
      mutate(destination = x$GEOID2[1]) %>% 
      select(-geometry)
    
  })
  
  together <- bind_rows(go_points)
  write_csv(together,paste0("Result/call data/csv",format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))
  
}

print_time = function(interval = 300) {
  the_call()
  later::later(print_time, interval)
}
print_time()