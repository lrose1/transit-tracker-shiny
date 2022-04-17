# clean up code, separate data cleaning 

library(mapsapi)
library(tidyverse)
library(sf)
library(readxl)
library(geodist)

#### Import ####

#CA census tracts
ca_tracts <- read_sf("/Users/liamrose/OneDrive - Leland Stanford Junior University/Misc/transit/Transit/Map data/tl_2016_06_tract/tl_2016_06_tract.shp") %>% 
  st_transform()
bayareacounties <- ca_tracts %>% 
  filter(COUNTYFP == "081" | COUNTYFP == "085" | COUNTYFP == "075" |
           COUNTYFP == "013" | COUNTYFP == "001")

# import RUCA data
ruca2010revised <- read_excel("Map data/ruca2010revised.xlsx", 
                              skip = 1, col_names = T)

#### Clean ####

# merge
merge1 <- bayareacounties %>% 
  left_join(ruca2010revised, 
            by = c("GEOID" = "State-County-Tract FIPS Code (lookup by address at http://www.ffiec.gov/Geocode/)"))
# get rid of low density tracts
quantile(merge1$`Population Density (per square mile), 2010`, na.rm =T, c(0,.1,.2,.3))

working <- merge1 %>% 
  filter(`Primary RUCA Code 2010` == 1) %>% 
  filter(`Population Density (per square mile), 2010` >=3500)

# centroids, then get distance matrix
working2 <- working %>% 
  mutate(centroid = st_centroid(x = working["GEOID"]))
shp_centroid <- st_centroid(x = working2["GEOID"])

distmatrix <- geodist(st_coordinates(working2$centroid))
# make it long
dimnames(distmatrix) <- dimnames(distmatrix) <- list(working2$GEOID, working2$GEOID) 
mydistances <- data.frame(GEOID1=colnames(distmatrix)[col(distmatrix)], GEOID2=rownames(distmatrix)[row(distmatrix)], dist=c(distmatrix))

# only within 10 miles
within10 <- mydistances %>% 
  filter(dist <= 16000) %>% 
  filter(dist != 0)

# merge back original sample
working3 <- within10 %>% 
  left_join(working2 %>% 
              select(GEOID, geometry), 
            by = c("GEOID1" = "GEOID")) %>% 
  st_as_sf(sf_column_name = "geometry")

# sample points in the set
# small 
testsample <- working3 %>% 
  select(-GEOID2, -dist) %>% 
  unique() 
howmanytimes = 1
samples <- st_sample(testsample ,size=rep(howmanytimes,nrow(testsample)),type = "random",
                     by_polygon=T) %>% 
  st_sf() %>% 
  mutate(GEOID1 = rep(testsample$GEOID1, each=howmanytimes))

# pull all together
look1 <- working3 %>% 
  left_join(as.data.frame(samples) %>% rename(geometry1 = geometry)) %>% 
  left_join(as.data.frame(samples) %>% rename(geometry2 = geometry), by = c("GEOID2" = "GEOID1")) 
write_csv(look1, "Created Data/points_to_use.csv")