library(mapsapi)
library(tidyverse)
library(sf)
library(readxl)
library(geodist)

# Import CA census tracts
ca_tracts <- read_sf("/Users/Liam/OneDrive - Stanford/Misc/transit/Transit/Map data/tl_2016_06_tract/tl_2016_06_tract.shp") %>% 
  st_transform()
bayareacounties <- ca_tracts %>% 
  filter(COUNTYFP == "081" | COUNTYFP == "085" | COUNTYFP == "075" |
           COUNTYFP == "013" | COUNTYFP == "001")

# import RUCA data
ruca2010revised <- read_excel("Map data/ruca2010revised.xlsx", 
                              skip = 1, col_names = T)


# merge
working <- bayareacounties %>% 
  left_join(ruca2010revised, 
            by = c("GEOID" = "State-County-Tract FIPS Code (lookup by address at http://www.ffiec.gov/Geocode/)"))
# take out low density tracts
quantile(working$`Population Density (per square mile), 2010`, na.rm =T, c(0,.1,.2,.3))
working2 <- working %>% 
  filter(`Primary RUCA Code 2010` == 1) %>% 
  filter(`Population Density (per square mile), 2010` >=3500)

# working2 %>% 
#   ggplot(.) +
#   geom_sf()

# first find census tracts within X meters, then pick two random points from those
# two tracts

# centroids
working2 <- working2 %>% 
  mutate(centroid = st_centroid(x = working2["GEOID"]))
shp_centroid <- st_centroid(x = working2["GEOID"])

# distances between matrix
test <- geodist(st_coordinates(working2$centroid))
dimnames(test) <- dimnames(test) <- list(working2$GEOID, working2$GEOID) 
mydistances <- data.frame(GEOID1=colnames(test)[col(test)], GEOID2=rownames(test)[row(test)], dist=c(test))

# subset to within desired distance
within5 <- mydistances %>% 
  filter(dist <= 16100) %>% 
  filter(dist != 0)

# merge back original sample
working3 <- within5 %>% 
  left_join(working2 %>% 
              select(GEOID, geometry), 
            by = c("GEOID1" = "GEOID")) %>% 
  st_as_sf(sf_column_name = "geometry")

# small 
testsample <- working3 %>% 
  select(-GEOID2, -dist) %>% 
  unique() 

# census tracts within 10 miles of each other. 
saveRDS(testsample, "/Users/Liam/OneDrive - Stanford/Misc/transit/Transit/Map data/testsample")
saveRDS(working3, "/Users/Liam/OneDrive - Stanford/Misc/transit/Transit/Map data/working3")
