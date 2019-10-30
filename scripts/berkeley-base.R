###################################################
## water access project in Berkeley County, SC ##
###################################################

## import libraries
library(tidyverse)
library(tidycensus)
options(tigris_use_cache = TRUE)
library(sf)
library(tmap)

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/berkeley-county')

## define census variables
alb <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" ## http://spatialreference.org/ref/sr-org/albers-conic-equal-area-for-florida-and-georgia/
bg <- NULL
YR <- 2017
ST <- c('SC', 'NC')
var = c(white = "B03002_003E", black = "B03002_004E",
        native_american = "B03002_005E", asian = "B03002_006E",
        hawaiian = "B03002_007E", other = "B03002_008E",
        multiracial = "B03002_009E", latinx = "B03002_012E", total = "B03002_001E",
        medhhinc = "B19049_001E", agghhinc = "B19025_001E", hu = "B25001_001E")

##import Berkeley data
wtr_main <- st_read(file.path(datadir, '/spatial-data/wtrMain'))
wtr_conn <- st_read(file.path(datadir, '/spatial-data/wtrConnections'))
dist7 <- st_read(file.path(datadir, '/spatial-data/district_7')) %>%
  st_transform(alb)

## download census variables for selected states
bg <- get_acs(geography = "block group",
                 variables = var,
                 state = 'SC',
                 county = 'Berkeley',
                 year = YR,
                 output = 'wide',
                 geometry = TRUE,
                 keep_geo_vars = TRUE)

bg2 <- bg %>%
  st_sf() %>%
  st_transform(alb) %>%
  mutate(sqkm_bg = as.numeric(st_area(geometry)) / 1e6, mnhhinc = agghhinc/hu,
         propPOC = 1 - (white/total)) %>%
  dplyr::select(GEOID, ALAND, AWATER, sqkm_bg, total, white, black, native_american, asian, hawaiian,
                other, multiracial, latinx, propPOC, medhhinc, agghhinc, hu, mnhhinc)

## export census data
bg2 %>% st_transform(crs = 4326) %>%
  st_write(file.path(datadir, "bg_data.geojson"), driver = 'geojson')

tm_shape(bg2) + 
  tm_polygons(col = 'propPOC') + 
tm_shape(dist7) + 
  tm_borders(col = 'black', lw = 2) + 
tm_shape(wtr_main) + 
  tm_lines(col = 'darkblue') 



###################################################
## apply proportional area adjustment to variables
## to assess count within area(s) of interest
###################################################

## define intersection between cons area ben zones and block groups
int <- as_tibble(st_intersection(dist7, bg2))

## proportional area adjustment/allocation method
percBGinBZ <- int %>%
  mutate(sqkm_bginbz = as.numeric(st_area(geometry) / 1e6)) %>%
  mutate(perc_bginbz = (sqkm_bginbz/sqkm_bg), sqkm_land = ALAND/ 1e6)

## save percBGinBUF data to use in median HH income estimation (see below)
bg_for_emed <- percBGinBZ %>%
  data.frame() %>%
  mutate(GEOID = as.character(GEOID)) %>%
  select(rowid, GEOID, perc_bginbz) %>%
  rename(BZID = rowid)

## demographic analysis of cons area ben zones
bz_geog <- percBGinBZ %>%
  mutate(tot_pop = total * perc_bginbz,
         white = white * perc_bginbz, 
         black = black * perc_bginbz,
         other = (native_american+asian+hawaiian+other+multiracial) * perc_bginbz,
         #other = multiracial * perc_bginbz,
         latinx = latinx * perc_bginbz,
         hu = hu * perc_bginbz,
         sqkm_land = sqkm_land * perc_bginbz) %>%
  mutate(agghhinc = hu * mnhhinc) %>%
  group_by(rowid) %>% ## regroups to cons areas after demo analysis on intersections
  summarise(tot_pop = sum(tot_pop), white = sum(white), black = sum(black), 
            other = sum(other), latinx = sum(latinx), 
            hu = round(sum(hu, na.rm = TRUE), 0), agghhinc = sum(agghhinc, na.rm = TRUE),
            sqkm_bz = mean(sqkm_bz), sqkm_land = sum(sqkm_land), bzone_m = mean(bzone_m)) %>%
  mutate(pwhite = round(white/tot_pop, 2), pblack = round(black/tot_pop, 2), pother = round(other/tot_pop, 2), 
         platinx = round(latinx/tot_pop, 2), popden = round(tot_pop/sqkm_land, 2), propPOC = round(1 - pwhite, 2),
         mnhhinc = round(agghhinc/hu, 0), pland = round((sqkm_land)/sqkm_bz, 2)) %>%
  merge(cons, by = 'rowid') %>%
  dplyr::select(rowid, conscat, bzone_m, buf_m, tot_pop, popden, sqkm_bz, pland, pwhite, pblack, pother, platinx, propPOC, hu, mnhhinc, geometry) %>%
  st_as_sf()


qtm(dist7)

tm_shape(dist7) +
  tm_borders(col = 'black') + 
# tm_shape(wtr_main) + 
#   tm_lines(col = 'darkblue') + 
tm_shape(wtr_conn) + 
  tm_lines(col = 'blue')

