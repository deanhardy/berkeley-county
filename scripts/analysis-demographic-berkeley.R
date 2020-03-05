###################################################
## water access project in Berkeley County, SC ##
###################################################

rm(list=ls())

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
ST <- c('SC')
CNTY <- c('Berkeley')
var = c(white = "B03002_003E", black = "B03002_004E",
        native_american = "B03002_005E", asian = "B03002_006E",
        hawaiian = "B03002_007E", other = "B03002_008E",
        multiracial = "B03002_009E", latinx = "B03002_012E", total = "B03002_001E",
        medhhinc = "B19049_001E", agghhinc = "B19025_001E", hu = "B25001_001E")

##import areo of interest data
AOI <- st_read(file.path(datadir, '/spatial-data/district_7')) %>%
  st_transform(alb) %>%
  rowid_to_column() %>%
  mutate(sqkm_aoi = as.numeric(st_area(geometry) / 1e6))
wtr_main <- st_read(file.path(datadir, '/spatial-data/wtrMain')) %>% st_transform(4326)
wtr_conn <- st_read(file.path(datadir, '/spatial-data/wtrConnections')) %>% st_transform(4326)

## get county level data
cnty<- get_acs(geography = "county",
               variables = var,
               state = ST,
               county = CNTY,
               year = YR,
               output = 'wide',
               geometry = TRUE,
               keep_geo_vars = TRUE)

## transform and tidy census data
cnty2 <- cnty %>%
  st_transform(alb) %>%
  mutate(sqkm_bg = as.numeric(st_area(geometry)) / 1e6, mnhhinc = agghhinc/hu,
         propPOC = 1 - (white/total)) %>%
  dplyr::select(GEOID, ALAND, AWATER, sqkm_bg, total, white, black, native_american, asian, hawaiian,
                other, multiracial, latinx, propPOC, medhhinc, agghhinc, hu, mnhhinc) %>%
  st_transform(4326)

## throws error, but not sure why...
st_write(cnty2, file.path(datadir, 'berkely-cnty-demographics.shp'), 'ESRI Shapefile', delete_dsn = TRUE)

## download blockgroup census variables for selected states
bg <- get_acs(geography = "block group",
                 variables = var,
                 state = ST,
                 county = CNTY,
                 year = YR,
                 output = 'wide',
                 geometry = TRUE,
                 keep_geo_vars = TRUE)

## transform and tidy census data
bg2 <- bg %>%
  st_sf() %>%
  st_transform(alb) %>%
  mutate(sqkm_bg = as.numeric(st_area(geometry)) / 1e6, mnhhinc = agghhinc/hu,
         propPOC = 1 - (white/total)) %>%
  dplyr::select(GEOID, ALAND, AWATER, sqkm_bg, total, white, black, native_american, asian, hawaiian,
                other, multiracial, latinx, propPOC, medhhinc, agghhinc, hu, mnhhinc)

## export census data
bg2 %>% st_transform(crs = 4326) %>%
  st_write(file.path(datadir, "bg_data.geojson"), driver = 'geojson', delete_dsn = TRUE)

## quick map of census data
tm_shape(bg2) + 
  tm_polygons(col = 'propPOC') + 
tm_shape(AOI) + 
  tm_borders(col = 'black', lw = 2) 
# tm_shape(wtr_main) + 
#   tm_lines(col = 'darkblue') 


###################################################
## apply proportional area adjustment to variables
## to assess count within area(s) of interest
###################################################

## define intersection between AOI and block groups
int <- as_tibble(st_intersection(AOI, bg2))

## proportional area adjustment/allocation method
percBGinAOI <- int %>%
  mutate(sqkm_bginaoi = as.numeric(st_area(geometry) / 1e6)) %>%
  mutate(perc_bginaoi = (sqkm_bginaoi/sqkm_bg), sqkm_land = ALAND/ 1e6)

## save percBGinAOI data to use in median HH income estimation (see below)
bg_for_emed <- percBGinAOI %>%
  data.frame() %>%
  mutate(GEOID = as.character(GEOID)) %>%
  select(rowid, GEOID, perc_bginaoi) %>%
  rename(AOIID = rowid)

## demographic analysis of AOI
aoi_demo <- percBGinAOI %>%
  mutate(tot_pop = total * perc_bginaoi,
         white = white * perc_bginaoi, 
         black = black * perc_bginaoi,
         other = (native_american+asian+hawaiian+other+multiracial) * perc_bginaoi,
         #other = multiracial * perc_bginaoi,
         latinx = latinx * perc_bginaoi,
         hu = hu * perc_bginaoi,
         sqkm_land = sqkm_land * perc_bginaoi) %>%
  mutate(agghhinc = hu * mnhhinc) %>%
  group_by(rowid) %>% ## regroups to cons areas after demo analysis on intersections
  summarise(tot_pop = sum(tot_pop), white = sum(white), black = sum(black), 
            other = sum(other), latinx = sum(latinx), 
            hu = round(sum(hu, na.rm = TRUE), 0), agghhinc = sum(agghhinc, na.rm = TRUE),
            sqkm_aoi = mean(sqkm_aoi), sqkm_land = sum(sqkm_land)) %>%
  mutate(pwhite = round(white/tot_pop, 2), pblack = round(black/tot_pop, 2), pother = round(other/tot_pop, 2), 
         platinx = round(latinx/tot_pop, 2), popden = round(tot_pop/sqkm_land, 2), propPOC = round(1 - pwhite, 2),
         mnhhinc = round(agghhinc/hu, 0), pland = round((sqkm_land)/sqkm_aoi, 2)) %>%
  merge(AOI, by = 'rowid') %>%
  dplyr::select(rowid, tot_pop, popden, sqkm_aoi.x, pland, pwhite, pblack, pother, platinx, propPOC, hu, mnhhinc, geometry) %>%
  st_as_sf()


#########################################################
## estimate median household incomes within AOI(s)
#########################################################
## this section of code calculates an estimated median from grouped (aka binned) household income data 
## within an area that overlaps several block groups

## good explanation for how this works mathematically and upon which the function below is based
## https://www.mathsisfun.com/data/frequency-grouped-mean-median-mode.html

## download hh income distribution tables for block groups & label bins

gm <- NULL # used in for loop for calculating gmedian

for(i in 1:length(CNTY)) {
  OUT <- get_acs(geography = "block group",
                 table = 'B19001',
                 state = ST,
                 county = CNTY[[i]],
                 year = YR) %>%
    select(-NAME, -moe) %>%
    rename(households = estimate) %>%
    filter(variable != 'B19001_001') %>%
    mutate(bin_min = ifelse(variable == 'B19001_002', 0, 
                            ifelse(variable == 'B19001_003', 10000, 
                                   ifelse(variable == 'B19001_004', 15000,
                                          ifelse(variable == 'B19001_005', 20000,
                                                 ifelse(variable == 'B19001_006', 25000,
                                                        ifelse(variable == 'B19001_007', 30000,
                                                               ifelse(variable == 'B19001_008', 35000,
                                                                      ifelse(variable == 'B19001_009', 40000,
                                                                             ifelse(variable == 'B19001_010', 45000,
                                                                                    ifelse(variable == 'B19001_011', 50000,
                                                                                           ifelse(variable == 'B19001_012', 60000,
                                                                                                  ifelse(variable == 'B19001_013', 75000,
                                                                                                         ifelse(variable == 'B19001_014', 100000,
                                                                                                                ifelse(variable == 'B19001_015', 125000,
                                                                                                                       ifelse(variable == 'B19001_016', 150000,
                                                                                                                              ifelse(variable == 'B19001_017', 200000, variable)))))))))))))))),
           bin_max = ifelse(variable == 'B19001_002', 9999, 
                            ifelse(variable == 'B19001_003', 14999, 
                                   ifelse(variable == 'B19001_004', 19999,
                                          ifelse(variable == 'B19001_005', 24999,
                                                 ifelse(variable == 'B19001_006', 29999,
                                                        ifelse(variable == 'B19001_007', 34999,
                                                               ifelse(variable == 'B19001_008', 39999,
                                                                      ifelse(variable == 'B19001_009', 44999,
                                                                             ifelse(variable == 'B19001_010', 49999,
                                                                                    ifelse(variable == 'B19001_011', 59999,
                                                                                           ifelse(variable == 'B19001_012', 74999,
                                                                                                  ifelse(variable == 'B19001_013', 99999,
                                                                                                         ifelse(variable == 'B19001_014', 124999,
                                                                                                                ifelse(variable == 'B19001_015', 149999,
                                                                                                                       ifelse(variable == 'B19001_016', 199999,
                                                                                                                              ifelse(variable == 'B19001_017', NA, variable))))))))))))))))) %>%
    mutate(interval = paste(bin_min, bin_max, sep = "-"))
  
  gm <- rbind(gm, OUT)
}

## define function following stackoverflow post
# https://stackoverflow.com/questions/18887382/how-to-calculate-the-median-on-grouped-dataset
## but revised per variables from 
# https://www.mathsisfun.com/data/frequency-grouped-mean-median-mode.html
## B modified to account for when median group is the 0-9999 bin
GMedian <- function(frequencies, intervals, sep = NULL, trim = NULL) {
  # If "sep" is specified, the function will try to create the 
  #   required "intervals" matrix. "trim" removes any unwanted 
  #   characters before attempting to convert the ranges to numeric.
  if (!is.null(sep)) {
    if (is.null(trim)) pattern <- ""
    else if (trim == "cut") pattern <- "\\[|\\]|\\(|\\)"
    else pattern <- trim
    intervals <- sapply(strsplit(gsub(pattern, "", intervals), sep), as.numeric)
  }
  
  cf <- cumsum(frequencies)
  Midrow <- findInterval(max(cf)/2, cf) + 1
  L <- intervals[1, Midrow]      # lower class boundary of the group containing the median 
  w <- diff(intervals[, Midrow]) # width of median class
  G <- frequencies[Midrow]       # frequency of median class
  B <- ifelse(Midrow > 1, cf[Midrow - 1], as.vector(0))  # cumulative frequency of the groups before median group
  n_2 <- max(cf)/2               # total observations divided by 2
  
  unname(L + (n_2 - B)/G * w)
}

gm2 <- gm %>%
  left_join(bg_for_emed, by = "GEOID") %>%
  filter(perc_bginaoi != 'NA') %>%
  mutate(eHH = households * perc_bginaoi) %>%
  group_by(AOIID, variable) %>%
  summarise(interval = interval[[1]], eHH = sum(eHH), households = sum(households)) %>%
  summarise(gmedian = GMedian(eHH, interval, sep = "-", trim = 'cut'))

## import gmedian estimates for hh income
emed <- gm2 %>%
  rename(rowid = AOIID, emedhhinc = gmedian) %>%
  mutate(emedhhinc = round(emedhhinc, 0))

## merge emedian hh income with other demographic data
df <- aoi_demo %>% 
  merge(emed, by = "rowid") %>%
  mutate(tot_pop = round(tot_pop, 0), 
         popden = round(popden, 1),
         sqkm_aoi.x = round(sqkm_aoi.x, 0),
         pwhite = round(pwhite*100, 0),
         propPOC = round(propPOC*100, 0)) %>%
  st_transform(4326) 

##############################
## export data
##############################

## export AOI as polygons
df %>% st_write(file.path(datadir, 'aoi.geojson'), driver = 'geojson', delete_dsn = TRUE)

## export ONLY attribute data
df %>%
  st_set_geometry(NULL) %>%
  write.csv(file.path(datadir, 'aoi_data.csv'), row.names = FALSE)

###################################
## make table for AOI demographics
###################################
# library(expss) ## https://cran.r-project.org/web/packages/expss/vignettes/tables-with-labels.html
# 
# df = apply_labels(df,
#                   tot_pop = 'Total Population',
#                   popden = 'Population Density (per km^2)',
#                   pwhite = 'Proportion White',
#                   pblack = 'Proportion Black',
#                   emedhhinc = 'Estimated Median Household Income',
#                   rowid = 'Row ID'
# )
# 
# df %>% 
#   tab_cells(tot_pop, popden, pwhite, pblack, emedhhinc) %>%
#   tab_cols(total(), rowid) %>%
#   tab_stat_cpct() %>% 
#   tab_pivot()

library(data.table)
library(formattable)

df %>%
  as.data.table() %>%
  select(tot_pop:emedhhinc) %>%
  select(-pland, -pblack, -pother, -platinx) %>%
  rename(., 
         'Area (km^2)' = sqkm_aoi.x,
         'Total Population' = tot_pop, 
         'Population Density (km^2)' = popden, 
         'White (%)' = pwhite,
         'People of Color (%)' = propPOC,
         'Housing Units (#)' = hu,
         'Mean Household Income ($)' = mnhhinc,
         'Estimated Median Household Income ($)' = emedhhinc) %>%
  formattable()



#############################################
## create maps
#############################################
library(leaflet)
library(leaflet.extras)
library(sf)

AOI <- AOI %>% st_transform(4326)

# all_vars <- load_variables(2016, 'acs5', cache = TRUE)
var = c(white = "B03002_003E", black = "B03002_004E",
        native_american = "B03002_005E", asian = "B03002_006E",
        hawaiian = "B03002_007E", other = "B03002_008E",
        multiracial = "B03002_009E", latinx = "B03002_012E", tot_pop = "B03002_001E",
        medhhinc = "B19049_001E", agghhinc = "B19025_001E", hu = "B25001_001E", ownocc = "B25003_002E")
CNTY = c('Berkeley')

dem <- get_acs(geography = 'block group',
               variables = var,
               state = 'SC',
               county = CNTY,
               year = 2017,
               output = 'wide',
               geometry = TRUE,
               keep_geo_vars = TRUE)

dem2 <- dem %>%
  st_sf() %>%
  st_transform(alb) %>%
  mutate(sqkm_bg = as.numeric(st_area(geometry)) / 1e6,
         propPOC = 1 - (white/tot_pop)) %>%
  dplyr::select(GEOID, ALAND, AWATER, sqkm_bg, tot_pop, white, black, native_american, asian, hawaiian,
                other, multiracial, latinx, propPOC, medhhinc, agghhinc, hu, ownocc) %>%
  mutate(pwhite = round(white/tot_pop, 2), pblack = round(black/tot_pop, 2), pother = round(other/tot_pop, 2), 
         platinx = round(latinx/tot_pop, 2), popden = round(tot_pop/ALAND, 2),
         pland = round((ALAND * 0.000001)/sqkm_bg, 2), pownocc = round(ownocc/hu, 2)) %>%
  st_transform(4326)

st_write(dem2, file.path(datadir, 'berkeley-bg-demographics.shp'), 'ESRI shapefile')

# factpal <- colorFactor(rainbow(8), buf$id)
bpal <- colorBin('Reds', dem2$medhhinc, 5, pretty = FALSE)
bpal2 <- colorBin('Greens', 100*dem2$propPOC, 5, pretty = FALSE)
bpal3 <- colorBin('Blues', 100*dem2$pownocc, 5, pretty = FALSE)
pops <- paste("People of Color (%):", round(100*dem2$propPOC, 0), "<br>",
              "Black (%):", 100*dem2$pblack, "<br>",
              "Other race (%):", 100*dem2$pother, "<br>",
              "Latinx (%):", 100*dem2$platinx, "<br>",
              "White (%):", 100*dem2$pwhite, "<br>",
              "Median HH Income (US$):", round(dem2$medhhinc, 0), "<br>",
              "Housing Units (#):", dem2$hu, "<br>",
              "Owner-Occupied HU (%):", 100*dem2$pownocc)

m <- leaflet() %>%
  addTiles(group = "Open Street Map") %>%  
  addTiles(attribution = '<a href="https://www.census.gov/programs-surveys/acs/"> | US Census American Community Survey 2013-2017</a>') %>%
  # addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
  setView(lng = -80, lat = 33.2, zoom = 10) %>%
  addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2)) %>%
  addPolygons(data = dem2,
              group = 'Median Household Income',
              fillColor = ~bpal(dem2$medhhinc),
              color = 'grey',
              weight = 1,
              fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "yellow", weight = 2,bringToFront = TRUE),
              popup = pops) %>%
  addPolygons(data = dem2,
              group = 'People of Color',
              fillColor = ~bpal2(100*dem2$propPOC),
              fillOpacity = 0.5,
              color = 'grey',
              weight = 1,
              highlightOptions = highlightOptions(color = "yellow", weight = 2,bringToFront = TRUE),
              popup = pops) %>%
  addPolygons(data = dem2,
              group = 'Owner Occupied Housing',
              fillColor = ~bpal3(100*dem2$pownocc),
              fillOpacity = 0.5,
              color = 'grey',
              weight = 1,
              highlightOptions = highlightOptions(color = "yellow", weight = 2,bringToFront = TRUE),
              popup = pops) %>%
  addPolygons(data = dem2,
              group = 'Demographic Info',
              color = 'grey',
              weight = 1,
              fillOpacity = 0,
              highlightOptions = highlightOptions(color = "yellow", weight = 2,bringToFront = TRUE),
              popup = pops) %>%
  addPolylines(data = wtr_main,
               color = "blue",
               weight = 2) %>%
  addPolylines(data = wtr_conn,
               color = "red",
               weight = 2) %>%
  addPolylines(data = AOI,
               color = 'black',
               weight = 3) %>%
  addLayersControl(baseGroups = c('Open Street Map'),
                  overlayGroups = c('Median Household Income', 'People of Color', "Owner Occupied Housing"),
                  options = layersControlOptions(collapsed = TRUE)) %>%
  addLegend('bottomright',
            group = 'Median Household Income',
            pal = bpal,
            values = dem2$medhhinc,
            title = 'Median HH Income') %>%
  addLegend('bottomright',
            group = 'People of Color',
            pal = bpal2,
            values = 100*dem2$propPOC,
            title = 'People of Color (%)') %>%
  addLegend('bottomright',
            group = 'Owner Occupied Housing',
            pal = bpal3,
            values = 100*dem2$pownocc,
            title = 'Owner Occupied Housing (%)') %>%
  hideGroup(group = c('People of Color', 'Owner Occupied Housing'))
m

## export as interactive html map
library(htmlwidgets)
saveWidget(m,
           file="/Users/dhardy/Dropbox/r_data/berkeley-county/map.html",
           title = "Berkeley County, SC Information")

# m <- leaflet() %>%
#   addTiles(group = "Open Street Map") %>%  
#   addTiles(attribution = '<a href="https://www.census.gov/programs-surveys/acs/"> | US Census American Community Survey 2013-2017</a>') %>%
#   # addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
#   setView(lng = -80, lat = 33.2, zoom = 10) %>%
#   addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2)) %>%
#   # addPolylines(data = wtr_main,
#   #              color = "blue",
#   #              weight = 1) %>%
#   addPolylines(data = wtr_conn,
#                color = "red",
#                weight = 2)
# m
