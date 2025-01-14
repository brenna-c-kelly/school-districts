

library(sf)
library(tmap)
library(dplyr)
library(haven)
library(tidycensus)

# OPTION 1: existing crosswalk
# only gets at unified school districts-census block *group*

## School district geographic relationship files:
#  https://nces.ed.gov/programs/edge/geographic/relationshipfiles
#     notes:
#   - these are national files and fairly large (~50 MB)
#   - these only go down to the census block *group* level, not census block
crosswalk_13 <- read_sas("../GRF13/grf13_lea_blkgrp.sas7bdat")
# ^you won't be able to read this in — I've reduced it down to only UT so it's an email-able size

crosswalk_13 <- crosswalk_13 |>
  filter(LEAID %in% sb_21$LEAID)

write.csv(crosswalk_13, "../data/GRF13_UT/crosswalk_13.csv", row.names = FALSE)


# OPTION 2: create the crosswalk spatially
# unified school district-census *block*

## School district TIGER / lines can be obtained here at the state level:
#   https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
#     notes:
#   - school districts may vary by grade; Utah only makes is unified school district data available to the census
#   - I'm also attaching a folder with all these shapefiles

sb_21 <- st_read("../data/tl_unified school districts/tl_2021_49_unsd/tl_2021_49_unsd.shp")

sb_21$LEAID <- paste0(sb_21$STATEFP, sb_21$UNSDLEA)

tm_shape(sb_21) +
  tm_polygons(col = "NAME") +
  tm_layout(legend.outside = TRUE)

## Census block shapefiles
# can be obtained here:
#   https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
# but I prefer tidycensus (you just need to register with the census API)
#   note: census blocks change every decade with the decennial census
#         so we only need to collect 2010 and 2020 shapefiles

# check variables
v20 <- load_variables(2020, "pl", cache = TRUE)
v10 <- load_variables(2010, "pl", cache = TRUE)

# get block geometry
cb_shp_10 <- get_decennial(geography = "block",
                        state = "UT",
                        variables = c("P001001"), # total population
                        geometry = TRUE,
                        year = 2010, output = "wide")
cb_shp_20 <- get_decennial(geography = "block",
                        state = "UT",
                        variables = c("P1_001N"), # total population
                        geometry = TRUE,
                        year = 2020, output = "wide")

# if you don't want to register with the API, just read in the data I've written here:
st_write(cb_shp_10, "../data/cb_shp/cb_shp_10.shp", append = FALSE)
st_write(cb_shp_20, "../data/cb_shp/cb_shp_20.shp", append = FALSE)

cb_shp_10 <- st_read("../data/cb_shp/cb_shp_10.shp")
cb_shp_20 <- st_read("../data/cb_shp/cb_shp_20.shp")

# the spatial join: spatially linking the school district and census data
# - use cb_shp_10 for years 2013-2019
# - use cb_shp_20 for years 2020-2021
# school district boundaries may change any year, so you'll create the crosswalk for each year of data

# there is a many-to-one issue

# e.g., to merge 2021 school districts with cb_shp_20:
sb_cb <- st_join(cb_shp_20, sb_21,    
                 join = st_intersects, 
                 largest = FALSE)
# check that the census blocks are unique (not assigned to more than one school district)

# the more computationally expensive process, only to be applied to blocks with many-to-one
not_unique <- sb_cb |>
  filter(duplicated(GEOID.y))

sb_cb_2 <- st_join(not_unique, sb_21,    
                 join = st_intersects, 
                 largest = TRUE)

# then combine

# if you find that census blocks are not unique in the crosswalk, use:
# largest = TRUE; ensures each block is only assigned to one school district 
# takes ~30s to run with largest = FALSE argument
# takes 15-20m to run with largest = TRUE argument
#   - note: there will not be 100% match, but you can check that these census block groups are unpopulated:

length(unique(sb_cb$GEOID.y))

missing_cb <- cb_shp_20 |>
  filter(!GEOID %in% sb_cb$GEOID.y)
head(missing_cb)

head(sb_cb)





