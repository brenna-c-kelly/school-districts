
library(sf)
library(tmap)
library(dplyr)
library(haven)
library(tidycensus)

# takes about 6m / year

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


folder_shps <- paste0("../data/tl_unified school districts/",
                      list.files("../data/tl_unified school districts/"))

districts_13 <- st_read('/Users/brenna/Documents/School/Research/dicking-around/school_districts/data/tl_unified school districts/tl_2021_49_unsd/tl_2021_49_unsd.shp')
districts_21 <- st_read('/Users/brenna/Documents/School/Research/dicking-around/school_districts/data/tl_unified school districts/tl_2016_49_unsd/tl_2016_49_unsd.shp')

districts_diff <- st_difference(districts_21, districts_13)

# confirmed, the boundaries changed in 2016 but not any other year
tm_shape(districts_21) +
  tm_polygons(col = "white", border.col = "red") +
  tm_shape(districts_13) +
  tm_polygons(col = "white", border.col = "darkblue") +
  tm_shape(districts_diff) +
  tm_polygons(col = "purple", alpha = 0.5)

tm_shape(districts_diff) +
  tm_polygons(col = "purple", alpha = 0.5)

store_crosswalk <- list()

for(i in 2) {
  
  shp_files <- list.files(paste0(folders[i], "/"), pattern = ".shp")[1]
  
  # school district boundaries
  sb <- st_read(paste0(paste0(folders[i], "/"), shp_files))
   
  sb$LEAID <- paste0(sb$STATEFP, sb$UNSDLEA)
  
  # t <- tm_shape(sb) +
  #   tm_polygons(col = "NAME") +
  #   tm_layout(legend.outside = TRUE)
  # print(t)
  
  if(i < 7) { # using 2010 for 2013-2019
    cb_shp <- cb_shp_10
  }
  else { # using 2020 for 2020-2021
    cb_shp <- cb_shp_20
  } 
  print("test -1")
  sb_cb <- st_join(cb_shp, sb,
                     join = st_intersects,
                     largest = TRUE)
  print("test 0")
  # not_unique <- sb_cb_1 |>
  #   filter(duplicated(GEOID.x))
  # 
  # cb_shp_not_unique <- cb_shp |>
  #   filter(GEOID %in% not_unique$GEOID.x)
  # summary(cb_shp_not_unique)
  # summary(sb)
  # sb_cb_2 <- st_join(cb_shp_not_unique, sb,
  #                    join = st_intersects,
  #                    largest = TRUE)
  # # takes ~3m to run with subset and largest = TRUE argument
  # sb_cb_2 <- sb_cb_2 |>
  #   select(GEOID.x, NAME.y, LOGRADE, HIGRADE, LEAID) |>
  #   rename(census_block_fips = GEOID.x,
  #          school_district = NAME.y)
  # print("test 2")
  # # combine the two
  # sb_cb <- sb_cb_1 |>
  #   filter(!GEOID.x %in% sb_cb_2$census_block_fips) |>
  #   select(GEOID.x, NAME.y, LOGRADE, HIGRADE, LEAID) |>
  #   rename(census_block_fips = GEOID.x,
  #          school_district = NAME.y) |>
  #   rbind(sb_cb_2)
  
  sb_cb$year <- i + 2012
  
  sb_cb <- st_drop_geometry(sb_cb) # keep codes, not geometry
  
  store_crosswalk[[i]] <- sb_cb
  
}

# check
store_crosswalk[[1]]
store_crosswalk[[9]]

tm_shape(store_crosswalk[[1]]) +
  tm_polygons(col = "school_district")

tm_shape(store_crosswalk[[9]]) +
  tm_polygons(col = "school_district")

# save



