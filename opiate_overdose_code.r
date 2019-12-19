###################################################
# Project: MUSA 507 Opiate Overdose Preduction
# Names: Claire Allen-Platt and Nicolas Corona
# Date: December 20, 2019
###################################################

# Load packages, data, colors etc ---------------------------------

library(here)
library(raster)
library(tidyverse)
library(dplyr)
library(janitor)
library(maps)
library(tools)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(QuantPsyc)
library(RSocrata)
library(viridis)
library(caret)
library(spatstat)
library(spdep)
library(FNN)
library(grid)
library(gridExtra)
library(knitr)
library(kableExtra)
library(tidycensus)
library(stringr)
library(lubridate)
census_api_key("f4a1e79b1b513acf5523856785193ebfdecdd406") # This is my personal API key.
                                                           # Please don't use it after  
                                                           # this project concludes!

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}

nn_function <- function(measureFrom,measureTo,k) {
  measureFrom_Matrix <-
    as.matrix(measureFrom)
  measureTo_Matrix <-
    as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint) %>%
    pull()
  
  return(output)  
}

palette_5_blOr <-  c("#1170AA","#5FA2CE","#A3CCE9","#FC7D0B","#C85200")

######################################################################################
#############################
# Loading data from online. #
#############################
######################################################################################

# ems <- read.socrata("https://data.cincinnati-oh.gov/resource/vnsz-a3wp.csv") 
# saveRDS(ems, "ems.RDS")
#
# drugs_cops <- read.socrata("https://data.cincinnati-oh.gov/resource/3gx7-se9a.csv")
# saveRDS(drugs_cops, "drugs_cops.RDS")
#
# heroin_cops <- read.socrata("https://data.cincinnati-oh.gov/resource/7mtn-nnb5.csv")
# saveRDS(heroin_cops, "heroin_cops.RDS")
#
# cops <- read.socrata("https://data.cincinnati-oh.gov/resource/gexm-h6bt.csv")
# saveRDS(cops, "cops.RDS")
# 
# crimes <- read.socrata("https://data.cincinnati-oh.gov/resource/k59e-2pvf.csv")
# saveRDS(crimes, "crimes.RDS")
# 
# c311 <- read.socrata("https://data.cincinnati-oh.gov/resource/4cjh-bm8b.csv")
# saveRDS(c311, "c311.RDS")
# 
# code <- read.socrata("https://data.cincinnati-oh.gov/resource/cncm-znd6.csv")
# saveRDS(code, "code.RDS")
#
# cincinnati <- st_read("https://opendata.arcgis.com/datasets/ed78f4754b044ac5815d0a9efe9bb336_1.geojson")
# saveRDS(cincinnati, "cincinnati.RDS")
# 
# sna <- st_read("https://opendata.arcgis.com/datasets/572561553c9e4d618d2d7939c5261d46_0.geojson")
# saveRDS(sna, "sna.RDS")
# 
# districts_cops <- st_read("https://opendata.arcgis.com/datasets/9bc1afaff72e4f44a6d19280c159c951_4.geojson")
# saveRDS(districts_cops, "districts_cops.RDS")
# 
# firehouses <- st_read("https://opendata.arcgis.com/datasets/a6f043d181f94e37a274975a3718b7af_16.geojson")
# saveRDS(firehouses, "firehouses.RDS")
# 
# sheriff <- st_read("https://opendata.arcgis.com/datasets/ad2bd178b8624b04ae1878fe598c6001_3.geojson")
# saveRDS(sheriff, "sheriff.RDS")
# 
# zoning <- st_read("https://opendata.arcgis.com/datasets/1dcafeb2b62d4a02b3a19ee5fc7041ce_4.geojson")
# saveRDS(zoning, "zoning.RDS")
#
# # Census data
# # Race from the 2010 US Census
# racevars <- c(total_pop = "P003001",
#               white = "P003002", 
#               black = "P003003", 
#               asian = "P003005", 
#               hispanic = "P004003",
#               two_or_more = "P003008")
# censusRace <- get_decennial(geography = "tract", variables = racevars, state = 39, 
#                             county = 061, geometry = TRUE) %>% st_transform(3735)
# censusRace <- censusRace %>% 
#   spread(key = variable, value = value) %>%
#   mutate(prop_white = round(white/total_pop,4),
#          prop_black = round(black/total_pop,4),
#          prop_asian = round(asian/total_pop,4),
#          prop_hispanic = round(hispanic/total_pop,4),
#          prop_two = round(two_or_more/total_pop,4)) %>%
#   gather("variable", "value", -GEOID, -NAME, -geometry) %>%
#   dplyr::select(GEOID, NAME, variable, value, geometry)
# st_write(censusRace, "censusRace.shp")

# Other place-related variables from the 2015 American Community Survey
# v15 <- load_variables(2015, "acs5", cache = TRUE)
# write_csv(v15,"acs_vars_2015.csv")
# acsvars <- c(total_pop = "B01003_001",
#                total_males = "B01001_002",
#                males_18_19 = "B01001_007",
#                males_20 = "B01001_008",
#                males_21 = "B01001_009",
#                males_22_24 = "B01001_010",
#                males_25_29 = "B01001_011",
#                males_30_34 = "B01001_012",
#                males_35_39 = "B01001_013",
#                males_40_44 = "B01001_014",
#                males_45_49 = "B01001_015",
#                males_50_54 = "B01001_016",
#                males_55_59 = "B01001_017",
#                males_60_61 = "B01001_018",
#                males_62_64 = "B01001_019",
#                males_65_66 = "B01001_020",
#                males_67_69 = "B01001_021",
#                males_70_74 = "B01001_022",
#                males_75_79 = "B01001_023",
#                males_80_84 = "B01001_024",
#                males_85plus = "B01001_025",
#                medincome = "B19013_001",
#                per_capita_income = "B19301_001",
#                total_blw_poverty_lvl = "B17020_002",
#                median_home_value = "B25077_001",
#                bachelors_or_higher = "B23006_023",
#                unemployed_over_16yrs = "B23025_007"
#                )
# acsData <- get_acs(geography = "tract",
#                    variables = acsvars,
#                    state = 39,
#                    county = 061,
#                    geometry = TRUE) %>% st_transform(3735)
# acsData <- acsData %>%
#   dplyr::select(-moe) %>%
#   spread(key = variable, value = estimate) %>%
#   mutate(log_pop = log(total_pop),
#          prop_males = round(total_males/total_pop,4),
#          prop_males_18to24 = round((males_18_19+males_20+males_21+males_22_24)/total_males,4),
#          prop_males_25to34 = round((males_25_29+males_30_34)/total_males,4),
#          prop_males_35to49 = round((males_35_39+males_40_44+males_45_49)/total_males,4),
#          prop_males_50to64 = round((males_50_54+males_55_59+males_60_61+males_62_64)/total_males,4),
#          prop_males_65up = round((males_65_66+males_67_69+males_70_74+males_75_79+males_85plus)/total_males,4),
#          prop_poverty = round(total_blw_poverty_lvl/total_pop,4),
#          prop_bach_or_higher = round(bachelors_or_higher/total_pop,4),
#          prop_unempl = round(unemployed_over_16yrs/total_pop,4)) %>%
#   gather("variable", "estimate", -GEOID, -NAME, -geometry) %>%
#   dplyr::select(GEOID, NAME, variable, estimate, geometry)
# acsDataTEMP <- acsData %>% filter(stringr::str_detect(variable, 'prop')) # checking to make sure my 'proportion' variables aren't out of whack
# min(acsDataTEMP$estimate) # should be between 0 and 1
# max(acsDataTEMP$estimate) # should be between 0 and 1
# st_write(acsData, "acsData.shp", update = T)

######################################################################################
################################
# Loading data from RDS files. #
################################
######################################################################################

ems <- read_rds("ems.RDS") %>% as_tibble() # Emergency services calls.
drugs_cops <- read_rds("drugs_cops.RDS") %>% as_tibble() # TODO: this is a subset of all police data. confirm whether this is a reasonable subset or we want to pull in more data.
heroin_cops <- read_rds("heroin_cops.RDS") %>% as_tibble() # TODO: same as above - heroin is a subset of drugs dataset.
# cops <- read_rds("cops.RDS") %>% as_tibble() # Police calls (proactive and reactive)
# Note -- I'm not seeing this dataset on Git. It looks like it's 2.9 million rows. 
# Is it helpful enough to justify the computational time for loading it into R?
# It might be, but I doubt it.  The drugs and heroin subsets should have all of the relevant occurrence data,
# but we'll likely need some of this data for correlated factors.
crimes <- read_rds("crimes.RDS") %>% as_tibble() # Crimes reported.
c311 <- read_rds("c311.RDS") %>% as_tibble() # 311 calls.
code <- read_rds("code.RDS") %>% as_tibble() # Code enforcement.
cincinnati <- read_rds("cincinnati.RDS") %>% st_transform(3735) # Cincinnati city boundary shapefile.
sna <- read_rds("sna.RDS") %>% st_transform(3735) # Cincinnati SNA (Statistical Neighborhood Approximations) Boundaries
districts_cops <- read_rds("districts_cops.RDS") %>% st_transform(3735) # Police districts.
firehouses <- read_rds("firehouses.RDS") %>% st_transform(3735) # Firehouses.
sheriff <- read_rds("sheriff.RDS") %>% st_transform(3735) # Sheriff stations.
censusRace <- st_read("censusRace.shp") # race/ethnicity counts and %s by census tract
acsData <- st_read("acsData.shp") # lots of place-based covariates from American Community Survey (see the code above, commented out, for variables list - based it on the Li et al article in the literature folder on GitHub, which is a spatial prediction model of heroin overdose in Cincy)
zoning <- read_rds("zoning.RDS") %>% st_transform(3735)

######################################################################################
##################################
# Loading data downloaded files. #
##################################
######################################################################################

# naloxone access sites in Hamilton County (source: https://takechargeohio.org/map) 
# note this was manually geocoded
nalox_sites <- read_csv("naloxone_distribution_sites.csv", col_names = T) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
  st_transform(3735) 

# Hamilton streets (source: https://catalog.data.gov/dataset/tiger-line-shapefile-2014-county-hamilton-county-oh-all-roads-county-based-shapefile)
hamilton_streets <- st_read("tl_2014_39061_roads.shp") %>%
  st_transform(3735) 

# Buildings ordered vacant by the city because unsafe/condemned/etc (source: City of Cincinnati http://cagismaps.hamilton-co.org/cincinnatiServices/CodeEnforcement/CincinnatiVacantBuildingCases/)
#vacant_buildings <- read_csv("vacantbldgs.csv", col_names = T) 
vacant_buildings <- st_read("vacantbldgs.shp") %>% st_transform(3735) # updating with shapefile created in ArcMap

# Ohio counties, state boundary, and Hamilton County only
ohio_counties <- st_read("tl_2016_39_cousub.shp") %>% st_transform(3735) # From:  https://catalog.data.gov/dataset/tiger-line-shapefile-2016-state-ohio-current-county-subdivision-state-based
ohio_boundary <- st_union(ohio_counties)

hamilton <- st_union(ohio_counties[which(ohio_counties$COUNTYFP == "061"),]) %>%
  st_transform(3735)

# Special business licenses 
biz_licenses <- read_csv("Business_Licenses.csv", col_names = T) %>%
  filter(!is.na(LATITUDE)) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, agr = "constant") %>%
  st_transform(3735)

# Weather measured from Cincinnati airport.
weather <- read_csv("weather.csv")
weather <- weather %>% dplyr::select(STATION, DATE, HourlyDryBulbTemperature, HourlyPrecipitation, SOURCE, REPORT_TYPE)

# Fast food chain locations
ffood <- st_read("Fast food Cincinnati.kml") %>% st_set_crs(4326) %>%
  st_transform(3735)

# # In case we want it ... gentrification measures by census tract. Perhaps non-gentrifying
# # tracts are overdose prone? (Or maybe no tracts are gentrifying in Cincinatti?)
# # This is just borrowed from our midterm code 
# # Source: Longitudinal Tract Database 
# # https://s4.ad.brown.edu/projects/diversity/Researcher/LTBDDload/DataList.aspx
# # Census data from 2000, with 2010 tract IDs included
# ltdb_fullMeasures_2000 <- read_csv("LTDB_Std_2000_fullcount.csv", col_names = T) %>% 
#   filter(county == "Hamilton County" & state == "OH") %>%
#   dplyr::select(tract_id = TRTID10,   # unique ID
#          tract,
#          POP00,     # total population 
#          NHBLK00,   # count of non-hispanic black residents
#          A60BLK00,  # count of black residents aged 60+
#          NHWHT00    # count of non-hispanic white residents
#   )
# get_dupes(ltdb_fullMeasures_2000, tract_id)
# 
# # Census data from 2010, with 2010 tract ids
# ltdb_fullMeasures_2010 <- read_csv("LTDB_Std_2010_fullcount.csv", col_names = T) %>% 
#   filter(county == "Hamilton County" & state == "OH") %>%
#   dplyr::select(tract_id = tractid,   # unique ID
#          pop10,     # total population 
#          nhblk10,   # count of non-hispanic black residents
#          a60blk10,  # count of black residents aged 60+
#          nhwht10    # count of non-hispanic white residents
#   )
# 
# ltdb_sampleMeasures_2000 <- read_csv("ltdb_std_2000_sample_SFonly.csv", col_names = T) %>%
#   select(tract_id = TRTID10, # unique ID
#          POP00SF3,  # total pop, as estimated from sample data
#          PROF00,    # sample estimate of persons in "professional occupations"
#          MANUF00,   # sample estimate of persons in "manufacturing occupations"
#          HINC00,    # median household income
#          HINCW00,   # median household income for whites
#          HHW00,     # total white households
#          HINCB00,   # median household income for whites
#          HHB00,     # total black households
#          INCPC00,   # per capita income
#          NPOV00,    # persons in poverty
#          N65POV00,  # persons 65+ in poverty
#          NBPOV00,   # blacks in poverty
#          MHMVAL00,  # median home value 
#          MRENT00,   # median gross rent
#          H30OLD00,  # structures built >30 years ago
#          H10YRS00,  # household heads moved into unit < 10 years ago
#          HS00,      # persons with high school or less
#          COL00      # persons with at least four-year college degree
#   )
# # ACS variables measured with sample data, from 2008-2012 ACS, with 2010 tract ID included
# ltdb_sampleMeasures_2010 <- read_csv("ltdb_std_2010_sample_SFonly.csv", col_names = T) %>%
#   select(tract_id = tractid, # unique ID
#          pop12,     # total pop (as estimated from ACS)
#          prof12,    # sample estimate of persons in "professional occupations"
#          manuf12,   # sample estimate of persons in "manufacturing occupations"
#          hinc12,    # median household income
#          hincw12,   # median household income for whites
#          hhw12,     # total white households
#          hincb12,   # median household income for blacks
#          hhb12,     # total black households
#          incpc12,   # per capita income
#          npov12,    # persons in poverty
#          n65pov12,  # persons 65+ in poverty
#          nbpov12,   # blacks in poverty
#          pbpov12,   # % blacks in poverty
#          mhmval12,  # median home value 
#          mrent12,   # median gross rent
#          h30old12,  # structures built >30 years ago
#          h10yrs12,  # household heads moved into unit < 10 years ago
#          phs12,     # % persons with high school or less
#          pcol12,    # % persons with at least four-year college degree
#   )
# 
# censusLongitudinal <- ltdb_fullMeasures_2000 %>%
#   left_join(ltdb_sampleMeasures_2000, by = "tract_id") %>%
#   left_join(ltdb_fullMeasures_2010, by = "tract_id") %>%
#   left_join(ltdb_sampleMeasures_2010, by = "tract_id")
# 
# # Replace -999 missing code with NAs
# censusLongitudinal <- mutate_at(censusLongitudinal, vars(POP00:h10yrs12), 
#                                 list(~ ifelse( . == -999, NA, .)))
# 
# # Check for missingness
# sapply(censusLongitudinal, function(x) sum(is.na(x)))
# 
# # Make percentage variables that are missing from 2000 sample data
# censusLongitudinal <- censusLongitudinal %>%
#   mutate(PBPOV00 = NBPOV00/POP00SF3,
#          PHS00 = HS00/POP00SF3,
#          PCOL00 = COL00/POP00SF3)
# 
# # Calculate the 50th, 75th and 90th percentile of median household income for 2000
# stats::quantile(censusLongitudinal$HINC00, c(.5, .75, .9), na.rm = T)
# 
# # Calculate the change in (1) % of college-educated residents per tract, (2) median 
# # monthly rent and (3) median home value, between 2000 and 2010/2012
# censusLongitudinal <- censusLongitudinal %>%
#   mutate(pcol_change = pcol12 - PCOL00,
#          mrent_change = mrent12 - MRENT00,
#          mhmv_change = mhmval12 - MHMVAL00)
# # Calculate the 50th percentile of change in college-educated residents
# stats::quantile(censusLongitudinal$pcol_change, c(.5), na.rm = T)
# 
# # Calculate the 50th and 75th percentile of change in median monthly rent
# stats::quantile(censusLongitudinal$mrent_change, c(.5, .75), na.rm = T)
# 
# # Calculate the 50th and 75th percentil of change in median home value
# stats::quantile(censusLongitudinal$mhmv_change, c(.5, .75), na.rm = T)
# 
# # Generate measure of gentrification
# # Three-level variable 
# censusLongitudinal <- censusLongitudinal %>% 
#   mutate(pcol_change_flag = if_else(pcol_change < 53.5942, 0, 1))
# censusLongitudinal <- censusLongitudinal %>% 
#   mutate(mrent_change_flag = if_else(mrent_change >= 486 & mrent_change < 630, 1,
#                                      if_else(mrent_change >= 630, 2, 0)))        
# censusLongitudinal <- censusLongitudinal %>% 
#   mutate(mhmv_change_flag = if_else(mhmv_change >= 290101.0 & mhmv_change < 351150.5, 1,
#                                     if_else(mhmv_change >= 351150.5, 2, 0)))      
# # This gentrification measure is based on Hirsch (2018), who suggests a three-level 
# # measure in which gentrification is a three-level variable (0 = not gentrified, 1 = 
# # gentrification, 2 = intense gentrification)
# censusLongitudinal <- censusLongitudinal %>% 
#   mutate(gentrification = if_else(pcol_change_flag == 0, 0, 
#                                   if_else(pcol_change_flag == 1 & mrent_change_flag == 0 & mhmv_change_flag == 0, 0,
#                                           if_else((pcol_change_flag == 1 & mrent_change_flag == 1) | (pcol_change_flag == 1 & mhmv_change_flag == 1), 1, 
#                                                   if_else((pcol_change_flag == 1 & mrent_change_flag == 2) | (pcol_change_flag == 1 & mhmv_change_flag == 2), 2, as.double(NA))))))
# 
# # Change gentrification to a factor
# censusLongitudinal$gentrification <- as.factor(censusLongitudinal$gentrification)
# censusLongitudinal$gentrification <- factor(censusLongitudinal$gentrification, 
#                                             labels = c("No gentrification", 
#                                                        "Gentrification", 
#                                                        "Intense gentrification"))
# tabyl(censusLongitudinal$gentrification)

######################################################################################
############################################
# Basic boundary and point visualizations. #
############################################
######################################################################################

# Cincinnati border.
ggplot() + geom_sf(data = cincinnati) + mapTheme()

# Cincinnati neighborhoods.
ggplot() + geom_sf(data = cincinnati) + geom_sf(data = sna) + mapTheme()

# Cincinatti police districts.
ggplot() + geom_sf(data = cincinnati) + geom_sf(data = districts_cops) + mapTheme()

# Cincinatti neighborhoods (SNAs).
ggplot() + geom_sf(data = cincinnati) + geom_sf(data = sna) + mapTheme()

# Hamilton County naloxone sites.
ggplot() + geom_sf(data = hamilton) + geom_sf(data = cincinnati) + geom_sf(data = nalox_sites) + mapTheme()

# Hamilton County streets - long time to load.
ggplot() + geom_sf(data = hamilton) + geom_sf(data = hamilton_streets) + mapTheme()

# Hamilton County zoning.
ggplot() + 
  geom_sf(data = cincinnati) +
  geom_sf(data = zoning) +
  mapTheme()

# Hamilton County firehouses.
ggplot() +
  geom_sf(data = hamilton) +
  geom_sf(data = cincinnati) +
  geom_sf(data = firehouses) +
  mapTheme()

# Hamilton County sheriff stations.
ggplot() +
  geom_sf(data = hamilton) +
  geom_sf(data = cincinnati) +
  geom_sf(data = sheriff) +
  mapTheme()

# Cincinnati city limits inside Hamilton County
ggplot() +
  geom_sf(data = hamilton) +
  geom_sf(data = cincinnati) +
  mapTheme()

# Vacant and condemned buildings
ggplot() +
  geom_sf(data = cincinnati) +
  geom_sf(data = sna) +
  geom_sf(data = vacant_buildings) +
  mapTheme()

######################################################################################
#########################################
# Basic cleaning and data manipulation. #
#########################################
######################################################################################

# (1) Outcome of interest: heroin overdose 
ems <- ems[which(!is.na(ems$latitude_x)), ] # TODO: see whether NAs are distributed spatially or across time.
ems$year <- str_sub(ems$create_time_incident, start = 1, end = 4)
ems15 <- ems[which(ems$year == "2015"),]
ems16 <- ems[which(ems$year == "2016"),]
ems17 <- ems[which(ems$year == "2017"),]
ems18 <- ems[which(ems$year == "2018"),]
ems19 <- ems[which(ems$year == "2019"),]

# These years only have 1-17 records per year.
crimes <- crimes[which(str_sub(crimes$date_reported, start = 1, end = 4) != 1991 &
                         str_sub(crimes$date_reported, start = 1, end = 4) != 2003 &
                         str_sub(crimes$date_reported, start = 1, end = 4) != 2007 &
                         str_sub(crimes$date_reported, start = 1, end = 4) != 2008 &
                         str_sub(crimes$date_reported, start = 1, end = 4) != 2009
),]

# These years only have 74-310 records per year.
code <- code[which(str_sub(code$entered_date, start = 1, end = 4) != 1999 &
                     str_sub(code$entered_date, start = 1, end = 4) != 2000
),]

# Heroin-related portions of ems dataset.  Note that this selects from disposition_text for narcan (naloxone) usage,
# used to combat fatal heroin overdoses, in addition to incident_type_ids from the data dictionaries.
# Nick also went through manually to confirm that the data dictionary doesn't miss any heroin-related incidents.
heroin_ems <- ems[which(
  str_detect(ems$disposition_text, "NAR") |
    str_detect(ems$disposition_text, "NART") |
    ems$incident_type_id == "HEROINF - FIRE ONLY" |
    ems$incident_type_id == "HEROIN-COMBINED" |
    ems$incident_type_id == "HEROINF-FIRE ONLY" |
    ems$incident_type_id == "HERONF" |
    ems$incident_type_id == "HEORIF"), ] %>% 
  st_as_sf(coords = c("longitude_x", "latitude_x"), crs = 4326, agr = "constant") %>%
  st_transform(3735)

heroin_ems$floor_date <- floor_date(ymd_hms(heroin_ems$create_time_incident, tz = "EST"), unit = "hour") %>% with_tz(tz = "UTC")
weather$floor_date <- ymd_hms(weather$DATE, tz = "UTC") %>% floor_date(unit = "hour")

heroin_ems <- left_join(heroin_ems, 
                        weather %>% 
                          dplyr::mutate(precipitation = HourlyPrecipitation,
                                        temperature = HourlyDryBulbTemperature) %>%
                          dplyr::select(floor_date, temperature, precipitation),
                        by = c("floor_date")
)

heroin_ems15 <- heroin_ems[which(heroin_ems$year == "2015"),]
heroin_ems16 <- heroin_ems[which(heroin_ems$year == "2016"),]
heroin_ems17 <- heroin_ems[which(heroin_ems$year == "2017"),]
heroin_ems18 <- heroin_ems[which(heroin_ems$year == "2018"),]
heroin_ems19 <- heroin_ems[which(heroin_ems$year == "2019"),]

# (2) drugs_cops
drugs_cops <- drugs_cops[!is.na(drugs_cops$latitude_x), ] %>%
  st_as_sf(coords = c("longitude_x", "latitude_x"), crs = 4326, agr = "constant") %>%
  st_transform(3735)

# (3) heroin_cops
heroin_cops <- heroin_cops[!is.na(heroin_cops$latitude_x), ] %>%
  st_as_sf(coords = c("longitude_x", "latitude_x"), crs = 4326, agr = "constant") %>%
  st_transform(3735)

# (4) crimes
crimes <- crimes[!is.na(crimes$latitude_x), ] %>%
  st_as_sf(coords = c("longitude_x", "latitude_x"), crs = 4326, agr = "constant") %>%
  st_transform(3735)

# (5) c311
c311 <- c311[!is.na(c311$latitude), ] %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
  st_transform(3735)

# # Broad category.
# unique(sapply(str_split(c311$service_name, ","), function(x) {x[[1]]})) # Do not delete this.
# 
# # Subcategory of the broad category.
# unique(sapply(str_split(c311$service_name, ","), function(x) {unlist(x)[2]})) # Do not delete this.
# 
# unique(sapply(str_split(c311$service_name, ","), 
#               function(x) {x[[1]]}))[which(str_detect(
#                 unique(sapply(str_split(c311$service_name, ","), 
#                               function(x) {x[[1]]})), "Graffiti"))]
# 
# unique(sapply(str_split(c311$service_name, ","), 
#               function(x) {x[[1]]}))[which(str_detect(
#                 unique(sapply(str_split(c311$service_name, ","), 
#                               function(x) {x[[1]]})), "Light"))]
# 
# # Only for 2019 but I'm just curious whether it is correlated w/ overdoses.
# unique(sapply(str_split(c311$service_name, ","), 
#               function(x) {x[[1]]}))[which(str_detect(
#                 unique(sapply(str_split(c311$service_name, ","), 
#                               function(x) {x[[1]]})), "Homeless"))]

# (6) code
code <- code %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
  st_transform(3735)

# unique(code$comp_type_desc)

abandoned_vehicles <- code %>% dplyr::filter(comp_type_desc == "Abandoned Vehicle Code Enforcement")

# Might only want to use one of these if they're virtually the same.
# Excludes description "Weed and Litter Complnt".
complainedTrash <- code %>% filter(comp_type_desc == "Trash/Litter/Tall Grass Complaint")
cleanedTrash <- code %>% filter(comp_type_desc == "Trash/Litter/Tall Grass Cleaning")

# (7) Firearms and weapons dealers
firearms <- biz_licenses %>% filter(LICENSE == "DANGEROUS WEAPONS" | 
                                    LICENSE == "RETAIL DEALER IN FIREARMS AND AMMUNITION")
# unique(firearms$LICENSE)

# (8) Cabaret & pool halls
funTimes <- biz_licenses %>% filter(LICENSE == "BILLIARDS AND POOL TABLE" | 
                                      LICENSE == "MASSAGE PRACTITIONER" |
                                      LICENSE == "GAME ARCADE" |
                                      LICENSE == "CABARET" |
                                      LICENSE == "DANCE HALL" |
                                      LICENSE == "SEXUALLY ORIENTED BUSINESS" |
                                      LICENSE == "MASSAGE ESTABLISHMENT")
# unique(funTimes$LICENSE)

# (9) Union Terminal location for Euclidean distance
lat <- c(-84.53228)
long <- c(39.108841)
unionTerminal <- as.data.frame(cbind(lat, long))
unionTerminal <- unionTerminal %>% st_as_sf(coords = c("long", "lat"), crs = 4326, 
                                            agr = "constant") %>%
  st_transform(3735)

######################################################################################
##################################
# Date ranges.
##################################
######################################################################################

# Sample code:
# summary(as.Date(str_sub(crimes$date_reported, start = 1, end = 10)))
# table(str_sub(code$entered_date, start = 1, end = 4))

# ems: 2015-01-01 to 2019-11-20

# drugs_cops: 2017-01-01 to 2017-12-31

# heroin_cops: 2015-07-17 to 2018-08-18

# crimes: 2010-01-03 to 2019-11-20

# c311: 2012-01-01 to 2019-11-20

# code: 2001-01-02 to 2019-11-20

# heroin_ems: 2015-07-22 to 2019-11-19

# weather: 2015-01-01 to 2019-12-19

######################################################################################
##################################
# Code for markdown.
##################################
######################################################################################

fishnet <- 
  st_make_grid(cincinnati, cellsize = 1000) %>%
  st_sf()

ggplot() + # Takes a while to generate.
  geom_sf(data=cincinnati, fill=NA, colour="black") +
  geom_sf(data=fishnet, fill=NA, colour="black") +
  labs(title = "Cincinnati and the fishnet") +
  mapTheme()

fishnet <- 
  fishnet[cincinnati,] %>%
  mutate(uniqueID = rownames(.)) %>%
  dplyr::select(uniqueID)

ggplot() +
  geom_sf(data=fishnet) +
  labs(title = "Fishnet in Cincinnati") +
  mapTheme()

ggplot() + 
  geom_sf(data = cincinnati) +
  geom_sf(data = heroin_ems18, colour="red", size=0.05, show.legend = "point") +
  labs(title= "Heroin-related EMS responses, Cincinnati, 2018") +
  mapTheme()

heroin_net <- 
  heroin_ems18 %>% 
  dplyr::select() %>% 
  mutate(countHeroinResponses = 1) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(countHeroinResponses = ifelse(is.na(countHeroinResponses), 0, countHeroinResponses),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), size=nrow(fishnet), replace = TRUE))

ggplot() +
  geom_sf(data = heroin_net, aes(fill = countHeroinResponses)) +
  scale_fill_viridis() +
  labs(title = "Count of Heroin Responses for the fishnet") +
  mapTheme()

# Euclidean distance 
UTpoint <-
  unionTerminal %>%
  st_centroid()

# Small multiple map of risk factors
ggplot() +
  geom_sf(data=cincinnati) +
  geom_sf(data = rbind(abandoned_vehicles %>% dplyr::select() %>%
                         st_transform(st_crs(fishnet)) %>%
                         mutate(Legend = "Abandoned_Cars"),
                       funTimes %>% dplyr::select() %>%
                         st_transform(st_crs(fishnet)) %>%
                         mutate(Legend = "Fun_Times"), 
                       firearms %>% dplyr::select() %>%
                         st_transform(st_crs(fishnet)) %>%
                         mutate(Legend = "Firearms_Vendors"),
                       vacant_buildings %>% dplyr::select() %>%
                         st_transform(st_crs(fishnet)) %>%
                         mutate(Legend = "Vacant_Buildings"), 
                       nalox_sites %>% dplyr::select() %>%
                         st_transform(st_crs(fishnet)) %>%
                         mutate(Legend = "Nalox_Sites")),
          size = .1) +
  facet_wrap(~Legend, ncol = 2) +
  labs(title = "Risk Factors") +  
  mapTheme()

# Aggregate the count data into the fishnet cells.
vars_net <- 
  rbind(abandoned_vehicles %>% dplyr::select() %>%
          st_transform(st_crs(fishnet)) %>%
          mutate(Legend = "Abandoned_Cars"),
        funTimes %>% dplyr::select() %>%
          st_transform(st_crs(fishnet)) %>%
          mutate(Legend = "Fun_Times"), 
        firearms %>% dplyr::select() %>%
          st_transform(st_crs(fishnet)) %>%
          mutate(Legend = "Firearms_Vendors"),
        vacant_buildings %>% dplyr::select() %>%
          st_transform(st_crs(fishnet)) %>%
          mutate(Legend = "Vacant_Buildings"), 
        nalox_sites %>% dplyr::select() %>%
          st_transform(st_crs(fishnet)) %>%
          mutate(Legend = "Nalox_Sites")) %>%
  st_join(., fishnet, join=st_within) %>%
  st_set_geometry(NULL) %>%
  group_by(uniqueID, Legend) %>%
  summarize(count = n()) %>%
  full_join(fishnet) %>%
  spread(Legend, count, fill=0) %>%
  st_sf() %>%
  dplyr::select(-`<NA>`) %>%
  na.omit()

# vars_net

vars_net.long <- 
  vars_net %>%
  gather(Variable, value, -geometry, -uniqueID)

vars <- unique(vars_net.long$Variable)
mapList <- list()

for(i in vars){
  mapList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(vars_net.long, Variable == i), aes(fill=value), colour=NA) +
    scale_fill_viridis(name="") +
    labs(title=i) +
    mapTheme()}

# This takes a while.
do.call(grid.arrange,c(mapList, ncol =2, top = "Risk Factors by Fishnet"))

vars_net$abandoned_vehicles.nn =
  nn_function(st_coordinates(st_centroid(vars_net)), st_coordinates(abandoned_vehicles), 4)

vars_net$vacant_buildings.nn =
  nn_function(st_coordinates(st_centroid(vars_net)), st_coordinates(vacant_buildings), 4)

vars_net$funTimes.nn =
  nn_function(st_coordinates(st_centroid(vars_net)), st_coordinates(funTimes), 3)

vars_net.long.nn <- 
  vars_net %>%
  dplyr::select(ends_with(".nn")) %>%
  gather(Variable, value, -geometry, -uniqueID)

vars <- unique(vars_net.long.nn$Variable)
mapList <- list()

for(i in vars){
  mapList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(vars_net.long.nn, Variable == i), aes(fill=value), colour=NA) +
    scale_fill_viridis(name="") +
    labs(title=i) +
    mapTheme()}

# Call this, Nick!
do.call(grid.arrange,c(mapList, ncol =2, top = "Nearest Neighbor risk Factors by Fishnet"))

final_net <-
  left_join(heroin_net, st_set_geometry(vars_net, NULL), by="uniqueID") 


# A digression into zoning.
# SF-20: Single Family - 20,000 SF lots
# SF-6: Single Family - 6,000 SF lots
# SF-4: Single Family - 4,000 SF lots
# SF-2: Single Family - 2,000 SF lots
# RMX: Residential Mixed - 1 to 3 units
# RM-2.0: Residential Multi-family - 2,000 SF of land per unit
# RM-1.2: Residential Multi-family - 1,200 SF of land per unit
# RM-0.7: Residential Multi-family - 700 SF of land per unit
# OL: Office Limited
# CN-P: Commercial Neighborhood - Pedestrian
# CN-M: Commercial Neighborhood - Mixed
# CC-P: Commercial Community - Pedestrian
# CC-M: Commercial Community - Mixed
# CC-A: Commercial Community - Auto-oriented
# CG-A: Commercial General - Auto-oriented
# ML: Manufacturing Limited
# MG: Manufacturing General
# PD: Planned Development

zoning <- zoning %>% mutate(zone_type =
                              if_else(grepl("SF-", ZONE_TYPE) == TRUE, "Single_Family",
                                      if_else(grepl("RM", ZONE_TYPE) == TRUE, "Residential_MultiFamOrMixed",
                                              if_else(grepl("RF", ZONE_TYPE) == TRUE, "Riverfront",
                                                      if_else(grepl("RM-0.7", ZONE_TYPE) == TRUE, "Residential_MultiFamOrMixed",
                                                              if_else(grepl("OL", ZONE_TYPE) == TRUE, "Office",
                                                                      if_else(grepl("OG", ZONE_TYPE) == TRUE, "Office",
                                                                              if_else(grepl("C", ZONE_TYPE) == TRUE, "Commercial",
                                                                                      if_else(grepl("ML", ZONE_TYPE) == TRUE, "Manufacturing",
                                                                                              if_else(grepl("MA", ZONE_TYPE) == TRUE, "Manufacturing",
                                                                                                      if_else(grepl("MG", ZONE_TYPE) == TRUE, "Manufacturing",
                                                                                                              if_else(grepl("ME", ZONE_TYPE) == TRUE, "Manufacturing",
                                                                                                                      if_else(grepl("PD", ZONE_TYPE) == TRUE, "Planned_Development",
                                                                                                                              if_else(grepl("PR", ZONE_TYPE) == TRUE, "Park_Or_Recreational",
                                                                                                                                      if_else(grepl("UM", ZONE_TYPE) == TRUE, "Urban",
                                                                                                                                              if_else(grepl("DD", ZONE_TYPE) == TRUE, "Downtown_District",
                                                                                                                                                      if_else(grepl("IR", ZONE_TYPE) == TRUE, "Institutional_Residential",
                                                                                                                                                              if_else(grepl("T4", ZONE_TYPE) == TRUE, "Urban",
                                                                                                                                                                      if_else(grepl("T5", ZONE_TYPE) == TRUE, "Urban",
                                                                                                                                                                              if_else(grepl("T3", ZONE_TYPE) == TRUE, "Suburban",
                                                                                                                                                                                      NA_character_))))))))))))))))))))

# Adding overlapping IDs, like neighborhood or zoning.
# Testing with neighborhood, zoning, and census tract data.
final_net <-
  st_centroid(final_net) %>%
  st_join(., dplyr::select(acsData_props %>% dplyr::mutate(prop_poverty = estimate), prop_poverty)) %>%
  st_join(., dplyr::select(sna, SNA_NAME)) %>%
  st_join(., dplyr::select(zoning, zone_type)) %>%
  st_set_geometry(NULL) %>%
  left_join(dplyr::select(final_net, geometry, uniqueID)) %>%
  st_sf() %>%
  na.omit()

# For some reason, final_net has lots of duplicated values.
final_net <- final_net[!duplicated(final_net),] 

dplyr::select(final_net, SNA_NAME, zone_type) %>%
  gather(Variable, Value, -geometry) %>%
  ggplot() +
  geom_sf(aes(fill = Value)) +
  facet_wrap(~Variable) +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Aggregate Areas") +
  mapTheme() + theme(legend.position = "none")

# Moran's I stuff.

final_net.nb <- poly2nb(final_net, queen=TRUE)
final_net.weights <- nb2listw(final_net.nb, style="W", zero.policy=TRUE)

final_net.localMorans <- 
  cbind(
    as.data.frame(localmoran(final_net$countHeroinResponses, final_net.weights)),
    as.data.frame(final_net, NULL)) %>% 
  st_sf() %>%
  dplyr::select(HeroinResponse_Count = countHeroinResponses, 
                Local_Morans_I = Ii, 
                P_Value = `Pr(z > 0)`) %>%
  mutate(Significant_Hotspots = ifelse(P_Value <= 0.05, 1, 0)) %>%
  gather(Variable, Value, -geometry)

vars <- unique(final_net.localMorans$Variable)
varList <- list()

for(i in vars){
  varList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(final_net.localMorans, Variable == i), aes(fill = Value), colour=NA) +
    scale_fill_viridis(name="") +
    labs(title=i) +
    mapTheme()}

# This needs space to generate correctly - make sure to expand the Plots window when running.
do.call(grid.arrange,c(varList, ncol = 4, top = "Local Morans I statistics, Heroin Responses"))

# Distance to significant heroin hotspots
final_net <-
  final_net %>% 
  mutate(heroin.isSig = ifelse(localmoran(final_net$countHeroinResponses, 
                                            final_net.weights)[,5] <= 0.0000001, 1, 0)) %>%
  mutate(heroin.isSig.dist = nn_function(st_coordinates(st_centroid(final_net)),
                                           st_coordinates(st_centroid(
                                             filter(final_net, heroin.isSig == 1))), 1 ))

ggplot() + 
  geom_sf(data = final_net, aes(fill = heroin.isSig.dist)) +
  scale_fill_viridis() +
  labs(title = "Distance to highly significant heroin response hotspots") +
  mapTheme()

# The next step is to build some faceted correlation plots.  The relevant code is under 
# "Both features for a given risk factor cannot be included in the model because of co-linearity. This plot and the correlation coefficients help us understand which type of feature for a given risk factor should be included."
# in the burglary markdown Ken gave us.  I'll omit it here.

ggplot(final_net, aes(countHeroinResponses)) + 
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of heroin response by grid cell")

# Ken's markdown also includes an additional set of regression variables called reg.ss.vars.
# He includes them so that he can compare the two models - we presumably will have chosen
# a model that we prefer, so I'll omit that one.  There is no need to run a time-consuming
# cross-validation on two models.
reg.vars <- c("abandoned_vehicles.nn", "vacant_buildings.nn", "funTimes.nn", "SNA_NAME", "zone_type")

crossValidate <- function(dataset, id, dependentVariable, indVariables) {
  
  allPredictions <- data.frame()
  cvID_list <- unique(dataset[[id]])
  
  for (i in cvID_list) {
    
    thisFold <- i
    cat("This hold out fold is", thisFold, "\n")
    
    fold.train <- filter(dataset, dataset[[id]] != thisFold) %>% as.data.frame() %>% 
      dplyr::select(id, geometry, indVariables, dependentVariable)
    fold.test  <- filter(dataset, dataset[[id]] == thisFold) %>% as.data.frame() %>% 
      dplyr::select(id, geometry, indVariables, dependentVariable)
    
    regression <-
      glm(countHeroinResponses ~ ., family = "poisson", 
          data = fold.train %>% 
            dplyr::select(-geometry, -id))
    
    thisPrediction <- 
      mutate(fold.test, Prediction = predict(regression, fold.test, type = "response"))
    
    allPredictions <-
      rbind(allPredictions, thisPrediction)
    
  }
  return(st_sf(allPredictions))
}

reg.cv <- crossValidate(
  dataset = final_net,
  id = "cvID",
  dependentVariable = "countHeroinResponses",
  indVariables = reg.vars) %>%
  dplyr::select(cvID = cvID, countHeroinResponses, Prediction, geometry)

reg.spatialCV <- crossValidate(
  dataset = final_net,
  id = "SNA_NAME",
  dependentVariable = "countHeroinResponses",
  indVariables = reg.vars) %>%
  dplyr::select(cvID = SNA_NAME, countHeroinResponses, Prediction, geometry)

reg.summary <- 
  rbind(
    mutate(reg.cv,           Error = Prediction - countHeroinResponses,
           Regression = "Random k-fold CV"),
    
    mutate(reg.spatialCV,    Error = Prediction - countHeroinResponses,
           Regression = "Spatial LOGO-CV")) %>%
  st_sf() 

grid.arrange(
  reg.summary %>%
    ggplot() +
    geom_sf(aes(fill = Prediction)) +
    facet_wrap(~Regression) +
    scale_fill_viridis() +
    labs(title = "Predicted heroin responses by Regression") +
    mapTheme() + theme(legend.position="bottom"),
  
  filter(reg.summary, Regression == "Random k-fold CV") %>%
    ggplot() +
    geom_sf(aes(fill = countHeroinResponses)) +
    scale_fill_viridis() +
    labs(title = "Observed heroin responses\n") +
    mapTheme() + theme(legend.position="bottom"), ncol = 2)

filter(reg.summary, Regression == "Spatial LOGO-CV") %>%
  ggplot() +
  geom_sf(aes(fill = Error)) +
  facet_wrap(~Regression) +
  scale_fill_viridis() +
  labs(title = "Heroin response errors by Regression") +
  mapTheme()

st_set_geometry(reg.summary, NULL) %>%
  group_by(Regression) %>% 
  summarize(MAE = round(mean(abs(Prediction - countHeroinResponses), na.rm = T),2),
            SD_MAE = round(sd(abs(Prediction - countHeroinResponses), na.rm = T),2)) %>% 
  kable(caption = "MAE by regression") %>%
  kable_styling("striped", full_width = F) %>%
  row_spec(1, color = "black", background = "#FDE725FF")
  row_spec(2, color = "black", background = "#FDE725FF") 

  st_set_geometry(reg.summary, NULL) %>%
    group_by(Regression) %>%
    mutate(heroinResponse_Decile = ntile(countHeroinResponses, 10)) %>%
    group_by(Regression, burglary_Decile) %>%
    summarize(meanObserved = mean(countHeroinResponses, na.rm=T),
              meanPrediction = mean(Prediction, na.rm=T)) %>%
    gather(Variable, Value, -Regression, -heroinResponse_Decile) %>%          
    ggplot(aes(heroinResponse_Decile, Value, shape = Variable)) +
    geom_point(size = 2) + geom_path(aes(group = heroinResponse_Decile), colour = "black") +
    scale_shape_manual(values = c(2, 17)) +
    facet_wrap(~Regression) + xlim(0,10) +
    labs(title = "Predicted and observed heroin responses by observed heroin responses decile")
  
  # At this point, there's some analysis of the model's accuracy by race of census tract.
  # I will omit that for the time being, since it requires loading in census data.
 
  final_reg <- 
    filter(reg.summary, Regression == "Spatial LOGO-CV") %>%
    mutate(uniqueID = rownames(.))
  
  heroin_ppp <- as.ppp(st_coordinates(heroin_ems18), W = st_bbox(final_net))
  heroin_KD.1000 <- spatstat::density.ppp(heroin_ppp, 1000)
  heroin_KD.1500 <- spatstat::density.ppp(heroin_ppp, 1500)
  heroin_KD.2000 <- spatstat::density.ppp(heroin_ppp, 2000)
  heroin_KD.df <- rbind(
    mutate(data.frame(rasterToPoints(mask(raster(heroin_KD.1000), as(sna, 'Spatial')))), Legend = "1000 Ft."),
    mutate(data.frame(rasterToPoints(mask(raster(heroin_KD.1500), as(sna, 'Spatial')))), Legend = "1500 Ft."),
    mutate(data.frame(rasterToPoints(mask(raster(heroin_KD.2000), as(sna, 'Spatial')))), Legend = "2000 Ft.")) 
  
  heroin_KD.df$Legend <- factor(heroin_KD.df$Legend, levels = c("1000 Ft.", "1500 Ft.", "2000 Ft."))
  
  ggplot(data=heroin_KD.df, aes(x=x, y=y)) +
    geom_raster(aes(fill=layer)) + 
    facet_wrap(~Legend) +
    coord_sf(crs=st_crs(final_net)) + 
    scale_fill_viridis() +
    mapTheme()
  
  heroin_ppp <- as.ppp(st_coordinates(heroin_ems18), W = st_bbox(final_net))
  heroin_KD <- spatstat::density.ppp(heroin_ppp, 1000)
  
  as.data.frame(heroin_KD) %>%
    st_as_sf(coords = c("x", "y"), crs = st_crs(final_net)) %>%
    aggregate(., final_net, mean) %>%
    ggplot() +
    geom_sf(aes(fill=value)) +
    geom_sf(data = sample_n(heroin_ems18, 1500), size = .5) +
    scale_fill_viridis() +
    mapTheme()
  
  # Compute kernel density
  heroin_ppp <- as.ppp(st_coordinates(heroin_ems18), W = st_bbox(final_net))
  heroin_KD <- spatstat::density.ppp(heroin_ppp, 1000)
  # Convert kernel density to grid cells taking the mean
  heroin_KDE_sf <- as.data.frame(heroin_KD) %>%
    st_as_sf(coords = c("x", "y"), crs = st_crs(final_net)) %>%
    aggregate(., final_net, mean) %>%
    #Mutate the Risk_Category field as defined below.
    mutate(label = "Kernel Density",
           Risk_Category = ntile(value, 100),
           Risk_Category = case_when(
             Risk_Category >= 90 ~ "90% to 100%",
             Risk_Category >= 70 & Risk_Category <= 89 ~ "70% to 89%",
             Risk_Category >= 50 & Risk_Category <= 69 ~ "50% to 69%",
             Risk_Category >= 30 & Risk_Category <= 49 ~ "30% to 49%",
             Risk_Category >= 1 & Risk_Category <= 29 ~ "1% to 29%")) %>%
    #Bind to a layer where test set crime counts are spatially joined to the fisnnet.
    bind_cols(
      aggregate(
        dplyr::select(heroin_ems18) %>% mutate(heroinCount = 1), ., length) %>%
        mutate(heroinCount = replace_na(heroinCount, 0))) %>%
    #Select the fields we need
    dplyr::select(label, Risk_Category, heroinCount)
  
  # Repeat for the risk predictions (our model, not kernel density)
  heroin_risk_sf <-
    filter(final_reg, Regression == "Spatial LOGO-CV") %>%
    mutate(label = "Risk Predictions",
           Risk_Category = ntile(Prediction, 100),
           Risk_Category = case_when(
             Risk_Category >= 90 ~ "90% to 100%",
             Risk_Category >= 70 & Risk_Category <= 89 ~ "70% to 89%",
             Risk_Category >= 50 & Risk_Category <= 69 ~ "50% to 69%",
             Risk_Category >= 30 & Risk_Category <= 49 ~ "30% to 49%",
             Risk_Category >= 1 & Risk_Category <= 29 ~ "1% to 29%")) %>%
    bind_cols(
      aggregate(
        dplyr::select(heroin_ems18) %>% mutate(heroinCount = 1), ., length) %>%
        mutate(heroinCount = replace_na(heroinCount, 0))) %>%
    dplyr::select(label,Risk_Category, heroinCount)
  
  rbind(heroin_KDE_sf, heroin_risk_sf) %>%
    gather(Variable, Value, -label, -Risk_Category, -geometry) %>%
    ggplot() +
    geom_sf(aes(fill = Risk_Category), colour = NA) +
    geom_sf(data = sample_n(heroin_ems18, 1500), size = .1, colour = "black") +
    facet_wrap(~label, ) +
    scale_fill_viridis(discrete = TRUE) +
    labs(title="Comparison of Kernel Density and Risk Predictions",
         subtitle="Relative to test set points (in black)") +
    mapTheme()
  
  rbind(heroin_KDE_sf, heroin_risk_sf) %>%
    st_set_geometry(NULL) %>%
    gather(Variable, Value, -label, -Risk_Category) %>%
    group_by(label, Risk_Category) %>%
    summarize(countHeroinResponses = sum(Value)) %>%
    ungroup() %>%
    group_by(label) %>%
    mutate(Rate_of_test_set_crimes = countHeroinResponses / sum(countHeroinResponses)) %>%
    ggplot(aes(Risk_Category,Rate_of_test_set_crimes)) +
    geom_bar(aes(fill=label), position="dodge", stat="identity") +
    scale_fill_viridis(discrete = TRUE)
######################################################################################
##################################
# Maps for exploratory analysis slides in the powerpoint 
##################################
######################################################################################

# Geographic context: Where is Cincinnati? 
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) # state boundaries
world <- ne_countries(scale = "medium", returnclass = "sf") # countries/states
states <- cbind(states, st_coordinates(st_centroid(states))) # state names
states$ID <- toTitleCase(states$ID) # convert state names to proper nouns

ggplot() +
  geom_sf(data = states, fill = NA) + 
  geom_text(data = states, aes(X, Y, label = ID), size = 4) +
  geom_sf(data = cincinnati, aes(fill = BND_NAME), alpha=0.3, color="#25CB10") +
  scale_fill_manual(values = c("#25CB10")) +
  coord_sf(xlim = c(-92, -79), ylim = c(36.15, 43.5), expand = FALSE) +
  labs(title = "Cincinnati, OH", subtitle = "In the Midwest context") +
  theme(legend.position = "none") +
  mapTheme()

# Cincinnati close-up
ggplot() +
  geom_sf(data = hamilton, fill = NA, color = "gray") +
  geom_sf(data = cincinnati, fill = "darkgray", color = "gray") +
  geom_sf(data = hamilton_streets, color = "gray") +
  geom_sf(data = heroin_ems17[1:1000,], size = 1, color = "#25CB10") +
  labs(title="Cincinnati, OH", subtitle = "And the surrounding Hamilton County, OH") +
  mapTheme()

# EMS calls related to heroin overdose in Cincinnati, over time:
ggplot() +
  geom_sf(data = cincinnati, fill = NA) +
  geom_sf(data = heroin_ems15, size = 1, color = "#8FA108") +
  labs(title="Heroin overdose events 2015", subtitle = "Cincinnati, OH") +
  theme(legend.position = "none") +
  mapTheme()

ggplot() +
  geom_sf(data = cincinnati, fill = NA) +
  geom_sf(data = heroin_ems17, size = 1, color = "#25CB10") +
  labs(title="Heroin overdose events 2017", subtitle = "Cincinnati, OH") +
  theme(legend.position = "none") +
  mapTheme()

ggplot() +
  geom_sf(data = cincinnati, fill = NA) +
  geom_sf(data = heroin_ems19, size = 1, color = "#5AB60C") +
  labs(title="Heroin overdose events 2019", subtitle = "Cincinnati, OH") +
  theme(legend.position = "none") +
  mapTheme()

# Heroin overdose vs. Naloxone distribution sites in Hamilton County:
ggplot() +
  geom_sf(data = hamilton, fill = NA, color = "gray") +
  geom_sf(data = hamilton_streets, color = "gray") +
  geom_sf(data = cincinnati, fill = NA) +
  geom_sf(data = heroin_ems17, size = 1, color = "#25CB10") +
  geom_sf(data = nalox_sites, size = 3, color = "#FA7800") +
  scale_color_manual(labels = c("Heroin overdose", "Naloxone distribution site")) +
  labs(title="Supply vs. Demand:\nLocation of Heroin Overdoses vs. Naloxone Distribution Sites",
       subtitle = "Cincinnati, OH") +
  mapTheme()

# Covariates
censusRace_props <- censusRace %>% dplyr::filter(variable == "prop_white" | 
                                                   variable == "prop_black")
censusRace_props <- censusRace_props[cincinnati,]

censusRace_props %>% ggplot(aes(fill = value)) +
  facet_wrap(~variable) +
  geom_sf(color = NA) +
  coord_sf(crs = 26915) + 
  scale_fill_viridis_c() + mapTheme()

acsData_props <- acsData %>% dplyr::filter(variable == "prop_poverty" |
                                             variable == "prop_bach_or_higher")
acsData_props <- acsData_props[cincinnati, ]
acsData_props %>% ggplot(aes(fill = estimate)) +
  facet_wrap(~variable) +
  geom_sf(color = NA) +
  coord_sf(crs = 26915) + 
  scale_fill_viridis_c() + mapTheme()

acsYoungMen <- acsData %>%
  spread(key = variable, value = estimate) 
acsYoungMen <- acsYoungMen %>% mutate(
  males_18to24 = males_18_19+males_20+males_21+males_22_24) %>%
  gather("variable", "estimate", -GEOID, -NAME, -geometry) %>%
  dplyr::select(GEOID, NAME, variable, estimate, geometry) %>%
  dplyr::filter(variable == "males_18to24")
acsYoungMen <- acsYoungMen[cincinnati, ]
acsYoungMen %>% ggplot(aes(fill = estimate)) +
  facet_wrap(~variable) +
  geom_sf(color = NA) +
  coord_sf(crs = 26915) + 
  scale_fill_viridis_c() + mapTheme()

acsMedHome <- acsData %>% dplyr::filter(variable == "median_home_value")
acsMedHome <- acsMedHome[cincinnati, ]
acsMedHome %>% ggplot(aes(fill = estimate)) +
  facet_wrap(~variable) +
  geom_sf(color = NA) +
  coord_sf(crs = 26915) + 
  scale_fill_viridis_c() + mapTheme()

ggplot() +
  geom_sf(data = cincinnati, fill = NA) +
  geom_sf(data = abandoned_vehicles, size = 1, color = "#404788FF") +
  labs(title="Abandoned vehicles", subtitle = "Cincinnati, OH") +
  theme(legend.position = "none") +
  mapTheme()

ggplot() +
  geom_sf(data = cincinnati, fill = NA) +
  geom_sf(data = complainedTrash, size = 1, color = "#29AF7FFF") +
  labs(title="Trash complaints", subtitle = "Cincinnati, OH") +
  theme(legend.position = "none") +
  mapTheme()
