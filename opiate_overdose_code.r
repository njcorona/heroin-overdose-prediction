###################################################
# Project: MUSA 507 Opiate Overdose Preduction
# Names: Claire Allen-Platt and Nicolas Corona
# Date: December 20, 2019
###################################################

# Load packages, data, colors etc ---------------------------------

library(here)
library(tidyverse)
library(dplyr)
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
# zoning <- st_read("https://opendata.arcgis.com/datasets/b43c6f6671d54da298fd6110a3088557_11.geojson")
# saveRDS(zoning, "zoning.RDS")
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
acsData <- st_read("acsData.shp") # lots of place-based covariates from American Community Survey (see the code above, commented out, for variables list - based it on the Li et al article in the literature folder on Git, which is a spatial prediction model of heroin overdose in Cincy)
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

# Special business licenses (promising? still looking for fast food restaurants)
biz_licenses <- read_csv("Business_Licenses.csv", col_names = T) %>%
  filter(!is.na(LATITUDE)) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, agr = "constant") %>%
  st_transform(3735)

# Weather measured from Cincinnati airport.
weather <- read_csv("weather.csv")
weather <- weather %>% dplyr::select(STATION, DATE, HourlyDryBulbTemperature, HourlyPrecipitation, SOURCE, REPORT_TYPE)

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
ggplot() + geom_sf(data = hamilton) + geom_sf(data = zoning) + mapTheme()

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
# Code from markdown.
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

crime_net <- 
  narcotics %>% 
  dplyr::select() %>% 
  mutate(countNarcotics = 1) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(countNarcotics = ifelse(is.na(countNarcotics), 0, countNarcotics),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), size=nrow(fishnet), replace = TRUE))

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
  labs(title="Cincinnati and Hamilton County, OH") +
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

######################################################################################
#############
# Note: Most of this code is what I submitted for the risk analysis for crimes.  
# I'll leave it in so I can adapt for this homework.
#############
######################################################################################

chicagoBoundary <- 
  st_read("chicagoBoundary.shp") %>%
  st_transform(crs=102271) 

fishnet <- 
  st_make_grid(chicagoBoundary, cellsize = 500) %>%
  st_sf()

# ggplot() + 
#   geom_sf(data=chicagoBoundary, fill=NA, colour="black") +
#   geom_sf(data=fishnet, fill=NA, colour="black") +
#   labs(title = "Chicago and the fishnet") +
#   mapTheme()

fishnet <- 
  fishnet[chicagoBoundary,] %>%
  mutate(uniqueID = rownames(.)) %>%
  dplyr::select(uniqueID)

# ggplot() +
#   geom_sf(data=fishnet) +
#   labs(title = "Fishnet in Chicago") +
#   mapTheme()

# burglaries <- 
#   read.socrata("https://data.cityofchicago.org/Public-Safety/Crimes-2017/d62x-nvdr") %>% 
#   filter(Primary.Type == "BURGLARY" & 
#            Description == "FORCIBLE ENTRY") %>%
#   mutate(x = gsub("[()]", "", Location)) %>%
#   separate(x,into= c("Y","X"), sep=",") %>%
#   mutate(X = as.numeric(X),
#          Y = as.numeric(Y)) %>% 
#   na.omit %>%
#   st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant")%>%
#   st_transform(102271) %>% 
#   distinct()

# saveRDS(burglaries, "burglaries.rds")
burglaries <- read_rds("burglaries.rds")

# narcotics <-
#   read.socrata("https://data.cityofchicago.org/Public-Safety/Crimes-2017/d62x-nvdr") %>%
#   filter(Primary.Type == "NARCOTICS") %>%
#   mutate(x = gsub("[()]", "", Location)) %>%
#   separate(x,into= c("Y","X"), sep=",") %>%
#   mutate(X = as.numeric(X),
#          Y = as.numeric(Y)) %>%
#   na.omit %>%
#   st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant")%>%
#   st_transform(102271) %>%
#   distinct()

# saveRDS(narcotics, "narcotics.rds")
narcotics <- read_rds("narcotics.rds")

# This is unresolved - not summing narcotics appropriately.
crime_net <- 
  narcotics %>% 
  dplyr::select() %>% 
  mutate(countNarcotics = 1) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(countNarcotics = ifelse(is.na(countNarcotics), 0, countNarcotics),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), size=nrow(fishnet), replace = TRUE))

# ggplot() + # Takes ~ 2 min to generate
#   geom_sf(data = chicagoBoundary) +
#   geom_sf(data = burglaries, colour="red", size=0.1, show.legend = "point") +
#   labs(title= "Burglaries, Chicago - 2017") +
#   mapTheme()

# abandonCars <- 
#   read.socrata("https://data.cityofchicago.org/Service-Requests/311-Service-Requests-Abandoned-Vehicles/3c9v-pnva") %>%
#   mutate(year = substr(creation_date,1,4)) %>%
#   filter(year == "2017") %>%
#   dplyr::select(Y = latitude, X = longitude) %>%
#   na.omit() %>%
#   st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
#   st_transform(st_crs(fishnet)) %>%
#   mutate(Legend = "Abandoned_Cars")
# 
# abandonBuildings <- 
#   read.socrata("https://data.cityofchicago.org/Service-Requests/311-Service-Requests-Vacant-and-Abandoned-Building/7nii-7srd") %>%
#   mutate(year = substr(date_service_request_was_received,1,4)) %>%
#   filter(year == "2017") %>%
#   dplyr::select(Y = latitude, X = longitude) %>%
#   na.omit() %>%
#   st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
#   st_transform(st_crs(fishnet)) %>%
#   mutate(Legend = "Abandoned_Buildings")
# 
# graffiti <- 
#   read.socrata("https://data.cityofchicago.org/Service-Requests/311-Service-Requests-Graffiti-Removal-Historical/hec5-y4x5") %>%
#   mutate(year = substr(creation_date,1,4)) %>%
#   filter(year == "2017") %>%
#   filter(where_is_the_graffiti_located_ == "Front" |
#            where_is_the_graffiti_located_ == "Rear" | where_is_the_graffiti_located_ == "Side") %>%
#   dplyr::select(Y = latitude, X = longitude) %>%
#   na.omit() %>%
#   st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
#   st_transform(st_crs(fishnet)) %>%
#   mutate(Legend = "Graffiti")
# 
# streetLightsOut <- 
#   read.socrata("https://data.cityofchicago.org/Service-Requests/311-Service-Requests-Street-Lights-All-Out/zuxi-7xem") %>%
#   mutate(year = substr(creation_date,1,4)) %>%
#   filter(year == "2017") %>%
#   dplyr::select(Y = latitude, X = longitude) %>%
#   na.omit() %>%
#   st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
#   st_transform(st_crs(fishnet)) %>%
#   mutate(Legend = "Street_Lights_Out")
# 
# sanitation <-
#   read.socrata("https://data.cityofchicago.org/Service-Requests/311-Service-Requests-Sanitation-Code-Complaints-Hi/me59-5fac") %>%
#   mutate(year = substr(creation_date,1,4)) %>%
#   filter(year == "2017") %>%
#   dplyr::select(Y = latitude, X = longitude) %>%
#   na.omit() %>%
#   st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
#   st_transform(st_crs(fishnet)) %>%
#   mutate(Legend = "Sanitation")
# 
# liquorRetail <- 
#   read.socrata("https://data.cityofchicago.org/Community-Economic-Development/Business-Licenses-Cur   rent-Liquor-and-Public-Places/nrmj-3kcf") %>%
#   filter(BUSINESS.ACTIVITY == "Retail Sales of Packaged Liquor") %>%
#   dplyr::select(Y = LATITUDE, X = LONGITUDE) %>%
#   na.omit() %>%
#   st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
#   st_transform(st_crs(fishnet)) %>%
#   mutate(Legend = "Liquor_Retail")
# 
# neighborhoods <- 
#   st_read("https://raw.githubusercontent.com/blackmad/neighborhoods/master/chicago.geojson") %>%
#   st_transform(st_crs(fishnet)) 

# saveRDS(abandonCars, "abandonCars.rds")
# saveRDS(abandonBuildings, "abandonBuildings.rds")
# saveRDS(graffiti, "graffiti.rds")
# saveRDS(streetLightsOut, "streetLightsOut.rds")
# saveRDS(sanitation, "sanitation.rds")
# saveRDS(liquorRetail, "liquorRetail.rds")
# saveRDS(neighborhoods, "neighborhoods.rds")
abandonCars <- read_rds("abandonCars.rds")
abandonBuildings <- read_rds("abandonBuildings.rds")
graffiti <- read_rds("graffiti.rds")
streetLightsOut <- read_rds("streetLightsOut.rds")
sanitation <- read_rds("sanitation.rds")
liquorRetail <- read_rds("liquorRetail.rds")
neighborhoods <- read_rds("neighborhoods.rds")

# health_clinics <- # Only STI and Mental Health.  WIC (women and children clinics) excluded.
#   read_csv("Chicago_Department_of_Public_Health_Clinic_Locations.csv") %>%
#   dplyr::select(Y = Latitude, X = Longitude) %>%
#   na.omit() %>%
#   st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
#   st_transform(st_crs(fishnet)) %>%
#   mutate(Legend = "Public_Health_Clinics")
# 
# cooling_centers <-
#   read.socrata("https://data.cityofchicago.org/resource/r23p-6uic.json") %>%
#   dplyr::select(Y = location.latitude, X = location.longitude) %>%
#   na.omit() %>%
#   st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
#   st_transform(st_crs(fishnet)) %>%
#   mutate(Legend = "Cooling_Centers")
# 
# warming_centers <-
#   read.socrata("https://data.cityofchicago.org/resource/5a76-tqs9.json") %>%
#   dplyr::select(Y = location.latitude, X = location.longitude) %>%
#   na.omit() %>%
#   st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
#   st_transform(st_crs(fishnet)) %>%
#   mutate(Legend = "Warming_Centers")
# 
# police_stations <-
#   read.socrata("https://data.cityofchicago.org/resource/gkur-vufi.json") %>%
#   dplyr::select(Y = location.latitude, X = location.longitude) %>%
#   na.omit() %>%
#   st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
#   st_transform(st_crs(fishnet)) %>%
#   mutate(Legend = "Police_Stations")

# saveRDS(cooling_centers, "cooling_centers.rds")
# saveRDS(warming_centers, "warming_centers.rds")
# saveRDS(police_stations, "police_stations.rds")
# saveRDS(health_clinics, "health_clinics.rds")
cooling_centers <- read_rds("cooling_centers.rds")
warming_centers <- read_rds("warming_centers.rds")
police_stations <- read_rds("police_stations.rds")
health_clinics <- read_rds("health_clinics.rds")

# # Map of new predictive features
# ggplot() +
#   geom_sf(data=chicagoBoundary) +
#   geom_sf(data = rbind(abandonCars,streetLightsOut,abandonBuildings,
#                        liquorRetail, graffiti, sanitation, cooling_centers,
#                        warming_centers, police_stations, health_clinics),
#           size = .1) +
#   facet_wrap(~Legend, ncol = 3) +
#   labs(title = "Risk Factors") +  
#   mapTheme()

# Note that cooling and warming centers... are the same thing!
# Our two will be police stations and cooling centers, with health clinics as a bonus.

vars_net <- 
  rbind(abandonCars,streetLightsOut,abandonBuildings,
        liquorRetail, graffiti, sanitation, cooling_centers,
        police_stations, health_clinics) %>%
  st_join(., fishnet, join=st_within) %>%
  st_set_geometry(NULL) %>%
  group_by(uniqueID, Legend) %>%
  summarize(count = n()) %>%
  full_join(fishnet) %>%
  spread(Legend, count, fill=0) %>%
  st_sf() %>%
  dplyr::select(-`<NA>`) %>%
  na.omit()

vars_net

vars_net.long <- 
  vars_net %>%
  gather(Variable, value, -geometry, -uniqueID)

vars <- unique(vars_net.long$Variable)
mapList <- list()

for(i in vars[c(3,6,7)]){
  mapList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(vars_net.long, Variable == i), aes(fill=value), colour=NA) +
    scale_fill_viridis(name="") +
    labs(title=i) +
    mapTheme()}

# do.call(grid.arrange,c(mapList, ncol = 3, top = "Risk Factors by Fishnet"))

vars_net$Abandoned_Buildings.nn =
  nn_function(st_coordinates(st_centroid(vars_net)), st_coordinates(abandonBuildings), 3)

vars_net$Abandoned_Cars.nn =
  nn_function(st_coordinates(st_centroid(vars_net)), st_coordinates(abandonCars), 3)

vars_net$Graffiti.nn =
  nn_function(st_coordinates(st_centroid(vars_net)), st_coordinates(graffiti), 3)

vars_net$Liquor_Retail.nn =
  nn_function(st_coordinates(st_centroid(vars_net)), st_coordinates(liquorRetail), 3)

vars_net$Street_Lights_Out.nn =
  nn_function(st_coordinates(st_centroid(vars_net)), st_coordinates(streetLightsOut), 3)

vars_net$Sanitation.nn =
  nn_function(st_coordinates(st_centroid(vars_net)), st_coordinates(sanitation), 3)

vars_net$Warming_Centers.nn =
  nn_function(st_coordinates(st_centroid(vars_net)), st_coordinates(warming_centers), 3)

vars_net$Police_Stations.nn =
  nn_function(st_coordinates(st_centroid(vars_net)), st_coordinates(police_stations), 1)

vars_net$Health_Clinics.nn =
  nn_function(st_coordinates(st_centroid(vars_net)), st_coordinates(health_clinics), 1)

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

do.call(grid.arrange,c(mapList, ncol =2, top = "Nearest Neighbor risk Factors by Fishnet"))

loopPoint <-
  neighborhoods %>%
  filter(name == "Loop") %>%
  st_centroid()

vars_net$loopDistance =
  st_distance(st_centroid(vars_net),loopPoint) %>%
  as.numeric() 

# ggplot() +
#   geom_sf(data=vars_net, aes(fill=loopDistance)) +
#   scale_fill_viridis() +
#   labs(title="Euclidean distance to The Loop") +
#   mapTheme() 

final_net <-
  left_join(crime_net, st_set_geometry(vars_net, NULL), by="uniqueID") 

final_net.localMorans <- 
  cbind(
    as.data.frame(localmoran(final_net$countBurglaries, final_net.weights)),
    as.data.frame(final_net, NULL)) %>% 
  st_sf() %>%
  dplyr::select(Burglary_Count = countBurglaries, 
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

do.call(grid.arrange,c(varList, ncol = 4, top = "Local Morans I statistics, Burglary"))
print("hi")