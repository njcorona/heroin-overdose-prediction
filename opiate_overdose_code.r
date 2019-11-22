###################################################
# Project: MUSA 507 Opiate Overdose Preduction
# Names: Claire Allen-Platt and Nicolas Corona
# Date: December 20, 2019
###################################################

# Load packages, data, colors etc ---------------------------------

library(here)
library(tidyverse)
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
# Still figuring out how to scrape this data -- but there is a map of naloxone access 
# sites in Hamilton County on this website (https://takechargeohio.org/map) and a list of
# addiction and substance use disorder treatment providers (specifically for opioids or
# heroin) on this site 
# (https://www.emeraldjennyfoundation.org/listings/?fwp_location=39.1031182%2C-84.51201960000003%2C25%2CCincinnati%252C%2520OH%252C%2520USA&fwp_category=opioid-or-heroin-specific-addiction)

######################################################################################
################################
# Loading data from RDS files. #
################################
######################################################################################

ems <- read_rds("ems.RDS") %>% as_tibble() # Emergency services calls.
drugs_cops <- read_rds("drugs_cops.RDS") %>% as_tibble() # TODO: this is a subset of all police data. confirm whether this is a reasonable subset or we want to pull in more data.
heroin_cops <- read_rds("heroin_cops.RDS") %>% as_tibble() # TODO: same as above - heroin is a subset of drugs dataset.
#cops <- read_rds("cops.RDS") %>% as_tibble() # Police calls (proactive and reactive)
  # Note -- I'm not seeing this dataset on Git. It looks like it's 2.9 million rows. 
  # Is it helpful enough to justify the computational time for loading it into R?
crimes <- read_rds("crimes.RDS") %>% as_tibble() # Crimes reported.
c311 <- read_rds("c311.RDS") %>% as_tibble() # 311 calls.
code <- read_rds("code.RDS") %>% as_tibble() # Code enforcement.
cincinnati <- read_rds("cincinnati.RDS") # Cincinnati city boundary shapefile.
sna <- read_rds("sna.RDS") # Cincinnati SNA (Statistical Neighborhood Approximations) Boundaries
districts_cops <- read_rds("districts_cops.RDS") # Police districts.
firehouses <- read_rds("firehouses.RDS") # Firehouses.
sheriff <- read_rds("sheriff.RDS") # Sheriff stations.

######################################################################################
############################################
# Basic boundary and point visualizations. #
############################################
######################################################################################

# # Cincinnati border.
# ggplot() + geom_sf(data = cincinnati)
# 
# # Cincinnati neighborhoods.
# ggplot() + geom_sf(data = cincinnati) + geom_sf(data = sna)
# 
# # Cincinatti police districts.
# ggplot() + geom_sf(data = cincinnati) + geom_sf(data = districts_cops)
# 
# # Hamilton County firehouses.
# ggplot() + geom_sf(data = cincinnati) + geom_sf(data = firehouses)
# 
# # Hamilton County sheriff stations.
# ggplot() + geom_sf(data = cincinnati) + geom_sf(data = sheriff)
#
# # Hamilton County naloxone distribution sites 
# ggplot() + geom_sf(data = cincinnati) + geom_sf(data = nalox)

######################################################################################
#########################################
# Basic cleaning and data manipulation. #
#########################################
######################################################################################

ems <- ems[which(!is.na(ems$latitude_x)), ] # TODO: see whether NAs are distributed spatially or across time.
ems$year <- str_sub(ems$create_time_incident, start = 1, end = 4)
ems15 <- ems[which(ems$year == "2015"),]
ems17 <- ems[which(ems$year == "2017"),]

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
  st_as_sf(coords = c("longitude_x", "latitude_x"), crs = 4326, agr = "constant")


ggplot() + 
  geom_sf(data = cincinnati) + 
  geom_sf(data = heroin_ems[1:1000,]) +
  mapTheme()












#############
#############
# Note: Most of this code is what I submitted for the risk analysis for crimes.  
# I'll leave it in so I can adapt for this homework.
#############
#############

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