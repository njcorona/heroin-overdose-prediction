###################################################
# Project: MUSA 507 Opiate Overdose Preduction
# Names: Claire Allen-Platt and Nicolas Corona
# Date: December 20, 2019
###################################################

# Load packages, data, colors etc ---------------------------------

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
ems <- read_rds("ems.RDS")

# %>%
  # mutate(year = substr(creation_date,1,4)) %>%
  # filter(year == "2017") %>%
  # dplyr::select(Y = latitude, X = longitude) %>%
  # na.omit() %>%
  # st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  # st_transform(st_crs(fishnet)) %>%
  # mutate(Legend = "Abandoned_Cars")






















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