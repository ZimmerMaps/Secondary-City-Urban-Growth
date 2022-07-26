#   Title:      Secondary City Population Growth
#   Name:       Andy Zimmer
#   Contact:    azimmer@arizona.edu
#   Updated:    jan 2020
#   Purpose:    generate dataset for analysis containing variables for urban population through time and
#               covariates on climate, distance and proximal land-use

####   load packages    ###
rm(list=ls()) #clear and close
library(sf)
library(rnaturalearth)
library(tidyverse)
library(raster)
library(reshape2)
library(lubridate)
library(scales)
library(SciViews)
library(viridis)

####    load and prepare vector data   ####

#   load urban polygons for southern africa, from Tuholske et al (2019)
africa_urban_polygons <- st_read("/Users/azimmer/Documents/PhD Geography/Research/African Urban Growth/Africa Urban Polygons/AfricaUrbanPop-master/ERL_Data/GHS2015_polyFINAL_PopTotERLv2.shp")
polygon_crs <- st_crs(africa_urban_polygons) # save crs for later use
#plot(africa_urban_polygons)

#  add adm-2 shapefile for southern africa
southern_africa_adm2_shp <- st_read("/Users/azimmer/Documents/PhD Geography/Research/African Urban Growth/Shapefiles/admin_2_merged_southern_africa.shp")
southern_africa_adm2_shp_moll <- st_transform(southern_africa_adm2_shp, crs = polygon_crs)
#plot(southern_africa_adm2_shp_moll)

#   buffer both shapefiles to clean up any topology problems
southern_africa_adm2_shp_moll <- st_buffer(southern_africa_adm2_shp_moll, byid=TRUE, dist=0)
africa_urban_polygons <- st_buffer(africa_urban_polygons, byid=TRUE, dist=0)

#   clip urban polygons to southern africa
southern_africa_urban_polygons <- st_intersection(southern_africa_adm2_shp_moll, africa_urban_polygons)
#plot(southern_africa_urban_polygons)


####    load and prepare raster data ####

####   population data
#   ghs-pop data for 1975, 1990, 2000 and 2015 at 250m resolution (from 2015 data release)
ghs_1975 <- raster("/Users/azimmer/Documents/PhD Geography/Research/African Urban Growth/GHS Population/GHS POP (2015) @ 250m resolution/GHS_POP_GPW41975_GLOBE_R2015A_54009_250_v1_0/GHS_POP_GPW41975_GLOBE_R2015A_54009_250_v1_0.tif")
ghs_1990 <- raster("/Users/azimmer/Documents/PhD Geography/Research/African Urban Growth/GHS Population/GHS POP (2015) @ 250m resolution/GHS_POP_GPW41990_GLOBE_R2015A_54009_250_v1_0/GHS_POP_GPW41990_GLOBE_R2015A_54009_250_v1_0.tif")
ghs_2000 <- raster("/Users/azimmer/Documents/PhD Geography/Research/African Urban Growth/GHS Population/GHS POP (2015) @ 250m resolution/GHS_POP_GPW42000_GLOBE_R2015A_54009_250_v1_0/GHS_POP_GPW42000_GLOBE_R2015A_54009_250_v1_0.tif")
ghs_2015 <- raster("/Users/azimmer/Documents/PhD Geography/Research/African Urban Growth/GHS Population/GHS POP (2015) @ 250m resolution/GHS_POP_GPW42015_GLOBE_R2015A_54009_250_v1_0/GHS_POP_GPW42015_GLOBE_R2015A_54009_250_v1_0.tif")

#   zonal statistics to calculate population in each urban polygon at each time point
#   takes a little time to run
urban_pop_1975 <- raster::extract(x=ghs_1975, y=southern_africa_urban_polygons, method = 'simple', fun=sum, df=TRUE)
urban_pop_1990 <- raster::extract(x=ghs_1990, y=southern_africa_urban_polygons, method = 'simple', fun=sum, df=TRUE)
urban_pop_2000 <- raster::extract(x=ghs_2000, y=southern_africa_urban_polygons, method = 'simple', fun=sum, df=TRUE)
urban_pop_2015 <- raster::extract(x=ghs_2015, y=southern_africa_urban_polygons, method = 'simple', fun=sum, df=TRUE)

#   rename column names to match the feature id in the shapefile
urban_pop_1975$ID <- southern_africa_urban_polygons$FID
urban_pop_1990$ID <- southern_africa_urban_polygons$FID
urban_pop_2000$ID <- southern_africa_urban_polygons$FID
urban_pop_2015$ID <- southern_africa_urban_polygons$FID

#   add adm-2 from original shapefile
urban_pop_1975$adm2 <- southern_africa_urban_polygons$NAME_2
urban_pop_1990$adm2 <- southern_africa_urban_polygons$NAME_2
urban_pop_2000$adm2 <- southern_africa_urban_polygons$NAME_2
urban_pop_2015$adm2 <- southern_africa_urban_polygons$NAME_2

# merge these together to have population through time. Cannot rbind due to different population column names ('75, '90, '00, '15)
southern_africa_urban_pop <- merge(urban_pop_1975, urban_pop_1990, by=c("ID", "adm2"))
southern_africa_urban_pop <- merge(southern_africa_urban_pop, urban_pop_2000, by=c("ID", "adm2"))
southern_africa_urban_pop <- merge(southern_africa_urban_pop, urban_pop_2015, by=c("ID", "adm2"))

# change column names
names(southern_africa_urban_pop) <- c("FID", "adm_2", "start_pop", "1990", "2000", "2015") #start_pop to enable merge w shapefile
#view(southern_africa_urban_pop)

southern_africa_urban_pop <- merge(x = southern_africa_urban_pop, 
                                    y = southern_africa_urban_polygons, by = "FID")
southern_africa_urban_pop <- dplyr::select(southern_africa_urban_pop, FID, start_pop, `1990`, `2000`, `2015`, country, osm_name, aez_class, adm_2)

#   calculate population growth rate using the natural log
southern_africa_urban_pop$log_perc_change_75_90 <- ((ln(southern_africa_urban_pop$'1990'/southern_africa_urban_pop$start_pop))/15)*100
southern_africa_urban_pop$log_perc_change_90_00 <- ((ln(southern_africa_urban_pop$'2000'/southern_africa_urban_pop$'1990'))/10)*100
southern_africa_urban_pop$log_perc_change_00_15 <- ((ln(southern_africa_urban_pop$'2015'/southern_africa_urban_pop$'2000'))/15)*100

#   calculate absolute population change 
southern_africa_urban_pop$annual_change_75_90 <- ((southern_africa_urban_pop$'1990' - southern_africa_urban_pop$start_pop)/15)
southern_africa_urban_pop$annual_change_90_00 <- ((southern_africa_urban_pop$'2000' - southern_africa_urban_pop$'1990')/10)
southern_africa_urban_pop$annual_change_00_15 <- ((southern_africa_urban_pop$'2015' - southern_africa_urban_pop$'2000')/15)

#   check everything looks ok!
view(southern_africa_urban_pop)





####   distance data
#   nasa sedac groads data used to create distance matrix. od-distance matrix in qgis
#   variables for distance between all urban areas in southern africa (domestically)
botswana_distance_matrix_raw <- st_read("/Users/azimmer/Documents/PhD Geography/Research/African Urban Growth/Country Level Roads & Centroids/Country Level OD Matrix/BWA_Matrix.shp")
lesotho_distance_matrix_raw<- st_read("/Users/azimmer/Documents/PhD Geography/Research/African Urban Growth/Country Level Roads & Centroids/Country Level OD Matrix/LSO_Matrix.shp")
malawi_distance_matrix_raw<- st_read("/Users/azimmer/Documents/PhD Geography/Research/African Urban Growth/Country Level Roads & Centroids/Country Level OD Matrix/MWI_Matrix.shp")
mozambique_distance_matrix_raw<- st_read("/Users/azimmer/Documents/PhD Geography/Research/African Urban Growth/Country Level Roads & Centroids/Country Level OD Matrix/MOZ_Matrix.shp")
namibia_distance_matrix_raw<- st_read("/Users/azimmer/Documents/PhD Geography/Research/African Urban Growth/Country Level Roads & Centroids/Country Level OD Matrix/NAM_Matrix.shp")
southafrica_distance_matrix_raw<- st_read("/Users/azimmer/Documents/PhD Geography/Research/African Urban Growth/Country Level Roads & Centroids/Country Level OD Matrix/ZAF_Matrix.shp")
zambia_distance_matrix_raw<- st_read("/Users/azimmer/Documents/PhD Geography/Research/African Urban Growth/Country Level Roads & Centroids/Country Level OD Matrix/ZMB_Matrix.shp")
zimbabwe_distance_matrix_raw<- st_read("/Users/azimmer/Documents/PhD Geography/Research/African Urban Growth/Country Level Roads & Centroids/Country Level OD Matrix/ZWE_Matrix.shp")

#   merge all countries together
complete_distance_matrix_raw <- rbind(botswana_distance_matrix_raw, lesotho_distance_matrix_raw,
                                      malawi_distance_matrix_raw, mozambique_distance_matrix_raw,
                                      namibia_distance_matrix_raw, southafrica_distance_matrix_raw,
                                      zambia_distance_matrix_raw, zimbabwe_distance_matrix_raw)

#   drop geometry, since these are no longer needed spatially
st_geometry(complete_distance_matrix_raw) <- NULL
view(complete_distance_matrix_raw)

#   quick plot of distribution
ggplot(complete_distance_matrix_raw, aes(x = total_cost/1000,)) +
    geom_density() +
    theme_bw() +
    scale_x_continuous(labels = comma) +
    labs(x = "total distance (km)", y = "density")

#   calculate distance to the largest urban place
largest_urban_places <- southern_africa_urban_pop %>%
                        group_by(country)%>%
                        top_n(1, `2015`)
                        
largest_urban_places <- dplyr::select(largest_urban_places, FID, country)

#   find rows where distance matrix FID = FID of largest place, merge into main dataset
#Botswana - Location FID 1276
botswana_capital_distance <- botswana_distance_matrix_raw %>%
  filter(destinatio == 1276)

#Lesotho - Location FID 329
lesotho_capital_distance <- lesotho_distance_matrix_raw %>%
  filter(destinatio == 329)

#Malawi - Location FID 4821
malawi_capital_distance <- malawi_distance_matrix_raw %>%
  filter(destinatio == 4821)

#Mozambique - Location FID 909
mozambique_capital_distance <- mozambique_distance_matrix_raw %>%
  filter(destinatio == 909)

#Namibia - Location FID 1925
namibia_capital_distance <- namibia_distance_matrix_raw %>%
  filter(destinatio == 1925)

#South Africa - Location FID 819
# extra step needed, find smallest value from Johannesburg or Cape Town
southafrica_capital_distance <- southafrica_distance_matrix_raw %>%
  filter(destinatio == 819 | destinatio == 21)

southafrica_capital_distance <- southafrica_capital_distance %>% 
  group_by(origin_id) %>% 
  top_n(1, total_cost)

#Zambia - Location FID 5218
zambia_capital_distance <- zambia_distance_matrix_raw %>%
  filter(destinatio == 5218)

#Zimbabwe - Location FID 3721
zimbabwe_capital_distance <- zimbabwe_distance_matrix_raw %>%
  filter(destinatio == 3721)

#   Merge to create dataframe with all distances to capital cities
capital_distance <- dplyr::bind_rows(botswana_capital_distance, lesotho_capital_distance, malawi_capital_distance, 
                                     mozambique_capital_distance, namibia_capital_distance, southafrica_capital_distance,
                                     zambia_capital_distance, zimbabwe_capital_distance)

capital_distance <- dplyr::select(capital_distance, origin_id, total_cost)
capital_distance$total_cost <- (capital_distance$total_cost/1000) #turning values into km
capital_distance$geometry <- NULL

names(capital_distance) <- c("FID", "distance_to_largest_city")

#   merge capital city distance with population growth dataset
southern_africa_urban_pop <- merge(southern_africa_urban_pop, capital_distance, by="FID")
view(southern_africa_urban_pop)

#   calculate distance to the closest neighboring urban place
small_urban_distance <- complete_distance_matrix_raw[!(complete_distance_matrix_raw$origin_id == complete_distance_matrix_raw$destinatio), ] 

#   remove those values where distance = 0
small_urban_distance<-small_urban_distance[!(small_urban_distance$total_cost=="0"),] #Distance = 0

#   find the value with the lowest total distance (total cost) to another urban area
small_urban_distance <- small_urban_distance %>% 
  group_by(origin_id) %>% 
  top_n(-1, total_cost)

small_urban_distance <- dplyr::select(small_urban_distance, origin_id, total_cost)
small_urban_distance$total_cost <- (small_urban_distance$total_cost/1000) #turning values into km

names(small_urban_distance) <- c("FID", "distance_to_next_urban")

#   merge this in to the main dataset
southern_africa_urban_pop <- merge(southern_africa_urban_pop, small_urban_distance, by="FID")

#   remove duplicates from main dataset after merging
southern_africa_urban_pop<- southern_africa_urban_pop[!duplicated(southern_africa_urban_pop), ]
view(southern_africa_urban_pop)


#   create average distance to closest 3 urban places
proximity_urban <- complete_distance_matrix_raw[!(complete_distance_matrix_raw$origin_id == complete_distance_matrix_raw$destinatio), ] 

#   find the lowest three urban places
proximity_urban <- proximity_urban %>%
  group_by(origin_id) %>%
  mutate(rank = rank(desc(total_cost)))%>%
  arrange(rank) %>%
  filter(rank <=3)

#   find the average distance from those three
proximity_urban <- proximity_urban %>%
  group_by(origin_id) %>%
  mutate(avg_dist_urban = mean(total_cost))

proximity_urban <- arrange(proximity_urban, origin_id)
proximity_urban <- proximity_urban[!duplicated(proximity_urban$origin_id),]

proximity_urban <- dplyr::select(proximity_urban, origin_id, avg_dist_urban)
proximity_urban$avg_dist_urban <- (proximity_urban$avg_dist_urban/1000) #turning values into km

names(proximity_urban) <- c("FID", "avg_distance_urban")

#   merge this in to the main dataset
southern_africa_urban_pop <- merge(southern_africa_urban_pop, proximity_urban, by="FID")
view(southern_africa_urban_pop)




####   land-use data
#   esa land-use data (2015), aggregated to each adm-2 area (district) in gee. values for area and percentage of land in each category
lulc_area <- read.csv("/Users/azimmer/Documents/PhD Geography/Research/African Urban Growth/Southern Africa LULCC/Admin 2/southern_africa_esa_zonal_statistics_km2_area.csv")
lulc_percent <- read.csv("/Users/azimmer/Documents/PhD Geography/Research/African Urban Growth/Southern Africa LULCC/Admin 2/southern_africa_esa_zonal_statistics_percentages.csv")

#   remove rows that are assigned to lakes
lulc_area<-lulc_area[!(lulc_area$NAME_2=="Lake Malawi"),]
lulc_area<-lulc_area[!(lulc_area$NAME_2=="Lake Chilwa"),]
lulc_percent<-lulc_percent[!(lulc_percent$NAME_2=="Lake Malawi"),]
lulc_percent<-lulc_percent[!(lulc_percent$NAME_2=="Lake Chilwa"),]

#   area - select time points of interest, 1992 (as proxy for 1990), 2000 and 2015
useful_lulc_area <- lulc_area  %>%
  dplyr::select(NAME_2, Year, Agriculture, Forest, Grassland, Wetland, Settlement, Other) %>%
  filter(Year == "1992-01-01" | Year == "2000-01-01" | Year == "2015-01-01")

#   rename columns to make merging easier
names(useful_lulc_area) <- c("adm_2", "year", "agriculture", "forest", "grassland", "wetland", "settlement", "other")

#   percentage - Select time points of interest, 1992 (as proxy for 1990), 2000 and 2015
useful_lulc_perc <- lulc_percent  %>%
  dplyr::select(NAME_2, Year, Agriculture, Forest, Grassland, Wetland, Settlement, Other) %>%
  filter(Year == "1992-01-01" | Year == "2000-01-01" | Year == "2015-01-01")

#   rename columns to make merging easier
names(useful_lulc_perc) <- c("adm_2", "year", "agriculture", "forest", "grassland", "wetland", "settlement", "other")

#   area for 1990
lulc_area_1990 <- subset(useful_lulc_area, year=='1992-01-01')
lulc_area_1990 <- lulc_area_1990[-c(2)]
names(lulc_area_1990) <- c("adm_2", "agriculture_area_1990", "forest_area_1990", "grassland_area_1990", "wetland_area_1990", 
                           "settlement_area_1990", "other_area_1990")

#   area for 2000
lulc_area_2000 <- subset(useful_lulc_area, year=='2000-01-01')
lulc_area_2000 <- lulc_area_2000[-c(2)]
names(lulc_area_2000) <- c("adm_2", "agriculture_area_2000", "forest_area_2000", "grassland_area_2000", "wetland_area_2000", 
                           "settlement_area_2000", "other_area_2000")

#   area for 2015
lulc_area_2015 <- subset(useful_lulc_area, year=='2015-01-01')
lulc_area_2015 <- lulc_area_2015[-c(2)]
names(lulc_area_2015) <- c("adm_2", "agriculture_area_2015", "forest_area_2015", "grassland_area_2015", "wetland_area_2015", 
                           "settlement_area_2015", "other_area_2015")

#   percentage for 1990
lulc_perc_1990 <- subset(useful_lulc_perc, year=='1992-01-01')
lulc_perc_1990 <- lulc_perc_1990[-c(2)]
names(lulc_perc_1990) <- c("adm_2", "agriculture_perc_1990", "forest_perc_1990", "grassland_perc_1990", "wetland_perc_1990", 
                           "settlement_perc_1990", "other_perc_1990")

#   percentage for 2000
lulc_perc_2000 <- subset(useful_lulc_perc, year=='2000-01-01')
lulc_perc_2000 <- lulc_perc_2000[-c(2)]
names(lulc_perc_2000) <- c("adm_2", "agriculture_perc_2000", "forest_perc_2000", "grassland_perc_2000", "wetland_perc_2000", 
                           "settlement_perc_2000", "other_perc_2000")

#   percentage for 2015
lulc_perc_2015 <- subset(useful_lulc_perc, year=='2015-01-01')
lulc_perc_2015 <- lulc_perc_2015[-c(2)]
names(lulc_perc_2015) <- c("adm_2", "agriculture_perc_2015", "forest_perc_2015", "grassland_perc_2015", "wetland_perc_2015", 
                           "settlement_perc_2015", "other_perc_2015")

#   merge back together
useful_lulc_combined <- merge(lulc_area_1990, lulc_area_2000, by = "adm_2")
useful_lulc_combined <- merge(useful_lulc_combined, lulc_area_2015, by = "adm_2")
useful_lulc_combined <- merge(useful_lulc_combined, lulc_perc_1990, by = "adm_2")
useful_lulc_combined <- merge(useful_lulc_combined, lulc_perc_2000, by = "adm_2")
useful_lulc_combined <- merge(useful_lulc_combined, lulc_perc_2015, by = "adm_2")

#   remove duplicates
useful_lulc_combined <- useful_lulc_combined %>% distinct(adm_2, .keep_all = TRUE)

#   calculate ag per capita with total district pop at each time point
district_population <- dplyr::select(southern_africa_urban_pop, adm_2, start_pop, `1990`, `2000`, `2015`)

district_population <- district_population %>%
  group_by(adm_2) %>%
  summarise_all(sum)

names(district_population) <- c("adm_2", "district_pop_1975", "district_pop_1990", "district_pop_2000", "district_pop_2015")

population_landuse <- merge(district_population, useful_lulc_combined, by = "adm_2")

population_landuse <- population_landuse %>%
  mutate(ag_per_capita_1990_d = ((agriculture_area_1990*1000000)/district_pop_1990)) %>%
  mutate(ag_per_capita_2000_d = ((agriculture_area_2000*1000000)/district_pop_2000)) %>%
  mutate(ag_per_capita_2015_d = ((agriculture_area_2015*1000000)/district_pop_2015))

population_landuse <- dplyr::select(population_landuse, adm_2, ag_per_capita_1990_d, ag_per_capita_2000_d, ag_per_capita_2015_d)

#   merge back in to main dataset (ag oer capita)
southern_africa_urban_pop <- merge(southern_africa_urban_pop, population_landuse, by = "adm_2")

#   merge back in to main dataset (ag area and percentage)
southern_africa_urban_pop <- merge(southern_africa_urban_pop, useful_lulc_combined, by = "adm_2")





####   climate data
#   chirps daily precipitation data aggregated to adm-2 area (district) in gee
#   variables for precip anomaly, consecutive dry years and % of years with <75% avg rainfall
precip_data <- read.csv("/Users/azimmer/Documents/PhD Geography/Research/African Urban Growth/Precipitation/chirps_monthly_total_SouthernAfrica.csv")
view(precip_data)

#   select useful columns, adm2 and precip vars
useful_precip <- precip_data %>% dplyr::select(7:19)
colnames(useful_precip)[1] <- "adm_2"

#   merge into main dataset
southern_africa_urban_pop <- merge(southern_africa_urban_pop, useful_precip, by="adm_2")
view(southern_africa_urban_pop)

####    export completed dataset
write.csv(southern_africa_urban_pop, file = "/Users/azimmer/Documents/PhD Geography/Research/African Urban Growth/Precipitation/merged_clean_wfp_dataset_covariates.csv")

####    plot map of urban areas  ####

#   get lat/lon from centroid of polygons
urban_pop_gps <-
  left_join(southern_africa_urban_pop,
            southern_africa_urban_polygons %>% dplyr::select(FID, lat, lon),
            by = "FID")

urban_pop_gps <- dplyr::select(urban_pop_gps, lat, lon, `2000`, log_perc_change_00_15)
urban_pop_gps <- st_as_sf(urban_pop_gps, coords = c("lon", "lat"), 
    crs = 4326, agr = "constant")
urban_pop_gps$log_perc_change_00_15 <- as.numeric(urban_pop_gps$log_perc_change_00_15)

#   plot map of southern africa with urban centroids located
spdf_africa <- ne_countries(continent = "africa", returnclass = "sf")

ggplot(data = spdf_africa) +
    geom_sf() +
    geom_sf(data = urban_pop_gps, aes(color = log_perc_change_00_15,
            size = `2000`)) +
    scale_color_viridis(limits = c(0,10), na.value = "transparent", alpha = 0.5) +
    scale_size_continuous(limits = c(500,100000)) +
    theme_void() +
    coord_sf(xlim = c(10, 42), ylim = c(-7, -36), expand = FALSE) +
    labs(x = "lon", y = "lat", color = "pop growth '00-'15", size = "'00 pop",
        title = "Population growth rates for secondary cities in southern Africa") +
    theme(legend.position = "right",
            legend.background = element_blank())


####    setting up data for regression analysis  ####

lowerbounds <- 500
upperbounds <- 100000

#   begin formatting data for regression analysis
regression_data <- na.omit(southern_africa_urban_pop)

#   retain columns to be used in analysis. edit to run new models
regression_data <- dplyr::select(regression_data, start_pop, `1990`, `2000`, `2015`, 
                                country, aez_class, 
                                log_perc_change_75_90, log_perc_change_90_00, log_perc_change_00_15, 
                                distance_to_largest_city.x, distance_to_next_urban, avg_distance_urban, 
                                ag_per_capita_1990_d.x, ag_per_capita_2000_d.x, ag_per_capita_2015_d.x,
                                agriculture_area_1990.x, agriculture_area_2000.x, agriculture_area_2015.x,
                                Precip_Anom_TP1, Precip_Anom_TP2, Precip_Anom_TP3, 
                                BelowAvePercent_TP1, BelowAvePercent_TP2, BelowAvePercent_TP3, 
                                ConsDryCount_TP1, ConsDryCount_TP2, ConsDryCount_TP3,
                                Below75Percent_TP1, Below75Percent_TP2, Below75Percent_TP3)

colnames(regression_data) <- c("start_pop", "1990", "2000", "2015", 
                                "country", "aez_class", 
                                "log_perc_change_75_90","log_perc_change_90_00", "log_perc_change_00_15", 
                                "distance_to_largest_city", "distance_to_next_urban", "avg_distance_urban", 
                                'ag_per_capita_1990_d', 'ag_per_capita_2000_d', 'ag_per_capita_2015_d',
                                'agriculture_area_1990', 'agriculture_area_2000', 'agriculture_area_2015',
                                "Precip_Anom_TP1", 'Precip_Anom_TP2', "Precip_Anom_TP3", 
                                "BelowAvePercent_TP1", "BelowAvePercent_TP2", "BelowAvePercent_TP3", 
                                'ConsDryCount_TP1', "ConsDryCount_TP2", "ConsDryCount_TP3",
                                'Below75Percent_TP1', 'Below75Percent_TP2', 'Below75Percent_TP3')


#   filtering data to focus on secondary cities
regression_data <- subset(regression_data, start_pop > lowerbounds)
regression_data <- subset(regression_data, start_pop < upperbounds)
regression_data<- regression_data %>% mutate_if(is.numeric, list(~na_if(., Inf))) #change all INF to NA
regression_data <- na.omit(regression_data)

view(regression_data)

# TP1
TP1_perc_fit <- lm(log_perc_change_75_90 ~ start_pop +
                     distance_to_next_urban +
                     avg_distance_urban + 
                     distance_to_largest_city +
                     Precip_Anom_TP1 +
                     ConsDryCount_TP1 +
                     Below75Percent_TP1 +
                     ag_per_capita_1990_d+ 
                     agriculture_area_1990 +
                     country +
                     aez_class, data = regression_data)

summary(TP1_perc_fit)

# TP2
TP2_perc_fit <- lm(log_perc_change_90_00 ~ `1990` +
                     distance_to_next_urban +
                     avg_distance_urban + 
                     distance_to_largest_city +
                     Precip_Anom_TP2 +
                     ConsDryCount_TP2 +
                     Below75Percent_TP2 +
                     ag_per_capita_2000_d+ 
                     agriculture_area_2000 +
                     country +
                     aez_class, data = regression_data)

summary(TP2_perc_fit)

# TP3
TP3_perc_fit <- lm(log_perc_change_00_15 ~ `2000` +
                     distance_to_next_urban +
                     avg_distance_urban + 
                     distance_to_largest_city +
                     Precip_Anom_TP3 +
                     ConsDryCount_TP3 +
                     Below75Percent_TP3 +
                     ag_per_capita_2015_d+ 
                     agriculture_area_2015 +
                     country +
                     aez_class, data = regression_data)

summary(TP3_perc_fit)

####    Plots of population growth through time ####
pop_plot_data <- southern_africa_urban_pop
lowerbounds <- 500
pop_plot_data <- subset(pop_plot_data, start_pop > lowerbounds)


citygrowth1 <- ggplot(pop_plot_data, aes(y = log_perc_change_75_90, x = `start_pop`)) +
  geom_hline(yintercept = 2, color = 'darkgrey', size = 0.5) +
  geom_point() +
  scale_x_log10(labels = comma) +
  ylim(-20,30) +
  labs( x = "1975 Population", y = "Growth Rate 1975-1990") +
  theme_bw()

citygrowth2 <- ggplot(pop_plot_data, aes(y = log_perc_change_90_00, x = `1990`)) +
  geom_hline(yintercept = 2, color = 'darkgrey', size = 0.5) +
  geom_point() +
  scale_x_log10(labels = comma) +
  ylim(-20,30) +
  labs( x = "1990 Population", y = "Growth Rate 1990-2000") +
  theme_bw()
  
citygrowth3 <- ggplot(pop_plot_data, aes(y = log_perc_change_00_15, x = `2000`)) +
  geom_hline(yintercept = 2, color = 'darkgrey', size = 0.5) +
  geom_point() +
  scale_x_log10(labels = comma) +
  ylim(-20,30) +
  labs( x = "2000 Population", y = "Growth Rate 2000-2015") +
  theme_bw()

ggarrange(citygrowth1, citygrowth2, citygrowth3, nrow = 3)


#     we now have a dataset of urban growth for all cities in southern africa, alongside detailed climate, distance and land-use metrics
#     next steps:   create spatial weights matrix for spatial regression models
#                   plot figures for paper