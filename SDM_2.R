###SDM 
library(reshape2) ##melting dataframes
library(dplyr) #data wrangling
library(raster) ##working with raster data
library(sp) 
library(geodata)
setwd("/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/MPM Community Science Project/SDM")
#get Prism data
install.packages("prism")
library(prism)
packageVersion("prism")
library(prism)
prism_dir<-prism_set_dl_dir('/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/MPM Community Science Project/SDM/HighLatitudeNorthAmericanButterflyOccupancy-main/data/climate')
#annual min temp
get_prism_annual(type = "tmin", years=c(1960:2023),keepZip = FALSE)
get_prism_annual(type = "ppt", years=c(1960:2023),keepZip = FALSE)

crs_1 <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
setwd(prism_dir)
tmin_early<-prism_archive_subset("tmin", "annual", years=1960:1989)
tmin_early<-pd_to_file(tmin_early)
tmin_early<-raster::stack(tmin_early)
tmin_early<-projectRaster(tmin_early, crs=crs_1)

tmin_late<-prism_archive_subset("tmin", "annual", years=1990:2023)
tmin_late<-pd_to_file(tmin_late)
tmin_late<-raster::stack(tmin_late)
tmin_late<-projectRaster(tmin_late, crs=crs_1)

ppt_early<-prism_archive_subset("ppt", "annual", years=1960:1989)
ppt_early<-pd_to_file(ppt_early)
ppt_early<-raster::stack(ppt_early)
ppt_early<-projectRaster(ppt_early, crs=crs_1)

ppt_late<-prism_archive_subset("ppt", "annual", years=1990:2023)
ppt_late<-pd_to_file(ppt_late)
ppt_late<-raster::stack(ppt_late)
ppt_late<-projectRaster(ppt_late, crs=crs_1)

ppt<-prism_archive_subset("ppt", "annual")
ppt<-pd_to_file(ppt)
ppt<-raster::stack(ppt)
ppt<-projectRaster(ppt, crs=crs_1)

tmin<-prism_archive_subset("tmin", "annual")
tmin<-pd_to_file(tmin)
tmin<-raster::stack(tmin)
tmin<-projectRaster(tmin, crs=crs_1)



ave_tmin_early <- raster::mean(tmin_early)
ave_tmin_late <- raster::mean(tmin_late)
ave_ppt_early <- raster::mean(ppt_early)
ave_ppt_late <- raster::mean(ppt_late)
ave_tmin<-raster::mean(tmin)
ave_ppt<-raster::mean(ppt)

climate_raster_avg<-raster::stack(ave_tmin,ave_ppt)
library(sf)
basemap <- st_read("/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/MPM Community Science Project/SDM/occ_historical/analysis/odonates/scripts/ne_10m_land.shp")
basemap <- basemap %>% 
  st_crop(xmin=-92.9, xmax=-86.8,
          ymin=42.5, ymax=47.3) %>% st_transform(crs_1)
basemap_df <- as.data.frame(st_coordinates(basemap))
colnames(basemap_df)<-c("long","lat")
plot(basemap_df$long,basemap_df$lat)
library(USAboundaries)
wi_shape <- us_states(states="Wisconsin")
wi_shape_proj <- st_transform(wi_shape, crs(climate_raster_avg))

wi_vect <- vect(wi_shape_proj)

climate_crop_raster<-raster::crop(climate_raster_avg, extent(basemap))
climate_crop_raster_avg<-raster::crop(climate_raster_avg, extent(wi_shape_proj))

#convert raster to point data
proj4string(climate_crop_raster)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info
proj4string(climate_crop_raster)<-CRS(crs_1) ##assign projection info
proj4string(climate_crop_raster_avg)<-CRS(crs_1) ##assign projection info

##convert raster to point data frame
clim_e_df <- data.frame(rasterToPoints(climate_crop_raster_avg))
clim_e_m.df <- reshape2::melt(clim_e_df, c("x", "y"))
names(clim_e_m.df)[1:2] <- c("lon", "lat") #rename columns

#save raster files 
prism_annuals<-'normals_tmin_ppt'
name<-paste0(prism_dir, prism_annuals,".csv")
write.csv(clim_e_m.df, name)
writeRaster(climate_crop_raster, name,overwrite=TRUE)

#rename(tmean=value)
clim_e_m.df$env <- ifelse(grepl("ppt", clim_e_m.df$variable), "ppt", "tmin")
dim(clim_e_m.df)
#attempt to plot
library(ggfun)
library(ggplot2)
clim_e_m.df$variable<-as.character(clim_e_m.df$variable)
temp_e_df<-filter(clim_e_m.df,variable=="layer.1")
ggplot()+
  geom_raster(data=temp_e_df, aes(x=lon, y=lat, fill=value
  ))+theme_minimal()+
  scale_fill_gradient2("Min temperature", low='darkslateblue',mid='lightblue',high = 'red', midpoint=0)+
  labs(title="Min Annual Temperature: 1960-2023")
ppt_e_df<-filter(clim_e_m.df,variable=="layer.2")
ggplot()+
  geom_raster(data=ppt_e_df, aes(x=lon, y=lat, fill=value
  ))+theme_minimal()+
  scale_fill_gradient2("Precip (mm)", low='darkslateblue',mid='lightblue',high = 'red', midpoint=825)+
  labs(title="Average annual Precipitation: 1960-2023")



library(USAboundaries)
wi_shape <- us_states(states="Wisconsin")
wi_shape_proj <- st_transform(wi_shape, crs(climate_crop_raster_avg))
wi_vect <- vect(wi_shape_proj)

climate_crop_raster_avg<-terra::rast(climate_crop_raster_avg)
climate_crop_raster_avg_wi <- crop(climate_crop_raster_avg, wi_vect)
climate_crop_mask_raster_avg_wi <- mask(climate_crop_raster_avg_wi, wi_vect)


#gbif data
library(rgbif)
#install.packages("usethis")
usethis::edit_r_environ() ##one time to add credentials
#complex data download
name_backbone("Magnoliopsida") #find the taxonKey (usageKey)
name_backbone("Liliopsida") #find the taxonKey (usageKey)

#7707728
higher_taxon<-"Tracheophyta" #vasc PLANTS
#custom search 
gbif_download<-occ_download(
  pred("hasCoordinate", TRUE),
  pred_in("taxonKey", c(220,196)), #monocots and dicots=angiosperms
  pred("stateProvince","Wisconsin"),
  format = "SIMPLE_CSV"
)
#prelim search for WI species
library(dplyr)
library(CoordinateCleaner)
#get all plants WI
occ_download_wait(gbif_download)
x<-gbif_download %>%
  occ_download_get() %>%
  occ_download_import()

head(x)#filtered
######rastering
library(raster)
library(usmap)
library(ggplot2)
library(leaflet)
library(tidyr)
#look at species in WI
xe<-xe%>%drop_na(scientificName,family) %>% count(family,taxonKey,sort=TRUE)#get counts of each species
#filter to taxa with enough occurrences (not worrying about taxonomy)
species<-filter(xe,n>30)
#filter to family
fam_level<-species %>% count(family,sort=TRUE)
#2152 sp, 130 families
WI_taxonkey<-species$taxonKey
###recollect data for north america for xe species of WI
#complex data download
#custom search 
gbif_download_plant<-occ_download(
  pred("hasCoordinate", TRUE), #has coordinate
  pred_in("taxonKey", WI_taxonkey),#wi species list
  pred("stateProvince","Wisconsin"), #in wi
  pred_in("year",c(1960:2023)),#match normals
  format = "SIMPLE_CSV"
)
#get WI plants full data
occ_download_wait(gbif_download_plant)
x<-gbif_download_plant %>%
  occ_download_get() %>%
  occ_download_import()

#filtering pipeline for all plants-clean at species level then consolidate
xe<-xe%>%
  #setNames(tolower(names(.))) %>% # set lowercase column names to work with CoordinateCleaner
  filter(!basisOfRecord %in% "FOSSIL_SPECIMEN") %>%
  filter(coordinatePrecision < 0.01 | is.na(coordinatePrecision)) %>% 
  filter(coordinateUncertaintyInMeters < 10000 | is.na(coordinateUncertaintyInMeters)) %>%
  filter(!coordinateUncertaintyInMeters %in% c(301,3036,999,9999)) %>% 
  filter(!decimalLatitude == 0 | !decimalLongitude == 0) %>%
  cc_cen(buffer = 2000) %>% # remove country centroids within 2km 
  cc_cap(buffer = 1000) %>% # remove capitals centroids within 2km
  cc_inst(buffer = 2000) %>% # remove zoo and herbaria within 2km 
  cc_sea(buffer=500) %>% # remove from ocean 
  distinct(decimalLongitude,decimalLatitude,speciesKey,datasetKey, .keep_all = TRUE) %>%
  glimpse() # look at results of pipeline
write.csv(x,"WI_plant_gbif_all.csv")
xe<-read.csv('/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/MPM Community Science Project/SDM/Plant_SDMS/WI_plant_gbif_all.csv')
xe <- dplyr::rename(xe, latitude = decimalLatitude, 
                    longitude = decimalLongitude)
library(leaflet)
library(tidyr)
xe<-xe %>% filter(taxonKey %in% WI_taxonkey) #not sure why need to refilter
xe_time <-xe %>% filter(year >1959)
xe_time %>% group_by(taxonKey) #good
#grid sampling
library(dismo)
library(raster)
library(tidyr)
library(dplyr)
lat_between <- xe_time$latitude[between(xe_time$latitude, 42.5, 47.3)]
lon_between <- xe_time$longitude[between(xe_time$longitude, -92.9, -86.8)]

# Specify the bounding box for Wisconsin
wisconsin_bounds <- c(left = -92.9, bottom = 42.5, right = -86.8, top = 47.3)
NA_bounds<-c(left = -125, bottom = 28, right = -66, top = 49)
length(xe_time$latitude) - length(lat_between)
length(xe_time$longitude) - length(lon_between)
#exclude the lat and long values outside those boundaries
xe_time$long_out <- xe_time$longitude %in% lon_between
xe_time$lat_out <- xe_time$latitude %in% lat_between
xe_time <- filter(xe_time, long_out == "TRUE") %>% filter(lat_out == "TRUE") #eliminated out of bounds points,,only took out a few
xe_time_early <- filter(xe_time, year<1990)  
xe_time_late <- filter(xe_time, year>1989)  

# Convert to spatial object (sf or SpatialPoints)
occ_sf <- st_as_sf(xe_time, coords = c("longitude", "latitude"), crs = 4326)  # WGS84
# Reproject occurrence data to match climate raster
occ_projected <- st_transform(occ_sf, crs = crs(climate_crop_mask_raster_avg_wi))

# Step 3: Extract coordinates back for grouping
xe_time_proj <- occ_projected %>%
  mutate(longitude = st_coordinates(.)[,1],
         latitude = st_coordinates(.)[,2]) %>%
  st_drop_geometry()
# Create a bounding box object...need to do with env
bbox<-extent(climate_crop_raster_avg)
# Step 1: Ensure CRS of climate raster is stored
clim_crs <- crs(climate_crop_mask_raster_avg_wi)
####loop di loop for each species
######gridsampling with each species
# create a RasterLayer with the extent of species
# Grouped iteration using dplyr and purrr
library(purrr)
result_list_wi_plants<-list()#prep list for plant samples
result_list_wi_plants <- xe_time_proj %>%
  group_by(family) %>% #family level diversity
  nest() %>%
  mutate(sampled_values = map2(data, family, function(group_data,group_name) {
    lon_range <- range(group_data$longitude, na.rm = TRUE)
    lat_range <- range(group_data$latitude, na.rm = TRUE)
    
    if (anyNA(lon_range) || anyNA(lat_range) || diff(lon_range) == 0 || diff(lat_range) == 0) {
      # Skip this iteration or return NA
      return(NULL)
    }
    # Create SpatialPointsDataFrame with climate raster CRS
    coordinates(group_data) <- ~longitude + latitude
    proj4string(group_data) <- clim_crs
    
    # Define a raster based on the group extent
    base_raster <- raster(extent(group_data))
    res(base_raster) <- 0.04  # Set desired resolution
    crs(base_raster) <- clim_crs
    
    # Optionally buffer the extent a bit
    base_raster <- extend(base_raster, extent(base_raster) + 1)
    
    # Sample values from the raster at points defined by the SpatialPointsDataFrame
    acsel <- gridSample(group_data, base_raster, n = 1)
    acsel <- data.frame(acsel)
   
    # Name the dataframe based on the group name
    return(acsel)
  })) %>%
  dplyr::select(-data)
#find low point families
result_list_wi_plants<-filter(result_list_wi_plants,!sampled_values=="NULL")

##########combine climate and species
head(result_list_wi_plants)
names(result_list_wi_plants$sampled_values)<-result_list_wi_plants$family#list of acsel df for each species
filtered_list <-result_list_wi_plants$sampled_values[!sapply(result_list_wi_plants$sampled_values,is.null)] #removed families with low points
library(sf) #note need to install with gdal for proper updates
occurrence_wi_sp<-sapply(filtered_list,SpatialPoints)
climate_df <- as.data.frame(rasterToPoints(climate_crop_raster))

###spot check 
aster_temp<-filtered_list$Asteraceae
names(aster_temp)<-c("longitude","latitude")
###single plot w temp
ggplot() +
  geom_raster(data = climate_df, aes(x = x, y = y, fill = layer.1)) +geom_point(data = aster_temp, aes(x =longitude, y = latitude), size = 0.5) +
  scale_fill_gradient(low = "blue", high = "red", name = "1967 Temp") +
  labs(x = "Longitude", y = "Latitude", fill = "Mean Monthly Temp (degC)")  + ggtitle("Asteraceae") +
  theme_minimal()
#and ppt
ggplot() +
  geom_raster(data = climate_df, aes(x = x, y = y, fill = layer.2)) +geom_point(data = aster_temp, aes(x =longitude, y = latitude), size = 0.5) +
  scale_fill_gradient(low = "blue", high = "red", name = "Annual Mean  Precipitation: 1967") +
  labs(x = "Longitude", y = "Latitude", fill = "Mean Annual Precipitation (mm)")  + ggtitle("Asteraceae") +
  theme_minimal()
####Hyper tuning params with SDMtune
install.packages("SDMtune")
library(SDMtune)
###test just one taxon
# Create SWD object
predictors<-terra::rast(climate_crop_raster)
data <- prepareSWD(species = "Virtual species", p = occurrence_wi_sp$Asteraceae,
                   env = predictors)
data<-addSamplesToBg(data)
# witholding a 20% sample for testing 
# Split presence locations in training (80%) and testing (20%) datasets
datasets <- trainValTest(data, test = 0.2, only_presence = TRUE, seed = 25)
train <- datasets[[1]]
test <- datasets[[2]]
# Train a Maxnet model
model <- train(method = "Maxnet", data = train)

# Define the hyperparameters to test
h<- list(reg = seq(0.1, 3, 0.1), fc = c("lq", "lh", "lqp", "lqph", "lqpht"))

# Test all the possible combinations with gridSearch
gs <- gridSearch(model, hypers = h, metric = "auc", test = test)
head(gs@results[order(-gs@results$test_AUC), ])  # Best combinations

# Use the genetic algorithm instead with optimizeModel
om <- optimizeModel(model, hypers = h, metric = "auc", test = test, seed = 4)
head(om@results)  # Best combinations
# Acquire environmental variables
files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
                    pattern = "grd", full.names = TRUE)
predictors <- terra::rast(files)

# Prepare presence and background locations
p_coords <- virtualSp$presence
bg_coords <- virtualSp$background

# Create SWD object
data <- prepareSWD(species = "Virtual species", p = p_coords, a = bg_coords,
                   env = predictors, categorical = "biome")

# Split presence locations in training (80%) and testing (20%) datasets
datasets <- trainValTest(data, test = 0.2, only_presence = TRUE, seed = 25)
train <- datasets[[1]]
test <- datasets[[2]]

# Train a Maxnet model
model <- train(method = "Maxnet", data = train)

# Define the hyperparameters to test
h<- list(reg = seq(0.1, 3, 0.1), fc = c("lq", "lh", "lqp", "lqph", "lqpht"))

# Test all the possible combinations with gridSearch
gs <- gridSearch(model, hypers = h, metric = "auc", test = test)
head(gs@results[order(-gs@results$test_AUC), ])  # Best combinations

# Use the genetic algorithm instead with optimizeModel
om <- optimizeModel(model, hypers = h, metric = "auc", test = test, seed = 4)
head(om@results)  # Best combinations
###maxent
library(dismo)
options(java.parameters = "-Xmx8000m")
library(rJava)
system.file("java", package="dismo") #moved maxent.jar to package in r
setwd("Plant_SDMS/")
#iterate for each species in occurrence_sp, just test out variables
run_maxent <- function(spatial_df,filename,threshold=100) {
  # witholding a 20% sample for testing 
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=length(occtrain)
  # Check if the number of background points is below the threshold
  if (nbg < threshold) {
    message("Skipping species due to insufficient background points: ", nbg, " points.")
    return(NULL)  # Skip running the MaxEnt model and return NULL
  }
  me<-maxent(climate_crop_raster,occtrain,nbg=nbg)
  pdf(file = filename)  # Open a PDF device
  plot(me)
  dev.off()             # Close the PDF device
  # Return the MaxEnt model
  return(me)
}
# Apply the function to each spatial object in the list
for (i in seq_along(occurrence_wi_sp)) {
  filename <- paste0("MaxEnt_Model_Variables", names(occurrence_wi_sp[i]), ".pdf")  # Define a unique filename for each plot
  result<-run_maxent(occurrence_wi_sp[[i]], filename)  # Run MaxEnt and save plot as PDF
  # Check if result is NULL, which indicates that the species was skipped
  if (is.null(result)) {
    message("Species ", names(occurrence_wi_sp)[i], " was skipped due to insufficient background points.")
  } else {
    message("MaxEnt model successfully run for species: ", names(occurrence_wi_sp)[i])
  }
}
# #skip for now
# ##with hypertuning
# run_maxent <- function(spatial_df,filename,threshold=100) {
#   # witholding a 20% sample for testing 
#   fold <- kfold(spatial_df, k=5)
#   occtest <- spatial_df[fold == 1, ]
#   occtrain <- spatial_df[fold != 1, ]
#   nbg=length(occtrain)
#   # Check if the number of background points is below the threshold
#   if (nbg < threshold) {
#     message("Skipping species due to insufficient background points: ", nbg, " points.")
#     return(NULL)  # Skip running the MaxEnt model and return NULL
#   }
#   me<-maxent(climate_early_raster,occtrain,nbg=nbg)
# 
#     # Define the hyperparameters to test
#   h<- list(reg = seq(0.1, 3, 0.1), fc = c("lq", "lh", "lqp", "lqph", "lqpht"))
#   
#   # Test all the possible combinations with gridSearch
#   gs <- gridSearch(me, hypers = h, metric = "auc", test = occtest)
#   head(gs@results[order(-gs@results$test_AUC), ])  # Best combinations
#   
#   # Use the genetic algorithm instead with optimizeModel
#   om <- optimizeModel(me, hypers = h, metric = "auc", test = occtest, seed = 4)
#   head(om@results)  # Best combinations
#   
#   # predict to entire dataset
#   r <- predict(me,climate_early_raster) 
#   pdf(file = filename)  # Open a PDF device
#   plot(r,main=paste0("Suitability of locations in the US\n for ",names(occurrence_early_sp[i])))
#   points(spatial_df,col="black",pch=4,cex=.25)
#   dev.off()             # Close the PDF device
#   # Return the MaxEnt model
#   return(r)
# }
# # Apply the function to each spatial object in the list
# for (i in seq_along(occurrence_early_sp)) {
#   filename <- paste0("MaxEnt_Model_", names(occurrence_early_sp[i]), ".pdf")  # Define a unique filename for each plot
#  result<- run_maxent(occurrence_early_sp[[i]], filename)  # Run MaxEnt and save plot as PDF
# # Check if result is NULL, which indicates that the species was skipped
# if (is.null(result)) {
#   message("Species ", names(occurrence_early_sp)[i], " was skipped due to insufficient background points.")
# } else {
#   message("MaxEnt model successfully run for species: ", names(occurrence_early_sp)[i])
# }
# }
##non plot maxent
run_maxent <- function(spatial_df,threshold=100) {
  # witholding a 20% sample for testing 
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=length(occtrain)
  # Check if the number of background points is below the threshold
  if (nbg < threshold) {
    message("Skipping species due to insufficient background points: ", nbg, " points.")
return(NULL)  # Skip running the MaxEnt model and return NULL
    }
  me<-maxent(climate_crop_raster,occtrain,nbg=nbg)
  return(me)
}
mes<-lapply(occurrence_wi_sp,run_maxent) #get all me values for each species
# Initialize an empty list to store predictions--correct for nulls
rs <- list()

# Loop through each MaxEnt model in the `mes` list
for (i in seq_along(mes)) {
  # Get the MaxEnt model
  me_model <- mes[[i]]
  name <- names(mes)[i]
  # Check if the model is not NULL
  if (!is.null(me_model)) {
    # Perform prediction using the model and raster stack
    prediction <- predict(me_model, climate_crop_raster)
    
    # Store the prediction in the `rs` list
    rs[[name]] <- prediction
  } else {
    # If the model is NULL, print a message and continue
    message("Skipping model at index ", i, " because it is NULL.")
  }
}
##remove NULLS
# Remove NULL items from the list using Filter()
rs <- Filter(Negate(is.null), rs)
#rs@data contains predictors , @file contains points
#other options not used
#r <- predict(me, summer_rast,args=c("-P", "noautofeature", "nothreshold", "noproduct", paste("maximumbackground=",nrow(summer_rast), sep=""), "noaddsamplestobackground"))
#loop di doop no plots just model testing for each
#iterate for each species in occurrence_sp
model_evaluation_list_e1<-list()
model_evaluation_list_e2<-list()
model_evaluation_list_e3<-list()
#function for evaluating models
run_maxent <- function(spatial_df,threshold=100) {
  # witholding a 20% sample for testing 
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=length(occtrain)
  # Check if the number of background points is below the threshold
  if (nbg < threshold) {
    message("Skipping species due to insufficient background points: ", nbg, " points.")
    return(NULL)  # Skip running the MaxEnt model and return NULL
  }
  bg <- randomPoints(climate_crop_raster, length(spatial_df))
  me<-maxent(climate_crop_raster,occtrain,nbg=nbg)
  
  #simplest way to use 'evaluate'
  e1 <- evaluate(me, p=occtest, a=bg, x=climate_crop_raster)
  return(e1) 
}

maxent_models_evaluate <- lapply(occurrence_wi_sp, run_maxent) #list of maxents evaluated method 1

# alternative 1
run_maxent <- function(spatial_df,threshold=100) {
  # witholding a 20% sample for testing 
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=length(occtrain)
  # Check if the number of background points is below the threshold
  if (nbg < threshold) {
    message("Skipping species due to insufficient background points: ", nbg, " points.")
    return(NULL)  # Skip running the MaxEnt model and return NULL
  }
  bg <- randomPoints(climate_crop_raster, length(spatial_df))
  me<-maxent(climate_crop_raster,occtrain,nbg=nbg)
  # extract values
  pvtest <- data.frame(raster::extract(climate_crop_raster, occtest))
  avtest <- data.frame(raster::extract(climate_crop_raster, bg))
  
  e2 <- evaluate(me, p=pvtest, a=avtest)
  
  return(e2) 
}
maxent_models_evaluate2 <- lapply(occurrence_wi_sp, run_maxent) #list of maxents
maxent_models_evaluate2
# alternative 2 
# predict to testing points 
run_maxent <- function(spatial_df,filename,threshold=100) {
  # witholding a 20% sample for testing 
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=length(occtrain)# Check if the number of background points is below the threshold
  if (nbg < threshold) {
    message("Skipping species due to insufficient background points: ", nbg, " points.")
    return(NULL)  # Skip running the MaxEnt model and return NULL
  }
  bg <- randomPoints(climate_crop_raster, length(spatial_df))
  me<-maxent(climate_crop_raster,occtrain,nbg=nbg)
  # extract values
  pvtest <- data.frame(raster::extract(climate_crop_raster, occtest))
  avtest <- data.frame(raster::extract(climate_crop_raster, bg))
  testp <- predict(me, pvtest) 
  testa <- predict(me, avtest) 
  
  e3 <- evaluate(p=testp, a=testa)
  pdf(file=filename)
  plot(e3,"ROC")
  dev.off()
  return(e3)
}
# Apply the function to each spatial object in the list using lapply
for (i in seq_along(occurrence_wi_sp)) {
  filename <- paste0("MaxEnt_Model_ROC", names(occurrence_wi_sp[i]), ".pdf")  # Define a unique filename for each plot
  result<-run_maxent(occurrence_wi_sp[[i]], filename)  # Run MaxEnt and save plot as PDF
  # Check if result is NULL, which indicates that the species was skipped
  if (is.null(result)) {
    message("Species ", names(occurrence_wi_sp)[i], " was skipped due to insufficient background points.")
  } else {
    message("MaxEnt model successfully run for species: ", names(occurrence_wi_sp)[i])
  }
}
######## stacking
devtools::install_github("sylvainschmitt/SSDM")
library(SSDM)
#convert occ_sp data into dataframe
data_list<-lapply(result_list_wi_plants$sampled_values,function(df){
  data.frame(x=df[,1],y=df[,2])
})
names(data_list)<-result_list_wi_plants$family
merge_and_add_names <- function(list_of_dataframes) {
  names_list <- names(data_list)
  merged_df <- bind_rows(data_list, .id = "family")
  return(merged_df)
}
occ_df_wi_plants<-merge_and_add_names(data_list)

write.csv(occ_df_wi_plants,"occ_df_wi_plants.csv")
occ_df_wi_plants<-read.csv('/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/MPM Community Science Project/SDM/Plant_SDMS/occ_df_wi_plants.csv')
#we did it Joe
#test individual SDM
aster<-filter(occ_df_wi_plants_filtered,family=="Asteraceae")

#which algos work
SDM_MODELS<-list()
GLM <- SSDM::modelling("GLM", aster,
                 climate_crop_raster, Xcol = 'x', Ycol = 'y', verbose = TRUE)
GAM <- SSDM::modelling("GAM", aster,
                       climate_crop_raster, Xcol = 'x', Ycol = 'y', verbose = TRUE)
MARS <- SSDM::modelling("MARS", aster,
                        climate_crop_raster, Xcol = 'x', Ycol = 'y', verbose = TRUE)
GBM <- SSDM::modelling("GBM", aster,
                       climate_crop_raster, Xcol = 'x', Ycol = 'y', verbose = TRUE)
CTA <- SSDM::modelling("CTA", aster,
                       climate_crop_raster, Xcol = 'x', Ycol = 'y', verbose = TRUE)
RF <- SSDM::modelling("RF", aster,
                       climate_crop_raster_avg, Xcol = 'x', Ycol = 'y', verbose = TRUE)
MAXENT <- SSDM::modelling("MAXENT", aster,
                          climate_crop_mask_raster_avg_wi, Xcol = 'x', Ycol = 'y', verbose = TRUE)
ANN <- SSDM::modelling("ANN", aster,
                       climate_crop_mask_raster_avg_wi, Xcol = 'x', Ycol = 'y', verbose = TRUE)
SVM <- SSDM::modelling("SVM", aster,
                       climate_crop_raster, Xcol = 'x', Ycol = 'y', verbose = TRUE)
SDM_MODELS<-list(GLM@evaluation,GAM@evaluation,GBM@evaluation,MARS@evaluation,CTA@evaluation,MAXENT@evaluation,ANN@evaluation,SVM@evaluation)

knitr::kable(SDM_MODELS)
#all close--GAM the best
plot(GAM@projection, main = 'SDM\nfor Asteraceae \nwith GAM algorithms')
#can't use maxent, GBM (needs update)
#stacked modelling
# WI_shape<-st_read("/Users/kaΩthrynsullivan/Downloads/Wisconsin_State_Boundary_24K/Wisconsin_State_Boundary_24K.shp")
# WI_raster<-st_transform(WI_shape,"+proj=longlat +ellps=WGS84 +datum=WGS84")
# WI_r<-crop(climate_early_raster,WI_raster)
###plants with env only
#for server
# Saving both raster and occurrence data to an .RData file
save(climate_crop_raster, occ_df_wi_plants, file = "ssdm_data.RData")
SSDM_plant_bg <- SSDM::stack_modelling(c("ANN","CTA","GAM","GBM","GLM","MARS","MAXENT","SVM"), occ_df_wi_plants, climate_crop_raster, rep = 1, 
                                    Xcol = 'x', Ycol = 'y',maxent.args=list(nbg=1000),
                                    Spcol = 'family',method = "pSSDM",verbose = TRUE,cores=7)#### max cores cannot be auto calculated or 0 apparently
plant_rasters <- lapply(SSDM_plant_bg@esdms, function(sdm) sdm@projection)
# Stack SDM rasters into one multi-layer raster
plant_stack <- stack(plant_rasters)

# Combine with your climate raster (or stack if there are multiple climate layers)
plant_combined_stack <- stack(plant_stack, climate_crop_raster)

#rerun with early dataset
climate_raster_early<-raster::stack(ave_tmin_early,ave_ppt_early)
library(sf)
basemap <- st_read("/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/MPM Community Science Project/SDM/occ_historical/analysis/odonates/scripts/ne_10m_land.shp")
basemap <- basemap %>% 
  st_crop(xmin=-92.9, xmax=-86.8,
          ymin=42.5, ymax=47.3) %>% st_transform(crs_1)
basemap_df <- as.data.frame(st_coordinates(basemap))
colnames(basemap_df)<-c("long","lat")
climate_crop_raster_early<-raster::crop(climate_raster_early, extent(basemap))
#convert raster to point data
proj4string(climate_crop_raster_early)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info
proj4string(climate_crop_raster_early)<-CRS(crs_1) ##assign projection info
occ_sf <- st_as_sf(xe_time_early, coords = c("longitude", "latitude"), crs = 4326)  # WGS84
# Reproject occurrence data to match climate raster
occ_projected <- st_transform(occ_sf, crs = crs(climate_crop_raster_early))
# Step 3: Extract coordinates back for grouping
xe_time_proj <- occ_projected %>%
  mutate(longitude = st_coordinates(.)[,1],
         latitude = st_coordinates(.)[,2]) %>%
  st_drop_geometry()
# Create a bounding box object...need to do with env
bbox<-extent(climate_crop_raster_early)
# Step 1: Ensure CRS of climate raster is stored
clim_crs <- crs(climate_crop_raster_early)

result_list_wi_plants_early<-list()#prep list for plant samples
result_list_wi_plants_early <- xe_time_proj %>%
  group_by(family) %>% #family level diversity
  nest() %>%
  mutate(sampled_values = map2(data, family, function(group_data,group_name) {
    lon_range <- range(group_data$longitude)
    lat_range <- range(group_data$latitude)
    
    if (diff(lon_range) == 0 || diff(lat_range) == 0) {
      # Skip this group if no spatial extent
      return(NULL)
    }
    # Create SpatialPointsDataFrame with climate raster CRS
    coordinates(group_data) <- ~longitude + latitude
    proj4string(group_data) <- clim_crs
    
    # Define a raster based on the group extent
    base_raster <- raster(extent(group_data))
    res(base_raster) <- 0.04  # Set desired resolution
    crs(base_raster) <- clim_crs
    
    # Optionally buffer the extent a bit
    base_raster <- extend(base_raster, extent(base_raster) + 1)
    
    # Sample values from the raster at points defined by the SpatialPointsDataFrame
    acsel <- gridSample(group_data, base_raster, n = 1)
    acsel <- data.frame(acsel)
    
    # Name the dataframe based on the group name
    return(acsel)
  })) %>%
  dplyr::select(-data)
#find low point families
result_list_wi_plants_early<-filter(result_list_wi_plants_early,!sampled_values=="NULL")

##########combine climate and species
head(result_list_wi_plants_early)
names(result_list_wi_plants_early$sampled_values)<-result_list_wi_plants_early$family#list of acsel df for each species
filtered_list <-result_list_wi_plants_early$sampled_values[!sapply(result_list_wi_plants_early$sampled_values,is.null)] #removed families with low points
library(sf) #note need to install with gdal for proper updates
occurrence_wi_sp_early<-sapply(filtered_list,SpatialPoints)
climate_df <- as.data.frame(rasterToPoints(climate_crop_raster_early))

###spot check 
aster_temp<-filtered_list$Asteraceae
names(aster_temp)<-c("longitude","latitude")
###single plot w temp
ggplot() +
  geom_raster(data = climate_df, aes(x = x, y = y, fill = layer.1)) +geom_point(data = aster_temp, aes(x =longitude, y = latitude), size = 0.5) +
  scale_fill_gradient(low = "blue", high = "red", name = "1967 Temp") +
  labs(x = "Longitude", y = "Latitude", fill = "Mean Monthly Temp (degC)")  + ggtitle("Asteraceae") +
  theme_minimal()
# Saving both raster and occurrence data to an .RData file
library(SSDM)
#convert occ_sp data into dataframe
data_list<-lapply(result_list_wi_plants_early$sampled_values,function(df){
  data.frame(x=df[,1],y=df[,2])
})
names(data_list)<-result_list_wi_plants_early$family
merge_and_add_names <- function(list_of_dataframes) {
  names_list <- names(data_list)
  merged_df <- bind_rows(data_list, .id = "family")
  return(merged_df)
}
occ_df_wi_plants_early<-merge_and_add_names(data_list)

write.csv(occ_df_wi_plants_early,"occ_df_wi_plants_early.csv")

save(climate_crop_raster_early, occ_df_wi_plants_early, file = "ssdm_data_early.RData")
SSDM_plant_early_bg <- SSDM::stack_modelling(c("ANN","CTA","GAM","GBM","GLM","MARS","MAXENT","SVM"), occ_df_wi_plants_early, climate_crop_raster_early, rep = 1, 
                                       Xcol = 'x', Ycol = 'y',maxent.args=list(nbg=100),
                                       Spcol = 'family',method = "pSSDM",verbose = TRUE,cores=7)#### max cores cannot be auto calculated or 0 apparently
plant_rasters_early <- lapply(SSDM_plant_early_bg@esdms, function(sdm) sdm@projection)
# Stack SDM rasters into one multi-layer raster
plant_stack_early <- stack(plant_rasters_early)

# Combine with your climate raster (or stack if there are multiple climate layers)
plant_combined_stack_early <- stack(plant_stack_early, climate_crop_raster_early)

##late
climate_raster_late<-raster::stack(ave_tmin_late,ave_ppt_late)
library(sf)
basemap <- st_read("/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/MPM Community Science Project/SDM/occ_historical/analysis/odonates/scripts/ne_10m_land.shp")
basemap <- basemap %>% 
  st_crop(xmin=-92.9, xmax=-86.8,
          ymin=42.5, ymax=47.3) %>% st_transform(crs_1)
basemap_df <- as.data.frame(st_coordinates(basemap))
colnames(basemap_df)<-c("long","lat")
climate_crop_raster_late<-raster::crop(climate_raster_late, extent(basemap))
#convert raster to point data
proj4string(climate_crop_raster_late)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info
proj4string(climate_crop_raster_late)<-CRS(crs_1) ##assign projection info
occ_sf <- st_as_sf(xe_time_late, coords = c("longitude", "latitude"), crs = 4326)  # WGS84
# Reproject occurrence data to match climate raster
occ_projected <- st_transform(occ_sf, crs = crs(climate_crop_raster_late))
# Step 3: Extract coordinates back for grouping
xe_time_proj <- occ_projected %>%
  mutate(longitude = st_coordinates(.)[,1],
         latitude = st_coordinates(.)[,2]) %>%
  st_drop_geometry()
# Create a bounding box object...need to do with env
bbox<-extent(climate_crop_raster_late)
# Step 1: Ensure CRS of climate raster is stored
clim_crs <- crs(climate_crop_raster_late)

result_list_wi_plants_late<-list()#prep list for plant samples
result_list_wi_plants_late <- xe_time_proj %>%
  group_by(family) %>% #family level diversity
  nest() %>%
  mutate(sampled_values = map2(data, family, function(group_data,group_name) {
    lon_range <- range(group_data$longitude)
    lat_range <- range(group_data$latitude)
    
    if (diff(lon_range) == 0 || diff(lat_range) == 0) {
      # Skip this group if no spatial extent
      return(NULL)
    }
    # Create SpatialPointsDataFrame with climate raster CRS
    coordinates(group_data) <- ~longitude + latitude
    proj4string(group_data) <- clim_crs
    
    # Define a raster based on the group extent
    base_raster <- raster(extent(group_data))
    res(base_raster) <- 0.04  # Set desired resolution
    crs(base_raster) <- clim_crs
    
    # Optionally buffer the extent a bit
    base_raster <- extend(base_raster, extent(base_raster) + 1)
    
    # Sample values from the raster at points defined by the SpatialPointsDataFrame
    acsel <- gridSample(group_data, base_raster, n = 1)
    acsel <- data.frame(acsel)
    
    # Name the dataframe based on the group name
    return(acsel)
  })) %>%
  dplyr::select(-data)
#find low point families
result_list_wi_plants_late<-filter(result_list_wi_plants_late,!sampled_values=="NULL")

##########combine climate and species
head(result_list_wi_plants_late)
names(result_list_wi_plants_late$sampled_values)<-result_list_wi_plants_late$family#list of acsel df for each species
filtered_list <-result_list_wi_plants_late$sampled_values[!sapply(result_list_wi_plants_late$sampled_values,is.null)] #removed families with low points
library(sf) #note need to install with gdal for proper updates
occurrence_wi_sp_late<-sapply(filtered_list,SpatialPoints)
climate_df <- as.data.frame(rasterToPoints(climate_crop_raster_late))

###spot check 
aster_temp<-filtered_list$Asteraceae
names(aster_temp)<-c("longitude","latitude")
###single plot w temp
ggplot() +
  geom_raster(data = climate_df, aes(x = x, y = y, fill = layer.1)) +geom_point(data = aster_temp, aes(x =longitude, y = latitude), size = 0.5) +
  scale_fill_gradient(low = "blue", high = "red", name = "1967 Temp") +
  labs(x = "Longitude", y = "Latitude", fill = "Mean Monthly Temp (degC)")  + ggtitle("Asteraceae") +
  theme_minimal()
# Saving both raster and occurrence data to an .RData file
library(SSDM)
#convert occ_sp data into dataframe
data_list<-lapply(result_list_wi_plants_late$sampled_values,function(df){
  data.frame(x=df[,1],y=df[,2])
})
names(data_list)<-result_list_wi_plants_late$family
merge_and_add_names <- function(list_of_dataframes) {
  names_list <- names(data_list)
  merged_df <- bind_rows(data_list, .id = "family")
  return(merged_df)
}
occ_df_wi_plants_late<-merge_and_add_names(data_list)

write.csv(occ_df_wi_plants_late,"occ_df_wi_plants_late.csv")

save(climate_crop_raster_late, occ_df_wi_plants_late, file = "ssdm_data_late.RData")
SSDM_plant_late_bg <- SSDM::stack_modelling(c("ANN","CTA","GAM","GBM","GLM","MARS","MAXENT","SVM"), occ_df_wi_plants_late, climate_crop_raster_late, rep = 1, 
                                             Xcol = 'x', Ycol = 'y',maxent.args=list(nbg=1000),
                                             Spcol = 'family',method = "pSSDM",verbose = TRUE,cores=7)#### max cores cannot be auto calculated or 0 apparently
plant_rasters_late <- lapply(SSDM_plant_late_bg@esdms, function(sdm) sdm@projection)
# Stack SDM rasters into one multi-layer raster
plant_stack_late <- stack(plant_rasters_late)

# Combine with your climate raster (or stack if there are multiple climate layers)
plant_combined_stack_late <- stack(plant_stack_late, climate_crop_raster_late)

###pretty plot
library(rasterVis)
library(maptools)
# Plot the richness stack using rasterVis
# Get Wisconsin state outline
wi_outline <- usmap::us_map(regions = "states", include = "WI")
# Convert to a data frame
wi_outline_df <- fortify(wi_outline)
#SSDM_plant<-readRDS("/Users/kathrynsullivan/Downloads/ssdm_stack_wi_plant.rds")
richness_df <- as.data.frame(rasterToPoints(SSDM_plant_early_bg@diversity.map), xy = TRUE)
# Create the plot
ggplot() +
  geom_raster(data = richness_df, aes(x = x, y = y, fill = diversity)) +
  scale_fill_gradientn(colours = rev(terrain.colors(100))) +  # Plot raster
  #geom_polygon(data = wi_outline_df, 
               #color = "black", fill = NA) +  # Add state outlines
  #geom_point(data = occ_df_wi_plants, aes(x = x, y = y), color = "black", size = 0.25) +
  coord_fixed() +
  theme_minimal() +
  labs(title = "SSDM Plant Richness Stack",
       fill = "Richness") +xlab("longitude") +ylab("latitude") 

plot(SSDM_plant_bg@diversity.map, main = 'Estimated Plant Richness\n by Climate Suitability')
plot(CanUS.proj,xlim = c(-92.9,-86.8),ylim= c(42.5, 47.3),
     add = TRUE)

#evaluation of SSDM--full
knitr::kable(SSDM_plant_bg@evaluation)
#importance analysis of env variables
knitr::kable(SSDM_plant_bg@variable.importance)
#evaluation of SSDM--early
knitr::kable(SSDM_plant_early_bg@evaluation)
#importance analysis of env variables
knitr::kable(SSDM_plant_late_bg@variable.importance) #overwhelm for temp
#evaluation of SSDM--early
knitr::kable(SSDM_plant_late_bg@evaluation)
#importance analysis of env variables
knitr::kable(SSDM_plant_early_bg@variable.importance) #overwhelm for temp

###stack with other raster
richness_raster_all<-stack(climate_crop_raster,SSDM_plant_bg@diversity.map)
richness_raster_early<-stack(climate_crop_raster_early,SSDM_plant_early_bg@diversity.map)
richness_raster_late<-stack(climate_crop_raster_late,SSDM_plant_late_bg@diversity.map)

# ####butterfly data cleaning
# ####idigbio
library(ridigbio)
idb <- idig_search_records(
  rq = list(
    family = c(
      "Hesperiidae",
      "Lycaenidae",
      "Pieridae",
      "Papilionidae",
      "Nymphalidae",
      "Riodinidae"
    ),
    stateprovince = "Wisconsin",
    geopoint = list(type = "exists")
  ),
  # `fields` is where you adjust what fields you want returned by the API
  #fields = c("uuid",
  # "family",
  # "genus",
  # "specificepithet",
  # "scientificname",
  # "stateprovince","eventDate")
  # `limit` is where you can set a limit on the number of records to return in
  # order to speed up your query; max is 100000
  limit = 100000,
  # `sort` is where you can specify fields for sorting
  #sort = c("stateprovince",
  #"scientificname","year""))
)
idb <- idb %>% rename(catalogNumber = "catalognumber")
#GBIF download[use gators] with idigbio
#complex data download
name_backbone("Papilionidae") #find the taxonKey (usageKey)
#9417
name_backbone("Pieridae") #find the taxonKey (usageKey)
#5481
name_backbone("Hesperiidae") #find the taxonKey (usageKey)
#6953
name_backbone("Nymphalidae") #find the taxonKey (usageKey)
#7017
name_backbone("Lycaenidae") #find the taxonKey (usageKey)
#5473
name_backbone("Riodinidae") #find the taxonKey (usageKey)
#1933999
butterfly_taxonkey <- c(9417, 5481, 6953, 7017, 5473, 1933999)
#custom search
gbif_download <- occ_download(
  pred("hasCoordinate", TRUE),
  pred_in("taxonKey", butterfly_taxonkey),
  pred("stateProvince", "Wisconsin"),
  format = "SIMPLE_CSV"
)
#get data
occ_download_wait(gbif_download)
gbif_bf <- gbif_download %>%
  occ_download_get() %>%
  occ_download_import() #should have all occurrences and observations for WI

EMU <- read.csv(
  "/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/MPM Community Science Project/butterfly/CSP_butterflies/mpm_emu_wi_georef.csv"
) #updated 9/18/2024
head(EMU)
SCAN <- read.csv(
  "/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/MPM Community Science Project/butterfly/CSP_butterflies/scan_bfly_wi/occurrences.csv"
) #downloaded 9/17/2024
head(SCAN)
#looking for dupes and null cat. numbers
SCAN$catalogNumber[duplicated(SCAN$catalogNumber)]
SCAN$catalogNumber[is.na(SCAN$catalogNumber)]
EMU$catalogNumber[duplicated(EMU$catalogNumber)]
EMU$catalogNumber[is.na(EMU$catalogNumber)]
gbif_bf$catalogNumber[duplicated(gbif_bf$catalogNumber)]
gbif_bf$catalogNumber[is.na(gbif_bf$catalogNumber)]
EMU$day <- as.integer(EMU$day)
#merge to include all cat numbers in GBIF EMU and SCAN
library(dplyr) #ADD IN MISSING CATALOG NUMBERS
gbif_bf <- gbif_bf %>%
  mutate(catalogNumber = ifelse(institutionCode == "BISON", as.numeric(
    gsub(".*/([0-9]+)$", "\\1", occurrenceID)
  ), catalogNumber))
SCAN <- SCAN %>%
  mutate(catalogNumber = ifelse(institutionCode == "BISON", as.numeric(
    gsub(".*/([0-9]+)$", "\\1", occurrenceID)
  ), catalogNumber))

SCAN <- SCAN %>%
  mutate(catalogNumber = ifelse(
    institutionCode == "LEPSOC",
    occurrenceID,
    # Directly assign occurrenceID
    catalogNumber
  )) # Retain existing catalogNumber where not NA
# Full join without specifying by, which uses all common columns automatically
shared_cols<-janitor::compare_df_cols(EMU, SCAN) %>% filter(!SCAN=="<NA>" & !EMU=="<NA>")
shared_cols<-shared_cols$column_name
shared_cols<-shared_cols[-(2)]#remove cat number
full_merged_df <- full_join(EMU,SCAN,by="catalogNumber")
# Mutate using coalesce in a loop
for (col in shared_cols) {
  df1_col <- paste0(col, ".x")
  df2_col <- paste0(col, ".y")
  
  full_merged_df <- full_merged_df %>%
    mutate(!!sym(col) := coalesce(.data[[df1_col]], .data[[df2_col]]))
}
full_merged_df <- full_merged_df %>%
  dplyr::select(-matches("\\.x$|\\.y$"))
shared_cols<-janitor::compare_df_cols(full_merged_df, gbif_bf) %>% filter(!full_merged_df=="<NA>" & !gbif_bf=="<NA>")
shared_cols<-shared_cols$column_name[-2]

full_merged_df <- full_merged_df %>%
  full_join(gbif_bf,by="catalogNumber")
for (col in shared_cols) {
  df1_col <- paste0(col, ".x")
  df2_col <- paste0(col, ".y")
  
  full_merged_df <- full_merged_df %>%
    mutate(!!sym(col) := coalesce(.data[[df1_col]], .data[[df2_col]]))
}
full_merged_df <- full_merged_df %>%
  dplyr::select(-matches("\\.x$|\\.y$"))
full_merged_df_clean <- full_merged_df %>%
  mutate(across(where(is.character), trimws))

##find duplicates in merged
full_merged_df$catalogNumber[duplicated(full_merged_df$catalogNumber)]
# Merging rows by `catNumber` keeping the first non-NA value for each column
#Mostly deals with merging the EMU and SCAN data with newly georeferenced coordinates
full_merged_df <- full_merged_df %>%
  group_by(catalogNumber) %>%
  mutate(
    decimalLatitude = first(decimalLatitude),
    decimalLongitude = first(decimalLongitude)
  ) %>%
  distinct(catalogNumber, .keep_all = TRUE)  # Retain only one row per catalogNumber, keeping all columns
#repeat
full_merged_df$catalogNumber[duplicated(full_merged_df$catalogNumber)]
#remove non lep records
full_merged_df_clean <- filter(full_merged_df_clean, order =="Lepidoptera")
#butts only
butts <- full_merged_df_clean %>% filter(
  family %in% c(
    "Nymphalidae",
    "Pieridae",
    "Papilionidae",
    "Lycaenidae",
    "Hesperiidae",
    "Riodinidae"
  )
)
##import controlled WI species list
#taxo<-read.csv("/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/MPM Community Science Project/WIButterflytaxo.csv")
#keep only butterfly names per Pollard list
#butts <- subset(butts, scientificName.y %in% taxo$BAMONA.Name | scientificName.x %in% taxo$BAMONA.Name)
head(butts)
butts <- butts[, -c(85:96)]
#cleaning up my messy codes, trim columns to relevant
occurrence_bfly <- butts %>%
  dplyr::select(
    catalogNumber,
    institutionCode,
    collectionCode,
    basisOfRecord,
    order,
    family,
    genus,
    specificEpithet,
    infraspecificEpithet,
    taxonRank,
    scientificName,
    scientificNameAuthorship,
    recordedBy,
    eventDate,
    year,
    month,
    day,
    verbatimEventDate,
    occurrenceRemarks,
    habitat,
    associatedTaxa,
    establishmentMeans,
    lifeStage,
    sex,
    individualCount,
    country,
    stateProvince,
    county,
    municipality,
    locality,
    decimalLatitude,
    decimalLongitude,
    verbatimCoordinates,
    locationRemarks,
    references
  )
write.csv(occurrence_bfly,"/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/MPM Community Science Project/butterfly/CSP_butterflies/occurrence_bfly_2025.csv")
no_coords <- filter(occurrence_bfly, is.na(decimalLatitude))
head(no_coords) #not terrible
occurrence_bfly <- filter(occurrence_bfly, !is.na(decimalLatitude))
#filter to skippers
occurrence_bfly_hesp <- filter(occurrence_bfly, family == "Hesperiidae")
###NOTE go back and try to get some of the records georefed for full analysis
#Pollard
pollard <- read.csv(
  "/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/2023-2024/SDM/SDM/amalg_2024.csv"
)
##should i keep duplicate records for different dates or just anytime? or just different years?
###more cleaning for butterfly records
#specimen<-data.frame(occurrence_bfly_hesp$catalogNumber,occurrence_bfly_hesp$year,occurrence_bfly_hesp$scientificName,occurrence_bfly_hesp$decimallatitude,occurrence_bfly_hesp$decimallongitude)
#head(specimen)
#standard col names
#new_col_names<-c("catalogNumber","year","scientificName","decimalLatitude","decimalLongitude")
#colnames(specimen)<-new_col_names
#head(specimen)
head(pollard)
pollard_trim <- data.frame(
  pollard$Site,
  pollard$Section,
  pollard$eventDate,
  pollard$Taxon,
  pollard$Count,
  pollard$decimalLatitude,
  pollard$decimalLongtidue
)
pollard_trim$catalogNumber <- paste("P", seq_len(nrow(pollard_trim)), sep = "")
new_col_names <- c(
  "locality",
  "habitat",
  "eventDate",
  "scientificName",
  "count",
  "decimalLatitude",
  "decimalLongitude",
  "catalogNumber"
)
colnames(pollard_trim) <- new_col_names
pollard_trim$decimalLatitude<-as.numeric(pollard_trim$decimalLatitude)
pollard_trim$decimalLongitude<-as.numeric(pollard_trim$decimalLongitude)

nrow(pollard)
##clean
head(pollard_trim)
pollard_trim$eventDate <- parse_date_time(pollard_trim$eventDate,
                                          orders = c("ymd HM", "mdy HM", "dmy", "ymd"))
pollard_names <- unique(pollard_trim$scientificName)
shared_cols<-janitor::compare_df_cols(occurrence_bfly, pollard_trim) %>% filter(!occurrence_bfly=="<NA>" & !pollard_trim=="<NA>")
shared_cols<-shared_cols$column_name[-1]

full_merged_df <- occurrence_bfly %>%
  full_join(pollard_trim,by="catalogNumber")
for (col in shared_cols) {
  df1_col <- paste0(col, ".x")
  df2_col <- paste0(col, ".y")
  
  full_merged_df <- full_merged_df %>%
    mutate(!!sym(col) := coalesce(.data[[df1_col]], .data[[df2_col]]))
}
full_merged_df <- full_merged_df %>%
  dplyr::select(-matches("\\.x$|\\.y$"))
full_merged_df_clean <- full_merged_df %>%
  mutate(across(where(is.character), trimws))
bamona <- read.csv(
  "/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/MPM Community Science Project/butterfly/CSP_butterflies/bamona_data_11_12_2024.csv"
)
new_col_names_bamona <- c(
  "catalogNumber",
  "scientificName",
  "eventDate",
  "basisOfRecord",
  "lifeStage",
  "establishmentMeans",
  "locality",
  "decimalLatitude",
  "decimalLongitude",
  "locationRemarks",
  "occurrenceRemarks",
  "recordedBy",
  "institutionCode",
  "collectionCode",
  "references"
)
bamona <- bamona[, -c(4, 11, 17)]
colnames(bamona) <- new_col_names_bamona
bamona$catalogNumber<-as.character(bamona$catalogNumber)
shared_cols<-janitor::compare_df_cols(full_merged_df_clean, bamona) %>% filter(!full_merged_df_clean=="<NA>" & !bamona=="<NA>")
shared_cols<-shared_cols$column_name[-2]

full_merged_df_clean <- full_merged_df_clean %>%
  full_join(bamona,by="catalogNumber")
for (col in shared_cols) {
  df1_col <- paste0(col, ".x")
  df2_col <- paste0(col, ".y")
  
  full_merged_df_clean <- full_merged_df_clean %>%
    mutate(!!sym(col) := coalesce(.data[[df1_col]], .data[[df2_col]]))
}
full_merged_df_clean <- full_merged_df_clean %>%
  dplyr::select(-matches("\\.x$|\\.y$"))
write.csv(
  full_merged_df_clean,
  "/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/MPM Community Science Project/butterfly/CSP_butterflies/all_wi_butterfly2025.csv"
)
#####repeat sdm protocol with butterfly
library(dplyr)

#install.packages("CoordinateCleaner")
library(CoordinateCleaner)
occurrence_bfly_cc<-full_merged_df_clean%>%
  filter(!decimalLatitude == 0 | !decimalLongitude == 0) %>%
  filter(!decimalLatitude == "NA" | !decimalLongitude == "NA") %>%
  cc_cen(buffer = 2000) %>% # remove country centroids within 2km 
  cc_cap(buffer = 2000) %>% # remove capitals centroids within 2km
  cc_inst(buffer = 2000) %>% # remove zoo and herbaria within 2km 
  cc_sea() %>% # remove from ocean 
  #distinct(decimalLongitude,decimalLatitude,scientificName, .keep_all = TRUE) %>%
  glimpse() # look at results of pipeline
lat_between <- occurrence_bfly_cc$decimalLatitude[between(occurrence_bfly_cc$decimalLatitude, 42.5, 47.3)]
lon_between <- occurrence_bfly_cc$decimalLongitude[between(occurrence_bfly_cc$decimalLongitude, -92.9, -86.8)]
# Specify the bounding box for Wisconsin
wisconsin_bounds <- c(left = -92.9, bottom = 42.5, right = -86.8, top = 47.3)
length(occurrence_bfly_cc$decimalLatitude) - length(lat_between)
length(occurrence_bfly_cc$decimalLongitude) - length(lon_between)
#exclude the lat and long values outside those boundaries
occurrence_bfly_cc$long_out <- occurrence_bfly_cc$decimalLongitude %in% lon_between
occurrence_bfly_cc$lat_out <- occurrence_bfly_cc$decimalLatitude %in% lat_between
occurrence_bfly_cc_wi <- filter(occurrence_bfly_cc, long_out == "TRUE") %>% filter(lat_out == "TRUE") 
#filtered to WI coordinates
butt_wi<-occurrence_bfly_cc_wi%>%ungroup %>%drop_na(scientificName) %>% count(scientificName,sort=TRUE)#get counts of each species
butt_wi #soooomany species--match the pollard backbone
write.csv(butt_wi,"butt_wi_names.csv")
taxo_recon<-read.csv("/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/MPM Community Science Project/SDM/Namematch_result_COL24.11.csv")
taxo_recon$scientificName<-NULL
taxo_recon<-taxo_recon %>% rename("scientificName"=providedScientificName)
#match names
occurrence_bfly_cc_wi_names<-merge(occurrence_bfly_cc_wi,taxo_recon,by="scientificName") #use acceptedScientificName for congruence
#check stats
name_count<-occurrence_bfly_cc_wi_names %>% group_by(acceptedScientificName) %>%count(sort=TRUE) %>%print(n=100)
#199 names, remove non species
occurrence_bfly_cc_wi_names <-filter(occurrence_bfly_cc_wi_names, !genus.y=="")
name_count<-occurrence_bfly_cc_wi_names %>% group_by(acceptedScientificName) %>%count(sort=TRUE) %>%print(n=163)
#163
name_count_30<-filter(name_count,n>29) #106 taxa
occurrence_bfly_cc_wi_names_sp<-filter(occurrence_bfly_cc_wi_names,acceptedScientificName %in% name_count_30$acceptedScientificName)
#standardize basis of record column
occurrence_bfly_cc_wi_names_sp$basisOfRecord <- gsub("PreservedSpecimen|Preserved Specimen|PRESERVED_SPECIMEN|OCCURRENCE|Specimen|Historical", "PRESERVED_SPECIMEN", occurrence_bfly_cc_wi_names_sp$basisOfRecord)
occurrence_bfly_cc_wi_names_sp$basisOfRecord <- gsub("HumanObservation|humanobservation|HUMAN_OBSERVATION|Photograph|Sighting", "HUMAN_OBSERVATION", occurrence_bfly_cc_wi_names_sp$basisOfRecord)

#check
data_dist<-butterfly_presence %>% group_by(acceptedScientificName,basisOfRecord) %>%count(sort=FALSE) %>%print(n=100)
#remove material samples, probably dupes anyway
occurrence_bfly_cc_wi_names_sp<- filter(occurrence_bfly_cc_wi_names_sp,!basisOfRecord=="MATERIAL_SAMPLE")
write.csv(occurrence_bfly_cc_wi_names_sp,"/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/MPM Community Science Project/SDM/butterfly_occurrences_clean.csv")
butterfly_presence<-read.csv('/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/MPM Community Science Project/SDM/butterfly_occurrences_clean.csv',header=TRUE)

#make plot
butterfly_presence %>%
  group_by(basisOfRecord) %>% count()
data_dist<-butterfly_presence %>% group_by(year,basisOfRecord) %>%count(sort=FALSE) %>%print(n=100)
data_dist<-filter(data_dist,year>1800 & year<2025)
# Create barplot-----intro fig!!!!
ggplot(data_dist, aes(x = year, y = n, fill = basisOfRecord)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Butterfly Records by Year and  Type", 
       x = "Year", 
       y = "Number of Records") +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))
#(go to occdet analysis)
####sampling list, per each dataset
hesp_presence<-filter(butterfly_presence,family.x=="Hesperiidae"|family.y=="Hesperiidae")
# Convert to spatial object (sf or SpatialPoints)
occ_sf <- st_as_sf(butterfly_presence, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)  # WGS84
# Reproject occurrence data to match climate raster
occ_projected <- st_transform(occ_sf, crs = crs(richness_raster_late))
# Step 3: Extract coordinates back for grouping
butterfly_proj <- occ_projected %>%
  mutate(longitude = st_coordinates(.)[,1],
         latitude = st_coordinates(.)[,2]) %>%
  st_drop_geometry()

clim_crs <- crs(richness_raster_late)

library(purrr)
result_list_hesp_obs<-list()#prep list
hesp_proj<-filter(butterfly_proj,(family.x=="Hesperiidae"|family.y=="Hesperiidae") & basisOfRecord=="HUMAN_OBSERVATION")
result_list_hesp_obs <- butterfly_proj %>%
  filter((family.x=="Hesperiidae"| family.y=="Hesperiidae") & basisOfRecord=="HUMAN_OBSERVATION")%>%
  group_by(acceptedScientificName,basisOfRecord) %>%
  nest() %>%
  mutate(sampled_values = map2(data, acceptedScientificName,function(group_data,group_name) {# Create SpatialPointsDataFrame for each group
    lon_range <- range(group_data$longitude)
    lat_range <- range(group_data$latitude)
    
    if (diff(lon_range) == 0 || diff(lat_range) == 0) {
      # Skip this group if no spatial extent
      return(NULL)
    }
    # Create SpatialPointsDataFrame with climate raster CRS
    coordinates(group_data) <- ~longitude + latitude
    proj4string(group_data) <- clim_crs
    
    # Define a raster based on the group extent
    base_raster <- raster(extent(group_data))
    res(base_raster) <- 0.04  # Set desired resolution
    crs(base_raster) <- clim_crs
    
    # Optionally buffer the extent a bit
    base_raster <- extend(base_raster, extent(base_raster) + 1)
    
    # Sample values from the raster at points defined by the SpatialPointsDataFrame
    acsel <- gridSample(group_data, base_raster, n = 1)
    acsel <- data.frame(acsel)
    
    # Name the dataframe based on the group name
    return(acsel)
  })) %>%
  dplyr::select(-data)
names(result_list_hesp_obs$sampled_values)<-paste0(result_list_hesp_obs$acceptedScientificName,"_",result_list_hesp_obs$basisOfRecord)#list of acsel df for each species
# Define the new column names
filtered_list <-result_list_hesp_obs$sampled_values[!sapply(result_list_hesp_obs$sampled_values,is.null)] #removed families with low points

# # Rename the columns in each dataframe
# filtered_list <- lapply(filtered_list$sampled_values, function(df) {
#   setNames(df, new_column_names)
#})
library(sf) #note need to install with gdal for proper updates
# Function to convert each dataframe to a SpatialPointsDataFrame
convert_to_sp <- function(df) {
  # Create SpatialPoints object using longitude and latitude columns
  spatial_points <- SpatialPoints(coords = df[, c("longitude", "latitude")])
  
  # Convert to SpatialPointsDataFrame
  #spatial_df <- SpatialPointsDataFrame(spatial_points, data = df)
  
  return(spatial_points)
}

# Apply the function to each dataframe in the list
spatial_list <- map(filtered_list, convert_to_sp)
# # names(observation_wi_hesp)<-result_list_hesp_obs$acceptedScientificName #assign names of sci names to df
# # Assuming 'raster_list' contains raster layers and 'spatial_list' contains spatial objects
# rs_brick<-brick(subset_raster)
# # Use map2 to perform extract operation on each pair of corresponding elements
# extracted_values_bfly_list <- map(spatial_list, function(spatial_object) {
#   extracted_values <- raster::extract(rs_brick, spatial_object, method = "simple", sp = TRUE)
#   return(extracted_values)
# })

# 'extracted_values_list' will contain the results of extract operation for each pair of corresponding elements
#extracted_values <- raster::extract(summer_rast,occurrence_sp,method="simple",sp=TRUE)
#visualize data
climateraster_df <- as.data.frame(rasterToPoints(richness_raster_late))
AH_temp<-result_list_hesp_obs$sampled_values[1]
names(AH_temp)<-"Amblyscirtes hegon-observation"
##crs standard
###single plot w temp
ggplot() +
  geom_raster(data = climateraster_df, aes(x = x, y = y, fill = layer.1)) +geom_point(data = AH_temp$`Amblyscirtes hegon-observation`, aes(x =longitude, y = latitude), size = 0.5) +
  scale_fill_gradient(low = "blue", high = "red", name = "Min Temp 1990-2023") +
  labs(x = "Longitude", y = "Latitude", fill = "Min Temp 1990-2023")  + ggtitle("Amblyscirtes hegon observation records") +
  theme_minimal()
###with temp
###maxent
library(dismo)
library(rJava)
library(geodata)
system.file("java", package="dismo") #moved maxent.jar to package in r
setwd("/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/2023-2024/SDM/SDM/Hesp_SDMS")
#iterate for each species in occurrence_sp, just test out variables
run_maxent <- function(spatial_df,filename) {
  # Check if the dataframe has fewer than 5 records
  if (nrow(spatial_df@coords) < 5) {
    message("Skipping MaxEnt for ", filename, ": Not enough records (less than 5).")
    return(NULL)  # Return NULL to skip this dataset
  }
  # witholding a 20% sample for testing 
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=1000
  me<-maxent(plant_combined_stack_late,occtrain,nbg=nbg) #change to include plant rich
  pdf(file = filename)  # Open a PDF device
  plot(me)
  dev.off()             # Close the PDF device
  # Return the MaxEnt model
  return(me)
}

# Apply the function to each spatial object in the list
for (i in seq_along(spatial_list)) {
  filename <- paste0("MaxEnt_Model_Variables", names(spatial_list[i]), ".pdf")  # Define a unique filename for each plot
  result<-run_maxent(spatial_list[[i]], filename)  # Run MaxEnt and save plot as PDF
  # Check if result is NULL, which indicates that the species was skipped
  if (is.null(result)) {
    message("Species ", names(spatial_list)[i], " was skipped due to insufficient background points.")
  } else {
    message("MaxEnt model successfully run for group: ", names(spatial_list)[i])
  }
}
#nonplot
run_maxent <- function(spatial_df) {
  # Check if the dataframe has fewer than 5 records
  if (nrow(spatial_df@coords) < 5) {
    message("Skipping MaxEnt for ", filename, ": Not enough records (less than 5).")
    return(NULL)  # Return NULL to skip this dataset
  }
  # witholding a 20% sample for testing
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=1000
  me<-maxent(plant_combined_stack_late,occtrain,nbg=nbg)
  return(me)
}
mes_plant<-lapply(spatial_list,run_maxent) #get all me values for each species
# Initialize an empty list to store predictions--correct for nulls

#xyridiaceae-polites origenes--2 spp in WI in danger
#Campanulaceae;Hypoxidaceae
#####VIF
library(usdm)
library(raster)
library(dplyr)
run_vif_by_species <- function(spatial_list, n_points = 1000, vif_threshold = 3) {
  results <- list()
  for (i in seq_along(spatial_list)) {
    species_name <- names(spatial_list)[i]
    cat("Running VIF for:", species_name, "\n")
    
    # Get the raster for this species
    spatial_df <- spatial_list[[i]]
    
    # Sample background points
    bg <- randomPoints(plant_combined_stack_late, length(spatial_df))
    
    
    # Extract environment values
    env_data <- raster::extract(plant_combined_stack_late, bg)  # extract all env layers at bg
    env_df <- as.data.frame(env_data)
    
    # Remove rows with NA
    valid_rows <- complete.cases(env_df)
    bg <- bg[valid_rows, ]
    env_df <- env_df[valid_rows, ]
    
    # Run vifstep
    vif_result <- tryCatch({
      vifstep(env_df, th = vif_threshold)
    }, error = function(e) {
      cat("Skipping", species_name, "- error during vifstep:", conditionMessage(e), "\n")
      return(NULL)
    })
    
    # Store the result
    if (!is.null(vif_result)) {
      results[[species_name]] <- vif_result@results
    } else {
      results[[species_name]] <- NA
    }
  }
  
  return(results)
}
# Assuming `plant_combined_stack_late` is your RasterStack of SDMs
vif_results_list <- run_vif_by_species(spatial_list, n_points = 1000, vif_threshold = 3)

#####assess colinearity of plant fams/climate prior to maxent
##use to reduce plant_raster_stack
library(raster)
library(usdm)  # for vifstep

# Function to keep only non-collinear layers
retain_non_collinear_layers <- function(raster_stack, vif_result) {
  # Extract names of retained (non-collinear) variables
  retained_vars <- vif_result$Variables
  
  # Subset the raster stack to keep only these layers
  reduced_stack <- raster_stack[[retained_vars]]
  
  return(reduced_stack)
}

layers.reduced <- retain_non_collinear_layers(plant_combined_stack_late, vif_results_list$`Amblyscirtes hegon (Scudder, 1863)_HUMAN_OBSERVATION`)
layers.reduced<-stack(layers.reduced,richness_raster_late$diversity)
###worked
fold <- kfold(spatial_list$`Amblyscirtes hegon (Scudder, 1863)_HUMAN_OBSERVATION`, k=5)
occtest <- spatial_list$`Amblyscirtes hegon (Scudder, 1863)_HUMAN_OBSERVATION`[fold == 1, ]
occtrain <- spatial_list$`Amblyscirtes hegon (Scudder, 1863)_HUMAN_OBSERVATION`[fold != 1, ]
bg<-randomPoints(layers.reduced,length(spatial_list$`Amblyscirtes hegon (Scudder, 1863)_HUMAN_OBSERVATION`))
me<-maxent(layers.reduced,occtrain,a=bg,args=c("replicates=10"))
library(ENMeval)
env<-terra::rast(layers.reduced)
colnames(bg)<-c("longitude","latitude")
occ<-spatial_list$`Amblyscirtes hegon (Scudder, 1863)_HUMAN_OBSERVATION`@coords
result <- ENMevaluate(
  occs = occ,
  envs = env,
  bg = bg,
  tune.args = list(fc = c("L","LQ","LQH","H"), rm = 1:5), 
  partitions = "randomkfold",
  partition.settings=list(kfolds=5),
  algorithm = 'maxent.jar',
  doClamp = TRUE,
  overlap = TRUE,
)
eval.results(result)
mods.maxent.jar <- eval.models(result)
pred.L2 <- predict(mods.maxent.jar$fc.L_rm.2, env)
pred.L2 <- maxnet.predictRaster(mods.maxent.jar$fc.L_rm.2, env, os)
plot(pred.L2)

eval.variable.importance(result)

#with richness
run_maxent <- function(spatial_df,filename) {
  # Check if the dataframe has fewer than 5 records
  if (nrow(spatial_df@coords) < 5) {
    message("Skipping MaxEnt for ", filename, ": Not enough records (less than 5).")
    return(NULL)  # Return NULL to skip this dataset
  }
  # witholding a 20% sample for testing 
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=1000
  me<-maxent(richness_raster_late,occtrain,nbg=nbg)
  pdf(file = filename)  # Open a PDF device
  plot(me)
  dev.off()             # Close the PDF device
  # Return the MaxEnt model
  return(me)
}

# Apply the function to each spatial object in the list
for (i in seq_along(spatial_list)) {
  filename <- paste0("MaxEnt_Model_Variables_Richness", names(spatial_list[i]), ".pdf")  # Define a unique filename for each plot
  result<-run_maxent(spatial_list[[i]], filename)  # Run MaxEnt and save plot as PDF
  # Check if result is NULL, which indicates that the species was skipped
  if (is.null(result)) {
    message("Species ", names(spatial_list)[i], " was skipped due to insufficient background points.")
  } else {
    message("MaxEnt model successfully run for group: ", names(spatial_list)[i])
  }
}
####
run_maxent <- function(spatial_df,filename) {
  # Check if the dataframe has fewer than 5 records
  if (nrow(spatial_df@coords) < 5) {
    message("Skipping MaxEnt for ", filename, ": Not enough records (less than 5).")
    return(NULL)  # Return NULL to skip this dataset
  }
  # witholding a 20% sample for testing 
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=1000
  me<-maxent(richness_raster_late,occtrain,nbg=nbg)
  
#   # predict to entire dataset
r <- predict(me,richness_raster_late)
pdf(file = filename)  # Open a PDF device
plot(r,main=paste0("Suitability of locations in WI\n for ",names(spatial_list[i])))
points(spatial_df,col="black",pch=4,cex=.25)
dev.off()             # Close the PDF device
#   # Return the MaxEnt model
return(r)
}
# # Apply the function to each spatial object in the list
for (i in seq_along(spatial_list)) {
  filename <- paste0("MaxEnt_", names(spatial_list[i]), ".pdf")  # Define a unique filename for each plot
  result<- run_maxent(spatial_list[[i]], filename)  # Run MaxEnt and save plot as PDF
  # # Check if result is NULL, which indicates that the species was skipped
  if (is.null(result)) {
    message("Species ", names(spatial_list)[i], " was skipped due to insufficient background points.")
  } else {
    message("MaxEnt model successfully run for species: ", names(spatial_list)[i])
  }
}

# ##with hypertuning
run_maxent <- function(spatial_df,filename) {
#   # witholding a 20% sample for testing 
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  bg <- randomPoints(plant_combined_stack_late, length(spatial_df)) #specialized
#   # Check if the number of background points is below the threshold
#   if (nbg < threshold) {
#     message("Skipping species due to insufficient background points: ", nbg, " points.")
#     return(NULL)  # Skip running the MaxEnt model and return NULL
#   }
   me<-maxent(plant_combined_stack_late,p=occtrain,a=bg)

#     # Define the hyperparameters to test
  #h<- list(reg = seq(0.1, 3, 0.1), fc = c("lq", "lh", "lqp", "lqph", "lqpht"))
#   
#   # Test all the possible combinations with gridSearch
  # gs <- gridSearch(me, hypers = h, metric = "auc", test = occtest)
  # head(gs@results[order(-gs@results$test_AUC), ])  # Best combinations
  # 
  # # Use the genetic algorithm instead with optimizeModel
  # om <- optimizeModel(me, hypers = h, metric = "auc", test = occtest, seed = 4)
  # head(om@results)  # Best combinations
  
#   # predict to entire dataset
   r <- predict(me,plant_combined_stack_late)
   pdf(file = filename)  # Open a PDF device
   plot(r,main=paste0("Suitability of locations in WI\n for ",names(spatial_list[i])))
   points(spatial_df,col="black",pch=4,cex=.25)
  dev.off()             # Close the PDF device
#   # Return the MaxEnt model
  return(r)
 }
# # Apply the function to each spatial object in the list
for (i in seq_along(spatial_list)) {
  filename <- paste0("MaxEnt_Model_Tuned", names(spatial_list[i]), ".pdf")  # Define a unique filename for each plot
 result<- run_maxent(spatial_list[[i]], filename)  # Run MaxEnt and save plot as PDF
# # Check if result is NULL, which indicates that the species was skipped
 if (is.null(result)) {
message("Species ", names(spatial_list)[i], " was skipped due to insufficient background points.")
 } else {
   message("MaxEnt model successfully run for species: ", names(spatial_list)[i])
 }
}

##non plot maxent
run_maxent <- function(spatial_df) {
  # Check if the dataframe has fewer than 5 records
  if (nrow(spatial_df@coords) < 5) {
    message("Skipping MaxEnt for ", filename, ": Not enough records (less than 5).")
    return(NULL)  # Return NULL to skip this dataset
  }
  # witholding a 20% sample for testing
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=1000
  me<-maxent(richness_raster_late,occtrain,nbg=nbg)
  return(me)
}
mes<-lapply(spatial_list,run_maxent) #get all me values for each species
# Initialize an empty list to store predictions--correct for nulls
rs <- list()

# Loop through each MaxEnt model in the `mes` list
for (i in seq_along(mes)) {
  # Get the MaxEnt model
  me_model <- mes[[i]]
  name <- names(mes)[i]
  # Check if the model is not NULL
  if (!is.null(me_model)) {
    # Perform prediction using the model and raster stack
    prediction <- predict(me_model, richness_raster_late)
    
    # Store the prediction in the `rs` list
    rs[[name]] <- prediction
  } else {
    # If the model is NULL, print a message and continue
    message("Skipping model at index ", i, " because it is NULL.")
  }
}
##remove NULLS
# Remove NULL items from the list using Filter()
rs <- Filter(Negate(is.null), rs)
#rs@data contains predictors , @file contains points
#other options not used
#r <- predict(me, summer_rast,args=c("-P", "noautofeature", "nothreshold", "noproduct", paste("maximumbackground=",nrow(summer_rast), sep=""), "noaddsamplestobackground"))
#loop di doop no plots just model testing for each
#iterate for each species in occurrence_sp
model_evaluation_list_e1<-list()
model_evaluation_list_e2<-list()
model_evaluation_list_e3<-list()
#function for evaluating models
run_maxent <- function(spatial_df) {
  # Check if the dataframe has fewer than 5 records
  if (nrow(spatial_df@coords) < 5) {
    message("Skipping MaxEnt for ", filename, ": Not enough records (less than 5).")
    return(NULL)  # Return NULL to skip this dataset
  }
  # witholding a 20% sample for testing 
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=1000
  
  bg <- randomPoints(richness_raster_late, length(spatial_df))
  me<-maxent(richness_raster_late,occtrain,nbg=nbg)
  
  #simplest way to use 'evaluate'
  e1 <- dismo::evaluate(me, p=occtest, a=bg, x=richness_raster_late)
  return(e1) 
}

maxent_models_evaluate <- lapply(spatial_list, run_maxent) #list of maxents evaluated method 1

# alternative 1
run_maxent <- function(spatial_df) {
  # Check if the dataframe has fewer than 5 records
  if (nrow(spatial_df@coords) < 5) {
    message("Skipping MaxEnt for ", filename, ": Not enough records (less than 5).")
    return(NULL)  # Return NULL to skip this dataset
  }
  # witholding a 20% sample for testing 
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=1000
  bg <- randomPoints(richness_raster_late, length(spatial_df))
  me<-maxent(richness_raster_late,occtrain,nbg=nbg)
  # extract values
  pvtest <- data.frame(raster::extract(richness_raster_late, occtest))
  avtest <- data.frame(raster::extract(richness_raster_late, bg))
  
  e2 <- dismo::evaluate(me, p=pvtest, a=avtest)
  
  return(e2) 
}
maxent_models_evaluate2 <- lapply(spatial_list, run_maxent) #list of maxents
maxent_models_evaluate2
# alternative 2 
# predict to testing points 
run_maxent <- function(spatial_df,filename) {
  # Check if the dataframe has fewer than 5 records
  if (nrow(spatial_df@coords) < 5) {
    message("Skipping MaxEnt for ", filename, ": Not enough records (less than 5).")
    return(NULL)  # Return NULL to skip this dataset
  }
  # witholding a 20% sample for testing 
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  bg <- randomPoints(plant_combined_stack_late, length(spatial_df))
  # extract environment at sites
  randomBgEnv <- extract(plant_combined_stack_late, bg)
  randomBgEnv <- as.data.frame(randomBgEnv)
  isNa <- is.na(rowSums(randomBgEnv))
  if (any(isNa)) {
    bg <- bg[-which(isNa), ]
    randomBgEnv <- randomBgEnv[-which(isNa), ]
  }
  
  # combine with coordinates and rename coordinate fields
  randomBg <- cbind(bg, randomBgEnv)
  names(randomBg)[1:2] <- c("lon", "lat")
  PredVars.NoCor <- vifstep(randomBgEnv, th = 3)
  
  
  me<-maxent(plant_combined_stack_late,occtrain,a=bg,args=c("betamultiplier=0.3"))
  # extract values
  pvtest <- data.frame(raster::extract(plant_combined_stack_late, occtest))
  avtest <- data.frame(raster::extract(plant_combined_stack_late, bg))
  testp <- predict(me, pvtest) 
  testa <- predict(me, avtest) 
  
  e3 <- dismo::evaluate(p=testp, a=testa)
  pdf(file=filename)
  plot(e3,"ROC")
  dev.off()
  return(e3)
}
# Apply the function to each spatial object in the list using lapply
for (i in seq_along(spatial_list)) {
  filename <- paste0("MaxEnt_Model_TunedBM03_Plant_ROC", names(spatial_list[i]), ".pdf")  # Define a unique filename for each plot
  result<-run_maxent(spatial_list[[i]], filename)  # Run MaxEnt and save plot as PDF
  # Check if result is NULL, which indicates that the species was skipped
  if (is.null(result)) {
    message("Species ", names(spatial_list)[i], " was skipped due to insufficient background points.")
  } else {
    message("MaxEnt model successfully run for species: ", names(spatial_list)[i])
  }
}




###state borders
library(usmap)
us <- gadm(country = "USA", level = 1, resolution = 1,path="~/")
canada <- gadm(country = "CAN", level = 1, resolution = 1,
               path = "~/")
CRS<- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
CanUS <- rbind(us, canada)  
CanUS.proj <- terra::project(CanUS, CRS)

pdf(file = "Predict_SSS.pdf")  # Open a PDF device
plot_usmap(include="WI")+
  plot(richness_raster,main=paste0("Suitability of locations in the US\n for Epargyreus clarus"))
points(result_list_hesp_obs$sampled_values$`Epargyreus clarus (Cramer, 1775)_HUMAN_OBSERVATION`,col="black",pch=4,cex=.25)
points(occ_bf_specimen$`Epargyreus clarus (Cramer, 1775)`,col="white",pch=4,cex=.25)

plot(CanUS.proj,xlim = c(-92.9,-86.8),ylim= c(42.5, 47.3),
     add = TRUE)
dev.off()             # Close the PDF device
#repeat with stacked plant richness
library(dismo)
library(rJava)
library(geodata)
system.file("java", package="dismo") #moved maxent.jar to package in r
setwd("/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/2023-2024/SDM/SDM/Hesp_SDMS")
#iterate for each species in occurrence_sp, just test out variables
run_maxent <- function(spatial_df,filename) {
  # witholding a 20% sample for testing 
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=1000
  # # Check if the number of background points is below the threshold
  # if (nbg <= threshold) {
  #   message("Skipping species due to insufficient background points: ", occtrain, " points.")
  #   return(NULL)  # Skip running the MaxEnt model and return NULL
  # }
  me<-maxent(richness_raster,occtrain,nbg=nbg)
  pdf(file = filename)  # Open a PDF device
  plot(me)
  dev.off()             # Close the PDF device
  # Return the MaxEnt model
  return(me)
}

# Apply the function to each spatial object in the list
for (i in seq_along(observation_wi_hesp)) {
  filename <- paste0("MaxEnt_Model_Richness", names(observation_wi_hesp[i]), ".pdf")  # Define a unique filename for each plot
  result<-run_maxent(observation_wi_hesp[[i]], filename)  # Run MaxEnt and save plot as PDF
  # Check if result is NULL, which indicates that the species was skipped
  if (is.null(result)) {
    message("Species ", names(observation_wi_hesp)[i], " was skipped due to insufficient background points.")
  } else {
    message("MaxEnt model successfully run for species: ", names(observation_wi_hesp)[i])
  }
}
# ##with hypertuning
run_maxent <- function(spatial_df,filename) {
  #   # witholding a 20% sample for testing 
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=1000
  #   # Check if the number of background points is below the threshold
  #   if (nbg < threshold) {
  #     message("Skipping species due to insufficient background points: ", nbg, " points.")
  #     return(NULL)  # Skip running the MaxEnt model and return NULL
  #   }
  me<-maxent(richness_raster,occtrain,nbg=nbg)
  
  #     # Define the hyperparameters to test
  h<- list(reg = seq(0.1, 3, 0.1), fc = c("lq", "lh", "lqp", "lqph", "lqpht"))
  #   
  #   # Test all the possible combinations with gridSearch
  gs <- gridSearch(me, hypers = h, metric = "auc", test = occtest)
  head(gs@results[order(-gs@results$test_AUC), ])  # Best combinations
  
  # Use the genetic algorithm instead with optimizeModel
  om <- optimizeModel(me, hypers = h, metric = "auc", test = occtest, seed = 4)
  head(om@results)  # Best combinations
  
  #   # predict to entire dataset
  r <- predict(me,richness_raster)
  pdf(file = filename)  # Open a PDF device
  plot(r,main=paste0("Suitability of locations in WI\n for ",names(observation_wi_hesp[i])))
  points(spatial_df,col="black",pch=4,cex=.25)
  dev.off()             # Close the PDF device
  #   # Return the MaxEnt model
  return(r)
}
# # Apply the function to each spatial object in the list
for (i in seq_along(observation_wi_hesp)) {
  filename <- paste0("MaxEnt_Model_Richness", names(observation_wi_hesp[i]), ".pdf")  # Define a unique filename for each plot
  result<- run_maxent(observation_wi_hesp[[i]], filename)  # Run MaxEnt and save plot as PDF
  # # Check if result is NULL, which indicates that the species was skipped
  if (is.null(result)) {
    message("Species ", names(observation_wi_hesp)[i], " was skipped due to insufficient background points.")
  } else {
    message("MaxEnt model successfully run for species: ", names(observation_wi_hesp)[i])
  }
}
#something wrong
##non plot maxent
run_maxent <- function(spatial_df) {
  # witholding a 20% sample for testing
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=1000
  # # Check if the number of background points is below the threshold
  # if (nbg < threshold) {
  #   message("Skipping species due to insufficient background points: ", nbg, " points.")
  #   return(NULL)  # Skip running the MaxEnt model and return NULL
  # }
  me<-maxent(richness_raster,occtrain,nbg=nbg)
  return(me)
}
mes<-lapply(result_list_hesp_all,run_maxent) #get all me values for each species
# Initialize an empty list to store predictions--correct for nulls
rs <- list()

# Loop through each MaxEnt model in the `mes` list
for (i in seq_along(mes)) {
  # Get the MaxEnt model
  me_model <- mes[[i]]
  name <- names(mes)[i]
  # Check if the model is not NULL
  if (!is.null(me_model)) {
    # Perform prediction using the model and raster stack
    prediction <- predict(me_model, richness_raster)
    
    # Store the prediction in the `rs` list
    rs[[name]] <- prediction
  } else {
    # If the model is NULL, print a message and continue
    message("Skipping model at index ", i, " because it is NULL.")
  }
}
##remove NULLS
# Remove NULL items from the list using Filter()
rs <- Filter(Negate(is.null), rs)
#rs@data contains predictors , @file contains points
#other options not used
#r <- predict(me, summer_rast,args=c("-P", "noautofeature", "nothreshold", "noproduct", paste("maximumbackground=",nrow(summer_rast), sep=""), "noaddsamplestobackground"))
#loop di doop no plots just model testing for each
#iterate for each species in occurrence_sp
model_evaluation_list_e1<-list()
model_evaluation_list_e2<-list()
model_evaluation_list_e3<-list()
#function for evaluating models
run_maxent <- function(spatial_df) {
  # witholding a 20% sample for testing 
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=1000
  
  bg <- randomPoints(richness_raster, length(spatial_df))
  me<-maxent(richness_raster,occtrain,nbg=nbg)
  
  #simplest way to use 'evaluate'
  e1 <- evaluate(me, p=occtest, a=bg, x=richness_raster)
  return(e1) 
}

maxent_models_evaluate <- lapply(observation_wi_hesp, run_maxent) #list of maxents evaluated method 1

# alternative 1
run_maxent <- function(spatial_df) {
  # witholding a 20% sample for testing 
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=1000
  bg <- randomPoints(richness_raster, length(spatial_df))
  me<-maxent(richness_raster,occtrain,nbg=nbg)
  # extract values
  pvtest <- data.frame(raster::extract(richness_raster, occtest))
  avtest <- data.frame(raster::extract(richness_raster, bg))
  
  e2 <- evaluate(me, p=pvtest, a=avtest)
  
  return(e2) 
}
maxent_models_evaluate2 <- lapply(observation_wi_hesp, run_maxent) #list of maxents
maxent_models_evaluate2
# alternative 2 
# predict to testing points 
run_maxent <- function(spatial_df,filename) {
  # witholding a 20% sample for testing 
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=1000# Check if the number of background points is below the threshold
  bg <- randomPoints(richness_raster, length(spatial_df))
  me<-maxent(richness_raster,occtrain,nbg=nbg)
  # extract values
  pvtest <- data.frame(raster::extract(richness_raster, occtest))
  avtest <- data.frame(raster::extract(richness_raster, bg))
  testp <- predict(me, pvtest) 
  testa <- predict(me, avtest) 
  
  e3 <- evaluate(p=testp, a=testa)
  pdf(file=filename)
  plot(e3,"ROC")
  dev.off()
  return(e3)
}
# Apply the function to each spatial object in the list using lapply
for (i in seq_along(observation_wi_hesp)) {
  filename <- paste0("MaxEnt_Model_Richness_ROC", names(observation_wi_hesp[i]), ".pdf")  # Define a unique filename for each plot
  result<-run_maxent(observation_wi_hesp[[i]], filename)  # Run MaxEnt and save plot as PDF
  # Check if result is NULL, which indicates that the species was skipped
  if (is.null(result)) {
    message("Species ", names(observation_wi_hesp)[i], " was skipped due to insufficient background points.")
  } else {
    message("MaxEnt model successfully run for species: ", names(observation_wi_hesp)[i])
  }
}
####sampling list, per each dataset
#occurrence
###with stacked plant richness
run_maxent <- function(spatial_df,filename) {
  # witholding a 20% sample for testing 
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=1000
  # # Check if the number of background points is below the threshold
  # if (nbg <= threshold) {
  #   message("Skipping species due to insufficient background points: ", occtrain, " points.")
  #   return(NULL)  # Skip running the MaxEnt model and return NULL
  # }
  me<-maxent(richness_raster,occtrain,nbg=nbg)
  pdf(file = filename)  # Open a PDF device
  plot(me)
  dev.off()             # Close the PDF device
  # Return the MaxEnt model
  return(me)
}

# Apply the function to each spatial object in the list
for (i in seq_along(occurrence_wi_hesp)) {
  filename <- paste0("MaxEnt_Model_Richness_Occ", names(occurrence_wi_hesp[i]), ".pdf")  # Define a unique filename for each plot
  result<-run_maxent(occurrence_wi_hesp[[i]], filename)  # Run MaxEnt and save plot as PDF
  # Check if result is NULL, which indicates that the species was skipped
  if (is.null(result)) {
    message("Species ", names(occurrence_wi_hesp)[i], " was skipped due to insufficient background points.")
  } else {
    message("MaxEnt model successfully run for species: ", names(occurrence_wi_hesp)[i])
  }
}
# ##with hypertuning
run_maxent <- function(spatial_df,filename) {
  #   # witholding a 20% sample for testing 
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=1000
  #   # Check if the number of background points is below the threshold
  #   if (nbg < threshold) {
  #     message("Skipping species due to insufficient background points: ", nbg, " points.")
  #     return(NULL)  # Skip running the MaxEnt model and return NULL
  #   }
  me<-maxent(richness_raster,occtrain,nbg=nbg)
  
  #     # Define the hyperparameters to test
  h<- list(reg = seq(0.1, 3, 0.1), fc = c("lq", "lh", "lqp", "lqph", "lqpht"))
  #   
  #   # Test all the possible combinations with gridSearch
  gs <- gridSearch(me, hypers = h, metric = "auc", test = occtest)
  head(gs@results[order(-gs@results$test_AUC), ])  # Best combinations
  
  # Use the genetic algorithm instead with optimizeModel
  om <- optimizeModel(me, hypers = h, metric = "auc", test = occtest, seed = 4)
  head(om@results)  # Best combinations
  
  #   # predict to entire dataset
  r <- predict(me,richness_raster)
  pdf(file = filename)  # Open a PDF device
  plot(r,main=paste0("Suitability of locations in WI\n for ",names(occurrence_wi_hesp[i])))
  points(spatial_df,col="black",pch=4,cex=.25)
  dev.off()             # Close the PDF device
  #   # Return the MaxEnt model
  return(r)
}
# # Apply the function to each spatial object in the list
for (i in seq_along(occurrence_wi_hesp)) {
  filename <- paste0("MaxEnt_Model_Richness_Occ", names(occurrence_wi_hesp[i]), ".pdf")  # Define a unique filename for each plot
  result<- run_maxent(occurrence_wi_hesp[[i]], filename)  # Run MaxEnt and save plot as PDF
  # # Check if result is NULL, which indicates that the species was skipped
  if (is.null(result)) {
    message("Species ", names(occurrence_wi_hesp)[i], " was skipped due to insufficient background points.")
  } else {
    message("MaxEnt model successfully run for species: ", names(occurrence_wi_hesp)[i])
  }
}
#something wrong
##non plot maxent
run_maxent <- function(spatial_df) {
  # witholding a 20% sample for testing
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=1000
  # # Check if the number of background points is below the threshold
  # if (nbg < threshold) {
  #   message("Skipping species due to insufficient background points: ", nbg, " points.")
  #   return(NULL)  # Skip running the MaxEnt model and return NULL
  # }
  me<-maxent(richness_raster,occtrain,nbg=nbg)
  return(me)
}
mes<-lapply(occurrence_wi_hesp,run_maxent) #get all me values for each species
# Initialize an empty list to store predictions--correct for nulls
rs <- list()

# Loop through each MaxEnt model in the `mes` list
for (i in seq_along(mes)) {
  # Get the MaxEnt model
  me_model <- mes[[i]]
  name <- names(mes)[i]
  # Check if the model is not NULL
  if (!is.null(me_model)) {
    # Perform prediction using the model and raster stack
    prediction <- predict(me_model, richness_raster)
    
    # Store the prediction in the `rs` list
    rs[[name]] <- prediction
  } else {
    # If the model is NULL, print a message and continue
    message("Skipping model at index ", i, " because it is NULL.")
  }
}
##remove NULLS
# Remove NULL items from the list using Filter()
rs <- Filter(Negate(is.null), rs)
#rs@data contains predictors , @file contains points
#other options not used
#r <- predict(me, summer_rast,args=c("-P", "noautofeature", "nothreshold", "noproduct", paste("maximumbackground=",nrow(summer_rast), sep=""), "noaddsamplestobackground"))
#loop di doop no plots just model testing for each
#iterate for each species in occurrence_sp
model_evaluation_list_e1<-list()
model_evaluation_list_e2<-list()
model_evaluation_list_e3<-list()
#function for evaluating models
run_maxent <- function(spatial_df) {
  # witholding a 20% sample for testing 
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=1000
  
  bg <- randomPoints(richness_raster, length(spatial_df))
  me<-maxent(richness_raster,occtrain,nbg=nbg)
  
  #simplest way to use 'evaluate'
  e1 <- evaluate(me, p=occtest, a=bg, x=richness_raster)
  return(e1) 
}

maxent_models_evaluate <- lapply(occurrence_wi_hesp, run_maxent) #list of maxents evaluated method 1

# alternative 1
run_maxent <- function(spatial_df) {
  # witholding a 20% sample for testing 
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=1000
  bg <- randomPoints(richness_raster, length(spatial_df))
  me<-maxent(richness_raster,occtrain,nbg=nbg)
  # extract values
  pvtest <- data.frame(raster::extract(richness_raster, occtest))
  avtest <- data.frame(raster::extract(richness_raster, bg))
  
  e2 <- evaluate(me, p=pvtest, a=avtest)
  
  return(e2) 
}
maxent_models_evaluate2 <- lapply(occurrence_wi_hesp, run_maxent) #list of maxents
maxent_models_evaluate2
# alternative 2 
# predict to testing points 
run_maxent <- function(spatial_df,filename) {
  # witholding a 20% sample for testing 
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=1000# Check if the number of background points is below the threshold
  bg <- randomPoints(richness_raster, length(spatial_df))
  me<-maxent(richness_raster,occtrain,nbg=nbg)
  # extract values
  pvtest <- data.frame(raster::extract(richness_raster, occtest))
  avtest <- data.frame(raster::extract(richness_raster, bg))
  testp <- predict(me, pvtest) 
  testa <- predict(me, avtest) 
  
  e3 <- evaluate(p=testp, a=testa)
  pdf(file=filename)
  plot(e3,"ROC")
  dev.off()
  return(e3)
}
# Apply the function to each spatial object in the list using lapply
for (i in seq_along(occurrence_wi_hesp)) {
  filename <- paste0("MaxEnt_Model_Richness_ROC_Occ", names(occurrence_wi_hesp[i]), ".pdf")  # Define a unique filename for each plot
  result<-run_maxent(occurrence_wi_hesp[[i]], filename)  # Run MaxEnt and save plot as PDF
  # Check if result is NULL, which indicates that the species was skipped
  if (is.null(result)) {
    message("Species ", names(occurrence_wi_hesp)[i], " was skipped due to insufficient background points.")
  } else {
    message("MaxEnt model successfully run for species: ", names(occurrence_wi_hesp)[i])
  }
}
####all the occ and obs together with variables
setwd("/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/2023-2024/SDM/SDM/Hesp_SDMS")
##rerun sampling with everything
library(purrr)
result_list_hesp_comb<-list()#prep list for all samples
# Load necessary libraries
library(sp)
library(raster)
library(dplyr)
library(purrr)
library(tidyr)
# Apply the processing for combined data
result_list_hesp_comb <- occurrence_bfly_hesp_cc_wi_names_sp %>%
  group_by(acceptedScientificName) %>%  # Group by species
  nest() %>%  # Nest data into a list-column
  mutate(sampled_values = map2(data, acceptedScientificName, function(group_data,group_name) {
    # Create SpatialPointsDataFrame for each group
    spatial_points <- SpatialPoints(coords = group_data[, c("decimalLongitude", "decimalLatitude")])
    spatial_df <- SpatialPointsDataFrame(spatial_points, data = group_data[,c("decimalLongitude", "decimalLatitude")])
    # Check if the extent is valid (i.e., min and max are not equal)
    extent_vals <- extent(spatial_df)
    if (extent_vals@xmin == extent_vals@xmax || extent_vals@ymin == extent_vals@ymax) {
      return(NULL)  # Skip this group if the extent is invalid
    }
    # Create raster with appropriate resolution and extent
    r <- raster(spatial_df)  # Define raster with extent and resolution
    res(r) <- 0.04  # Set the resolution of the raster
    r <- extend(r, extent(r) + 1)  # Extend the raster extent if necessary
    
    # Sample values from the raster at points defined by the SpatialPointsDataFrame
    acsel <- gridSample(spatial_df, r, n = 1)
    acsel <- data.frame(acsel)
    # Name the dataframe based on the group name
    names(acsel) <- paste0("sampled_values_", group_name)
    return(acsel)
  })) %>%
  dplyr::select(-data)  # Drop the nested data column
names(result_list_hesp_comb$sampled_values)<-result_list_hesp_comb$acceptedScientificName#list of acsel df for each species
# Define the new column names
new_column_names <- c("longitude", "latitude")

# Rename the columns in each dataframe
result_list_hesp_comb$sampled_values <- lapply(result_list_hesp_comb$sampled_values, function(df) {
  setNames(df, new_column_names)
})
filtered_list <-result_list_hesp_comb$sampled_values[!sapply(result_list_hesp_comb$sampled_values,is.null)] #removed families with low points
library(sf) #note need to install with gdal for proper updates
comb_wi_hesp<-sapply(filtered_list,SpatialPoints)
names(comb_wi_hesp)<-result_list_hesp_comb$acceptedScientificName #assign names of sci names to df


#iterate for each species just test out variables
run_maxent<-function(spatial_df,filename) {
#   # witholding a 20% sample for testing 
fold <- kfold(spatial_df, k=5)
occtest <- spatial_df[fold == 1, ]
occtrain <- spatial_df[fold != 1, ]
nbg=1000  # # Check if the number of background points is below the threshold
  # if (nbg <= threshold) {
  #   message("Skipping species due to insufficient background points: ", occtrain, " points.")
  #   return(NULL)  # Skip running the MaxEnt model and return NULL
  # }
  me<-maxent(richness_raster,occtrain,nbg=nbg)
  pdf(file = filename)  # Open a PDF device
  plot(me)
  dev.off()             # Close the PDF device
  # Return the MaxEnt model
  return(me)
}

# Apply the function to each spatial object in the list
for (i in seq_along(comb_wi_hesp)) {
  filename <- paste0("MaxEnt_Model_Combined_Variables", names(comb_wi_hesp[i]), ".pdf")  # Define a unique filename for each plot
  result<-run_maxent(comb_wi_hesp[[i]], filename)  # Run MaxEnt and save plot as PDF
  # Check if result is NULL, which indicates that the species was skipped
  if (is.null(result)) {
    message("Species ", names(comb_wi_hesp)[i], " was skipped due to insufficient background points.")
  } else {
    message("MaxEnt model successfully run for species: ", names(comb_wi_hesp)[i])
  }
}
# ##with hypertuning
run_maxent <- function(spatial_df,filename) {
  #   # witholding a 20% sample for testing 
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=1000
  #   # Check if the number of background points is below the threshold
  #   if (nbg < threshold) {
  #     message("Skipping species due to insufficient background points: ", nbg, " points.")
  #     return(NULL)  # Skip running the MaxEnt model and return NULL
  #   }
  me<-maxent(richness_raster,occtrain,nbg=nbg)
  
  #     # Define the hyperparameters to test
  h<- list(reg = seq(0.1, 3, 0.1), fc = c("lq", "lh", "lqp", "lqph", "lqpht"))
  #   
  # #   # Test all the possible combinations with gridSearch
  # gs <- gridSearch(me, hypers = h, metric = "auc", test = occtest)
  # head(gs@results[order(-gs@results$test_AUC), ])  # Best combinations
  # 
  # Use the genetic algorithm instead with optimizeModel
  #om <- optimizeModel(me, hypers = h, metric = "auc", test = occtest, seed = 4)
  #head(om@results)  # Best combinations
  
  #   # predict to entire dataset
  r <- predict(me,richness_raster)
  pdf(file = filename)  # Open a PDF device
  plot(r,main=paste0("Suitability of locations in WI\n for ",names(comb_wi_hesp[i])))
  points(spatial_df,col="black",pch=4,cex=.25)
  dev.off()             # Close the PDF device
  #   # Return the MaxEnt model
  return(r)
}
# # Apply the function to each spatial object in the list
for (i in seq_along(comb_wi_hesp)) {
  filename <- paste0("MaxEnt_Model_Comb_hypertuned", names(comb_wi_hesp[i]), ".pdf")  # Define a unique filename for each plot
  result<- run_maxent(comb_wi_hesp[[i]], filename)  # Run MaxEnt and save plot as PDF
  # # Check if result is NULL, which indicates that the species was skipped
  if (is.null(result)) {
    message("Species ", names(comb_wi_hesp)[i], " was skipped due to insufficient background points.")
  } else {
    message("MaxEnt model successfully run for species: ", names(comb_wi_hesp)[i])
  }
}
#something wrong
##non plot maxent
run_maxent <- function(spatial_df) {
  # witholding a 20% sample for testing
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=1000
  # # Check if the number of background points is below the threshold
  #if (nbg < threshold) {
  #   message("Skipping species due to insufficient background points: ", nbg, " points.")
  #   return(NULL)  # Skip running the MaxEnt model and return NULL
  # }
  me<-maxent(richness_raster,occtrain,nbg=nbg)
  return(me)
}
mes<-lapply(comb_wi_hesp,run_maxent) #get all me values for each species
# Save the list to a file
saveRDS(mes, file = "mes.rds")
writeRaster(plant_raster,file="plant_raster.csv")
# Initialize an empty list to store predictions--correct for nulls
rs <- list()

# Loop through each MaxEnt model in the `mes` list
for (i in seq_along(mes)) {
  # Get the MaxEnt model
  me_model <- mes[[i]]
  name <- names(mes)[i]
  # Check if the model is not NULL
  if (!is.null(me_model)) {
    # Perform prediction using the model and raster stack
    prediction <- predict(me_model, richness_raster)
    
    # Store the prediction in the `rs` list
    rs[[name]] <- prediction
  } else {
    # If the model is NULL, print a message and continue
    message("Skipping model at index ", i, " because it is NULL.")
  }
}
##remove NULLS
# Remove NULL items from the list using Filter()
rs <- Filter(Negate(is.null), rs)
#rs@data contains predictors , @file contains points
#other options not used
#r <- predict(me, summer_rast,args=c("-P", "noautofeature", "nothreshold", "noproduct", paste("maximumbackground=",nrow(summer_rast), sep=""), "noaddsamplestobackground"))
#loop di doop no plots just model testing for each
#iterate for each species in occurrence_sp
model_evaluation_list_e1<-list()
model_evaluation_list_e2<-list()
model_evaluation_list_e3<-list()
#function for evaluating models
run_maxent <- function(spatial_df) {
  # witholding a 20% sample for testing 
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=1000
  
  bg <- randomPoints(richness_raster, length(spatial_df))
  me<-maxent(richness_raster,occtrain,nbg=nbg)
  
  #simplest way to use 'evaluate'
  e1 <- evaluate(me, p=occtest, a=bg, x=richness_raster)
  return(e1) 
}

maxent_models_evaluate <- lapply(comb_wi_hesp, run_maxent) #list of maxents evaluated method 1

# alternative 1
run_maxent <- function(spatial_df) {
  # witholding a 20% sample for testing 
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=1000
  bg <- randomPoints(richness_raster, length(spatial_df))
  me<-maxent(richness_raster,occtrain,nbg=nbg)
  # extract values
  pvtest <- data.frame(raster::extract(richness_raster, occtest))
  avtest <- data.frame(raster::extract(richness_raster, bg))
  
  e2 <- evaluate(me, p=pvtest, a=avtest)
  
  return(e2) 
}
maxent_models_evaluate2 <- lapply(comb_wi_hesp, run_maxent) #list of maxents
maxent_models_evaluate2
# alternative 2 
# predict to testing points 
run_maxent <- function(spatial_df,filename) {
  # witholding a 20% sample for testing 
  fold <- kfold(spatial_df, k=5)
  occtest <- spatial_df[fold == 1, ]
  occtrain <- spatial_df[fold != 1, ]
  nbg=1000# Check if the number of background points is below the threshold
  bg <- randomPoints(richness_raster, length(spatial_df))
  me<-maxent(richness_raster,occtrain,nbg=nbg)
  # extract values
  pvtest <- data.frame(raster::extract(richness_raster, occtest))
  avtest <- data.frame(raster::extract(richness_raster, bg))
  testp <- predict(me, pvtest) 
  testa <- predict(me, avtest) 
  
  e3 <- evaluate(p=testp, a=testa)
  pdf(file=filename)
  plot(e3,"ROC")
  dev.off()
  return(e3)
}
# Apply the function to each spatial object in the list using lapply
for (i in seq_along(comb_wi_hesp)) {
  filename <- paste0("MaxEnt_Model_comb_ROC", names(comb_wi_hesp[i]), ".pdf")  # Define a unique filename for each plot
  result<-run_maxent(comb_wi_hesp[[i]], filename)  # Run MaxEnt and save plot as PDF
  # Check if result is NULL, which indicates that the species was skipped
  if (is.null(result)) {
    message("Species ", names(comb_wi_hesp)[i], " was skipped due to insufficient background points.")
  } else {
    message("MaxEnt model successfully run for species: ", names(comb_wi_hesp)[i])
  }
}
library(SSDM) #bfly stacky stack
#convert occ_sp data into dataframe
data_list_bf<-lapply(result_list_hesp_obs$sampled_values,function(df){
  data.frame(x=df[,1],y=df[,2])
})
names(data_list_bf)<-result_list_hesp_obs$acceptedScientificName
merge_and_add_names <- function(list_of_dataframes) {
  names_list <- names(data_list_bf)
  merged_df <- bind_rows(data_list_bf, .id = "species")
  return(merged_df)
}
occ_bf_obs_df<-merge_and_add_names(data_list_bf)
SSDM_bf_obs <- SSDM::stack_modelling("MAXENT", occ_bf_obs_df, richness_raster, rep = 1, 
                                      Xcol = 'x', Ycol = 'y',
                                      Spcol = 'species',method = "pSSDM",verbose = TRUE,cores=7)####cores cannot be auto calculated or 0 apparently
##results
plot(SSDM_bf_obs@diversity.map, main = 'Estimated Richness of Butterflies')
plot(CanUS.proj,xlim = c(-92.9,-86.8),ylim= c(42.5, 47.3),
     add = TRUE)
richness_df <- as.data.frame(rasterToPoints(SSDM_bf_obs@diversity.map), xy = TRUE)
# Create the plot
c<-ggplot() +
  geom_raster(data = richness_df, aes(x = x, y = y, fill = diversity)) +
  scale_fill_gradientn(colors=rev(terrain.colors(100))) +
  #geom_raster(data = WI_raster, aes(x = long, y = lat, group = group), color = "black", fill = NA)+
  geom_point(data = occ_bf_obs_df, aes(x = x, y = y), color = "black", size = 0.5) +
  coord_fixed() +
  theme_minimal() +
  labs(title = "SSDM Richness with Combined Data",
       fill = "Richness")+ xlab("longitude")+ylab("latitude") 

#evaluation of SSDM
knitr::kable(SSDM_bf_obs@evaluation)
#importance analysis of env variables
knitr::kable(SSDM_bf_obs@variable.importance$diversity)#diversity over env

###stack with other raster
both_richness_raster<-stack(richness_raster,SSDM_bf_obs@diversity.map)
###correlation??
values_plants <- getValues(both_richness_raster$diversity.1)
values_butterfly <- getValues(both_richness_raster$diversity.2)
regression<-data.frame(values_plants,values_butterfly)
lm_model <- lm(values_butterfly ~ values_plants, data = regression)
lm_model
# Get R-squared value
r_squared <- summary(lm_model)$r.squared
r_squared
r_squared_label <- sprintf("R² = %.2f", r_squared)
library(ggplot2)
d<-ggplot(regression, aes(x = values_plants, y = values_butterfly)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Plant vs Butterfly Richness-Combined", x = "Plant Richness", y = "Butterfly Richness") +
  geom_text(x=60,y=1,label = r_squared_label, hjust = 1, vjust = -1)
d
grid.arrange(c,d,ncol=2)
##rerun with dataset split
library(SSDM) #bfly stacky stack
#convert occ_sp data into dataframe
data_list_bf<-lapply(result_list_hesp_all$sampled_values,function(df){
  data.frame(x=df[,1],y=df[,2])
})
names(data_list_bf)<-result_list_hesp_all$acceptedScientificName
merge_and_add_names <- function(list_of_dataframes) {
  names_list <- names(data_list_bf)
  merged_df <- bind_rows(data_list_bf, .id = "species")
  return(merged_df)
}
occ_bf_df<-merge_and_add_names(data_list_bf)
##split by basis of record
occ_bf_specimen<-filter(result_list_hesp_all,basisOfRecord=="PRESERVED_SPECIMEN")
occ_bf_observation<-filter(result_list_hesp_all,basisOfRecord=="HUMAN_OBSERVATION")
data_list_bf<-lapply(occ_bf_specimen$sampled_values,function(df){
  data.frame(x=df[,1],y=df[,2])
})
names(data_list_bf)<-occ_bf_specimen$acceptedScientificName
merge_and_add_names <- function(list_of_dataframes) {
  names_list <- names(data_list_bf)
  merged_df <- bind_rows(data_list_bf, .id = "species")
  return(merged_df)
}
occ_bf_specimen<-merge_and_add_names(data_list_bf)
#observation
data_list_bf<-lapply(occ_bf_observation$sampled_values,function(df){
  data.frame(x=df[,1],y=df[,2])
})
names(data_list_bf)<-occ_bf_observation$acceptedScientificName
merge_and_add_names <- function(list_of_dataframes) {
  names_list <- names(data_list_bf)
  merged_df <- bind_rows(data_list_bf, .id = "species")
  return(merged_df)
}
occ_bf_observation<-merge_and_add_names(data_list_bf)

#we did it Joe
#test individual SDM
SDM <- modelling('MAXENT', subset(occ_bf_df, occ_bf_df$species == "Pieris rapae"), 
                 richness_raster, Xcol = 'x', Ycol = 'y', verbose = TRUE)
plot(SDM@projection, main = 'SDM\nfor Pieris rapae \nwith MAXENT algorithm')
#stacked modelling
SSDM_bf_spec_bg <- SSDM::stack_modelling("MAXENT", occ_bf_specimen, richness_raster, rep = 1, maxent.args=list(nbg=1000),
                              Xcol = 'x', Ycol = 'y',
                              Spcol = 'species',method = "pSSDM",verbose = TRUE,cores=8)####cores cannot be auto calculated or 0 apparently
SSDM_bf_obs <- SSDM::stack_modelling("MAXENT", occ_bf_observation, richness_raster, rep = 1, 
                                      Xcol = 'x', Ycol = 'y',
                                      Spcol = 'species',method = "pSSDM",verbose = TRUE,cores=8)####cores cannot be auto calculated or 0 apparently

saveRDS(SSDM_bf,"SSDM_bf_richness.rds")
SSDM_bf<-readRDS("/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/2023-2024/SDM/SDM/Hesp_SDMS/SSDM_bf_richness.rds")
plot(SSDM_bf_spec@diversity.map, main = 'Estimated Richness of Butterflies')
plot(CanUS.proj,xlim = c(-92.9,-86.8),ylim= c(42.5, 47.3),
     add = TRUE)
richness_df <- as.data.frame(rasterToPoints(SSDM_bf_obs@diversity.map), xy = TRUE)
# Create the plot
ggplot() +
  geom_raster(data = richness_df, aes(x = x, y = y, fill = diversity)) +
  scale_fill_gradientn(colors=rev(terrain.colors(100))) +
  #geom_raster(data = WI_raster, aes(x = long, y = lat, group = group), color = "black", fill = NA)+
  geom_point(data = occ_bf_observation, aes(x = x, y = y), color = "black", size = 0.5) +
  coord_fixed() +
  theme_minimal() +
  labs(title = "SSDM Richness with Observation Data",
       fill = "Richness")+ xlab("longitude")+ylab("latitude") 

#evaluation of SSDM
knitr::kable(SSDM_bf_spec@evaluation)
#evaluation of SSDM
knitr::kable(SSDM_bf_obs@evaluation)

#importance analysis of env variables
knitr::kable(SSDM_bf_spec@variable.importance)#diversity over env
knitr::kable(SSDM_bf_obs@variable.importance)#diversity over env

###stack with other raster
both_richness_raster<-stack(richness_raster,SSDM_bf_obs@diversity.map)
###correlation??
values_plants <- getValues(both_richness_raster$diversity.1)
values_butterfly <- getValues(both_richness_raster$diversity.2)
regression<-data.frame(values_plants,values_butterfly)
lm_model <- lm(values_butterfly ~ values_plants, data = regression)
lm_model
# Get R-squared value
r_squared <- summary(lm_model)$r.squared
r_squared
r_squared_label <- sprintf("R² = %.2f", r_squared)
library(ggplot2)
b<-ggplot(regression, aes(x = values_plants, y = values_butterfly)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Plant vs Butterfly Richness-Observation", x = "Plant Richness", y = "Butterfly Richness") +
  geom_text(x=60,y=1,label = r_squared_label, hjust = 1, vjust = -1)
###stack with other data
both_richness_raster<-stack(richness_raster,SSDM_bf_spec@diversity.map)
###correlation??
values_plants <- getValues(both_richness_raster$diversity.1)
values_butterfly <- getValues(both_richness_raster$diversity.2)
regression<-data.frame(values_plants,values_butterfly)
lm_model <- lm(values_butterfly ~ values_plants, data = regression)
lm_model
# Get R-squared value
r_squared <- summary(lm_model)$r.squared
r_squared
r_squared_label <- sprintf("R² = %.2f", r_squared)
library(ggplot2)
a<-ggplot(regression, aes(x = values_plants, y = values_butterfly)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Plant vs Butterfly Richness-Specimen", x = "Plant Richness", y = "Butterfly Richness") +
  geom_text(x=60,y=1,label = r_squared_label, hjust = 1, vjust = -1)
##combine plots
library(gridExtra)
# Arrange the plots in one figure
grid.arrange(a, b, ncol = 2)  # Adjust 'ncol' or 'nrow' as needed
##rerun with comb dataset

head(pollard)
pollard$catalogNumber <- paste("P", seq_len(nrow(pollard)), sep = "")
new_col_names<-c()
colnames(pollard)[9]<-"decimalLongitude"

pollard_trim_hesp$latitude<-as.numeric(pollard_trim_hesp$latitude)
pollard_trim_hesp$longitude<-as.numeric(pollard_trim_hesp$longitude)

pollard_trim_hesp<-pollard_trim_hesp%>%
  drop_na(latitude) %>% drop_na(longitude)
  
  cc_cen(buffer = 2000) %>% # remove country centroids within 2km 
  cc_cap(buffer = 2000) %>% # remove capitals centroids within 2km
  cc_inst(buffer = 2000) %>% # remove zoo and herbaria within 2km 
  cc_sea() %>% # remove from ocean 
  distinct(decimalLongitude,decimalLatitude,Taxon,Site,Section, .keep_all = TRUE) %>%
  glimpse() # look at results of pipeline
pollard_wi<-pollard_trim%>%drop_na(Taxon) %>% count(Taxon,sort=TRUE)#get counts of each species *note not actual abundance but number of unique events for each species
pollard_wi #111 species--match the pollard backbone
#filter to non singletons
species<-filter(pollard_wi,n>11)#eliminate more low numbers
pollard_trim<-filter(pollard_trim,Taxon %in% species$Taxon)
pollard_trim<-filter(pollard_trim,!Taxon %in% c("Speyeria sp.","Coliadinae sp.","Hesperiinae sp."))
#cleaning

pollard_trim <- dplyr::rename(pollard_trim, latitude = decimalLatitude, 
                              longitude = decimalLongitude)
library(leaflet)
library(tidyr)
#grid sampling
library(dismo)
library(raster)
library(tidyr)
lat_between <- pollard_trim_hesp$latitude[between(pollard_trim_hesp$latitude, 42.5, 47.3)]
lon_between <- pollard_trim_hesp$longitude[between(pollard_trim_hesp$longitude, -92.9, -86.8)]
# Specify the bounding box for Wisconsin
wisconsin_bounds <- c(left = -92.9, bottom = 42.5, right = -86.8, top = 47.3)
length(pollard_trim_hesp$latitude) - length(lat_between)
length(pollard_trim_hesp$longitude) - length(lon_between)
#exclude the lat and long values outside those boundaries
pollard_trim_hesp$long_out <- pollard_trim_hesp$longitude %in% lon_between
pollard_trim_hesp$lat_out <- pollard_trim_hesp$latitude %in% lat_between
pollard_trim_hesp <- filter(pollard_trim_hesp, long_out == "TRUE") %>% filter(lat_out == "TRUE") #eliminated out of bounds points,,only took out a few
library(purrr)
result_list_pollard<-list()#prep list
result_list_pollard <- pollard_trim_hesp %>%
  group_by(scientificName) %>%
  nest() %>%
  mutate(sampled_values = map2(data, scientificName, function(group_data,group_name) {
    # Create SpatialPointsDataFrame for each group
    spatial_points <- SpatialPoints(coords = group_data[, c("longitude", "latitude")])
    spatial_df <- SpatialPointsDataFrame(spatial_points, data = group_data[, c("longitude", "latitude")])
    
    return(spatial_df)
  })) 
library(sf)

occurrence_sp_pd<-sapply(result_list_pollard$sampled_values,SpatialPoints)
names(occurrence_sp_pd)<-result_list_pollard$scientificName #assign names of sci names to df
# Assuming 'raster_list' contains raster layers and 'spatial_list' contains spatial objects

# Use map2 to perform extract operation on each pair of corresponding elements
extracted_values_pd_list <- map(occurrence_sp_pd, function(spatial_object) {
  extracted_values <- raster::extract(richness_raster, spatial_object, method = "simple", sp = TRUE)
  return(extracted_values)
})

# 'extracted_values_list' will contain the results of extract operation for each pair of corresponding elements
#extracted_values <- raster::extract(summer_rast,occurrence_sp,method="simple",sp=TRUE)
raster_df <- as.data.frame(rasterToPoints(richness_raster))
TP_temp<-result_list_bfly$sampled_values[1]
names(TP_temp)<-"Thorybes pylades"
names(TP_temp$`Thorybes pylades`)<-c("longitude","latitude")
TP_temp<-data.frame(TP_temp$`Thorybes pylades`)
###single plot w ppt
ggplot() +
  geom_raster(data = raster_df, aes(x = x, y = y, fill = PRISM_ppt_provisional_4kmM3_202308_bil)) +geom_point(data = TP_temp, aes(x =longitude, y = latitude), size = 0.5) +
  scale_fill_gradient(low = "blue", high = "red", name = "Precipitation Aug 2023") +
  labs(x = "Longitude", y = "Latitude", fill = "Precipitation (mm) Aug 2023")  + ggtitle("Thorybes pylades") +
  theme_minimal()
###with temp
###single plot w temp
ggplot() +
  geom_raster(data = raster_df, aes(x = x, y = y, fill = PRISM_tmean_provisional_4kmM3_202308_bil)) +geom_point(data = TP_temp, aes(x =longitude, y = latitude), size = 0.5) +
  scale_fill_gradient(low = "blue", high = "red", name = "Mean Temperature Aug 2023") +
  labs(x = "Longitude", y = "Latitude", fill = "Mean Temp C Aug 2023")  + ggtitle("Thorybes pylades") +
  theme_minimal()
##richness
ggplot() +
  geom_raster(data = raster_df, aes(x = x, y = y, fill = diversity)) +geom_point(data = TP_temp, aes(x =longitude, y = latitude), size = 0.5) +
  scale_fill_gradient(low = "blue", high = "red", name = "Plant Species Diversity") +
  labs(x = "Longitude", y = "Latitude", fill = "Plant Species Diversity")  + ggtitle("Thorybes pylades") +
  theme_minimal()

###maxent
library(dismo)
library(rJava)
library(geodata)
system.file("java", package="dismo") #moved maxent.jar to package in r
setwd("/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/2023-2024/SDM/SDM")
#iterate for each species in occurrence_sp, just test out variables
run_maxent <- function(spatial_df,filename) {
  # witholding a 20% sample for testing 
  fold <- kfold(occ_bf_specimen$`Thorybes pylades`, k=5)
  occtest <- occurrence_sp_bf$`Thorybes pylades`[fold == 1, ]
  occtrain <- occurrence_sp_bf$`Thorybes pylades`[fold != 1, ]
  me<-maxent(richness_raster,occtrain,nbg=100) ##small bckrd pts, ignore for now
  pdf(file = filename)  # Open a PDF device
  plot(me)
  dev.off()             # Close the PDF device
  # Return the MaxEnt model
  return(me)
}
# Apply the function to each spatial object in the list
#for (i in seq_along(occurrence_sp)) {
#filename <- paste0("MaxEnt_Model_Variables", names(occurrence_sp[i]), ".pdf")  # Define a unique filename for each plot
#run_maxent(occurrence_sp[[i]], filename)  # Run MaxEnt and save plot as PDF
}
#predict
#run_maxent <- function(spatial_df,filename) {
# witholding a 20% sample for testing 
fold <- kfold(spatial_df, k=5)
occtest <- spatial_df[fold == 1, ]
occtrain <- spatial_df[fold != 1, ]
me<-maxent(subset_raster,occtrain,nbg=length(occtrain))
r <- predict(mes$`Epargyreus clarus (Cramer, 1775)`,richness_raster) 
###state borders
us <- gadm(country = "USA", level = 1, resolution = 1,path="~/")
canada <- gadm(country = "CAN", level = 1, resolution = 1,
               path = "~/")
CRS<- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
CanUS <- rbind(us, canada)  
CanUS.proj <- project(CanUS, CRS)

pdf(file = "Predict_SSS.pdf")  # Open a PDF device
plot_usmap(include="WI")+
  plot(r,main=paste0("Suitability of locations in the US\n for Epargyreus clarus"))
points(occ_bf_specimen$`Thorybes pylades`,col="black",pch=4,cex=.25)
plot(CanUS.proj,xlim = c(-92.9,-86.8),ylim= c(42.5, 47.3),
     add = TRUE)
dev.off()             # Close the PDF device
library(SSDM) #bfly stacky stack
#convert pd data into dataframe
data_list_pd<-lapply(result_list_pollard$sampled_values,function(df){
  data.frame(x=df[,1],y=df[,2])
})
names(data_list_pd)<-result_list_pollard$scientificName
merge_and_add_names <- function(list_of_dataframes) {
  names_list <- names(data_list_pd)
  merged_df <- bind_rows(data_list_pd, .id = "species")
  return(merged_df)
}
pd_bf<-merge_and_add_names(data_list_pd)
write.csv(pd_bf,"/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/2023-2024/SDM/SDM/pd.stack.csv")
#we did it Joe
#test individual SDM
SDM <- modelling('all', occ_bf, 
                 richness_raster, Xcol = 'x', Ycol = 'y', verbose = TRUE)
plot(SDM@projection, main = 'SDM\nfor Pieris rapae \nwith MAXENT algorithm')
#stacked modelling--should incorporate absence data somehow
SSDM_hesp_pd <- SSDM::stack_modelling("all", occ_bf, richness_raster, rep = 1, 
                              Xcol = 'x', Ycol = 'y',
                              Spcol = 'species',method = "pSSDM",verbose = TRUE,cores=16)####cores cannot be auto calculated or 0 apparently
SSDM_hesp_occ<-readRDS("/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/2023-2024/SDM/SDM/Hesp_SDMS/SSDM_hesp_occ.rds")
plot(SSDM_hesp_occ@diversity.map, main = 'SSDM\nfor Hesperiidae occurrence records')
richness_df <- as.data.frame(rasterToPoints(SSDM_hesp_occ@diversity.map), xy = TRUE)
# Create the plot
ggplot() +
  geom_raster(data = richness_df, aes(x = x, y = y, fill = diversity)) +
  scale_fill_gradientn(colors=rev(terrain.colors(100))) +
  #geom_raster(data = WI_raster, aes(x = long, y = lat, group = group), color = "black", fill = NA)+
  geom_point(data = occ_bf, aes(x = x, y = y), color = "black", size = 0.5) +
  coord_fixed() +
  theme_minimal() +
  labs(title = "SSDM Richness with Occurrence Data",
       fill = "Richness")+ xlab("longitude")+ylab("latitude") 

#evaluation of SSDM
knitr::kable(SSDM_hesp_occ@evaluation)
#importance analysis of env variables
knitr::kable(SSDM_hesp_occ@variable.importance)
###stack with other raster
richness_raster<-brick("/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/2023-2024/SDM/SDM/Hesp_SDMS/richness_raster.gri")
both_richness_raster<-stack(richness_raster,SSDM_hesp_occ@diversity.map)
###correlation??
values_plants <- getValues(both_richness_raster$diversity.1)
values_butterfly <- getValues(both_richness_raster$diversity.2)
regression<-data.frame(values_plants,values_butterfly)
lm_model <- lm(values_butterfly ~ values_plants, data = regression)
lm_model
# Get R-squared value
r_squared <- summary(lm_model)$r.squared
r_squared_label <- sprintf("R² = %.2f", r_squared)
r_squared
library(ggplot2)
ggplot(regression, aes(x = values_plants, y = values_butterfly)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter Plot of Plant vs Butterfly Richness with Occurrence Records", x = "Plant Richness", y = "Butterfly Richness") +
  annotate("text", label = r_squared_label, hjust = 1, vjust = -1)

#convert obs_sp data into dataframe
data_list_obs<-lapply(result_list_hesp_obs$sampled_values,function(df){
  data.frame(x=df[,1],y=df[,2])
})
names(data_list_ocbs)<-result_list_hesp_obs$acceptedScientificName
merge_and_add_names <- function(list_of_dataframes) {
  names_list <- names(data_list_obs)
  merged_df <- bind_rows(data_list_obs, .id = "species")
  return(merged_df)
}
obs_bf<-merge_and_add_names(data_list_obs)
write.csv(obs_bf,"/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/2023-2024/SDM/SDM/obs.stack.csv")
#we did it Joe
#test individual SDM
SDM <- modelling('all', occ_bf, 
                 richness_raster, Xcol = 'x', Ycol = 'y', verbose = TRUE)
plot(SDM@projection, main = 'SDM\nfor Pieris rapae \nwith MAXENT algorithm')
#stacked modelling--should incorporate absence data somehow
SSDM_hesp_obs <- SSDM::stack_modelling("all", obs_bf, richness_raster, rep = 1, 
                                       Xcol = 'x', Ycol = 'y',
                                       Spcol = 'species',method = "pSSDM",verbose = TRUE,cores=4)####cores cannot be auto calculated or 0 apparently
SSDM_hesp_obs<-readRDS("/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/2023-2024/SDM/SDM/Hesp_SDMS/SSDM_hesp_obs.rds")
plot(SSDM_hesp_obs@diversity.map, main = 'SSDM\nfor all butterflies\nwith MAXENT')
#evaluation of SSDM
richness_df <- as.data.frame(rasterToPoints(SSDM_hesp_obs@diversity.map), xy = TRUE)
# Create the plot
ggplot() +
  geom_raster(data = richness_df, aes(x = x, y = y, fill = diversity)) +
  scale_fill_gradientn(colors=rev(terrain.colors(100))) +
  #geom_raster(data = WI_raster, aes(x = long, y = lat, group = group), color = "black", fill = NA)+
  geom_point(data = occ_bf, aes(x = x, y = y), color = "black", size = 0.5) +
  coord_fixed() +
  theme_minimal() +
  labs(title = "SSDM Richness with Observation Data",
       fill = "Richness")+ xlab("longitude")+ylab("latitude") 

knitr::kable(SSDM_hesp_obs@evaluation)
#importance analysis of env variables
knitr::kable(SSDM_hesp_obs@variable.importance)
###stack with other raster
both_richness_raster<-stack(richness_raster,SSDM_hesp_obs@diversity.map)
###correlation??
values_plants <- getValues(both_richness_raster$diversity.1)
values_butterfly <- getValues(both_richness_raster$diversity.2)
regression<-data.frame(values_plants,values_butterfly)
lm_model <- lm(values_butterfly ~ values_plants, data = regression)
lm_model
# Get R-squared value
r_squared <- summary(lm_model)$r.squared
r_squared_label <- sprintf("R² = %.2f", r_squared)
r_squared
library(ggplot2)
ggplot(regression, aes(x = values_plants, y = values_butterfly)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter Plot of Plant vs Butterfly Richness with Observation Records", x = "Plant Richness", y = "Butterfly Richness") +
  annotate("text", label = r_squared_label, hjust = 1, vjust = -1)



###filter to southeastern WI
lat_between <- pollard_trim$latitude[between(pollard_trim$latitude, 42.5, 43.2)]
lon_between <- pollard_trim$longitude[between(pollard_trim$longitude, -89.0, -86.8)]
# Specify the bounding box for Wisconsin
wisconsin_bounds <- c(left = -92.9, bottom = 42.5, right = -86.8, top = 47.3)
length(pollard_trim$latitude) - length(lat_between)
length(pollard_trim$longitude) - length(lon_between)
#exclude the lat and long values outside those boundaries
pollard_trim$long_out <- pollard_trim$longitude %in% lon_between
pollard_trim$lat_out <- pollard_trim$latitude %in% lat_between
pollard_trim <- filter(pollard_trim, long_out == "TRUE") %>% filter(lat_out == "TRUE") #eliminated out of bounds points,,only took out a few
library(purrr)
result_list_pollard<-list()#prep list
result_list_pollard <- pollard_trim %>%
  group_by(Taxon) %>%
  nest() %>%
  mutate(sampled_values = map2(data, Taxon, function(group_data,group_name) {
    # Create SpatialPointsDataFrame for each group
    spatial_points <- SpatialPoints(coords = group_data[, c("longitude", "latitude")])
    spatial_df <- SpatialPointsDataFrame(spatial_points, data = group_data[, c("longitude", "latitude")])
    
    # Convert SpatialPointsDataFrame to raster
    r <- raster(spatial_df)
    res(r) <- 0.04  # Set the resolution of the raster
    
    # Extend the extent of the raster
    r <- extend(r, extent(r) + 1)
    
    # Sample values from the raster at points defined by the SpatialPointsDataFrame
    acsel <- gridSample(spatial_df, r, n = 1)
    acsel <- data.frame(acsel)
    # Name the dataframe based on the group name
    names(acsel) <- paste0("sampled_values_", group_name)
    return(acsel)
  })) %>%
  select(-data)
library(sf)
occurrence_sp_pd<-sapply(result_list_pollard$sampled_values,SpatialPoints)
names(occurrence_sp_pd)<-result_list_pollard$Taxon #assign names of sci names to df
# Assuming 'raster_list' contains raster layers and 'spatial_list' contains spatial objects

# Use map2 to perform extract operation on each pair of corresponding elements
bbox_sewisconsin <- extent(-89.0, -86.8, 42.5, 43.2)
se_raster <- crop(richness_raster, bbox_sewisconsin)

extracted_values_pd_list <- map(occurrence_sp_pd, function(spatial_object) {
  extracted_values <- raster::extract(se_raster, spatial_object, method = "simple", sp = TRUE)
  return(extracted_values)
})
#stacked modelling--should incorporate absence data somehow
SSDM_se <- SSDM::stack_modelling("MAXENT", pd_bf, se_raster, rep = 1, 
                                 Xcol = 'x', Ycol = 'y',
                                 Spcol = 'species',method = "pSSDM",verbose = TRUE,cores=4)####cores cannot be auto calculated or 0 apparently
plot(SSDM_se@diversity.map, main = 'SSDM\nfor all butterflies\nwith MAXENT')
#evaluation of SSDM
knitr::kable(SSDM_se@evaluation)
#importance analysis of env variables
knitr::kable(SSDM_se@variable.importance)
###stack with other raster
both_richness_raster<-stack(se_raster,SSDM_se@diversity.map)
###correlation??
values_plants <- getValues(both_richness_raster$diversity.1)
values_butterfly <- getValues(both_richness_raster$diversity.2)
regression<-data.frame(values_plants,values_butterfly)
lm_model <- lm(values_butterfly ~ values_plants, data = regression)
lm_model
# Get R-squared value
r_squared <- summary(lm_model)$r.squared
r_squared_label <- sprintf("R² = %.2f", r_squared)
r_squared
library(ggplot2)
ggplot(regression, aes(x = values_plants, y = values_butterfly)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter Plot of Plant vs Butterfly Richness with Pollard walks only", x = "Plant Richness", y = "Butterfly Richness") +
  annotate("text", label = r_squared_label, hjust = 1, vjust = -1)
###GLM--need binary matrix for P/A data of Pollard walks
extract_danaus<-raster::extract(richness_raster,occurrence_sp_pd$`Danaus plexippus`,method="simple",sp="TRUE")
danaus_df<-data.frame(extract_danaus)
danaus_df$optional<-1
colnames(danaus_df)[12]<-"Presence"
summary(danaus_df)
dp_glm <- glm( Presence ~ bio_2 + I(bio_2^2) + bio_5 + I(bio_5^2) + bio_14 + I(bio_14^2), family='binomial', data=avi_df)

summary(m_glm)

##what plants best predict butterfly distribution?
head(rs)#all plant maxent models
# witholding a 20% sample for testing 
fold <- kfold(occurrence_sp_bf$`Colias eurytheme`, k=5)
occtest <- occurrence_sp_bf$`Colias eurytheme`[fold == 1, ]
occtrain <- occurrence_sp_bf$`Colias eurytheme`[fold != 1, ]
me<-maxent(rs_brick,occtrain,nbg=1000)
bg <- randomPoints(rs_brick, length(occurrence_sp_bf$`Colias eurytheme`))
#simplest way to use 'evaluate'
e1 <- evaluate(me, p=occtest, a=bg, x=rs_brick)
e1
# predict to entire dataset
r <- predict(me,rs_brick)
#testing--adjust for loops to evaluate

pvtest <- data.frame(raster::extract(rs_brick, occtest))
avtest <- data.frame(raster::extract(rs_brick, bg))

e2 <- evaluate(me, p=pvtest, a=avtest)
e2
# alternative 2 
# predict to testing points 
testp <- predict(me, pvtest) 
testa <- predict(me, avtest) 

e3 <- evaluate(p=testp, a=testa)
plot(e3,"ROC")
plot(me,cex=0.5)
##cool
#time--pre 1980
#look at downloaded files in directory
temp<-prism_archive_subset("tmean", "monthly", years=1979,mon=6:8)
ppt<-prism_archive_subset("ppt", "monthly", years=1979,mon=6:8)
ppt<-pd_to_file(ppt)
temp<-pd_to_file(temp)
temp_stack<-raster::stack(temp)
ppt_stack<-raster::stack(ppt)
old_rast<-raster::stack(temp_stack,ppt_stack)
bbox_wisconsin <- extent(-92.9, -86.8, 42.5, 47.3)
old_rast <- crop(old_rast, bbox_wisconsin)
plot(old_rast)
#plants
xe_pre<-filter(xe,year<1980)
library(purrr)
result_list_pre<-list()#prep list
result_list_pre <- xe_pre %>%
  group_by(scientificName) %>%
  nest() %>%
  mutate(sampled_values = map2(data, scientificName, function(group_data,group_name) {
    # Create SpatialPointsDataFrame for each group
    spatial_points <- SpatialPoints(coords = group_data[, c("longitude", "latitude")])
    spatial_df <- SpatialPointsDataFrame(spatial_points, data = group_data[, c("longitude", "latitude")])
    
    # Convert SpatialPointsDataFrame to raster
    r <- raster(spatial_df)
    res(r) <- 0.04  # Set the resolution of the raster
    
    # Extend the extent of the raster
    r <- extend(r, extent(r) + 1)
    
    # Sample values from the raster at points defined by the SpatialPointsDataFrame
    acsel <- gridSample(spatial_df, r, n = 1)
    acsel <- data.frame(acsel)
    # Name the dataframe based on the group name
    names(acsel) <- paste0("sampled_values_", group_name)
    return(acsel)
  })) %>%
  dplyr::select(-data)

##########combine climate and species
names(result_list_pre$sampled_values)<-result_list_pre$scientificName#list of acsel df for each species
library(sf)
occurrence_sp_pre<-sapply(result_list_pre$sampled_values,SpatialPoints)
names(occurrence_sp_pre)<-result_list_pre$scientificName #assign names of sci names to df
# Assuming 'raster_list' contains raster layers and 'spatial_list' contains spatial objects

# Use map2 to perform extract operation on each pair of corresponding elements
extracted_values_list <- map(occurrence_sp_pre, function(spatial_object) {
  extracted_values <- raster::extract(old_rast, spatial_object, method = "simple", sp = TRUE)
  return(extracted_values)
})
library(SSDM)
#convert occ_sp data into dataframe
data_list_pre<-lapply(result_list_pre$sampled_values,function(df){
  data.frame(x=df[,1],y=df[,2])
})
names(data_list_pre)<-result_list_pre$scientificName
merge_and_add_names <- function(list_of_dataframes) {
  names_list_pre <- names(data_list_pre)
  merged_df <- bind_rows(data_list_pre, .id = "species")
  return(merged_df)
}
occ_df_pre<-merge_and_add_names(data_list_pre)
#we did it Joe
#test individual SDM
#stacked modelling
SSDM <- SSDM::stack_modelling("MAXENT", occ_df_pre, old_rast, rep = 1, 
                              Xcol = 'x', Ycol = 'y',
                              Spcol = 'species',method = "pSSDM",verbose = TRUE,cores=4)####cores cannot be auto calculated or 0 apparently
plot(SSDM@diversity.map, main = 'SSDM\nfor all plants\nwith MAXENT pre 1980')
#evaluation of SSDM
knitr::kable(SSDM@evaluation)
#importance analysis of env variables
knitr::kable(SSDM@variable.importance)

##################################################################
#SDM paper figures
#sconsin
library(ggplot2)
library(maps)
library(dplyr)

# Get map data
states_map <- map_data("state")

# Plot
ggplot(states_map, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = region == "wisconsin"), color = "black") +
  scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "white"), guide = "none") +
  coord_fixed(1.3) +
  theme_minimal() 
  
###Fig 2 plants
# Create the plot
# Extract diversity values from all maps
v1 <- values(SSDM_plant_all@diversity.map)
v2 <- values(SSDM_plant_early@diversity.map)
v3 <- values(SSDM_plant_late@diversity.map)  # assuming this is the 1990–2023 raster
# Calculate global min and max
global_min <- min(c(v1, v2, v3), na.rm = TRUE)
global_max <- max(c(v1, v2, v3), na.rm = TRUE)

richness_df_all <- as.data.frame(rasterToPoints(SSDM_plant_all@diversity.map), xy = TRUE)

p1<-ggplot() +
  geom_raster(data = richness_df_all, aes(x = x, y = y, fill = diversity)) +
  scale_fill_gradientn(colors=rev(terrain.colors(100)),limits = c(global_min, global_max), na.value = NA)  +
  coord_fixed() +
  theme_void() +
  labs(title = "1960-2023",
       fill = "Richness")
richness_df_early <- as.data.frame(rasterToPoints(SSDM_plant_early@diversity.map), xy = TRUE)

p2<-ggplot() +
  geom_raster(data = richness_df_early, aes(x = x, y = y, fill = diversity)) +
  scale_fill_gradientn(colors=rev(terrain.colors(100)),limits = c(global_min, global_max), na.value = NA) +
  coord_fixed() +
  theme_void() +
  labs(title = "1960-1989",
       fill = "Richness")
richness_df_late <- as.data.frame(rasterToPoints(SSDM_plant_late@diversity.map), xy = TRUE)

p3<-ggplot() +
  geom_raster(data = richness_df_late, aes(x = x, y = y, fill = diversity)) +
  scale_fill_gradientn(colors=rev(terrain.colors(100)),limits = c(global_min, global_max), na.value = NA) +
  coord_fixed() +
  theme_void() +
  labs(title = "1990-2023",
       fill = "Richness")

# Extract diversity values from all maps
v1 <- values(climate_crop_mask_raster_avg_wi$layer.1)
v2 <- values(climate_crop_mask_raster_wi_early$layer.1)
v3 <- values(climate_crop_mask_raster_wi_late$layer.1) # assuming this is the 1990–2023 raster
# Calculate global min and max
global_min <- min(c(v1, v2, v3), na.rm = TRUE)
global_max <- max(c(v1, v2, v3), na.rm = TRUE)

###climate variables
climate_df <- as.data.frame(rasterToPoints(climate_crop_mask_raster_avg_wi))
###single plot w temp
p4<-ggplot() +
  geom_raster(data = climate_df, aes(x = x, y = y, fill = layer.1)) +
  scale_fill_viridis_c(option = "plasma", name = "Temp (°C)",limits = c(global_min, global_max), na.value = NA) +
  labs(fill = "Mean Annual Minimum Temperature",title="1960-2023") +
  coord_fixed() +
  theme_void() 
v1 <- values(climate_crop_mask_raster_avg_wi$layer.2)
v2 <- values(climate_crop_mask_raster_wi_early$layer.2)
v3 <- values(climate_crop_mask_raster_wi_late$layer.2) # assuming this is the 1990–2023 raster
# Calculate global min and max
global_min <- min(c(v1, v2, v3), na.rm = TRUE)
global_max <- max(c(v1, v2, v3), na.rm = TRUE)

#and ppt
p7<-ggplot() +
  geom_raster(data = climate_df, aes(x = x, y = y, fill = layer.2)) +
  scale_fill_viridis_c(option = "viridis", name = "Precip (mm)",limits = c(global_min, global_max), na.value = NA)+
labs(fill = "Mean Annual Precipitation (mm)",title="1960-2023") +
  coord_fixed() +
  theme_void() 
climate_df <- as.data.frame(rasterToPoints(climate_crop_mask_raster_wi_early))
###single plot w temp
p5<-ggplot() +
  geom_raster(data = climate_df, aes(x = x, y = y, fill = layer.1)) +
  scale_fill_viridis_c(option = "plasma", name = "Temp (°C)",limits = c(global_min, global_max), na.value = NA) +
  labs(fill = "Mean Annual Minimum Temperature",title="1960-1989") +
  coord_fixed() +
  theme_void() 
#and ppt
p8<-ggplot() +
  geom_raster(data = climate_df, aes(x = x, y = y, fill = layer.2)) +
  scale_fill_viridis_c(option = "viridis", name = "Precip (mm)",limits = c(global_min, global_max), na.value = NA)+
  labs(fill = "Mean Annual Precipitation (mm)",title="1960-1989") +
  coord_fixed() +
  theme_void() 
climate_df <- as.data.frame(rasterToPoints(climate_crop_mask_raster_wi_late))
###single plot w temp
p6<-ggplot() +
  geom_raster(data = climate_df, aes(x = x, y = y, fill = layer.1)) +
  scale_fill_viridis_c(option = "plasma", name = "Temp (°C)",limits = c(global_min, global_max), na.value = NA) +
  labs(fill = "Mean Annual Minimum Temperature",title="1990-2023") +
  coord_fixed() +
  theme_void() 
#and ppt
p9<-ggplot() +
  geom_raster(data = climate_df, aes(x = x, y = y, fill = layer.2)) +
  scale_fill_viridis_c(option = "viridis", name = "Precip (mm)",limits = c(global_min, global_max), na.value = NA)+
  labs(fill = "Mean Annual Precipitation (mm)",title="1990-2023") +
  coord_fixed() +
  theme_void() 
library(patchwork)

# Assume your plots are named p1 through p9
Fig2<-(p1 | p2 | p3) /
  (p4 | p5 | p6) /
  (p7 | p8 | p9) +
plot_annotation(tag_levels = 'a')
ggsave("/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/MPM Community Science Project/SDM/Plots/plant_sdms.pdf", plot = Fig2, width = 6, height = 8, units = "in")
######skippers
#diversity
# Extract diversity values from all maps
v1 <- values(SSDM_hesp_plant_reduced@diversity.map)
v2 <- values(SSDM_hesp_plant_ealry_reduced@diversity.map)
v3 <- values(SSDM_hesp_plant_late_reduced@diversity.map)  # assuming this is the 1990–2023 raster
# Calculate global min and max
global_min <- min(c(v1, v2, v3), na.rm = TRUE)
global_max <- max(c(v1, v2, v3), na.rm = TRUE)

richness_df <- as.data.frame(rasterToPoints(SSDM_hesp_plant_reduced@diversity.map), xy = TRUE)
p1<-ggplot() +
  geom_raster(data = richness_df, aes(x = x, y = y, fill = diversity)) +
  scale_fill_gradientn(colors=rev(terrain.colors(100)),limits = c(global_min, global_max), na.value = NA) +
  guides(fill = guide_colorbar(barwidth = 0.4, barheight = 4)) +
  coord_fixed() +
  theme_void() +
  labs(title = "1960-2023",
       fill = "Richness")
richness_df <- as.data.frame(rasterToPoints(SSDM_hesp_plant_ealry_reduced@diversity.map), xy = TRUE)
p2<-ggplot() +
  geom_raster(data = richness_df, aes(x = x, y = y, fill = diversity)) +
  scale_fill_gradientn(colors=rev(terrain.colors(100)),limits = c(global_min, global_max), na.value = NA) +
  guides(fill = guide_colorbar(barwidth = 0.4, barheight = 4)) +
  coord_fixed() +
  theme_void() +
  labs(title = "1960-1989",
       fill = "Richness")
richness_df <- as.data.frame(rasterToPoints(SSDM_hesp_plant_late_reduced@diversity.map), xy = TRUE)
p3<-ggplot() +
  geom_raster(data = richness_df, aes(x = x, y = y, fill = diversity)) +
  scale_fill_gradientn(colors=rev(terrain.colors(100)),limits = c(global_min, global_max), na.value = NA) +
  guides(fill = guide_colorbar(barwidth = 0.4, barheight = 4)) +
  coord_fixed() +
  theme_void() +
  labs(title = "1990-2023",
       fill = "Richness")
###SSS
v1 <- values(SSDM_hesp_plant_reduced@esdms$`Epargyreus clarus (Cramer, 1775).Ensemble.SDM`@projection)
v2 <- values(SSDM_hesp_plant_ealry_reduced@esdms$`Epargyreus clarus (Cramer, 1775).Ensemble.SDM`@projection)
v3 <- values(SSDM_hesp_plant_late_reduced@esdms$`Epargyreus clarus (Cramer, 1775).Ensemble.SDM`@projection)  # assuming this is the 1990–2023 raster
# Calculate global min and max
global_min <- min(c(v1, v2, v3), na.rm = TRUE)
global_max <- max(c(v1, v2, v3), na.rm = TRUE)

sss_df<-as.data.frame(rasterToPoints(SSDM_hesp_plant_reduced@esdms$`Epargyreus clarus (Cramer, 1775).Ensemble.SDM`@projection), xy = TRUE)
p4<-ggplot() +
  geom_raster(data = sss_df, aes(x = x, y = y, fill = Probability)) +
  scale_fill_gradientn(colors=rev(terrain.colors(100)),limits = c(global_min, global_max), na.value = NA) +
  guides(fill = guide_colorbar(barwidth = 0.4, barheight = 4)) +
  coord_fixed() +
  theme_void() +
  labs(title="1960-2023",
       fill = "Presence Prob")
sss_df<-as.data.frame(rasterToPoints(SSDM_hesp_plant_ealry_reduced@esdms$`Epargyreus clarus (Cramer, 1775).Ensemble.SDM`@projection), xy = TRUE)
p5<-ggplot() +
  geom_raster(data = sss_df, aes(x = x, y = y, fill = Probability)) +
  scale_fill_gradientn(colors=rev(terrain.colors(100)),limits = c(global_min, global_max), na.value = NA) +
  guides(fill = guide_colorbar(barwidth = 0.4, barheight = 4)) +
  coord_fixed() +
  theme_void() +
  labs(title="1960-1989",
       fill = "Presence Prob")
sss_df<-as.data.frame(rasterToPoints(SSDM_hesp_plant_late_reduced@esdms$`Epargyreus clarus (Cramer, 1775).Ensemble.SDM`@projection), xy = TRUE)
p6<-ggplot() +
  geom_raster(data = sss_df, aes(x = x, y = y, fill = Probability)) +
  scale_fill_gradientn(colors=rev(terrain.colors(100)),limits = c(global_min, global_max), na.value = NA) +
  guides(fill = guide_colorbar(barwidth = 0.4, barheight = 4)) +
  coord_fixed() +
  theme_void() +
  labs(title="1990-2023",
       fill = "Presence Prob")
####fiery
v1 <- values(SSDM_hesp_plant_reduced@esdms$`Hylephila phyleus (Drury, 1773).Ensemble.SDM`@projection)
v2 <- values(SSDM_hesp_plant_ealry_reduced@esdms$`Hylephila phyleus (Drury, 1773).Ensemble.SDM`@projection)
v3 <- values(SSDM_hesp_plant_late_reduced@esdms$`Hylephila phyleus (Drury, 1773).Ensemble.SDM`@projection)  # assuming this is the 1990–2023 raster
# Calculate global min and max
global_min <- min(c(v1, v2, v3), na.rm = TRUE)
global_max <- max(c(v1, v2, v3), na.rm = TRUE)

fs_df<-as.data.frame(rasterToPoints(SSDM_hesp_plant_reduced@esdms$`Hylephila phyleus (Drury, 1773).Ensemble.SDM`@projection), xy = TRUE)
p7<-ggplot() +
  geom_raster(data = fs_df, aes(x = x, y = y, fill = Probability)) +
  scale_fill_gradientn(colors=rev(terrain.colors(100)),limits = c(global_min, global_max), na.value = NA) +
  guides(fill = guide_colorbar(barwidth = 0.4, barheight = 4)) +
  coord_fixed() +
  theme_void() +
  labs(
       fill = "Presence Prob")
fs_df<-as.data.frame(rasterToPoints(SSDM_hesp_plant_ealry_reduced@esdms$`Hylephila phyleus (Drury, 1773).Ensemble.SDM`@projection), xy = TRUE)
p8<-ggplot() +
  geom_raster(data = fs_df, aes(x = x, y = y, fill = Probability)) +
  scale_fill_gradientn(colors=rev(terrain.colors(100)),limits = c(global_min, global_max), na.value = NA) +
  guides(fill = guide_colorbar(barwidth = 0.4, barheight = 4)) +
  coord_fixed() +
  theme_void() +
  labs(
       fill = "Presence Prob")
fs_df<-as.data.frame(rasterToPoints(SSDM_hesp_plant_late_reduced@esdms$`Hylephila phyleus (Drury, 1773).Ensemble.SDM`@projection), xy = TRUE)
p9<-ggplot() +
  geom_raster(data = fs_df, aes(x = x, y = y, fill = Probability)) +
  scale_fill_gradientn(colors=rev(terrain.colors(100)),limits = c(global_min, global_max), na.value = NA) +
  guides(fill = guide_colorbar(barwidth = 0.4, barheight = 4)) +
  coord_fixed() +
  theme_void() +
  labs(
       fill = "Presence Prob")
###least
v1 <- values(SSDM_hesp_plant_reduced@esdms$`Ancyloxypha numitor (Fabricius, 1793).Ensemble.SDM`@projection)
v2 <- values(SSDM_hesp_plant_ealry_reduced@esdms$`Ancyloxypha numitor (Fabricius, 1793).Ensemble.SDM`@projection)
v3 <- values(SSDM_hesp_plant_late_reduced@esdms$`Ancyloxypha numitor (Fabricius, 1793).Ensemble.SDM`@projection)  # assuming this is the 1990–2023 raster
# Calculate global min and max
global_min <- min(c(v1, v2, v3), na.rm = TRUE)
global_max <- max(c(v1, v2, v3), na.rm = TRUE)

ls_df<-as.data.frame(rasterToPoints(SSDM_hesp_plant_reduced@esdms$`Ancyloxypha numitor (Fabricius, 1793).Ensemble.SDM`@projection), xy = TRUE)
p10<-ggplot() +
  geom_raster(data = ls_df, aes(x = x, y = y, fill = Probability)) +
  scale_fill_gradientn(colors=rev(terrain.colors(100)),limits = c(global_min, global_max), na.value = NA) +
  guides(fill = guide_colorbar(barwidth = 0.4, barheight = 4)) +
  coord_fixed() +
  theme_void() +
  labs(
       fill = "Presence Prob")
ls_df<-as.data.frame(rasterToPoints(SSDM_hesp_plant_ealry_reduced@esdms$`Ancyloxypha numitor (Fabricius, 1793).Ensemble.SDM`@projection), xy = TRUE)
p11<-ggplot() +
  geom_raster(data = ls_df, aes(x = x, y = y, fill = Probability)) +
  scale_fill_gradientn(colors=rev(terrain.colors(100)),limits = c(global_min, global_max), na.value = NA) +
  guides(fill = guide_colorbar(barwidth = 0.4, barheight = 4)) +
  coord_fixed() +
  theme_void() +
  labs(
       fill = "Presence Prob")
ls_df<-as.data.frame(rasterToPoints(SSDM_hesp_plant_late_reduced@esdms$`Ancyloxypha numitor (Fabricius, 1793).Ensemble.SDM`@projection), xy = TRUE)
p12<-ggplot() +
  geom_raster(data = ls_df, aes(x = x, y = y, fill = Probability)) +
  scale_fill_gradientn(colors=rev(terrain.colors(100)),limits = c(global_min, global_max), na.value = NA) +
  guides(fill = guide_colorbar(barwidth = 0.4, barheight = 4)) +
  coord_fixed() +
  theme_void() +
  labs(
       fill = "Presence Prob")
#delaware
v1 <- values(SSDM_hesp_plant_reduced@esdms$`Anatrytone logan (Edwards, 1863).Ensemble.SDM`@projection)
v2 <- values(SSDM_hesp_plant_ealry_reduced@esdms$`Anatrytone logan (Edwards, 1863).Ensemble.SDM`@projection)
v3 <- values(SSDM_hesp_plant_late_reduced@esdms$`Anatrytone logan (Edwards, 1863).Ensemble.SDM`@projection)  # assuming this is the 1990–2023 raster
# Calculate global min and max
global_min <- min(c(v1, v2, v3), na.rm = TRUE)
global_max <- max(c(v1, v2, v3), na.rm = TRUE)

ds_df<-as.data.frame(rasterToPoints(SSDM_hesp_plant_reduced@esdms$`Anatrytone logan (Edwards, 1863).Ensemble.SDM`@projection), xy = TRUE)
p13<-ggplot() +
  geom_raster(data = ds_df, aes(x = x, y = y, fill = Probability)) +
  scale_fill_gradientn(colors=rev(terrain.colors(100)),limits = c(global_min, global_max), na.value = NA) +
  guides(fill = guide_colorbar(barwidth = 0.4, barheight = 4)) +
  coord_fixed() +
  theme_void() +
  labs(
       fill = "Presence Prob")
ds_df<-as.data.frame(rasterToPoints(SSDM_hesp_plant_ealry_reduced@esdms$`Anatrytone logan (Edwards, 1863).Ensemble.SDM`@projection), xy = TRUE)
p14<-ggplot() +
  geom_raster(data = ds_df, aes(x = x, y = y, fill = Probability)) +
  scale_fill_gradientn(colors=rev(terrain.colors(100)),limits = c(global_min, global_max), na.value = NA) +
  guides(fill = guide_colorbar(barwidth = 0.4, barheight = 4)) +
  coord_fixed() +
  theme_void() +
  labs(
       fill = "Presence Prob")
ds_df<-as.data.frame(rasterToPoints(SSDM_hesp_plant_late_reduced@esdms$`Anatrytone logan (Edwards, 1863).Ensemble.SDM`@projection), xy = TRUE)

p15<-ggplot() +
  geom_raster(data = ds_df, aes(x = x, y = y, fill = Probability)) +
  scale_fill_gradientn(colors=rev(terrain.colors(100)),limits = c(global_min, global_max), na.value = NA) +
  guides(fill = guide_colorbar(barwidth = 0.4, barheight = 4)) +
  coord_fixed() +
  theme_void() +
  labs(
    fill = "Presence Prob")


###naked plot?
p15<-ggplot() +
  geom_sf(data = wi_shape_proj, fill = NA, color = "black", size = 0.4) +
   geom_point(data = subset(occ_df_hesp_late,acceptedScientificName=="Anatrytone logan (Edwards, 1863)"),aes(x = x, y = y), color = "black", size = 0.25,alpha = 0.5)  +
  coord_sf() +theme_void() 
###grids
Fig3<-(p1 | p2 | p3) /
  (p13 | p14 | p15) +
  plot_annotation(tag_levels = 'a')
Fig3
ggsave("/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/MPM Community Science Project/SDM/Plots/hesp_sdms_1.pdf", plot = Fig3, width = 8, height = 6, units = "in")
Fig4<-
(p4 | p5 | p6) /
  (p7 | p8 | p9) /
  (p10 | p11 | p12)  + plot_annotation(tag_levels = 'a')
Fig4
ggsave("/Users/kathrynsullivan/Library/CloudStorage/OneDrive-MarquetteUniversity/MPM Community Science Project/SDM/Plots/hesp_sdms_2.pdf", plot = Fig4, width = 8, height = 8, units = "in")

early<-SSDM_hesp_plant_ealry_reduced@diversity.map
raster::mean(early@data@values,na.rm=TRUE)
late<-raster::mean(SSDM_hesp_plant_late_reduced@diversity.map)
raster::mean(late@data@values,na.rm=TRUE)
#############
all<-SSDM_hesp_plant_reduced@diversity.map
raster::mean(all@data@values,na.rm=TRUE)
sss<-SSDM_hesp_plant_ealry_reduced@esdms$`Epargyreus clarus (Cramer, 1775).Ensemble.SDM`
raster::mean(sss@projection@data@values,na.rm=TRUE)
sss<-SSDM_hesp_plant_late_reduced@esdms$`Epargyreus clarus (Cramer, 1775).Ensemble.SDM`
raster::mean(sss@projection@data@values,na.rm=TRUE)
del<-SSDM_hesp_plant_ealry_reduced@esdms$`Anatrytone logan (Edwards, 1863).Ensemble.SDM`
raster::mean(del@projection@data@values,na.rm=TRUE)
del<-SSDM_hesp_plant_late_reduced@esdms$`Anatrytone logan (Edwards, 1863).Ensemble.SDM`
raster::mean(del@projection@data@values,na.rm=TRUE)

plant<-SSDM_plant_all@diversity.map
p_early<-SSDM_plant_early@diversity.map
#diversity correlation figs
#p vs.b all
values_plants <- getValues(plant$diversity)
values_butterfly <- getValues(all$diversity)
regression<-data.frame(values_plants,values_butterfly)
lm_model <- lm(values_butterfly ~ values_plants, data = regression)
# Get R-squared value
r_squared <- summary(lm_model)$r.squared
r_squared_label <- bquote(R^2 == .(round(r_squared, 2)))
library(boot)

# Define bootstrap function
boot_fn <- function(data, indices) {
  d <- data[indices, ]
  coef(lm(values_butterfly ~ values_plants, data = d))[2]  # slope
}

# Run bootstrap
set.seed(42)
boot_out <- boot(data = regression, statistic = boot_fn, R = 2000)

# Bootstrap results
boot_out

# 95% percentile confidence interval
boot.ci(boot_out, type = "perc")
##R2 bootstraps
boot_r2 <- function(data, indices) {
  d <- data[indices, ]
  summary(lm(values_butterfly ~ values_plants, data = d))$r.squared
}

boot_out_r2 <- boot(data = regression, statistic = boot_r2, R = 2000)
boot.ci(boot_out_r2, type = "perc")
##Spearman rank for curvy data
spearman<-cor.test(regression$values_butterfly,
                   regression$values_plants,
                   method = "spearman")
spearman$estimate
library(ggplot2)
cor_all<-ggplot(regression, aes(x = values_plants, y = values_butterfly)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Plant Richness", y = "Butterfly Richness",title="1960-2023") +
  # annotate(
  #   "text",
  #    x = 45, y = .5,
  #    label = as.expression(bquote(R^2 == .(round(r_squared, 2)))),
  #    hjust = 1, vjust = -1,
  #    size = 4
   #)+
  theme_minimal()
cor_all
library(ggplot2)

ggplot(regression, aes(x = values_plants, y = values_butterfly)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  geom_smooth(method = "loess", color = "red", linetype = "dashed") +
  labs(caption = "Blue = linear; Red = LOESS") +
  theme_minimal()

#p vs.b early
values_plants <- getValues(p_early$diversity)
values_butterfly <- getValues(early$diversity)
regression<-data.frame(values_plants,values_butterfly)
lm_model <- lm(values_butterfly ~ values_plants, data = regression)
# Get R-squared value
r_squared <- summary(lm_model)$r.squared
r_squared_label <- bquote(R^2 == .(round(r_squared, 2)))
set.seed(42)
boot_out <- boot(data = regression, statistic = boot_fn, R = 2000)

# Bootstrap results
boot_out

# 95% percentile confidence interval
boot.ci(boot_out, type = "perc")
##R2 bootstraps
boot_out_r2 <- boot(data = regression, statistic = boot_r2, R = 2000)
boot.ci(boot_out_r2, type = "perc")
##Spearman rank for curvy data
spearman<-cor.test(regression$values_butterfly,
                   regression$values_plants,
                   method = "spearman")
spearman$estimate
library(ggplot2)
cor_ear<-ggplot(regression, aes(x = values_plants, y = values_butterfly)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Plant Richness", y = "Butterfly Richness",title="1960-1989") +
  theme_minimal()
cor_ear
ggplot(regression, aes(x = values_plants, y = values_butterfly)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  geom_smooth(method = "loess", color = "red", linetype = "dashed") +
  labs(caption = "Blue = linear; Red = LOESS") +
  theme_minimal()

#p vs.b late
late<-SSDM_hesp_plant_late_reduced@diversity.map
p_late<-SSDM_plant_late@diversity.map

values_plants <- getValues(p_late$diversity)
values_butterfly <- getValues(late$diversity)
regression<-data.frame(values_plants,values_butterfly)
lm_model <- lm(values_butterfly ~ values_plants, data = regression)
# Get R-squared value
r_squared <- summary(lm_model)$r.squared
r_squared_label <- bquote(R^2 == .(round(r_squared, 2)))
set.seed(42)
boot_out <- boot(data = regression, statistic = boot_fn, R = 2000)

# Bootstrap results
boot_out

# 95% percentile confidence interval
boot.ci(boot_out, type = "perc")

##R2 bootstraps
boot_out_r2 <- boot(data = regression, statistic = boot_r2, R = 2000)
boot.ci(boot_out_r2, type = "perc")
##Spearman rank for curvy data
spearman<-cor.test(regression$values_butterfly,
                   regression$values_plants,
                   method = "spearman")
spearman$estimate
ggplot(regression, aes(x = values_plants, y = values_butterfly)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  geom_smooth(method = "loess", color = "red", linetype = "dashed") +
  labs(caption = "Blue = linear; Red = LOESS") +
  theme_minimal()

library(ggplot2)
cor_late<-ggplot(regression, aes(x = values_plants, y = values_butterfly)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Plant Richness", y = "Butterfly Richness",title="1990-2023") +
  theme_minimal()
cor_late





#fig 5
Fig5<-(cor_all /
  cor_ear/
    cor_late) +
  plot_annotation(tag_levels = 'a')
Fig5
ggsave("corr.pdf", plot = Fig5, width = 8, height = 10, units = "in")
########supplement
#climate and species--early
values_ppt <- getValues(climate_crop_mask_raster_wi_early$layer.2)
values_butterfly <- getValues(diversity_early$diversity.2)
regression<-data.frame(values_ppt,values_butterfly)

lm_model <- lm(values_butterfly ~ values_ppt, data = regression)
lm_model
# Get R-squared value
r_squared <- summary(lm_model)$r.squared
r_squared
r_squared_label <- sprintf("R² = %.2f", r_squared)
d<-ggplot(regression, aes(x = values_ppt, y = values_butterfly)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "1960-1989", x = "Mean Annual Precipitation (mm)", y = "Butterfly Richness") +
  annotate(
    "text",
    x = 900, y = 15,
    label = as.expression(bquote(R^2 == .(round(r_squared, 2)))),
    hjust = 1, vjust = -1,
    size = 4
  )+
  theme_minimal()
d

values_ppt <- getValues(climate_crop_mask_raster_wi_late$layer.2)
values_butterfly <- getValues(diversity_late$diversity.2)
regression<-data.frame(values_ppt,values_butterfly)

lm_model <- lm(values_butterfly ~ values_ppt, data = regression)
lm_model
# Get R-squared value
r_squared <- summary(lm_model)$r.squared
r_squared
r_squared_label <- sprintf("R² = %.2f", r_squared)
d2<-ggplot(regression, aes(x = values_ppt, y = values_butterfly)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "1990-2023", x = "Mean Annual Precipitation (mm)", y = "Butterfly Richness") +
  annotate(
    "text",
    x = 1000, y = 13,
    label = as.expression(bquote(R^2 == .(round(r_squared, 2)))),
    hjust = 1, vjust = -1,
    size = 4
  )+
  theme_minimal()
d2
###more wet
#climate variables--tmin
values_tmin <- getValues(climate_crop_mask_raster_wi_early$layer.1)
values_butterfly <- getValues(diversity_early$diversity.2)
regression<-data.frame(values_tmin,values_butterfly)

lm_model <- lm(values_butterfly ~ values_tmin, data = regression)
lm_model
# Get R-squared value
r_squared <- summary(lm_model)$r.squared
r_squared
r_squared_label <- sprintf("R² = %.2f", r_squared)
d1<-ggplot(regression, aes(x = values_tmin, y = values_butterfly)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "1960-1989", x = "Mean Annual Minimum Temperature (C)", y = "Butterfly Richness") +
  annotate(
    "text",
    x = 4, y = 1,
    label = as.expression(bquote(R^2 == .(round(r_squared, 2)))),
    hjust = 1, vjust = -1,
    size = 4
  )+
  theme_minimal()
d1
values_tmin <- getValues(climate_crop_mask_raster_wi_late$layer.1)
values_butterfly <- getValues(diversity_late$diversity.2)
regression<-data.frame(values_tmin,values_butterfly)

lm_model <- lm(values_butterfly ~ values_tmin, data = regression)
lm_model
# Get R-squared value
r_squared <- summary(lm_model)$r.squared
r_squared
r_squared_label <- sprintf("R² = %.2f", r_squared)
d3<-ggplot(regression, aes(x = values_tmin, y = values_butterfly)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "1990-2023", x = "Mean Annual Minimum Temperature (C)", y = "Butterfly Richness") +
  annotate(
    "text",
    x = 5, y = 1,
    label = as.expression(bquote(R^2 == .(round(r_squared, 2)))),
    hjust = 1, vjust = -1,
    size = 4
  )+
  theme_minimal()
d3

S51<-(d | d2) /
(d1 | d3) +  plot_annotation(tag_levels = 'a')
S51
ggsave("hesp_climate.pdf", plot = S51, width = 8, height = 6, units = "in")
