
options(scipen=999)

library(ggplot2)
library(sf)
library(dplyr)
library(data.table)
library(tidyr)
library(ncdf4)
library(writexl)
library(readxl)



# User must ensure repository root is the working directory (i.e., project_dir<- "C:/path/to/catch-dependence")

external_temp_dir <- file.path(
  project_dir,
  "data",
  "external",
  "bottom_temperature"
)

stock_area_dir <- file.path(
  project_dir,
  "data",
  "spatial",
  "stock_areas"
)

derived_spatial_dir <- file.path(
  project_dir,
  "data",
  "spatial",
  "derived"
)

processed_temp_dir <- file.path(
  project_dir,
  "data",
  "processed",
  "bottom_temperature"
)

work_temp_dir <- file.path(
  project_dir,
  "work",
  "02_compile_bottom_temperature"
)

dir.create(processed_temp_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(work_temp_dir, recursive = TRUE, showWarnings = FALSE)


# Load the derived convex hull footprint
hull_gpkg <- file.path(
  derived_spatial_dir,
  "VTR_convex_hull.gpkg"
)

if (!file.exists(hull_gpkg)) {
  stop("Convex hull GeoPackage not found at: ", hull_gpkg,
       "\nDid you run the earlier code that writes VTR_convex_hull.gpkg?")
}

VTR_boundaries_subset <- sf::st_read(hull_gpkg, layer = "VTR_convex_hull", quiet = TRUE) %>%
  sf::st_make_valid()

# standardize CRS to lon/lat WGS84
if (is.na(sf::st_crs(VTR_boundaries_subset))) {
  sf::st_crs(VTR_boundaries_subset) <- 4326
} else {
  VTR_boundaries_subset <- sf::st_transform(VTR_boundaries_subset, 4326)
}


# Pull in bt temp deltas for years 1-40 
# set path and filename
ncfname_1_40 <- file.path(
  external_temp_dir,
  "CM2.6_2xCO2_USNES_bottom_temp_delta_monthly_years_1_40.nc"
)

dname <- "TEMP_BOTTOM_DELTA"  # note: tmp means temperature (not temporary)

ncin <- nc_open(ncfname_1_40)
print(ncin)

ncin

# get longitude and latitude
lon <- ncvar_get(ncin,"XT_OCEAN2001_225021_150")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"YT_OCEAN1560_180961_177")
nlat <- dim(lat)
head(lat)

# get time
time <- ncvar_get(ncin, "TIME")
tunits <- ncatt_get(ncin,"TIME","units")
nt <- dim(time)
print(nt)
print(tunits)
length(tunits)

# get temperature
tmp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)


# get global attributes
title <- ncatt_get(ncin,0,"title")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
history <- ncatt_get(ncin,0,"history")
Conventions <- ncatt_get(ncin,0,"Conventions")

# Loop through years to get bt_tmp by month. Then combine all the years into one dataset
lat_lons = list()
lat_lons_all = list()

dim(time)
for (m in 1:dim(time)){
    
  #m <- m #get a single slice or layer (year m)
  tmp_slice <- tmp_array[,,m]
  dim(tmp_slice)
  
  # levelplot of the slice
  grid <- expand.grid(lon=lon, lat=lat)

  # create dataframe -- reshape data
  # matrix (nlon*nlat rows by 2 cols) of lons and lats
  lonlat <- as.matrix(expand.grid(lon,lat))
  dim(lonlat)
  
  # vector of `tmp` values
  tmp_vec <- as.vector(tmp_slice)
  length(tmp_vec)
  
  # reshape the array into vector
  tmp_vec_long <- as.vector(tmp_vec)
  length(tmp_vec_long)
  
  # reshape the vector into a matrix
  tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat)
  dim(tmp_mat)
  
  head(na.omit(tmp_mat))
  
  # create a dataframe
  lonlat <- as.matrix(expand.grid(lon,lat))
  tmp_df2 <- data.frame(cbind(lonlat,tmp_mat))
  names(tmp_df2) <- c("lon","lat","tmp")
  
  tmp_df2 <- na.omit(tmp_df2)
  tmp_df2$month <- m
  
  lat_lons[[m]] <- tmp_df2
  
}
bt_temp_deltas= rbindlist(lat_lons, fill=TRUE)
ls(bt_temp_deltas)
sp_points = st_as_sf(bt_temp_deltas,coords = c('lon' ,'lat'), crs = 4326) #make points spatial

# full data intersection
kept_points_all <- sf::st_join(sp_points, VTR_boundaries_subset)
kept_points_all1<-kept_points_all %>% 
  dplyr::select(area, tmp, month, geometry)
kept_points_all1<-kept_points_all1 %>% replace_na(list(area = 'outside'))
kept_points_all1<-kept_points_all1 %>% 
  dplyr::filter(area=="VTR_convex_hull")

# collapse mean bottom temperatures by year and month
GOM_bt_temps<- kept_points_all1 %>% as.data.frame()  %>% select(c(month, tmp)) %>% 
  dplyr::group_by(month) %>% 
  dplyr::summarise(mean_temp=mean(tmp))

GOM_bt_temps$year <- c(0, rep(1:(nrow(GOM_bt_temps)-1)%/%12))
GOM_bt_temps$year <-GOM_bt_temps$year+1

# save the within-GOM mean bottom temps by month 
first_half <- GOM_bt_temps

ncdf4::nc_close(ncin)

# Repeat for years 41-80
# set path and filename
ncfname_41_80 <- file.path(
  external_temp_dir,
  "CM2.6_2xCO2_USNES_bottom_temp_delta_monthly_years_41_80.nc"
)

ncin <- ncdf4::nc_open(ncfname_41_80)
dname <- "TEMP_BOTTOM_DELTA"  # note: tmp means temperature (not temporary)

print(ncin)

ncin

# get longitude and latitude
lon <- ncvar_get(ncin,"XT_OCEAN2001_225021_150")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"YT_OCEAN1560_180961_177")
nlat <- dim(lat)
head(lat)

# get time
time <- ncvar_get(ncin, "TIME")
tunits <- ncatt_get(ncin,"TIME","units")
nt <- dim(time)
print(nt)
print(tunits)
length(tunits)

# get temperature
tmp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)


# get global attributes
title <- ncatt_get(ncin,0,"title")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
history <- ncatt_get(ncin,0,"history")
Conventions <- ncatt_get(ncin,0,"Conventions")


# loop through years to get bt_tmp by month. Then combine all the years into one dataset
lat_lons = list()
lat_lons_all = list()

dim(time)
for (m in 1:dim(time)){
  
  tmp_slice <- tmp_array[,,m]
  dim(tmp_slice)
  
  # levelplot of the slice
  grid <- expand.grid(lon=lon, lat=lat)
  # levelplot(tmp_slice ~ lon * lat, data=grid,   pretty=T, 
  #           col.regions=(rev(brewer.pal(10,"RdBu"))))
  
  
  # create dataframe -- reshape data
  # matrix (nlon*nlat rows by 2 cols) of lons and lats
  lonlat <- as.matrix(expand.grid(lon,lat))
  dim(lonlat)
  
  # vector of `tmp` values
  tmp_vec <- as.vector(tmp_slice)
  length(tmp_vec)
  
  # reshape the array into vector
  tmp_vec_long <- as.vector(tmp_vec)
  length(tmp_vec_long)
  
  # reshape the vector into a matrix
  tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat)
  dim(tmp_mat)
  
  head(na.omit(tmp_mat))
  
  # create a dataframe
  lonlat <- as.matrix(expand.grid(lon,lat))
  tmp_df2 <- data.frame(cbind(lonlat,tmp_mat))
  names(tmp_df2) <- c("lon","lat","tmp")
  
  tmp_df2 <- na.omit(tmp_df2)
  tmp_df2$month <- m
  
  lat_lons[[m]] <- tmp_df2
  
}
bt_temp_deltas= rbindlist(lat_lons, fill=TRUE)
ls(bt_temp_deltas)
sp_points = st_as_sf(bt_temp_deltas,coords = c('lon' ,'lat'), crs = 4326) #make points spatial


sp_points=st_transform(sp_points,st_crs(VTR_boundaries_subset)) # Match the point and polygon CRS

# full data intersection
kept_points_all <- sf::st_join(sp_points, VTR_boundaries_subset)
kept_points_all1<-kept_points_all %>% 
  dplyr::select(area, tmp, month, geometry)
kept_points_all1<-kept_points_all1 %>% replace_na(list(area = 'outside'))
kept_points_all1<-kept_points_all1 %>% 
  dplyr::filter(area=="VTR_convex_hull")


# collapse mean bottom temperatures by year and month
GOM_bt_temps<- kept_points_all1 %>% as.data.frame()  %>% select(c(month, tmp)) %>% 
  dplyr::group_by(month) %>% 
  dplyr::summarise(mean_temp=mean(tmp))

GOM_bt_temps$year <- c(0, rep(1:(nrow(GOM_bt_temps)-1)%/%12))
GOM_bt_temps$year <-GOM_bt_temps$year+1
GOM_bt_temps$year <-GOM_bt_temps$year+40 #this is the second half of the data 
GOM_bt_temps$month <-GOM_bt_temps$month+480 #this is the second half of the data 

# save the within-GOM mean bottom temps by month 
second_half <- GOM_bt_temps
ncdf4::nc_close(ncin)


# combine the two datasets
full_data <- dplyr::bind_rows(
  first_half,
  second_half
)


# FINAL OUTPUT DATASET
writexl::write_xlsx(
  full_data,
  file.path(
    processed_temp_dir,
    "mean_bottom_temp_deltas_GoM_convex_hull_yrs_1_80.xlsx"
  )
)

#ggplot(full_data)+aes(x=month,y=mean_temp)+geom_line()

