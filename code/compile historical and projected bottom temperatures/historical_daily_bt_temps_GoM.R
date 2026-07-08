
# load packages 
pkgs_to_use <- c("sf", "ncdf4", "raster","rasterVis", "RColorBrewer" , "chron", "lattice", "RColorBrewer", "rlist", "data.table", 
                 "rlang","foreign","tidyverse", "sp", "sf", "here", "scales", "raster", "stringi", "ggplot2", "tigris",
                 "dplyr", "writexl", "readxl","mapview","geosphere", "sf",  "terra", "maps", "spatialEco",
                 "ggspatial", "ggrepel", "marmap")
install.packages(setdiff(pkgs_to_use, rownames(installed.packages())))  
lapply(pkgs_to_use, library, character.only = TRUE, quietly = TRUE)

# set directory which holds all the data
# Data required: 

  # Gom cod stock areas - NOAA public use shapefiles available at:  https://www.fisheries.noaa.gov/resources/maps?title=&tid%5B1000001111%5D=1000001111&field_species_vocab_target_id=&sort_by=created and archived in this repository

  # lat-lons from for-hire fishing vessels that reported cathing cod or haddock - not publicly available due to NOAA confidentiality restrictions. Derived convex hulls inclued in this capsule. To replicate, start at section 2) 

  # Historical bottom temperature data - included in this capsule

### Section 1) Generate convex hull of fishing activity based on for-hire vessel trip reports ###

cd <-"[enter data directory]"

PROJ.USE = CRS('+proj=aea +lat_1=28 +lat_2=42 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 ')

# GoM cod stock areas
GoM_shape <- st_read(dsn=paste0(cd, "/Stock_Areas"),
                      layer="Stock_Areas")
 
 GoM_shape <-GoM_shape %>%
   dplyr::filter(AREANAME  %in% c("GOM Cod Stock Area"))

# East Coast US state map for visualization
states <- map_data("state")
east_coast <- subset(states, region %in% c("maine", "massachusetts", "vermont", "new hampshire","rhode island", "connecticut", "new york", "pennsylvania",
                                           "new jersey", "maryland", "delaware", "district of columbia", "virginia", "west virginia", "north carolina"))

#VTR lat-lons - not publicly available
#Create a "fishing footprint" (convex hull) based on VTR points for all years combined
vtr_lat_lons_cod<-read_csv(paste0(cd, "Yearly_Cod_Recreational_Lat_Lon.csv"), show_col_types = FALSE)
vtr_lat_lons_hadd<-read_csv(paste0(cd, "Yearly_Haddock_Recreational_Lat_Lon.csv"), show_col_types = FALSE)

vtr_lat_lons_both<-rbind(vtr_lat_lons_cod, vtr_lat_lons_hadd)
vtr_lat_lons_both<-vtr_lat_lons_both %>% 
  dplyr::mutate(Species="BOTH")

lat_lons_ready <- vtr_lat_lons_both %>%
  dplyr::select(Species, LAT, LON, YEAR) %>%
  drop_na()

# Compute the mean lat/lon
lat_lons_ready <- lat_lons_ready %>%  
  group_by(Species) %>%
  mutate(mean_lat = mean(LAT))  %>%
  mutate(mean_lon = mean(LON)) %>% 
  ungroup()

# Compute the distance of each lat/lon to the centroid 
library(geosphere)
lat_lons_ready$distance<-distGeo(lat_lons_ready[,2:3], lat_lons_ready[,5:6])

ggplot(east_coast) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey40", alpha = 0.3, color = "black") +
  geom_point(data = lat_lons_ready, aes(x = LON, y = LAT), color = "red", alpha = 0.3, show.legend = FALSE) +
  geom_sf(data = GoM_shape['AREANAME'], fill = "blue", alpha = 0.3, show.legend = FALSE) +
  coord_sf(xlim = c(-74.6, -66), ylim = c(40, 44.7)) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey60", "white")
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20"    )
  )


# Before taking the 95th percentile of distance to the centoid and creating the convex hull, keep only VTR lat lons falling within the stock area
mgt_boundaries_subset<- st_as_sf(GoM_shape) 
mgt_boundaries_subset<-mgt_boundaries_subset %>% 
  dplyr::select(geometry, COMMNAME) 

sp_points = st_as_sf(lat_lons_ready,coords = c('LON' ,'LAT' )) # make points spatial
st_crs(sp_points)= 4326

mgt_boundaries_subset=st_transform(mgt_boundaries_subset,st_crs(sp_points)) # Match the point and polygon CRS

VTR_GoM_points2 <- sf::st_join(sp_points, mgt_boundaries_subset)

# allocate spatial points as outside the GOM if the COMMNAME is missing 
VTR_GoM_points2_outside <- VTR_GoM_points2 %>% 
  dplyr::filter(is.na(COMMNAME))
VTR_GoM_points2_outside = data.frame(st_coordinates(st_cast(VTR_GoM_points2_outside$geometry,"POINT")))
VTR_GoM_points2_outside<-VTR_GoM_points2_outside %>% 
  dplyr::rename(LON=X, LAT=Y)

# allocate spatial points as inside the GOM if the COMMNAME is not missing 
VTR_GoM_points2_inside <- VTR_GoM_points2 %>% 
  dplyr::filter(!is.na(COMMNAME))
VTR_GoM_points2_inside = data.frame(st_coordinates(st_cast(VTR_GoM_points2_inside$geometry,"POINT")))
VTR_GoM_points2_inside<-VTR_GoM_points2_inside %>% 
  dplyr::rename(LON=X, LAT=Y)

# check to make sure points are correctly allocated within the stock area 
ggplot(east_coast) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey40", alpha = 0.3, color = "black") +
  geom_point(data = VTR_GoM_points2_outside, aes(x = LON, y = LAT), color = "red", alpha = 0.3, show.legend = FALSE) +
  geom_point(data = VTR_GoM_points2_inside, aes(x = LON, y = LAT), color = "black", alpha = 0.3, show.legend = FALSE) +
  geom_sf(data = GoM_shape['AREANAME'], fill = "blue", alpha = 0.3, show.legend = FALSE) +
  coord_sf(xlim = c(-74.6, -66), ylim = c(40, 44.7)) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey60", "white")
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20"    )
  )


# use only VTR lat-lons within the GoM to make convex hull
VTR_GoM_points2_inside <- VTR_GoM_points2_inside %>%  
  mutate(LON_mean = mean(LON))  %>%
  mutate(LAT_mean = mean(LAT)) 

# Calculate the distance of each lat/lon to the centroid and 95th percentile 
library(geosphere)
VTR_GoM_points2_inside$distance<-distGeo(VTR_GoM_points2_inside[,1:2], VTR_GoM_points2_inside[,3:4])

VTR_GoM_points2_inside <- VTR_GoM_points2_inside %>%  
  mutate(q95 = quantile(distance, 0.95)) 

VTR_GoM_points2_inside2 <- VTR_GoM_points2_inside %>%  
  dplyr::filter(distance<=q95)

VTR_GoM_points2_inside3 <- VTR_GoM_points2_inside %>%  
  dplyr::filter(distance>q95)

ggplot(east_coast) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey40", alpha = 0.3, color = "black") +
  geom_point(data = VTR_GoM_points2_inside3, aes(x = LON, y = LAT), color = "#0072B2", size=.5, alpha = 1, show.legend = FALSE) +
  geom_point(data = VTR_GoM_points2_inside2, aes(x = LON, y = LAT), color = "#E69F00", size=.5, alpha = 1, show.legend = FALSE) +
  coord_sf(xlim = c(-73, -66), ylim = c(41, 44.7), crs = sf::st_crs(4326)) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey60", "white")
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20"    )
  )

# Create convex hull of VTR points inside the stock area and within the 95th percentile of distance to the centroid of recorded lat-lons
lat_lon_hulls <- VTR_GoM_points2_inside2 %>%
  slice(chull(LAT, LON))

# Create convex lat_lon_hulls shapefile
hull_poly <- lat_lon_hulls %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

hull_poly<-hull_poly %>% 
  dplyr::mutate(area="VTR_convex_hull")

hull_poly2 = data.frame(st_coordinates(st_cast(hull_poly$geometry,"POINT")))
hull_poly2<-hull_poly2 %>% 
  dplyr::rename(LON=X, LAT=Y) %>% 
  dplyr::mutate(area="VTR_convex_hull")

ggplot(east_coast) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey40", alpha = 0.3, color = "black") +
  geom_point(data = VTR_GoM_points2_inside3, aes(x = LON, y = LAT), color = "#0072B2", size=.5, alpha = 1, show.legend = FALSE) +
  geom_point(data = VTR_GoM_points2_inside2, aes(x = LON, y = LAT), color = "#E69F00", size=.5, alpha = 1, show.legend = FALSE) +
  geom_polygon(data = hull_poly2, aes(x = LON, y = LAT,group = area), color = "#E69F00", alpha = .0001, show.legend = FALSE) +
  coord_sf(xlim = c(-73, -66), ylim = c(41, 44.7), crs = sf::st_crs(4326)) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey60", "white")
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20"    )
  )


#
hull_poly3<-hull_poly2[!duplicated(hull_poly2), ] #remove duplicates points from convex hull

VTR_boundaries_subset<- st_as_sf(hull_poly3, coords = c('LON' ,'LAT' )) 
VTR_boundaries_subset <- VTR_boundaries_subset %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>% 
  mutate(area="VTR_convex_hull")

st_crs(VTR_boundaries_subset)= 4326

st_crs(sp_points)= 4326

VTR_boundaries_subset=st_transform(VTR_boundaries_subset,st_crs(sp_points)) # Match the point and polygon CRS

VTR_boundaries2 <- sf::st_join(sp_points, VTR_boundaries_subset)

# Save convex hull footprint (to share instead of confidential VTR points)
# Object to save: VTR_boundaries_subset (sf POLYGON)

# choose an output folder (inside your project directory)
out_dir <- file.path(cd, "derived_shapefiles")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# make sure geometry is valid before writing
VTR_boundaries_subset <- sf::st_make_valid(VTR_boundaries_subset)

# ensure a clean attribute field (shapefiles like short names)
VTR_boundaries_subset <- VTR_boundaries_subset %>%
  dplyr::mutate(area = as.character(area)) %>%
  dplyr::select(area, geometry)

# save derived convex hull 
gpkg_path <- file.path(out_dir, "VTR_convex_hull.gpkg")
if (file.exists(gpkg_path)) file.remove(gpkg_path)

sf::st_write(
  obj = VTR_boundaries_subset,
  dsn = gpkg_path,
  layer = "VTR_convex_hull",
  driver = "GPKG",
  append = FALSE
)


### Section 2) Extract bottom temperatures inside convex hull ###

# Load the derived convex hull footprint
hull_gpkg <- file.path(cd, "derived_shapefiles", "VTR_convex_hull.gpkg")

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

# Pull in the bottom temp data
ncpath <- cd
ncname <- "bottom_temp_combined_product_1959_2022_soe"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "sea_water_temperature_at_sea_floor"  # note: tmp means temperature (not temporary)

ncin <- nc_open(ncfname)
print(ncin)

ncin

# get longitude and latitude
lon <- ncvar_get(ncin,"longitude")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"latitude")
nlat <- dim(lat)
head(lat)

# get time
time <- ncvar_get(ncin, "year")
tunits <- ncatt_get(ncin,"year","units")
nt <- dim(time)
nt

# get temperature
tmp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)

head(tmp_array)
head(time)

# get global attributes
title <- ncatt_get(ncin,0,"title")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
history <- ncatt_get(ncin,0,"history")
Conventions <- ncatt_get(ncin,0,"Conventions")

#Loop through years to get bt_tmp by month. Then combine all the years into one dataset
lat_lons = list()
lat_lons_all = list()


for (m in 1:dim(time)){
  dim(tmp_array)
  
  #m <- 1 #get a single slice or layer (year m)
  tmp_slice <- tmp_array[,,,m]
  dim(tmp_slice)
  
  # # levelplot of the slice
  # grid <- expand.grid(lon=lon, lat=lat)
  #  levelplot(tmp_slice ~ lon * lat, data=grid,   pretty=T, 
  #            col.regions=(rev(brewer.pal(10,"RdBu"))))
  
  
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
  head(tmp_vec_long)
  
  #tmp_vec_long<-na.omit(tmp_vec_long)
  # reshape the vector into a matrix
  tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat, ncol=366) #one column for each day +1 to align with data
  dim(tmp_mat)
  
  head(na.omit(tmp_mat))
  
  # create a dataframe
  lonlat <- as.matrix(expand.grid(lon,lat))
  tmp_df02 <- data.frame(cbind(lonlat,tmp_mat))
  
  #Reshape the columns to days, noting that the first two are the lon lats
  tmp_df02_long <- tmp_df02 %>% 
    pivot_longer(
      cols = 'V3':'V368', 
      names_to = "day",
      values_to = "value"
    )
  
  
  tmp_df02_long<-na.omit(tmp_df02_long) #remove lon-lats with no bt_data
  tmp_df02_long$day<-substring(tmp_df02_long$day, 2) #extract the day of year
  tmp_df02_long$day<-as.numeric(tmp_df02_long$day) 
  tmp_df02_long$day<-tmp_df02_long$day-3 #subtract 3 from day because there were two extra columns for lat and lon 
  # in the reshape, and in the code below, as.date starts at 0 
  y<-1958+m
  tmp_df02_long$date<-as.Date(tmp_df02_long$day, origin = paste0(y, "-01-01")) # day of the year to date 
  tmp_df02_long$month <- format(tmp_df02_long$date, "%m") #extract the month 
  tmp_df02_long$year <-  1958+m
  
  tmp_df02_long<- tmp_df02_long %>% 
    dplyr::rename(lon=Var1, lat=Var2, bt_tmp=value) %>% 
    dplyr::select("lon","lat","bt_tmp", "date")   
  
  sp_points = st_as_sf(tmp_df02_long,coords = c('lon' ,'lat' ))#make points spatial
  st_crs(sp_points)= 4326
  
  VTR_boundaries_subset=st_transform(VTR_boundaries_subset,st_crs(sp_points)) # Match the point and polygon CRS
  
  VTR_boundaries2 <- sf::st_join(sp_points, VTR_boundaries_subset)
  
  VTR_boundaries2_inside <- VTR_boundaries2 %>% 
    dplyr::filter(!is.na(area))
  
  lat_lons[[m]] <- VTR_boundaries2_inside
  
}
lat_lons_all= rbindlist(lat_lons, fill=TRUE)
ls(lat_lons_all)

# Compute mean bottom temperature by day 
lat_lons_all_agg<-lat_lons_all %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarise(mean_tmp=mean(bt_tmp), sd_tmp=sd(bt_tmp), .groups="drop")

write_xlsx(lat_lons_all_agg,paste0(cd,"hist_daily_bt_tmps_agg_GoM.xlsx"))






