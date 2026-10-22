
# load packages 
pkgs_to_use <- c("data.table",
                 "dplyr",
                 "ncdf4",
                 "sf",
                 "tidyr",
                 "writexl")

lapply(pkgs_to_use, library, character.only = TRUE, quietly = TRUE)

# Data required: 
# Gom cod stock areas from NOAA public use shapefiles
# Archived in this repository and available at:  
# https://www.fisheries.noaa.gov/resources/maps?title=&tid%5B1000001111%5D=1000001111&field_species_vocab_target_id=&sort_by=created 

# lat-lons of fishing locations from for-hire fishing vessels that reported catching cod or haddock -
# Not publicly available due to NOAA confidentiality restrictions.
# Derived convex hulls included in this repository. To replicate, start at section 2) 

# Historical bottom temperature data product **FOR 1959-2022**
# NetCDF file included in the repository.

# OpenAI ChatGPT (GPT-5.6 Sol) was used to optimize this code. 

# Set global directories - user edit here
project_dir<-"C:/Users/andrew.carr-harris/Desktop/Git/Climate-and-Catch-Dependence-in-Rec.-Fisheries"

process_data_dir <- file.path(
  project_dir,
  "data"
)

stock_area_dir <- file.path(
  project_dir,
  "data",
  "stock_areas"
)

figure_dir <- file.path(
  project_dir,
  "outputs",
  "figures"
)

bottom_temp_file <- file.path(process_data_dir,"bottom_temp_combined_product_1959_2022_soe.nc")
bottom_temp_output_xlsx <- file.path(process_data_dir, "daily_btemp_GoM_by_area_1959_2022.xlsx")

### Section 1) Generate convex hull of fishing activity based on for-hire vessel trip reports

# Restricted-data processing retained for documentation only.
# This section is not part of the public reproducibility workflow and cannot 
# be run with public repo data; use the provided VTR_convex_hull.gpkg and begin at Section 2.

# PROJ.USE = CRS('+proj=aea +lat_1=28 +lat_2=42 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 ')
# 
# # GoM cod stock areas
# GoM_shape <- st_read(dsn=paste0(cd, "/Stock_Areas"),
#                       layer="Stock_Areas")
#  
#  GoM_shape <-GoM_shape %>%
#    dplyr::filter(AREANAME  %in% c("GOM Cod Stock Area"))
# 
# # East Coast US state map for visualization
# states <- map_data("state")
# east_coast <- subset(states, region %in% c("maine", "massachusetts", "vermont", "new hampshire","rhode island", "connecticut", "new york", "pennsylvania",
#                                            "new jersey", "maryland", "delaware", "district of columbia", "virginia", "west virginia", "north carolina"))
# 
# #VTR lat-lons - not publicly available
# #Create a "fishing footprint" (convex hull) based on VTR points for all years combined
# vtr_lat_lons_cod<-read_csv(paste0(cd, "Yearly_Cod_Recreational_Lat_Lon.csv"), show_col_types = FALSE)
# vtr_lat_lons_hadd<-read_csv(paste0(cd, "Yearly_Haddock_Recreational_Lat_Lon.csv"), show_col_types = FALSE)
# 
# vtr_lat_lons_both<-rbind(vtr_lat_lons_cod, vtr_lat_lons_hadd)
# vtr_lat_lons_both<-vtr_lat_lons_both %>% 
#   dplyr::mutate(Species="BOTH")
# 
# lat_lons_ready <- vtr_lat_lons_both %>%
#   dplyr::select(Species, LAT, LON, YEAR) %>%
#   drop_na()
# 
# # Compute the mean lat/lon
# lat_lons_ready <- lat_lons_ready %>%  
#   group_by(Species) %>%
#   mutate(mean_lat = mean(LAT))  %>%
#   mutate(mean_lon = mean(LON)) %>% 
#   ungroup()
# 
# # Compute the distance of each lat/lon to the centroid 
# library(geosphere)
# lat_lons_ready$distance<-distGeo(lat_lons_ready[,2:3], lat_lons_ready[,5:6])
# 
# ggplot(east_coast) +
#   geom_polygon(aes(x = long, y = lat, group = group), fill = "grey40", alpha = 0.3, color = "black") +
#   geom_point(data = lat_lons_ready, aes(x = LON, y = LAT), color = "red", alpha = 0.3, show.legend = FALSE) +
#   geom_sf(data = GoM_shape['AREANAME'], fill = "blue", alpha = 0.3, show.legend = FALSE) +
#   coord_sf(xlim = c(-74.6, -66), ylim = c(40, 44.7)) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         plot.title = element_text(hjust = 0.5)) +
#   ggspatial::annotation_scale(
#     location = "br",
#     bar_cols = c("grey60", "white")
#   ) +
#   ggspatial::annotation_north_arrow(
#     location = "tl", which_north = "true",
#     pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
#     style = ggspatial::north_arrow_nautical(
#       fill = c("grey40", "white"),
#       line_col = "grey20"    )
#   )
# 
# 
# # Before taking the 95th percentile of distance to the centoid and creating the convex hull, keep only VTR lat lons falling within the stock area
# mgt_boundaries_subset<- st_as_sf(GoM_shape) 
# mgt_boundaries_subset<-mgt_boundaries_subset %>% 
#   dplyr::select(geometry, COMMNAME) 
# 
# sp_points = st_as_sf(lat_lons_ready,coords = c('LON' ,'LAT' )) # make points spatial
# st_crs(sp_points)= 4326
# 
# mgt_boundaries_subset=st_transform(mgt_boundaries_subset,st_crs(sp_points)) # Match the point and polygon CRS
# 
# VTR_GoM_points2 <- sf::st_join(sp_points, mgt_boundaries_subset)
# 
# # allocate spatial points as outside the GOM if the COMMNAME is missing 
# VTR_GoM_points2_outside <- VTR_GoM_points2 %>% 
#   dplyr::filter(is.na(COMMNAME))
# VTR_GoM_points2_outside = data.frame(st_coordinates(st_cast(VTR_GoM_points2_outside$geometry,"POINT")))
# VTR_GoM_points2_outside<-VTR_GoM_points2_outside %>% 
#   dplyr::rename(LON=X, LAT=Y)
# 
# # allocate spatial points as inside the GOM if the COMMNAME is not missing 
# VTR_GoM_points2_inside <- VTR_GoM_points2 %>% 
#   dplyr::filter(!is.na(COMMNAME))
# VTR_GoM_points2_inside = data.frame(st_coordinates(st_cast(VTR_GoM_points2_inside$geometry,"POINT")))
# VTR_GoM_points2_inside<-VTR_GoM_points2_inside %>% 
#   dplyr::rename(LON=X, LAT=Y)
# 
# # check to make sure points are correctly allocated within the stock area 
# ggplot(east_coast) +
#   geom_polygon(aes(x = long, y = lat, group = group), fill = "grey40", alpha = 0.3, color = "black") +
#   geom_point(data = VTR_GoM_points2_outside, aes(x = LON, y = LAT), color = "red", alpha = 0.3, show.legend = FALSE) +
#   geom_point(data = VTR_GoM_points2_inside, aes(x = LON, y = LAT), color = "black", alpha = 0.3, show.legend = FALSE) +
#   geom_sf(data = GoM_shape['AREANAME'], fill = "blue", alpha = 0.3, show.legend = FALSE) +
#   coord_sf(xlim = c(-74.6, -66), ylim = c(40, 44.7)) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         plot.title = element_text(hjust = 0.5)) +
#   ggspatial::annotation_scale(
#     location = "br",
#     bar_cols = c("grey60", "white")
#   ) +
#   ggspatial::annotation_north_arrow(
#     location = "tl", which_north = "true",
#     pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
#     style = ggspatial::north_arrow_nautical(
#       fill = c("grey40", "white"),
#       line_col = "grey20"    )
#   )
# 
# 
# # use only VTR lat-lons within the GoM to make convex hull
# VTR_GoM_points2_inside <- VTR_GoM_points2_inside %>%  
#   mutate(LON_mean = mean(LON))  %>%
#   mutate(LAT_mean = mean(LAT)) 
# 
# # Calculate the distance of each lat/lon to the centroid and 95th percentile 
# library(geosphere)
# VTR_GoM_points2_inside$distance<-distGeo(VTR_GoM_points2_inside[,1:2], VTR_GoM_points2_inside[,3:4])
# 
# VTR_GoM_points2_inside <- VTR_GoM_points2_inside %>%  
#   mutate(q95 = quantile(distance, 0.95)) 
# 
# VTR_GoM_points2_inside2 <- VTR_GoM_points2_inside %>%  
#   dplyr::filter(distance<=q95)
# 
# VTR_GoM_points2_inside3 <- VTR_GoM_points2_inside %>%  
#   dplyr::filter(distance>q95)
# 
# ggplot(east_coast) +
#   geom_polygon(aes(x = long, y = lat, group = group), fill = "grey40", alpha = 0.3, color = "black") +
#   geom_point(data = VTR_GoM_points2_inside3, aes(x = LON, y = LAT), color = "#0072B2", size=.5, alpha = 1, show.legend = FALSE) +
#   geom_point(data = VTR_GoM_points2_inside2, aes(x = LON, y = LAT), color = "#E69F00", size=.5, alpha = 1, show.legend = FALSE) +
#   coord_sf(xlim = c(-73, -66), ylim = c(41, 44.7), crs = sf::st_crs(4326)) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         plot.title = element_text(hjust = 0.5)) +
#   ggspatial::annotation_scale(
#     location = "br",
#     bar_cols = c("grey60", "white")
#   ) +
#   ggspatial::annotation_north_arrow(
#     location = "tl", which_north = "true",
#     pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
#     style = ggspatial::north_arrow_nautical(
#       fill = c("grey40", "white"),
#       line_col = "grey20"    )
#   )
# 
# # Create convex hull of VTR points inside the stock area and within the 95th percentile of distance to the centroid of recorded lat-lons
# lat_lon_hulls <- VTR_GoM_points2_inside2 %>%
#   slice(chull(LAT, LON))
# 
# # Create convex lat_lon_hulls shapefile
# hull_poly <- lat_lon_hulls %>%
#   st_as_sf(coords = c("LON", "LAT"), crs = 4326) %>%
#   summarise(geometry = st_combine(geometry)) %>%
#   st_cast("POLYGON")
# 
# hull_poly<-hull_poly %>% 
#   dplyr::mutate(area="VTR_convex_hull")
# 
# hull_poly2 = data.frame(st_coordinates(st_cast(hull_poly$geometry,"POINT")))
# hull_poly2<-hull_poly2 %>% 
#   dplyr::rename(LON=X, LAT=Y) %>% 
#   dplyr::mutate(area="VTR_convex_hull")
# 
# ggplot(east_coast) +
#   geom_polygon(aes(x = long, y = lat, group = group), fill = "grey40", alpha = 0.3, color = "black") +
#   geom_point(data = VTR_GoM_points2_inside3, aes(x = LON, y = LAT), color = "#0072B2", size=.5, alpha = 1, show.legend = FALSE) +
#   geom_point(data = VTR_GoM_points2_inside2, aes(x = LON, y = LAT), color = "#E69F00", size=.5, alpha = 1, show.legend = FALSE) +
#   geom_polygon(data = hull_poly2, aes(x = LON, y = LAT,group = area), color = "#E69F00", alpha = .0001, show.legend = FALSE) +
#   coord_sf(xlim = c(-73, -66), ylim = c(41, 44.7), crs = sf::st_crs(4326)) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         plot.title = element_text(hjust = 0.5)) +
#   ggspatial::annotation_scale(
#     location = "br",
#     bar_cols = c("grey60", "white")
#   ) +
#   ggspatial::annotation_north_arrow(
#     location = "tl", which_north = "true",
#     pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
#     style = ggspatial::north_arrow_nautical(
#       fill = c("grey40", "white"),
#       line_col = "grey20"    )
#   )
# 
# 
# #
# hull_poly3<-hull_poly2[!duplicated(hull_poly2), ] #remove duplicates points from convex hull
# 
# VTR_boundaries_subset<- st_as_sf(hull_poly3, coords = c('LON' ,'LAT' )) 
# VTR_boundaries_subset <- VTR_boundaries_subset %>%
#   st_as_sf(coords = c("LON", "LAT"), crs = 4326) %>%
#   summarise(geometry = st_combine(geometry)) %>%
#   st_cast("POLYGON") %>% 
#   mutate(area="VTR_convex_hull")
# 
# st_crs(VTR_boundaries_subset)= 4326
# 
# st_crs(sp_points)= 4326
# 
# VTR_boundaries_subset=st_transform(VTR_boundaries_subset,st_crs(sp_points)) # Match the point and polygon CRS
# 
# VTR_boundaries2 <- sf::st_join(sp_points, VTR_boundaries_subset)
# 
# # Save convex hull footprint (to share instead of confidential VTR points)
# # Object to save: VTR_boundaries_subset (sf POLYGON)
# 
# # choose an output folder (inside your project directory)
# out_dir <- file.path(cd, "derived_shapefiles")
# if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
# 
# # make sure geometry is valid before writing
# VTR_boundaries_subset <- sf::st_make_valid(VTR_boundaries_subset)
# 
# # ensure a clean attribute field (shapefiles like short names)
# VTR_boundaries_subset <- VTR_boundaries_subset %>%
#   dplyr::mutate(area = as.character(area)) %>%
#   dplyr::select(area, geometry)
# 
# # save derived convex hull 
# gpkg_path <- file.path(out_dir, "VTR_convex_hull.gpkg")
# if (file.exists(gpkg_path)) file.remove(gpkg_path)
# 
# sf::st_write(
#   obj = VTR_boundaries_subset,
#   dsn = gpkg_path,
#   layer = "VTR_convex_hull",
#   driver = "GPKG",
#   append = FALSE
# )

### Section 2) Extract daily bottom temperatures inside convex hull
hull_gpkg <- file.path(process_data_dir, "VTR_convex_hull.gpkg")

if (!file.exists(hull_gpkg)) {
  stop(
    "Convex-hull GeoPackage not found at:\n",
    hull_gpkg
  )
}

if (!file.exists(bottom_temp_file)) {
  stop(
    "Bottom-temperature NetCDF file not found at:\n",
    bottom_temp_file
  )
}

# Read spatial fishing footprint data
VTR_boundaries_subset <- sf::st_read(
  dsn = hull_gpkg,
  layer = "VTR_convex_hull",
  quiet = TRUE
)

VTR_boundaries_subset <- sf::st_make_valid(
  VTR_boundaries_subset
)

if (is.na(sf::st_crs(VTR_boundaries_subset))) {
  
  warning(
    "The convex-hull file has no CRS. ",
    "Assigning EPSG:4326 under the assumption that coordinates are longitude/latitude."
  )
  
  sf::st_crs(VTR_boundaries_subset) <- 4326
  
} else {
  
  VTR_boundaries_subset <- sf::st_transform(
    VTR_boundaries_subset,
    4326
  )
}

VTR_footprint <- sf::st_union(
  VTR_boundaries_subset
)

# Split fishing footprint into inshore/offshore
inshore_distance_m <- 3 * 1852

# Coastline state polygons
northeast_land <- sf::st_as_sf(
  maps::map(
    database = "state",
    regions = c(
      "maine",
      "new hampshire",
      "massachusetts"
    ),
    plot = FALSE,
    fill = TRUE
  )
)

sf::st_crs(northeast_land) <- 4326

northeast_land <- northeast_land |>
  sf::st_make_valid() |>
  sf::st_union()

distance_crs <- 26919

land_projected <- sf::st_transform(
  northeast_land,
  distance_crs
)

footprint_projected <- sf::st_transform(
  VTR_footprint,
  distance_crs
)

coastal_buffer_projected <- sf::st_buffer(
  land_projected,
  dist = inshore_distance_m
)

inshore_footprint_projected <- sf::st_intersection(
  footprint_projected,
  coastal_buffer_projected
)

offshore_footprint_projected <- sf::st_difference(
  footprint_projected,
  coastal_buffer_projected
)

inshore_footprint <- sf::st_transform(
  inshore_footprint_projected,
  4326
)

offshore_footprint <- sf::st_transform(
  offshore_footprint_projected,
  4326
)

footprint_bbox <- sf::st_bbox(
  VTR_footprint
)

message(
  "Fishing-footprint bounding box:\n",
  "  Longitude: ",
  round(footprint_bbox["xmin"], 4),
  " to ",
  round(footprint_bbox["xmax"], 4),
  "\n  Latitude: ",
  round(footprint_bbox["ymin"], 4),
  " to ",
  round(footprint_bbox["ymax"], 4)
)


# Bottom temperature data
ncin <- ncdf4::nc_open(
  bottom_temp_file
)

lat_lons_all_agg <- tryCatch({
  
  print(ncin)
  
  message(
    "NetCDF dimensions: ",
    paste(names(ncin$dim), collapse = ", ")
  )
  
  message(
    "NetCDF variables: ",
    paste(names(ncin$var), collapse = ", ")
  )
  
  lon_name <- "longitude"
  lat_name <- "latitude"
  day_name <- "day"
  year_name <- "year"
  dname <- "sea_water_temperature_at_sea_floor"
  
  required_dimensions <- c(
    lon_name,
    lat_name,
    day_name,
    year_name
  )
  
  available_dims <- names(ncin$dim)
  missing_dims <- setdiff(required_dimensions, available_dims)
  
  if (length(missing_dims) > 0L) {
    stop(
      "The following required NetCDF dimensions were not found: ",
      paste(missing_dims, collapse = ", "),
      "\nAvailable dimensions are: ",
      paste(available_dims, collapse = ", ")
    )
  }
  
  missing_dimensions <- setdiff(
    required_dimensions,
    names(ncin$dim)
  )
  
  if (length(missing_dimensions) > 0L) {
    stop(
      "The following required NetCDF dimensions were not found: ",
      paste(missing_dimensions, collapse = ", "),
      "\nAvailable dimensions are: ",
      paste(names(ncin$dim), collapse = ", ")
    )
  }
  
  if (!dname %in% names(ncin$var)) {
    stop(
      "The expected bottom-temperature variable '",
      dname,
      "' was not found.\nAvailable variables are: ",
      paste(names(ncin$var), collapse = ", ")
    )
  }
  
  # Read longitude, latitude, day, and year
  lon_native <- ncdf4::ncvar_get(ncin, lon_name)
  lat <- ncdf4::ncvar_get(ncin, lat_name)
  day_values <- ncdf4::ncvar_get(ncin, day_name)
  year_values <- ncdf4::ncvar_get(ncin, year_name)
  
  message("Number of longitude coordinates: ", length(lon_native))
  message("Number of latitude coordinates: ", length(lat))
  message("Number of day positions per year: ", length(day_values))
  message("Number of years: ", length(year_values))
  
  lon_spatial <- lon_native
  
  if (any(lon_native > 180, na.rm = TRUE)) {
    message(
      "NetCDF longitude uses a 0-to-360 convention. ",
      "Converting coordinates to -180 to 180 for spatial operations."
    )
    
    lon_spatial <- ifelse(
      lon_native > 180,
      lon_native - 360,
      lon_native
    )
  }
  
  message(
    "NetCDF longitude range: ",
    round(min(lon_spatial, na.rm = TRUE), 4),
    " to ",
    round(max(lon_spatial, na.rm = TRUE), 4)
  )
  
  message(
    "NetCDF latitude range: ",
    round(min(lat, na.rm = TRUE), 4),
    " to ",
    round(max(lat, na.rm = TRUE), 4)
  )
  
  dates_by_year <- lapply(
    as.integer(year_values),
    function(current_year) {
      candidate_dates <- as.Date(
        paste0(current_year, "-01-01")
      ) + seq_along(day_values) - 1L
      
      candidate_dates[
        as.integer(format(candidate_dates, "%Y")) == current_year
      ]
    }
  )
  
  dates <- do.call(
    c,
    dates_by_year
  )
  
  message(
    "Date range: ",
    min(dates),
    " through ",
    max(dates)
  )
  
  message(
    "Number of valid calendar dates: ",
    length(dates)
  )
  
  if (anyNA(dates)) {
    stop("One or more dates could not be constructed.")
  }
  
  if (anyDuplicated(dates)) {
    stop("The constructed date vector contains duplicate dates.")
  }
  
  
  # Read temperature metadata
  temp_units_attribute <- ncdf4::ncatt_get(
    ncin,
    dname,
    "units"
  )
  
  temp_units <- if (isTRUE(temp_units_attribute$hasatt)) {
    as.character(temp_units_attribute$value)
  } else {
    NA_character_
  }
  
  temp_long_name_attribute <- ncdf4::ncatt_get(
    ncin,
    dname,
    "long_name"
  )
  
  temp_long_name <- if (isTRUE(temp_long_name_attribute$hasatt)) {
    as.character(temp_long_name_attribute$value)
  } else {
    dname
  }
  
  message("Temperature variable: ", dname)
  message("Temperature description: ", temp_long_name)
  message("Temperature units: ", temp_units)
  
  temp_dimension_names <- vapply(
    ncin$var[[dname]]$dim,
    function(x) x$name,
    character(1)
  )
  
  temp_dimension_lengths <- vapply(
    ncin$var[[dname]]$dim,
    function(x) x$len,
    numeric(1)
  )
  
  temp_dimension_table <- data.frame(
    dimension = temp_dimension_names,
    length = temp_dimension_lengths
  )
  
  print(temp_dimension_table)
  
  if (!all(
    c(lon_name, lat_name, day_name, year_name) %in%
    temp_dimension_names
  )) {
    stop(
      "The temperature variable does not contain all four expected dimensions: ",
      paste(c(lon_name, lat_name, day_name, year_name), collapse = ", "),
      ".\nIts dimensions are: ",
      paste(temp_dimension_names, collapse = ", ")
    )
  }
  
  if (length(temp_dimension_names) != 4L) {
    stop(
      "The temperature variable has ",
      length(temp_dimension_names),
      " dimensions. This code expects exactly four: ",
      "longitude, latitude, day, and year."
    )
  }
  
  lon_position <- match(
    lon_name,
    temp_dimension_names
  )
  
  lat_position <- match(
    lat_name,
    temp_dimension_names
  )
  
  day_position <- match(
    day_name,
    temp_dimension_names
  )
  
  year_position <- match(
    year_name,
    temp_dimension_names
  )
  
  lon_index <- which(
    lon_spatial >= unname(footprint_bbox["xmin"]) &
      lon_spatial <= unname(footprint_bbox["xmax"])
  )
  
  lat_index <- which(
    lat >= unname(footprint_bbox["ymin"]) &
      lat <= unname(footprint_bbox["ymax"])
  )
  
  if (length(lon_index) == 0L) {
    stop(
      "No NetCDF longitude coordinates overlap the footprint bounding box."
    )
  }
  
  if (length(lat_index) == 0L) {
    stop(
      "No NetCDF latitude coordinates overlap the footprint bounding box."
    )
  }
  
  lon_read_index <- seq.int(
    min(lon_index),
    max(lon_index)
  )
  
  lat_read_index <- seq.int(
    min(lat_index),
    max(lat_index)
  )
  
  lon_subset <- lon_spatial[
    lon_read_index
  ]
  
  lat_subset <- lat[
    lat_read_index
  ]
  
  message(
    "Bounding-box NetCDF subset contains ",
    length(lon_subset),
    " longitude coordinates and ",
    length(lat_subset),
    " latitude coordinates."
  )
  

  grid_subset <- expand.grid(
    lon = lon_subset,
    lat = lat_subset,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  
  lon_resolution <- stats::median(abs(diff(sort(unique(lon_subset)))))
  lat_resolution <- stats::median(abs(diff(sort(unique(lat_subset)))))
  
  lon_half_width <- lon_resolution / 2
  lat_half_width <- lat_resolution / 2
  
  grid_cell_geometry <- sf::st_sfc(
    lapply(
      seq_len(nrow(grid_subset)),
      function(i) {
        x <- grid_subset$lon[i]
        y <- grid_subset$lat[i]
        
        sf::st_polygon(
          list(
            matrix(
              c(
                x - lon_half_width, y - lat_half_width,
                x + lon_half_width, y - lat_half_width,
                x + lon_half_width, y + lat_half_width,
                x - lon_half_width, y + lat_half_width,
                x - lon_half_width, y - lat_half_width
              ),
              ncol = 2,
              byrow = TRUE
            )
          )
        )
      }
    ),
    crs = 4326
  )
  
  grid_cells <- sf::st_sf(
    grid_id = seq_len(nrow(grid_subset)),
    lon = grid_subset$lon,
    lat = grid_subset$lat,
    geometry = grid_cell_geometry
  )
  
  grid_cells_projected <- sf::st_transform(
    grid_cells,
    distance_crs
  )
  
  # Determine which complete cells overlap the fishing footprint.
  inside_footprint <- lengths(
    sf::st_intersects(
      grid_cells_projected,
      footprint_projected
    )
  ) > 0L
  
  # A cell is inshore if any portion of it overlaps the part of the
  # fishing footprint within three miles
  inshore_cell <- inside_footprint &
    lengths(
      sf::st_intersects(
        grid_cells_projected,
        inshore_footprint_projected
      )
    ) > 0L
  
  # All other fishing-footprint cells are assigned offshore.
  offshore_cell <- inside_footprint & !inshore_cell
  

  product_label <- "Bottom-temperature product, 1959–2022"
  
  selected_grid_cells <- grid_cells |>
    dplyr::mutate(
      temperature_area = dplyr::case_when(
        inshore_cell ~ "Inshore",
        offshore_cell ~ "Offshore",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(temperature_area)) |>
    dplyr::mutate(
      temperature_area = factor(
        temperature_area,
        levels = c("Inshore", "Offshore")
      )
    )
  
  selected_cell_counts <- selected_grid_cells |>
    sf::st_drop_geometry() |>
    dplyr::count(temperature_area, name = "n_selected_cells")
  
  print(selected_cell_counts)
  
  map_bbox <- sf::st_bbox(selected_grid_cells)
  
  selected_cell_map <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = northeast_land,
      fill = "grey85",
      color = "grey55",
      linewidth = 0.2
    ) +
    ggplot2::geom_sf(
      data = selected_grid_cells,
      ggplot2::aes(fill = temperature_area),
      color = "grey20",
      linewidth = 0.12,
      alpha = 0.75
    ) +
    ggplot2::geom_sf(
      data = VTR_footprint,
      fill = NA,
      color = "black",
      linewidth = 0.7
    ) +
    ggplot2::geom_sf(
      data = inshore_footprint,
      fill = NA,
      color = "#0072B2",
      linewidth = 0.8,
      linetype = "dashed"
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Inshore" = "#0072B2",
        "Offshore" = "#E69F00"
      ),
      drop = FALSE
    ) +
    ggplot2::coord_sf(
      xlim = unname(map_bbox[c("xmin", "xmax")]) + c(-0.15, 0.15),
      ylim = unname(map_bbox[c("ymin", "ymax")]) + c(-0.15, 0.15),
      expand = FALSE
    ) +
    ggplot2::labs(
      title = product_label,
      subtitle = paste0(
        "Exact NetCDF cells assigned using polygon overlap\n",
        "Dashed blue line: inshore footprint; black line: complete fishing footprint"
      ),
      fill = "Temperature area",
      x = "Longitude",
      y = "Latitude"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(
        color = "grey90",
        linewidth = 0.2
      ),
      legend.position = "right"
    )
  
  print(selected_cell_map)
  
  
  n_bbox_cells <- nrow(grid_subset)
  n_inside_cells <- sum(inside_footprint)
  n_inshore_cells <- sum(inshore_cell)
  n_offshore_cells <- sum(offshore_cell)
  
  message(
    n_inside_cells,
    " of ",
    n_bbox_cells,
    " bounding-box grid-cell centers fall inside the fishing footprint."
  )
  
  message(
    "Selected cells by area:\n",
    "  Inshore: ",
    n_inshore_cells,
    "\n  Offshore: ",
    n_offshore_cells
  )
  
  if (n_inside_cells == 0L) {
    stop(
      "No NetCDF grid-cell centers fall inside the fishing footprint. ",
      "Check the footprint CRS and longitude convention."
    )
  }
  
  if (n_inshore_cells == 0L) {
    stop(
      "No selected GLORYS grid-cell centers were classified as inshore."
    )
  }
  
  if (n_offshore_cells == 0L) {
    stop(
      "No selected GLORYS grid-cell centers were classified as offshore."
    )
  }
  
  n_bbox_cells <- nrow(grid_subset)
  n_inside_cells <- sum(inside_footprint)
  
  message(
    n_inside_cells,
    " of ",
    n_bbox_cells,
    " bounding-box grid-cell centers fall inside the fishing footprint."
  )
  
  if (n_inside_cells == 0L) {
    stop(
      "No NetCDF grid-cell centers fall inside the fishing footprint. ",
      "Check the footprint CRS and longitude convention."
    )
  }
  
    # Optional diagnostic grid
  grid_diagnostic <- grid_subset |>
    dplyr::mutate(
      inside_footprint = inside_footprint,
      inshore_cell = inshore_cell,
      offshore_cell = offshore_cell,
      temperature_area = dplyr::case_when(
        inshore_cell ~ "Inshore",
        offshore_cell ~ "Offshore",
        TRUE ~ "Outside footprint"
      )
    )
  
  print(
    grid_diagnostic |>
      dplyr::count(
        temperature_area
      )
  )
  
  
  
  # Function to read one block of daily temperatures
  summarize_temperature_area <- function(temperature_matrix, cell_selector, area_name) {
    
    temperature_subset <- temperature_matrix[
      cell_selector,
      ,
      drop = FALSE
    ]
    
    n_valid_cells <- colSums(!is.na(temperature_subset))
    mean_temperature <- colMeans(temperature_subset, na.rm = TRUE)
    
    sd_temperature <- apply(
      temperature_subset,
      2,
      function(x) if (sum(!is.na(x)) <= 1L) NA_real_ else stats::sd(x, na.rm = TRUE)
    )
    
    min_temperature <- apply(
      temperature_subset,
      2,
      function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
    )
    
    max_temperature <- apply(
      temperature_subset,
      2,
      function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
    )
    
    mean_temperature[n_valid_cells == 0L] <- NA_real_
    
    output <- data.frame(
      mean_tmp = mean_temperature,
      sd_tmp = sd_temperature,
      min_tmp = min_temperature,
      max_tmp = max_temperature,
      n_cells = n_valid_cells
    )
    
    names(output) <- paste0(
      names(output),
      "_",
      area_name
    )
    
    output
  }
  
  read_temperature_year <- function(year_index) {
    
    start_values <- rep(
      1L,
      length(temp_dimension_names)
    )
    
    count_values <- as.integer(
      temp_dimension_lengths
    )
    
    start_values[lon_position] <- min(lon_read_index)
    count_values[lon_position] <- length(lon_read_index)
    
    start_values[lat_position] <- min(lat_read_index)
    count_values[lat_position] <- length(lat_read_index)
    
    start_values[day_position] <- 1L
    count_values[day_position] <- length(day_values)
    
    start_values[year_position] <- year_index
    count_values[year_position] <- 1L
    
    temp_year_native <- ncdf4::ncvar_get(
      nc = ncin,
      varid = dname,
      start = start_values,
      count = count_values,
      collapse_degen = FALSE
    )
    
    # Reorder to longitude × latitude × day × year.
    permutation <- match(
      c(lon_name, lat_name, day_name, year_name),
      temp_dimension_names
    )
    
    temp_year <- aperm(
      temp_year_native,
      permutation
    )
    
    expected_dimensions <- c(
      length(lon_subset),
      length(lat_subset),
      length(day_values),
      1L
    )
    
    if (!identical(
      as.integer(dim(temp_year)),
      as.integer(expected_dimensions)
    )) {
      stop(
        "Unexpected annual temperature-array dimensions.\n",
        "Expected: ",
        paste(expected_dimensions, collapse = " × "),
        "\nReceived: ",
        paste(dim(temp_year), collapse = " × ")
      )
    }
    
    temp_year <- temp_year[, , , 1L, drop = TRUE]
    
    temp_year[abs(temp_year) > 100] <- NA_real_
    
    temp_year
  }
  

  # Process one calendar year at a time
  daily_temperature_chunks <- vector(
    mode = "list",
    length = length(year_values)
  )
  
  for (year_index in seq_along(year_values)) {
    
    current_year <- as.integer(
      year_values[year_index]
    )
    
    candidate_dates <- as.Date(
      paste0(current_year, "-01-01")
    ) + seq_along(day_values) - 1L
    
    valid_day <- as.integer(
      format(candidate_dates, "%Y")
    ) == current_year
    
    current_dates <- candidate_dates[
      valid_day
    ]
    
    message(
      "Processing year ",
      current_year,
      " (",
      year_index,
      " of ",
      length(year_values),
      "): ",
      min(current_dates),
      " through ",
      max(current_dates)
    )
    
    temp_year <- read_temperature_year(
      year_index = year_index
    )
    
    # Drop day 366 for non-leap years.
    temp_year <- temp_year[
      ,
      ,
      valid_day,
      drop = FALSE
    ]
    
    n_current_days <- length(
      current_dates
    )
    
    temp_matrix <- matrix(
      temp_year,
      nrow = length(lon_subset) * length(lat_subset),
      ncol = n_current_days
    )
    
    inshore_temperature <- summarize_temperature_area(
      temperature_matrix = temp_matrix,
      cell_selector = inshore_cell,
      area_name = "inshore"
    )
    
    offshore_temperature <- summarize_temperature_area(
      temperature_matrix = temp_matrix,
      cell_selector = offshore_cell,
      area_name = "offshore"
    )
    
    daily_temperature_chunks[[year_index]] <- data.frame(
      date = current_dates
    ) |>
      dplyr::bind_cols(
        inshore_temperature,
        offshore_temperature
      )
    
    rm(
      temp_year,
      temp_matrix,
      inshore_temperature,
      offshore_temperature
    )
    
    invisible(
      gc()
    )
  }
  
 # Combine daily chunks
  daily_bottom_temperature <- dplyr::bind_rows(
    daily_temperature_chunks
  ) |>
    dplyr::mutate(
      year = as.integer(format(date, "%Y")),
      month = as.integer(format(date, "%m")),
      day = as.integer(format(date, "%d")),
      day_of_year = as.integer(format(date, "%j"))
    ) |>
    dplyr::select(
      date,
      year,
      month,
      day,
      day_of_year,
      mean_tmp_inshore,
      sd_tmp_inshore,
      min_tmp_inshore,
      max_tmp_inshore,
      n_cells_inshore,
      mean_tmp_offshore,
      sd_tmp_offshore,
      min_tmp_offshore,
      max_tmp_offshore,
      n_cells_offshore
    ) |>
    dplyr::arrange(
      date
    )
  
  if (nrow(daily_bottom_temperature) != length(dates)) {
    stop(
      "The final output has ",
      nrow(daily_bottom_temperature),
      " rows but the NetCDF file contains ",
      length(dates),
      " time observations."
    )
  }
  
  message(
    "Completed extraction of ",
    format(nrow(daily_bottom_temperature), big.mark = ","),
    " daily observations."
  )
  
  message(
    "Output date range: ",
    min(daily_bottom_temperature$date),
    " through ",
    max(daily_bottom_temperature$date)
  )
  
  message(
    "Days with no valid inshore temperature cells: ",
    sum(daily_bottom_temperature$n_cells_inshore == 0L)
  )
  
  message(
    "Days with no valid offshore temperature cells: ",
    sum(daily_bottom_temperature$n_cells_offshore == 0L)
  )
  
  print(
    summary(
      daily_bottom_temperature$mean_tmp_inshore
    )
  )
  
  print(
    summary(
      daily_bottom_temperature$mean_tmp_offshore
    )
  )
  
  daily_bottom_temperature
  
}, finally = {
  
  ncdf4::nc_close(
    ncin
  )
  
  message(
    "NetCDF connection closed."
  )
})


# Check completed daily dataset
dplyr::glimpse(lat_lons_all_agg)
head(lat_lons_all_agg)
tail(lat_lons_all_agg)
summary(lat_lons_all_agg$mean_tmp_inshore)
summary(lat_lons_all_agg$mean_tmp_offshore)
table(lat_lons_all_agg$year)


# Plot daily inshore and offshore mean bottom temperatures
graphics::matplot(
  x = lat_lons_all_agg$date,
  y = cbind(
    lat_lons_all_agg$mean_tmp_inshore,
    lat_lons_all_agg$mean_tmp_offshore
  ),
  type = "l",
  lty = c(1, 2),
  xlab = "Date",
  ylab = "Mean bottom temperature (degrees C)",
  main = "Mean Daily Bottom Temperature by Fishing Area"
)

graphics::legend(
  "topright",
  legend = c(
    "Inshore",
    "Offshore"
  ),
  lty = c(1, 2),
  bty = "n"
)

# Save daily bottom-temperature results
writexl::write_xlsx(lat_lons_all_agg, bottom_temp_output_xlsx)


###################################
# Section 3) Spatial diagnostics and plotting

# This code is largely the same as the code above, but pulls one date
# from the NetCDF in order to create the map in the mansucript. 
###################################

diagnostic_date <- as.Date("2021-07-15")

diagnostic_buffer_degrees <- 0.50

inshore_distance_m <- 3 * 1852


# Read fishing-footprint polygon
hull_gpkg <- file.path(
  process_data_dir,
  "VTR_convex_hull.gpkg"
)

bottom_temp_file <- file.path(
  process_data_dir,
  "bottom_temp_combined_product_1959_2022_soe.nc"
)

if (!file.exists(hull_gpkg)) {
  stop(
    "Convex-hull GeoPackage not found at:\n",
    hull_gpkg
  )
}

if (!file.exists(bottom_temp_file)) {
  stop(
    "Bottom-temperature NetCDF file not found at:\n",
    bottom_temp_file
  )
}

VTR_boundaries_subset <- sf::st_read(
  dsn = hull_gpkg,
  layer = "VTR_convex_hull",
  quiet = TRUE
) |>
  sf::st_make_valid()

if (is.na(sf::st_crs(VTR_boundaries_subset))) {
  
  warning(
    "The convex-hull file has no CRS. ",
    "Assigning EPSG:4326 under the assumption that its coordinates ",
    "are longitude and latitude."
  )
  
  sf::st_crs(VTR_boundaries_subset) <- 4326
  
} else {
  
  VTR_boundaries_subset <- sf::st_transform(
    VTR_boundaries_subset,
    4326
  )
}

VTR_footprint <- sf::st_union(
  VTR_boundaries_subset
)

footprint_bbox <- sf::st_bbox(
  VTR_footprint
)

northeast_land <- sf::st_as_sf(
  maps::map(
    database = "state",
    regions = c(
      "maine",
      "new hampshire",
      "massachusetts"
    ),
    plot = FALSE,
    fill = TRUE
  )
)

sf::st_crs(northeast_land) <- 4326

northeast_land <- northeast_land |>
  sf::st_make_valid() |>
  sf::st_union()

distance_crs <- 26919

land_projected <- sf::st_transform(
  northeast_land,
  distance_crs
)

footprint_projected <- sf::st_transform(
  VTR_footprint,
  distance_crs
)

coastal_buffer_projected <- sf::st_buffer(
  land_projected,
  dist = inshore_distance_m
)

inshore_footprint_projected <- sf::st_intersection(
  footprint_projected,
  coastal_buffer_projected
)

offshore_footprint_projected <- sf::st_difference(
  footprint_projected,
  coastal_buffer_projected
)

inshore_footprint <- sf::st_transform(
  inshore_footprint_projected,
  4326
)

offshore_footprint <- sf::st_transform(
  offshore_footprint_projected,
  4326
)

# Open the NetCDF file
ncin <- ncdf4::nc_open(
  bottom_temp_file
)

diagnostic_grid <- tryCatch({
  
  lon_name <- "longitude"
  lat_name <- "latitude"
  day_name <- "day"
  year_name <- "year"
  dname <- "sea_water_temperature_at_sea_floor"
  
  # Read longitude, latitude, day, and year
  lon_native <- ncdf4::ncvar_get(ncin, lon_name)
  lat <- ncdf4::ncvar_get(ncin, lat_name)
  day_values <- ncdf4::ncvar_get(ncin, day_name)
  year_values <- ncdf4::ncvar_get(ncin, year_name)
  
  lon_spatial <- lon_native
  
  if (any(lon_native > 180, na.rm = TRUE)) {
    lon_spatial <- ifelse(
      lon_native > 180,
      lon_native - 360,
      lon_native
    )
  }
  
  diagnostic_year <- as.integer(
    format(diagnostic_date, "%Y")
  )
  
  diagnostic_day_index <- as.integer(
    format(diagnostic_date, "%j")
  )
  
  diagnostic_year_index <- match(
    diagnostic_year,
    as.integer(year_values)
  )
  
  if (is.na(diagnostic_year_index)) {
    stop(
      "The diagnostic year ",
      diagnostic_year,
      " was not found. Available years range from ",
      min(year_values),
      " through ",
      max(year_values),
      "."
    )
  }
  
  if (
    diagnostic_day_index < 1L ||
    diagnostic_day_index > length(day_values)
  ) {
    stop(
      "The diagnostic day-of-year index is outside the NetCDF day dimension."
    )
  }
  
  diagnostic_xmin <- unname(
    footprint_bbox["xmin"]
  ) - diagnostic_buffer_degrees
  
  diagnostic_xmax <- unname(
    footprint_bbox["xmax"]
  ) + diagnostic_buffer_degrees
  
  diagnostic_ymin <- unname(
    footprint_bbox["ymin"]
  ) - diagnostic_buffer_degrees
  
  diagnostic_ymax <- unname(
    footprint_bbox["ymax"]
  ) + diagnostic_buffer_degrees
  
  lon_index <- which(
    lon_spatial >= diagnostic_xmin &
      lon_spatial <= diagnostic_xmax
  )
  
  lat_index <- which(
    lat >= diagnostic_ymin &
      lat <= diagnostic_ymax
  )
  
  if (length(lon_index) == 0L) {
    stop(
      "No longitude coordinates overlap the diagnostic bounding box."
    )
  }
  
  if (length(lat_index) == 0L) {
    stop(
      "No latitude coordinates overlap the diagnostic bounding box."
    )
  }
  
  lon_read_index <- seq.int(
    min(lon_index),
    max(lon_index)
  )
  
  lat_read_index <- seq.int(
    min(lat_index),
    max(lat_index)
  )
  
  lon_subset <- lon_spatial[
    lon_read_index
  ]
  
  lat_subset <- lat[
    lat_read_index
  ]
  
  # Read bottom temperature for the selected date
  temp_dimension_names <- vapply(
    ncin$var[[dname]]$dim,
    function(x) x$name,
    character(1)
  )
  
  temp_dimension_lengths <- vapply(
    ncin$var[[dname]]$dim,
    function(x) x$len,
    numeric(1)
  )
  
  lon_position <- match(
    lon_name,
    temp_dimension_names
  )
  
  lat_position <- match(
    lat_name,
    temp_dimension_names
  )
  
  day_position <- match(
    day_name,
    temp_dimension_names
  )
  
  year_position <- match(
    year_name,
    temp_dimension_names
  )
  
  start_values <- rep(
    1L,
    length(temp_dimension_names)
  )
  
  count_values <- as.integer(
    temp_dimension_lengths
  )
  
  start_values[lon_position] <- min(
    lon_read_index
  )
  
  count_values[lon_position] <- length(
    lon_read_index
  )
  
  start_values[lat_position] <- min(
    lat_read_index
  )
  
  count_values[lat_position] <- length(
    lat_read_index
  )
  
  start_values[day_position] <- diagnostic_day_index
  count_values[day_position] <- 1L
  
  start_values[year_position] <- diagnostic_year_index
  count_values[year_position] <- 1L
  
  diagnostic_temp_array <- ncdf4::ncvar_get(
    nc = ncin,
    varid = dname,
    start = start_values,
    count = count_values,
    collapse_degen = FALSE
  )
  
  dimension_permutation <- match(
    c(lon_name, lat_name, day_name, year_name),
    temp_dimension_names
  )
  
  diagnostic_temp_array <- aperm(
    diagnostic_temp_array,
    dimension_permutation
  )
  
  diagnostic_temp_matrix <- diagnostic_temp_array[
    ,
    ,
    1L,
    1L,
    drop = TRUE
  ]
  
  diagnostic_temp_matrix[
    abs(diagnostic_temp_matrix) > 100
  ] <- NA_real_
  
  diagnostic_grid <- expand.grid(
    lon = lon_subset,
    lat = lat_subset,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  
  diagnostic_grid$bottom_temperature <- as.vector(
    diagnostic_temp_matrix
  )
  
  diagnostic_points <- sf::st_as_sf(
    diagnostic_grid,
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )
  
  inside_footprint <- lengths(
    sf::st_intersects(
      diagnostic_points,
      VTR_footprint
    )
  ) > 0L
  
  inshore_cell <- inside_footprint &
    lengths(
      sf::st_intersects(
        diagnostic_points,
        inshore_footprint
      )
    ) > 0L
  
  offshore_cell <- inside_footprint & !inshore_cell
  
  diagnostic_grid <- diagnostic_grid |>
    dplyr::mutate(
      inside_footprint = inside_footprint,
      inshore_cell = inshore_cell,
      offshore_cell = offshore_cell,
      temperature_area = dplyr::case_when(
        inshore_cell ~ "Inshore",
        offshore_cell ~ "Offshore",
        TRUE ~ "Outside footprint"
      ),
      diagnostic_date = diagnostic_date
    )
  
  diagnostic_grid
  
}, finally = {
  
  ncdf4::nc_close(
    ncin
  )
  
  message(
    "Diagnostic NetCDF connection closed."
  )
})

# Check grid-cell counts and temperatures
diagnostic_grid |>
  dplyr::count(
    temperature_area,
    name = "n_grid_cells"
  ) |>
  print()

diagnostic_grid |>
  dplyr::group_by(
    temperature_area
  ) |>
  dplyr::summarise(
    n_grid_cells = dplyr::n(),
    n_valid_temperature = sum(
      !is.na(bottom_temperature)
    ),
    mean_temperature = mean(
      bottom_temperature,
      na.rm = TRUE
    ),
    min_temperature = min(
      bottom_temperature,
      na.rm = TRUE
    ),
    max_temperature = max(
      bottom_temperature,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  print()

northeast_states <- sf::st_as_sf(
  maps::map(
    database = "state",
    regions = c(
      "maine",
      "new hampshire",
      "massachusetts"
    ),
    plot = FALSE,
    fill = TRUE
  )
)

sf::st_crs(northeast_states) <- 4326

map_x_limits <- c(-72.5, -66.0)
map_y_limits <- c(41.0, 47.5)

northeast_states_crop <- sf::st_crop(
  northeast_states,
  xmin = map_x_limits[1],
  xmax = map_x_limits[2],
  ymin = map_y_limits[1],
  ymax = map_y_limits[2]
)

longitude_resolution <- stats::median(
  abs(
    diff(
      sort(
        unique(diagnostic_grid$lon)
      )
    )
  )
)

latitude_resolution <- stats::median(
  abs(
    diff(
      sort(
        unique(diagnostic_grid$lat)
      )
    )
  )
)

# Map 1: Explicit inshore/offshore classification
area_map <- ggplot2::ggplot() +
  
  ggplot2::geom_tile(
    data = diagnostic_grid |>
      dplyr::filter(
        inside_footprint
      ),
    ggplot2::aes(
      x = lon,
      y = lat,
      fill = temperature_area
    ),
    width = longitude_resolution,
    height = latitude_resolution
  ) +
  
  ggplot2::geom_sf(
    data = northeast_states_crop,
    fill = "grey90",
    color = "grey45",
    linewidth = 0.4,
    inherit.aes = FALSE
  ) +
  
  ggplot2::geom_sf(
    data = inshore_footprint,
    fill = NA,
    color = "black",
    linewidth = 0.7,
    linetype = "dashed",
    inherit.aes = FALSE
  ) +
  
  ggplot2::geom_sf(
    data = VTR_footprint,
    fill = NA,
    color = "black",
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  
  ggplot2::coord_sf(
    xlim = map_x_limits,
    ylim = map_y_limits,
    expand = FALSE,
    datum = sf::st_crs(4326)
  ) +
  
  ggplot2::scale_fill_manual(
    values = c(
      "Inshore" = "#0072B2",
      "Offshore" = "#E69F00"
    ),
    drop = FALSE
  ) +
  
  ggplot2::labs(
    title = "GLORYS Grid Cells Assigned to Inshore and Offshore Areas",
    subtitle = paste0(
      "Inshore is within three nautical miles of shore; ",
      "classification is based on each grid-cell center"
    ),
    x = "Longitude",
    y = "Latitude",
    fill = "Fishing area"
  ) +
  
  ggplot2::theme_bw() +
  
  ggplot2::theme(
    legend.position = "right",
    panel.grid.major = ggplot2::element_line(
      color = "grey85",
      linewidth = 0.25
    ),
    plot.title = ggplot2::element_text(
      hjust = 0.5
    ),
    plot.subtitle = ggplot2::element_text(
      hjust = 0.5
    )
  )

print(area_map)



