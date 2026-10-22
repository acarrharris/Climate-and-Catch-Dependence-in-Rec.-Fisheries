
# load packages 
pkgs_to_use <- c("data.table",
                 "dplyr",
                 "ncdf4",
                 "sf",
                 "tidyr",
                 "writexl", 
                 "readr")

lapply(pkgs_to_use, library, character.only = TRUE, quietly = TRUE)


# Data required: 
# Gom cod stock areas from NOAA public use shapefiles
# Archived in this repository and available at:  
# https://www.fisheries.noaa.gov/resources/maps?title=&tid%5B1000001111%5D=1000001111&field_species_vocab_target_id=&sort_by=created 

# lat-lons of fishing locations from for-hire fishing vessels that reported catching cod or haddock -
# Not publicly available due to NOAA confidentiality restrictions.
# Derived convex hulls included in this repository. To replicate, start at section 2) 

# Historical bottom temperature data product **FOR 1993-2026**
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

bottom_temp_file <- file.path(process_data_dir,"GLORYS_BottomT_USNES_1993_2026.nc")

bottom_temp_output_xlsx <- file.path(process_data_dir, "daily_btemp_GoM_by_area_1993_2026.xlsx")


# Section 1) Generate convex hull of fishing activity based on for-hire vessel trip reports

# Restricted-data processing retained for documentation only.
# This section is not part of the public reproducibility workflow and cannot 
# be run with public repo data; use the provided VTR_convex_hull.gpkg and begin at Section 2.

# PROJ.USE = CRS('+proj=aea +lat_1=28 +lat_2=42 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 ')
# 
# GoM cod stock areas
# GoM_shape <- st_read(dsn=paste0("X:", "/Stock_Areas"),
#                       layer="Stock_Areas")
# 
#  GoM_shape <-GoM_shape %>%
#    dplyr::filter(AREANAME  %in% c("GOM Cod Stock Area"))

# East Coast US state map for visualization
# states <- map_data("state")
# east_coast <- subset(states, region %in% c("maine", "massachusetts", "vermont", "new hampshire","rhode island", "connecticut", "new york", "pennsylvania",
#                                            "new jersey", "maryland", "delaware", "district of columbia", "virginia", "west virginia", "north carolina"))

#VTR lat-lons - not publicly available
#Create a "fishing footprint" (convex hull) based on VTR points for all years combined
# vtr_lat_lons_cod<-read_csv(paste0("X:/", "Yearly_Cod_Recreational_Lat_Lon.csv"), show_col_types = FALSE)
# vtr_lat_lons_hadd<-read_csv(paste0("X:/", "Yearly_Haddock_Recreational_Lat_Lon.csv"), show_col_types = FALSE)
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
# #
# #
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
# #
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
# # make sure geometry is valid before writing
# VTR_boundaries_subset <- sf::st_make_valid(VTR_boundaries_subset)
# 
# # ensure a clean attribute field (shapefiles like short names)
# VTR_boundaries_subset <- VTR_boundaries_subset %>%
#   dplyr::mutate(area = as.character(area)) %>%
#   dplyr::select(area, geometry)
# 
# # save derived convex hull
# gpkg_path <- file.path(process_data_dir, "VTR_convex_hull.gpkg")
# if (file.exists(gpkg_path)) file.remove(gpkg_path)
# 
# sf::st_write(
#   obj = VTR_boundaries_subset,
#   dsn = gpkg_path,
#   layer = "VTR_convex_hull",
#   driver = "GPKG",
#   append = FALSE
# )

# Section 2) Extract daily bottom temperatures inside convex hull
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

# Read fishing footprint data
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

# Classify cells using their NetCDF coordinate locations, which are the grid-cell centroids.
classify_temperature_cells_by_centroid <- function(grid_subset, footprint_projected, inshore_footprint_projected, distance_crs) {
  
  lon_resolution <- stats::median(abs(diff(sort(unique(grid_subset$lon)))))
  lat_resolution <- stats::median(abs(diff(sort(unique(grid_subset$lat)))))
  lon_half_width <- lon_resolution / 2
  lat_half_width <- lat_resolution / 2
  
  grid_cell_geometry <- sf::st_sfc(
    lapply(seq_len(nrow(grid_subset)), function(i) {
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
    }),
    crs = 4326
  )
  
  grid_cells <- sf::st_sf(
    grid_id = seq_len(nrow(grid_subset)),
    lon = grid_subset$lon,
    lat = grid_subset$lat,
    geometry = grid_cell_geometry
  )
  
  grid_cells_projected <- sf::st_transform(grid_cells, distance_crs)

  grid_centroids <- sf::st_as_sf(
    grid_subset,
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )

  grid_centroids_projected <- sf::st_transform(grid_centroids, distance_crs)
  
  # Include a cell only when its centroid falls within the fishing footprint.
  inside_footprint <- lengths(
    sf::st_intersects(grid_centroids_projected, footprint_projected)
  ) > 0L
  
  # Among included cells, classify a cell as inshore only when its centroid
  # falls within the portion of the footprint located within three nautical
  # miles of land.
  inshore_cell <- inside_footprint &
    lengths(
      sf::st_intersects(grid_centroids_projected, inshore_footprint_projected)
    ) > 0L
  
  offshore_cell <- inside_footprint & !inshore_cell
  
  list(
    inside_footprint = inside_footprint,
    inshore_cell = inshore_cell,
    offshore_cell = offshore_cell,
    grid_cells = grid_cells,
    grid_cells_projected = grid_cells_projected,
    grid_centroids = grid_centroids,
    grid_centroids_projected = grid_centroids_projected
  )
}

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
  time_name <- "time"
  dname <- "bottomT"
  
  # Verify required dimensions and variables
  required_dimensions <- c(
    lon_name,
    lat_name,
    time_name
  )
  
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
  
  # Read longitude, latitude, and time
  lon_native <- ncdf4::ncvar_get(
    ncin,
    lon_name
  )
  
  lat <- ncdf4::ncvar_get(
    ncin,
    lat_name
  )
  
  time_raw <- ncdf4::ncvar_get(
    ncin,
    time_name
  )
  
  message("Number of longitude coordinates: ", length(lon_native))
  message("Number of latitude coordinates: ", length(lat))
  message("Number of daily observations: ", length(time_raw))
  
  
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
  
  # Convert NetCDF time to calendar dates

  time_units_attribute <- ncdf4::ncatt_get(
    ncin,
    time_name,
    "units"
  )
  
  if (!isTRUE(time_units_attribute$hasatt)) {
    stop(
      "The time variable does not have a units attribute."
    )
  }
  
  time_units <- time_units_attribute$value
  
  message("NetCDF time units: ", time_units)
  
  if (!grepl(
    "^hours since ",
    time_units,
    ignore.case = TRUE
  )) {
    stop(
      "This code expects time units expressed as 'hours since ...'. ",
      "The file instead reports: ",
      time_units
    )
  }
  
  time_origin_text <- sub(
    "^hours since\\s+",
    "",
    time_units,
    ignore.case = TRUE
  )
  
  # Remove common trailing timezone labels, if present.
  time_origin_text <- sub(
    "\\s+(UTC|GMT|Z)$",
    "",
    time_origin_text,
    ignore.case = TRUE
  )
  
  time_origin <- as.POSIXct(
    time_origin_text,
    tz = "UTC",
    tryFormats = c(
      "%Y-%m-%d %H:%M:%S",
      "%Y-%m-%d %H:%M",
      "%Y-%m-%d"
    )
  )
  
  if (is.na(time_origin)) {
    stop(
      "Could not parse the NetCDF time origin: ",
      time_origin_text
    )
  }
  
  dates <- as.Date(
    time_origin + time_raw * 60 * 60,
    tz = "UTC"
  )
  
  if (length(dates) != length(time_raw)) {
    stop(
      "The converted date vector does not match the NetCDF time dimension."
    )
  }
  
  if (anyNA(dates)) {
    stop(
      "One or more NetCDF time values could not be converted to dates."
    )
  }
  
  message(
    "Date range: ",
    min(dates),
    " through ",
    max(dates)
  )
  
  message(
    "Number of unique dates: ",
    length(unique(dates))
  )
  
  if (anyDuplicated(dates)) {
    warning(
      "The NetCDF file contains more than one time observation for at least ",
      "one calendar date. Daily output will contain repeated dates unless ",
      "the observations are aggregated."
    )
  }
  
  
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
    c(lon_name, lat_name, time_name) %in%
    temp_dimension_names
  )) {
    stop(
      "The temperature variable does not contain all three expected dimensions: ",
      paste(c(lon_name, lat_name, time_name), collapse = ", "),
      ".\nIts dimensions are: ",
      paste(temp_dimension_names, collapse = ", ")
    )
  }
  
  if (length(temp_dimension_names) != 3L) {
    stop(
      "The temperature variable has ",
      length(temp_dimension_names),
      " dimensions. This code currently expects exactly three: ",
      "longitude, latitude, and time."
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
  
  time_position <- match(
    time_name,
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
  
  grid_classification <- classify_temperature_cells_by_centroid(
    grid_subset = grid_subset,
    footprint_projected = footprint_projected,
    inshore_footprint_projected = inshore_footprint_projected,
    distance_crs = distance_crs
  )
  
  inside_footprint <- grid_classification$inside_footprint
  inshore_cell <- grid_classification$inshore_cell
  offshore_cell <- grid_classification$offshore_cell
  grid_cells <- grid_classification$grid_cells
  

  # Plot the NetCDF grid cells assigned to each temperature area
  product_label <- "Bottom-temperature product, 1993–2026"
  
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
  
  map_padding_degrees <- 0.75
  
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
        "NetCDF cells assigned using grid-cell centroids\n",
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
  
  ggplot2::ggsave(
    filename = file.path(figure_dir,
      "bottom_temperature_cells_1993_2026.png"
    ),
    plot = selected_cell_map,
    width = 9,
    height = 8,
    units = "in",
    dpi = 400
  )
  
  
  selected_cell_map2 <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = northeast_land,
      fill = "grey85",
      color = "grey55",
      linewidth = 0.2
    ) +
    ggplot2::geom_sf(
      data = selected_grid_cells,
      fill = "#56B4E9",
      color = "grey20",
      linewidth = 0.12,
      alpha = 1
    ) +
    ggplot2::geom_sf(
      data = VTR_footprint,
      fill = NA,
      color = "black",
      linewidth = 0.8
    ) +
    ggplot2::coord_sf(
      xlim = unname(map_bbox[c("xmin", "xmax")]) +
        c(-map_padding_degrees, map_padding_degrees),
      ylim = unname(map_bbox[c("ymin", "ymax")]) +
        c(-map_padding_degrees, map_padding_degrees),
      expand = FALSE
    ) +
    ggplot2::labs(
      title = product_label,
      subtitle = paste0(
        "NetCDF cells selected using grid-cell centroids\n",
        "Black line: complete fishing footprint"
      ),
      x = "Longitude",
      y = "Latitude"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(
        color = "grey90",
        linewidth = 0.2
      ),
      legend.position = "none"
    )
  
  print(selected_cell_map2)
  
  ggplot2::ggsave(
    filename = file.path(figure_dir,
      "bottom_temperature_cells_1993_2026_common.png"
    ),
    plot = selected_cell_map2,
    width = 9,
    height = 8,
    units = "in",
    dpi = 400
  )
  
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
  

  # Diagnostic grid
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
      MARGIN = 2,
      FUN = function(x) {
        if (sum(!is.na(x)) <= 1L) NA_real_ else stats::sd(x, na.rm = TRUE)
      }
    )
    
    min_temperature <- apply(
      temperature_subset,
      MARGIN = 2,
      FUN = function(x) {
        if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
      }
    )
    
    max_temperature <- apply(
      temperature_subset,
      MARGIN = 2,
      FUN = function(x) {
        if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
      }
    )
    
    mean_temperature[n_valid_cells == 0L] <- NA_real_
    sd_temperature[n_valid_cells <= 1L] <- NA_real_
    min_temperature[n_valid_cells == 0L] <- NA_real_
    max_temperature[n_valid_cells == 0L] <- NA_real_
    
    output <- data.frame(
      mean_tmp = mean_temperature,
      sd_tmp = sd_temperature,
      min_tmp = min_temperature,
      max_tmp = max_temperature,
      n_cells = n_valid_cells
    )
    
    names(output) <- paste0(names(output), "_", area_name)
    output
  }
  
  read_temperature_chunk <- function(
    time_start,
    time_count
  ) {
    
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
    
    start_values[time_position] <- time_start
    count_values[time_position] <- time_count
    
    temp_chunk_native <- ncdf4::ncvar_get(
      nc = ncin,
      varid = dname,
      start = start_values,
      count = count_values,
      collapse_degen = FALSE
    )
    
    # Reorder the returned array to:
    # longitude × latitude × time
    permutation <- match(
      c(
        lon_name,
        lat_name,
        time_name
      ),
      temp_dimension_names
    )
    
    temp_chunk <- aperm(
      temp_chunk_native,
      permutation
    )
    
    expected_dimensions <- c(
      length(lon_subset),
      length(lat_subset),
      time_count
    )
    
    if (!identical(
      as.integer(dim(temp_chunk)),
      as.integer(expected_dimensions)
    )) {
      stop(
        "Unexpected temperature chunk dimensions.\n",
        "Expected: ",
        paste(expected_dimensions, collapse = " × "),
        "\nReceived: ",
        paste(dim(temp_chunk), collapse = " × ")
      )
    }
    
    
    temp_chunk
  }
  
  chunk_size <- 366L
  
  chunk_starts <- seq.int(
    from = 1L,
    to = length(dates),
    by = chunk_size
  )
  
  daily_temperature_chunks <- vector(
    mode = "list",
    length = length(chunk_starts)
  )
  
  for (chunk_number in seq_along(chunk_starts)) {
    
    time_start <- chunk_starts[
      chunk_number
    ]
    
    time_count <- min(
      chunk_size,
      length(dates) - time_start + 1L
    )
    
    time_end <- time_start + time_count - 1L
    
    message(
      "Processing chunk ",
      chunk_number,
      " of ",
      length(chunk_starts),
      ": ",
      dates[time_start],
      " through ",
      dates[time_end]
    )
    
    temp_chunk <- read_temperature_chunk(
      time_start = time_start,
      time_count = time_count
    )
    
    temp_matrix <- matrix(
      temp_chunk,
      nrow = length(lon_subset) * length(lat_subset),
      ncol = time_count
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
    
    daily_temperature_chunks[[chunk_number]] <- data.frame(
      date = dates[time_start:time_end]
    ) |>
      dplyr::bind_cols(
        inshore_temperature,
        offshore_temperature
      )
    
    rm(
      temp_chunk,
      temp_matrix,
      inshore_temperature,
      offshore_temperature
    )
    
    invisible(
      gc()
    )
  }
  
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
  
  print(summary(daily_bottom_temperature$mean_tmp_inshore))
  print(summary(daily_bottom_temperature$mean_tmp_offshore))
  
  daily_bottom_temperature
  
}, finally = {
  
  ncdf4::nc_close(
    ncin
  )
  
  message(
    "NetCDF connection closed."
  )
})


# Check the completed daily dataset
dplyr::glimpse(lat_lons_all_agg)
head(lat_lons_all_agg)
tail(lat_lons_all_agg)
summary(lat_lons_all_agg$mean_tmp_inshore)
summary(lat_lons_all_agg$mean_tmp_offshore)
table(lat_lons_all_agg$year)


# Save daily bottom-temperature results
writexl::write_xlsx(lat_lons_all_agg, bottom_temp_output_xlsx)


###################################
# Section 3) Spatial diagnostics and fishing-footprint figure

# This code is largely the same as the code above, but pulls one date
# from the NetCDF in order to create the map in the mansucript. 

###################################

diagnostic_date <- as.Date("2025-07-15")

diagnostic_buffer_degrees <- 0.50

inshore_distance_m <- 3 * 1852

hull_gpkg <- file.path(
  process_data_dir,
  "VTR_convex_hull.gpkg"
)

bottom_temp_file <- file.path(
  process_data_dir,
  "GLORYS_BottomT_USNES_1993_2026.nc"
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

ncin <- ncdf4::nc_open(
  bottom_temp_file
)

diagnostic_grid <- tryCatch({
  
  lon_name <- "longitude"
  lat_name <- "latitude"
  time_name <- "time"
  dname <- "bottomT"
  
  lon_native <- ncdf4::ncvar_get(
    ncin,
    lon_name
  )
  
  lat <- ncdf4::ncvar_get(
    ncin,
    lat_name
  )
  
  time_raw <- ncdf4::ncvar_get(
    ncin,
    time_name
  )
  
  lon_spatial <- lon_native
  
  if (any(lon_native > 180, na.rm = TRUE)) {
    lon_spatial <- ifelse(
      lon_native > 180,
      lon_native - 360,
      lon_native
    )
  }
  
  time_units <- ncdf4::ncatt_get(
    ncin,
    time_name,
    "units"
  )$value
  
  time_origin_text <- sub(
    "^hours since\\s+",
    "",
    time_units,
    ignore.case = TRUE
  )
  
  time_origin_text <- sub(
    "\\s+(UTC|GMT|Z)$",
    "",
    time_origin_text,
    ignore.case = TRUE
  )
  
  time_origin <- as.POSIXct(
    time_origin_text,
    tz = "UTC",
    tryFormats = c(
      "%Y-%m-%d %H:%M:%S",
      "%Y-%m-%d %H:%M",
      "%Y-%m-%d"
    )
  )
  
  dates <- as.Date(
    time_origin + time_raw * 60 * 60,
    tz = "UTC"
  )
  
  diagnostic_time_index <- which(
    dates == diagnostic_date
  )
  
  if (length(diagnostic_time_index) == 0L) {
    stop(
      "The diagnostic date ",
      diagnostic_date,
      " was not found in the NetCDF file.\n",
      "Available dates range from ",
      min(dates),
      " through ",
      max(dates),
      "."
    )
  }
  
  if (length(diagnostic_time_index) > 1L) {
    
    warning(
      "More than one NetCDF time index matches ",
      diagnostic_date,
      ". Using the first match."
    )
    
    diagnostic_time_index <- diagnostic_time_index[1]
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
  
  time_position <- match(
    time_name,
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
  
  start_values[time_position] <- diagnostic_time_index
  count_values[time_position] <- 1L
  
  diagnostic_temp_array <- ncdf4::ncvar_get(
    nc = ncin,
    varid = dname,
    start = start_values,
    count = count_values,
    collapse_degen = FALSE
  )
  
  dimension_permutation <- match(
    c(
      lon_name,
      lat_name,
      time_name
    ),
    temp_dimension_names
  )
  
  diagnostic_temp_array <- aperm(
    diagnostic_temp_array,
    dimension_permutation
  )
  
  diagnostic_temp_matrix <- diagnostic_temp_array[
    ,
    ,
    1,
    drop = TRUE
  ]
  
  diagnostic_grid <- expand.grid(
    lon = lon_subset,
    lat = lat_subset,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  
  diagnostic_grid$bottom_temperature <- as.vector(
    diagnostic_temp_matrix
  )
  
  diagnostic_temperature_cells <- selected_grid_cells |>
    dplyr::left_join(
      diagnostic_grid |>
        sf::st_drop_geometry() |>
        dplyr::select(
          lon,
          lat,
          bottom_temperature
        ),
      by = c("lon", "lat")
    ) |>
    dplyr::mutate(
      temperature_status = dplyr::if_else(
        is.finite(bottom_temperature),
        "Available",
        "Selected but missing"
      ),
      map_category = paste(
        temperature_area,
        temperature_status,
        sep = ": "
      )
    )
  
  diagnostic_cell_map <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = northeast_land,
      fill = "grey85",
      color = "grey55",
      linewidth = 0.2
    ) +
    ggplot2::geom_sf(
      data = diagnostic_temperature_cells,
      ggplot2::aes(fill = map_category),
      color = "grey30",
      linewidth = 0.12
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
        "Inshore: Available" = "#0072B2",
        "Inshore: Selected but missing" = "#B9DDF2",
        "Offshore: Available" = "#E69F00",
        "Offshore: Selected but missing" = "#F6D7A2"
      )
    ) +
    ggplot2::coord_sf(
      xlim = unname(map_bbox[c("xmin", "xmax")]) + c(-0.15, 0.15),
      ylim = unname(map_bbox[c("ymin", "ymax")]) + c(-0.15, 0.15),
      expand = FALSE
    ) +
    ggplot2::labs(
      title = product_label,
      subtitle = paste(
        "NetCDF cells assigned using grid-cell centroids"
      ),
      fill = NULL,
      x = "Longitude",
      y = "Latitude"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "right")
  
  print(diagnostic_cell_map)
  
  ggplot2::ggsave(
    filename = file.path(figure_dir,
      "available_bottom_temperature_cells_1993_2026.png"
    ),
    plot = diagnostic_cell_map,
    width = 9,
    height = 8,
    units = "in",
    dpi = 400
  )
  
  diagnostic_classification <- classify_temperature_cells_by_centroid(
    grid_subset = diagnostic_grid[, c("lon", "lat")],
    footprint_projected = footprint_projected,
    inshore_footprint_projected = inshore_footprint_projected,
    distance_crs = distance_crs
  )
  
  inside_footprint <- diagnostic_classification$inside_footprint
  inshore_cell <- diagnostic_classification$inshore_cell
  offshore_cell <- diagnostic_classification$offshore_cell
  
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


# Create map
# Note the top panel of the map in the paper uses confidential VTR data and cannot be reproduced here. 
# Thus, the relevenant code is commented out. 

northeast_states <- sf::st_as_sf(
  maps::map(
    database = "state",
    regions = c(
      "maine", "massachusetts", "vermont", "new hampshire","rhode island", "connecticut", "new york", "pennsylvania",
      "new jersey", "maryland", "delaware", "district of columbia", "virginia", "west virginia", "north carolina"),
    plot = FALSE,
    fill = TRUE
  )
)

sf::st_crs(northeast_states) <- 4326

map_x_limits <- c(-73, -66.0)
map_y_limits <- c(41, 44.7)

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
    data = northeast_states,
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

northeast_states <- northeast_states |>
  dplyr::mutate(
    state_abbr = state.abb[match(tolower(ID), tolower(state.name))]
  )

map_x_limits <- c(-73, -66)
map_y_limits <- c(41, 44.7)

# vtr_map <- ggplot2::ggplot() +
#   ggplot2::geom_sf(
#     data = northeast_states,
#     fill = "grey90",
#     color = "grey45",
#     inherit.aes = FALSE
#   ) +
#   ggplot2::geom_point(
#     data = VTR_GoM_points2_inside3,
#     ggplot2::aes(x = LON, y = LAT),
#     color = "#0072B2",
#     size = 0.5,
#     alpha = 1,
#     show.legend = FALSE
#   ) +
#   ggplot2::geom_point(
#     data = VTR_GoM_points2_inside2,
#     ggplot2::aes(x = LON, y = LAT),
#     color = "#E69F00",
#     size = 0.5,
#     alpha = 1,
#     show.legend = FALSE
#   ) +
#   ggplot2::geom_polygon(
#     data = hull_poly2,
#     ggplot2::aes(x = LON, y = LAT, group = area),
#     color = "black",
#     fill = NA,
#     linewidth = 0.6,
#     show.legend = FALSE
#   ) +
#   ggplot2::coord_sf(
#     xlim = map_x_limits,
#     ylim = map_y_limits,
#     expand = FALSE,
#     crs = sf::st_crs(4326)
#   ) +
#   ggplot2::theme_bw() +
#   ggplot2::theme(
#     axis.title = ggplot2::element_blank(),
#     axis.text.x = ggplot2::element_blank(),
#     axis.ticks.x = ggplot2::element_blank(),
#     plot.title = ggplot2::element_text(hjust = 0.5)
#   ) +
#   ggspatial::annotation_scale(
#     location = "br",
#     bar_cols = c("grey60", "white")
#   ) +
#   ggspatial::annotation_north_arrow(
#     location = "tl",
#     which_north = "true",
#     pad_x = grid::unit(0.2, "in"),
#     pad_y = grid::unit(0.2, "in"),
#     style = ggspatial::north_arrow_nautical(
#       fill = c("grey40", "white"),
#       line_col = "grey20"
#     )
#   )

area_map <- ggplot2::ggplot() +
  ggplot2::geom_tile(
    data = diagnostic_grid |>
      dplyr::filter(inside_footprint),
    ggplot2::aes(
      x = lon,
      y = lat,
      fill = temperature_area
    ),
    width = longitude_resolution,
    height = latitude_resolution,
    color = "grey35"
  ) +
  ggplot2::geom_sf(
    data = northeast_states,
    fill = "grey90",
    color = "grey45",
    inherit.aes = FALSE
  ) +
  ggplot2::geom_sf(
    data = VTR_footprint,
    fill = NA,
    color = "black",
    linewidth = 0.6,
    inherit.aes = FALSE
  ) +
  ggplot2::coord_sf(
    xlim = map_x_limits,
    ylim = map_y_limits,
    expand = FALSE,
    crs = sf::st_crs(4326)
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Inshore" = "#8DA0CB",
      "Offshore" = "#66C2A5"
    ),
    drop = FALSE
  ) +
  ggplot2::labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Fishing area"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "none",
    panel.grid.major = ggplot2::element_line(color = "grey85")
  )

state_labels <- data.frame(
  state = c(
    "Maine",
    "New Hampshire",
    "Massachusetts",
    "Vermont",
    "Rhode Island",
    "Connecticut"
  ),
  lon = c(-70.5, -71.7, -71.7, -72.6, -71.4, -72.5),
  lat = c(44.0, 43.3, 42.3, 43.9, 41.2, 41.7)
)

state_label_layer <- ggplot2::geom_text(
  data = state_labels,
  ggplot2::aes(x = lon, y = lat, label = state),
  size = 2.5,
  fontface = "plain",
  color = "grey25",
  inherit.aes = FALSE
)

#vtr_map <- vtr_map + state_label_layer
area_map <- area_map + state_label_layer

# combined_map <- patchwork::wrap_plots(
#   vtr_map,
#   area_map,
#   ncol = 1,
#   heights = c(1, 1)
# )
# 
# print(combined_map)
#  
# save_plot <- function(plot, name, width = 8, height = 10) {
#   ggplot2::ggsave(
#     filename = file.path(figure_dir, name),
#     plot = plot,
#     device = if (grepl("\\.eps$", name, ignore.case = TRUE)) grDevices::cairo_ps else NULL,
#     width = width, height = height, units = "in", dpi = 300
# 
#   )
# }
# 
# save_plot(
#   combined_map,
#   paste0("fishing_footprint.eps")
# )
# save_plot(
#   combined_map,
#   paste0("fishing_footprint.jpg")
# )

