
# This scripts reads in the VTR convex hull (fishing footprint), the CM2.6 projected deltas, 
# and extracts deltas within the convex hull for inshore/offshore cells. 

options(scipen = 999)
library(dplyr)


# OpenAI ChatGPT (GPT-5.6 Sol) was used to optimize this code. 

# Set global directories - user edit here
project_dir <- "C:/Users/andrew.carr-harris/Desktop/Git/Climate-and-Catch-Dependence-in-Rec.-Fisheries"
process_data_dir <- file.path(
  project_dir,
  "data"
)

figure_dir <- file.path(
  project_dir,
  "outputs",
  "figures"
)



# Read fishing footprint
hull_gpkg <- file.path(process_data_dir, "VTR_convex_hull.gpkg")

if (!file.exists(hull_gpkg)) stop("Convex hull GeoPackage not found at: ", hull_gpkg)

VTR_boundaries_subset <- sf::st_read(hull_gpkg, layer = "VTR_convex_hull", quiet = TRUE) |> sf::st_make_valid()

if (is.na(sf::st_crs(VTR_boundaries_subset))) {
  warning("The convex-hull file has no CRS. Assigning EPSG:4326 under the assumption that coordinates are longitude/latitude.")
  sf::st_crs(VTR_boundaries_subset) <- 4326
} else {
  VTR_boundaries_subset <- sf::st_transform(VTR_boundaries_subset, 4326)
}

VTR_footprint <- sf::st_union(VTR_boundaries_subset)

# inshore/offshore split
inshore_distance_m <- 3 * 1852

northeast_land <- sf::st_as_sf(maps::map(database = "state", regions = c("maine", "new hampshire", "massachusetts"), plot = FALSE, fill = TRUE))
sf::st_crs(northeast_land) <- 4326
northeast_land <- northeast_land |> sf::st_make_valid() |> sf::st_union()

distance_crs <- 26919

land_projected <- sf::st_transform(northeast_land, distance_crs)
footprint_projected <- sf::st_transform(VTR_footprint, distance_crs)
coastal_buffer_projected <- sf::st_buffer(land_projected, dist = inshore_distance_m)

inshore_footprint_projected <- sf::st_intersection(footprint_projected, coastal_buffer_projected)
offshore_footprint_projected <- sf::st_difference(footprint_projected, coastal_buffer_projected)

inshore_footprint <- sf::st_transform(inshore_footprint_projected, 4326)
offshore_footprint <- sf::st_transform(offshore_footprint_projected, 4326)

# Function to extract monthly projected bottom-temperature deltas
# by area from one NetCDF file
extract_projected_bottom_temp_by_area <- function(ncfname, starting_year) {

  if (!file.exists(ncfname)) stop("Projected bottom-temperature NetCDF file not found at: ", ncfname)

  ncin <- ncdf4::nc_open(ncfname)
  on.exit(ncdf4::nc_close(ncin), add = TRUE)

  dname <- "TEMP_BOTTOM_DELTA"
  lon_name <- "XT_OCEAN2001_225021_150"
  lat_name <- "YT_OCEAN1560_180961_177"
  time_name <- "TIME"

  message("Processing: ", basename(ncfname))

  lon_native <- ncdf4::ncvar_get(ncin, lon_name)
  lat <- ncdf4::ncvar_get(ncin, lat_name)
  time <- ncdf4::ncvar_get(ncin, time_name)

  lon_spatial <- ifelse(lon_native > 180, lon_native - 360, lon_native)

  grid <- expand.grid(lon = lon_spatial, lat = lat, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  grid_points <- sf::st_as_sf(grid, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

  inside_footprint <- lengths(sf::st_intersects(grid_points, VTR_footprint)) > 0L
  inshore_cell <- inside_footprint & lengths(sf::st_intersects(grid_points, inshore_footprint)) > 0L
  offshore_cell <- inside_footprint & !inshore_cell

  message("Projected grid cells inside fishing footprint: ", sum(inside_footprint),
          "; inshore: ", sum(inshore_cell), "; offshore: ", sum(offshore_cell))

  if (sum(inside_footprint) == 0L) stop("No projected-model grid-cell centers fall inside the fishing footprint.")
  if (sum(inshore_cell) == 0L) stop("No projected-model grid-cell centers were classified as inshore.")
  if (sum(offshore_cell) == 0L) stop("No projected-model grid-cell centers were classified as offshore.")

  temp_dimension_names <- vapply(ncin$var[[dname]]$dim, function(x) x$name, character(1))
  expected_dimension_names <- c(lon_name, lat_name, time_name)

  if (!all(expected_dimension_names %in% temp_dimension_names) || length(temp_dimension_names) != 3L) {
    stop("Unexpected dimensions for ", dname, ". Found: ", paste(temp_dimension_names, collapse = ", "))
  }

  tmp_array <- ncdf4::ncvar_get(ncin, dname, collapse_degen = FALSE)
  tmp_array <- aperm(tmp_array, match(expected_dimension_names, temp_dimension_names))

  expected_dims <- c(length(lon_native), length(lat), length(time))
  if (!identical(as.integer(dim(tmp_array)), as.integer(expected_dims))) {
    stop("Unexpected temperature-array dimensions. Expected ", paste(expected_dims, collapse = " x "),
         " but received ", paste(dim(tmp_array), collapse = " x "))
  }

  tmp_matrix <- matrix(tmp_array, nrow = length(lon_native) * length(lat), ncol = length(time))

  summarize_area <- function(selector) {
    area_matrix <- tmp_matrix[selector, , drop = FALSE]
    n_cells <- colSums(!is.na(area_matrix))
    mean_temp <- colMeans(area_matrix, na.rm = TRUE)
    mean_temp[n_cells == 0L] <- NA_real_
    list(mean_temp = mean_temp, n_cells = n_cells)
  }

  inshore_summary <- summarize_area(inshore_cell)
  offshore_summary <- summarize_area(offshore_cell)

  # starting_year identifies whether the file represents projection years 1-40 or 41-80.
  month_index_within_file <- seq_along(time)
  year <- starting_year + (month_index_within_file - 1L) %/% 12L
  calendar_month <- (month_index_within_file - 1L) %% 12L + 1L
  projection_month <- (starting_year - 1L) * 12L + month_index_within_file

  data.frame(
    year = year,
    month = calendar_month,
    projection_month = projection_month,
    mean_temp_inshore = inshore_summary$mean_temp,
    n_cells_inshore = inshore_summary$n_cells,
    mean_temp_offshore = offshore_summary$mean_temp,
    n_cells_offshore = offshore_summary$n_cells
  )
}

# Extract projection years 1-40 and 41-80
ncfname_1_40 <- file.path(process_data_dir, "CM2.6_2xCO2_USNES_bottom_temp_delta_monthly_years_1_40.nc")
ncfname_41_80 <- file.path(process_data_dir, "CM2.6_2xCO2_USNES_bottom_temp_delta_monthly_years_41_80.nc")

first_half <- extract_projected_bottom_temp_by_area(ncfname_1_40, starting_year = 1L)
second_half <- extract_projected_bottom_temp_by_area(ncfname_41_80, starting_year = 41L)

# Combine and check final output
full_data <- dplyr::bind_rows(first_half, second_half) |>
  dplyr::arrange(year, month)

if (nrow(full_data) != 80L * 12L) warning("Expected 960 monthly projection observations but found ", nrow(full_data), ".")

print(dplyr::glimpse(full_data))
print(summary(full_data$mean_temp_inshore))
print(summary(full_data$mean_temp_offshore))

# Plot projected bottom-temperature deltas by area
full_data_long <- full_data %>%
  tidyr::pivot_longer(
    cols = c(mean_temp_inshore, mean_temp_offshore),
    names_to = "area",
    values_to = "mean_temp"
  ) %>%
  dplyr::mutate(
    area = dplyr::recode(
      area,
      mean_temp_inshore = "Inshore",
      mean_temp_offshore = "Offshore"
    )
  )

# Plot projected bottom-temperature deltas by area
annual_temp <- full_data_long %>%
  dplyr::group_by(year, area) %>%
  dplyr::summarise(mean_temp = mean(mean_temp, na.rm = TRUE), .groups = "drop")

ggplot2::ggplot(annual_temp, ggplot2::aes(x = year, y = mean_temp, linetype = area)) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::labs(
    x = "Projection year",
    y = "Mean bottom temperature delta (°C)",
    linetype = "Area",
    title = "Projected Bottom Temperature Change by Fishing Area"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 11),
    axis.title = ggplot2::element_text(size = 12),
    plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
    legend.position = "bottom"
  )



# Save final dataset
writexl::write_xlsx(full_data, file.path(process_data_dir, "mean_btemp_proj_deltas_GoM_by_area.xlsx"))
