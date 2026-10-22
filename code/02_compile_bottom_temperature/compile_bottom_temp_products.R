
# This script combines data from the newer and the older bottom products. 
# The full time series is composed of:
  # the older product through 1959-1992
  # the newer product 1993-2026

# The climatology bias correction is based on the newer product

# OpenAI ChatGPT (GPT-5.6 Sol) was used to optimize this code. 

# Set global directories - user edit here
project_dir<-"C:/Users/andrew.carr-harris/Desktop/Git/Climate-and-Catch-Dependence-in-Rec.-Fisheries"

process_data_dir <- file.path(project_dir,"data")

final_bottom_tempfile <- file.path(process_data_dir, "daily_btemp_GoM_by_area_combined_1959_2026.xlsx")


#########
old_file <- file.path(process_data_dir,"daily_btemp_GoM_by_area_1959_2022.xlsx")
new_file <- file.path(process_data_dir,"daily_btemp_GoM_by_area_1993_2026.xlsx")

old_daily <- readxl::read_xlsx(old_file)
new_daily <- readxl::read_xlsx(new_file)

prepare_monthly_temperature <- function(data, source_name) {
  data |>
    dplyr::mutate(date = as.Date(date)) |>
    tidyr::pivot_longer(
      cols = c(mean_tmp_inshore, mean_tmp_offshore),
      names_to = "area",
      names_pattern = "mean_tmp_(.*)",
      values_to = "temperature"
    ) |>
    dplyr::group_by(year, month, area) |>
    dplyr::summarise(
      temperature = mean(temperature, na.rm = TRUE),
      n_days = sum(!is.na(temperature)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      source = source_name,
      expected_days = lubridate::days_in_month(as.Date(sprintf("%d-%02d-01", year, month))),
      complete_month = n_days == expected_days
    )
}

old_monthly <- prepare_monthly_temperature(old_daily, "Older product")
new_monthly <- prepare_monthly_temperature(new_daily, "Newer product")


overlap_monthly <- old_monthly |>
  dplyr::filter(year >= 1993, year <= 2021) |>
  dplyr::select(year, month, area, temperature_old = temperature) |>
  dplyr::inner_join(
    new_monthly |>
      dplyr::filter(year >= 1993, year <= 2021) |>
      dplyr::select(year, month, area, temperature_new = temperature),
    by = c("year", "month", "area")
  ) |>
  dplyr::mutate(difference = temperature_new - temperature_old)

overlap_summary <- overlap_monthly |>
  dplyr::group_by(area) |>
  dplyr::summarise(
    n_months = dplyr::n(),
    mean_difference = mean(difference),
    sd_difference = stats::sd(difference),
    rmse = sqrt(mean(difference^2)),
    correlation = stats::cor(temperature_old, temperature_new),
    .groups = "drop"
  )

print(overlap_summary)

overlap_long <- overlap_monthly |>
  dplyr::mutate(date = as.Date(sprintf("%d-%02d-01", year, month))) |>
  tidyr::pivot_longer(
    cols = c(temperature_old, temperature_new),
    names_to = "product",
    values_to = "temperature"
  ) |>
  dplyr::mutate(
    product = dplyr::recode(
      product,
      temperature_old = "Older product",
      temperature_new = "Newer product"
    )
  )

p_overlap <- ggplot2::ggplot(
  overlap_long,
  ggplot2::aes(x = date, y = temperature, colour = product)
) +
  ggplot2::geom_line(linewidth = 0.5, alpha = 0.8) +
  ggplot2::facet_wrap(~area, ncol = 1, scales = "free_y") +
  ggplot2::scale_colour_manual(values = c("Older product" = "#0072B2", "Newer product" = "#D55E00")) +
  ggplot2::labs(x = NULL, y = "Monthly mean bottom temperature (°C)", colour = "Dataset") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom")

p_overlap

old_file <- file.path(process_data_dir,"daily_btemp_GoM_by_area_1959_2022.xlsx")
new_file <- file.path(process_data_dir,"daily_btemp_GoM_by_area_1993_2026.xlsx")

old_temperature <- readxl::read_excel(old_file) |>
  dplyr::mutate(date = as.Date(date))

new_temperature <- readxl::read_excel(new_file) |>
  dplyr::mutate(date = as.Date(date))

if (!identical(names(old_temperature), names(new_temperature))) {
  stop(
    paste0(
      "The two datasets do not have identical columns.\n",
      "Only in old data: ",
      paste(setdiff(names(old_temperature), names(new_temperature)), collapse = ", "),
      "\nOnly in new data: ",
      paste(setdiff(names(new_temperature), names(old_temperature)), collapse = ", ")
    )
  )
}

# First date available in the newer product is the transition date.
new_product_start <- min(new_temperature$date, na.rm = TRUE)

message("New product begins on ", new_product_start)
message("Old product will be retained only through ", new_product_start - 1L)

old_temperature_pre_new <- old_temperature |>
  dplyr::filter(date < new_product_start) |>
  dplyr::mutate(source_product = "Older product: 1959–2022")

new_temperature_retained <- new_temperature |>
  dplyr::mutate(source_product = "Newer product: 1993–2026")

bottom_temperature_combined <- dplyr::bind_rows(
  old_temperature_pre_new,
  new_temperature_retained
) |>
  dplyr::arrange(date) |>
  dplyr::relocate(source_product, .after = date)

source_summary <- bottom_temperature_combined |>
  dplyr::group_by(source_product) |>
  dplyr::summarise(
    first_date = min(date),
    last_date = max(date),
    n_dates = dplyr::n_distinct(date),
    n_rows = dplyr::n(),
    .groups = "drop"
  )

print(source_summary)

duplicate_dates <- bottom_temperature_combined |>
  dplyr::count(date, name = "n_rows") |>
  dplyr::filter(n_rows > 1L)

if (nrow(duplicate_dates) > 0L) {
  print(duplicate_dates)
  stop("The combined dataset contains duplicated dates.")
} else {
  message("No duplicated dates were found.")
}

expected_dates <- tibble::tibble(
  date = seq(
    min(bottom_temperature_combined$date),
    max(bottom_temperature_combined$date),
    by = "day"
  )
)

missing_dates <- expected_dates |>
  dplyr::anti_join(
    bottom_temperature_combined |>
      dplyr::distinct(date),
    by = "date"
  )

if (nrow(missing_dates) > 0L) {
  message(nrow(missing_dates), " dates are missing from the combined series.")
  print(missing_dates)
} else {
  message("The combined daily series has no missing calendar dates.")
}

if (any(
  bottom_temperature_combined$date < new_product_start &
  bottom_temperature_combined$source_product != "Older product: 1959–2022"
)) {
  stop("At least one pre-1993 date was assigned to the wrong product.")
}

if (any(
  bottom_temperature_combined$date >= new_product_start &
  bottom_temperature_combined$source_product != "Newer product: 1993–2026"
)) {
  stop("At least one date from 1993 onward was assigned to the wrong product.")
}

# save the combined temperature data
openxlsx::write.xlsx(
  bottom_temperature_combined,
  file = final_bottom_tempfile,
  overwrite = TRUE
)
