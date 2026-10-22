# Plots from the reformatted model and Kendall's tau output.
# Edit the paths and years; run format_model_output.R first.

# adjust globals - user edit here 

input_data_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/Climate-and-Catch-Dependence-in-Rec.-Fisheries/data/" 
input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/Climate-and-Catch-Dependence-in-Rec.-Fisheries/code/09_fishery_simulation/"
figure_dir <- "C:/Users/andrew.carr-harris/Desktop/Git/Climate-and-Catch-Dependence-in-Rec.-Fisheries/outputs/figures/"

library(ggplot2)
library(patchwork)

copula_levels <- c("independent", "clayton", "frank", "gaussian", "gumbel", "plackett")
decade_labels <- c("2021–2030", "2031–2040", "2041–2050", "2051–2060",
                   "2061–2070", "2071–2080", "2081–2090", "2091–2100")

decade_labels <- c("Decade 1", "Decade 2", "Decade 3", "Decade 4",
                   "Decade 5", "Decade 6", "Decade 7", "Decade 8")

plot_theme <- theme_bw(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(size = 14)
  )

copula_scale <- scale_color_discrete(name = NULL, limits = copula_levels,
                                     guide = guide_legend(nrow = 1))
combined_theme <- theme(
  legend.position = "bottom",
  legend.direction = "horizontal",
  legend.text = element_text(size = 14),
  legend.key.width = grid::unit(1.2, "cm"),
  legend.key.height = grid::unit(0.5, "cm"))

save_plot <- function(plot, name, width = 12, height = 8) {
  ggplot2::ggsave(
    filename = file.path(figure_dir, name),
    plot = plot,
    device = if (grepl("\\.eps$", name, ignore.case = TRUE)) grDevices::cairo_ps else NULL,
    width = width, height = height, units = "in", dpi = 300
    
  )
}

read_model_plot <- function(year, level) {
  
  path <- file.path(
    input_data_cd,
    paste0("model_output_y", year, "_reformat_", level, ".rds")
  )
  
  x <- readRDS(path)
  
  # Variables required for the manuscript plots.
  required <- c(
    "decade", "copula", "correlation",
    "delta_codcattrip", "delta_haddcattrip",
    "delta_codkeeptrip", "delta_haddkeeptrip",
    "diff_codcat_ind", "diff_haddcat_ind",
    "diff_codkeep_ind", "diff_haddkeep_ind",
    "diff_cv_ind", "codcat", "haddcat","ntrips_base","cv"
  )
  if (level != "coast") required <- c(required, level)
  
  missing_cols <- setdiff(required, names(x))
  
  if (length(missing_cols)) {
    stop(
      "Missing columns in ", path, ": ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  # Keep projection decades.
  x <- x[
    !is.na(x$decade) & x$decade %in% 1:8,
    ,
    drop = FALSE
  ]
  
  # Check for missing values in variables used in plots.
  plot_vars <- c(
    "delta_codcattrip", "delta_haddcattrip",
    "delta_codkeeptrip", "delta_haddkeeptrip",
    "cv"
  )
  
  na_counts <- vapply(
    x[plot_vars],
    function(z) sum(is.na(z)),
    integer(1)
  )
  
  if (any(na_counts > 0)) {
    stop(
      "Missing plot values in ", path, ": ",
      paste(
        names(na_counts)[na_counts > 0],
        na_counts[na_counts > 0],
        sep = "=",
        collapse = ", "
      )
    )
  }
  
  # Plot labels.
  x$decade <- factor(
    x$decade,
    levels = 1:8,
    labels = decade_labels
  )
  
  x$copula <- factor(
    as.character(x$copula),
    levels = copula_levels
  )
  
  if (anyNA(x$copula)) {
    stop("Unexpected copula label in ", path)
  }
  
  # State labels.
  if (level == "state") {
    x$state <- factor(
      as.character(x$state),
      levels = c("23", "33", "25"),
      labels = c("ME", "NH", "MA")
    )
    
    if (anyNA(x$state)) {
      stop("Unexpected state code in ", path)
    }
  }
  
  # Fishing-mode labels.
  if (level == "mode") {
    x$mode <- factor(
      x$mode,
      levels = c("fh", "pr"),
      labels = c("Party & Charter", "Private")
    )
    
    if (anyNA(x$mode)) {
      stop("Unexpected fishing mode in ", path)
    }
  }
  
  # Units for plots.
  # CV per baseline fishing trip ($/trip)
  x$cv_per_base_trip <- ifelse(
    !is.na(x$ntrips_base) & x$ntrips_base > 0,
    x$cv / x$ntrips_base,
    NA_real_
  )
  
  x$cv_per_trip <- ifelse(
    !is.na(x$ntrips_alt) & x$ntrips_alt > 0,
    x$cv / x$ntrips_alt,
    NA_real_
  )
  
  
  x$cv <- x$cv / 1e6
  x$codcat <- x$codcat / 1000
  x$haddcat <- x$haddcat / 1000
  x$diff_codcat_ind <- x$diff_codcat_ind / 1000
  x$diff_haddcat_ind <- x$diff_haddcat_ind / 1000
  x$diff_codkeep_ind <- x$diff_codkeep_ind / 1000
  x$diff_haddkeep_ind <- x$diff_haddkeep_ind / 1000
  x$diff_cv_ind <- x$diff_cv_ind / 1e6
  
  x
}

read_tau_plot <- function(year) {
  path <- file.path(input_data_cd, paste0("ktau_output_y", year, "_reformat_coast.rds"))
  x <- readRDS(path)
  required <- c("decade", "copula", "k_tau_catch_est", "k_tau_keep_est")
  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols)) stop("Missing columns in ", path, ": ", paste(missing_cols, collapse = ", "))
  x <- x[!is.na(x$decade) & x$decade %in% 1:8, , drop = FALSE]
  x$decade <- factor(x$decade, levels = 1:8, labels = decade_labels)
  x$copula <- factor(as.character(x$copula), levels = copula_levels)
  if (anyNA(x$copula)) stop("Unexpected tau copula label in ", path)
  x
}

boxplot_panel <- function(data, variable, label, show_strip = FALSE, zero_line = FALSE, show_legend = FALSE) {
  
  p <- ggplot(data, aes(x = copula, y = .data[[variable]], color = copula)) +
    geom_boxplot(outlier.size = 0.5, linewidth = 0.4, show.legend = show_legend) +
    facet_wrap(~decade, nrow = 1) +
    ggplot2::scale_color_discrete(
      name = NULL,
      limits = copula_levels,
      breaks = if (any(as.character(data$copula) == "independent")) copula_levels else correlated_copula_levels,
      drop = FALSE,
      guide = ggplot2::guide_legend(nrow = 1)
    ) +
    labs(x = NULL, y = label) +
    plot_theme +
    theme(strip.text = if (show_strip) element_text() else element_blank())
  
  if (zero_line) {
    p <- p + geom_hline(yintercept = 0, color = "grey40", linetype = "dashed", linewidth = 0.25)
  }
  
  p
}

#===============================================================================
# TOTAL CATCH AND WELFARE BY STATE AND MODE
#===============================================================================

outcome_levels_plot <- function(data, level) {
  
  variables <- c("codcat", "haddcat", "cv_per_trip")
  labels <- c("Cod total catch ('000s)", "Haddock total catch ('000s)",
              "CV per trip ($)")
  
  panels <- lapply(seq_along(variables), function(i) {
    
    ggplot2::ggplot(data, ggplot2::aes(x = copula, y = .data[[variables[i]]], color = copula)) +
      ggplot2::geom_boxplot(outlier.size = 0.5, linewidth = 0.4, show.legend = i == 1) +
      ggplot2::geom_hline(yintercept = 0, color = "grey40", linetype = "dashed", linewidth = 0.25) +
      ggh4x::facet_nested(
        rows = ggplot2::vars(!!rlang::sym(level)),
        cols = ggplot2::vars(decade)
      ) +
      ggplot2::scale_color_discrete(
        name = NULL, limits = copula_levels, breaks = copula_levels,
        drop = FALSE, guide = ggplot2::guide_legend(nrow = 1)
      ) +
      ggplot2::labs(x = NULL, y = labels[i]) +
      plot_theme +
      ggplot2::theme(
        strip.text.x = if (i == 1) ggplot2::element_text() else ggplot2::element_blank(),
        strip.text.y = ggplot2::element_text(size = 12)
      )
  })
  
  patchwork::wrap_plots(panels, ncol = 1, guides = "collect") & combined_theme
}

#===============================================================================
# INDEPENDENCE DIFFERENCES BY STATE AND MODE
#===============================================================================

independence_panel_group <- function(data, variable, label, level, show_strip = FALSE, show_legend = FALSE) {
  
  ggplot2::ggplot(data, ggplot2::aes(x = copula, y = .data[[variable]], color = copula)) +
    ggplot2::geom_boxplot(outlier.size = 0.5, linewidth = 0.4, show.legend = show_legend) +
    ggplot2::geom_hline(yintercept = 0, color = "grey40", linetype = "dashed", linewidth = 0.25) +
    ggh4x::facet_nested(
      rows = ggplot2::vars(!!rlang::sym(level)),
      cols = ggplot2::vars(decade)
    ) +
    ggplot2::scale_color_discrete(
      name = NULL, limits = copula_levels, breaks = correlated_copula_levels,
      drop = FALSE, guide = ggplot2::guide_legend(nrow = 1)
    ) +
    ggplot2::labs(x = NULL, y = label) +
    plot_theme +
    ggplot2::theme(
      strip.text.x = if (show_strip) ggplot2::element_text() else ggplot2::element_blank(),
      strip.text.y = ggplot2::element_text(size = 12)
    )
}

make_independence_group_plot <- function(data, level) {
  
  data <- data %>% dplyr::filter(correlation == "correlated")
  
  variables <- c("diff_codcat_ind", "diff_haddcat_ind",
                 "diff_codkeep_ind", "diff_haddkeep_ind", "diff_cv_ind")
  
  labels <- c("Cod total\ncatch ('000s)", "Haddock total\ncatch ('000s)",
              "Cod total\nharvest ('000s)", "Haddock total\nharvest ('000s)",
              "Compensating\nvariation ($M)")
  
  variables <- c("diff_codcat_ind", "diff_haddcat_ind",
                 "diff_cv_ind")
  
  labels <- c("Cod total\ncatch ('000s)", "Haddock total\ncatch ('000s)",
              "Compensating\nvariation ($M)")
  
  
  panels <- lapply(seq_along(variables), function(i) {
    independence_panel_group(
      data, variables[i], labels[i], level,
      show_strip = i == 1, show_legend = i == 1
    )
  })
  
  patchwork::wrap_plots(panels, ncol = 1, guides = "collect") +
    patchwork::plot_annotation(subtitle = "Differences (independent − dependent)") &
    combined_theme &
    ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 16, hjust = 0.5))
}
copula_levels <- c("independent", "clayton", "frank", "gaussian", "gumbel", "plackett")

correlated_copula_levels <- c(
  "clayton", "frank", "gaussian", "gumbel", "plackett"
)

for (yr in years) {
  
  coast <- read_model_plot(yr, "coast")
  tau <- read_tau_plot(yr)
  
  coast_corr <- coast |>
    dplyr::filter(correlation == "correlated")
  
  tau_corr <- tau |>
    dplyr::filter(correlation == "correlated")
  
  # catch 
  catch_plot <- patchwork::wrap_plots(
    list(
      boxplot_panel(
        coast_corr,
        "delta_codcattrip",
        "Δ cod\ncatch-per-trip",
        show_strip = TRUE,
        zero_line = TRUE
      ),
      
      boxplot_panel(
        coast_corr,
        "delta_haddcattrip",
        "Δ haddock\ncatch-per-trip",
        zero_line = TRUE
      ),
      
      boxplot_panel(
        tau,
        "k_tau_catch_est",
        "Catch correlation\n(Kendall's \u03C4)",
        zero_line = TRUE, 
        show_legend= TRUE
      ),
      
      boxplot_panel(
        coast_corr,
        "cv",
        "Compensating\nvariation ($M)",
        zero_line = TRUE
      )
    ),
    ncol = 1,
    guides = "collect"
  ) & combined_theme
  
  save_plot(
    catch_plot,
    paste0("catch_results_y", yr, ".eps")
  )
  
  save_plot(
    catch_plot,
    paste0("catch_results_y", yr, ".jpg")
  )
  
  # harvest
  harvest_plot <- patchwork::wrap_plots(
    list(
      
      boxplot_panel(
        coast_corr,
        "delta_codkeeptrip",
        "Δ cod\nharvest-per-trip",
        zero_line = TRUE, 
        show_strip = TRUE
        
      ),
      
      boxplot_panel(
        coast_corr,
        "delta_haddkeeptrip",
        "Δ haddock\nharvest-per-trip",
        zero_line = TRUE
      ),
      
      
      boxplot_panel(
        tau,
        "k_tau_keep_est",
        "Harvest correlation\n(Kendall's \u03C4)",
        zero_line = TRUE, 
        show_legend= TRUE
      ),
      
      boxplot_panel(
        coast_corr,
        "cv",
        "Compensating\nvariation ($M)",
        zero_line = TRUE
      )
    ),
    ncol = 1,
    guides = "collect"
  ) & combined_theme
  
  save_plot(
    harvest_plot,
    paste0("harvest_results_y", yr, ".eps")
  )
  
  save_plot(
    harvest_plot,
    paste0("harvest_results_y", yr, ".jpg")
  )
  
  independence_plot <- patchwork::wrap_plots(
    list(
      boxplot_panel(
        coast_corr,
        "diff_codcat_ind",
        "Cod total \ncatch ('000s)",
        show_strip = TRUE,
        zero_line = TRUE,
        show_legend= TRUE
      ),

      boxplot_panel(
        coast_corr,
        "diff_haddcat_ind",
        "Haddock total \ncatch ('000s)",
        zero_line = TRUE
      ),

      boxplot_panel(
        coast_corr,
        "diff_codkeep_ind",
        "Cod total \nharvest ('000s)",
        zero_line = TRUE
      ),

      boxplot_panel(
        coast_corr,
        "diff_haddkeep_ind",
        "Haddock total \nharvest ('000s)",
        zero_line = TRUE
      ),

      boxplot_panel(
        coast_corr,
        "diff_cv_ind",
        "Compensating\nvariation ($M)",
        zero_line = TRUE
      )
    ),
    ncol = 1,
    guides = "collect"
  )   +
    patchwork::plot_annotation(
      subtitle = "Differences (independent − dependent)"
    ) &
    combined_theme &
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(size = 16, hjust = 0.5)
    )

  save_plot(
    independence_plot,
    paste0("independence_differences_y", yr, ".eps")
  )

  save_plot(
    independence_plot,
    paste0("independence_differences_y", yr, ".jpg")
  )
  
  #===============================================================================
  # STATE- AND MODE-LEVEL FIGURES
  #===============================================================================
  if (yr==2024){
  for (level in c("mode", "state")) {

    x <- read_model_plot(yr, level)

    # 1. Total catch and CV (including independent scenario)
    levels_plot <- outcome_levels_plot(x, level)

    save_plot(levels_plot, paste0("catch_cv_by_", level, "_y", yr, ".eps"),
              width = 14, height = 11)

    save_plot(levels_plot, paste0("catch_cv_by_", level, "_y", yr, ".jpg"),
              width = 14, height = 11)

    # 2. Independence differences (independent minus correlated)
    independence_group_plot <- make_independence_group_plot(x, level)

    save_plot(independence_group_plot, paste0("independence_differences_by_", level, "_y", yr, ".eps"),
              width = 14, height = 11)

    save_plot(independence_group_plot, paste0("independence_differences_by_", level, "_y", yr, ".jpg"),
              width = 14, height = 11)
  }
  }
}


#===============================================================================
# BOTTOM TEMPERATURE PLOT
#===============================================================================

temp_raw <- data.table::as.data.table(
  readr::read_csv(
    file.path(input_data_cd, "decadal_bottom_temp_distributions_levels.csv"),
    show_col_types = FALSE
  )
)

if ("...1" %in% names(temp_raw)) temp_raw[, `...1` := NULL]

# Convert inshore/offshore columns to long format.
temp_plot_data <- data.table::rbindlist(list(
  temp_raw[, .(month, decade, area = "Inshore",
               mean_temp = average_temp_inshore, sd_temp = sd_temp_inshore)],
  temp_raw[, .(month, decade, area = "Offshore",
               mean_temp = average_temp_offshore, sd_temp = sd_temp_offshore)]
))

temp_plot_data[, `:=`(
  lower = mean_temp - 1.96*sd_temp,
  upper = mean_temp + 1.96*sd_temp
)]

# Format decade and area labels.
temp_plot_data[, decade := factor(as.integer(as.character(decade)),
                                  levels = 1:8, labels = decade_labels)]
temp_plot_data[, area := factor(area, levels = c("Inshore", "Offshore"))]

# Average temperature across all 12 months, by decade and area.
temp_decadal_means <- temp_plot_data |>
  dplyr::group_by(decade, area) |>
  dplyr::summarise(
    decadal_mean = mean(mean_temp, na.rm = TRUE),
    .groups = "drop"
  )

# Plot monthly means +/- SD, with decadal means as dashed lines.
temp_panel <- ggplot(temp_plot_data, aes(x = month, y = mean_temp)) +
  geom_hline(
    data = temp_decadal_means,
    aes(yintercept = decadal_mean),
    color = "red",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.25,
    linewidth = 0.3,
    color = "grey40"
  ) +
  geom_point(size = 1.2, color = "black") +
  facet_grid(area ~ decade) +
  scale_x_continuous(
    breaks = 1:12,
    labels = ifelse(1:12 %% 2 == 0, as.character(1:12), ""),
    limits = c(0.5, 12.5)
  ) +
  scale_y_continuous(
    breaks = seq(-5, 30, by = 2),
    minor_breaks = seq(-5, 30, by = 1)
  ) +
  labs(x = "Month", y = "Bottom temperature (°C)") +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor.y = element_line(color = "grey92", linewidth = 0.2),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.ticks.x = element_line(),
    strip.text = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "none"
  )

temp_panel
save_plot(temp_panel, paste0("proj_tmps_inshore_offshore.eps"))
