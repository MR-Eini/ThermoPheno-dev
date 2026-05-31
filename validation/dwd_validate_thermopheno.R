# Real-data validation for ThermoPheno using DWD Germany crop phenology and daily climate observations.
# Run from repository root:
#   source("validation/dwd_validate_thermopheno.R")

suppressPackageStartupMessages({
  library(ThermoPheno)
  library(readr)
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(tidyr)
})

options(stringsAsFactors = FALSE)

PHENO_HIST_URL <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/phenology/annual_reporters/crops/historical/"
PHENO_RECENT_URL <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/phenology/annual_reporters/crops/recent/"
CLIMATE_URL <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/"

cache_dir <- "validation/cache"
res_dir <- "validation/results"
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(res_dir, recursive = TRUE, showWarnings = FALSE)

message("Reading validation crop configuration...")
cfg <- readr::read_csv("validation/crop_config.csv", show_col_types = FALSE, trim_ws = TRUE) %>%
  mutate(vernalization_required = as.logical(vernalization_required))

read_index_links <- function(url) {
  html <- paste(readLines(url, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  href <- regmatches(html, gregexpr('href="[^"]+"', html))[[1]]
  href <- gsub('^href="|"$', '', href)
  href[grepl("\\.zip$|\\.txt$|\\.TXT$|\\.csv$|\\.CSV$", href)]
}

download_cached <- function(url, dest_dir = cache_dir) {
  dest <- file.path(dest_dir, basename(url))
  if (!file.exists(dest)) {
    message("Downloading: ", basename(url))
    utils::download.file(url, dest, mode = "wb", quiet = TRUE)
  }
  dest
}

haversine_km <- function(lon1, lat1, lon2, lat2) {
  r <- 6371
  to_rad <- pi / 180
  dlat <- (lat2 - lat1) * to_rad
  dlon <- (lon2 - lon1) * to_rad
  a <- sin(dlat / 2)^2 + cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(dlon / 2)^2
  2 * r * atan2(sqrt(a), sqrt(1 - a))
}

normalise_names <- function(x) {
  names(x) <- tolower(names(x))
  names(x) <- gsub("[^a-z0-9]+", "_", names(x))
  x
}

read_semicolon_table <- function(path) {
  out <- tryCatch(
    readr::read_delim(path, delim = ";", trim_ws = TRUE, show_col_types = FALSE, locale = locale(encoding = "Latin1")),
    error = function(e) readr::read_delim(path, delim = ";", trim_ws = TRUE, show_col_types = FALSE)
  )
  normalise_names(as.data.frame(out))
}

read_zip_first_table <- function(zip_path) {
  td <- tempfile("dwdzip")
  dir.create(td)
  utils::unzip(zip_path, exdir = td)
  files <- list.files(td, recursive = TRUE, full.names = TRUE)
  tab <- files[grepl("\\.(txt|csv|TXT|CSV)$", files)]
  if (length(tab) == 0) stop("No table file found in ", zip_path)

  # DWD ZIP archives often include metadata text files in addition to the
  # actual observation table. Prefer the product table when it is present.
  product_tab <- tab[grepl("produkt|product", basename(tab), ignore.case = TRUE)]
  if (length(product_tab) > 0) tab <- product_tab

  read_semicolon_table(tab[1])
}

read_fwf_latin1 <- function(path, widths, col_names, skip = 0) {
  # DWD station-description files are commonly encoded as Latin-1 / Windows-1252.
  # Base read.fwf() may fail on characters such as ü; readr handles the encoding safely.
  spec <- readr::fwf_widths(widths, col_names = col_names)
  out <- readr::read_fwf(
    file = path,
    col_positions = spec,
    skip = skip,
    locale = readr::locale(encoding = "Latin1"),
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE,
    show_col_types = FALSE
  )
  normalise_names(as.data.frame(out))
}

message("Discovering DWD phenology files...")
pheno_links <- unique(c(read_index_links(PHENO_HIST_URL), read_index_links(PHENO_RECENT_URL)))
pheno_zip_links <- pheno_links[grepl("\\.zip$", pheno_links)]

message("Discovering DWD climate files...")
climate_links <- read_index_links(CLIMATE_URL)
climate_zip_links <- climate_links[grepl("tageswerte_KL_.*_hist\\.zip$", climate_links)]
station_desc_link <- climate_links[grepl("KL_Tageswerte_Beschreibung_Stationen", climate_links)][1]

message("Downloading climate station metadata...")
station_desc_file <- download_cached(paste0(CLIMATE_URL, station_desc_link))
clim_meta_raw <- read_fwf_latin1(
  station_desc_file,
  widths = c(6, 9, 9, 15, 12, 10, 41, 99),
  col_names = c("station_id", "date_start", "date_end", "height_m", "lat", "lon", "station_name", "state"),
  skip = 2
)
clim_meta <- clim_meta_raw %>%
  mutate(
    station_id = sprintf("%05d", as.integer(trimws(station_id))),
    lat = as.numeric(gsub(",", ".", lat)),
    lon = as.numeric(gsub(",", ".", lon)),
    height_m = as.numeric(gsub(",", ".", height_m)),
    date_start = as.Date(as.character(date_start), "%Y%m%d"),
    date_end = as.Date(as.character(date_end), "%Y%m%d")
  ) %>%
  filter(!is.na(lat), !is.na(lon), date_start <= as.Date("2011-01-01"), date_end >= as.Date("2020-12-31"))

extract_station_id_from_climate_zip <- function(x) sub(".*tageswerte_KL_([0-9]{5})_.*", "\\1", x)
clim_available <- data.frame(
  climate_zip = climate_zip_links,
  station_id = extract_station_id_from_climate_zip(climate_zip_links),
  stringsAsFactors = FALSE
)
clim_meta <- clim_meta %>% inner_join(clim_available, by = "station_id")

read_climate_station <- function(station_row) {
  zip_file <- download_cached(paste0(CLIMATE_URL, station_row$climate_zip))
  dat <- read_zip_first_table(zip_file)
  # Typical DWD columns: mess_datum, tm_k, txk, tnk. Names are normalized above.
  date_col <- names(dat)[grepl("mess_datum|datum", names(dat))][1]
  tmin_col <- names(dat)[grepl("tnk|tn_k|minimum", names(dat))][1]
  tmax_col <- names(dat)[grepl("txk|tx_k|maximum", names(dat))][1]
  if (is.na(date_col) || is.na(tmin_col) || is.na(tmax_col)) {
    stop("Could not identify date/tmin/tmax columns in climate station ", station_row$station_id)
  }
  out <- data.frame(
    date = as.Date(as.character(dat[[date_col]]), "%Y%m%d"),
    tmin = as.numeric(dat[[tmin_col]]),
    tmax = as.numeric(dat[[tmax_col]]),
    station_id = station_row$station_id
  )
  out <- out %>% filter(!is.na(date), is.finite(tmin), is.finite(tmax), tmin > -80, tmax < 80)
  ThermoPheno::prepare_weather(out)
}

all_pairs <- list()
all_phase_candidates <- list()

for (ii in seq_len(nrow(cfg))) {
  cc <- cfg[ii, ]
  message("Processing crop: ", cc$crop_name)
  crop_links <- pheno_zip_links[grepl(cc$file_pattern, pheno_zip_links, ignore.case = TRUE)]
  if (length(crop_links) == 0) {
    warning("No phenology zip matched file_pattern for ", cc$crop_name)
    next
  }

  # Prefer large historical files; fall back to recent.
  crop_link <- crop_links[which.max(nchar(crop_links))]
  pheno_file <- download_cached(paste0(ifelse(crop_link %in% read_index_links(PHENO_HIST_URL), PHENO_HIST_URL, PHENO_RECENT_URL), crop_link))
  pheno <- read_zip_first_table(pheno_file)

  # Try to detect station coordinates and phase/object/date columns.
  date_col <- names(pheno)[grepl("eintritt|datum|date", names(pheno))][1]
  year_col <- names(pheno)[grepl("referenzjahr|jahr|year", names(pheno))][1]
  station_col <- names(pheno)[grepl("stations_id|station_id|stationsnummer|station", names(pheno))][1]
  phase_col <- names(pheno)[grepl("phase|phaenophase|phas", names(pheno))][1]
  phase_name_col <- names(pheno)[grepl("phase.*name|beschreibung|phas.*name", names(pheno))][1]
  lat_col <- names(pheno)[grepl("breite|lat", names(pheno))][1]
  lon_col <- names(pheno)[grepl("laenge|l_ngrad|lon", names(pheno))][1]

  if (is.na(date_col) || is.na(station_col)) {
    warning("Could not identify essential phenology columns for ", cc$crop_name, ". Inspect the raw DWD file.")
    next
  }

  # Store possible phase candidates for manual inspection.
  if (!is.na(phase_col)) {
    cand <- pheno %>%
      group_by(.data[[phase_col]]) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(crop_name = cc$crop_name, phase_col = phase_col)
    if (!is.na(phase_name_col)) cand$phase_name <- as.character(cand[[phase_col]])
    all_phase_candidates[[cc$crop_name]] <- cand
  }

  pheno$date_obs <- if (is.numeric(pheno[[date_col]])) as.Date(as.character(pheno[[date_col]]), "%Y%m%d") else as.Date(pheno[[date_col]])
  pheno$station_id_pheno <- as.character(pheno[[station_col]])
  pheno$year_ref <- if (!is.na(year_col)) as.integer(pheno[[year_col]]) else as.integer(format(pheno$date_obs, "%Y"))
  pheno$phase_text <- if (!is.na(phase_name_col)) as.character(pheno[[phase_name_col]]) else if (!is.na(phase_col)) as.character(pheno[[phase_col]]) else ""

  planting_obs <- pheno %>% filter(grepl(cc$planting_phase_regex, phase_text, ignore.case = TRUE), !is.na(date_obs))
  harvest_obs <- pheno %>% filter(grepl(cc$harvest_phase_regex, phase_text, ignore.case = TRUE), !is.na(date_obs))

  if (nrow(planting_obs) == 0 || nrow(harvest_obs) == 0) {
    warning("No planting or harvest phases matched for ", cc$crop_name, ". Check phase_candidates.csv and edit crop_config.csv.")
    next
  }

  obs <- inner_join(
    planting_obs %>% transmute(station_id_pheno, year_ref, observed_planting_date = date_obs),
    harvest_obs %>% transmute(station_id_pheno, year_ref, observed_harvest_date = date_obs),
    by = c("station_id_pheno", "year_ref")
  ) %>% distinct()

  obs <- obs %>% filter(year_ref >= 1991, year_ref <= 2024)
  if (nrow(obs) == 0) next

  # Get phenology station coordinates if present. If not present, skip nearest-station validation.
  if (is.na(lat_col) || is.na(lon_col)) {
    warning("Phenology station coordinates not detected for ", cc$crop_name, ". Cannot match climate stations automatically.")
    next
  }

  pheno_sites <- pheno %>%
    transmute(station_id_pheno = as.character(.data[[station_col]]),
              pheno_lat = as.numeric(.data[[lat_col]]),
              pheno_lon = as.numeric(.data[[lon_col]])) %>%
    filter(is.finite(pheno_lat), is.finite(pheno_lon)) %>%
    distinct()

  obs <- obs %>% inner_join(pheno_sites, by = "station_id_pheno")
  station_counts <- obs %>% count(station_id_pheno, sort = TRUE) %>% filter(n >= 10)
  obs <- obs %>% semi_join(station_counts, by = "station_id_pheno")
  if (nrow(obs) == 0) next

  selected_sites <- obs %>% distinct(station_id_pheno, pheno_lat, pheno_lon) %>% head(25)

  for (ss in seq_len(nrow(selected_sites))) {
    site <- selected_sites[ss, ]
    d <- haversine_km(site$pheno_lon, site$pheno_lat, clim_meta$lon, clim_meta$lat)
    nearest <- clim_meta[which.min(d), , drop = FALSE]
    nearest$distance_km <- min(d, na.rm = TRUE)

    weather <- tryCatch(read_climate_station(nearest), error = function(e) NULL)
    if (is.null(weather)) next

    obs_site <- obs %>% filter(station_id_pheno == site$station_id_pheno)
    calib_years <- 1991:2010
    valid_years <- 2011:2024

    req <- tryCatch(
      ThermoPheno::estimate_required_tt(
        weather = weather,
        baseline_years = calib_years,
        planting_mmdd = cc$baseline_planting_mmdd,
        days_to_maturity = cc$days_to_maturity,
        t_base = cc$t_base,
        t_opt = cc$t_opt,
        t_max_cut = cc$t_max_cut,
        tt_mode = "triangular",
        crop_type = cc$crop_type,
        vernalization_required = isTRUE(cc$vernalization_required),
        vernalization_days_required = cc$vernalization_days_required
      ), error = function(e) NULL
    )
    if (is.null(req)) next

    sim <- ThermoPheno::run_simulation(
      weather = weather,
      crop_name = cc$crop_name,
      required_tt = req$required_tt,
      earliest_planting_mmdd = cc$earliest_planting_mmdd,
      latest_planting_mmdd = cc$latest_planting_mmdd,
      latest_harvest_mmdd = cc$latest_harvest_mmdd,
      t_base = cc$t_base,
      t_opt = cc$t_opt,
      t_max_cut = cc$t_max_cut,
      tt_mode = "triangular",
      crop_type = cc$crop_type,
      min_mean_temp_plant = cc$min_mean_temp_plant,
      forced_harvest_allowed = TRUE,
      min_fraction_tt_for_forced_harvest = 0.8,
      vernalization_required = isTRUE(cc$vernalization_required),
      vernalization_days_required = cc$vernalization_days_required,
      winter_plant_temp_min = cc$winter_plant_temp_min,
      winter_plant_temp_max = cc$winter_plant_temp_max
    )

    pairs <- obs_site %>%
      filter(year_ref %in% valid_years) %>%
      inner_join(sim, by = c("year_ref" = "year")) %>%
      transmute(
        crop_name = cc$crop_name,
        crop_type = cc$crop_type,
        station_id_pheno,
        climate_station_id = nearest$station_id,
        climate_station_name = nearest$station_name,
        distance_km = nearest$distance_km,
        year = year_ref,
        observed_planting_date,
        simulated_planting_date = planting_date,
        planting_error_days = as.numeric(simulated_planting_date - observed_planting_date),
        observed_harvest_date,
        simulated_harvest_date = harvest_date,
        harvest_error_days = as.numeric(simulated_harvest_date - observed_harvest_date),
        status,
        required_tt = req$required_tt
      )
    all_pairs[[paste(cc$crop_name, site$station_id_pheno, sep = "_")]] <- pairs
  }
}

phase_candidates <- if (length(all_phase_candidates)) bind_rows(all_phase_candidates) else data.frame()
readr::write_csv(phase_candidates, file.path(res_dir, "phase_candidates.csv"))

pairs <- if (length(all_pairs)) bind_rows(all_pairs) else data.frame()
readr::write_csv(pairs, file.path(res_dir, "validation_pairs.csv"))

if (nrow(pairs) == 0) {
  writeLines("No validation pairs were created. Inspect phase_candidates.csv and adjust validation/crop_config.csv.", file.path(res_dir, "VALIDATION_SUMMARY.txt"))
  stop("No validation pairs were created. See validation/results/phase_candidates.csv.")
}

metric_one <- function(df, phase) {
  if (phase == "planting") {
    e <- df$planting_error_days
  } else {
    e <- df$harvest_error_days
  }
  e <- e[is.finite(e)]
  if (length(e) == 0) return(data.frame(n = 0, mae = NA_real_, rmse = NA_real_, bias = NA_real_, medae = NA_real_, within_7_days_pct = NA_real_, within_14_days_pct = NA_real_))
  data.frame(n = length(e), mae = mean(abs(e)), rmse = sqrt(mean(e^2)), bias = mean(e), medae = median(abs(e)), within_7_days_pct = mean(abs(e) <= 7) * 100, within_14_days_pct = mean(abs(e) <= 14) * 100)
}

metrics <- pairs %>%
  group_by(crop_name) %>%
  group_modify(~ bind_rows(
    cbind(phase = "planting", metric_one(.x, "planting")),
    cbind(phase = "harvest", metric_one(.x, "harvest"))
  )) %>% ungroup()

metrics_station <- pairs %>%
  group_by(crop_name, station_id_pheno, climate_station_id) %>%
  group_modify(~ bind_rows(
    cbind(phase = "planting", metric_one(.x, "planting")),
    cbind(phase = "harvest", metric_one(.x, "harvest"))
  )) %>% ungroup()

readr::write_csv(metrics, file.path(res_dir, "validation_metrics.csv"))
readr::write_csv(metrics_station, file.path(res_dir, "validation_metrics_by_station.csv"))

p1 <- ggplot(pairs, aes(observed_planting_date, simulated_planting_date, colour = crop_name)) +
  geom_point(alpha = 0.7) + geom_abline(slope = 1, intercept = 0, linetype = 2) +
  labs(x = "Observed planting date", y = "Simulated planting date", colour = "Crop") +
  theme_minimal(base_size = 12)

ggsave(file.path(res_dir, "validation_scatter_planting.png"), p1, width = 8, height = 6, dpi = 300)

p2 <- ggplot(pairs, aes(observed_harvest_date, simulated_harvest_date, colour = crop_name)) +
  geom_point(alpha = 0.7) + geom_abline(slope = 1, intercept = 0, linetype = 2) +
  labs(x = "Observed harvest date", y = "Simulated harvest date", colour = "Crop") +
  theme_minimal(base_size = 12)

ggsave(file.path(res_dir, "validation_scatter_harvest.png"), p2, width = 8, height = 6, dpi = 300)

summary_lines <- c(
  "ThermoPheno DWD validation summary",
  paste("Run date:", Sys.Date()),
  paste("Validation pairs:", nrow(pairs)),
  "",
  capture.output(print(metrics))
)
writeLines(summary_lines, file.path(res_dir, "VALIDATION_SUMMARY.txt"))
message("DWD validation completed. Results written to ", res_dir)
