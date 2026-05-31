# Real-data validation for ThermoPheno using DWD Germany crop phenology and daily climate observations.
# Run from repository root:
#   source("validation/dwd_validate_thermopheno.R")
#
# This version fixes DWD phenology discovery for the current CDC structure:
# DWD crop phenology files are plain .txt files, not ZIP archives.

suppressPackageStartupMessages({
  library(ThermoPheno)
  library(readr)
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(tidyr)
})

options(stringsAsFactors = FALSE)

PHENO_HIST_URL   <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/phenology/annual_reporters/crops/historical/"
PHENO_RECENT_URL <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/phenology/annual_reporters/crops/recent/"
PHENO_HELP_URL   <- "https://opendata.dwd.de/climate_environment/CDC/help/"
CLIMATE_URL      <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/"

PHASE_DEF_RECENT <- paste0(PHENO_RECENT_URL, "PH_Beschreibung_Phasendefinition_Jahresmelder_Landwirtschaft_Kulturpflanze.txt")
PHASE_DEF_HIST   <- paste0(PHENO_HIST_URL,   "PH_Beschreibung_Phasendefinition_Jahresmelder_Landwirtschaft_Kulturpflanze.txt")
PHENO_STATIONS   <- paste0(PHENO_HELP_URL,   "PH_Beschreibung_Phaenologie_Stationen_Jahresmelder.txt")

cache_dir <- "validation/cache"
res_dir <- "validation/results"
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(res_dir, recursive = TRUE, showWarnings = FALSE)

message("Reading validation crop configuration...")
cfg <- readr::read_csv("validation/crop_config.csv", show_col_types = FALSE, trim_ws = TRUE) %>%
  mutate(vernalization_required = as.logical(vernalization_required))

normalise_names <- function(x) {
  names(x) <- iconv(names(x), from = "Latin1", to = "ASCII//TRANSLIT", sub = "")
  names(x) <- tolower(names(x))
  names(x) <- gsub("[^a-z0-9]+", "_", names(x))
  names(x) <- gsub("^_|_$", "", names(x))
  x
}

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

read_semicolon_table <- function(path) {
  out <- readr::read_delim(
    file = path,
    delim = ";",
    trim_ws = TRUE,
    show_col_types = FALSE,
    locale = readr::locale(encoding = "Latin1"),
    na = c("", "NA", "-999", "-9999")
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
  product_tab <- tab[grepl("produkt|product", basename(tab), ignore.case = TRUE)]
  if (length(product_tab) > 0) tab <- product_tab
  read_semicolon_table(tab[1])
}

read_dwd_table <- function(path) {
  if (grepl("\\.zip$", path, ignore.case = TRUE)) {
    read_zip_first_table(path)
  } else {
    read_semicolon_table(path)
  }
}

read_fwf_latin1 <- function(path, widths, col_names, skip = 0) {
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

haversine_km <- function(lon1, lat1, lon2, lat2) {
  r <- 6371
  to_rad <- pi / 180
  dlat <- (lat2 - lat1) * to_rad
  dlon <- (lon2 - lon1) * to_rad
  a <- sin(dlat / 2)^2 + cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(dlon / 2)^2
  2 * r * atan2(sqrt(a), sqrt(1 - a))
}

as_num <- function(x) as.numeric(gsub(",", ".", as.character(x)))
first_matching_col <- function(nm, pattern) {
  x <- nm[grepl(pattern, nm, ignore.case = TRUE)]
  if (length(x) == 0) NA_character_ else x[1]
}

message("Discovering DWD phenology files...")
pheno_hist_links <- read_index_links(PHENO_HIST_URL)
pheno_recent_links <- read_index_links(PHENO_RECENT_URL)
pheno_files <- bind_rows(
  data.frame(file = pheno_hist_links, base_url = PHENO_HIST_URL, source = "historical"),
  data.frame(file = pheno_recent_links, base_url = PHENO_RECENT_URL, source = "recent")
) %>%
  filter(grepl("PH_Jahresmelder_Landwirtschaft_Kulturpflanze", file)) %>%
  filter(!grepl("Beschreibung|Spezifizierung", file, ignore.case = TRUE))

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
    lat = as_num(lat),
    lon = as_num(lon),
    height_m = as_num(height_m),
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
  date_col <- first_matching_col(names(dat), "mess_datum|datum")
  tmin_col <- first_matching_col(names(dat), "^tnk$|tn_k|minimum|min")
  tmax_col <- first_matching_col(names(dat), "^txk$|tx_k|maximum|max")
  if (is.na(date_col) || is.na(tmin_col) || is.na(tmax_col)) {
    stop("Could not identify date/tmin/tmax columns in climate station ", station_row$station_id)
  }
  out <- data.frame(
    date = as.Date(as.character(dat[[date_col]]), "%Y%m%d"),
    tmin = as_num(dat[[tmin_col]]),
    tmax = as_num(dat[[tmax_col]]),
    station_id = station_row$station_id
  )
  out <- out %>% filter(!is.na(date), is.finite(tmin), is.finite(tmax), tmin > -80, tmax < 80)
  ThermoPheno::prepare_weather(out)
}

message("Downloading DWD phenology station metadata...")
read_pheno_station_metadata <- function() {
  f <- tryCatch(download_cached(PHENO_STATIONS), error = function(e) NA_character_)
  if (is.na(f)) return(data.frame())

  # First try as semicolon table.
  dat <- tryCatch(read_semicolon_table(f), error = function(e) data.frame())
  if (ncol(dat) > 1) {
    nm <- names(dat)
    st_col  <- first_matching_col(nm, "stations_id|station_id|stationsnummer|station")
    lat_col <- first_matching_col(nm, "breite|lat")
    lon_col <- first_matching_col(nm, "laenge|lange|lon")
    if (!is.na(st_col) && !is.na(lat_col) && !is.na(lon_col)) {
      return(dat %>% transmute(
        station_id_pheno = as.character(.data[[st_col]]),
        pheno_lat = as_num(.data[[lat_col]]),
        pheno_lon = as_num(.data[[lon_col]])
      ) %>% filter(is.finite(pheno_lat), is.finite(pheno_lon)) %>% distinct())
    }
  }

  # Fallback for fixed-width/space-aligned station metadata.
  lines <- readLines(f, warn = FALSE, encoding = "Latin1")
  lines <- iconv(lines, from = "Latin1", to = "UTF-8", sub = "")
  # Expected first numeric columns are often: station_id from until height lat lon ...
  rx <- "^\\s*([0-9]+)\\s+([0-9]{8})\\s+([0-9]{8})\\s+([-0-9,.]+)\\s+([-0-9,.]+)\\s+([-0-9,.]+)"
  m <- regexec(rx, lines)
  mm <- regmatches(lines, m)
  mm <- mm[lengths(mm) > 0]
  if (length(mm) == 0) return(data.frame())
  mat <- do.call(rbind, lapply(mm, function(z) z[2:7]))
  tmp <- data.frame(
    station_id_pheno = mat[, 1],
    v4 = as_num(mat[, 4]),
    v5 = as_num(mat[, 5]),
    v6 = as_num(mat[, 6]),
    stringsAsFactors = FALSE
  )
  # Usually v5 = latitude, v6 = longitude after height.
  tmp %>% transmute(
    station_id_pheno = as.character(station_id_pheno),
    pheno_lat = ifelse(v5 >= 45 & v5 <= 56, v5, v6),
    pheno_lon = ifelse(v6 >= 4 & v6 <= 16, v6, v5)
  ) %>% filter(is.finite(pheno_lat), is.finite(pheno_lon)) %>% distinct()
}
pheno_sites_all <- read_pheno_station_metadata()
if (nrow(pheno_sites_all) == 0) {
  stop("Could not read DWD phenology station coordinates from PH_Beschreibung_Phaenologie_Stationen_Jahresmelder.txt")
}
readr::write_csv(pheno_sites_all, file.path(res_dir, "phenology_station_metadata_used.csv"))

message("Downloading DWD phase-definition table...")
read_phase_definition <- function() {
  f <- tryCatch(download_cached(PHASE_DEF_RECENT), error = function(e) NA_character_)
  if (is.na(f)) f <- tryCatch(download_cached(PHASE_DEF_HIST), error = function(e) NA_character_)
  if (is.na(f)) return(data.frame())
  tryCatch(read_semicolon_table(f), error = function(e) data.frame())
}
phase_def <- read_phase_definition()
readr::write_csv(phase_def, file.path(res_dir, "phase_definition_raw.csv"))

join_phase_names <- function(pheno, phase_def) {
  nm <- names(pheno)
  station_col <- first_matching_col(nm, "stations_id|station_id|stationsnummer|station")
  date_col    <- first_matching_col(nm, "eintritt|datum|date")
  year_col    <- first_matching_col(nm, "referenzjahr|jahr|year")
  object_col  <- first_matching_col(nm, "objekt|object")
  phase_col   <- first_matching_col(nm, "phase|phas")
  qb_col      <- first_matching_col(nm, "qb|qualitaets_byte|qualitats_byte")

  if (is.na(date_col) || is.na(station_col) || is.na(phase_col)) {
    stop("Could not identify essential phenology columns. Available columns: ", paste(nm, collapse = ", "))
  }

  out <- pheno %>% mutate(
    station_id_pheno = as.character(.data[[station_col]]),
    date_obs = if (is.numeric(.data[[date_col]])) as.Date(as.character(.data[[date_col]]), "%Y%m%d") else as.Date(as.character(.data[[date_col]]), tryFormats = c("%Y%m%d", "%Y-%m-%d", "%d.%m.%Y")),
    year_ref = if (!is.na(year_col)) as.integer(.data[[year_col]]) else as.integer(format(date_obs, "%Y")),
    object_id_tmp = if (!is.na(object_col)) as.character(.data[[object_col]]) else NA_character_,
    phase_id_tmp = as.character(.data[[phase_col]]),
    qb_tmp = if (!is.na(qb_col)) as.character(.data[[qb_col]]) else NA_character_
  )

  if (nrow(phase_def) > 0) {
    pd <- phase_def
    pnm <- names(pd)
    pd_obj <- first_matching_col(pnm, "objekt|object")
    pd_phase <- first_matching_col(pnm, "phase|phas")
    if (!is.na(pd_phase)) {
      pd$phase_id_tmp <- as.character(pd[[pd_phase]])
      pd$object_id_tmp <- if (!is.na(pd_obj)) as.character(pd[[pd_obj]]) else NA_character_
      text_cols <- names(pd)[sapply(pd, function(z) is.character(z) || is.factor(z))]
      pd$phase_text_joined <- apply(pd[, text_cols, drop = FALSE], 1, function(z) paste(z, collapse = " | "))
      pd <- pd %>% distinct(object_id_tmp, phase_id_tmp, phase_text_joined)
      if (!all(is.na(out$object_id_tmp)) && !all(is.na(pd$object_id_tmp))) {
        out <- out %>% left_join(pd, by = c("object_id_tmp", "phase_id_tmp"))
      } else {
        out <- out %>% left_join(pd %>% select(-object_id_tmp) %>% distinct(phase_id_tmp, .keep_all = TRUE), by = "phase_id_tmp")
      }
    }
  }

  if (!"phase_text_joined" %in% names(out)) out$phase_text_joined <- NA_character_
  out$phase_text <- ifelse(is.na(out$phase_text_joined), out$phase_id_tmp, out$phase_text_joined)
  out
}

all_pairs <- list()
all_phase_candidates <- list()
all_matches <- list()

for (ii in seq_len(nrow(cfg))) {
  cc <- cfg[ii, ]
  message("Processing crop: ", cc$crop_name)

  crop_files <- pheno_files %>% filter(grepl(cc$file_pattern, file, ignore.case = TRUE))
  if (nrow(crop_files) == 0) {
    warning("No DWD phenology file matched file_pattern for ", cc$crop_name, ". Pattern was: ", cc$file_pattern)
    next
  }

  # Prefer the most recent historical full file. If absent, use the recent rolling file.
  crop_files <- crop_files %>% mutate(
    priority = case_when(
      source == "historical" & grepl("2024_hist", file) ~ 1L,
      source == "historical" & grepl("2023_hist", file) ~ 2L,
      source == "historical" ~ 3L,
      TRUE ~ 4L
    ),
    size_proxy = nchar(file)
  ) %>% arrange(priority, desc(size_proxy))

  chosen <- crop_files[1, ]
  message("  Using DWD phenology file: ", chosen$file)
  pheno_path <- download_cached(paste0(chosen$base_url, chosen$file))
  pheno_raw <- read_dwd_table(pheno_path)
  pheno <- join_phase_names(pheno_raw, phase_def)

  # Save phase candidates for manual inspection.
  cand <- pheno %>%
    group_by(object_id_tmp, phase_id_tmp, phase_text) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(crop_name = cc$crop_name) %>%
    arrange(crop_name, object_id_tmp, phase_id_tmp)
  all_phase_candidates[[cc$crop_name]] <- cand

  # Filter out clearly bad quality flags when available: 5 doubtful, 8 incorrect.
  pheno <- pheno %>% filter(is.na(qb_tmp) | !(qb_tmp %in% c("5", "8")))

  planting_obs <- pheno %>% filter(grepl(cc$planting_phase_regex, phase_text, ignore.case = TRUE), !is.na(date_obs))
  harvest_obs  <- pheno %>% filter(grepl(cc$harvest_phase_regex,  phase_text, ignore.case = TRUE), !is.na(date_obs))

  if (nrow(planting_obs) == 0 || nrow(harvest_obs) == 0) {
    warning("No planting or harvest phases matched for ", cc$crop_name, ". Check validation/results/phase_candidates.csv and edit validation/crop_config.csv.")
    next
  }

  planting_obs <- planting_obs %>% mutate(
    crop_year = if (cc$crop_type == "winter" & lubridate::month(date_obs) >= 8) lubridate::year(date_obs) + 1L else lubridate::year(date_obs)
  )
  harvest_obs <- harvest_obs %>% mutate(crop_year = lubridate::year(date_obs))

  obs <- inner_join(
    planting_obs %>% transmute(station_id_pheno, crop_year, observed_planting_date = date_obs),
    harvest_obs  %>% transmute(station_id_pheno, crop_year, observed_harvest_date  = date_obs),
    by = c("station_id_pheno", "crop_year")
  ) %>%
    distinct() %>%
    filter(crop_year >= 1991, crop_year <= 2024) %>%
    inner_join(pheno_sites_all, by = "station_id_pheno")

  if (nrow(obs) == 0) {
    warning("No paired planting-harvest observations after station-coordinate join for ", cc$crop_name)
    next
  }

  station_counts <- obs %>% count(station_id_pheno, sort = TRUE) %>% filter(n >= 8)
  obs <- obs %>% semi_join(station_counts, by = "station_id_pheno")
  if (nrow(obs) == 0) {
    warning("No station has at least 8 complete planting-harvest years for ", cc$crop_name)
    next
  }

  selected_sites <- obs %>% distinct(station_id_pheno, pheno_lat, pheno_lon) %>% head(25)

  for (ss in seq_len(nrow(selected_sites))) {
    site <- selected_sites[ss, ]
    d <- haversine_km(site$pheno_lon, site$pheno_lat, clim_meta$lon, clim_meta$lat)
    nearest <- clim_meta[which.min(d), , drop = FALSE]
    nearest$distance_km <- min(d, na.rm = TRUE)

    all_matches[[paste(cc$crop_name, site$station_id_pheno, sep = "_")]] <- data.frame(
      crop_name = cc$crop_name,
      station_id_pheno = site$station_id_pheno,
      pheno_lat = site$pheno_lat,
      pheno_lon = site$pheno_lon,
      climate_station_id = nearest$station_id,
      climate_station_name = nearest$station_name,
      climate_lat = nearest$lat,
      climate_lon = nearest$lon,
      distance_km = nearest$distance_km
    )

    weather <- tryCatch(read_climate_station(nearest), error = function(e) {
      warning("Could not read climate station ", nearest$station_id, ": ", conditionMessage(e))
      NULL
    })
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
      ), error = function(e) {
        warning("TT calibration failed for ", cc$crop_name, " / ", site$station_id_pheno, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(req)) next

    sim <- tryCatch(
      ThermoPheno::run_simulation(
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
      ), error = function(e) {
        warning("Simulation failed for ", cc$crop_name, " / ", site$station_id_pheno, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(sim)) next

    pairs <- obs_site %>%
      filter(crop_year %in% valid_years) %>%
      inner_join(sim, by = c("crop_year" = "year")) %>%
      transmute(
        crop_name = cc$crop_name,
        crop_type = cc$crop_type,
        station_id_pheno,
        climate_station_id = nearest$station_id,
        climate_station_name = nearest$station_name,
        distance_km = nearest$distance_km,
        year = crop_year,
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

matches <- if (length(all_matches)) bind_rows(all_matches) else data.frame()
readr::write_csv(matches, file.path(res_dir, "nearest_climate_station_matches.csv"))

pairs <- if (length(all_pairs)) bind_rows(all_pairs) else data.frame()
readr::write_csv(pairs, file.path(res_dir, "validation_pairs.csv"))

if (nrow(pairs) == 0) {
  writeLines(c(
    "No validation pairs were created.",
    "This usually means that the phase regex in validation/crop_config.csv did not match the DWD phase-definition text.",
    "Open validation/results/phase_candidates.csv and set planting_phase_regex / harvest_phase_regex to the relevant phase IDs or German phase labels."
  ), file.path(res_dir, "VALIDATION_SUMMARY.txt"))
  stop("No validation pairs were created. See validation/results/phase_candidates.csv and nearest_climate_station_matches.csv.")
}

metric_one <- function(df, phase) {
  e <- if (phase == "planting") df$planting_error_days else df$harvest_error_days
  e <- e[is.finite(e)]
  if (length(e) == 0) {
    return(data.frame(n = 0, mae = NA_real_, rmse = NA_real_, bias = NA_real_, medae = NA_real_, within_7_days_pct = NA_real_, within_14_days_pct = NA_real_, r2 = NA_real_))
  }
  data.frame(
    n = length(e),
    mae = mean(abs(e)),
    rmse = sqrt(mean(e^2)),
    bias = mean(e),
    medae = median(abs(e)),
    within_7_days_pct = mean(abs(e) <= 7) * 100,
    within_14_days_pct = mean(abs(e) <= 14) * 100,
    r2 = NA_real_
  )
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
