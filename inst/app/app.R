library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(ggplot2)
library(DT)
library(readr)
library(ThermoPheno)

options(shiny.maxRequestSize = 200 * 1024^2)

load_example_weather <- function() {
  f <- system.file("extdata", "Germany_historical_1981_2010_dummy_data.csv", package = "ThermoPheno")
  ThermoPheno::prepare_weather(readr::read_csv(f, show_col_types = FALSE))
}

summarise_results <- function(x) {
  if (nrow(x) == 0) return(data.frame())
  x %>%
    mutate(group_label = ifelse(is.na(scenario), dataset, paste(dataset, scenario, model, sep = " | "))) %>%
    group_by(group_label) %>%
    summarise(
      n_years = n(),
      mature_pct = round(mean(status == "mature", na.rm = TRUE) * 100, 1),
      forced_harvest_pct = round(mean(status == "forced_harvest_immature", na.rm = TRUE) * 100, 1),
      failed_pct = round(mean(status %in% c("failed_to_mature", "insufficient_vernalization", "not_planted"), na.rm = TRUE) * 100, 1),
      median_planting_doy = round(median(yday(planting_date), na.rm = TRUE), 1),
      median_harvest_doy = round(median(yday(harvest_date), na.rm = TRUE), 1),
      median_season_length = round(median(season_length_days, na.rm = TRUE), 1),
      mean_maturity_fraction = round(mean(maturity_fraction, na.rm = TRUE), 3),
      .groups = "drop"
    )
}

plot_timing <- function(x) {
  if (is.null(x) || nrow(x) == 0) return(ggplot() + theme_void() + annotate("text", x = 0, y = 0, label = "No results."))
  ev <- bind_rows(
    x %>% filter(!is.na(planting_date)) %>% transmute(dataset, scenario, model, operation = "Planting", doy = yday(planting_date)),
    x %>% filter(!is.na(harvest_date)) %>% transmute(dataset, scenario, model, operation = "Harvest", doy = yday(harvest_date))
  ) %>% mutate(group_label = ifelse(is.na(scenario), dataset, paste(dataset, scenario, model, sep = " | ")))
  if (nrow(ev) == 0) return(ggplot() + theme_void() + annotate("text", x = 0, y = 0, label = "No event dates."))
  ggplot(ev, aes(x = group_label, y = doy, fill = operation)) +
    geom_boxplot(outlier.alpha = 0.4) +
    coord_flip() +
    labs(x = NULL, y = "Day of year", fill = NULL, title = "Planting and harvest timing") +
    theme_minimal(base_size = 13)
}

plot_temperature_cycle <- function(weather, future = NULL) {
  hw <- weather %>% mutate(group_label = "Historical", month = month(date))
  if (!is.null(future)) {
    fw <- future %>% mutate(group_label = ifelse(is.na(scenario), "Climate", paste(scenario, model, sep = " | ")), month = month(date))
    df <- bind_rows(hw, fw)
  } else df <- hw
  monthly <- df %>% group_by(group_label, month) %>% summarise(mean_tmean = mean(tmean), .groups = "drop")
  ggplot(monthly, aes(month, mean_tmean, group = group_label, colour = group_label)) +
    geom_line(linewidth = 0.8) +
    geom_point() +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    labs(x = NULL, y = "Mean daily temperature (°C)", colour = NULL, title = "Mean monthly temperature cycle") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")
}

ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#146C94"),
  title = "ThermoPheno: thermal-time crop phenology",
  sidebar = sidebar(
    width = 360,
    fileInput("weather_file", "Historical weather CSV", accept = ".csv"),
    checkboxInput("use_example", "Use bundled example if no file is uploaded", value = TRUE),
    fileInput("climate_file", "Optional climate scenario CSV", accept = ".csv"),
    hr(),
    selectInput("crop_type", "Crop type", choices = c("summer", "winter"), selected = "summer"),
    textInput("crop_name", "Crop name", value = "Maize"),
    numericInput("days_to_maturity", "Reference days to maturity", value = 140, min = 1),
    textInput("baseline_planting_mmdd", "Reference planting date (MM-DD)", value = "04-15"),
    textInput("earliest_planting_mmdd", "Earliest planting date (MM-DD)", value = "03-15"),
    textInput("latest_planting_mmdd", "Latest planting date (MM-DD)", value = "05-31"),
    textInput("latest_harvest_mmdd", "Latest harvest date (MM-DD)", value = "10-01"),
    hr(),
    selectInput("tt_mode", "Thermal-time method", choices = c("simple", "capped", "triangular"), selected = "triangular"),
    numericInput("t_base", "Base temperature (°C)", value = 8),
    numericInput("t_opt", "Optimum temperature (°C)", value = 25),
    numericInput("t_max_cut", "Upper cutoff (°C)", value = 35),
    numericInput("min_mean_temp_plant", "Summer planting threshold (°C)", value = 8),
    conditionalPanel("input.crop_type == 'winter'",
      numericInput("winter_plant_temp_min", "Winter planting min (°C)", value = 5),
      numericInput("winter_plant_temp_max", "Winter planting max (°C)", value = 15),
      checkboxInput("vernalization_required", "Require vernalization", value = TRUE),
      numericInput("vernalization_days_required", "Vernalization days", value = 45),
      numericInput("winter_dormancy_temp", "Dormancy temp (°C)", value = 0),
      numericInput("spring_regrowth_temp", "Spring regrowth temp (°C)", value = 5)
    ),
    checkboxInput("forced_harvest_allowed", "Allow forced harvest", value = TRUE),
    numericInput("min_fraction_tt_for_forced_harvest", "Minimum maturity fraction for forced harvest", value = 0.8, min = 0, max = 1, step = 0.05),
    actionButton("run_model", "Run simulation", class = "btn-primary w-100")
  ),
  navset_card_underline(
    nav_panel("Summary", tableOutput("summary"), verbatimTextOutput("required_tt")),
    nav_panel("Plots", plotOutput("timing_plot", height = "650px"), plotOutput("temp_plot", height = "450px")),
    nav_panel("Results", DTOutput("results")),
    nav_panel("Data requirements", tags$pre("Required columns: date, tmin, tmax. Optional scenario columns: scenario, model, period, station."))
  )
)

server <- function(input, output, session) {
  observeEvent(input$crop_type, {
    p <- ThermoPheno::default_parameters(input$crop_type)
    updateTextInput(session, "crop_name", value = p$crop_name)
    updateNumericInput(session, "days_to_maturity", value = p$days_to_maturity)
    updateNumericInput(session, "t_base", value = p$t_base)
    updateNumericInput(session, "t_opt", value = p$t_opt)
    updateNumericInput(session, "t_max_cut", value = p$t_max_cut)
    updateTextInput(session, "baseline_planting_mmdd", value = p$baseline_planting_mmdd)
    updateTextInput(session, "earliest_planting_mmdd", value = p$earliest_planting_mmdd)
    updateTextInput(session, "latest_planting_mmdd", value = p$latest_planting_mmdd)
    updateTextInput(session, "latest_harvest_mmdd", value = p$latest_harvest_mmdd)
    updateNumericInput(session, "min_mean_temp_plant", value = p$min_mean_temp_plant)
  }, ignoreInit = TRUE)

  weather_data <- reactive({
    if (!is.null(input$weather_file)) {
      return(ThermoPheno::prepare_weather(readr::read_csv(input$weather_file$datapath, show_col_types = FALSE)))
    }
    req(input$use_example)
    load_example_weather()
  })

  climate_data <- reactive({
    if (is.null(input$climate_file)) return(NULL)
    ThermoPheno::prepare_weather(readr::read_csv(input$climate_file$datapath, show_col_types = FALSE))
  })

  required_tt <- eventReactive(input$run_model, {
    w <- weather_data()
    ThermoPheno::estimate_required_tt(
      weather = w,
      baseline_years = sort(unique(w$year)),
      planting_mmdd = input$baseline_planting_mmdd,
      days_to_maturity = input$days_to_maturity,
      t_base = input$t_base,
      t_opt = input$t_opt,
      t_max_cut = input$t_max_cut,
      tt_mode = input$tt_mode,
      crop_type = input$crop_type,
      winter_dormancy_temp = ifelse(input$crop_type == "winter", input$winter_dormancy_temp, 0),
      vernalization_required = ifelse(input$crop_type == "winter", input$vernalization_required, FALSE),
      vernalization_days_required = ifelse(input$crop_type == "winter", input$vernalization_days_required, 0),
      spring_regrowth_temp = ifelse(input$crop_type == "winter", input$spring_regrowth_temp, 5)
    )
  })

  historical_results <- eventReactive(input$run_model, {
    ThermoPheno::run_simulation(
      weather = weather_data(), crop_name = input$crop_name, required_tt = required_tt()$required_tt,
      earliest_planting_mmdd = input$earliest_planting_mmdd,
      latest_planting_mmdd = input$latest_planting_mmdd,
      latest_harvest_mmdd = input$latest_harvest_mmdd,
      t_base = input$t_base, t_opt = input$t_opt, t_max_cut = input$t_max_cut,
      tt_mode = input$tt_mode, crop_type = input$crop_type,
      min_mean_temp_plant = input$min_mean_temp_plant,
      forced_harvest_allowed = input$forced_harvest_allowed,
      min_fraction_tt_for_forced_harvest = input$min_fraction_tt_for_forced_harvest,
      winter_dormancy_temp = ifelse(input$crop_type == "winter", input$winter_dormancy_temp, 0),
      vernalization_required = ifelse(input$crop_type == "winter", input$vernalization_required, FALSE),
      vernalization_days_required = ifelse(input$crop_type == "winter", input$vernalization_days_required, 0),
      spring_regrowth_temp = ifelse(input$crop_type == "winter", input$spring_regrowth_temp, 5),
      winter_plant_temp_min = ifelse(input$crop_type == "winter", input$winter_plant_temp_min, 5),
      winter_plant_temp_max = ifelse(input$crop_type == "winter", input$winter_plant_temp_max, 15)
    ) %>% mutate(dataset = "Historical", scenario = NA_character_, model = NA_character_)
  })

  future_results <- eventReactive(input$run_model, {
    cd <- climate_data()
    if (is.null(cd)) return(NULL)
    group_cols <- intersect(c("scenario", "model", "period", "station"), names(cd))
    if (length(group_cols) == 0) groups <- list(cd) else groups <- dplyr::group_split(dplyr::group_by(cd, dplyr::across(dplyr::all_of(group_cols))))
    bind_rows(lapply(groups, function(g) {
      meta <- if (length(group_cols) == 0) data.frame() else g[1, group_cols, drop = FALSE]
      res <- ThermoPheno::run_simulation(
        weather = g, crop_name = input$crop_name, required_tt = required_tt()$required_tt,
        earliest_planting_mmdd = input$earliest_planting_mmdd,
        latest_planting_mmdd = input$latest_planting_mmdd,
        latest_harvest_mmdd = input$latest_harvest_mmdd,
        t_base = input$t_base, t_opt = input$t_opt, t_max_cut = input$t_max_cut,
        tt_mode = input$tt_mode, crop_type = input$crop_type,
        min_mean_temp_plant = input$min_mean_temp_plant,
        forced_harvest_allowed = input$forced_harvest_allowed,
        min_fraction_tt_for_forced_harvest = input$min_fraction_tt_for_forced_harvest,
        winter_dormancy_temp = ifelse(input$crop_type == "winter", input$winter_dormancy_temp, 0),
        vernalization_required = ifelse(input$crop_type == "winter", input$vernalization_required, FALSE),
        vernalization_days_required = ifelse(input$crop_type == "winter", input$vernalization_days_required, 0),
        spring_regrowth_temp = ifelse(input$crop_type == "winter", input$spring_regrowth_temp, 5),
        winter_plant_temp_min = ifelse(input$crop_type == "winter", input$winter_plant_temp_min, 5),
        winter_plant_temp_max = ifelse(input$crop_type == "winter", input$winter_plant_temp_max, 15)
      )
      bind_cols(res, meta) %>% mutate(dataset = "Climate")
    }))
  })

  combined_results <- reactive({
    req(historical_results())
    fut <- future_results()
    if (is.null(fut)) historical_results() else bind_rows(historical_results(), fut)
  })

  output$summary <- renderTable({ req(combined_results()); summarise_results(combined_results()) })
  output$required_tt <- renderPrint({ req(required_tt()); cat("Estimated required thermal time:", round(required_tt()$required_tt, 2), "°C-days\n") })
  output$timing_plot <- renderPlot({ req(combined_results()); plot_timing(combined_results()) })
  output$temp_plot <- renderPlot({ req(weather_data()); plot_temperature_cycle(weather_data(), climate_data()) })
  output$results <- renderDT({ req(combined_results()); datatable(combined_results(), options = list(scrollX = TRUE, pageLength = 25), rownames = FALSE) })
}

shinyApp(ui, server)
