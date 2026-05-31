#' Launch the ThermoPheno Shiny application
#'
#' Starts the ThermoPheno interactive Shiny application for historical analysis
#' and climate-change impact assessment of crop phenology.
#'
#' @return The function is called for its side effect of launching the app.
#' @export
#' @examples
#' \dontrun{
#' run_thermopheno_app()
#' ThermoPheno()
#' }
run_thermopheno_app <- function() {
  app_dir <- system.file("app", package = "ThermoPheno")
  if (app_dir == "") {
    stop("App not found. Please reinstall ThermoPheno.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}

#' Launch the ThermoPheno Shiny application
#'
#' Alias for [run_thermopheno_app()].
#'
#' @return The function is called for its side effect of launching the app.
#' @export
ThermoPheno <- function() {
  run_thermopheno_app()
}
