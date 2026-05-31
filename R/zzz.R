.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
"ThermoPheno loaded.

Run the app using:
  ThermoPheno::run_thermopheno_app()

Example files:
  system.file('extdata', package = 'ThermoPheno')"
  )
}
