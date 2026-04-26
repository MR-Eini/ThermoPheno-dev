if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("date", "required_tt", "row_role", "tmax", "tmin", "year"))
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
"ThermoPheno loaded.

Run the app using:
  ThermoPheno()

Example files are available via system.file('extdata', package = 'ThermoPheno')."
  )
}
