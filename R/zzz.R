.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "ThermoPheno loaded.\n",
    "Run the app using: ThermoPheno::run_thermopheno_app()\n",
    "Example files: system.file('extdata', package = 'ThermoPheno')"
  )
}
