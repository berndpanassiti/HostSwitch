print.HostSwitch.startupInfo <- function()
{
  version <- utils::packageVersion('HostSwitch')
  hello <- paste("Welcome to the HostSwitch simulation package, v.",version, '.\n', "For overview type vignette('HostSwitch',package='HostSwitch').\nNote: For an interactive plot, see ?shinyHostSwitch for details." ,sep="")
  packageStartupMessage(cat(hello))
}

.onLoad <- function(...) {

}

.onAttach <- function(...) {
  print.HostSwitch.startupInfo()
}
