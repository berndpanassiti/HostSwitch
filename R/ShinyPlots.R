#' Shiny plot of survival probability of parasite after host switch
#'
#' @details The function creates a shiny plot with host switches by a parasite.
#'
#' @import shiny
#'
#' @export
shinyHostSwitch <- function() {
  appDir <- system.file("shiny-examples", "shinyHostSwitch", package = "HostSwitch")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
