#' Shiny-plot of consumer host-switching
#'
#' @details The function creates an interactive shiny-plot of simulated dispersal and colonization events of a consumer on novel hosts.
#'
#'The parameter values of the function HostSwitch can be modified on the slider bars. The refresh button on the top plots the new simulation.
#'
#'Black dots are the phenotype values after each event of reproduction. The green squares represent the value of phenotype favored by a new host proposed at each generation. The red squares are the phenotype values favored by the current host (=the initial one and the host after successful colonization).
#'
#'The total number of tentative dispersal by the parasites and successful host switches are calculated below the plot.
#'  @import shiny
#'  @examples
#'  shinyHostSwitch()
#'
#' @export
shinyHostSwitch <- function() {
  appDir <- system.file("shiny-examples", "shinyHostSwitch", package = "HostSwitch")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
