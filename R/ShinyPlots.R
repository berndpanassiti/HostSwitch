#' Shiny-plot of consumer host-switching
#'
#' @details The function creates an interactive web-based front-end using Shiny App. The Shiny-Plot simulates dispersal and colonization events of a consumer on novel hosts.
#'
#'The parameter values included in the function \code{\link{simHostSwitch}} can be modified by the slider bars. The refresh button on the top left (Refresh simulation) plots the new simulation.
#'
#'Black dots are the phenotype values of the Consumer after each event of reproduction. The green squares represent the value of phenotype values of the Consumer favored by the novel host proposed at each generation. The red squares are the phenotype values favored by the current host.
#'
#'The total number of dispersal and successful host switch events (or individuals) by Consumers for each are reported below the plot.
#' @import shiny
#' @import tippy
#' @return a dynamic interface plotting the dispersion events and host switch simulations and reacting to user input.
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'  shinyHostSwitch()
#' }
#'
#' @export
shinyHostSwitch <- function() {
  appDir <- system.file("shiny-examples", "shinyHostSwitch", package = "HostSwitch")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
