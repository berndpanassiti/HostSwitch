#' Consumer life cycle parameters to simulate host switches
#'
#' Data derived from real world experiments (see references). Data includes
#' life cycle parameters for a wildlife ecology, agricultural
#' and biomedical arena  to simulate host switches.
#'
#' @docType data
#'
#' @usage data(parli)
#'
#' @format An object of class "list".
#'
#' @keywords datasets
#'
#' @references Trivellone V, Araujo SBL and Panassiti B
#' (2021) HostSwitch: An R Package to Simulate
#' the Extent of Host-Switching by a Consumer
#'
#' @examples
#' data(parli)
#' knitr::kable(parli$Cephaloleia) # tibble::as.tibble(parli$Cephaloleia)
#'
"parli"
