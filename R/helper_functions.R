#' Flatten a nested list
#' @param x A nested list
#' #'@details Flattens a nested list; https://stackoverflow.com/questions/8139677/how-to-flatten-a-list-to-a-list-without-coercion
#' @keywords internal

flatten2 <- function(x) {
  len <- sum(rapply(x, function(z) 1L))
  y <- vector('list', len)
  i <- 0L
  rapply(x, function(x) { i <<- i+1L; y[[i]] <<- x })
  y
}


#' Create input data of simulated jumped/survived individuals for plots
#' @param x A list of simulated jumped/survived parasite phenotypes for each generation
#' @details Converts list to dataframe and stack it
#' @keywords internal

createPlotInput_Ind_sim <- function(x){
  xx        = flatten2(x)
  parasites = utils::stack(setNames(xx,seq_along(xx)-1)) # need to start from generation 0!
  colnames(parasites)   = c("y","x")
  parasites$x           = as.numeric(as.character(parasites$x))
  return(parasites)
}


