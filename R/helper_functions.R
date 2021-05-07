#' Flatten a nested list
#' @param x A nested list
#' @details Flattens a nested list; https://stackoverflow.com/questions/8139677/how-to-flatten-a-list-to-a-list-without-coercion
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

#' Echo personalized error message for jump back
#' @param x User input (string)
#' @details Checks if input is a string and either yes or no
#' @keywords internal

assert_JumpBack <- function(x) {
  res <- try(expr = checkmate::assert_string(x,pattern="^no$|^yes$"), silent = TRUE)
  msg_attr=attr(x = res, which = "condition")$message
  msg_gsub = gsub("[^[:alnum:][:blank:]+':|?&/\\-]","",msg_attr)
  msg = gsub("\\|","' or '",msg_gsub)
  if (inherits(x = res, what = "try-error")) {
    stop(
      call. = FALSE,
      msg
    )
  }
}
