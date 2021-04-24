#' Create input data of simulated individuals for plots
#' @param x A list of simulated parasite phenotypes for each generation
#' @details Converts list to dataframe and stack it
#' @keywords internal

createPlotInput_sim_pInd <- function(x){
df1 = data.frame(sapply(x, "length<-", max(lengths(x))))
colnames(df1)        = 0:(dim(df1)[2]-1)
pInd_sim             = utils::stack(df1)
colnames(pInd_sim)   = c("y","x")
pInd_sim             = pInd_sim[stats::complete.cases(pInd_sim),] # remove NA
pInd_sim$x           = as.numeric(as.character(pInd_sim$x))
return(pInd_sim)
}

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

createPlotInput_sim_which <- function(x){
  xx        = flatten2(x)
  which_sim = utils::stack(setNames(xx, seq_along(xx)))
  colnames(which_sim)   = c("y","x")
  which_sim$x           = as.numeric(as.character(which_sim$x))
  return(which_sim)
}
