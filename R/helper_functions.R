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

#' Create input data of simulated individuals for plots
#' @param x A nested list
#' @details Flattens a nested list; https://stackoverflow.com/questions/8139677/how-to-flatten-a-list-to-a-list-without-coercion
#' @keywords internal

createPlotInput_sim_pInd <- function(x){
# simulated individuals
df1 = flatten2(x)
df2 = data.frame(sapply(df1, "length<-", max(lengths(df1))))
colnames(df2)        = 0:(dim(df2)[2]-1)
pInd_sim             = utils::stack(df2)
colnames(pInd_sim)   = c("y","x")
pInd_sim             = pInd_sim[stats::complete.cases(pInd_sim),] # remove NA
pInd_sim$x           = as.numeric(as.character(pInd_sim$x))
}
