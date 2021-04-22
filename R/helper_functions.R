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
