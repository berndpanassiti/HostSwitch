#' Test for differences of HostSwitch simulations
#'
#' @param x1 An object created by \code{\link{simHostSwitch}}
#' @param x2 An object created by \code{\link{simHostSwitch}}
#' @param parameter Quantity of interest, possible values are:
#' \itemize{
#'   \item "s" for \strong{s}uccessful host switches
#'   \item "d" for \strong{d}istance between parasite and new host phenotypes if host switch successful
#' }
#' @param test Statistical test, available tests are:
#' \itemize{
#'   \item ''t'' for \strong{t}-test
#' }
#' @param plot If \emph{TRUE}, a boxplot is drawn
#'
#' @details This function can be used to compare HostSwitch simulations with different settings and test for differences.
#' @return An object of class HostSwitch
#' @examples
#' HostSwitch_simulated_quantities = simHostSwitch(K=100,b=10, mig=0.01, sd=0.2, sigma=1, pRes_min=1, pRes_max=10, n_generation=200,iter=100)
#' testHostSwitch(HostSwitch_simulated_quantities)
#' @import ArgumentCheck
#' @import stats
#' @export

testHostSwitch = function(x1,x2,parameter,test,plot=FALSE){
  #
  # methods::setClass("HostSwitch", representation("list"))
  # methods::setMethod("show",signature = "HostSwitch", definition = function(object) {
  #   cat("An object of class ", class(object), "\n", sep = "")
  #   cat("Summary of HostSwitch simulations\n\n")
  #   cat("Settings of Simulation:\n")
  #   cat("iter:",object$iter,", n_generations:",object$n_generation,", pRes_min:",object$pRes_min,", pRes_max:",object$pRes_max,"\n",sep="")
  #   cat("K:",object$K,", sd:",object$sd,", sigma:",object$sigma,"\n\n",sep="")
  #   cat("Summary of phenotypes:\n")
  #   print(object$summaryP)
  #   cat("\nSummary of host switches by parasites:\n")
  #   print(object$summaryHS)
  #   invisible(NULL)
  # })
  #
  # # compute mean for each iteration
  # out=list()
  # out$n_generation = HostSwitch_simulated_quantities$n_generation
  # out$pRes_min     = HostSwitch_simulated_quantities$pRes_min
  # out$pRes_max     = HostSwitch_simulated_quantities$pRes_max
  # out$K            = HostSwitch_simulated_quantities$K
  # out$b            = HostSwitch_simulated_quantities$b
  # out$mig          = HostSwitch_simulated_quantities$mig
  # out$sd           = HostSwitch_simulated_quantities$sd
  # out$sigma        = HostSwitch_simulated_quantities$sigma
  # out$iter         = HostSwitch_simulated_quantities$iter
  #
  #
  #
  # summaryP = data.frame(matrix(NA, ncol = 6, nrow = 3))
  # rownames(summaryP) = c("pRes","pRes_new","pInd")
  # colnames(summaryP) = c("Min.", "1st Qu.",  "Median" ,   "Mean", "3rd Qu.",    "Max.")
  # summaryP[1,] = round(summary(plyr::laply(HostSwitch_simulated_quantities$pRes_sim,mean)),2)
  # summaryP[2,] = round(summary(plyr::laply(HostSwitch_simulated_quantities$pRes_new_sim,mean)),2)
  # summaryP[3,] = round(summary(plyr::laply(HostSwitch_simulated_quantities$pInd_sim, function(x) mean(unlist(x)))),2)
  # out$summaryP=summaryP
  #
  # summaryHS = data.frame(matrix(NA, ncol = 2, nrow = 2))
  # rownames(summaryHS) = c("Number of parasite jumps:","Number of successful host switches:")
  # colnames(summaryHS) = c("Mean", "Max")
  # summaryHS[1,] = c(round(mean(plyr::laply(HostSwitch_simulated_quantities$pInd_jump_sim,function(x) length(which(x>0)))),2),
  #                   round(max(plyr::laply(HostSwitch_simulated_quantities$pInd_jump_sim,function(x) length(which(x>0)))),2))
  #
  # sucessfullHS = rep(0,HostSwitch_simulated_quantities$iter)
  #
  # for (i in 1:HostSwitch_simulated_quantities$iter){
  #   dat = lapply(df[c(1,2)], `[[`, i)
  #   sucessfullHS[i] = length(which(dat$pRes_sim[-1]==dat$pRes_new_sim))
  # }
  #
  # summaryHS[2,] = c(mean(sucessfullHS),max(sucessfullHS))
  #
  # out$summaryHS = summaryHS
  #
  # methods::new("HostSwitch", out)
}


