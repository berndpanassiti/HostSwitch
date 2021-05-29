#' Test for differences of HostSwitch models
#'
#' @param simulated_quantities1 An object created by \code{\link{simHostSwitch}}
#' @param simulated_quantities2 An object created by \code{\link{simHostSwitch}}
#' @param parameter Quantity of interest, possible values are:
#' \itemize{
#'   \item "j" for \strong{j}umps of consumers from resource to new resource
#'   \item "s" for \strong{s}uccessful host switches
#'   \item "d" for \strong{d}istance between consumers and new host phenotypes if host switch successful
#' }
#' @param test Statistical test, available tests are:
#' \itemize{
#'   \item ''t'' for \strong{t}-test (parametric)
#'   \item ''w'' for \strong{W}ilcoxon-test (non-parametric)
#' }
#' @param warmup Number of warmup steps to be excluded when comparing models, see details. Possible value are NULL or positive integer (min=1,max=50). Default value = 1
#' @param plot If \emph{TRUE}, a boxplot is drawn
#' @details This function can be used to compare HostSwitch models with different settings and test for differences.
#' Warmup represents the initial condition and is defined as an adaptation stage of the simulation model. The initial condition corresponds to the number of
#' generations (n_generations): warmup = 1 means that the generation at time 0 is excluded from comparison; warmup = 2 means generations at times 0 and 1 are excluded and so on.
#' If warmup = NULL all generations are considered for comparison, i.e. initial condition is not considered.
#' @return An object of class HostSwitch
#' @examples
#' m1 = simHostSwitch(n_generations=100,n_sim=100)
#' m2 = simHostSwitch(n_generations=50,n_sim=50)
#' testHostSwitch(simulated_quantities1=m1,simulated_quantities2=m2,
#' parameter="j",test="t",plot=TRUE)
#' @import checkmate
#' @import stats
#' @import ggplot2
#' @importFrom purrr map
#' @importFrom plyr laply
#' @importFrom utils head
#' @export

testHostSwitch = function(simulated_quantities1,simulated_quantities2,parameter,test,warmup=1,plot=FALSE){

  # input checks
  checkmate::assert_class(simulated_quantities1,"HostSwitch") # class HostSwitch
  checkmate::assert_class(simulated_quantities2,"HostSwitch") # class HostSwitch
  checkmate::assertChoice(parameter, c("d", "j","s")) # parameter
  checkmate::assertChoice(test, c("t")) # test
  checkmate::assertChoice(plot, c(TRUE,FALSE)) # plot
  checkmate::assertCount(warmup,positive=TRUE,null.ok = TRUE);checkmate::assertNumeric(warmup,upper=50,null.ok = TRUE) # warmup



  values <- NULL # global variables

  out=list()
  n_sim1 = simulated_quantities1$n_sim; n_sim2 = simulated_quantities2$n_sim
  n_generations1 = simulated_quantities1$n_generations; n_generations2 = simulated_quantities2$n_generations

  if (length(warmup)>0){
    # model 1
    simulated_quantities1[["pRes_sim"]]      = lapply(simulated_quantities1[["pRes_sim"]], function(x) x[-c(1:warmup)])
    simulated_quantities1[["pRes_new_sim"]]  = lapply(simulated_quantities1[["pRes_new_sim"]], function(x) x[-c(1:warmup)])
    simulated_quantities1[["pInd_jump_sim"]] = lapply(simulated_quantities1[["pInd_jump_sim"]], function(x) x[-c(1:warmup)])
    for (i in 1:n_sim1){
      simulated_quantities1[["pInd_sim"]][[i]] = simulated_quantities1[["pInd_sim"]][[i]][-c(1:warmup)]
    }
    n_generations1 = n_generations1 - warmup

    # model 2
    simulated_quantities2[["pRes_sim"]]      = lapply(simulated_quantities2[["pRes_sim"]], function(x) x[-c(1:warmup)])
    simulated_quantities2[["pRes_new_sim"]]  = lapply(simulated_quantities2[["pRes_new_sim"]], function(x) x[-c(1:warmup)])
    simulated_quantities2[["pInd_jump_sim"]] = lapply(simulated_quantities2[["pInd_jump_sim"]], function(x) x[-c(1:warmup)])
    for (i in 1:n_sim2){
      simulated_quantities2[["pInd_sim"]][[i]] = simulated_quantities2[["pInd_sim"]][[i]][-c(1:warmup)]
    }
    n_generations2 = n_generations2 - warmup
  }





# choice of parameter
## JUMPS
  if(parameter == "j"){
    title = "Comparison of number of parasite jumps to a new host"
   x = plyr::laply(simulated_quantities1$pInd_jump_sim,function(x) length(which(x>0)))
   y = plyr::laply(simulated_quantities2$pInd_jump_sim,function(x) length(which(x>0)))
  }
## SUCCESSFUL HOST SWITCHES
  if(parameter == "s"){
    title = "Comparison of number of successful host switches by a parasite"
    x = rep(0,n_sim1)
    y = rep(0,n_sim2)

    for (i in 1:n_sim1){
      dat = lapply(simulated_quantities1[c(1,2)], `[[`, i)
      x[i] = length(which(dat$pRes_sim[-1]==dat$pRes_new_sim))
    }

    for (i in 1:n_sim2){
      dat = lapply(simulated_quantities1[c(1,2)], `[[`, i)
      y[i] = length(which(dat$pRes_sim[-1]==dat$pRes_new_sim))
    }
  }



## Distance between mean parasite and new host phenotype
  if(parameter == "d"){
    title = "Comparison of phenotype distance btw. parasite and new host"
    x = rep(0,(n_sim1*n_generations1))
    y = rep(0,(n_sim2*n_generations2))

    # sim1
    Ind = sapply(flatten2(purrr::map(simulated_quantities1$pInd_sim,utils::head,-1)),mean)
    pRes_new=unlist(flatten2(simulated_quantities1$pRes_new_sim))
    x = abs(Ind-pRes_new)

    # sim2
    Ind = sapply(flatten2(purrr::map(simulated_quantities2$pInd_sim,utils::head,-1)),mean)
    pRes_new=unlist(flatten2(simulated_quantities2$pRes_new_sim))
    y = abs(Ind-pRes_new)
  }

# statistical test
if(test == "t"){
  out$result = stats::t.test(x,y)
}

if(test == "w"){
    out$result = stats::wilcox.test(x,y)
}

# plot
if(plot == TRUE){
  df1 = data.frame(x=rep("sim1",length(x)),values=x) # simulated_quantities1
  df2 = data.frame(x=rep("sim2",length(y)),values=y) # simulated_quantities2
  plotInput = data.frame(rbind(df1,df2))
  g=ggplot2::ggplot(data=plotInput,aes(x=x,y=values,group=x)) + geom_boxplot() +
  labs(title = title) +
  ggplot2::theme_bw()
 print(g)
}

  return(out$result)

}


