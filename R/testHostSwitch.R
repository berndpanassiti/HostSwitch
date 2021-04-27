#' Test for differences of HostSwitch simulations
#'
#' @param simulated_quantities1 An object created by \code{\link{simHostSwitch}}
#' @param simulated_quantities2 An object created by \code{\link{simHostSwitch}}
#' @param parameter Quantity of interest, possible values are:
#' \itemize{
#'   \item "j" for \strong{j}umps of parasites from resource to new resource
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
#' m1 = simHostSwitch(n_generation=100,iter=100)
#' m2 = simHostSwitch(n_generation=50,iter=50)
#' testHostSwitch(simulated_quantities1=m1,simulated_quantities2=m2,parameter="j",test="t",plot=TRUE)
#' @import ArgumentCheck
#' @import stats
#' @import ggplot2
#' @importFrom purrr map
#' @importFrom plyr laply
#' @export

testHostSwitch = function(simulated_quantities1,simulated_quantities2,parameter,test,plot=FALSE){

  out=list()

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
    x = rep(0,simulated_quantities1$iter)
    y = rep(0,simulated_quantities2$iter)

    for (i in 1:simulated_quantities1$iter){
      dat = lapply(simulated_quantities1[c(1,2)], `[[`, i)
      x[i] = length(which(dat$pRes_sim[-1]==dat$pRes_new_sim))
    }

    for (i in 1:simulated_quantities2$iter){
      dat = lapply(simulated_quantities1[c(1,2)], `[[`, i)
      y[i] = length(which(dat$pRes_sim[-1]==dat$pRes_new_sim))
    }
  }



## Distance between mean parasite and new host phenotype
  if(parameter == "d"){
    title = "Comparison of phenotype distance btw. parasite and new host"
    x = rep(0,(simulated_quantities1$iter*simulated_quantities1$n_generation))
    y = rep(0,(simulated_quantities2$iter*simulated_quantities2$n_generation))

    # sim1
    Ind = sapply(flatten2(purrr::map(simulated_quantities1$pInd_sim,head,-1)),mean)
    pRes_new=unlist(flatten2(simulated_quantities1$pRes_new_sim))
    x = abs(Ind-pRes_new)

    # sim2
    Ind = sapply(flatten2(purrr::map(simulated_quantities2$pInd_sim,head,-1)),mean)
    pRes_new=unlist(flatten2(simulated_quantities2$pRes_new_sim))
    y = abs(Ind-pRes_new)
  }

# statistical test
if(test == "t"){
  out$result = stats::t.test(x,y)
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


