#' Survival probability of the parasite in a new host (novel resource)
#'
#' @param pInd Phenotype of parasite #(Phenotype of ith parasite attempting to disperse in a new host)
#' @param pHost Phenotype of resource #(The optimum phenotype the parasite should have to maximize the colonization success)
#' @param sigma Selection intensity #(standard deviation of selection intensity)
#' @details This function calculates the survival probability of individual parasites that attempt dispersal in a new host. It is the core function of \code{\link{simHostSwitch}}.
#' The function is based on the probability density function of a normal distribution. By ignoring the  normalization constant \eqn{(1/sqrt(2*pi)*sigma)}, it provides then the survival probability.
#'
#' @return The survival probability of the parasite
#' @examples
#' ## Example 1a - The ith parasite has the phenotype that maximize its colonization success on the new host, then pInd is equal to pHost (pInd = pHost), and the survival probability is 1.
#' survivalProbability(pInd=5,pHost=5,sigma=1)
#'
#' ## Example 1b - Increasing |pInd-pHost| the survival probability decreases
#' survivalProbability(pInd=5,pHost=30,sigma=1)
#'
#' ## Example 1c - Give a |pInd-pHost|> 1, increases sigma the survival probability increases
#' survivalProbability(pInd=5,pHost=30,sigma=1)
#'
#'
#' @export

survivalProbability = function(pInd,pHost,sigma){
  exp(-(pInd-pHost)^2/(2*sigma^2))
}


#' Simulate host switches by parasites
#'
#' @param K Carrying capacity
#' @param b Birth rate
#' @param mig Cut off for migration, individuals below cutoff jump
#' @param sd Standard deviation for mutation
#' @param sigma Standard deviation for selection
#' @param pRes_min Initial value, smallest phenotype of resource (original host) and parasite
#' @param pRes_max Initial value, maximum phenotype of resource (original host) and parasite
#' @param n_generation Number of generations
#' @param jump_back Options for parasites that do not survive on the new host. If "yes" the parasite(s) jump back to the current host and will be considered in the selective pressure and reproduction stage for the n+1 generation, if "no" (default) it dies on the new host.
#' @param seed Random number to ensure reproducible plots
#' @param iter Number of iterations
#' @details This function simulates the number of host switches by the population of a parasite. Results are stored to a HostSwitch object, to make use of summary and plotting functions in the HostSwitch package. The HostSwitch object includes the following simulated quantities are: $pRes_sim (all the optimal phenotypes favored by the selected new hosts), $pRes_new_sim (new resource), $pInd and of individual parasite. These simulated quantities of interest are available for each generation step and can be used for summary statistics or plots.
#' @return An object of class HostSwitch
#' @examples
#' HostSwitch_simulated_quantities = simHostSwitch(K=100,b=10, mig=0.01, sd=0.2, sigma=1, pRes_min=1, pRes_max=10, n_generation=200)
#' HostSwitch_simulated_quantities
#' @import ArgumentCheck
#' @export


simHostSwitch=function (K=100,b=10, mig=0.01, sd=0.2,sigma=1, pRes_min=1, pRes_max=10,n_generation=200,jump_back='no',seed=NULL, iter=0){
  set.seed(seed)

  #* Establish a new 'ArgCheck' object
  Check <- ArgumentCheck::newArgCheck()

  #* Add a warning
  if (!jump_back %in% c("no","yes")){
    ArgumentCheck::addWarning(
      msg = "'jump_back' must be either 'no' or 'yes'! 'jump_back has been set to 'yes'.",
      argcheck = Check)}


  pRes_sim_list           = list()
  pRes_new_sim_list       = list()
  pInd_sim_list           = list()
  pInd_jump_sim_list      = list()
  pInd_whichjump_sim_list = list()
  pInd_whichsurv_sim_list = list()

  for (i in 1:iter){

  # record quantities of interest
  pRes_sim           = rep(NA,n_generation) ### phenotype original host (Valeria: vector of optimum phenotypes favored by the new host)
  pRes_new_sim       = rep(NA,n_generation) ### phenotype new host (Valeria: ???)
  pInd_sim           = list()               # phenotype of individuals (valeria: phenotype of individuals at each generation)
  pInd_jump_sim      = rep(0,n_generation)  # vector of number of parasites that disperse (jumped)
  pInd_whichjump_sim = list()  # which parasites jumped
  pInd_whichsurv_sim = list()  # which parasites survived

  pInitial=mean(c(pRes_min,pRes_max)) ### Initial phenotype equal for host and individual (Valeria: the Initial phenotype for the parasite at n=0 is the average value of the IS)
  pRes=pInitial; pRes_sim[1]  = pInitial # The sine qua non condition for the simulation to starts is to have the first individual parasite having the phenotype equal to optimum favored by the current host.
  pInd=pInitial; pInd_sim[[1]]= pInitial # ....

  n=0

  while(n<n_generation & length(pInd)>0){
    n=n+1
    # Host switch
    pRes_new=pRes_min+(pRes_max-pRes_min)*stats::runif(1) ### fct creates phenotype for new host
    which_jump=which(stats::runif(length(pInd))<mig) # position of individuals
    pInd_jump=pInd[which_jump] # selected phenotype depending on poision

    pInd_jump_sim[n+1]        = length(pInd_jump) # record how many individuals jumped
    if(length(pInd_jump)>0){
    pInd_whichjump_sim[[n+1]] = pInd_jump         # record which individuals jumped
    } else{
      pInd_whichjump_sim[[n+1]] = NA
    }

    ## Selection in the new host
    prob=survivalProbability(pInd=pInd_jump,pHost=pRes_new,sigma=sigma) # survival probability of jumped individuals; eq. 1
    pInd_new=pInd_jump[prob>stats::runif(length(pInd_jump))]

    if(length(pInd_new)>0){ # Host switch successful, at least 1 individual jumped & survived
      pRes=pRes_new
      pInd=pInd_new                         # survivded individuals on new plant
      pInd_whichsurv_sim[[n+1]] = pInd_new  # record which individuals survived
    }
    else{
      # If SOME INDIVIDUALS JUMPED BUT DID NOT SURVIVE, remaining! individuals on original host
      # are further used to calculate survival probability on original host and generate new offspring
      # Note: jumped individuals are not allowed come back!
      pInd_whichsurv_sim[[n+1]] = NA # no survived individuals
      if(length(which_jump)>0 & jump_back=="no"){pInd=pInd[-which_jump]} # select remaining individuals of original host
      prob=survivalProbability(pInd=pInd,pHost=pRes,sigma=sigma)
      pInd=pInd[prob>stats::runif(length(pInd))]
    }


    # Reproduction
    descendants=b*length(pInd)
    if(descendants>K){descendants=K} # Carrying capacity = upper limit
    if(length(pInd)>1)   {
      pInd_desc=sample(pInd,descendants,replace=TRUE) # randomly select offspring phenotype with replacement
    }else{
      pInd_desc=rep(pInd,descendants)
    }
    pInd=pInd_desc+stats::rnorm(descendants, mean=0, sd=sd) # fct adds variation to offspring phenotype

    pRes_sim[n+1]     = pRes
    pRes_new_sim[n+1] = pRes_new
    pInd_sim[[n+1]]   = pInd


  }



  # Store parameters and arguments of interst
  pRes_sim     = pRes_sim[!is.na(pRes_sim)]         # remove NA
  pRes_new_sim = pRes_new_sim[!is.na(pRes_new_sim)] # remove NA

# length + 1 for new iteration
  pRes_sim_list[[length(pRes_sim_list) + 1]]                     = pRes_sim
  pRes_new_sim_list[[length(pRes_new_sim_list) + 1]]             = pRes_new_sim
  pInd_sim_list[[i]]                                             = pInd_sim
  pInd_jump_sim_list[[length(pInd_jump_sim_list) + 1]]           = pInd_jump_sim
  pInd_whichjump_sim_list[[length(pInd_whichjump_sim_list) + 1]] = pInd_whichjump_sim
  pInd_whichsurv_sim_list[[length(pInd_whichsurv_sim_list) + 1]] = pInd_whichsurv_sim

}





out = list()
out$pRes_sim           = pRes_sim_list
out$pRes_new_sim       = pRes_new_sim_list
out$pInd_sim           = pInd_sim_list
out$n_generation       = n_generation
out$pRes_min           = pRes_min
out$pRes_max           = pRes_max
out$pInd_jump_sim      = pInd_jump_sim_list
out$pInd_whichjump_sim = pInd_whichjump_sim_list
out$pInd_whichsurv_sim = pInd_whichsurv_sim_list
out$K=K;out$b=b; out$mig=mig; out$sd=sd;out$sigma=sigma;out$iter=iter
class(out) = "HostSwitch"

#* Return errors and warnings (if any)
ArgumentCheck::finishArgCheck(Check)


return(out)

}

#' Set class and method
#'
#' This is a build-time dependency on methods, as opposed to a run-time
#' dependency, thus requiring the importFrom tag to avoid a NOTE when checking
#' the package on CRAN.
#'
#' @keywords internal
#' @importFrom methods setClass setMethod
#' @export
#'
methods::setClass("summaryHostSwitch", representation=representation("list"))
methods::setMethod("show",signature = "summaryHostSwitch", definition = function(object) {
  cat("An object of class ", class(object), "\n", sep = "")
  cat("Summary of HostSwitch simulations\n\n")
  cat("Settings of Simulation:\n")
  cat("iter:",object$iter,", n_generations:",object$n_generation,", pRes_min:",object$pRes_min,", pRes_max:",object$pRes_max,"\n",sep="")
  cat("K:",object$K,", sd:",object$sd,", sigma:",object$sigma,"\n\n",sep="")
  cat("Summary of phenotypes:\n")
  print(object$summaryP)
  cat("\nSummary of host switches by parasites:\n")
  print(object$summaryHS)
  invisible(NULL)
})


#' Summary statistics of HostSwitch simulation
#'
#' @param HostSwitch_simulated_quantities An object created by \code{\link{simHostSwitch}}
#' @details This function generates summary statistcs for HostSwitch simulations.
#' @return Summary of HostSwitch simulations
#' @examples
#' HostSwitch_simulated_quantities = simHostSwitch(K=100,b=10, mig=0.01, sd=0.2, sigma=1, pRes_min=1, pRes_max=10, n_generation=200,iter=100)
#' summaryHostSwitch(HostSwitch_simulated_quantities)
#' @import ArgumentCheck
#' @import methods
#' @import plyr
#' @export



summaryHostSwitch = function(HostSwitch_simulated_quantities){

  # compute mean for each iteration
  out=list()
  out$n_generation = HostSwitch_simulated_quantities$n_generation
  out$pRes_min     = HostSwitch_simulated_quantities$pRes_min
  out$pRes_max     = HostSwitch_simulated_quantities$pRes_max
  out$K            = HostSwitch_simulated_quantities$K
  out$b            = HostSwitch_simulated_quantities$b
  out$mig          = HostSwitch_simulated_quantities$mig
  out$sd           = HostSwitch_simulated_quantities$sd
  out$sigma        = HostSwitch_simulated_quantities$sigma
  out$iter         = HostSwitch_simulated_quantities$iter



  summaryP = data.frame(matrix(NA, ncol = 6, nrow = 3))
  rownames(summaryP) = c("pRes","pRes_new","pInd")
  colnames(summaryP) = c("Min.", "1st Qu.",  "Median" ,   "Mean", "3rd Qu.",    "Max.")
  summaryP[1,] = round(summary(plyr::laply(HostSwitch_simulated_quantities$pRes_sim,mean)),2)
  summaryP[2,] = round(summary(plyr::laply(HostSwitch_simulated_quantities$pRes_new_sim,mean)),2)
  summaryP[3,] = round(summary(plyr::laply(HostSwitch_simulated_quantities$pInd_sim, function(x) mean(unlist(x)))),2)
  out$summaryP=summaryP

  summaryHS = data.frame(matrix(NA, ncol = 2, nrow = 2))
  rownames(summaryHS) = c("Number of parasite jumps:","Number of successful host switches:")
  colnames(summaryHS) = c("Mean", "Max")
  summaryHS[1,] = c(round(mean(plyr::laply(HostSwitch_simulated_quantities$pInd_jump_sim,function(x) length(which(x>0)))),2),
                    round(max(plyr::laply(HostSwitch_simulated_quantities$pInd_jump_sim,function(x) length(which(x>0)))),2))

  sucessfullHS = rep(0,HostSwitch_simulated_quantities$iter)

  for (i in 1:HostSwitch_simulated_quantities$iter){
    dat = lapply(HostSwitch_simulated_quantities[c(1,2)], `[[`, i)
    sucessfullHS[i] = length(which(dat$pRes_sim[-1]==dat$pRes_new_sim))
  }

  summaryHS[2,] = c(mean(sucessfullHS),max(sucessfullHS))

  out$summaryHS = summaryHS
  methods::new("summaryHostSwitch", out)
}


