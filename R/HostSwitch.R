#' Survival probability of the consumer in a new host (novel resource)
#'
#' @param pInd Phenotype of consumer #(Phenotype of ith consumer attempting to disperse in a new host)
#' @param pHost Phenotype of resource #(The optimum phenotype the consumer should have to maximize the colonization success)
#' @param sigma Selection intensity #(standard deviation of selection intensity)
#' @details This function calculates the survival probability of individual consumers that attempt dispersal in a new host. It is the core function of \code{\link{simHostSwitch}}.
#' The function is based on the probability density function of a normal distribution. By ignoring the  normalization constant \eqn{(1/sqrt(2*pi)*sigma)}, it provides then the survival probability.
#'
#' @return The survival probability of the consumer
#' @examples
#' ## Example 1a - The ith consumer has the phenotype that maximize its
#' ## colonization success on the new host, then pInd is equal to pHost (pInd = pHost),
#' ## and the survival probability is 1.
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


#' Simulate host switches by consumers
#'
#' @param K Carrying capacity, positive integer (min=1, max=1000), default value: 100
#' @param b Average number of offspring each consumer can have (birth rate), numeric value (min=0, max=K), default value: 10
#' @param mig Cut off for migration, individuals below cutoff jump, numeric value (min=0, max=1), default value: 0.01
#' @param sd Standard deviation for mutation, numeric value (min=0, max=10), default value: 0.2
#' @param sigma Standard deviation for selection, numeric value (min=0, max=10), default value: 1
#' @param pRes_min Initial value, smallest phenotype of resource (original host) and consumer, numeric value (min=0, max=pRes_max), default value: 1
#' @param pRes_max Initial value, maximum phenotype of resource (original host) and consumer, numeric value (min=pRes_min, max=100), default value: 10
#' @param n_generations Number of generations, positive integer (min=1, max=50000), default value: 200
#' @param jump_back Options for consumers that do not survive on the new host. If "yes" the consumer(s) jump back to the current host and will be considered in the selective pressure and reproduction stage for the n+1 generation, if "no" (default) it dies on the new host.
#' @param seed Random number to ensure reproducible plots, positive integer (>0), default value: NULL
#' @param n_sim Number of simulations, positive integer (min=1, max = 50000), default value: 1
#' @details This function simulates the number of host switches by the population of a consumer. Results are stored to a HostSwitch object, to make use of summary and plotting functions in the HostSwitch package. The HostSwitch object includes the following simulated quantities are: $pRes_sim (all the optimal phenotypes favored by the selected new hosts), $pRes_new_sim (new resource), $pInd and of individual consumer. These simulated quantities of interest are available for each generation step and can be used for summary statistics or plots.
#' @return An object of class HostSwitch
#' @examples
#' m1 = simHostSwitch() # default values for arguments
#' m1
#' @import checkmate
#' @export


simHostSwitch=function (K=100,b=10, mig=0.01, sd=0.2,sigma=1, pRes_min=1, pRes_max=10,n_generations=200,jump_back='no',seed=NULL, n_sim=1){
  set.seed(seed)
  # checks
  checkmate::assertCount(K,positive=TRUE);checkmate::assertNumeric(K,upper=1000) # K
  checkmate::assertNumeric(b,lower=0,upper=K) # b
  checkmate::assertNumeric(mig,lower=0,upper=1) # sd
  checkmate::assertNumeric(sd,lower=0,upper=10) # sd
  checkmate::assertNumeric(sigma,lower=0,upper=10) # sigma
  checkmate::assertNumeric(pRes_min,lower=0,upper=pRes_max) # pRes_min
  checkmate::assertNumeric(pRes_min,lower=pRes_min,upper=100) # pRes_max
  checkmate::assertCount(n_generations,positive=TRUE);checkmate::assertNumeric(n_generations,upper=50000) # n_generations
  checkmate::assertChoice(jump_back, c("no","yes"))
  checkmate::assertCount(seed,positive=TRUE,null.ok = TRUE) # seed
  checkmate::assertCount(n_sim,positive=TRUE);checkmate::assertNumeric(n_sim,upper=50000) # n_sim

  pRes_sim_list           = list()
  pRes_new_sim_list       = list()
  pInd_sim_list           = list()
  pInd_jump_sim_list      = list()
  pInd_whichjump_sim_list = list()
  pInd_whichsurv_sim_list = list()

  for (i in 1:n_sim){

  # record quantities of interest
  pRes_sim           = rep(NA,n_generations) ### phenotype original host (Valeria: vector of optimum phenotypes favored by the new host)
  pRes_new_sim       = rep(NA,n_generations) ### phenotype new host (Valeria: ???)
  pInd_sim           = list()               # phenotype of individuals (valeria: phenotype of individuals at each generation)
  pInd_jump_sim      = rep(0,n_generations)  # vector of number of consumers that disperse (jumped)
  pInd_whichjump_sim = list()  # which consumers jumped
  pInd_whichsurv_sim = list()  # which consumers survived

  pInitial=mean(c(pRes_min,pRes_max)) ### Initial phenotype equal for host and individual (Valeria: the Initial phenotype for the consumer at n=0 is the average value of the IS)
  pRes=pInitial; pRes_sim[1]  = pInitial # The sine qua non condition for the simulation to starts is to have the first individual consumer having the phenotype equal to optimum favored by the current host.
  pInd=pInitial; pInd_sim[[1]]= pInitial # ....

  n=0

  while(n<n_generations & length(pInd)>0){
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

# length + 1 for new simlation
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
out$n_generations       = n_generations
out$pRes_min           = pRes_min
out$pRes_max           = pRes_max
out$pInd_jump_sim      = pInd_jump_sim_list
out$pInd_whichjump_sim = pInd_whichjump_sim_list
out$pInd_whichsurv_sim = pInd_whichsurv_sim_list
out$K=K;out$b=b; out$mig=mig; out$sd=sd;out$sigma=sigma;out$n_sim=n_sim
class(out) = "HostSwitch"


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
  cat("General settings of individual based model:\n")
  cat("n_sim:",object$n_sim,", warmup:",object$warmup,", n_generations:",object$n_generations,", pRes_min:",object$pRes_min,", pRes_max:",object$pRes_max,"\n",sep="")
  cat("K:",object$K,", sd:",object$sd,", sigma:",object$sigma,"\n\n",sep="")
  cat("Summary of phenotypes:\n")
  print(object$summaryP)
  cat("\nSummary of host switches by consumers:\n")
  print(object$summaryHS)
  invisible(NULL)
})


#' Summary statistics of HostSwitch simulation
#'
#' @param HostSwitch_simulated_quantities An object created by \code{\link{simHostSwitch}}
#' @param warmup Number of warmup steps to be excluded from summary statstistics, see details. Possible value are NULL or positive integer (min=1,max=50). Default value = 1
#' @details This function generates summary statistcs for HostSwitch simulations.
#' Warmup represents the initial condition and is defined as an adaptation stage of the simulation model. The initial condition corresponds to the number of
#' generations (n_generations): warmup = 1 means that the generation at time 0 is excluded from summary; warmup = 2 means generations at times 0 and 1 are excluded and so on.
#' If warmup = NULL all generations are considered for summary statistics, i.e. initial condition is not considered.
#' @return Summary of HostSwitch simulations
#' @examples
#' m1 = simHostSwitch(n_sim=100) # except n_sim, default values for arguments
#' summaryHostSwitch(m1)
#' @import checkmate
#' @import methods
#' @import plyr
#' @export



summaryHostSwitch = function(HostSwitch_simulated_quantities,warmup = 1){

  # input checks
  checkmate::assert_class(HostSwitch_simulated_quantities,"HostSwitch") # class HostSwitch
  checkmate::assertCount(warmup,positive=TRUE,null.ok = TRUE);checkmate::assertNumeric(warmup,upper=50,null.ok = TRUE) # warmup

  # compute mean for each simulation
  out=list()
  out$n_generations = HostSwitch_simulated_quantities$n_generations
  out$pRes_min      = HostSwitch_simulated_quantities$pRes_min
  out$pRes_max      = HostSwitch_simulated_quantities$pRes_max
  out$K             = HostSwitch_simulated_quantities$K
  out$b             = HostSwitch_simulated_quantities$b
  out$mig           = HostSwitch_simulated_quantities$mig
  out$sd            = HostSwitch_simulated_quantities$sd
  out$sigma         = HostSwitch_simulated_quantities$sigma
  out$n_sim         = HostSwitch_simulated_quantities$n_sim
  out$warmup        = warmup


  # exclude warmup
  if (length(warmup)>0){
    HostSwitch_simulated_quantities[["pRes_sim"]]      = lapply(HostSwitch_simulated_quantities[["pRes_sim"]], function(x) x[-c(1:warmup)])
    HostSwitch_simulated_quantities[["pRes_new_sim"]]  = lapply(HostSwitch_simulated_quantities[["pRes_new_sim"]], function(x) x[-c(1:warmup)])
    HostSwitch_simulated_quantities[["pInd_jump_sim"]] = lapply(HostSwitch_simulated_quantities[["pInd_jump_sim"]], function(x) x[-c(1:warmup)])
  for (i in 1:out$n_sim){
    HostSwitch_simulated_quantities[["pInd_sim"]][[i]] = HostSwitch_simulated_quantities[["pInd_sim"]][[i]][-c(1:warmup)]
    }

  }




  summaryP = data.frame(matrix(NA, ncol = 6, nrow = 3))
  rownames(summaryP) = c("pRes","pRes_new","pInd")
  colnames(summaryP) = c("Min.", "1st Qu.",  "Median" ,   "Mean", "3rd Qu.",    "Max.")
  summaryP[1,] = round(summary(plyr::laply(HostSwitch_simulated_quantities$pRes_sim,mean)),2)
  summaryP[2,] = round(summary(plyr::laply(HostSwitch_simulated_quantities$pRes_new_sim,mean)),2)
  summaryP[3,] = round(summary(plyr::laply(HostSwitch_simulated_quantities$pInd_sim, function(x) mean(unlist(x)))),2)
  out$summaryP=summaryP

  # summary table of jumps and successfult host switches
  summaryHS = data.frame(matrix(NA, ncol = 2, nrow = 2))
  rownames(summaryHS) = c("Total events of dispersion:","Number of successful host switches:")
  colnames(summaryHS) = c("Mean", "Max")
  ## calculate jumps
  summaryHS[1,] = c(round(mean(plyr::laply(HostSwitch_simulated_quantities$pInd_jump_sim,function(x) length(which(x>0)))),2),
                    round(max(plyr::laply(HostSwitch_simulated_quantities$pInd_jump_sim,function(x) length(which(x>0)))),2))

  ## calculate successful host switches
  sucessfullHS = rep(0,out$n_sim)

  for (i in 1:out$n_sim){
    dat = lapply(HostSwitch_simulated_quantities[c(1,2)], `[[`, i)
    sucessfullHS[i] = length(which(dat$pRes_sim[-1]==dat$pRes_new_sim))
  }

  summaryHS[2,] = c(mean(sucessfullHS),max(sucessfullHS))

  out$summaryHS = summaryHS
  methods::new("summaryHostSwitch", out)
}


