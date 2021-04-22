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
#' @param mig Migration rate
#' @param sd Standard deviation for mutation
#' @param sigma Standard deviation for selection
#' @param pRes_min Initial value, smallest phenotype of resource (original host) and parasite
#' @param pRes_max Initial value, maximum phenotype of resource (original host) and parasite
#' @param n_generation Number of generations
#' @param jump_back Options for parasites that do not survive on the new host. If "yes" the parasite(s) jump back to the current host and will be considered in the selective pressure and reproduction stage for the n+1 generation, if "no" (default) it dies on the new host.
#' @param seed Random number to ensure reproducible plots.
#' @details This function simulates the number of host switches by the population of a parasite. Results are stored to a HostSwitch object, to make use of summary and plotting functions in the HostSwitch package. The HostSwitch object includes the following simulated quantities are: $pRes_sim (all the optimal phenotypes favored by the selected new hosts), $pRes_new_sim (new resource), $pInd and of individual parasite. These simulated quantities of interest are available for each generation step and can be used for summary statistics or plots.
#' @return An object of class HostSwitch
#' @examples
#' HostSwitch_simulated_quantities = simHostSwitch(K=100,b=10, mig=0.01, sd=0.2, sigma=1, pRes_min=1, pRes_max=10, n_generation=200)
#' HostSwitch_simulated_quantities
#' @import ArgumentCheck
#' @export


simHostSwitch=function (K=100,b=10, mig=0.01, sd=0.2,sigma=1, pRes_min=1, pRes_max=10,n_generation=200,jump_back='no',seed=NULL){
  set.seed(seed)

  #* Establish a new 'ArgCheck' object
  Check <- ArgumentCheck::newArgCheck()

  #* Add a warning
  if (!jump_back %in% c("no","yes")){
    ArgumentCheck::addWarning(
      msg = "'jump_back' must be either 'no' or 'yes'! 'jump_back has been set to 'yes'.",
      argcheck = Check)}



  # record quantities of interest
  pRes_sim      = rep(NA,n_generation) ### phenotype original host (Valeria: vector of optimum phenotypes favored by the new host)
  pRes_new_sim  = rep(NA,n_generation) ### phenotype new host (Valeria: ???)
  pInd_sim      = list()               # phenotype of individuals (valeria: phenotype of individuals at each generation)
  pInd_jump_sim = rep(0,n_generation) # jumped parasites (Valeria: vector of number of parasites that disperse/colonized??  )

  pInitial=mean(c(pRes_min,pRes_max)) ### Initial phenotype equal for host and individual (Valeria: the Initial phenotype for the parasite at n=0 is the average value of the IS)
  pRes=pInitial; pRes_sim[1]  = pInitial # The sine qua non condition for the simulation to starts is to have the first individual parasite having the phenotype equal to optimum favored by the current host.
  pInd=pInitial; pInd_sim[[1]]= pInitial # ....

  n=0

  while(n<n_generation & length(pInd)>0){
    n=n+1
    # Host switch
    pRes_new=pRes_min+(pRes_max-pRes_min)*stats::runif(1) ### fct creates phenotype for new host
    which_jump=which(stats::runif(length(pInd))<mig) #
    pInd_jump=pInd[which_jump]
    pInd_jump_sim[n+1] = length(pInd_jump)
    ## Selection in the new host
    prob=survivalProbability(pInd=pInd_jump,pHost=pRes_new,sigma=sigma) # survival probability of jumped individuals; eq. 1
    pInd_new=pInd_jump[prob>stats::runif(length(pInd_jump))]

    if(length(pInd_new)>0){ # Host switch successful, at least 1 individual jumped & survived
      pRes=pRes_new
      pInd=pInd_new
    }
    else{
      # If SOME INDIVIDUALS JUMPED BUT DID NOT SURVIVE, remaining! individuals on original host
      # are further used to calculate survival probability on original host and generate new offspring
      # Note: jumped individuals are not allowed come back!
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

  out = list()
  out$pRes_sim = pRes_sim
  out$pRes_new_sim = pRes_new_sim
  out$pInd_sim = pInd_sim
  out$n_generation = n_generation
  out$pRes_min= pRes_min
  out$pRes_max= pRes_max
  out$pInd_jump_sim= pInd_jump_sim
  class(out) = "HostSwitch"

  #* Return errors and warnings (if any)
  ArgumentCheck::finishArgCheck(Check)


  return(out)

}
