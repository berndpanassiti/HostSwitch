#' @title HostSwitch
#' @name HostSwitch
#' @docType package
#' @description Package info: Simulate and visualize the extent of host switching by a parasite
#' @details See vignette for details
#' @vignette("HostSwitch", package="HostSwitch")
NULL


#' Simulate host switches by parasites
#'
#' @param K Carrying capacity
#' @param b Birth rate
#' @param mig Migration rate
#' @param sdm Standard deviation
#' @param sigma Sigma
#' @param pRes_min Smallest phenotype of resource (original host)
#' @param pRes_max Maximum phenotype of resource (original host)
#' @param n_generation Number of generations
#' @param seed Random number to ensure reproducible plots
#' @details The use of this function is to simulate host switches by parasites
#' @return A list with simulated quantities of interest: which can be used for summary statistics or plots. Quantities of interests are phenotpyes of resource (original host, 'pRes_sim'), new resource (new host, 'pRes_new_sim') and of individual parasites ('pInd'). These parameters are available for each generation step.
#' @examples
#' simHostSwitch(K=100,b=10, mig=0.01, sdm=0.2, sigma=1, pRes_min=1, pRes_max=10, n_generation=200)
#' @import tidyverse
#' @import tibble
#' @import dplyr
#' @export


simHostSwitch=function (K,b, mig, sdm,sigma, pRes_min, pRes_max,n_generation,seed=123){
  #set.seed(seed)
  # record quantities of interest
  pRes_sim     = rep(NA,n_generation) # phenotype original host
  pRes_new_sim = rep(NA,n_generation) # phenotype new host
  pInd_sim     = list()               # phenotype of individuals


  pInitial=mean(c(pRes_min,pRes_max)) # Initial phenotype equal for host and individual
  pRes=pInitial; pRes_sim[1]  = pInitial
  pInd=pInitial; pInd_sim[[1]]= pInitial

  n=0

  while(n<n_generation & length(pInd)>0){
    n=n+1
    # Host switch
    pRes_new=pRes_min+(pRes_max-pRes_min)*runif(1) # fct creates phenotype for new host
    which_jump=which(runif(length(pInd))<mig)
    pInd_jump=pInd[which_jump]

    ## Selection in the new host
    prob=exp(-(pInd_jump-pRes_new)^2/(2*sigma^2)) # survival pResobability of jumped individuals; eq. 1
    pInd_new=pInd_jump[prob>runif(length(pInd_jump))]

    if(length(pInd_new)>0){ # Host switch successful, at least 1 individual jumped & survived
      pRes=pRes_new
      pInd=pInd_new
    }
    else{
      # If SOME INDIVIDUALS JUMPED BUT DID NOT SURVIVE, remaining! individuals on original host
      # are further used to calculate survival pResobability on original host and generate new offspResing
      # Note: jumped individuals are not allowed come back!
      if(length(which_jump)>0){pInd=pInd[-which_jump]} # select remaining individuals of original host
      prob=exp(-(pInd-pRes)^2/(2*sigma^2))
      pInd=pInd[prob>runif(length(pInd))]
    }


    # RepResoduction
    descendants=b*length(pInd)
    if(descendants>K){descendants=K} # Carrying capacity = upper limit
    if(length(pInd)>1)   {
      pi_desc=sample(pInd,descendants,replace=TRUE) # randomly select offspResing phenotype with replacement
    }else{
      pInd_desc=rep(pInd,descendants)
    }
    pInd=pInd_desc+rnorm(descendants, mean=0, sd=sdm) # fct adds variation to offspResing phenotype

    pRes_sim[n+1]     = pRes
    pRes_new_sim[n+1] = pRes_new
    pInd_sim[[n+1]]   = pInd
  }


  # Store parameters and arguments of interst
  pRes_sim     = pRes_sim[!is.na(pRes_sim)]         # remove NA
  pRes_new_sim = pRes_new_sim[!is.na(pRes_new_sim)] # remove NA

  HostSwitch_simulated_quantities =
    tibble::tibble(
      character = c("pRes_sim", "pRes_new_sim","pInd_sim","n_generation","pRes_min","pRes_max"),
      metadata = list(pRes_sim,pRes_new_sim,pInd_sim,n_generation,pRes_min,pRes_max))
  return(HostSwitch_simulated_quantities)
}
