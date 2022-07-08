#' Survival probability of the Consumer in a novel Resource
#'
#' @param pInd Phenotype of ith Consumer attempting to disperse on the novel Resource
#' @param pOpt The optimum phenotype the Consumer should have to maximize the colonization success
#' @param sigma Standard deviation of the survival function (see Details for more explanations)
#' @details This function calculates the survival probability of individual consumers that attempt dispersal to a new host. It is the core function of \code{\link{simHostSwitch}}.
#' The probability of survival of each individual of the consumer to a novel Resource follows a normal distribution.
#' The formula is formalized as follows \deqn{P(pInd,pOpt) = e^{-\frac{(pInd-pOpt)^2}{2\sigma^2}}}
#' The normalizing constant \deqn{ NC = \frac{1}{\sigma(\sqrt(2\pi))}} is ignored here.\\
#' "Sigma" the higher the sigma, the lower the selection and the higher the probability of surviving. Ecologically this value may be related to the niche breadth for the Consumer (species).
#' @return The survival probability of the consumer
#' @examples
#' ## Example 1a - The ith consumer has the phenotype that maximize its
#' ## colonization success on the new host, then pInd is equal to pOpt (pInd = pOpt),
#' ## and the survival probability is 1.
#' survivalProbability(pInd=5,pOpt=5,sigma=1)
#'
#' ## Example 1b - Increasing |pInd-pOpt| the survival probability decreases
#' survivalProbability(pInd=5,pOpt=30,sigma=1)
#'
#' ## Example 1c - Give a |pInd-pOpt|> 1, increases sigma the survival probability increases
#' survivalProbability(pInd=5,pOpt=30,sigma=1)
#'
#'
#' @export

survivalProbability = function(pInd,pOpt,sigma){
  exp(-(pInd-pOpt)^2/(2*sigma^2))
}


#' Simulates the number of dispersion and successful host switch events by individuals of the Consumer until all individuals die.
#' @param data A matrix or dataset, the columns may indicate different types of Consumes characterized by a specific set of parameters (rows), see details. Default value: NULL.
#' @param column Used together with data argument; indicate the column name, string. Default value: NULL.
#' @param K Carrying capacity, positive integer (min=1, max=1000), default value: 100.
#' @param b net reproduction rate; average number of offspring that a population of the Consumer produces at each generation, numeric value (min=0), default value: 10.
#' @param mig define the proportion of successful migrating individuals at each generation, numeric value (min=0, max=1), default value: 0.01.
#' @param sd Standard deviation for mutation, numeric value (min=0, max=10), default value: 0.2.
#' @param sigma Standard deviation of the survival function, numeric value (min=0, max=10), default value: 1.
#' @param pRes_min  smallest optimum phenotype value imposed by the Resource, numeric value (min=1, max=pRes_max), default value: 1.
#' @param pRes_max highest optimum phenotype value imposed by the Resource, numeric value (min=pRes_min, max=100), default value: 10.
#' @param n_generations Number of generations, positive integer (min=1, max=50000), default value: 200.
#' @param jump_back Option for consumers that do not survive on the novel resource. If "yes" the consumer(s) jump back to the current resource and will be considered in the selective pressure and reproduction stage for the n+1 generation, if "no" (default) it dies on the new host.
#' @param seed a single value useful for creating simulations or random objects that can be reproduced, positive integer (>0), default value: NULL.
#' @param n_sim Number of simulations, positive integer (min=1, max = 50000), default value: 1.
#' @param nInitConsumer propagule size (or number of initial individuals) at the generation n = 0, default value: 20.
#' @details
#' This function simulates the number of host switches by the population of a consumer.
#' There are 2 ways to provide parameters to the \code{\link{simHostSwitch}} function:
#' \describe{
#'   \item{data}{\bold{"data","column"}: Provide names of matrix/dataframe and column, e.g. data= "parli$Cephaloleia", column = "Cb.mLxjN"}
#'   \item{parameter}{\bold{individual parameter}: e.g. b=5, n_generations=500, etc...}
#' }
#'If no data/column or individual parameters are provided, default parameter values are used.
#'The rownames of the data must match the parameter argument names. You may use one of the \code{\link{parli}}
#'datasets as a template.\cr\cr
#' Results are stored to an object of class \sQuote{HostSwitch}.
#' to make use of summary and plotting functions in the \pkg{HostSwitch} package.
#' Please note that when arguments "data" and "column" are provided, the results are stored to the global environment
#' using the colname provided to the argument "column" (in our example above Cb.mLxjN).
#' \cr\cr
#' The object of class \sQuote{'HostSwitch} includes the following simulated quantities:
#' \describe{
#'   \item{pRes_sim}{\bold{$pRes_sim}:  a vector of the optimum phenotypes (one for each generation) that Consumers should have to be favored by the current Resource.}
#'   \item{pRes_new_sim}{\bold{$pRes_new_sim}: a vector of the optimum phenotypes (one for each generation) that Consumers should have to be favored by the novel Resource.}
#'   \item{pInd_sim}{\bold{$pInd_sim}: list of vectors that includes the individual phenotype values of the Consumers in the population of each generation.}
#'   \item{pInd_jump_sim}{\bold{$pInd_jump_sim}: vector of number of migrating individuals at each generation. The vector length is always equal to the 'n_generation' parameter, if the simulation ends before the 'n_generation' value then the vector will include a 'NA' by default.}
#'   \item{pInd_whichjump_sim}{\bold{$pInd_whichjump_sim}: list of vectors that extracts the individual phenotype values of the Consumers who disperse in a novel Resource in each population and generation.}
#'   \item{pInd_whichsurv_sim}{\bold{$pInd_whichsurv_sim}: list of vectors that extracts the individual phenotype values of the Consumers who successful colonize a novel Resource in each population and generation.}
#' }
#' These simulated quantities of interest are available for each generation step and can be used for summary statistics and plots using functions \code{\link{summaryHostSwitch}} and \code{\link{plotHostSwitch}}, respectively.\cr
#'
#' Note: One important aspect of \emph{simHostswitch} is that it is based on the \code{\link{survivalProbability}} function.
#'
#' @seealso \code{\link{survivalProbability}}, \code{\link{summaryHostSwitch}}, \code{\link{plotHostSwitch}}
#'
#' @return An object of class \sQuote{HostSwitch}.
#' @examples
#' m1 = simHostSwitch() # using default values for arguments
#'
#' data(parli)
#' Cephaloleia=parli$Cephaloleia
#' m2 = simHostSwitch(data=Cephaloleia, column="Cb.mLxjN")
#'
#' \dontrun{
#' simHostSwitch(sigma=100)}
#'
#' @import checkmate
#' @export


simHostSwitch=function (data=NULL, column=NULL, K=100,b=10, mig=0.01, sd=0.2,sigma=1, pRes_min=1, pRes_max=10,n_generations=200,jump_back='no',seed=NULL, n_sim=1,nInitConsumer=20){
  fctArgs <- match.call()

  if(!is.null(data)){
    checkmate::assert(checkmate::checkMatrix(data),checkmate::checkDataFrame(data))
    checkmate::assertCharacter(column)
    checkmate::assertString(column)

    if (!column %in% colnames(data)) {
      stop(column, " not a colname of data")
    }
  usedParamters=NULL
  if('K' %in% names(data[,column])){K = as.numeric(data[,column]['K']);usedParamters=append(usedParamters,"K")}
  if('b' %in% names(data[,column])){b = as.numeric(data[,column]['b']);usedParamters=append(usedParamters,"b")}
  if('mig' %in% names(data[,column])){mig = as.numeric(data[,column]['mig']);usedParamters=append(usedParamters,"mig")}
  if('sd' %in% names(data[,column])){sd = as.numeric(data[,column]['sd']);usedParamters=append(usedParamters,"sd")}
  if('sigma' %in% names(data[,column])){sigma = as.numeric(data[,column]['sigma']);usedParamters=append(usedParamters,"sigma")}
  if('pRes_min' %in% names(data[,column])){pRes_min = as.numeric(data[,column]['pRes_min']);usedParamters=append(usedParamters,"pRes_min")}
  if('pRes_max' %in% names(data[,column])){pRes_max = as.numeric(data[,column]['pRes_max']);usedParamters=append(usedParamters,"pRes_max")}
  if('n_generations' %in% names(data[,column])){n_generations = as.numeric(data[,column]['n_generations']);usedParamters=append(usedParamters,"n_generations")}
  if('jump_back' %in% names(data[,column])){jump_back = data[,column]['jump_back'];usedParamters=append(usedParamters,"jump_back")}
  if('seed' %in% names(data[,column])){seed = as.numeric(data[,column]['seed']);usedParamters=append(usedParamters,"seed")}
  if('n_sim' %in% names(data[,column])){n_sim = as.numeric(data[,column]['n_sim']);usedParamters=append(usedParamters,"n_sim")}
  if('nInitConsumer' %in% names(data[,column])){n_sim = as.numeric(data[,column]['nInitConsumer']);usedParamters=append(usedParamters,"nInitConsumer")}
  #print(paste("Parameters provided from your data are: ",do.call(paste, c(as.list(usedParamters), sep = ",")),sep=""))

   # overwrite if single arguments are provided additionally to dataset
  if("K" %in% names(fctArgs)){
    ArgPosition = which(names(fctArgs) == "K");K = fctArgs[[ArgPosition]]}
  if("b" %in% names(fctArgs)){
    ArgPosition = which(names(fctArgs) == "b");b = fctArgs[[ArgPosition]]}
  if("mig" %in% names(fctArgs)){
    ArgPosition = which(names(fctArgs) == "mig");mig = fctArgs[[ArgPosition]]}
  if("sd" %in% names(fctArgs)){
    ArgPosition = which(names(fctArgs) == "sd");sd = fctArgs[[ArgPosition]]}
  if("sigma" %in% names(fctArgs)){
    ArgPosition = which(names(fctArgs) == "sigma");sigma = fctArgs[[ArgPosition]]}
  if("pRes_min" %in% names(fctArgs)){
    ArgPosition = which(names(fctArgs) == "pRes_min");pRes_min = fctArgs[[ArgPosition]]}
  if("pRes_max" %in% names(fctArgs)){
    ArgPosition = which(names(fctArgs) == "pRes_max");pRes_max = fctArgs[[ArgPosition]]}
  if("n_generations" %in% names(fctArgs)){
    ArgPosition = which(names(fctArgs) == "n_generations");n_generations = fctArgs[[ArgPosition]]}
  if("jump_back" %in% names(fctArgs)){
    ArgPosition = which(names(fctArgs) == "jump_back");jump_back = fctArgs[[ArgPosition]]}
  if("seed" %in% names(fctArgs)){
    ArgPosition = which(names(fctArgs) == "seed");seed = fctArgs[[ArgPosition]]}
  if("n_sim" %in% names(fctArgs)){
    ArgPosition = which(names(fctArgs) == "n_sim");n_sim = fctArgs[[ArgPosition]]}
  if("nInitConsumer" %in% names(fctArgs)){
    ArgPosition = which(names(fctArgs) == "nInitConsumer");nInitConsumer = fctArgs[[ArgPosition]]}
}

  # check on parameters
  checkmate::assertCount(K,positive=TRUE);checkmate::assertNumeric(K,upper=1000) # K
  checkmate::assertNumeric(b,lower=0,upper=K) # b
  checkmate::assertNumeric(mig,lower=0,upper=1) # mig
  checkmate::assertNumeric(sd,lower=0,upper=10) # sd
  checkmate::assertNumeric(sigma,lower=0,upper=10) # sigma
  checkmate::assertNumeric(pRes_min,lower=0,upper=pRes_max) # pRes_min
  checkmate::assertNumeric(pRes_min,lower=pRes_min,upper=100) # pRes_max
  checkmate::assertCount(n_generations,positive=TRUE);checkmate::assertNumeric(n_generations,upper=50000) # n_generations
  checkmate::assertChoice(jump_back, c("no","yes"))
  checkmate::assertCount(seed,positive=TRUE,null.ok = TRUE) # seed
  checkmate::assertCount(n_sim,positive=TRUE);checkmate::assertNumeric(n_sim,upper=50000) # n_sim
  checkmate::assertCount(nInitConsumer,positive=TRUE);checkmate::assertNumeric(nInitConsumer,upper=K) # nInitConsumer



  set.seed(seed)


  pRes_sim_list           = vector(mode = "list", length = n_sim)
  pRes_new_sim_list       = vector(mode = "list", length = n_sim)
  pInd_sim_list           = vector(mode = "list", length = n_sim)
  pInd_jump_sim_list      = vector(mode = "list", length = n_sim)
  pInd_whichjump_sim_list = vector(mode = "list", length = n_sim)
  pInd_whichsurv_sim_list = vector(mode = "list", length = n_sim)

  for (i in 1:n_sim){

  # record quantities of interest
  pRes_sim           = rep(NA,n_generations) ### at each generation a optimum phenotype that consumers should have to be favored by the current Resource
  pRes_new_sim       = rep(NA,n_generations) ### at each generation a optimum phenotype that consumers should have to be favored by the novel Resource
  pInd_sim           = vector(mode = "list", length = n_generations+1) # phenotype of individuals at each generation
  pInd_jump_sim      = rep(NA,n_generations)  # number of consumers that disperse
  pInd_whichjump_sim = vector(mode = "list", length = n_generations+1) # which consumers jumped
  pInd_whichsurv_sim = vector(mode = "list", length = n_generations+1) # which consumers survived

  pInitial=mean(c(pRes_min,pRes_max)) ### Initial phenotype for the consumer at n=0
  pRes=pInitial; pRes_sim[1]  = pInitial # The sine qua non condition for the simulation to starts is to have the first individual consumer having the phenotype equal to optimum favored by the current host.
  pInd=rep(pInitial,nInitConsumer); pInd_sim[[1]]= rep(pInitial,nInitConsumer) # ....

  n=0

  while(n<n_generations & length(pInd)>0){
    n=n+1
    # Host switch
    pRes_new=pRes_min+(pRes_max-pRes_min)*stats::runif(1) ### fct creates phenotype for new host
    which_jump=which(stats::runif(length(pInd))<mig) # position of individuals
    pInd_jump=pInd[which_jump] # selected phenotype depending on position

    pInd_jump_sim[n+1]        = length(pInd_jump) # record how many individuals jumped
    if(length(pInd_jump)>0){
    pInd_whichjump_sim[[n+1]] = pInd_jump         # record which individuals jumped
    } else{
      pInd_whichjump_sim[[n+1]] = NA
    }

    ## Selection in the new host
    prob=survivalProbability(pInd=pInd_jump,pOpt=pRes_new,sigma=sigma) # survival probability of jumped individuals; eq. 1
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

      # all individuals: pInd
      # 1. case: no jump: only ind of of old resource
      # 2. case: length(which_jump)>0 & jump back yes: all individuals

      prob=survivalProbability(pInd=pInd,pOpt=pRes,sigma=sigma)
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

  # assign new simlations to prelocated lists
  pRes_sim_list[[i]]           = pRes_sim
  pRes_new_sim_list[[i]]       = pRes_new_sim
  pInd_sim_list[[i]]           = pInd_sim
  pInd_jump_sim_list[[i]]      = pInd_jump_sim
  pInd_whichjump_sim_list[[i]] = pInd_whichjump_sim
  pInd_whichsurv_sim_list[[i]] = pInd_whichsurv_sim

}





out = list(
pRes_sim           = pRes_sim_list,
pRes_new_sim       = pRes_new_sim_list,
pInd_sim           = pInd_sim_list,
n_generations      = n_generations,
pRes_min           = pRes_min,
pRes_max           = pRes_max,
pInd_jump_sim      = pInd_jump_sim_list,
pInd_whichjump_sim = pInd_whichjump_sim_list,
pInd_whichsurv_sim = pInd_whichsurv_sim_list,
K                  = K,
b                  = b,
mig                = mig,
sd                 = sd,
sigma              = sigma,
n_sim              = n_sim,
jump_back          = jump_back,
seed               = seed,
nInitConsumer      = nInitConsumer)
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
  cat("K:",object$K,", b:",object$b,", mig:",object$mig,", sd:",object$sd,", sigma:",object$sigma,", pRes_min:",object$pRes_min,", pRes_max:",object$pRes_max,"\n",sep="")
  cat("n_generations:",object$n_generations,", jump_back:",object$jump_back,", seed:",object$seed,", n_sim:",object$n_sim,", warmup:",object$warmup,", nInitConsumer:",object$nInitConsumer,"\n\n",sep="")

  cat("Summary of phenotypes:\n")
  print(object$summaryP)
  cat("\nSummary of host switches by consumers:\n")
  print(object$summaryHS)
  invisible(NULL)
})


#' Summary statistics of HostSwitch simulation
#'
#' @param HostSwitch_simulated_quantities An object created by \code{\link{simHostSwitch}}
#' @param warmup warmup is the number of initial generations to be excluded from summary statistics, see details. Possible value are NULL or positive integer (min=1, max=50), default value = 1
#' @details This function generates summary statistics for HostSwitch simulations.
#' Quantities of interest for each simulation are averaged. If \emph{n_sim = 1}, these averages for this single simulation are shown. If \emph{n_sim > 1}, summary statistics are applied on the simulation averages.\cr\cr
#' \strong{Warmup} represents the initial condition for the simulation, the users may defined it as an adaptation stage of the simulation model.
#' If warmup = 1 the generation at time 0 is excluded from summary, if warmup = 2 the generations at times 0 and 1 are excluded and so on.
#' If warmup = NULL all generations are considered for summary statistics.
#'
#' @return Summary of HostSwitch simulations
#' @examples
#' ## Create an object HostSwitch with 100 simulations and default values for all the other parameters
#' m1 = simHostSwitch(n_sim=100)
#'
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
  out=list(
  n_generations = HostSwitch_simulated_quantities$n_generations,
  pRes_min      = HostSwitch_simulated_quantities$pRes_min,
  pRes_max      = HostSwitch_simulated_quantities$pRes_max,
  K             = HostSwitch_simulated_quantities$K,
  b             = HostSwitch_simulated_quantities$b,
  mig           = HostSwitch_simulated_quantities$mig,
  sd            = HostSwitch_simulated_quantities$sd,
  sigma         = HostSwitch_simulated_quantities$sigma,
  n_sim         = HostSwitch_simulated_quantities$n_sim,
  jump_back     = HostSwitch_simulated_quantities$jump_back,
  seed          = HostSwitch_simulated_quantities$seed,
  nInitConsumer = HostSwitch_simulated_quantities$nInitConsumer,
  warmup        = warmup
)

  # exclude warmup
  if (length(warmup)>0){
    HostSwitch_simulated_quantities[["pRes_sim"]]      = lapply(HostSwitch_simulated_quantities[["pRes_sim"]], function(x) x[-c(1:warmup)])
    HostSwitch_simulated_quantities[["pRes_new_sim"]]  = lapply(HostSwitch_simulated_quantities[["pRes_new_sim"]], function(x) x[-c(1:warmup)])
    HostSwitch_simulated_quantities[["pInd_jump_sim"]] = lapply(HostSwitch_simulated_quantities[["pInd_jump_sim"]], function(x) x[-c(1:warmup)])
  for (i in 1:out$n_sim){
    HostSwitch_simulated_quantities[["pInd_sim"]][[i]] = HostSwitch_simulated_quantities[["pInd_sim"]][[i]][-c(1:warmup)]
    }

  }



# more than 1 simulation; n_sim > 1
if (out$n_sim>1){
  summaryP = data.frame(matrix(NA, ncol = 6, nrow = 3))
  rownames(summaryP) = c("pRes","pRes_new","pInd")
  colnames(summaryP) = c("Min.", "1st Qu.",  "Median" ,   "Mean", "3rd Qu.",    "Max.")
  summaryP[1,] = round(summary(plyr::laply(HostSwitch_simulated_quantities$pRes_sim,mean)),2)
  summaryP[2,] = round(summary(as.numeric(na.omit(plyr::laply(HostSwitch_simulated_quantities$pRes_new_sim,mean)))),2)
  summaryP[3,] = round(summary(as.numeric(na.omit(plyr::laply(HostSwitch_simulated_quantities$pInd_sim, function(x) mean(unlist(x)))))),2)
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
}


  # only 1 simulation; n_sim = 1
  if (out$n_sim==1){
    summaryP = data.frame(matrix(NA, ncol = 1, nrow = 3))
    rownames(summaryP) = c("pRes","pRes_new","pInd")
    colnames(summaryP) = c("Value (simulation average)")
    summaryP[1,] = round(as.numeric(plyr::laply(HostSwitch_simulated_quantities$pRes_sim,mean)),2)
    summaryP[2,] = round(as.numeric(plyr::laply(HostSwitch_simulated_quantities$pRes_new_sim,mean)),2)
    summaryP[3,] = round(as.numeric(plyr::laply(HostSwitch_simulated_quantities$pInd_sim, function(x) mean(unlist(x)))),2)
    out$summaryP=summaryP

    # summary table of jumps and successfult host switches
    summaryHS = data.frame(matrix(NA, ncol = 1, nrow = 2))
    rownames(summaryHS) = c("Total events of dispersion:","Number of successful host switches:")
    colnames(summaryHS) = c("Value (simulation average)")
    ## calculate jumps
    summaryHS[1,1] = round(length(which(unlist(HostSwitch_simulated_quantities$pInd_jump_sim)>0)),2)

    ## calculate successful host switches
    dat = lapply(HostSwitch_simulated_quantities[c(1,2)], `[[`, 1)
    summaryHS[2,1] = length(which(dat$pRes_sim[-1]==dat$pRes_new_sim))
  }


  out$summaryHS = summaryHS
  methods::new("summaryHostSwitch", out)
}


