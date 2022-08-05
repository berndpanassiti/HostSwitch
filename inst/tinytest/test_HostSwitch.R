# ==================================================================== #
# TITLE                                                                #
# HostSwitch: An R Package to Simulate the Extent of Host-Switching    #
# by a Consumer                                                        #
#                                                                      #
# SOURCE                                                               #
# https://reptalus.github.io/HostSwitch/                               #
# https://CRAN.R-project.org/package=HostSwitch                        #
# https://github.com/berndpanassiti/HostSwitch                         #
#                                                                      #
# LICENCE                                                              #
# (c) 2020-2022 Trivellone, V., Araujo, S.B.L., Panassiti, B.          #
# Developed with passion and dedication.                               #
#                                                                      #
# This R package is free software; you can freely use and distribute   #
# it for both personal and commercial purposes under the terms of the  #
# GNU General Public License version GPL (≥ 3) , as published by       #
# the Free Software Foundation.                                        #
# We created this package for both academic research and teaching.     #
# The package provides functions to investigate host switches by       #
# consumers. It was publicly released in the hope that it will be      #
# useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
#                                                                      #
# Visit our website for introduction on theoretical basis of host      #
# switches, ongoing-developments and updates of the package:           #
# https://reptalus.github.io/HostSwitch/                               #
# ==================================================================== #



# Tests for fct simHostSwitch
## Check if length of vector pInd_jump_sim includes 1 or more NA,
## meaning that the simulation ends with the number of generation (NA = 1) or before (NA > 1)
n_generations = 100
simulatedQuantities = simHostSwitch(n_generations = n_generations)
NA_in_pInd_jump_sim =unlist(simulatedQuantities$pInd_jump_sim)


flatten2 <- function(x) {
  len <- sum(rapply(x, function(z)
    1L))
  y <- vector('list', len)
  i <- 0L
  rapply(x, function(x) {
    i <<- i + 1L
    y[[i]] <<- x
  })
  y
}


if(length(flatten2(simulatedQuantities$pInd_sim))<n_generations+1){ # less
  expect_true(sum(is.na(NA_in_pInd_jump_sim))>1)
}else
{expect_true(sum(is.na(NA_in_pInd_jump_sim))==1)}

## Check for error messages
expect_error(simHostSwitch(jump_back = "Yeees"))
expect_error(simHostSwitch(pRes_min = 0))

## First individuals that survives has penology different than mean(c(pRes_min,pRes_max))
pRes_min = 10
pRes_max = 30
simulatedQuantities = simHostSwitch(pRes_min = pRes_min, pRes_max = pRes_max)
testValue = as.vector(na.omit(unlist(simulatedQuantities$pInd_whichsurv_sim)))[1]
expect_false(testValue==mean(c(pRes_min,pRes_max)))


## Number of matches between pRes_sim and pRes_new_sim is equal to number of successful colonization
## phenotype new resources equal to number of GENERATIONS with survived individuals
simulatedQuantities = simHostSwitch()
sim = as.vector(unlist(simulatedQuantities$pRes_sim))[-1]
new = as.vector(unlist(simulatedQuantities$pRes_new_sim))
testValue = length(which(sim == new))

# N generations with survived individuals
expectValue = length(which(unlist(lapply(simulatedQuantities$pInd_whichsurv_sim[[1]],function(x) is.numeric(x)))))

expect_identical(testValue,expectValue)





# Tests for fct summaryHostSwitch
## Check if phenotypes are within range (1-100)
simulatedQuantities = simHostSwitch()
testValue = any(summaryHostSwitch(simulatedQuantities)[[14]]$Value<101)==TRUE
expect_true(testValue) # phenotypes less than 11

## testing of extremes
### migration 0 -> dispersion and hostswitch 0
testValue = (summaryHostSwitch(simHostSwitch(mig = 0))[[15]]$Value)
expect_equal(testValue,c(0,0))




# Tests for fct survivalProbability
# Test behaviour of fct in relation to |pInd-pOpt|
## Check if identical inputs for pInd and pOpt equal 1
pInd = sample(1:100,1)
pOpt = pInd
testValue = survivalProbability(pInd=pInd,pOpt=pOpt,sigma=1)
expect_identical(testValue,1)

# Check if increasing |pInd-pOpt| results in lower survival probability
testValue1 = survivalProbability(pInd=5,pOpt=10,sigma=1)
testValue2 = survivalProbability(pInd=5,pOpt=20,sigma=1)
expect_true(testValue1>testValue2)

