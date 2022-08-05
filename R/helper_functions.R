#' Flatten a nested list
#' @param x A nested list
#' @details Flattens a nested list; https://stackoverflow.com/questions/8139677/how-to-flatten-a-list-to-a-list-without-coercion
#' @keywords internal

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



#' Create input data of simulated jumped/survived individuals for plots
#' @param x A list of simulated jumped/survived parasite phenotypes for each generation
#' @details Converts list to dataframe and stack it
#' @keywords internal

createPlotInput_Ind_sim <- function(x) {
  xx        = flatten2(x)
  # "-1" need to start from generation 0!
  parasites = utils::stack(setNames(xx, seq_along(xx) - 1))
  colnames(parasites)   = c("y", "x")
  parasites$x           = as.numeric(as.character(parasites$x))
  return(parasites)
}


#' Check valid parameters of function simHostSwitch
#' @param data A list of arguments
#' @details Uses checkmate to evaluate arguments (either default or provided by the user) from simHostSwitch function
#' @keywords internal

check_valid_parameters <- function(data){

  with(
    data,
    {
      # check on paramters
      coll <- checkmate::makeAssertCollection()
      checkmate::assertCount(K, positive = TRUE,add = coll)
      checkmate::assertNumeric(K, upper = 1000,add = coll) # K
      checkmate::assertNumeric(b, lower = 0, upper = K,add = coll) # b
      checkmate::assertNumeric(mig, lower = 0, upper = 1,add = coll) # mig
      checkmate::assertNumeric(sd, lower = 0, upper = 10,add = coll) # sd
      checkmate::assertNumeric(sigma, lower = 0, upper = 10,add = coll) # sigma
      checkmate::assertNumeric(pRes_min, lower = 1, upper = pRes_max,add = coll) # pRes_min
      checkmate::assertNumeric(pRes_max, lower = pRes_min, upper = 100,add = coll) # pRes_max
      checkmate::assertCount(n_generations, positive = TRUE,add = coll)
      checkmate::assertNumeric(n_generations, upper = 50000,add = coll) # n_generations
      checkmate::assertChoice(jump_back, c("no", "yes"),add = coll)
      checkmate::assertCount(seed, positive = TRUE, null.ok = TRUE,add = coll) # seed
      checkmate::assertInt(n_sim, lower = 1, upper = 50000,add = coll) # n_sim
      checkmate::assertInt(nInitConsumer, lower=1,upper = K,add = coll) # nInitConsumer
      checkmate::reportAssertions(coll)
    }
  )

}
