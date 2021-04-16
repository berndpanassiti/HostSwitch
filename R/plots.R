#' HostSwitch standard plot
#'
#' This function creates a standard plot for the simulated host switches
#'
#' @param HostSwitch_simulated_quantities an object created by \code{\link{simHostSwitch}}
#'
#' @details The function creates a plot with host switches by a parasite. X axis shows the number of generations, y axis the considered phenotpye range.
#'
#' Red squares show the phenotype of the resource (original host), green squares the phenotype of the new host and black dots individual parasites.
#'
#' @import ggplot2
#' @import tidyverse
#' @export
plotHostSwitch <- function(HostSwitch_simulated_quantities){

  n_generations <- pRes_max <- pRes_min <- x <- y <- NULL # global variables

  mytheme <-  theme(axis.title = element_text(size =24),
                    axis.text = element_text(size =25),
                    strip.text= element_text(size = 30),
                    legend.position = "none",
                    plot.title = element_text(hjust = 0.5))

  dat= HostSwitch_simulated_quantities
  pRes_sim     = data.frame(x=0:(length(dat$metadata[[1]])-1),y=dat$metadata[[1]])
  pRes_new_sim = data.frame(x=1:(length(dat$metadata[[2]])),y=dat$metadata[[2]])

  pInd_sim = createPlotInput_sim_pInd(dat$metadata[[3]])

    ggplot2::ggplot(data=pRes_sim,aes(x,y)) + geom_point(fill = 'red',shape=22,size=3) +               # plot resourcce
      xlim(0, n_generations) + ylim(pRes_min,pRes_max)+
      geom_point(data = pRes_new_sim, aes(x,y), col = 'green',size=2) +  # plot new resource
      geom_point(data =  pInd_sim, aes(x,y))+
      labs(y = "Phenotype", x = "Number of generations") +# rename y-axis
      theme_bw() + mytheme
  }
