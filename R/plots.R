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

  mytheme <-  ggplot2::theme(axis.title = element_text(size =24),
                    axis.text = element_text(size =25),
                    strip.text= element_text(size = 30),
                    plot.title = element_text(hjust = 0.5))

  dat= HostSwitch_simulated_quantities
  pRes_sim     = data.frame(x=0:(length(dat$metadata[[1]])-1),y=dat$metadata[[1]])
  pRes_new_sim = data.frame(x=1:(length(dat$metadata[[2]])),y=dat$metadata[[2]])

  pInd_sim = createPlotInput_sim_pInd(dat$metadata[[3]])

  n_generations = dat$metadata[[4]]
  pRes_min = dat$metadata[[5]]; pRes_max = dat$metadata[[6]]

    ggplot2::ggplot() +
      xlim(0, n_generations) + ylim(pRes_min,pRes_max)+
      geom_point(data=pRes_sim,aes(x,y,color="Host",shape="Host"),size=4) +  # plot resourcce
      geom_point(data = pRes_new_sim, aes(x,y, color = "New host",shape="New host"),size=4) +  # plot new resource
      geom_point(data =  pInd_sim, aes(x,y, color = "Parasite",shape="Parasite"))+
      labs(y = "Phenotype", x = "Number of generations") +# rename y-axis
      ggplot2::theme_bw() + mytheme +
      scale_color_manual(name="",values=c("Host"="red","New host"="green","Parasite"="black"))+
      scale_shape_manual(name="",values=c("Host"=15,"New host"=16,"Parasite"=20))
  }
