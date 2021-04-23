#' HostSwitch standard plot
#'
#' This function creates a standard plot for the simulated host switches
#'
#' @param HostSwitch_simulated_quantities An object created by \code{\link{simHostSwitch}}
#' @param iter Iteration of simulation that is plotted
#'
#' @details The function creates a plot with host switches by a parasite. X axis shows the number of generations, y axis the considered phenotpye range.
#'
#' Red squares show the phenotype of the resource (original host), green squares the phenotype of the new host and black dots individual parasites.
#'
#' @import ggplot2
#' @examples
#' HostSwitch_simulated_quantities = simHostSwitch(K=100,b=10, mig=0.01, sd=0.2, sigma=1, pRes_min=1, pRes_max=10, n_generation=200,iter=100)
#' plotHostSwitch(HostSwitch_simulated_quantities,iter=1)
#' @export
plotHostSwitch <- function(HostSwitch_simulated_quantities,iter=1){

  n_generations <- pRes_max <- pRes_min <- x <- y <- NULL # global variables

  mytheme <-  ggplot2::theme(axis.title = element_text(size =24),
                    axis.text = element_text(size =25),
                    strip.text= element_text(size = 30),
                    plot.title = element_text(hjust = 0.5))

  dat= HostSwitch_simulated_quantities[c("pRes_sim","pRes_new_sim","pInd_sim")]
  dat = sapply(dat, "[[", iter)
  pRes_sim     = data.frame(x=0:(length(dat$pRes_sim)-1),y=dat$pRes_sim)
  pRes_new_sim = data.frame(x=1:length(dat$pRes_new_sim),y=dat$pRes_new_sim)

  pInd_sim = createPlotInput_sim_pInd(dat$pInd_sim)

  n_generations = HostSwitch_simulated_quantities$n_generation
  pRes_min = HostSwitch_simulated_quantities$pRes_min; pRes_max = HostSwitch_simulated_quantities$pRes_max

    ggplot2::ggplot() +
      xlim(0, n_generations) + ylim(pRes_min,pRes_max)+
      geom_point(data =  pInd_sim, aes(x,y, color = "pInd",shape="pInd"))+
      geom_point(data=pRes_sim,aes(x,y,shape="pInd favered by pRes"),size=4) +  # plot resourcce
      geom_point(data = pRes_new_sim, aes(x,y, shape="pInd favered by pRes_new"),size=4) +  # plot new resource
      labs(y = "Phenotype of parasite", x = "Number of generations") +# rename y-axis
      ggplot2::theme_bw() + mytheme +
      scale_color_manual(name="",values=c("pInd"="black"))+
      scale_shape_manual(name="",values=c("pInd favered by pRes"=5,"pInd favered by pRes_new"=1,"pInd"=20))
  }
