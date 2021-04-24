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
                    plot.title = element_text(hjust = 0.5),
                    legend.title=element_blank(),
                    legend.text=element_text(size=12))

  dat= HostSwitch_simulated_quantities[c("pRes_sim","pRes_new_sim","pInd_sim","pInd_whichjump_sim","pInd_whichsurv_sim")]
  dat = sapply(dat, "[[", iter)
  pRes_sim     = data.frame(p=rep("pRes",length(dat$pRes_sim)), y=dat$pRes_sim,x=0:(length(dat$pRes_sim)-1))
  pRes_new_sim = data.frame(p=rep("pRes_new",length(dat$pRes_new_sim)),y=dat$pRes_new_sim,x=1:length(dat$pRes_new_sim))

  pInd_sim_df = createPlotInput_sim_pInd(dat$pInd_sim)
  pInd_sim = data.frame(cbind(p=rep("pInd",nrow(pInd_sim_df)),pInd_sim_df))

  whichJump_sim_df = createPlotInput_sim_which(dat$pInd_whichjump_sim)
  whichJump_sim = data.frame(cbind(p=rep("whichJump",nrow(whichJump_sim_df)),whichJump_sim_df))

  whichSurv_sim_df = createPlotInput_sim_which(dat$pInd_whichsurv_sim)
  whichSurv_sim = data.frame(cbind(p=rep("whichSurv",nrow(whichSurv_sim_df)),whichSurv_sim_df))

  plotInput = data.frame(rbind(pRes_sim,pRes_new_sim,pInd_sim,whichJump_sim,whichSurv_sim))

  n_generations = HostSwitch_simulated_quantities$n_generation
  pRes_min = HostSwitch_simulated_quantities$pRes_min; pRes_max = HostSwitch_simulated_quantities$pRes_max


  labels=c("Phenotpye parasites","Phenotype host","Phenotype new host", "Phenotype of jumped parasites", "Phenotype of successful colonizing parasites")

  ggplot2::ggplot(plotInput,aes(x = x, y = y, group = p)) +
    xlim(0, n_generations) + ylim(pRes_min,pRes_max)+
    geom_point(aes(fill = p, shape =p, size=p))+
    scale_shape_manual(values=c(21,22,24,21,21), labels=labels) +
    scale_fill_manual(values=c("black","red","green","blue","yellow"), labels=labels) +
    scale_size_manual(values=c(2,4,4,4,4), labels=labels)+
    labs(y = "Phenotype of parasite", x = "Number of generations") +# rename y-axis
    ggplot2::theme_bw() + mytheme

  }
