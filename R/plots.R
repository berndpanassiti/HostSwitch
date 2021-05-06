#' HostSwitch standard plot
#'
#' This function creates a standard plot for the simulated host switches
#'
#' @param HostSwitch_simulated_quantities The object HostSwitch created by \code{\link{simHostSwitch}}
#' @param iter the iteration of the simulation plotted. The "iter" values possible are saved in the HostSwitch object, the plot from the first iteration is shown as default (iter = 1).An error message "Error in FUN(X[[i]], ...) : subscript out of bounds" will be returned, if the called iteration is not present in the object HostSwitch.
#'
#' @details The function plots the simulation for the dispersal, colonization (successful host switch), and reproduction of a parasite. The X-axis shows the number of generations, the y-axis the phenotype values of the parasite.
#'
#' Black dots are the phenotype values after each event of reproduction. The green squares represent the value of phenotype favored by a new host proposed at each generation. The red squares are the phenotype values favored by the current host (=the initial one and the host after successful colonization).
#'
#' @import ggplot2
#' @examples
#' HostSwitch_simulated_quantities = simHostSwitch(K=100,b=10, mig=0.01, sd=0.2, sigma=1, pRes_min=1, pRes_max=10, n_generation=200,iter=100)
#'
#' ## The first iteration results are plotted as default
#' plotHostSwitch(HostSwitch_simulated_quantities)
#'
#' ## The 50th iteration is plotted
#' plotHostSwitch(HostSwitch_simulated_quantities,iter=50)
#'
#' @export
plotHostSwitch <- function(HostSwitch_simulated_quantities,iter=1){

  n_generations <- pRes_max <- pRes_min <- x <- y <- NULL # global variables

  mytheme <-  ggplot2::theme(axis.title   = ggplot2::element_text(size =24),
                             axis.text    = ggplot2::element_text(size =25),
                             strip.text   = ggplot2::element_text(size = 30),
                             plot.title   = ggplot2::element_text(hjust = 0.5),
                             legend.title = ggplot2::element_blank(),
                             legend.text  = ggplot2::element_text(size=12))

  dat= HostSwitch_simulated_quantities[c("pRes_sim","pRes_new_sim","pInd_sim","pInd_whichjump_sim","pInd_whichsurv_sim")]
  dat = sapply(dat, "[[", iter)
  pRes_sim     = data.frame(p=rep("pRes",length(dat$pRes_sim)), y=dat$pRes_sim,x=0:(length(dat$pRes_sim)-1))
  pRes_new_sim = data.frame(p=rep("pRes_new",length(dat$pRes_new_sim)),y=dat$pRes_new_sim,x=0:(length(dat$pRes_new_sim)-1))

  pInd_sim_df = createPlotInput_Ind_sim(dat$pInd_sim)
  pInd_sim = data.frame(cbind(p=rep("pInd",nrow(pInd_sim_df)),pInd_sim_df))

  whichJump_sim_df = createPlotInput_Ind_sim(dat$pInd_whichjump_sim)
  whichJump_sim = data.frame(cbind(p=rep("whichJump",nrow(whichJump_sim_df)),whichJump_sim_df))

  whichSurv_sim_df = createPlotInput_Ind_sim(dat$pInd_whichsurv_sim)
  whichSurv_sim = data.frame(cbind(p=rep("whichSurv",nrow(whichSurv_sim_df)),whichSurv_sim_df))

  plotInput = data.frame(rbind(pRes_sim,pRes_new_sim,pInd_sim,whichJump_sim,whichSurv_sim))

  n_generations = HostSwitch_simulated_quantities$n_generation
  pRes_min = HostSwitch_simulated_quantities$pRes_min; pRes_max = HostSwitch_simulated_quantities$pRes_max


  labels=c("pInd: Phenotpye parasites","pInd favored by current host","pInd favored by new host", "Phenotype of dispersing parasites", "Phenotype of colonizing parasites")

  ggplot2::ggplot(plotInput,aes(x = x, y = y, group = p)) +
    xlim(0, n_generations) + ylim(pRes_min,pRes_max)+
    geom_point(aes(fill = p, shape =p, size=p), na.rm=TRUE)+
    scale_shape_manual(values=c(21,22,22,21,21), labels=labels) +
    scale_fill_manual(values=c("black","red","green","blue","yellow"), labels=labels) +
    scale_size_manual(values=c(1,4,3,3,3), labels=labels)+
    labs(y = "Phenotype of parasite", x = "Number of generations") + # rename y-axis
    ggplot2::theme_bw() + mytheme

  }
