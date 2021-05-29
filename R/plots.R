#' HostSwitch standard plot
#'
#' This function creates a standard plot for the simulated host switches
#'
#' @param HostSwitch_simulated_quantities The object HostSwitch created by \code{\link{simHostSwitch}}
#' @param n_sim for HostSwitch object including more than 1 simulation saved, "n_sim" specifies which simulation have to be plotted. The plot from the first simulation is shown as default (n_sim = 1). If the called simulation number is not present in the HostSwitch object, the error message "Error in FUN(X[[i]], ...) : subscript out of bounds" will be returned.
#'
#' @details The function plots dispersal and colonization (host-switching events) of Consumers on a novel host offered at each generation given the values of parameters related to carrying capacity, fitness space, migration, reproduction, selection, and biological model. The X-axis shows the total number of possible generations defined in the object HostSwitch, the Y-axis the phenotype values of the Consumer.
#'
#' Black dots are the phenotype values of the Consumer after each event of reproduction. The green squares represent the value of phenotype favored by the novel Resource offered at each generation. The red squares are the phenotype values of the Consumer favored by the current Resource.The blue dots represents the phenotypes of dispersing Consumers, and the yellow dots the successfully colonizing Consumers.
#'
#' @import ggplot2
#' @examples
#' m1 = simHostSwitch(n_sim=100) # create an HostSwitch object with 100 simulations.
#'
#' ## The results of the first simulation (n_sim=1) are plotted as default
#' plotHostSwitch(m1)
#'
#' ## The 50th simulation of the model is plotted
#' plotHostSwitch(m1,n_sim=50)
#'
#' @export
plotHostSwitch <- function(HostSwitch_simulated_quantities,n_sim=1){

  n_generations <- pRes_max <- pRes_min <- p <- x <- y <- NULL # global variables

  mytheme <-  ggplot2::theme(axis.title   = ggplot2::element_text(size =24),
                             axis.text    = ggplot2::element_text(size =25),
                             strip.text   = ggplot2::element_text(size = 30),
                             plot.title   = ggplot2::element_text(hjust = 0.5),
                             legend.title = ggplot2::element_blank(),
                             legend.text  = ggplot2::element_text(size=12))

  dat= HostSwitch_simulated_quantities[c("pRes_sim","pRes_new_sim","pInd_sim","pInd_whichjump_sim","pInd_whichsurv_sim")]
  dat = sapply(dat, "[[", n_sim)
  pRes_sim     = data.frame(p=rep("pRes",length(dat$pRes_sim)), y=dat$pRes_sim,x=0:(length(dat$pRes_sim)-1))
  pRes_new_sim = data.frame(p=rep("pRes_new",length(dat$pRes_new_sim)),y=dat$pRes_new_sim,x=0:(length(dat$pRes_new_sim)-1))

  pInd_sim_df = createPlotInput_Ind_sim(dat$pInd_sim)
  pInd_sim = data.frame(cbind(p=rep("pInd",nrow(pInd_sim_df)),pInd_sim_df))

  whichJump_sim_df = createPlotInput_Ind_sim(dat$pInd_whichjump_sim)
  whichJump_sim = data.frame(cbind(p=rep("whichJump",nrow(whichJump_sim_df)),whichJump_sim_df))

  whichSurv_sim_df = createPlotInput_Ind_sim(dat$pInd_whichsurv_sim)
  whichSurv_sim = data.frame(cbind(p=rep("whichSurv",nrow(whichSurv_sim_df)),whichSurv_sim_df))

  plotInput = data.frame(rbind(pInd_sim,pRes_sim,pRes_new_sim,whichJump_sim,whichSurv_sim))

  n_generations = HostSwitch_simulated_quantities$n_generations
  pRes_min = HostSwitch_simulated_quantities$pRes_min; pRes_max = HostSwitch_simulated_quantities$pRes_max
  #ranges$x = c(0, n_generations); ranges$y = c(pRes_min,pRes_max)

  labels=c("Consumer phenotpye","Consumer phenotype favored by current resource","Consumer phenotype favored by new resource", "Phenotype of dispersing consumer", "Phenotype of successfully colonizing consumer")

  ggplot2::ggplot(plotInput,aes(x = x, y = y, group = p)) +
    #xlim(ranges$x) + ylim(ranges$y)+
    xlim(0, n_generations) + ylim(pRes_min,pRes_max)+
    geom_point(aes(fill = p, shape =p, size=p), na.rm=TRUE)+
    scale_shape_manual(values=c(21,22,22,21,21), labels=labels) +
    scale_fill_manual(values=c("black","red","green","blue","yellow"), labels=labels) +
    scale_size_manual(values=c(1,4,3,3,2), labels=labels)+
    labs(y = "Phenotype of consumer", x = "Number of generations") + # rename y-axis
    ggplot2::theme_bw() + mytheme

  }
