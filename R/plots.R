#' HostSwitch standard plot
#'
#' This function creates a standard plot for the simulated host switches
#'
#' @param x an object of created by \code{\link{simHostSwitch}}
#'
#' @details The function creates a plot with host switches by a parasite. X axis shows the number of generations, y axis the considered phenotpye range.
#'
#' Red squares show the phenotype of the resource (original host), green squares the phenotype of the new host and black dots individual parasites.
#'
#' @import ggplot2
#' @import tidyverse
#' @import tibble
#' @import dplyr
#' @export
plotHostSwitch <- function(x){

    mytheme <-  theme(axis.title = element_text(size =24),
                      axis.text = element_text(size =25),
                      strip.text= element_text(size = 30),
                      legend.position = "none",
                      plot.title = element_text(hjust = 0.5))

    # prepare data
    dat=HostSwitch_simulated_quantities
    pRes_sim     = data.frame(x=0:(length(dat$metadata[[1]])-1),y=dat$metadata[[1]])
    pRes_new_sim = data.frame(x=1:(length(dat$metadata[[2]])),y=dat$metadata[[2]])

    # simulated individuals
    df1 = flatten2(dat$metadata[[3]])
    df2 = data.frame(sapply(df1, "length<-", max(lengths(df1))))
    colnames(df2)        = 0:(dim(df2)[2]-1)
    pInd_sim             = stack(df2)
    colnames(pInd_sim)   = c("y","x")
    pInd_sim             = pInd_sim[complete.cases(pInd_sim),] # remove NA
    pInd_sim$x           = as.numeric(as.character(pInd_sim$x))

    n_generations = dat$metadata[[4]]
    pRes_min = dat$metadata[[5]]; pRes_max = dat$metadata[[6]]



    ggplot2::ggplot(data=pRes_sim,aes(x,y)) + geom_point(fill = 'red',shape=22,size=3) +               # plot resourcce
      xlim(0, n_generations) + ylim(pRes_min,pRes_max)+
      geom_point(data = pRes_new_sim, aes(x,y), col = 'green',size=2) +  # plot new resource
      geom_point(data =  pInd_sim, aes(x,y))+
      labs(y = "Phenotype", x = "Number of generations") +# rename y-axis
      theme_bw() + mytheme
  }

