# HostSwitch
HostSwitch: An R Package to simulate host switching by a parasite


[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/HostSwitch)](https://cran.r-project.org/package=HostSwitch)
[![](http://cranlogs.r-pkg.org/badges/grand-total/HostSwitch?color=green)](https://cran.r-project.org/package=HostSwitch)
[![](http://cranlogs.r-pkg.org/badges/last-month/HostSwitch?color=green)](https://cran.r-project.org/package=HostSwitch)
[![](http://cranlogs.r-pkg.org/badges/last-week/HostSwitch?color=green)](https://cran.r-project.org/package=HostSwitch)

#### Trivellone, V. <a itemprop="sameAs" content="https://orcid.org/0000-0003-1415-4097" href="https://orcid.org/0000-0003-1415-4097" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a>; Araujo B.L., S. <a itemprop="sameAs" content="https://orcid.org/0000-0002-8759-8310" href="https://orcid.org/0000-0002-8759-8310" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a>; Panassiti B.  <a itemprop="sameAs" content="https://orcid.org/0000-0002-5899-4584" href="https://orcid.org/0000-0002-5899-4584" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a>



[R/HostSwitch](https://cran.r-project.org/web/packages/HostSwitch/index.html) is an [R](https://www.r-project.org) package that uses an individual-based model to simulate dispersion and host switch events by a consumer.The host-consumer association refers to any type of symbiotic (sensu lato) biological interaction.


#### Installation

Install R/HostSwitch from CRAN using:


```{r}
install.packages("HostSwitch")
```

or from GitHub using:


```{r}
devtools::install_github(repo <- "berndpanassiti/HostSwitch",build_vignettes = TRUE)
```


#### Example use

Try the following example to simulate dispersal and host switch events by a consumer. You can set up a maximum of 20 generations and 3 simulations
```{r}
library (HostSwitch)
simulated_quantities <- simHostSwitch(seed=123,n_sim=3,n_generation=20)
```

Also try `summaryHostSwitch` function to get summary statistics of simulated quantities of interest: optimum phenotypes that Consumers should have to be favored by the current Resource (pRes_sim), optimum phenotypes that Consumers should have to be favored by the novel Resource (pRes_new_sim), individual phenotype values of the Consumers (pInd), number of migrating individuals at each generation (pInd_jump_sim), individual phenotype values of the Consumers who disperse in a novel Resource (pInd_whichjump_sim), and individual phenotype values of the Consumers who successful colonize a novel Resource (pInd_whichsurv_sim).

```{r}
summaryHostSwitch(simulated_quantities)
```

Finally, try `plotHostSwitch` to plot of the simulated quantities and to select a specific simulation use the parameter "n_sim":
```{r}
gg1 <- plotHostSwitch(simulated_quantities, sim_n = 1)
gg1
```

You can compare two "simHostSwitch" objects using the function `testHostSwitch`. The comparison is between three estimated quantities: "j" total number of dispersing events; "s" total number of successful host switch events; "d" distance between the pRes_sim andpRes_new_sim for the generations where a successful host switch occurs, or phenotype distance.The The available tests are:”t” for t-test (parametric), and ”w” for Wilcoxon-test (non-parametric).For this compparison the number of simulation need to be greater than 1.
```{r}
m1 <- simHostSwitch(seed=123,n_sim=100,b=10) 
m2 <- simHostSwitch(seed=123,n_sim=100,b=15) 
testHostSwitch(simulated_quantities1=m1,simulated_quantities2=m2,parameter="j",test="t",plot=FALSE)
```

Using Shiny application for R, it is possible to run an interactive plot to simulate host switching on the web without detailed knowledge of the underlying code:
```{r}
shinyHostSwitch()
```

#### License

The R/HostSwitch package as a whole is distributed under GPL-3 (GNU General Public License version 3).

