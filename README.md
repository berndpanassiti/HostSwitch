[![License: AGPL v3](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)


# HostSwitch: Simulation of host switching by a parasite

The 'HostSwitch' package uses a simulation-based approach to investigate host switches by a parasite. The individual based model is based on the concepts of Stockholm paradigm and ecological fitting.
The orginial model is published by Araujo et al. 2015. Understanding Host-Switching by Ecological Fitting ([https://doi.org/10.1371/journal.pone.0139225](https://doi.org/10.1371/journal.pone.0139225)). The package provides an R-friendly and modified version of this model which can be applied to different host-parasite scenarios.

## Getting HostSwitch

### Development release 

If you want to install the current (development) version from this repository, run

```{r}
devtools::install_github(repo = "berndpanassiti/HostSwitch", dependencies = T, build_vignettes = T)
```

To get an overview about its functionality once the package is installed, run

```{r}
library(HostSwitch)
?simHostSwitch
vignette("HostSwitch", package="HostSwitch")
```
The vignette provides an examples about how to use the package function. To cite the package, run 

```{r}
citation("HostSwitch")
```

