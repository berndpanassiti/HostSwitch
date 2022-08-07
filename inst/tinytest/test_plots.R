# ==================================================================== #
# TITLE                                                                #
# HostSwitch: An R Package to Simulate the Extent of Host-Switching    #
# by a Consumer                                                        #
#                                                                      #
# SOURCE                                                               #
# https://reptalus.github.io/HostSwitch/                               #
# https://CRAN.R-project.org/package=HostSwitch                        #
# https://github.com/berndpanassiti/HostSwitch                         #
#                                                                      #
# LICENCE                                                              #
# (c) 2020-2022 Trivellone, V., Araujo, S.B.L., Panassiti, B.          #
# Developed with passion and dedication.                               #
#                                                                      #
# This R package is free software; you can freely use and distribute   #
# it for both personal and commercial purposes under the terms of the  #
# GNU General Public License version GPL (≥ 3) , as published by       #
# the Free Software Foundation.                                        #
# We created this package for both academic research and teaching.     #
# The package provides functions to investigate host switches by       #
# consumers. It was publicly released in the hope that it will be      #
# useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
#                                                                      #
# Visit our website for introduction on theoretical basis of host      #
# switches, ongoing-developments and updates of the package:           #
# https://reptalus.github.io/HostSwitch/                               #
# ==================================================================== #



# Tests for fct plotHostSwitch
## Check for error messages
### Specified simulation number must be present in Host simulation
m1 = simHostSwitch(n_sim = 10)
expect_error(plotHostSwitch(m1,sim_n=11))
expect_silent(plotHostSwitch(m1,sim_n=10))
