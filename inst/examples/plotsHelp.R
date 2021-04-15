HostSwitch_simulated_quantities = simHostSwitch(K=1000,b=50, mig=0.001, sdm=0.2, sigma=1, pRes_min=1, pRes_max=10, n_generation=1000)


######### main plotting function #############



gg1 = plotHostSwitch(HostSwitch_simulated_quantities)
gg1

# If parasite population went extinct at an early stage it makes sense to zoom in using
#gg1 + xlim(0,100)




############ interaction plot ################

gg2 = plotHostSwitch2(HostSwitch_simulated_quantities)
gg2

