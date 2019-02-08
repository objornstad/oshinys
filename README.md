The oshinys R-package contains shinyApps of a variety of ecological and epidemiological models. 

The package REQUIRES shiny,
    scatterplot3d,
    deSolve,
    phaseR and
    polspline R-packages from CRAN to run.

The current models are:

LPA.app - LPA Tribolium model with a 2D and 3D phase plane

LVcomp.app - Lotka-Volterra competion model with phase plane and isoclines

LVpred.app - Lotka-Volterra predation model with phase plane and isoclines

NB.app - Nicholson-Bailey Host-Parasitoid model

May.app - May et al's Negative-Binomial parasitoid-host model

Ricker.app - The Ricker ("discrete logistic") model

RM.app - Rosenzweig-MacArthur Predator-Prey model with phase plane and isoclines

SEIR.app - the seasonally forced SEIR model in time and in the phase plane

SEIRS.app - the unforced SEIRS model in time and in the phase plane with ressonant periodicity calculations

SIR.app - the unforced SIR model in time and in the phase plane with R0 calculations

TSIR.app - the unforced TSIR model with demographic an environmental stochasticity in time and in 
the phase plane and with simulated and transfer function derived periodograms


The HOWTO details how to get the package installed and any of the shinyApp's up and running


All code was written by Ottar N. Bjornstad (onb1@psu.edu) and is licensed under the CC-BY-NC Creative Commons attribution-noncommercial license (http://creativecommons.org/licenses/by-nc/3.0/). Please share & remix non-commercially, mentioning its origin.

To change or modify any of the apps, edit the allapps3.R source-file in the "source/oshinys/R/directory" please update me with any improvements (as per the CC-BY-NC lisence).

[![CRAN Status](https://www.r-pkg.org/badges/version/oshinys)](https://cran.r-project.org/package=oshinys)
