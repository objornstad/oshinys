NEW! I'm working to embed the Apps in standalone Rmarkdown documents posted in /markdown. Current Apps are

lotkavolterracompetition.rmd

lotkavolterrapredation.rmd

lpatribolium.rmd

parasitoidhost.rmd

rosenzweigmacarthur.rmd

rossmacdonald.rmd

seasonalseir.rmd

sir.rmd


______________________________________

The oshinys R-package contains shinyApps of a variety of ecological and epidemiological models. 

The package REQUIRES shiny,
    scatterplot3d,
    deSolve,
    phaseR and
    polspline R-packages from CRAN to run.
    
Thes source of the Apps are in /source/oshinys
________________________________________

IF you have devtools installed you can  build the package directly from the github source within R:


require('devtools')

   devtools::install_github('objornstad/oshinys/source/oshinys')

   install.packages(c("shiny", "deSolve", "scatterplot3d", "polspline", "phaseR"))


THEN launch any app from within R, for example:


require('oshinys')

   RM.app


IF you don't have devtools installed; see HOWTO
________________________________________

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

________________________________

All code was written by Ottar N. Bjornstad (onb1@psu.edu) and is licensed under the CC-BY-NC Creative Commons attribution-noncommercial license (http://creativecommons.org/licenses/by-nc/3.0/). Please share & remix non-commercially, mentioning its origin.

To change or modify any of the apps, edit the allapps3.R source-file in the "source/oshinys/R/directory" please update me with any improvements (as per the CC-BY-NC lisence).

[![CRAN Status](https://www.r-pkg.org/badges/version/oshinys)](https://cran.r-project.org/package=oshinys)
