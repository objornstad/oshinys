
The oshinys R-package contains shinyApps of a variety of ecological and epidemiological models. 

The package REQUIRES shiny,
    scatterplot3d,
    deSolve,
    phaseR and
    polspline R-packages from CRAN to run.
    
The source of the Apps are in /source/R/allaps3.r
________________________________________

Easisest way to install is from within R do:

install.packages(c("shiny", "deSolve", "scatterplot3d", "polspline", "phaseR", devtools))

require('devtools')

devtools::install_github('objornstad/oshinys/source/')

THEN launch any app from within R, for example:

require('oshinys')

runApp(rosenzweigmacarthur.app)


IF you can't install devtools; see HOWTO for "manual" installation
________________________________________

The current models are:

lpatribolium.app - LPA Tribolium model with a 2D and 3D phase plane

lotkavolterracompetition.app - Lotka-Volterra competion model with phase plane and isoclines

lotkavolterrapredation.app - Lotka-Volterra predation model with phase plane and isoclines

negbinparasitoid.app - May's Negative-Binomial parasitoid-host model

nicholsonbailey.app - Nicholson-Bailey Host-Parasitoid model

ricker.app - The Ricker ("discrete logistic") model

rosenzweigmacarthur.app - Rosenzweig-MacArthur Predator-Prey model with phase plane and isoclines

rossmacdonald.App - A simple Ross-Macdonald type malaria model

seir.app - the seasonally forced SEIR model in time and in the phase plane

seirs.app - the unforced SEIRS model in time and in the phase plane with ressonant periodicity calculations

sir.app - the unforced SIR model in time and in the phase plane with R0 calculations

tsir.app - the unforced TSIR model with demographic an environmental stochasticity in time and in 
the phase plane and with simulated and transfer function derived periodograms

______________________________________

NEW! I'm working to embed the Apps in standalone Rmarkdown documents. See

https://github.com/objornstad/ecomodelmarkdowns

All these can be launced without compilationn from Rstudio with "> Run Document".
________________________________

All code was written by Ottar N. Bjornstad (onb1@psu.edu) and is licensed under the CC-BY-NC Creative Commons attribution-noncommercial license (http://creativecommons.org/licenses/by-nc/3.0/). Please share & remix non-commercially, mentioning its origin.

When change, modify and improve any of the apps, please update me with any improvements (as per the CC-BY-NC lisence).

[![CRAN Status](https://www.r-pkg.org/badges/version/oshinys)](https://cran.r-project.org/package=oshinys)
