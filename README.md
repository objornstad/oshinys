The oshinys R-package contains shinyApps of a variety of ecological and epidemiological models. 

The pakage REQUIRES 

shiny,
    scatterplot3d,
    deSolve,
    phaseR,
    polspline

from CRAN to run.

The current models are:

LPA.app - LPA Tribolium model

LVcomp.app - Lotka-Volterra competion model

LVpred.app - Lotka-Volterra predation model

NB.app - Nicholson-Bailey Host-Parasitoid model

May.app - May et al's Negative-Binomial parasitoid-host model

Ricker.app - The Ricker ("discrete logistic") model

RM.app - Rosenzweig-MacArthur Predator-Prey model

SEIR.app - the seasonally forced SEIR model

SEIRS.app - the unforced SEIRS model

SIR.app - the unforced SIR model

TSIR.app - the unforced TSIR model with demographic an environmental stochasticity


Tar-ball is Mac/Linux source, zip-file is widows. 

To INSTALL download to local disk and do (depending on OS)

> install.packages("/yourpath/oshinys_0.1-1.tar.gz", repos = NULL, type = "source")

or

> install.packages("~/yourpath/oshinys_0.1-1.zip", repos = NULL, type = "source")

Then install required packages from CRAN:

> install.packages("shiny")

> install.packages("deSolve")

> install.packages("scatterplot3d")

> install.packages("polspline")

> install.packages("phaseR")


All code was written by Ottar N. Bjornstad (onb1@psu.edu) and is licensed under the CC-BY-NC Creative Commons attribution-noncommercial license (http://creativecommons.org/licenses/by-nc/3.0/). Please share & remix non-commercially, mentioning its origin.

To change or modify any of the apps edit the allapps3.R source-file in the source/oshinys/R/ directory
