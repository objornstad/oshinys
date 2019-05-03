.onAttach <- function(libname, pkgname) {
packageStartupMessage("Current apps are:\n
lpatribolium.app - LPA Tribolium model\n
lotkavolterracompetition.app - Lotka-Volterra competion model\n
lotkavolterrapredation.app - Lotka-Volterra predation model\n
negbinparasitoid.app - May's Negative-Binomial parasitoid-host model\n
nicholsonbailey.app - Nicholson-Bailey Host-Parasitoid model\n
ricker.app - The Ricker ('discrete logisitic') mode\n
rosenzweigmacarthur.app - Rosenzweig-MacArthur Predator-Prey model\n
rossmacdonald.app - A simple Ross-Macdonald type malaria model\n
seir.app - the seasonally forced SEIR model\n
seirs.app - the unforced SEIRS model\n
sir.app - the unforced SIR model\n
tsir.app - the unforced TSIR model with demographic an environmental stochasticity\n
")
}