smi <- read.csv("SwedishMotorInsurance.csv")

smi_sub <- subset(smi, Claims > 0)

lik <- function(param){
  liklihood <- dexp(smi_sub$Payment/smi_sub$Claims, rate = 1/param, log = T)
return(-sum(liklihood))
}

op <- nlminb(1,lik)
op$par
#5206.054
library(numDeriv)

Info <- hessian(lik, op$par)

InfoInv <- solve(Info)

diagElm <- diag(InfoInv)

StdErr <- sqrt(diagElm)

StdErr
#122.8102
c( op$par[1] - StdErr[1] * 1.96,
   op$par[1] + StdErr[1] * 1.96)
#(4965.346, 5446.762)