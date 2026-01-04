
d <- read.csv("Data/SwedishMotorInsurance.csv")
head(d)

install.packages("gamlss")
library(gamlss)
dsub <- subset(d, Claims>0)
dsub$Avg <- dsub$Payment / dsub$Claims
con <- gamlss.control(n.cyc=500, mu.step=0.1, sigma.step=0.1)
model <- gamlss(Claims ~ log(Insured),
       sigma.formula = ~ 1,
       family=NBI, data=d, con)
plot(log(predict(model,type="response")+1), log(d$Claims+1))
abline(0,1,col="red")

