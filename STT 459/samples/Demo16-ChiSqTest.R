
d <- read.csv("Data/SwedishMotorInsurance.csv")

head(d)

samp <- 1:floor(nrow(d)*0.8)
train <- d[samp,]
test <- d[-samp,]

model1 <- glm(Claims ~ log(Insured), data=train, family=poisson(link="log"))
summary(model1)

model2 <- glm(Claims ~ log(Insured) + factor(Zone) +
                factor(Bonus) + factor(Make), data=train, family=poisson(link="log"))
summary(model2)

# Chi-Square Test
test$Cat <- cut(test$Claims, breaks=c(-Inf,1,3,5,7,9,Inf))
test$Cat1 <- cut(predict(model1, test, "response"), breaks=c(-Inf,1,3,5,7,9,Inf))
test$Cat2 <- cut(predict(model2, test, "response"), breaks=c(-Inf,1,3,5,7,9,Inf))

table(test$Cat)
table(test$Cat1)
table(test$Cat2)

sum((table(test$Cat) - table(test$Cat1))^2 / table(test$Cat1)) # 72.01883
sum((table(test$Cat) - table(test$Cat2))^2 / table(test$Cat2)) # 40.63404

library(stats)

help(chisq.test)

probs1 <- as.numeric(table(test$Cat1)) / sum(as.numeric(table(test$Cat1)))
probs2 <- as.numeric(table(test$Cat2)) / sum(as.numeric(table(test$Cat2)))
chisq.test(as.numeric(table(test$Cat)), p=probs1) # 72.019
chisq.test(as.numeric(table(test$Cat)), p=probs2) # 40.634
