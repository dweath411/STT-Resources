
d <- read.csv("Data/LGPIFDataSimple.csv")
d[1:10,]

plot(d[1:5,]$Year, log(d[1:5,]$Claims+1), 
     col="red", pch=1, cex=2, lwd=2, ylim=c(-1,20),
     xlab="Year", ylab="Log Claims",
     main="Log Claims")
points(d[6:10,]$Year, log(d[6:10,]$Claims+1), col="blue", 
       pch=4, cex=2, lwd=2)
legend("topleft", 
       c("Your Car", "Friend's Car"),
       col=c("red","blue"), pch=c(1,4), cex=1)
