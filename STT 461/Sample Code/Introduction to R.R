# Introduction to R

# Variable assignment
x <- 2 #integer
y <- 2. #double
message <- "Hello World" # string
vect <- c(0,2,4,3) #vector
z <- seq(0,10) # vectorized sequence

# reading in data from csv file
pizza <- read.csv(file = "pizza.csv")

# examples with sample
sample(c(0,1),5, replace = TRUE) # 5 coin flips
sample(seq(1,6), 50, replace = TRUE) # 5 dice rolls
sample(c(0,1,2), 4) # oops

# examples with runif
runif(10) # 10 samples between 0 and 1
runif(5,1, 3) # 5 samples between 1 and 3
runif(10, 4, 2) # oops

# if-else syntax
if(x == 2){
  print("x = 2!")
} else {
  print("x != 2!")
}

# for loop syntax
for(i in 1:5){
  print(i)
}

# function syntax
product <- function(x,y){
  z = x*y
  return(z)
  }
