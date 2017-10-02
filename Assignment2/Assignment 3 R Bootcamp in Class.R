tt <- 0
N0 <- 100
RR <- 1.05
pop.size = numeric(11)

ttmax = 10
#set a variable instead of using a sequence of numbers for more flexibility

NN <-matrix(NA, nrow=1, ncol=ttmax+1)
NN[1] <- N0
#sets value at first point in vector

for(tt in 1:ttmax){ 
  NN[tt+1] = RR*NN[tt]
}

plot(1:(ttmax+1),NN, xlba="time",ylab="N",col='blue')

#########################################
#changed formula, initialized new variables 

tt <- 0
N0 <- 100
rr <- 1.05
pop.size = numeric(11)
KK = 400
ttmax = 10

NN <-matrix(NA, nrow=1, ncol=ttmax+1)
NN[1] <- N0
#sets value at first point in vector

for(tt in 1:ttmax) { 
  NN[tt+1] = NN[tt]*(1+rr*(1-(NN[tt]/KK)))
}
NN
#only use square bracket
#changed equation from above to new equation - once you have framework easy to adapt to do different stuff

plot(1:(ttmax+1),NN, xlab = "time",ylab="N", col='blue')

#to check if your y and x axis are the same length, type function "length(variable name)"

######################################
#assign output of function to a variable so that you can use it
#instead of initialzing each variable outside of function, include it so that you can assign numbers to variables in the function

geomFun <- function(RR, N0, ttMax){
  # Initialize vector to hold output - doing it inside function will allow you to assign variable start within the function
  NN <- rep(NA,ttMax+1)
  NN[1] <- N0
  # Use a for loop to step forward
  for (tt in 1:ttMax){
    NN[tt+1] <- RR*NN[tt]
  }
  plot(1:(ttMax+1),NN,lty=2, type='l', xlab='t', ylab='Population size')
  return(NN)
}

#####

geomFun <- function(RR=1.01, N0 = 200, ttMax =20){
  
  NN <- rep(NA,ttMax+1)
  NN[1] <- N0
   for (tt in 1:ttMax){
    NN[tt+1] <- RR*NN[tt]
  }
  plot(1:(ttMax+1),NN,lty=2, type='l', xlab='t', ylab='Population size')
  return(NN)
}
myOutput <- geomFun(RR=1.01, N0=200, ttMax=20)


####Convert your discrete logistic model into a function. 
#Use this function to explore the model's dynamics with ease and grace

LogisticsFun <- function(RR, N0, ttMax, KK){
  # Initialize vector to hold output - doing it inside function will allow you to assign variable start within the function
  NN <- rep(NA,ttMax+1)
  NN[1] <- N0
  # Use a for loop to step forward
  for (tt in 1:ttMax){
    NN[tt+1] = NN[tt]*(1+rr*(1-(NN[tt]/KK)))
  }
  plot(1:(ttMax+1),NN,lty=2, type='l', xlab='t', ylab='Population size')
  return(NN)
}
LogisticsFun(1,100,10,8000)
####_____________________________________________________________
####To make a plot of model summary output versus parameter value
## have to put function inside a for loop (can't put a sequence inside),

LogisticsFun <- function(RR, N0, ttMax, KK){
## Define parameter values
  
# for parameter of interest, make a vector of values you want to consider
  
}
