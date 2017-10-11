#Bootcamp modeling exercises
#Assignment 2 - Ioana Anghel
#Ricker Model
###(a)

tt <- 0
N0 <- 100
rr <- 1.05
pop.size = numeric(11)
KK = 300
ttmax = 10

NN <-matrix(NA, nrow=1, ncol=ttmax+1)
NN[1] <- N0
#sets value at first point in vector

RickerFun <- function(rr,N0,KK,ttMax,PLOTFLAG=0){
  NN <- rep(NA,ttMax+1)
  NN[1] <- N0
  
  for(tt in 1:ttMax) { 
    NN[tt+1] = NN[tt]*exp(rr*(1-(NN[tt]/KK)))
  }
  
  if (PLOTFLAG==1){
    plot(1:(ttMax+1),NN, xlab = "Time",ylab="Population size", col='blue',type='l')
  }
  return(NN)
}

#to supress plot from generating whenever running the function RickerFun, 
# PLOTFLAG=number other than the default, which in this case is 1
# ex: RickerFun(1.05,100,400,50,0)

#########################################################################################
#(b)
## (rr,N0,KK,ttMax,PLOTFLAG=1)
#Population decreases to n = 0. 
# to reach zero population must have negative growth (decline)

RickerFun(-0.25,10,400,50)

#Population approaches stable equilibrium at n??? = K, without oscillations

RickerFun(0.15,10,400,50)

#Decaying oscillations around n??? = K.

RickerFun(1.5,100,400,50)

#Persistent, regular oscillations.

RickerFun(2.3,100,400,50)

#Crazy, random-looking ???uctuations (chaos).
# growth rate is so high that boom and bust cycles are not consistent
RickerFun(3,100,400,50)

#growth rate is the key driver of these patterns

#########################################################################################
#(c)

#run for loop with 6 different values 
# tt for time
# iterating over

#par(mfrow=c(3,2)) plots multiple plots on one page

par(mfrow=c(3,2))

rr <- c(-0.25,0.15,1,1.5,2.3,3)
for (ii in 1:length(rr)){
  RickerFun(rr[ii],100,400,50)
}


#########################################################################################
#(d)
##(rr,N0,KK,ttMax,PLOTFLAG=1)
nvec <- RickerFun(1.05,20,1000,50,0)
which(nvec >= 500)[1]

#when calling a specific position, put square brackets with position number after the vector

#########################################################################################
#(e)

KK <- 1000
N0 <- 100
ttMax <- 50
aa <- 0.1
rr <- seq(0.1,0.9,aa)

duration <- seq(0,ttMax,1)
TimeToHalfK <- vector('numeric',length(rr))
  
#vector of half time to carrying capacity for each of the growth rates in "duration"

for (jj in 1:length(rr)){
  nvec <- RickerFun(rr[jj],N0,KK,ttMax)
   vec <- which(nvec >= KK/2)[1]
   TimeToHalfK[jj] <- duration[vec]
}
par(mfrow=c(1,1))

plot(rr,TimeToHalfK, xlab = "r",ylab="Time to Half K", col='blue',type='l')

#########################################################################################
#(f)

#
#initialize inputs for new vectors KK & rr; also initalize N0 & ttMax
#   create blank vector for population number outputs with length equal to number of combinations of KK x rr
#   initalize Ricker function at 0, so that you can increment it inside of the loop
#output a matrix of the population at each combination of KK & rr
#   write a nested for loop to run through all of the possibilies of rr, with all of the possibilities of KK
#   inside of the nested loop increment Ricker function
#   run RickerFun function with the variables at rr[jj] and KK[ii], returns NN 
#   place outputs of RickerFun into the popMatrix created earlier


#########################################################################################
#(g)

rr <- c(0.5,1.0,1.5)
KK <- c(100,200,300)
N0 <- 2
ttMax <- 10 

#nested loop
#run Ricker last because NN will be the output 
#cycle to all other functions first

popMatrix <- matrix(nrow = length(KK),ncol = length(rr))

for(ii in 1:length(KK)){
  for(jj in 1:length(rr)){
    nvec <- RickerFun(rr[jj],N0,KK[ii],ttMax)
    popMatrix[ii,jj] <- nvec[ttMax]
    #popMatrix[Rickers] = RickerFun(rr[jj],N0,KK[ii],ttMax)
#how to have RickerFun output save into popMatrix, and increment popMatrix index? 
   # popMatrix[Rickers] = RickerFun()
  }
}
#outputs the matrix
popMatrix

# check thar Ricker fun is running correctly
RickerFun(rr[3],N0,KK[2],10)

contour(x = seq(min(rr), max(rr), length.out = nrow(popMatrix)),
        y = seq(min(KK), max(KK), length.out = ncol(popMatrix)),
        z = popMatrix)
#OR
library("lattice")
levelplot(popMatrix)

