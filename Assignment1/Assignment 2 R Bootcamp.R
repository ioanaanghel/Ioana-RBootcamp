###1
## incorrect 1st try 
#get_heights <- rnorm(100,mean=69,sd=10)
#heights <- c(get_heights)
#head(heights)

#correct
get_heights = function(n){
  heights <- rnorm(n,mean=69,sd=10)
}


###2
get_heights = function(n){
  heights <- rnorm(n,mean=69,sd=10)
  mean(heights)
}


###3
get_heights = function(n){
  heights <- rnorm(n,mean=69,sd=10)
  return(mean(heights))
}


###4.Use a "for" loop to call your "get_heights" function 1000 times,
#with taking a sample of size 100 from the population. Save the mean height 
#from each replicate in a vector called "mean_heights_100".

mean_heights_100 <- rep(NA,1000)

for(xx in 1:1000){
  mean_heights_100[xx] <- get_heights(100)
}
head(mean_heights_100)


###5. Use a "for" loop to call your "get_heights" function 1000 times,
#with taking a sample of size 1000 from the population. 
#Save the mean height from each replicate in a vector called
#"mean_heights_1000".
  
##incorrrect 
#xx<- 1
#for(xx in 1:1000){
#  get_heights_1000<- rnorm(1000,mean=69,sd=10) }
# mean_heights_1000 <- mean(get_heights_1000)

## correct

mean_heights_1000 <- rep(NA,1000)

for(xx in 1:1000){
  mean_heights_1000[xx] <- get_heights(1000)
}
head(mean_heights_1000)

###6. Plot a histogram of the distribution of the average heights for 
#your sample size of 100 and 1000 individuals. The two sets of data 
#should be plotted on the same axes. Add a legend. Label the axes. 
#Plot the data from the 100 samples in red and the data from the 
#1000 samples in blue. Your plot should look something like the one 
#shown on the next page. 
setwd("C:/Users/ioana/Documents/GitHub/RBootcamp")

bins<-seq(65,73,by=0.5)
hist(mean_heights_100,breaks=bins)$breaks
hist(mean_heights_1000,breaks=bins)$breaks

counts_mean_heights_1\00 <- hist(mean_heights_100,breaks=bins)$counts
counts_mean_heights_1000 <- hist(mean_heights_1000,breaks=bins)$counts

pdf(file="normal_barplot.pdf", width=5,height=5); #open the file
#remove line that sets plotting area and margins if the margins are giving errors "par(mfrow=c(1,1), mar=c(4, 4, 3, 2))"
barplot(rbind(counts_mean_heights_100,counts_mean_heights_1000),col=c(2,4),
        beside=T, names.arg=seq(65,72.75,by=0.5), xlab="Average height (inches)",ylab="Count")

legend(5,400,c(expression(paste(n,"=100")),expression(paste(n,"=1000"))),col=c(2,4), pch =19)
dev.off()
