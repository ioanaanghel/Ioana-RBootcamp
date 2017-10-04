#Additional in class excercises for R bootcamp:
#EEB 201, September 27, 2017
#Kirk Lohmueller


#1a.  To start let's revisit our tests of Hardy-Weinberg. 
#   Go back and perform the chi-square test for Hardy-Weinberg 
#   that we did in class on all SNPs in the "hapmap_CEU_r23a_chr2_ld.txt" file.
#   Hint: you already have the code for this. 
#   Save your P-values in a vector called "pvals"


setwd("~/GitHub/RBootcamp/Assignment2")
snpsDataFrame <- read.table('hapmap_CEU_r23a_chr2_Id.txt',header=TRUE)

dim(snpsDataFrame)
head(snpsDataFrame)

names(snpsDataFrame)
row.names(snpsDataFrame)

# Because the data are really just a large numeric matrix, we convert the dataframe to a matrix:
snps=as.matrix(snpsDataFrame)

# With row names we can easily extract certain SNPs using the id's
testSNP=snps["rs218206_G",]

table(testSNP)

# What is proportion of heterozygotes at this locus?
het=sum(testSNP==1)/length(testSNP)

# What if there is missing data?
testSNP=snps["rs6717613_A",]

# Try these commands
table(testSNP)
testSNP==1
length(testSNP)
is.na(testSNP)

# Now let's compute the observed heterozygosity
het=sum(testSNP==1,na.rm=TRUE)/sum(!is.na(testSNP))  # but this doesn't, because 

# What is the frequency of the minor allele?
freq=sum(testSNP,na.rm=TRUE)/(2.0*sum(!is.na(testSNP)))

# Now, let's define functions that do this for a generic set of SNP data
calc_freq=function(x){
  return(sum(x,na.rm=TRUE)/(2.0*sum(!is.na(x))))
}

calc_het=function(x){
  return(sum(x==1,na.rm=TRUE)/(sum(!is.na(x))))
}

# And now let's apply the functions to each and every SNP
freq=apply(snps,1,calc_freq)
het=apply(snps,1,calc_het)

# And now we can make exploratory plots
plot(freq,het,xlab="Frequency",ylab="Heterozygosity")  # Scatter plot

# Let's add a line to show what relationship we'd expect under Hardy-Weinberg expectations
p=seq(0,0.5,by=0.05)   # Set-up a vector with a sequence of allele frequencies
points(p,2*p*(1-p),type="l",col=2) # Plot the HW expectation as a line in red

## APPYLING A CHI-SQUARE TEST TO EACH SNP TO FORMALLY LOOK FOR DEPARTURES FROM HARDY-WEINBERG EXPECTATIONS ###

compute_chisquare=function(x){
  freq=sum(x,na.rm=TRUE)/(2.0*sum(!is.na(x)))
  cnt0=sum(x==0,na.rm=TRUE)
  cnt1=sum(x==1,na.rm=TRUE)
  cnt2=sum(x==2,na.rm=TRUE)
  obscnts=c(cnt0,cnt1,cnt2)
  #print(obscnts)
  n=sum(obscnts)
  expcnts=c((1-freq)^2,2*freq*(1-freq),freq^2)*n
  chisq=sum((obscnts-expcnts)^2/expcnts)
  return(chisq)
}

#let's make a second funtion that makes use of R's built in chisq.test function

compute_chisquare_2=function(x){
  freq=sum(x,na.rm=TRUE)/(2.0*sum(!is.na(x)))
  cnt0=sum(x==0,na.rm=TRUE)
  cnt1=sum(x==1,na.rm=TRUE)
  cnt2=sum(x==2,na.rm=TRUE)
  obscnts=c(cnt0,cnt1,cnt2)
  #print(obscnts)
  n=sum(obscnts)
  #here we use the built-in function for the chi-sq distribution:
  exp_probs=c((1-freq)^2,2*freq*(1-freq),freq^2) #note, here we don't multiply by n
  chisq<-chisq.test(obscnts,p=exp_probs, correct = FALSE)$statistic
  return(chisq)
}

# Apply the compute_chi_square function to each snp
chisqs=apply(snps,1,compute_chisquare)
chisqs2=apply(snps,1,compute_chisquare_2)

#check to see that the chisquare statistcs are the same:
#first do this by computing Pearson's correlation coefficient:
cor.test(chisqs,chisqs2)

#we can also do a quick scatterplot:
plot(chisqs,chisqs2)

# Compute p-values for each chi-square value using the pchisq function
pvals=pchisq(chisqs,1,lower.tail=FALSE)

#######################################################################################################
#1b.  What proportion of P-values from the test (put the vector called "pvals") 
#   are <0.05?
  
signifthres05<-0.05
# sum of a conditional statement only adds up the binary variables that are true per the condition
proportion05=(sum(pvals<signifthres05))/length(pvals)
proportion05
# OR
mean(pvals<signifthres05)
# mean of a conditional statement only adds a binary value of the items in the vector that meet the condition, 
# so it is essentially taking a proportion

# What proportion are <0.01? 

signifthres01<-0.01
proportion01=(sum(pvals<signifthres01))/length(pvals)
proportion01
#OR
mean(pvals<signifthres01)

# Are any <0.001? 

signifthres001<-0.001
proportion001=(sum(pvals<signifthres001))/length(pvals)
proportion001
#OR
mean(pvals<signifthres001)

#######################################################################################################
#1c.  How many SNPs were tested for departures from Hardy-Weinberg equilibrium?
#   Hint: How many P-values do you have? Second hint: Try using the "length" function. 
#   Save this value in the variable called "num_pval".

num_pval<-length(pvals)

#######################################################################################################
#1d.  Say that you have "num_pval" total P-values. Assuming that the null hypothesis is true
#   (i.e. all SNPs are in Hardy-Weinberg), the smallest P-values is expected to be 1/num_pval. 
#   The second smallest P-value is expected to be 2/num_pval. The third smallest P-value is expected 
#   to be 3/num_pval, etc. The largest P-value is expected to be num_pval/num_pval (or 1). 
#   Calculate the vector of expected P-values for the  chi-square test. Save these in the vector called "exp_pvals". 

exp_pvals<-seq(1,num_pval,1)/num_pval
# create a sequence for the denominators, then divide each of them by the denominator - outside of the seq function
head(exp_pvals)
tail(exp_pvals)

#######################################################################################################
#1e. The observed P-values in the "pvals" vector are in the order that they SNPs appear across the chromosome. 
#   We need to sort them, smallest to largest. Use the "sort" function to sort the P-values. 
#   Store them in the vector "sort_pvals".  

sort_pvals <- sort(pvals)
head(sort_pvals)
tail(sort_pvals)

#######################################################################################################
#1f.  In order to see what is happening with the small P-values (these are the ones we really care about), 
#   people often take the -log10(Pvalue). Find the -log10 of the observed and expected P-values. 
#   Store these in the vector "log_sort_pvals" and "log_exp_pvals". 

log_sort_pvals <- -log10(exp_pvals)
log_exp_pvals <- -log10(sort_pvals)

#######################################################################################################
#1g.  You're ready to make the QQ plot! Plot the "log_sort_pvals" vs. the "log_exp_pvals". 

plot(log_sort_pvals, log_exp_pvals)

#######################################################################################################
#1h.  Where should these P-values fall under the null hypothesis? They should fall along the diagonal. 
#   Add a diagonal line to the QQ plot.

abline(a=0,b=1,lty=5, lwd =3, col=2)
# a is Y intercept, b is slope, h = horizonatal line y value

#######################################################################################################
#2  Researchers are very interested in testing whether certain alleles are present in higher frequency 
#     in individuals with traits, such as type 2 diabetes. We have blood glucose levels for the 
#     60 individuals in this study. 

#2a. Load the file "pheno.sim.2014.txt". Store the phenotypes in a data frame called "zz". 
#   The second column in this file contains the blood glucose measurements. 
#     Hint: you probably want to use "header=T" in the "read.table" command. 

zz=read.table("pheno.sim.2014.txt",header=TRUE)
#   colnames(zz) = tells me the column names in the table
#   colnames(zz)[2] = tells me the name of the second column

#######################################################################################################
#2b.  Find the value of the phenotype such that 25% of the individuals have a phenotype LESS than this value.

# mean(zz[,"glucose_mmolperL"])  this outputs the mean in matriz zz, all rows, column "glucose_mmolperL"
#
# Quantile by hand - h<- sort(zz[,"glucose_mmolperL"])
#     THEN (h[15]+h[16])/2
#     value will be approximate but not equal to quantile because quantile is at exactly 25% on the normal distribution
quantile(zz[,2])
#outputs all the of the quantile data using the second column from data set zz
quantile(zz[,2])[2]
#outputs the 2th output from the quantile matrix

#######################################################################################################
#2c. Find the value of the phenotype such that 25% of the individuals have a phenotype GREATER than this 
#     value (i.e. 75% of the individuals have a phenotype LESS than this value).

quantile(zz[,2])[4]
#outputs the 4th output from the quantile matrix

#######################################################################################################
#2d. Make a density plot of the distribution of phenotypes (i.e. the blood glucose levels). Add vertical
#     lines to the plot to denote the 25% and 75% tails of the distribution.


phen=zz$glucose_mmolperL
hist(phen)
abline(h=quantile(zz[,2])[4],lwd =1, col=3)
abline(h=quantile(zz[,2])[2],lwd =1, col=3)


