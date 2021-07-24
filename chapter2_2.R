 #### CASE PROBLEM 1   PAGE 82
 #### Example: Meteorites
# enter data as a vector
toco <- c(5.6, 2.7, 6.2, 2.9, 1.5, 4.0, 4.3, 3.0, 3.6, 2.4, 6.7, 3.8)

op <- par(mar = rep(0, 4))   
plot.new( )
par(mfrow=c(2,1))
#Histogram overlaid with kernel density curve
hist(toco, freq = FALSE, breaks = 6)
points(density(toco), type = "l")
rug(toco)

#violin plot
library(vioplot)
vioplot(toco, horizontal=TRUE, col="gray")

#stem-and-leaf plot
stem(toco, scale = 2)

#t.crit
qt(1 - 0.05/2, df = length(toco) - 1)
t.summary<-t.test(toco, mu = 0.54)
t.summary
summary(toco)
t.dist.pval(t.summary)
#install.packages("bs.one.samp.dist")
bs.one.samp.dist(toco)

####  PAGE 70-71
#### Visual comparison of whether sampling distribution is close to Normal via Bootstrap
# a function to compare the bootstrap sampling distribution with
# a normal distribution with mean and SEM estimated from the data
bs.one.samp.dist <- function(dat,N = 1e4) {
  n <- length(dat) ;
  # resample from data
  sam <- matrix(sample(dat, size = N* n, replace = TRUE), ncol = N);
  # draw a histogram of the means
  sam.mean <- colMeans(sam) ;
  #save par( ) settings
  old.par <-par(no.readonly = TRUE)
  #make smaller margins
  par(mfrow=c(2,1), mar=c(3,2,2,1), oma=c(1,1,1,1))
  # Histogram overlaid with kernel density curve
  hist(dat, freq = FALSE, breaks = 6
       , main = "Plot of data with smoothed density curve")
  points (density(dat), type = "l")
  rug(dat)
 
 hist(sam.mean, freq = FALSE, breaks = 25
       , main = "Bootstrap sampling distribution of the mean"
       , xlab = paste("Data: n=", n
                     , ", mean =", signif (mean(dat), digits = 5)
                     , ", se =", signif (sd(dat)/sqrt(n)), digits = 5))
  # overlay a density curve for the sample means
  points (density (sam.mean) , type = "l")
  # overlay a normal distribution, bord and red
  x<- seq(min(sam.mean), max(sam.mean), length = 1000)
  points(x,dnorm(x, mean = mean(dat), sd = sd (dat)/sqrt(n))
         , type = "l", lwd = 2, col = "red")
  # place a rug of points under the plot
  rug(sam.mean)
  # restore par( ) settings
  par(old.par)
}

  # example data, skewed --- try others datasets to develop your intuition
  x<-rgamma(10, shape = .5, scale = 20)
  #install.packages("bs.one.samp.dist")
    bs.one.samp.dist(x)

####  PAGE 78-81
#### Example: Age at First Transplant
# enter data as vector
age <- c(54, 42, 51, 54, 49, 56, 33, 58, 54, 64, 49)
op <- par(mar = rep(0, 4))   
plot.new( )
par(mfrow=c(2,1))
# Histogram overlaid with kernel density curve
hist(age, freq = FALSE, breaks = 6)
points(density(age), type = "l")
rug(age)

# install.packages("vioplot")
# violin plot
     library(vioplot)
     vioplot(age, horizontal=TRUE, col="gray")


#stem-and-leaf plot
stem(age, scale = 2)
#t.crit
qt(1 - 0.05/2, df = length(age) - 1)
# look at help for t.test
     ?t.test
     # defaults include: alternative = "two.sided", conf.level = 0.95
     t.summary<- t.test(age, mu = 50)
     t.summary
     summary(age)
  bs.one.samp.dist(age)



# Function ot plot t-distribution with shaded p-value
t.dist.pval <- function(t.summary){
  par(mfrow=c(1,1))
  lim.extreme <- max(4, abs(t.summary$statistic) + 0.5)
  lim.lower <- -lim.extreme ;
  lim.upper <- lim.extreme ;
  x.curve<-seq(lim.lower, lim.upper, length=200)
  y.curve <- dt(x.curve, df = t.summary$parameter)
  plot(x.curve, y.curve, type = "n"
       ,ylab = paste("t-dist( df =", signif(t.summary$parameter,3),") ")
       ,xlab = paste("t-stat =", signif (t.summary$statistic, 5)
                     ,", Shaded area is p-value =", signif (t.summary$p.value, 5)))
  if ( (t.summary$alternative == "less")
       | (t.summary$alternative == "two.sided")){
    x.pval.l<- seq(lim.lower, -abs (t.summary$statistic), length=200)
    y.pval.l <- dt(x.pval.l, df = t.summary$parameter)
    polygon(c(lim.lower, x.pval.l, -abs(t.summary$statistic))
            ,c(0, y.pval.l, 0), col="gray")
  }
  if ((t.summary$alternative == "greater")
  | (t.summary$alternative == "two.sided")) {
    x.pval.u <- seq(abs(t.summary$statistic) , lim.upper , length=200)
    y.pval.u <- dt(x.pval.u, df = t.summary$parameter)
    polygon(c(abs (t.summary$statistic), x.pval.u, lim.upper)
            , c(0, y.pval.u, 0), col="gray")
  }
points(x.curve, y.curve, type="l", lwd = 2, col = "blue")
}

#for the age erample
t.dist.pval(t.summary)

names(t.summary)
t.summary$statistic
t.summary$parameter
t.summary$p.value
t.summary$conf.int
t.summary$estimate
t.summary$null.value
t.summary$alternative
t.summary$method
t.summary$data.name