# demo.clt.unif(N, n)
# draws N samples of size n from Uniform(o,1)
# and plots the N means with a normal distribution overlay
demo.clt.unif <- function(N, n){
  # draw sample in a matrix with N columns and n rows
  sam <- matrix(runif (N*n, 0,1),ncol=N);
  # calculate the mean of each column
  sam.mean <- colMeans (sam)
  # the sd of the mean is the SEM
  sam.se <- sd(sam.mean)
  # calculate the true SEM given the sample size n
  true.se <- sqrt((1/12)/n)
  # draw a histogram of the means
  hist(sam.mean,freq=FALSE,breaks=25
       ,main =paste("True SEM=",round(true.se, 4)
                    ,", Est SEM= ", round( sam.se, 4))
       ,xlab = paste("n=",n))
  # overlay a density curve for the sаmрle means
  points (density(sam.mean) , type ="l")
  # overlay a normal distribution,bold and red
  x <- seq(0,1,length = 1000)
  points(x, dnorm(x,mean=0.5,sd=true.se) ,type="l",lwd=2, col="red")
  # place a rug of points under the plot
  rug(sam.mean)
}
op <- par(mar = rep(0,4))   
plot.new()
par (mfrow=c(2,2));
demo.clt.unif(10000,1);
demo.clt.unif(10000,2);
demo.clt.unif(10000,6);
demo.clt.unif(10000,12);


#### Illustration of Central Limit Theorem, Exponential distribution
# demo.clt.exp(N, n)  draws N samples of size n from Exponential(1)
# and plots the N means with a normal distribution overlay
demo.clt.exp <- function(N, n) {
  # draw sample in a matrix with N columns and n rows
  sam <- matrix(runif (N*n, 1),ncol=N);
  # calculate the mean of each column
  sam.mean <- colMeans (sam)
  # the sd of the mean is the SEM
  sam.se <- sd(sam.mean)
  # calculate the true SEM given the sample size n
  true.se <- sqrt(1/n)
  # draw a histogram of the means
  hist(sam.mean,freq=FALSE,breaks=25
       ,main =paste("True SEM=",round(true.se, 4),", Est SEM= ", round( sam.se, 4))
       ,xlab = paste("n=",n))
  # overlay a density curve for the sаmрle means
  points (density(sam.mean) , type ="l")
  # overlay a normal distribution,bold and red
  x <- seq(0,5,length = 1000)
  points(x, dnorm(x,mean=1,sd=true.se) ,type="l",lwd=2, col="red")
  # place a rug of points under the plot
  rug(sam.mean)
}
op <- par(mar = rep(0,4))   
plot.new()
par (mfrow=c(2,2));
demo.clt.exp(10000,     1);
demo.clt.exp(10000,     6);
demo.clt.exp(10000,   30);
demo.clt.exp(10000, 100);


#### More examples for Central Limit Theorem can be illustrated with this code
# install.packages("TeachingDemos")
library(TeachingDemos)
# look at examples at bottom of the help page
?clt.examp


#### Normal vs t -distributions with a range of degrees-of- freedom
x <- seq(-8, 8, length = 1000)
par(mfrow=c(1, 1))
plot(x, dnorm(x), type = "l",lwd = 2,col= "red"
       ,main = "Normal (red) vs t-dist with df=1, 2, 6, 12, 30, 100")
points(x, dt(x,1), type="l")
points(x, dt(x,2), type="l")
points(x, dt(x,6), type="l")
points(x, dt(x,12), type="l")
points(x, dt(x,30), type="l")
points(x, dt(x,100), type="l")


#### Illustration of Confidence Intervals (consistent with their interpretation)
# install.packages("TeachingDemos")
library(TeachingDemos)
ci.examp(mean.sim = 10, sd = 2, n = 25, reps = 100, conf.level = 0.95, method = "t")