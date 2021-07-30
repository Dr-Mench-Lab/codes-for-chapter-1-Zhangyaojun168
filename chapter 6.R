income <- c(7, 1110, 7, 5, 8, 12, 0, 5, 2, 2, 46, 7)
income <- sort(income, decreasing = TRUE)
income
summary(income)
sd(income)
par(mfrow=c(3,1))
hist(income, freq = FALSE, breaks = 1000)
points(density(income), type = "l")
rug(income)
library(vioplot)
vioplot(income, horizontal=TRUE, col="gray")
boxplot(income, horizontal=TRUE)
library(car)
qqPlot(income, las = 1, id=list(n = 0, cex = 1), lwd = 1, main="QQ Plot, Income")
bs.one.samp.dist <- function(dat, N = 1e4){
  n <- length(dat) ;
  sam <- matrix(sample(dat,size = N * n, replace = TRUE), ncol=N);
  sam.mean <- colMeans(sam) ;
  old.par <- par(no.readonly =TRUE)
  par(mfrow=c(2,1), mar=c(3,2,2,1), oma=c(1,1,1,1))
  hist(dat,freq = FALSE, breaks = 6,main = "Plot of data with smoothed density curve")
  points(density(dat), type = "l")
  rug(dat)
  hist(sam.mean,freq = FALSE,breaks = 25, main = "Bootstrap sampling distribution of the mean", 
xlab = paste( "Data: n =", n, " , mean =", signif(mean(dat) , digits = 5), " , se =", signif(sd(dat)/sqrt(n)), digits = 5))
  points(density(sam.mean), type = "l")
  x <- seq(min(sam.mean) , max(sam.mean), length = 1000)
  points(x, dnorm(x,mean = mean(dat), sd = sd(dat)/sqrt(n)), type = "l",lwd = 2, col = "red")
  rug(sam.mean)
  par(old.par)
}
bs.one.samp.dist(income)
install.packages('BSDA')
library(BSDA)
t.test(income)
SIGN.test(income)
age <- c(54, 42, 51, 54, 49, 56, 33, 58, 54, 64, 49)
age <- sort(age, decreasing = TRUE)
age
summary(age)
sd(age)
par(mfrow=c(3,1))
hist(age, freq = FALSE, breaks = 10)
points(density(age), type = "l")
rug(age)
library(vioplot)
vioplot(age, horizontal=TRUE, col="gray")
boxplot(age, horizontal=TRUE)
library(car)
qqPlot(age, las = 1, id=list(n = 0, cex = 1), lwd = 1, main="QQ Plot , Income")
bs.one.samp.dist(age)
library(BSDA)
t.test(age, mu=50)
SIGN.test(age, md=50)
dat <- c(20, 18, 23, 5, 14, 8, 18, 22)
dat <- sort(dat, decreasing = TRUE)
dat
summary(dat)
sd(dat)
par(mfrow=c(3,1))
hist(dat , freq = FALSE, breaks = 10)
points(density(dat), type = "l")
rug(dat)
library(vioplot)
vioplot(dat , horizontal=TRUE, col="gray")
boxplot(dat , horizontal=TRUE)
par(mfrow=c(1,1))
library(car)
qqPlot(dat, las = 1, id=list(n = 0, cex = 1), lwd = 1, main="QQ Plot , Income")
bs.one.samp.dist(dat)
t.test(dat, mu=10)
wilcox.test(dat, mu=10, conf.int=TRUE)
wilcox.test(dat, mu=10, conf.int=TRUE, correct=FALSE)
a <- c( 0.7, -1.6, -0.2, -1.2, 0.1, 3.4, 3.7, 0.8, 0.0, 2.0)
b <- c( 1.9, 0.8, 1.1, 0.1, -0.1, 4.4, 5.5, 1.6, 4.6, 3.0)
d <- b - a;
sleep <- data.frame(a, b, d)
summary(sleep$d)
shapiro.test(sleep$d)
library(ggplot2)
p3 <- ggplot(sleep, aes(x = "d", y = d))
p3 <- p3 + geom_hline(yintercept=0, colour="#BB0000", linetype="dashed")
p3 <- p3 + geom_boxplot()
p3 <- p3 + geom_point()
p3 <- p3 + stat_summary(fun.y = mean, geom = "point", shape = 18,
size = 4, alpha = 0.3)
p3 <- p3 + coord_flip()
print(p3)

t.test(sleep$d, mu=0)
wilcox.test(sleep$d, mu=0, conf.int=TRUE)
Uwet <- c(0.21, 0.25, 0.16, 0.23, 0.47, 1.20, 0.29, 1.10, 0.16)
Walker <- c(0.69, 0.23, 0.10, 0.03, 0.56, 0.10, 0.01, 0.02, 0.04, 0.22)
met <- data.frame(Uwet=c(Uwet,NA), Walker)
library(reshape2)
met.long <- melt(met, variable.name = "site", value.name = "cool", na.rm=TRUE)
names(met.long) <- c("site", "cool")
library(ggplot2)
p <- ggplot(met.long, aes(x = site, y = cool, fill=site))
p <- p + geom_boxplot()
p <- p + geom_point(position = position_jitter(w = 0.05, h = 0), alpha = 0.2)
p <- p + stat_summary(fun.y = mean, geom = "point" , shape = 3, size = 2)
p <- p + coord_flip()
p <- p + labs(title = "Cooling rates for samples of meteorites at two locations")
p <- p + theme(legend.position="none")
print(p)
par(mfrow=c(1,2))
library(car)
qqPlot(Walker, las = 1, id=list(n = 0, cex = 1), lwd = 1, main="QQ Plot, Walker")
qqPlot(Uwet, las = 1, id=list(n = 0, cex = 1), lwd = 1, main="QQ Plot, Uwet")
summary(Uwet)
c(sd(Uwet), IQR(Uwet), length(Uwet))
summary(Walker)
c(sd(Walker), IQR(Walker), length(Walker))
t.test(Uwet, Walker, var.equal = TRUE)
t.test(Uwet, Walker)
wilcox.test(Uwet, Walker, conf.int = TRUE)
rank(met.long$cool)
by(rank(met.long$cool), met.long$site, summary)
t.test(rank(met.long$cool) ~ met.long$site, var.equal = TRUE)
time <- c(24.828, 24.833, 24.834, 24.826, 24.824, 24.756
          ,24.827, 24.840, 24.829, 24.816, 24.798, 24.822
          ,24.824, 24.825, 24.823, 24.821, 24.830, 24.829
          ,24.831, 24.824, 24.836, 24.819, 24.820, 24.832
          ,24.836, 24.825, 24.828, 24.828, 24.821, 24.829
          ,24.837, 24.828, 24.830, 24.825, 24.826, 24.832
          ,24.836, 24.830, 24.836, 24.826, 24.822, 24.823
          ,24.827, 24.828, 24.831, 24.827, 24.827, 24.827
          ,24.826, 24.826, 24.832, 24.833, 24.832, 24.824
          ,24.839, 24.824, 24.832, 24.828, 24.825, 24.825
          ,24.829, 24.828, 24.816, 24.827, 24.829, 24.823)
library(nortest)
ad.test(time)
Passage_df <- data.frame(time)
p1 <- ggplot(Passage_df, aes(x = time))

p1 <- p1 + geom_histogram(aes(y=..density..)
, binwidth=0.001
, colour="black" , fill="white")

p1 <- p1 + geom_density(alpha=0.1, fill="#FF6666")
p1 <- p1 + geom_point(aes(y = -1)
                      ,position = position_jitter(height = .5)
                      ,alpha = 1/5)
p2 <- ggplot(Passage_df, aes(x = "t", y = time))
p2 <- p2 + geom_violin(fill = "gray50")
p2 <- p2 + geom_boxplot(width = 0.2, alpha = 3/4)
p2 <- p2 + coord_flip()
p3 <- ggplot(Passage_df, aes(x = "t", y = time))
p3 <- p3 + geom_boxplot()
p3 <- p3 + coord_flip()
library(gridExtra)
grid.arrange(p1, p2, p3, ncol=1)
par(mfrow=c(1,1))
library(car)
qqPlot(time, las = 1, id=list(n = 0,cex = 1), lwd = 1, main="QQ Plot, Time")
bs.one.samp.dist(time)
t.sum <- t.test(time)
t.sum$conf.int
diff(t.test(time)$conf.int)
s.sum <- SIGN.test(time)
s.sum[2,c(2,3)]
diff(s.sum[2,c(2,3)])
w.sum <- wilcox.test(time, conf.int=TRUE)
w.sum$conf.int
#6.5
emis <- read.table(text="
Pre-y63 y63-7 y68-9 y70-l y72-4
2351 620 1088 141 140
1293 940 388 359 160
541 350 111 247 20
1058 700 558 940 20
411 1150 294 882 223
570 2000 211 494 60
800 823 460 306 20
630 1058 470 200 95
905 423 353 100 360
347 900 71 300 70
NA 405 241 223 220
NA 780 2999 190 400
NA 270 199 140 217
NA NA 188 880 58
NA NA 353 200 235
NA NA 117 223 1880
NA NA NA 188 200
NA NA NA 435 175
NA NA NA 940 85
NA NA NA 241 NA
", header=TRUE)
emis.long <- melt(emis,
                  variable.name = "year",
                  value.name = "hc",
                  na.rm = TRUE )
names(emis.long) <- c("year", "hc")
by(emis.long$hc, emis.long$year, summary)
by(emis.long$hc , emis.long$year, function(X){ c(IQR(X), sd(X), length(X))})
library(ggplot2)
p <- ggplot(emis.long, aes(x = year, y = hc))
p <- p + geom_hline(yintercept = mean(emis.long$hc),
colour = "black", linetype = "dashed", size = 0.3, alpha = 0.5)
p <- p + geom_boxplot(size = 0.75, alpha = 0.5)
p <- p + geom_point(position = position_jitter(w = 0.05, h = 0), alpha = 0.5)
p <- p + stat_summary(fun.y = mean, geom = "point", shape = 18, size = 6,
aes(colour=year), alpha = 0.8)
p <- p + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",
width = .2, aes(colour=year), alpha = 0.8)
p <- p + labs(title = "Albuquerque automobile hydrocarbon emissions data") + ylab("hc (ppm)")
p <- p + scale_x_discrete(limits = rev(levels(emis.long$year)))
p <- p + coord_flip()
p <- p + theme(legend.position="none")
print(p)

fit.e <- aov(hc ~ year, data = emis.long)
summary(fit.e)
fit.er <- aov(rank(hc) ~ year, data = emis.long)
summary(fit.er)
fit.ek <- kruskal.test(hc ~ year, data = emis.long)
fit.ek
emis.long$loghc <- log(emis.long$hc)
by(emis.long$loghc, emis.long$year, summary)
by(emis.long$loghc, emis.long$year, function(X){ c(IQR(X), sd(X), length(X))})
library(ggplot2)
p <- ggplot(emis.long, aes(x = year, y = loghc))
p <- p + geom_hline(yintercept = mean(emis.long$loghc),
colour = "black", linetype = "dashed", size = 0.3, alpha = 0.5)
p <- p + geom_boxplot(size = 0.75, alpha = 0.5)
p <- p + geom_point(position = position_jitter(w = 0.05, h = 0), alpha = 0.5)
p <- p + stat_summary(fun.y = mean, geom = "point", shape = 18, size = 6,
aes(colour=year), alpha = 0.8)
p <- p + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar" ,
width = .2, aes(colour=year), alpha = 0.8)
p <- p + labs(title = "Albuquerque automobile hydrocarbon emissions data (log scale)")
p <- p + ylab("log(hc)(log(ppm))")
p <- p + scale_x_discrete(limits = rev(levels(emis.long$year)))
p <- p + coord_flip()
p <- p + theme(legend.position="none")
print(p)
fit.le <- aov(loghc ~ year, data = emis.long)
summary(fit.le)
fit.lek <- kruskal.test(loghc ~ year, data = emis.long)
fit.lek
hd <- read.table(text="
nc ahd ihd
5.37 3.96 5.37
5.80 3.04 10.60
4.70 5.28 5.02
5.70 3.40 14.30
3.40 4.10 9.90
8.60 3.61 4.27
7.48 6.16 5.75
5.77 3.22 5.03
7.15 7.48 5.74
6.49 3.87 7.85
4.09 4.27 6.82
5.94 4.05 7.90
6.38 2.40 8.36
9.24 5.81 5.72
5.66 4.29 6.00
4.53 2.77 4.75
6.51 4.40 5.83
7.00 NA 7.30
6.20 NA 7.52
7.04 NA 5.32
4.82 NA 6.05
6.73 NA 5.68
5.26 NA 7.57
  NA NA 5.68
  NA NA 8.91
  NA NA 5.39
  NA NA 4.40
  NA NA 7.13
", header=TRUE)
hd.long <- melt(hd,
variable.name = "patient" ,
value.name = "level" ,
na.rm = TRUE)
names(hd.long) <- c("patient", "level")
by(hd.long$level, hd.long$patient, summary)
by(hd.long$level, hd.long$patient, function(X){ c(IQR(X), sd(X), length(X))})
hd.long$loglevel <- log(hd.long$level)
by(hd.long$loglevel, hd.long$patient, summary)
by(hd.long$loglevel, hd.long$patient, function(X){ c(IQR(X), sd(X), length(X))})
library(ggplot2)
p <- ggplot(hd.long, aes(x = patient, y = level))
p <- p + geom_hline(yintercept = mean(hd.long$level),
colour = "black", linetype = "dashed", size = 0.3, alpha = 0.5)
p <- p + geom_boxplot(size = 0.75, alpha = 0.5)
p <- p + geom_point(position = position_jitter(w = 0.05, h = 0), alpha = 0.5)
p <- p + stat_summary(fun.y = mean, geom = "point", shape = 18, size = 6,
aes(colour=patient), alpha = 0.8)
p <- p + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",
width = .2, aes(colour=patient), alpha = 0.8)
p <- p + labs(title = "Plasma bradykininogen levels for three patient groups")
p <- p + ylab("level (mg/ml)")
p <- p + scale_x_discrete(limits = rev(levels(hd.long$patient)))
p <- p + ylim(c(0,max(hd.long$level)))
p <- p + coord_flip()
p <- p + theme(legend.position="none")
print(p)
library(ggplot2)
p <- ggplot(hd.long, aes(x = patient, y = loglevel))
p <- p + geom_hline(yintercept = mean(hd.long$loglevel),
colour = "black", linetype = "dashed", size = 0.3, alpha = 0.5)
p <- p + geom_boxplot(size = 0.75, alpha = 0.5)
p <- p + geom_point(position = position_jitter(w = 0.05, h = 0), alpha = 0.5)
p <- p + stat_summary(fun.y = mean, geom = "point", shape = 18, size = 6,
aes(colour=patient), alpha = 0.8)
p <- p + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",
width = .2, aes(colour=patient), alpha = 0.8)
p <- p + labs(title = "Plasma bradykininogen levels for three patient groups (log scale)")
p <- p + ylab("log(level)(log(mg/ml))")
p <- p + scale_x_discrete(limits = rev(levels(hd.long$patient)))
p <- p + ylim(c(0,max(hd.long$loglevel)))
p <- p + coord_flip()
p <- p + theme(legend.position="none")
print(p)
fit.h <- kruskal.test(level ~ patient, data = hd.long)
wilcox.test(hd$nc , hd$ahd, conf.int=TRUE, conf.level = 0.9833)
wilcox.test(hd$nc , hd$ihd, conf.int=TRUE, conf.level = 0.9833)
wilcox.test(hd$ahd, hd$ihd, conf.int=TRUE, conf.level = 0.9833)
wilcox.test(emis$y63.7, emis$Pre.y63, conf.int=TRUE, conf.level = 0.9875)
wilcox.test(emis$y68.9, emis$y63.7 , conf.int=TRUE, conf.level = 0.9875)
wilcox.test(emis$y72.4, emis$y70.1 , conf.int=TRUE, conf.level = 0.9875)
Tobs <- mean(met.long[(met.long$site == "Uwet") , 2] ) -
mean(met.long[(met.long$site == "Walker"), 2])
Tobs

R <- 1e4
Tperm <- rep(NA, R)

for (i.R in 1:R){
    ind.perm <- sample.int(nrow(met.long))
    lab.U <- (ind.perm <= sum(met.long$site == "Uwet"))
    lab.W <- !lab.U
    Tperm[i.R] <- mean(met.long[lab.U, 2]) - mean(met.long[lab.W, 2])
}
dat <- data.frame(Tperm)
library(ggplot2)
p <- ggplot(dat, aes(x = Tperm))
p <- p + geom_histogram(aes(y=..density..)
                        ,binwidth=0.01
   ,colour="black", fill="white")

p <- p + geom_density(alpha=0.2, fill="#FF6666")
p <- p + geom_vline(aes(xintercept=Tobs), colour="#BB0000", linetype="dashed")
p <- p + labs(title = "Permutation distribution of difference in means, Uwet and Walker Meteoi")
p <- p + xlab("difference in means (red line = observed difference in means)")
print(p)
p.upper <- sum((Tperm >= abs(Tobs))) / R
p.upper
p.lower <- sum((Tperm <= -abs(Tobs))) / R
p.lower
p.twosided <- p.lower + p.upper
p.twosided
t.summary <- t.test(cool ~ site, data = met.long, var.equal = TRUE)
t.summary
lm.summary <- lm(cool ~ site, data = met.long)
summary(lm.summary)
install.packages('lmPerm',repos = 'https://mirrors.nju.edu.cn/CRAN/')
library(lmPerm)
lmp.summary <- lmp(cool ~ site, data = met.long)
summary(lmp.summary)
time2 <- time[(rank(time) >= 3)]
old.par <- par(no.readonly = TRUE)
par(mfrow=c(5,1), mar=c(3,2,2,1), oma=c(1,1,1,1))
hist(time2, breaks=1,  main="1 break", xlim=c(24.80,24.84), xlab=""); rug(time2)
hist(time2,            main="default", xlim=c(24.80,24.84), xlab=""); rug(time2)
hist(time2, breaks=10, main="10 breaks" , xlim=c(24.80,24.84), xlab=""); rug(time2)
hist(time2, breaks=20, main="20 breaks" , xlim=c(24.80,24.84), xlab=""); rug(time2)
hist(time2, breaks=100, main="100 breaks", xlim=c(24.80,24.84), xlab=""); rug(time2)
par(old.par)
par(mfrow=c(3,1))
hist(time2, prob=TRUE, main="")
den = density(time2)
lines(den, col=2, lty=2, lwd=2)
b = round(den$bw, 4)
title(main=paste("Default =", b), col.main=2)
hist(time2, prob=TRUE, main="")
lines(density(time2, bw=0.0004), col=3, lwd=2)
text(17.5, .35,"", col=3, cex=1.4)
title(main=paste("Undersmooth, BW = 0.0004"), col.main=3)
hist(time2, prob=TRUE, main="")
lines(density(time2, bw=0.008), col=4, lwd=2)
title(main=paste("0versmooth, BW = 0.008"), col.main=4)
par(mfrow=c(1,1))
hist(time2, prob=TRUE, main= "")
lines(density(time2), col=2, lty=1, lwd=2)
lines(density(time2, ker="epan"), col=3, lty=1, lwd=2)
lines(density(time2, ker="rect"), col=4, lty=1, lwd=2)
title(main="Gaussian, Epanechnikov, Rectangular")
