fat <- read.table(text="
Row fat1 fat2 fat3 fat4
1 164 178 175 155
2 172 191 186 166
3 168 197 178 149
4 177 182 171 164
5 190 185 163 170
6 176 177 176 168
", header=TRUE)
fat
library(reshape2)
fat.long <- melt(fat,id.vars=c("Row"),
                 measure.vars = c("fat1", "fat2", "fat3", "fat4"),
                 variable.name = "type",
                 value.name = "amount")
fat.long
fat.wide <- dcast(fat.long, Row ~ type, value.var = "amount")
library(plyr)
fat.summary <- ddply(fat.long,
                     "type",
                     function(X){
                       data.frame( m = mean(X$amount),
                                   s = sd(X$amount),
                                   n = length(X$amount) ) })
fat.summary$se <- fat.summary$s/sqrt(fat.summary$n)
fat.summary$ci.l <- fat.summary$m - qt(1-.05/2, df=fat.summary$n-1) * fat.summary$se
fat.summary$ci.u <- fat.summary$m + qt(1-.05/2, df=fat.summary$n-1) * fat.summary$se
fat.summary
library(ggplot2)
p <- ggplot(fat.long, aes(x = type, y = amount))
p <- p + geom_hline(yintercept = mean(fat.long$amount),
colour = "black" , linetype = "dashed" , size = 0.3, alpha = 0.5)
p <- p + geom_boxplot(size = 0.75, alpha = 0.5)
p <- p + geom_point(position = position_jitter(w = 0.05, h = 0), alpha = 0.5)
p <- p + stat_summary(fun = mean, geom = "point", shape = 18, size = 6,aes(colour = type), alpha = 0.8)
p <- p + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",width = .2, aes(colour=type), alpha = 0.8)
p <- p + labs(title = "Doughnut fat absorption") + ylab("amount absorbed (g)")
print(p)
fit.f <- aov(amount ~ type, data = fat.long)
summary(fit.f)
pairwise.t.test(fat.long$amount, fat.long$type,
 pool.sd = TRUE, p.adjust.method = "none")

pairwise.t.test (fat.long$amount, fat.long$type,
 pool.sd = TRUE, p.adjust.method = "bonf")
glabella <- read.table(text="
Row cauc afam naaa
1   5.75  6.00  8.00
2   5.50  6.25  7.00
3   6.75  6.75  6.00
4   5.75  7.00  6.25
5   5.00  7.25  5.50
6   5.75  6.75  4.00
7   5.75  8.00  5.00
8   7.75  6.50  6.00
9   5.75  7.50  7.25
10  5.25  6.25  6.00
11  4.50  5.00  6.00
12  6.25  5.75  4.25
13  NA   5.00  4.75
14  NA   NA   6.00
", header=TRUE)
glabella.long <- melt(glabella,
                      id.vars=c("Row"),
                      variable.name = "pop",
                      value.name = "thickness",
                      na.rm = TRUE
)
names(glabella.long) <- c("Row", "pop", "thickness")
library(ggplot2)
p <- ggplot(glabella.long, aes(x = pop, y = thickness))
p <- p + geom_hline(yintercept = mean(glabella.long$thickness),
                    colour = "black", linetype = "dashed", size = 0.3, alpha = 0.5)
p <- p + geom_boxplot(size = 0.75, alpha = 0.5)
p <- p + geom_point(position = position_jitter(w = 0.05, h = 0), alpha = 0.5)
p<-p + stat_summary(fun = mean, geom = "point", shape = 18, size = 6,aes(colour=pop), alpha = 0.8)

p <- p + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",width = .2, aes(colour=pop), alpha = 0.8)
p <- p + labs(title = "Glabella thickness") + ylab("thickness (mm)")
print(p)
glabella.summary <- ddply(glabella.long, "pop",
                          function(X) { data.frame( m = mean(X$thickness),
   s = sd(X$thickness),
   n = length(X$thickness) ) } )
glabella.summary
fit.g <- aov(thickness ~ pop, data = glabella.long)
summary(fit.g)
fit.g
pairwise.t.test(glabella.long$thickness, glabella.long$pop,
 pool.sd = TRUE, p.adjust.method = "bonf")
TukeyHSD(fit.f)
TukeyHSD(fit.g)
pairwise.t.test(fat.long$amount, fat.long$type,pool.sd = TRUE, p.adjust.method = "BH")
pairwise.t.test(glabella.long$thickness, glabella.long$pop,pool.sd = TRUE, p.adjust.method = "BH")
par(mfrow=c(3,1))
hist(fit.g$residuals, freq = FALSE, breaks = 20)
points(density(fit.g$residuals), type = "l")
rug(fit.g$residuals)
library(zoo)
library(sm)
library(vioplot)
vioplot(fit.g$residuals, horizontal=TRUE, col="gray")
boxplot(fit.g$residuals, horizontal=TRUE)
par(mfrow=c(1,1))
library(carData)
library(car)
qqPlot(fit.g$residuals, las = 1, id = list(n = 8, cex = 1), lwd = 1, main="QQ Plot")
shapiro.test(fit.g$residuals)
library(nortest)
ad.test(fit.g$residuals)
cvm.test(fit.g$residuals)
bartlett.test(thickness ~ pop, data = glabella.long)
library(carData)
library(car)
leveneTest(thickness ~ pop, data = glabella.long)
fligner.test(thickness ~ pop, data = glabella.long)
chds <- read.csv("http://statacumen.com/teach/ADA1/ADA1_notes_05-CHDS.csv")
chds$smoke <- rep(NA, nrow(chds));
chds[(chds$m_smok == 0), "smoke"] <- "0 cigs" ;
chds[(chds$m_smok > 0)&(chds$m_smok < 20),"smoke"] <- "1-19 cigs" ;
chds[(chds$m_smok >= 20),"smoke"] <- "20+ cigs";
chds$smoke <- factor(chds$smoke)
pi <- ggplot(chds, aes(x = c_bwt))
pi <- pi + geom_histogram(binwidth = .4)
pi <- pi + facet_grid(smoke ~ .)
pi <- pi + labs(title = "Child birthweight vs maternal smoking") +
  xlab("child birthweight (lb)")
print(pi)
p2 <- ggplot(chds, aes(x = c_bwt, fill=smoke))
p2 <- p2 + geom_histogram(binwidth = .4, alpha = 1/3, position="identity")
p2 <- p2 + labs(title = "Child birthweight vs maternal smoking") +
xlab("child birthweight (lb)")
print(p2)
library(gridExtra)
grid.arrange(pi, p2, ncol=1)
library(ggplot2)
p <- ggplot(chds, aes(x = smoke, y = c_bwt))
p <- p + geom_hline(yintercept = mean(chds$c_bwt),
                    colour = "black" , linetype = "dashed" , size = 0.3, alpha = 0.5)
p <- p + geom_boxplot(size = 0.75, alpha = 0.5)
p <- p + geom_point(position = position_jitter(w = 0.05, h = 0), alpha = 0.2)
p <- p + stat_summary(fun = mean, geom = "point", shape = 18, size = 4,
                   aes(colour=smoke), alpha = 0.8)
p <- p + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",
                      width = .2, aes(colour=smoke), alpha = 0.8)
p <- p + labs(title = "Child birthweight vs maternal smoking") +
 ylab("child birthweight (lb)")
print(p)
library(carData)
library(car)
par(mfrow=c(1,3))
qqPlot(subset(chds, smoke == "0 cigs")$c_bwt, las = 1, id = list(n = 0, cex =1),lwd = 1, main="QQ Plot, 0 cigs")
qqPlot(subset(chds, smoke == "1-19 cigs")$c_bwt, las = 1, id = list(n = 0, cex =1),lwd = 1, main="QQ Plot, 1-19 cigs")
qqPlot(subset(chds, smoke == "20+ cigs" )$c_bwt, las = 1, id = list(n = 0, cex =1),lwd = 1, main="QQ Plot, 20+ cigs")
library(nortest)
shapiro.test(subset(chds, smoke == "0 cigs" )$c_bwt)
ad.test( subset(chds, smoke == "0 cigs" )$c_bwt)
cvm.test( subset(chds, smoke == "0 cigs" )$c_bwt)
shapiro.test(subset(chds, smoke == "1-19 cigs")$c_bwt)
ad.test( subset(chds, smoke == "1-19 cigs")$c_bwt)
cvm.test( subset(chds, smoke == "1-19 cigs")$c_bwt)
shapiro.test(subset(chds, smoke == "20+ cigs" )$c_bwt)
ad.test( subset(chds, smoke == "20+ cigs" )$c_bwt)
cvm.test( subset(chds, smoke == "20+ cigs" )$c_bwt)
fit.c <- aov(c_bwt ~ smoke, data = chds)
par(mfrow=c(3,1))
hist(fit.c$residuals, freq = FALSE, breaks = 20)
points(density(fit.c$residuals), type = "l")
rug(fit.c$residuals)
library(vioplot)
vioplot(fit.c$residuals, horizontal=TRUE, col="gray")
boxplot(fit.c$residuals, horizontal=TRUE)
par(mfrow=c(1,1))
library(sm)
library(car)
qqPlot(fit.c$residuals, las = 1, id=list(n = 0, cex =1), lwd = 1, main="QQ Plot")
shapiro.test(fit.c$residuals)
library(nortest)
ad.test(fit.c$residuals)
cvm.test(fit.c$residuals)
chds.summary <- ddply(chds,"smoke",
                      function(X) { data.frame( m = mean(X$c_bwt),
  s = sd(X$c_bwt),
  n = length(X$c_bwt) ) } )
bartlett.test(c_bwt ~ smoke, data = chds)
library(car)
leveneTest(c_bwt ~ smoke, data = chds)
fligner.test(c_bwt ~ smoke, data = chds)
summary(fit.c)
TukeyHSD(fit.c)

