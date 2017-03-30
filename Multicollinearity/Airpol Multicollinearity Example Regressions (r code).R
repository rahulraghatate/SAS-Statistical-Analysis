library(haven)
library(QuantPsyc)
library(car)
library(perturb)

options(scipen=999)###gets rid of scientific notation
airpol.dat<-read_sas("C:/Box Sync/P507/Datasets/airpol.sas7bdat")

#########Correlation Matrices######
panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
rect(breaks[-nB], 0, breaks[-1], y, col="lavender", ...)
}
## put correlations & 95% CIs on the upper panels,
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y,use="complete.obs")
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    prefix <- "r = "
    rc <- cor.test(x,y)
    rci <- rc$conf.int
    txt2 <- format(c(rci, 0.123456789), digits=digits)[1]
    txt3 <- format(c(rci, 0.123456789), digits=digits)[2]
    prefix2 <- "\nCI = "
    txt <- paste(prefix, txt, prefix2, txt2, ", ", txt3, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = 1)
}
pairs(airpol.dat[,2:7], lower.panel=panel.smooth, cex = .8, pch = 21, bg="steelblue",
       diag.panel=panel.hist, cex.labels = 1.2, font.labels=2, upper.panel=panel.cor)

cor(airpol.dat[,2:7])

###Base Model and Diagnostics#####
mod.base<-lm(so~temp+me+pop+wind+precip+days,data=airpol.dat)
summary(mod.base)
base.vif<-vif(mod.base)
base.tol<-1/base.vif
colldiag(mod.base,add.intercept=TRUE)
colldiag(mod.base,add.intercept=FALSE)###This does not appear to be working correctly; still trying to figure out how to get equivalent of collinoint

####Drop out me and then pop to see effects#
mod.medrop<-lm(so~temp+pop+wind+precip+days,data=airpol.dat)
summary(mod.medrop)
mod.propdrop<-lm(so~temp+me+wind+precip+days,data=airpol.dat)
summary(mod.propdrop)


####Auxiliary Regressions####
mod.meaux<-lm(me~temp+pop+wind+precip+days,data=airpol.dat)
summary(mod.meaux)
mod.popaux<-lm(pop~temp+me+wind+precip+days,data=airpol.dat)
summary(mod.popaux)

####Final Regressions######
airpol.dat$mepc<-airpol.dat$me/airpol.dat$pop

mod.fin<-lm(so~temp+mepc+pop+wind+precip+days,data=airpol.dat)
summary(mod.fin)

mod.fin1<-lm(so~temp+mepc+wind+precip+days,data=airpol.dat)
summary(mod.fin1)
