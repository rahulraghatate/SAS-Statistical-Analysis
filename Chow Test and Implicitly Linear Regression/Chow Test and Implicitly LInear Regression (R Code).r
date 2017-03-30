library(haven)
library(QuantPsyc)

options(scipen=999)###gets rid of scientific notation
gpa.dat<-read_sas("C:/Box Sync/P507/Datasets/gpa.sas7bdat")

##Creating Logged and Polynomial Transformations
##ifelse command recodes to missing if variable is equal to zero, otherwise it produces value of -inf inf, and you can't use regression
gpa.dat$lncolgpa<-ifelse(gpa.dat$COLGPA!=0,log(gpa.dat$COLGPA),NA)
gpa.dat$lnhsize<-ifelse(gpa.dat$HSIZE!=0,log(gpa.dat$HSIZE),NA)
gpa.dat$lnhsrank<-ifelse(gpa.dat$HSRANK!=0,log(gpa.dat$HSRANK),NA)
gpa.dat$satsq<-gpa.dat$SAT*gpa.dat$SAT

summary(gpa.dat)

par(mfrow=c(2,5))
hist(gpa.dat$COLGPA)
hist(gpa.dat$lncolgpa)
hist(gpa.dat$SAT)
hist(gpa.dat$TOTHRS)
hist(gpa.dat$HSIZE)
hist(gpa.dat$lnhsize)
hist(gpa.dat$HSRANK)
hist(gpa.dat$lnhsrank)
hist(gpa.dat$FEMALE)
hist(gpa.dat$ATHLETE)

basereg<-lm(COLGPA~SAT+TOTHRS+HSIZE+HSRANK+FEMALE,data=gpa.dat)
summary(basereg)
plot(basereg$fitted.values,basereg$residuals,xlab="Predicted Values",ylab="Residuals")
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(basereg)
lm.beta(basereg)###Gives Standardized Regression Coefficients, using QuantPsyc package#
###Compute ESS, RSS, and TSS
baseregESS<-sum(anova(basereg)[1:(length(basereg$coefficients)-1),2])
baseregTSS<- sum(anova(basereg)[,2] )
baseregRSS<-sum(anova(basereg)[length(basereg$coefficients),2])

newreg<-lm(COLGPA~SAT+TOTHRS+HSIZE+HSRANK+FEMALE+ATHLETE,data=gpa.dat)
summary(newreg)
plot(newreg$fitted.values,newreg$residuals,xlab="Predicted Values",ylab="Residuals")
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(newreg)
lm.beta(newreg)###Gives Standardized Regression Coefficients, using QuantPsyc package#
###Compute ESS, RSS, and TSS
newregESS<-sum(anova(newreg)[1:(length(newreg$coefficients)-1),2])
newregTSS<- sum(anova(newreg)[,2] )
newregRSS<-sum(anova(newreg)[length(newreg$coefficients),2])

anova(basereg,newreg)###Chow F Test if New Model is Statistically Better than Old Model

loglinmod<-lm(lncolgpa~SAT+TOTHRS+HSIZE+HSRANK+FEMALE+ATHLETE,data=gpa.dat) #Interpret as 100*Bi% in Y given unit change in Xi#
summary(loglinmod)
plot(loglinmod$fitted.values,loglinmod$residuals,xlab="Predicted Values",ylab="Residuals")
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(loglinmod)
lm.beta(loglinmod)###Gives Standardized Regression Coefficients, using QuantPsyc package#
###Compute ESS, RSS, and TSS
loglinmodESS<-sum(anova(loglinmod)[1:(length(loglinmod$coefficients)-1),2])
loglinmodTSS<- sum(anova(loglinmod)[,2] )
loglinmodRSS<-sum(anova(loglinmod)[length(loglinmod$coefficients),2])

linlogmod<-lm(COLGPA~SAT+TOTHRS+HSIZE+lnhsrank+FEMALE+ATHLETE,data=gpa.dat) #Interpret as a 1% change in HSRANK causes COLGPA to change by B4/100#
summary(linlogmod)
plot(linlogmod$fitted.values,linlogmod$residuals,xlab="Predicted Values",ylab="Residuals")
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(linlogmod)
lm.beta(linlogmod)###Gives Standardized Regression Coefficients, using QuantPsyc package#
###Compute ESS, RSS, and TSS
linlogmodESS<-sum(anova(linlogmod)[1:(length(linlogmod$coefficients)-1),2])
linlogmodTSS<- sum(anova(linlogmod)[,2] )
linlogmodRSS<-sum(anova(linlogmod)[length(linlogmod$coefficients),2])

satsqmod<-lm(COLGPA~SAT+satsq+TOTHRS+HSIZE+HSRANK+FEMALE+ATHLETE,data=gpa.dat)
summary(satsqmod)
plot(satsqmod$fitted.values,satsqmod$residuals,xlab="Predicted Values",ylab="Residuals")
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(satsqmod)
lm.beta(satsqmod)###Gives Standardized Regression Coefficients, using QuantPsyc package#
###Compute ESS, RSS, and TSS
satsqmodESS<-sum(anova(satsqmod)[1:6,2])
satsqmodTSS<- sum(anova(satsqmod)[,2] )
satsqmodRSS<-satsqmodTSS-satsqmodESS
