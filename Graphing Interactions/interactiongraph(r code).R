library(foreign)
library(car)
library(xtable)
library(DAMisc)
library(effects)


setwd("C:/Dropbox/myclasses/P507/Spring 2017/Datasets/")

collgrades.dat<-read.csv("collegegpa.csv")

collgrades.dat$hsperc_rev<-100-collgrades.dat$hsperc

intmod<-lm(colgpa~tothrs+hsize+hsrank+athlete*hsperc_rev,data=collgrades.dat)

hsperc_rev.seq<-with(collgrades.dat,data.frame(seq(min(hsperc_rev),max(hsperc_rev),length=1000)))
colnames(hsperc_rev.seq) <- c("hsperc_rev")
sim.dat<-with(collgrades.dat,data.frame(athlete=rep(c(0,1),1000),tothrs=mean(tothrs),hsrank=mean(hsrank),hsize=mean(hsize)))
sim.dat <- sim.dat[order(-sim.dat$athlete),]
sim.dat<-as.data.frame(cbind(hsperc_rev.seq,sim.dat))
sim.dat <- sim.dat[order(-sim.dat$hsperc_rev,-sim.dat$athlete),]

gpapred<-predict(intmod,sim.dat,se.fit=T)

gpa.up<-gpapred$fit+1.96*gpapred$se.fit
gpa.low<-gpapred$fit-1.96*gpapred$se.fit
gpahats<-cbind(as.data.frame(gpapred),gpa.up,gpa.low)

gpacomb<-cbind(gpahats,sim.dat)
gpacomb<-gpacomb[order(gpacomb$hsperc_rev,gpacomb$athlete),]

poly.x<-c(hsperc_rev.seq,rev(hsperc_rev.seq),hsperc_rev.seq[1])
noathlete.y<-with(gpacomb,c(gpa.low[athlete==0],rev(gpa.up[athlete==0]),gpa.low[athlete==0][1]))
athlete.y<-with(gpacomb,c(gpa.low[athlete==1],rev(gpa.up[athlete==1]),gpa.low[athlete==1][1]))

png("C:/Dropbox/myclasses/P507/Spring 2017/Datasets/gpaintgraph.png",units="in",width=9.104167,height=5.750000,res=600)
par(mar=c(4,4.5,2,.5))
with(gpacomb,plot(hsperc_rev[athlete==1],fit[athlete==1],type="l",lwd=3,xlab="HS Percentile Rank",main="College GPA and Percentile in HS Class",ylab="Predicted College GPA",col="red",xlim=c(0,100),ylim=c(0,4)))
with(gpacomb,lines(hsperc_rev[athlete==0],fit[athlete==0],type="l",lwd=3,lty=2,col="blue"))
with(collgrades.dat[collgrades.dat$athlete==0,],points(hsperc_rev,colgpa,pch=16,col=rgb(0,0,1, .15, 1)))
with(collgrades.dat[collgrades.dat$athlete==1,],points(hsperc_rev,colgpa,pch=16,col=rgb(1,0,0, .75, 1)))
legend("bottomleft",c("Athlete","Non-Athlete"),col=c("red","blue"),lty=c(1,2),lwd=3,cex=1,inset=0.05)
dev.off()
