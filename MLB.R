install.packages("Lahman")
install.packages("plyr")
rm(list=ls())
setwd("c:/kpu_data")
getwd()
library(Lahman)
library(plyr)

a<-subset(Batting,yearID==2014)
b<-subset(Batting,yearID==2015)
c<-merge(a,b,by="playerID")
d<-c[c$AB.x>10&c$AB.y>10,]

a<-subset(Batting,yearID>2014)
a$teamID<-as.numeric(as.factor(a$teamID))
b<-function(a){return(data.frame(
  team=ifelse(mean(a$teamID)==a$teamID,0,1),
  a$playerID,a$lgID,a$SH,a$H,
  a$yearID,a$teamID,a$teamID,a$RBI,a$AB))}
d<-ddply(a,.(playerID),b)

#관측기간 중 이동 선수 빼기
d$lag_team<-as.numeric(sapply(1:nrow(d),function(x){d$a.teamID[x-1]}))
d$lag_RBI<-as.numeric(sapply(1:nrow(d),function(x){d$a.RBI[x-1]}))
d$lag_AB<-as.numeric(sapply(1:nrow(d),function(x){d$a.AB[x-1]}))
#d$lag_SF<-as.numeric(sapply(1:nrow(d),function(x){d$a.SF[x-1]}))
d$lag_SH<-as.numeric(sapply(1:nrow(d),function(x){d$a.SH[x-1]}))
d$lag_H<-as.numeric(sapply(1:nrow(d),function(x){d$a.H[x-1]}))
d$lag_player<-as.playerID<-as.character(sapply(1:nrow(d),function(x){d$playerID[x-1]}))

#전년도 성적 데이터 제거
#d$lag_team<-ifelse(d$playerID==d$lag_playerID,d$lag_team,"NA")
d$lag_avg<-d$lag_H/d$lag_AB
#d$sac<-d$lag_SF+d$lag_SH
d<-subset(d,a.AB>400&lag_AB>400)
d$change_rbi<-d$a.RBI/d$lag_RBI
d<-subset(d,!((lag_team=="NA")|(a.teamID==lag_team)))
d$lg_col<-ifelse(d$a.lgID=="NL","gray","black")
d$lg_shape<-ifelse(d$a.lgID=="NL",2,15)

#전년도 타율과 타점 변화율 상관관계
cor(d$lag_avg,d$change_rbi)
plot(d$lag_avg,d$change_rbi,main="Predictor of RBI",xlab="Batting Average of Prior year",ylab="Change in RBI",las=1,
     cex.axis=0.8,pch=19,col=d$lg_col)

#
text(x=0.3,y=1.6,label="r=-0.49")
abline(lm(change_rbi~lag_avg,d))
legend(x=0.29,y=1.4,c("National","American"),col=c("grey","black"),pch=c(19,19))

#cor(d$sac,d$change_rbi)

#plot(d$sac,d$change_rbi,main="Predictor of RBI",fron.main=3,xlab="Sacrifice Files & Hits",ylab="change in RBI",las=1,
#     cex.axis=0.8,pch=d$lg_shape)
text(x=4,y=1.6,label="r=0.50")
#abline(lm(change_rbi~sac,d),lty=2,lwd=3)

install.packages("tableHTML")
library(tableHTML)
e<-with(d,data.frame(change_rbi,sac,lag_avg))
colnames(e)<-c("c_RBI","Sacrifice","AVG")
cor(e)

tableHTML(round(cor(e),3))