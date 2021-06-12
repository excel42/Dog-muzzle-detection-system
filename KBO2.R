rm(list=ls())
setwd("c:/kpu_data")
getwd()
library(dplyr)
library(car)

df <- read.csv("2020_KBO_Player_Data.csv", na="-")

str(df)
summary(df)
plot(df)

lg.obp<- mean(df$출루율)
lg.slg<- mean(df$장타율)
#lg.wOBA<- mean(df$wOBA)
lg.wOBA<- 0.3315#(리그 평균)
lg.R<- sum(df$득점)
lg.PA<- sum(df$타석)
#경기당 득점 RG
df$rg <- df$득점/df$경기
#경기당 타점 RBIG
df$rbig <- df$득점/df$경기
df$A_OPS <- 100*(df$출루율/lg.obp)+(df$장타율/lg.slg)-1
df$wRAA<- (df$wOBA-lg.wOBA)/1.15*df$타석
df <- transform(df, wRC = wRAA+(lg.R/lg.PA)*타석)

#결측치 제거
df <- na.omit(df)

str(df)
summary(df)
plot(df)

# 회귀 분석
par(mfrow=c(2,2))

# 득점 관련 상관관계 분석
model1 <- lm(rg ~ 안타+홈런+볼넷+도루, data=df)
summary(model1)
plot(model1)
vif(model1)

# 변수 선택법
full <- lm(rg ~ 안타+홈런+볼넷+도루, data=df)
null <- lm(rg ~ 1, data=df)

# 전방 선택법
forward <- step(null, direction = "forward", scope=list(lower=null, upper=full))
summary(forward)

# 후방 소거법
backward <- step(full, direction = "backward", scope=list(lower=null, upper=full))
summary(backward)

# 단계별 회귀
both <- step(null, direction = "both", scope=list(lower=null, upper=full))
summary(both)
AIC(both)

# 타점 관련 상관관계 분석
model2 <- lm(rbig ~ 안타+홈런+삼진, data=df)
summary(model2)
plot(model2)
vif(model2)

# 변수 선택법
full <- lm(rbig ~ 안타+홈런+삼진, data=df)
null <- lm(rbig ~ 1, data=df)

# 전방 선택법
forward <- step(null, direction = "forward", scope=list(lower=null, upper=full))
summary(forward)

# 후방 소거법
backward <- step(full, direction = "backward", scope=list(lower=null, upper=full))
summary(backward)

# 단계별 회귀
both <- step(null, direction = "both", scope=list(lower=null, upper=full))
summary(both)
AIC(both)

#OPS-WAR
plot(OPS~wOBA,data=df,pch=20,col='grey',cex=1.5)
#회귀모델 작성
lm<-lm(OPS~wOBA,data=df)
lm
abline(lm,lwd=2,col="red")
cor(df$OPS,df$wOBA)
summary(lm)

#wRRA-wRC
plot(wRAA~wRC,data=df,pch=20,col='grey',cex=1.5)
#회귀모델 작성
lm<-lm(wRAA~wRC,data=df)
abline(lm,lwd=2,col="red")
lm
cor(df$wRAA,df$wRC)
summary(lm)

#WAR-wRC
plot(WAR~wRC,data=df,pch=20,col='grey',cex=1.5)
#회귀모델 작성
lm<-lm(WAR~wRC,data=df)
abline(lm,lwd=2,col="red")
lm
cor(df$WAR,df$wRC)
summary(lm)

#WAR-wRAA
plot(WAR~wRAA,data=df,pch=20,col='grey',cex=1.5)
#회귀모델 작성
lm<-lm(WAR~wRAA,data=df)
abline(lm,lwd=2,col="red")
lm
cor(df$WAR,df$wRAA)

summary(lm)
confint(lm)

