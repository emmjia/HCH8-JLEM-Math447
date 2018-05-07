#############################################
##     analysis of the one-half fraction    #
#############################################
#recode the data: x=1 at level +; X=-1 at level -
coded=function(x) #a function to code variable x
{
  ifelse(x=="+", 1, -1)
}
#filtration rate data (eg. 8.1 page 325)
filtration=read.table("data/filtration.txt", header = TRUE)
for (j in 1:4)
  filtration[, j]=as.numeric(coded(filtration[, j]))
#factors have already been converted from "-","+" to -1, +1 coding

#2^(4-1) with I=ABCD (Resolution IV)
fraction=with(filtration, filtration[A * B * C * D == 1,])

summary(lm(Rate ~ A * B * C * D, fraction))
#Note:1.the effects with missing estimates are aliased with earlier effects.
#2.Each estimate must be interpreted as the sum of the effects in its alias chain.

#Alias structure
alias(lm(Rate ~ A * B * C * D, fraction))
#Note: 1.The alias table has a column for each effect that has an estimate in the output,
#and a row for each effect with a missing estimate. 
#2.The “1” in a column shows which effect is aliased with the column label.

#Half-normal plot
library(gplots)
qqnorm(aov(Rate ~ A * B * C * D, fraction), label = TRUE)
#observations:
#1.A, C, and D are all relatively large.
#Each is aliased with only a three-factor interaction, which we shall
#ignore, so we interpret them as main effects.
#2.The B main effect is aliased with ACD, so both of these seem small;
#in particular, B seems inactive.
#3.The AC estimate represents AC + BD; if B is inactive, it must represent AC.
#4.The BC estimate represents BC + AD, and if B is inactive it must represent AD.
#5. Finally, the AB estimate represents AB + CD, so both seem small.
#6. The plot suggests that the interesting effects are A, C, D, AC, and AD.

#refined model
summary(lm(Rate ~ A * C + A * D, fraction))

#############################################
##     analysis of the one-quarter fraction #
#############################################
#injection example (eg8.4, pp. 336)
#2^(6-2) runs
#defining relations: I=ABCE=BCDF=ADEF
injection=read.table("data/injection.txt", header = TRUE)
for (j in 1:(ncol(injection) - 1))
  injection[ , j]=coded(injection[ , j])
injection.lm=lm(Shrinkage ~ A * B * C * D * E * F, injection)
summary(injection.lm)

#Half-normal plot
qqnorm(aov(Shrinkage ~ A * B * C * D * E * F, injection), label = TRUE)

#interaction plot and main effect
with(injection, interaction.plot(A, B, Shrinkage))
with(injection, tapply(Shrinkage,A,mean))
with(injection, tapply(Shrinkage,B,mean))
#plots indicates that A=-1, B=-1

#refined model
injection.lm1=lm(Shrinkage ~ A * B, injection)

#residual plots and qqplot
res=injection$Shrinkage-fitted(injection.lm1)
library(car)
qqPlot(res)
plot(fitted(injection.lm1), res) 
plot(injection$A, res)
plot(injection$B, res)
plot(injection$C, res)
plot(injection$D, res)
plot(injection$E, res)
plot(injection$F, res)
#Residual plots suggest that C is a dispersion effect.

#Analyze absolute residuals
summary(aov(abs(res) ~ A * B * C * D * E * F, injection))
qqnorm(aov(abs(res) ~ A * B * C * D * E * F, injection), label = TRUE)
qqnorm(aov(res^2 ~ A * B * C * D * E * F, injection), label = TRUE)
#All three half-normal plots confirm that C is a dispersion effect

#main effect of C when A=-1 and B=-1
injection1=with(injection, injection[A==-1 & B==-1,])
with(injection1, tapply(Shrinkage,C,mean))
#decision: A=-1, B=-1, C=-1

#############################################
##     analysis of resolution III designs   #
#############################################
#Eye focus time example (2^(7-4) resolution III design, eg.8.7, pp. 354)
A=rep(c(-1,1),4)
B=rep(c(-1,-1,1,1),2)
C=c(rep(-1,4),rep(1,4))
Time=c(88.5,75.1,93.2,145.4, 83.7,77.6,95,141.8)
eye=data.frame(A=A,B=B, C=C, Time=Time)
eye=within(eye, {D = A*B;E=A*C; F=B*C; G=A*B*C}) 
summary(lm(Time ~ A * B * C * D * E * F * G, eye))
library(gplots)
qqnorm(aov(Time ~  A * B * C * D * E * F * G, eye), label = TRUE)

#add fold-over design
eye.FO=-eye
eye.FO$Time=c(91.3,136.7, 82.4, 73.4, 94.1, 143.8, 87.3, 71.9)
eye.whole=rbind(eye, eye.FO)
summary(lm(Time ~ A * B * C * D * E * F * G, eye.whole))
qqnorm(aov(Time ~  A * B * C * D * E * F * G, eye.whole), label = TRUE)

#refine model
summary(aov(Time ~ B*D, eye.whole))
summary(aov(Time ~ B*D, eye.whole))

#with blocks (confounded with ABD, ACE, BCF, ABCG)
eye$Block=1
eye.FO$Block=2
eye.whole1=rbind(eye, eye.FO)
summary(lm(Time ~ Block + A * B * C * D * E * F * G, eye.whole1))
qqnorm(aov(Time ~ Block + A * B * C * D * E * F * G, eye.whole1), label = TRUE)
