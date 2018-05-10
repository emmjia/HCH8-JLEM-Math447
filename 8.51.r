A <- rep(x = c("-", "+"), times = 8)
B <- rep(x = c("-", "+"), each = 2, times = 4)
C <- rep(x = c("-", "+"), each = 4, times = 2)
D <- rep(x = c("-", "+"), each = 8)
E <- c("+","+","-","-","+","+","-","-","-","-","+","+","-","-","+","+")
F <- c("-","-","+","+","+","+","-","-","+","+","-","-","-","-","+","+")
G <- c("+","-","+","-","-","+","-","+","+","-","+","-","-","+","-","+")
H <- c("-","+","-","+","+","-","+","-","+","-","+","-","-","+","-","+")
J <- c("+","-","-","+","+","-","-","+","+","-","-","+","+","-","-","+")
FTMOD <- c(7.52,4.18,1.57,2.12,1.87,2.12,7.12,1.57,1.21,0.50,1.87,3.54,1.87,2.12,0.50,0.50)
#data
hardata = data.frame(A,B,C,D,E,F,G,H,J,FTMOD)

coded=function(x) #a function to code variable x
{
  ifelse(x=="+", 1, -1)
}
for (j in 1:9)
  hardata[, j]=as.numeric(coded(hardata[, j]))

fraction.hardata=with(hardata, hardata[A * B * C * D * E * F * G * H * J== 1,])

#linear regression
hardata.lm <- lm(FTMOD ~ A*B*C*D*E*F*G*H*J, fraction.hardata)

#alias
#alias(hardata.lm)


qqnorm(aov(FTMOD ~ A*B*C*D*E*F*G*H*J, fraction.hardata), label = TRUE)#F and D
