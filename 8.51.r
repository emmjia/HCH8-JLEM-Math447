A <- rep(x = c("-", "+"), times = 8)
B <- rep(x = c("-", "+"), each = 2, times = 4)
C <- rep(x = c("-", "+"), each = 4, times = 2)
D <- rep(x = c("-", "+"), each = 8)
E <- c("+","+","+","+","-","-","-","-","-","-","-","-","+","+","+","+")
F <- c("+","+","-","-","+","+","-","-","-","-","+","+","-","-","+","+")
G <- c("+","+",rep(x =c("-","+"), each = 4), rep(x =c("-"), each = 4),"+","+")
H <- c("+","-","+","-","-","+","-","+","+","-","+","-","-","+","-","+")
J <- c("+","-","-","+","+","-","-","+","+","-","-","+","+","-","-","+")
K <- c("-","+","+","-","+","-","-","+","-","+","+","-","+","-","-","+")
FTMOD <- c(1.363,1.555,1.417,1.076,1.363,1.363,1.123,1.259,0.968,1.083,1.556,1.242,1.363,1.130,1.160,1.356)
#data
hardata = data.frame(A,B,C,D,E,F,G,H,J,K,FTMOD)

coded=function(x) #a function to code variable x
{
  ifelse(x=="+", 1, -1)
}
for (j in 1:10)
  hardata[, j]=as.numeric(coded(hardata[, j]))

fraction.hardata=with(hardata, hardata[A * B * C * D * E * F * G * H * J * K== 1,])

#linear regression
hardata.lm <- lm(FTMOD ~ A*B*C*D*E*F*G*H*J*K, hardata)

#alias
alias(hardata.lm)


qqnorm(aov(FTMOD ~ A*B*C*D*E*F*G*H*J*K, hardata), label = TRUE)#K and F
