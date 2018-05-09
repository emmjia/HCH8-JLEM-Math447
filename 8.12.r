# declaring data
A <- rep(x = c("-", "+"), times = 8)
B <- rep(x = c("-", "+"), each = 2, times = 4)
C <- rep(x = c("-", "+"), each = 4, times = 2)
D <- c("-", "+", "+", "-", "+", "-", "-", "+", "-", "+", "+", "-", "+", "-", "-", "+")
E <- rep(x = c("-", "+"), each = 8)
FH1 <- c(7.78, 8.15, 7.5, 7.59, 7.54, 7.69, 7.56, 7.56, 7.5, 7.88, 7.5, 7.63, 7.32, 7.56, 7.18, 7.81)
FH2 <- c(7.78, 8.18, 7.56, 7.56, 8, 8.09, 7.52, 7.81, 7.25, 7.88, 7.56, 7.75, 7.44, 7.69, 7.18, 7.5)
FH3 <- c(7.81, 7.88, 7.5, 7.75, 7.88, 8.06, 7.44, 7.69, 7.12, 7.44, 7.5, 7.56, 7.44, 7.62, 7.25, 7.59)

# creating table
A <- c(A, A, A)
B <- c(B, B, B)
C <- c(C, C, C)
D <- c(D, D, D)
E <- c(E, E, E)
FH <- as.numeric(c(FH1, FH2, FH3))

spring <- data.frame(cbind(A, B, C, D, E, FH))

# defining coded
coded=function(x)
{
  ifelse(x=="+", 1, -1)
}

# decoding data
for (j in 1:5)
  spring[, j]=as.numeric(coded(spring[, j]))

#Half Normal Probability Plot
qqnorm(aov(as.numeric(FH) ~ A*B*C*D*E, spring), label = TRUE)#A,B,E,BE from plot

#Interaction plot with main effect
with(spring, interaction.plot(A, E, as.numeric(FH)))
with(spring, interaction.plot(B, E, as.numeric(FH)))
with(spring, interaction.plot(C, E, as.numeric(FH)))
with(spring, interaction.plot(D, E, as.numeric(FH)))
##mean
with(spring, tapply(as.numeric(FH),B,mean))
with(spring, tapply(as.numeric(FH),E,mean))

