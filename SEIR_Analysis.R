library(data.table)
library(tidyverse)
library(deSolve)

epi.curve <- expression(1/(1+ (exp(-beta*t)*(1-a0)/a0)))

# Cumulative_Fraction_infected

epi.curve.plot <- data.table(t=seq(from = 0, to = 100, by = 1))
a0 <- 0.01

for(i in 1:5){
  beta <- i/10
  epi.curve.plot[,paste(i/10) := eval(epi.curve)]
}

epi.curve.plot <- melt(epi.curve.plot,id.vars = c('t'))

names(epi.curve.plot) <- c('t','Beta','Cumulative_Fraction_infected')

ggplot(data = epi.curve.plot, aes(x=t,y=Cumulative_Fraction_infected, color = Beta))+
  geom_line() 


# Incident Fraction Infected

epi.curve.plot <- data.table(t=seq(from = 1, to = 100, by = 1))
a0 <- 0.01

for(i in 1:5){
  beta <- i/10
  a <- eval(epi.curve)
  b <- diff(a)
  epi.curve.plot[,paste(i/10) := b]
}

epi.curve.plot <- melt(epi.curve.plot,id.vars = c('t'))

names(epi.curve.plot) <- c('t','Beta','Incident_Fraction_Infected')

ggplot(data = epi.curve.plot, aes(x=t,y=Incident_Fraction_Infected, color = Beta))+
  geom_line() 



#######################################################

pars <- c("beta"=0.05,"nu"=0.075)
times <- seq(0,10,0.1)
y0 <- c(100,1,0)
sir <- function(t,y,p) {
  yd1 <- -p["beta"] * y[1]*y[2]
  yd2 <- p["beta"] * y[1]* y[2] - p["nu"]*y[2]
  yd3 <- p["nu"]*y[2]
  list(c(yd1,yd2,yd3),c(N=sum(y)))
}

sir.out <- lsoda(y0,times,sir,pars)


plot(sir.out[,1],sir.out[,2],type="l",col="blue",xlab="Time",
     ylab="Compartment Size")
lines(sir.out[,1],sir.out[,3],col="green")
lines(sir.out[,1],sir.out[,4],col="red")
legend(8,90,c("S","I","R"),col=c("blue","green","red"),lty=c(1,1,1))

sirOut <- as.data.table(sir.out)

sirOut %>%
  rename('S' = '1', 'I' = '2', 'R' = '3') %>%
  mutate(Cond_Epi = pars[1]*S*I - pars[2]*I)

pars[1]/pars[2]

#################################################################################

lambda.dyn <- function(t,y,p){
  yd1 <- p["mu"] - (p["mu"]+y[2])*y[1]
  yd2 <- (p["mu"] + p["nu"]) * y[2] * (p["R0"]*y[1] - 1)
  list(c(yd1,yd2))
}
pars <- c("R0"=5,"nu"=1.0,"mu"=0.014)
times <- seq(0,100,.1)
y0 <- c(.999,1e-4)
lambda.out <- lsoda(y0,times,lambda.dyn,pars)
plot(lambda.out[,1],lambda.out[,2],type="l",col="blue",
     xlab="Time",ylab="Fraction Susceptible, x(t)")
abline(h=.2,lty=2,col="red")

lambda.out %>%
  rename('S' = '1', 'I' = '2') 

plot(lambda.out[,3],lambda.out[,2],type="l",col="blue",
     xlab="Force of Infection", ylab="Fraction Susceptible")
