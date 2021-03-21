library(data.table)
library(tidyverse)
library(deSolve)

SEIR.model <- function(t, b, g, c , cv, k, pep_vac,
                                                population = 1000000, infect = 100, exposed = 1000,
                                                country = 'US', province = 'New York' ,
                                                start_day = '2020-01-22'){
  #100,.50,1/14,1/14 
  #t = 100
 # b = 1/2
 # g = 1/14
 # c =  1/14
 # population = 1000000
 # infect = 100
 # exposed = 1000
  
  init <- c(S=(population-infect-exposed)/population,I=infect/population,R=0, E=exposed/population)
  parameters <- c(bet=b,gamm=g,eta=c)
  time <- seq(0,t,by=t/(1*length(1:t)))
  eqn <- function(time,state,parameters){
    with(as.list(c(state,parameters)),{
      dS <- -bet*S*I 
      dI <- eta*E-gamm*I
      dR <- gamm*I
      dE <- bet*S*I -eta*E
      return(list(c(dS,dI,dR,dE)))})}
  out<-ode(y=init,times=time,eqn,parms=parameters)
  out.dt<-as.data.table(out)
  
  out.dt[,Suspected  := S*population]
  out.dt[,Infeceted  := I*population]
  out.dt[,Recovered  := R*population]
  out.dt[,Exposed    := E*population]
  out.dt[,Vacinated  := 0]
  out.dt[,V  := 0]
  out.dt[,Population := Suspected + Infeceted + Recovered + Exposed]
  out.dt[,Model := "No_Vaccine"]
  
  #cv = 1
  #k = 0
    
  
  init <- c(SV=(population-infect-exposed)/population, IV=infect/population, R=0, EV=exposed/population, V = 0)
  parameters <- c(bet=b,gamm=g,eta=c, cc = cv, k = k)
  time <- seq(0,t,by=t/(1*length(1:t)))
  eqn <- function(time,state,parameters){
    with(as.list(c(state,parameters)),{
      dSV <- -bet*SV*IV - cc*SV*(IV)^k
      dIV <- eta*EV-gamm*IV
      dR  <- gamm*IV
      dEV <- bet*SV*IV -eta*EV
      dV <- cc*SV*(IV)^k
      return(list(c(dSV,dIV,dR,dEV,dV)))})}
  out2<-ode(y=init,times=time,eqn,parms=parameters)
  out.dtv<-as.data.table(out2)
  
  out.dtv[,Suspected  := SV*population]
  out.dtv[,Infeceted  := IV*population]
  out.dtv[,Recovered  := R*population]
  out.dtv[,Exposed    := EV*population]
  out.dtv[,Vacinated :=  V*population]
  out.dtv[,Population := Suspected + Infeceted + Recovered + Exposed]
  out.dtv[,Model := "Vaccine"]
  
  out.dtv <-
  out.dtv %>%
    rename(S = SV, I = IV, E = EV)
  
  init <- c(SV=(population-infect-exposed)/population, IV=infect/population, R=0, EV=exposed/population, V = 0)
  parameters <- c(bet=b,gamm=g,eta=c, cc = cv, k = k)
  time <- seq(0,t,by=t/(1*length(1:t)))
  eqn <- function(time,state,parameters){
    with(as.list(c(state,parameters)),{
      dSV <- ifelse(-(bet*SV*IV + pep_vac/population) <=0,-bet*SV*IV - pep_vac/population,0)
      dIV <- eta*EV-gamm*IV
      dR  <- gamm*IV
      dEV <- bet*SV*IV -eta*EV
      dV <- pep_vac/population
      return(list(c(dSV,dIV,dR,dEV,dV)))})}
  out3<-ode(y=init,times=time,eqn,parms=parameters)
  out.dtvc<-as.data.table(out3)
  
  out.dtvc[,Suspected  := SV*population]
  out.dtvc[,Infeceted  := IV*population]
  out.dtvc[,Recovered  := R*population]
  out.dtvc[,Exposed    := EV*population]
  out.dtvc[,Vacinated :=  V*population]
  out.dtvc[,Population := Suspected + Infeceted + Recovered + Exposed]
  out.dtvc[,Model := "Vaccine_Const_Admin_Rate"]
  
  out.dtvc <-
    out.dtvc %>%
    rename(S = SV, I = IV, E = EV)
  
  out.dt <- rbind(out.dt,out.dtv,out.dtvc)

  out.dt  =  
    out.dt %>%
    mutate(Confirmed_sum = Population - Suspected - Exposed) %>%
    mutate(type = rep('Model',times = nrow(out.dt)),
           Recovered_sum = Recovered,
           Exposed_sum = Exposed) %>%
    select(numDays = time, Suspected, Confirmed = Infeceted  ,Recovered, Exposed, Population, Vacinated, Confirmed_sum, Recovered_sum,Exposed_sum,Model ) 
  
  print(tail(out.dt))
  
  title <- paste("Coronavirus(2019-nCoV) SEIR Model: Basic vs. Vaccine",sep=" ")
  subtit <- bquote(list(beta==.(parameters[1]),~gamma==.(round(parameters[2],3)),~eta==.(round(parameters[3],3))))
  
  res<-ggplot(out.dt,aes(x=numDays))+
    ggtitle(bquote(atop(bold(.(title)),atop(bold(.(subtit))))))+
    geom_line(size = I(1), aes(y=Confirmed_sum,colour="Confirmed"))+
    geom_line(aes(y=Recovered_sum,colour="Recovered"))+
    geom_line(aes(y=Exposed_sum,colour="Incubation"))+
    geom_line(aes(y=Confirmed,colour="Infected"))+
    geom_line(aes(y=Vacinated,colour="Vacinated"))+
    scale_y_continuous(limits = c(0,population)) +
    ylab(label="Count")+
    xlab(label="Time (days)")+
    facet_grid(Model~.)+
    theme(legend.justification=c(1,0), legend.position=c(.825,0.75))+
    theme(legend.title=element_text(size=12,face="bold"),
          legend.background = element_rect(fill='#FFFFFF',
                                           size=0.5,linetype="solid"),
          legend.text=element_text(size=10),
          legend.key=element_rect(colour="#FFFFFF",
                                  fill='#C2C2C2',
                                  size=0.25,
                                  linetype="solid"))+
    scale_colour_manual("Compartments",
                        breaks=c("Susceptible","Confirmed","Recovered","Incubation","Infected","Vacinated"),
                        values=c("blue","red","green","black","orange","darkblue"))
  print(res)
  return(out.dt)
}

# beta is the number infection contacts per day.
# gamma is 1 over the duration of the infection.
# e is 1 over the incubation period

SEIR.model(250,.50,1/14,1/14,0.01,0.01,pep_vac = 100)

SEIR.model(100,.50,1/14,1/14,0.01,0.01,pep_vac = 1000, infect = 100000, exposed = 300000)

SEIR.model(100,1,1/14,1/14,0.04,0.01,pep_vac = 10000, infect = 100000, exposed = 300000)
