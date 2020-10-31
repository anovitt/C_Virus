library(data.table)
library(ggplot2)
library(lubridate)
library(leaflet)
library(tidyverse)
library(deSolve)
library(directlabels)

# Load data

source("R/2019-coronavirus-dataset-01212020-01262020/cvirusDataLoad.R")

#####
# Plot by all states confirmed cases
#####

notIn <- c('Princess', 'Guam', 'Samoa', 'Islands','Rico','Evacuee','Recovered')

datUS %>%
  filter(!grepl(paste(notIn, collapse="|"),Province_State)) %>%
  mutate(Existing = Confirmed - Deaths - Recovered) %>%
  arrange(Province_State) %>%
  ggplot(aes(y=Province_State,x=Confirmed)) +
  geom_line() +
  geom_point(shape = 1)

datUS %>%
  filter(!grepl(paste(notIn, collapse="|"),Province_State)) %>%
  mutate(Existing = Confirmed - Deaths - Recovered) %>%
  arrange(Province_State) %>%
  ggplot(aes(y=Province_State,x=Deaths)) +
  geom_line() +
  geom_point(shape = 1)

#  normalized by state population 

datUS %>%
  filter(!grepl(paste(notIn, collapse="|"),Province_State)) %>%
  inner_join(select(usStatePopulation,`Geographic Area`,'2019'), by = c('Province_State' = 'Geographic Area')) %>%
  mutate(`2019` = as.numeric(gsub(",","",`2019`,fixed=TRUE))) %>%
  rename(Pop = `2019`) %>%
  ungroup() %>%
  mutate(conf_per_million = (Confirmed/Pop)*1000000,
         deaths_per_million = (Deaths/Pop)*1000000) %>%
  ggplot(aes(y=Province_State,x=conf_per_million)) +
  geom_line() +
  geom_point(shape = 1)

top_125_Country =
  dat %>% 
  filter(Last_Update == max(Last_Update)) %>%
  filter(!grepl('US',`Country_Region`) & !grepl('Other',`Country_Region`)  |
           `Country_Region` == 'US' & !grepl('County',`Province_State`) & !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1))) %>%
  group_by(`Country_Region`) %>%
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered),
            Existing = sum(Existing)) %>%
  top_n(125,Confirmed) %>%
  pull(`Country_Region`) 

dat %>% 
  filter(Country_Region %in% top_125_Country  & !grepl('County',`Province_State`) & 
           !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1)))  %>%
  filter(Last_Update > '2020-03-15') %>%
  group_by(`Country_Region`, Last_Update) %>%
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered),
            Existing = sum(Existing)) %>%
  arrange(Country_Region) %>%
  ggplot(aes(y=Country_Region,x=Confirmed)) +
  geom_line() +
  geom_point(shape = 1) +
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90)) 

# US testing rate normalized vs. confirmed cases

rateLines <- data.table(slope = c(2,5,10,20,50,100),intercept = c(0,0,0,0,0,0))

#  Confirmed per million vs. test per million, rolling seven day average.
datUS %>%
  filter(!grepl(paste(notIn, collapse="|"),Province_State)) %>%
  mutate(Last_Update = date(Last_Update)) %>%
  group_by(Province_State) %>%
  mutate(Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
         Rate_Test = People_Tested - lag(People_Tested,default = 0)) %>%
  mutate(Rate_Confirmed = zoo::rollapply(Rate_Confirmed,5,mean,align='right',fill=0),
         Rate_Test = zoo::rollapply(Rate_Test,5,mean,align='right',fill=0)) %>%
  select(Province_State,Last_Update,Confirmed,People_Tested,Rate_Confirmed,Rate_Test) %>%
  inner_join(select(usStatePopulation,`Geographic Area`,'2019'), by = c('Province_State' = 'Geographic Area')) %>%
  mutate(`2019` = as.numeric(gsub(",","",`2019`,fixed=TRUE))) %>%
  rename(Pop = `2019`) %>%
  ungroup() %>%
  mutate(conf_per_million = (Rate_Confirmed/Pop)*1000000,
         test_per_million = (Rate_Test/Pop)*1000000) %>%
  filter(Last_Update == Sys.Date()-1 |Last_Update == '2020-06-01' |Last_Update == '2020-05-01' |
           Last_Update == '2020-07-01' | Last_Update =='2020-08-01'| Last_Update =='2020-09-01'| Last_Update =='2020-10-01',
         test_per_million >0 & test_per_million <40000,
         conf_per_million >= 0) %>%
  ggplot(aes(x=conf_per_million,y=test_per_million,color=Province_State)) +
  geom_point() +
  geom_text(aes(x=conf_per_million,y=test_per_million,label=Province_State),nudge_x = 5, nudge_y = 75) +
  geom_abline(data = rateLines,aes(slope = slope,intercept = intercept),linetype = 2, color = 'grey') +
  scale_y_continuous(limits = c(0,10000)) +
  facet_grid( .~Last_Update) +
  theme(legend.position = "none")


datUS %>%
  filter(!grepl(paste(notIn, collapse="|"),Province_State)) %>%
  mutate(Last_Update = date(Last_Update)) %>%
  group_by(Province_State) %>%
  mutate(Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
         Rate_Test = People_Tested - lag(People_Tested,default = 0)) %>%
  mutate(Rate_Confirmed = zoo::rollapply(Rate_Confirmed,5,mean,align='right',fill=0),
         Rate_Test = zoo::rollapply(Rate_Test,5,mean,align='right',fill=0)) %>%
  select(Province_State,Last_Update,Confirmed,People_Tested,Rate_Confirmed,Rate_Test) %>%
  inner_join(select(usStatePopulation,`Geographic Area`,'2019'), by = c('Province_State' = 'Geographic Area')) %>%
  mutate(`2019` = as.numeric(gsub(",","",`2019`,fixed=TRUE))) %>%
  rename(Pop = `2019`) %>%
  ungroup() %>%
  mutate(conf_per_million = (Rate_Confirmed/Pop)*1000000,
         test_per_million = (Rate_Test/Pop)*1000000) %>%
  filter(Last_Update == Sys.Date()-1,
         test_per_million >0 & test_per_million < 40000,
         conf_per_million >= 0) %>%
  ggplot(aes(x=conf_per_million,y=test_per_million,color=Province_State)) +
  geom_point() +
  geom_text(aes(x=conf_per_million,y=test_per_million,label=Province_State),nudge_x = 5, nudge_y = 75) +
  #scale_y_continuous(limits = c(0,10000)) +
  geom_abline(data = rateLines,aes(slope = slope,intercept = intercept),linetype = 2, color = 'grey') +
  facet_grid( .~Last_Update) +
  theme(legend.position = "none")


In <- c('Texas', 'Florida', 'Alabama', 'Georgia','California','Louisiana','Mississippi')

datUS %>%
  filter(grepl(paste(In, collapse="|"),Province_State)) %>%
  mutate(Last_Update = date(Last_Update)) %>%
  group_by(Province_State) %>%
  mutate(Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
         Rate_Test = People_Tested - lag(People_Tested,default = 0)) %>%
  mutate(Rate_Confirmed = zoo::rollapply(Rate_Confirmed,5,mean,align='right',fill=0),
         Rate_Test = zoo::rollapply(Rate_Test,5,mean,align='right',fill=0)) %>%
  select(Province_State,Last_Update,Confirmed,People_Tested,Rate_Confirmed,Rate_Test) %>%
  inner_join(select(usStatePopulation,`Geographic Area`,'2019'), by = c('Province_State' = 'Geographic Area')) %>%
  mutate(`2019` = as.numeric(gsub(",","",`2019`,fixed=TRUE))) %>%
  rename(Pop = `2019`) %>%
  ungroup() %>%
  mutate(conf_per_million = (Rate_Confirmed/Pop)*1000000,
         test_per_million = (Rate_Test/Pop)*1000000) %>%
  filter(Last_Update == Sys.Date()-1 |Last_Update == '2020-06-01' |Last_Update == '2020-05-01' |
           Last_Update == '2020-07-01' | Last_Update =='2020-08-01'| Last_Update =='2020-09-01'| 
           Last_Update =='2020-10-01',
         test_per_million >0) %>%
  ggplot(aes(x=conf_per_million,y=test_per_million,color=Province_State)) +
  geom_point() +
  geom_text(aes(x=conf_per_million,y=test_per_million,label=Province_State),nudge_x = 5, nudge_y = 75) +
  geom_abline(data = rateLines,aes(slope = slope,intercept = intercept),linetype = 2, color = 'grey') +
  facet_grid( .~Last_Update) +
  theme(legend.position = "none")

### Infection rate plots

dat %>%
    filter(`Country_Region` == 'US' & !grepl('County',`Province_State`) &
             !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1)) &
             Last_Update > '2020-03-09') %>%
    group_by( Last_Update) %>%
    summarise_if(is.numeric,sum) %>%
    select(Last_Update,Confirmed,Deaths,Recovered,Existing) %>%
    mutate(Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
           Country_Region = 'USA')%>%
  mutate(Roll_Rate_Confirmed = zoo::rollmean(Rate_Confirmed,7,mean,align='right',fill=Rate_Confirmed)) %>%
  ggplot(aes(x = Last_Update))+
  geom_line(aes(y=Rate_Confirmed, color = "Infections_per_day"))+
  geom_line(aes(y=Roll_Rate_Confirmed, color = "Rolling_Infrections_per_day"))+
  geom_point(size = I(3), shape = 1, aes(y=Rate_Confirmed, color = "Infections_per_day"))+
  #geom_point(size = I(3), shape = 1,aes(y=Roll_Rate_Confirmed, color = "Rolling_Infrections_per_day"))+
  facet_grid(.~Country_Region) +
  ylab(label="Count")+
  xlab(label="Date")+
  theme(legend.justification=c(1,0), legend.position=c(0.25,0.75))+
  theme(legend.title=element_text(size=12,face="bold"),
        legend.background = element_rect(fill='#FFFFFF',
                                         size=0.5,linetype="solid"),
        legend.text=element_text(size=10),
        legend.key=element_rect(colour="#FFFFFF",
                                fill='#C2C2C2',
                                size=0.25,
                                linetype="solid"))+
  scale_colour_manual("Compartments",
                      breaks=c("Infections_per_day","Existing","Recovered","Rolling_Infrections_per_day"),
                      values=c("green","red","blue","black"))+
  labs(title = "Coronavirus(2019-nCoV)",
       subtitle = paste("United States Of America",sep =" "))
  
# Grouped by states

dat %>%
  filter(`Country_Region` == 'US' & !grepl('County',`Province_State`) &
           !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1)) &
           Last_Update > '2020-03-09') %>%
  filter(!grepl(paste(notIn, collapse="|"),Province_State)) %>%
  arrange(Province_State,Last_Update) %>%
  group_by( Province_State,Last_Update) %>%
  summarise_if(is.numeric,sum) %>%
  select(Last_Update,Confirmed,Deaths,Recovered,Existing, Province_State) %>%
  mutate(Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
         Country_Region = 'USA')%>%
  mutate(Roll_Rate_Confirmed = zoo::rollmean(Rate_Confirmed,7,mean,align='right',fill=Rate_Confirmed)) %>%
  ggplot(aes(x = Last_Update))+
  #geom_line(aes(y=Rate_Confirmed, color = Province_State))+
  geom_line(aes(y=Roll_Rate_Confirmed, color = Province_State))+
  #geom_point(size = I(3), shape = 1, aes(y=Rate_Confirmed, color = Province_State))+
  #geom_point(size = I(3), shape = 1,aes(y=Roll_Rate_Confirmed, color = "Rolling_Infrections_per_day"))+
  facet_wrap(vars(Province_State)) +
  ylab(label="Infection Rate")+
  xlab(label="Date")+
  theme(legend.position = "none") +
  labs(title = "Coronavirus(2019-nCoV)",
       subtitle = paste("United States Of America",sep =" "))

dat %>%
  filter(`Country_Region` == 'US' & !grepl('County',`Province_State`) &
           !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1)) &
           Last_Update > '2020-03-09') %>%
  filter(!grepl(paste(notIn, collapse="|"),Province_State)) %>%
  arrange(Province_State,Last_Update) %>%
  group_by( Province_State,Last_Update) %>%
  summarise_if(is.numeric,sum) %>%
  select(Last_Update,Confirmed,Deaths,Recovered,Existing, Province_State) %>%
  mutate(Rate_Deaths = Deaths - lag(Deaths,default = 0),
         Country_Region = 'USA')%>%
  mutate(Roll_Rate_Deaths = zoo::rollmean(Rate_Deaths,7,mean,align='right',fill=Rate_Deaths)) %>%
  ggplot(aes(x = Last_Update))+
  #geom_line(aes(y=Rate_Confirmed, color = Province_State))+
  geom_line(aes(y=Roll_Rate_Deaths, color = Province_State))+
  #geom_point(size = I(3), shape = 1, aes(y=Rate_Confirmed, color = Province_State))+
  #geom_point(size = I(3), shape = 1,aes(y=Roll_Rate_Confirmed, color = "Rolling_Infrections_per_day"))+
  facet_wrap(vars(Province_State)) +
  ylab(label="Infection Rate")+
  xlab(label="Date")+
  theme(legend.position = "none") +
  labs(title = "Coronavirus(2019-nCoV)",
       subtitle = paste("United States Of America",sep =" "))
# Normalized by state population
dat %>%
  filter(`Country_Region` == 'US' & !grepl('County',`Province_State`) &
           !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1)) &
           Last_Update > '2020-03-09') %>%
  filter(!grepl(paste(notIn, collapse="|"),Province_State)) %>%
  arrange(Province_State,Last_Update) %>%
  group_by( Province_State,Last_Update) %>%
  summarise_if(is.numeric,sum) %>%
  select(Last_Update,Confirmed,Deaths,Recovered,Existing, Province_State) %>%
  left_join(select(usStatePopulation,`Geographic Area`,'2019'), by = c('Province_State' = 'Geographic Area')) %>%
  mutate(`2019` = as.numeric(gsub(",","",`2019`,fixed=TRUE))) %>%
  rename(Pop = `2019`) %>%
  mutate(Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
         Country_Region = 'USA')%>%
  mutate(Roll_Rate_Confirmed = zoo::rollmean(Rate_Confirmed,7,mean,align='right',fill=Rate_Confirmed)) %>%
  ungroup() %>%
  mutate(Normalized_Roll_Rate_Confirmed = 1000000* Roll_Rate_Confirmed/Pop) %>%
  ggplot(aes(x = Last_Update))+
  #geom_line(aes(y=Rate_Confirmed, color = Province_State))+
  geom_line(aes(y=Normalized_Roll_Rate_Confirmed, color = Province_State))+
  #geom_point(size = I(3), shape = 1, aes(y=Rate_Confirmed, color = Province_State))+
  #geom_point(size = I(3), shape = 1,aes(y=Roll_Rate_Confirmed, color = "Rolling_Infections_per_day"))+
  facet_wrap(vars(Province_State)) +
  ylab(label="Normalize Rolling Avgerage Confirmed Cases Rate Per Day Per million")+
  xlab(label="Date")+
  theme(legend.position = "none") +
  labs(title = "Coronavirus(2019-nCoV)",
       subtitle = paste("United States Of America",sep =" "))


dat %>%
  filter(`Country_Region` == 'US' & !grepl('County',`Province_State`) &
           !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1)) &
           Last_Update > '2020-03-09') %>%
  filter(!grepl(paste(notIn, collapse="|"),Province_State)) %>%
  arrange(Province_State,Last_Update) %>%
  group_by( Province_State,Last_Update) %>%
  summarise_if(is.numeric,sum) %>%
  select(Last_Update,Confirmed,Deaths,Recovered,Existing, Province_State) %>%
  left_join(select(usStatePopulation,`Geographic Area`,'2019'), by = c('Province_State' = 'Geographic Area')) %>%
  mutate(`2019` = as.numeric(gsub(",","",`2019`,fixed=TRUE))) %>%
  rename(Pop = `2019`) %>%
  mutate(Rate_Deaths = Deaths - lag(Deaths,default = 0),
         Country_Region = 'USA')%>%
  mutate(Roll_Rate_Deaths = zoo::rollmean(Rate_Deaths,7,mean,align='right',fill=Rate_Deaths)) %>%
  ungroup() %>%
  mutate(Normalized_Roll_Rate_Deaths = 1000000* Roll_Rate_Deaths/Pop) %>%
  ggplot(aes(x = Last_Update))+
  #geom_line(aes(y=Rate_Confirmed, color = Province_State))+
  geom_line(aes(y=Normalized_Roll_Rate_Deaths, color = Province_State))+
  #geom_point(size = I(3), shape = 1, aes(y=Rate_Confirmed, color = Province_State))+
  #geom_point(size = I(3), shape = 1,aes(y=Roll_Rate_Confirmed, color = "Rolling_Infrections_per_day"))+
  facet_wrap(vars(Province_State)) +
  ylab(label="Normalize Rolling Avgerage Death Rate Per Day Per million")+
  xlab(label="Date")+
  theme(legend.position = "none") +
  labs(title = "Coronavirus(2019-nCoV)",
       subtitle = paste("United States Of America",sep =" "))

# USA Brazil India

rbind(
  dat %>%
        filter(`Country_Region` == 'US' & !grepl('County',`Province_State`) &
                 !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1)) &
                 Last_Update > '2020-03-09') %>%
        group_by( Last_Update) %>%
        summarise_if(is.numeric,sum) %>%
        select(Last_Update,Confirmed,Deaths,Recovered,Existing) %>%
        mutate(Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
               Country_Region = 'USA')%>%
        mutate(Roll_Rate_Confirmed = zoo::rollmean(Rate_Confirmed,7,mean,align='right',fill=Rate_Confirmed)),
  
  dat %>%
    filter(`Country_Region` == 'India'  &
             
             Last_Update > '2020-03-09') %>%
    group_by( Last_Update) %>%
    summarise_if(is.numeric,sum) %>%
    select(Last_Update,Confirmed,Deaths,Recovered,Existing) %>%
    mutate(Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
           Country_Region = 'India')%>%
    mutate(Roll_Rate_Confirmed = zoo::rollmean(Rate_Confirmed,7,mean,align='right',fill=Rate_Confirmed)),
  
  dat %>%
    filter(`Country_Region` == 'Brazil'  &
             
             Last_Update > '2020-03-09') %>%
    group_by( Last_Update) %>%
    summarise_if(is.numeric,sum) %>%
    select(Last_Update,Confirmed,Deaths,Recovered,Existing) %>%
    mutate(Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
           Country_Region = 'Brazil')%>%
    mutate(Roll_Rate_Confirmed = zoo::rollmean(Rate_Confirmed,7,mean,align='right',fill=Rate_Confirmed))
  
  ) %>%
  ggplot(aes(x = Last_Update))+
  geom_line(aes(y=Rate_Confirmed, color = "Infections_per_day"))+
  geom_line(aes(y=Roll_Rate_Confirmed, color = "Rolling_Infrections_per_day"))+
  geom_point(size = I(3), shape = 1, aes(y=Rate_Confirmed, color = "Infections_per_day"))+
  #geom_point(size = I(3), shape = 1,aes(y=Roll_Rate_Confirmed, color = "Rolling_Infrections_per_day"))+
  facet_grid(.~Country_Region) +
  ylab(label="Count")+
  xlab(label="Date")+
  theme(legend.justification=c(1,0), legend.position=c(0.25,0.75))+
  theme(legend.title=element_text(size=12,face="bold"),
        legend.background = element_rect(fill='#FFFFFF',
                                         size=0.5,linetype="solid"),
        legend.text=element_text(size=10),
        legend.key=element_rect(colour="#FFFFFF",
                                fill='#C2C2C2',
                                size=0.25,
                                linetype="solid"))+
  scale_colour_manual("Compartments",
                      breaks=c("Infections_per_day","Existing","Recovered","Rolling_Infrections_per_day"),
                      values=c("green","red","blue","black"))+
  facet_grid(.~Country_Region) +
  labs(title = "Coronavirus(2019-nCoV)",
       subtitle = paste("USA, India, Brazil",sep =" "))

# Death rate plot  
dat %>%
    filter(`Country_Region` == 'US' & !grepl('County',`Province_State`) &
             !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1)) &
             Last_Update > '2020-03-09') %>%
    group_by( Last_Update) %>%
    summarise_if(is.numeric,sum) %>%
    select(Last_Update,Confirmed,Deaths,Recovered,Existing) %>%
    mutate(Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
           Rate_Deaths = Deaths - lag(Deaths,default = 0),
           Country_Region = 'USA')%>%
  mutate(Roll_Rate_Deaths = zoo::rollmean(Rate_Deaths,7,mean,align='right',fill=Rate_Deaths)) %>%
    ggplot(aes(x = Last_Update))+
    #geom_line(aes(y=Rate_Confirmed, color = "Infections_per_day"))+
    geom_line(aes(y=Rate_Deaths, color = "Deaths_per_day"))+
    geom_line(aes(y=Roll_Rate_Deaths, color = "Roll_Deaths_per_day"))+
    #geom_point(size = I(3), shape = 1, aes(y=Rate_Confirmed, color = "Infections_per_day"))+
    geom_point(size = I(3), shape = 1,aes(y=Rate_Deaths, color = "Deaths_per_day"))+
    facet_grid(.~Country_Region) +
    ylab(label="Count")+
    xlab(label="Date")+
    theme(legend.justification=c(1,0), legend.position=c(0.25,0.75))+
    theme(legend.title=element_text(size=12,face="bold"),
          legend.background = element_rect(fill='#FFFFFF',
                                           size=0.5,linetype="solid"),
          legend.text=element_text(size=10),
          legend.key=element_rect(colour="#FFFFFF",
                                  fill='#C2C2C2',
                                  size=0.25,
                                  linetype="solid"))+
    scale_colour_manual("Compartments",
                        breaks=c("Infections_per_day","Roll_Deaths_per_day","Recovered","Deaths_per_day"),
                        values=c("green","red","blue","black"))+
    labs(title = "Coronavirus(2019-nCoV)",
         subtitle = paste("United States Of America",sep =" "))  


# Plots of various countries and states

plotDat('US','California')
plotDat('US','New Jersey')
plotDat('US','New York')
plotDat('US','Michigan')
plotDat('US','Ohio')
plotDat('US', 'Washington')
plotDat('US', 'Illinois')
plotDat('US', 'Louisiana')
plotDat('US', 'Georgia')
plotDat('US', 'Texas')
plotDat('US', 'Colorado')
plotDat('US', 'Florida')
plotDat('US', 'Alabama')
plotDat('US', 'Mississippi')
plotDat('US', 'Indiana')
plotDat('US', 'Wisconsin')
plotDat('US', 'Arkansas')
plotDat('US', 'Arizona')
plotDat('US', 'North Carolina')
plotDat('US', 'South Carolina')
plotDat('US', 'North Dakota')
plotDat('US', 'South Dakota')
plotDat('US', 'Indiana')
plotDat('US', 'Tennessee')
plotDat('US', 'Iowa')
plotDat('US', 'Nevada')
plotDat('US', 'Minnesota')

plotDatContryUSA()
# 
plotDatContry2('China')
plotDat('Korea','')
plotDat('Iceland','')
plotDatContry2('Sweden')
plotDatContry2('Canada')
plotDatContry2('Mexico')
plotDatContry2('Brazil')
plotDatContry2('Germany')
plotDatContry2('Spain')
plotDatContry2('France')
plotDatContry('New Zealand')
plotDatContry2('Philippines')
plotDatContry2('Japan')
plotDatContry2('Italy')
plotDatContry2('Morocco')
plotDatContry2('India')
plotDatContry2('United Kingdom')
plotDatContry2('Australia')

# 
plotDat('China','Hubei')
plotDat('China','Shanghai')
plotDat('China','Beijing')

# Phase plots
plotDatPhase('US','California')
plotDatPhase('US','New Jersey')
plotDatPhase('US','New York')
plotDatPhase('US','Michigan')
plotDatPhase('US','Ohio')
plotDatPhase('US','Washington')
plotDatPhase('US','Illinois')
plotDatPhase('US', 'Louisiana')
plotDatPhase('US','Georgia')
plotDatPhase('US','Texas')
plotDatPhase('US','Colorado')
plotDatPhase('US', 'Florida')
plotDatPhase('US', 'Alabama')
plotDatPhase('US', 'Mississippi')
plotDatPhase('US', 'Indiana')
plotDatPhase('US', 'Wisconsin')
plotDatPhase('US', 'Arkansas')
plotDatPhase('US', 'Arizona')
plotDatPhase('US', 'North Carolina')
plotDatPhase('US', 'South Carolina')
plotDatPhase('US', 'North Dakota')
plotDatPhase('US', 'South Dakota')

plotDatPhase('China','Shanghai')
plotDatPhase('China','Beijing')
plotDatPhase('Korea','')
plotDatPhase('Iceland','')
plotDatPhaseCountry('Sweden','')
plotDatPhaseCountry('Canada','')
plotDatPhaseCountry('Mexico','')
plotDatPhaseCountry('Brazil','')
plotDatPhaseCountry('Germany','')
plotDatPhaseCountry('Spain','')
plotDatPhaseCountry('France','')
plotDatPhase('New Zealand','')
plotDatPhaseCountry('Philippines','')
plotDatPhase('Japan','')
plotDatPhase('Italy','')
plotDatPhase('India','')
plotDatPhase('Australia','')

# plot of top 10 states and provinces
# get the names of the 10 province

# Shanghai and Beijing only

dat %>% 
  filter(grepl('China',`Country_Region`) & `Province_State` %in% c('Shanghai','Beijing'))  %>%
  group_by(`Province_State`) %>% 
  ggplot(aes(x = Last_Update, y= Existing, color = `Province_State`))+
  geom_line()+
  geom_point(size = I(3), shape = 1)+
  ylab(label="Count")+
  xlab(label="Date")+
  labs(title = "Wuhan Coronavirus(2019-nCoV)",
       subtitle = "Confirmed Cases Shanghai and Beijing")


######
# lat lon for the provinces

provinceCoord <- data.table(Province = c('Shanghai',
'Beijing','Guangdong','Hubei','Tianjin','Chongqing',
'Liaoning','Sichuan','Jiangsu','Guizhou','Heilongjiang',
'Jilin','Zhejiang','Yunnan','Shanxi','Shandong',
'Fujian','Hunan','Gansu','Hebei','Guangxi',
'Xinjiang','Hainan','Anhui','Inner Mongolia',
'Qinghai','Ningxia','Tibet','Shaanxi','Henan','Jiangxi'
),
lng = c(121.458056, 116.388869,113.25,114.266667,117.176667,106.552778,123.432778,
        104.066667,118.777778,106.716667,126.65,125.322778,120.161419,102.718333,
        112.560278,116.997222,119.306111,112.966667,103.839868,114.478611,108.316667,
        87.630506,110.341667,117.280833,111.652222,101.75739,106.273056,91.1,108.928611,
        113.648611, 115.883333),
lat = c(31.222222, 39.928819,23.116667, 30.583333,39.142222,29.562778,41.792222,30.666667,
        32.061667,26.583333,45.75,43.88,30.29365,25.038889,37.869444,36.668333,26.061389,
        28.2,36.057006,38.041389,22.816667,43.807347,20.045833,31.863889, 40.810556,
        36.625541,38.468056,29.65,34.258333,34.757778,28.683333))

dat %>% 
  filter(grepl('China',`Country_Region`))  %>%
  group_by(`Province_State`)  %>% 
  filter(`Last_Update` == max(`Last_Update`)) %>%
  #left_join(provinceCoord, by = c('Province_State' = 'Province')) %>%
  select(`Province_State`,lng,lat,Confirmed) %>%
  leaflet() %>%
  addTiles() %>%
  addCircles(lng = ~lng, lat = ~lat, weight = 1,
             radius = ~log(Confirmed) * 20000, 
             popup = ~`Province_State`,
             label = ~Confirmed) %>%
  addPopups(lng=121.4, lat=50, paste(sep = "<br/>", "Scroll over the circle to see the confirmed count","Click the circle to see the provice name"),
            options = popupOptions(closeButton = FALSE)) %>%
  addPopups(lng=90, lat=50, paste(sep = "<br/>", "<b>Coronavirus(2019-nCoV</b>","Confirmed Infection Counts By Province",max(dat$`Last Update`)),
            options = popupOptions(closeButton = FALSE))


## existing cases map

dat %>% 
  filter(grepl('China',`Country_Region`))  %>%
  group_by(`Province_State`)  %>% 
  filter(`Last_Update` == max(`Last_Update`)) %>%
  #left_join(provinceCoord, by = c('Province_State' = 'Province')) %>%
  select(`Province_State`,lng,lat,Existing) %>%
  leaflet() %>%
  addTiles() %>%
  addCircles(lng = ~lng, lat = ~lat, weight = 1,
             radius = ~log(Existing) * 20000, 
             popup = ~`Province_State`,
             label = ~Existing) %>%
  addPopups(lng=121.4, lat=50, paste(sep = "<br/>", "Scroll over the circle to see the existing count","Click the circle to see the provice name"),
            options = popupOptions(closeButton = FALSE)) %>%
  addPopups(lng=90, lat=50, paste(sep = "<br/>", "<b>Coronavirus(2019-nCoV</b>","Existing Infection Counts By Province",max(dat$`Last Update`)),
            options = popupOptions(closeButton = FALSE))

# Chi Square Test for Recovered cases and deaths

datChiSq <-
  dat %>% 
  filter(Country_Region %in% top_125_Country & !grepl('County',`Province_State`) & 
           !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1)))  %>%
  filter(Last_Update == max(Last_Update)) %>%
  group_by(`Country_Region`) %>%
  summarise(Deaths = sum(Deaths),
            Recovered = sum(Recovered),
            Confirmed = sum(Confirmed)) %>%
  as.matrix() %>%
  t()

M <- as.table(rbind(as.numeric(datChiSq[2,]), as.numeric(datChiSq[3,]) , as.numeric(datChiSq[4,])))
dimnames(M) <- list(Outcome = c("Deaths", "Recovered","Confirmed"),
                    Country = datChiSq[1,])
(Xsq <- chisq.test(M))  # Prints test summary
Xsq$observed   # observed counts (same as M)
round(Xsq$expected,0) # expected counts under the null
(Xsq$observed - round(Xsq$expected,0))
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals
round(Xsq$observed[1,]/Xsq$observed[3,],3)

# Chi square test for US states

datChiSq <-
dat %>%
  filter(Country_Region == 'US' & Last_Update == max(Last_Update) & !grepl(paste(notIn, collapse="|"), Province_State)) %>%
  select(Province_State,Deaths,Recovered,Confirmed) %>%
  as.matrix() %>%
  t()

M <- as.table(rbind(as.numeric(datChiSq[2,]), as.numeric(datChiSq[3,]) , as.numeric(datChiSq[4,])))
dimnames(M) <- list(Outcome = c("Deaths", "Recovered","Confirmed"),
                    Country = datChiSq[1,])
(Xsq <- chisq.test(M))  # Prints test summary
Xsq$observed   # observed counts (same as M)
round(Xsq$expected,0) # expected counts under the null
(Xsq$observed - round(Xsq$expected,0))
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals
round(Xsq$observed[1,]/Xsq$observed[3,],3)

as.data.table(t(M)) %>%
  pivot_wider(names_from = 'Outcome', values_from = 'N') %>%
  mutate(D_Rate = Deaths / Confirmed) %>%
  ggplot(aes(x = Country, y = D_Rate)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))


#########################
# Estimate the number of people in a room to have 50% of a person confirmed
#########################

bday<-function(n,pr=1/365,stp = 1){
  pepBuy <- seq(from = 1, to = n, by = stp)
  #print(pepBuy)
  prob<-function(n){1-dbinom(1,1:n,pr)}
  bday<-vector()
  for(i in pepBuy){
    out<-prob(i)
    bday<-c(bday,1-prod(out))
  }
  #plot(pepBuy,bday)
  return(bday)
}

probUsa =
dat %>% 
  filter(`Country_Region` == 'US' & !grepl('County',`Province_State`) & !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1))) %>%
    filter(Last_Update == max(Last_Update)) %>%
  mutate(Existing = Confirmed - Deaths - Recovered) %>%
  summarise(total_confirmed = sum(Confirmed),
            total_existing = sum(Existing))
# population of the USA is approximately 327.2 million

probContact <- data.table(x= seq(from = 1, to = 100, by = 1))
probContact[ , prob_conf := bday(n = max(x), pr = probUsa$total_confirmed/327200000)]
probContact[ , prob_exist := bday(n = max(x), pr = probUsa$total_existing/327200000)]
probContact <- melt(probContact, id.vars = c('x'))
probContact[value >= 0.48 & value <= 0.53,max(x)]

ggplot(data = probContact, aes(x=x,y=value,color = variable))+
  geom_line() +
  geom_hline(yintercept = 0.5)+
  geom_vline(xintercept = c(probContact[value >= 0.499 & value <= 0.501,max(x)],probContact[value >= 0.499 & value <= 0.501,min(x)]))

## Michigan data

probMichian =
  dat %>% 
  filter(`Country_Region` == 'US' & !grepl('County',`Province_State`) & !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1))) %>%
  filter(Last_Update == max(Last_Update)) %>%
  filter(Province_State == 'Michigan') %>%
  summarise(total_confirmed = sum(Confirmed),
            total_existing = sum(Existing))

# population of the Michigan is approximately 9996000 Probability for 50% chance of contact

probContact <- data.table(x= seq(from = 1, to = 100, by = 1))
probContact[ , prob_conf := bday(n = max(x), pr = probMichian$total_confirmed/9996000)]
probContact[ , prob_exist := bday(n = max(x), pr = probMichian$total_existing/9996000)]
probContact <- melt(probContact, id.vars = c('x'))
probContact[value >= 0.47 & value <= 0.52,max(x)]

ggplot(data = probContact, aes(x=x,y=value,color = variable))+
  geom_line() +
  geom_hline(yintercept = 0.5)+
  geom_vline(xintercept = c(probContact[value >= 0.499 & value <= 0.501,max(x)],probContact[value >= 0.499 & value <= 0.501,min(x)]))

probMichian2 =
  dat %>% 
  filter(`Country_Region` == 'US' & !grepl('County',`Province_State`) & !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1))) %>%
  filter(Province_State == 'Michigan') %>%
  group_by(Last_Update ) %>%
  summarise(total_confirmed = sum(Confirmed),
            total_existing = sum(Existing))

