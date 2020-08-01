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

notIn <- c('Princess', 'Guam', 'Samoa', 'Islands','Rico')

datUS %>%
  filter(!grepl(paste(notIn, collapse="|"),Province_State)) %>%
  mutate(Existing = Confirmed - Deaths - Recovered) %>%
  arrange(Province_State) %>%
  ggplot(aes(y=Province_State,x=Confirmed)) +
  geom_line() +
  geom_point(shape = 1)


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

#  nomalized by state population 

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
  filter(Last_Update == Sys.Date()-1 |Last_Update == '2020-06-01' |Last_Update == '2020-05-01' |Last_Update == '2020-07-01',
         test_per_million >0) %>%
  ggplot(aes(x=conf_per_million,y=test_per_million,color=Province_State)) +
  geom_point() +
  geom_text(aes(x=conf_per_million,y=test_per_million,label=Province_State),nudge_x = 5, nudge_y = 75) +
  geom_abline(data = rateLines,aes(slope = slope,intercept = intercept),linetype = 2, color = 'grey') +
  facet_grid( .~Last_Update) +
  theme(legend.position = "none")

# Plots of various countries and states

plotDat('US','California')
plotDatPhase('US','California')
plotDat('US','New Jersey')
plotDatPhase('US','New Jersey')
plotDat('US','New York')
plotDatPhase('US','New York')
plotDat('US','Michigan')
plotDatPhase('US','Michigan')
plotDat('US','Ohio')
plotDatPhase('US','Ohio')
plotDat('US', 'Washington')
plotDatPhase('US','Washington')
plotDat('US', 'Illinois')
plotDatPhase('US','Illinois')
plotDat('US', 'Louisiana')
plotDatPhase('US', 'Louisiana')
plotDat('US', 'Georgia')
plotDatPhase('US','Georgia')
plotDat('US', 'Texas')
plotDatPhase('US','Texas')
plotDat('US', 'Colorado')
plotDatPhase('US','Colorado')
plotDat('US', 'Florida')
plotDatPhase('US', 'Florida')
plotDat('US', 'Alabama')
plotDatPhase('US', 'Alabama')
plotDat('US', 'Mississippi')
plotDatPhase('US', 'Mississippi')
plotDat('US', 'Indiana')
plotDatPhase('US', 'Indiana')
plotDat('US', 'Wisconsin')
plotDatPhase('US', 'Wisconsin')
plotDat('US', 'Arkansas')
plotDatPhase('US', 'Arkansas')
plotDat('US', 'Arizona')
plotDatPhase('US', 'Arizona')
plotDat('US', 'North Carolina')
plotDatPhase('US', 'North Carolina')
plotDat('US', 'South Carolina')
plotDatPhase('US', 'South Carolina')
# USA
plotDatContryUSA()

#China
plotDatContry2('China')

plotDat('China','Shanghai')
plotDatPhase('China','Shanghai')

plotDat('China','Beijing')
plotDatPhase('China','Beijing')

plotDat('Korea','')
plotDatPhase('Korea','')

plotDat('Iceland','')
plotDatPhase('Iceland','')

plotDatContry2('Sweden')
plotDatPhaseCountry('Sweden','')

plotDatContry2('Canada')
plotDatPhaseCountry('Canada','')

plotDatContry2('Mexico')
plotDatPhaseCountry('Mexico','')

plotDatContry2('Brazil')
plotDatPhaseCountry('Brazil','')

plotDatContry2('Germany')
plotDatPhaseCountry('Germany','')

plotDatContry2('Spain')
plotDatPhaseCountry('Spain','')

plotDatContry2('France')
plotDatPhaseCountry('France','')

plotDatContry('New Zealand')
plotDatPhase('New Zealand','')

plotDatContry2('Philippines')
plotDatPhaseCountry('Philippines','')

# Hubei
plotDat('China','Hubei')


# Japan
plotDatContry2('Japan')
plotDatPhase('Japan','')
# Italy
plotDatContry2('Italy')
plotDatPhase('Italy','')

# Morocco
plotDatContry2('Morocco')

plotDatContry2('India')
plotDatPhase('India','')

# UK
plotDatContry2('United Kingdom')

# Austrilia
plotDatContry2('Australia')
plotDatPhase('Australia','')

# plot of top 10 states and provinces
# get the names of the 10 provinces

top_10 =
dat %>% 
  filter(grepl('China',`Country_Region`) ) %>%
  filter(Last_Update == max(Last_Update)) %>%
  top_n(10,Confirmed) %>%
  pull(`Province_State`)

dat%>% 
  filter(grepl('China',`Country_Region`) & `Province_State` %in% top_10)  %>%
  ggplot(aes(x = Last_Update, y= Confirmed, color = `Province_State`))+
  geom_line()+
  geom_point(size = I(3), shape = 1)+
  ylab(label="Count")+
  xlab(label="Date")+
  labs(title = "Coronavirus(2019-nCoV)",
       subtitle = "Top 10 Confirmed Cases By Province, China")

# top 10 US states
top_10_US_State =
  dat %>% 
  filter(`Country_Region` == 'US'& !grepl('County',`Province_State`)& !grepl('Princess',`Province_State`)& !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1))) %>%
  filter(Last_Update == max(Last_Update)) %>%
  #group_by(`Province_State`)  %>% 
  #filter(grepl('US',`Country/Region`) & !grepl('County',`Province/State`) )%>%
  top_n(10,Confirmed) %>%
  pull(`Province_State`)

dat %>% 
  filter(grepl('US',`Country_Region`) & `Province_State` %in% top_10_US_State & !grepl('County',`Province_State`) & !grepl('Princess',`Province_State`)& !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1)))  %>%
  filter(Last_Update > '2020-03-09') %>%
  ggplot(aes(x = Last_Update, y= Confirmed, color = `Province_State`))+
  geom_line()+
  geom_point(size = I(3), shape = 1)+
  geom_dl(aes(label = Province_State), method = list(dl.trans(x = x -1,y=y+.25), "last.points", cex = 0.8)) +
  ylab(label="Count")+
  xlab(label="Date")+
  labs(title = "Coronavirus(2019-nCoV)",
       subtitle = "Top 10 Confirmed Cases By State, United States")

dat %>% 
  filter(grepl('US',`Country_Region`) & `Province_State` %in% top_10_US_State & !grepl('County',`Province_State`) & !grepl('Princess',`Province_State`)& !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1)))  %>%
  filter(Last_Update > '2020-03-09') %>%
  ggplot(aes(x = Last_Update, y= Existing, color = `Province_State`))+
  geom_line()+
  geom_point(size = I(3), shape = 1)+
  geom_dl(aes(label = Province_State), method = list(dl.trans(x = x -1,y=y+.25), "last.points", cex = 0.8)) +
  ylab(label="Count")+
  xlab(label="Date")+
  labs(title = "Coronavirus(2019-nCoV)",
       subtitle = "Top 10 Existing Cases By State, United States")

# top 10 US states
top_10_US_Deaths =
  dat %>% 
  filter(`Country_Region` == 'US'& !grepl('County',`Province_State`)& !grepl('Princess',`Province_State`)& !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1))) %>%
  filter(Last_Update == max(Last_Update)) %>%
  #group_by(`Province_State`)  %>% 
  #filter(grepl('US',`Country/Region`) & !grepl('County',`Province/State`) )%>%
  top_n(10,Deaths) %>%
  pull(`Province_State`)

dat %>% 
  filter(grepl('US',`Country_Region`) & `Province_State` %in% top_10_US_Deaths & !grepl('County',`Province_State`) & !grepl('Princess',`Province_State`)& !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1)))  %>%
  filter(Last_Update > '2020-03-16') %>%
  ggplot(aes(x = Last_Update, y= Deaths, color = `Province_State`))+
  geom_line()+
  geom_point(size = I(3), shape = 1)+
  geom_dl(aes(label = Province_State), method = list(dl.trans(x = x -1,y=y+.25), "last.points", cex = 0.8)) +
  ylab(label="Count")+
  xlab(label="Date")+
  labs(title = "Coronavirus(2019-nCoV)",
       subtitle = "Top 10 Deaths Cases By State, United States")

# Shanghai and Beijing only

dat %>% 
  filter(grepl('China',`Country_Region`) & `Province_State` %in% c('Shanghai','Beijing'))  %>%
  group_by(`Province_State`) %>% 
  ggplot(aes(x = Last_Update, y= Confirmed, color = `Province_State`))+
  geom_line()+
  geom_point(size = I(3), shape = 1)+
  ylab(label="Count")+
  xlab(label="Date")+
  labs(title = "Wuhan Coronavirus(2019-nCoV)",
       subtitle = "Confirmed Cases Shanghai and Beijing")

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

#USA Brazil

rbind(
  dat %>%
    filter(Country_Region == 'Brazil',
           Last_Update > '2020-04-01') %>%
    group_by(Last_Update) %>%
    summarise_if(is.numeric,sum) %>%
    select(Last_Update,Confirmed,Deaths,Recovered,Existing) %>%
    mutate(Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
           Rate_Deaths = Deaths - lag(Deaths,default = 0),
           Country_Region = 'Brazil'),
  
  
  dat %>%
    filter(`Country_Region` == 'US' & !grepl('County',`Province_State`) &
             !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1)) &
             Last_Update > '2020-03-09') %>%
    group_by( Last_Update) %>%
    summarise_if(is.numeric,sum) %>%
    select(Last_Update,Confirmed,Deaths,Recovered,Existing) %>%
    mutate(Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
           Rate_Deaths = Deaths - lag(Deaths,default = 0),
           Country_Region = 'USA')
) %>%
  
  ggplot(aes(x = Last_Update))+
  geom_line(aes(y=Confirmed, color = "Infected"))+
  geom_line(aes(y=Existing, color = "Existing"))+
  geom_line(aes(y=Recovered, color = "Recovered"))+
  geom_line(aes(y=Deaths, color = "Deaths"))+
  geom_point(size = I(3), shape = 1, aes(y=Confirmed, color = "Infected"))+
  geom_point(size = I(3), shape = 1,aes(y=Existing, color = "Existing"))+
  geom_point(size = I(3), shape = 1,aes(y=Recovered, color = "Recovered"))+
  geom_point(size = I(3), shape = 1,aes(y=Deaths, color = "Deaths"))+
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
                      breaks=c("Infected","Existing","Recovered","Deaths"),
                      values=c("green","red","blue","black"))+
  labs(title = "Coronavirus(2019-nCoV)",
       subtitle = paste("United States Of America",sep =" "))

# Look only at deaths

dat %>%
  filter(`Country_Region` == 'US' & !grepl('County',`Province_State`) &
           !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1)) &
           Last_Update > '2020-03-09') %>%
  group_by( Last_Update) %>%
  summarise_if(is.numeric,sum) %>%
  select(Last_Update,Confirmed,Deaths,Recovered,Existing) %>%
  mutate(Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
         Rate_Deaths = Deaths - lag(Deaths,default = 0),
         Country_Region = 'USA') %>%
  
  ggplot(aes(x = Last_Update))+
  #geom_line(aes(y=Confirmed, color = "Infected"))+
  #geom_line(aes(y=Existing, color = "Existing"))+
  #geom_line(aes(y=Recovered, color = "Recovered"))+
  geom_line(aes(y=Deaths, color = "Deaths"))+
  #geom_point(size = I(3), shape = 1, aes(y=Confirmed, color = "Infected"))+
  #geom_point(size = I(3), shape = 1,aes(y=Existing, color = "Existing"))+
  #geom_point(size = I(3), shape = 1,aes(y=Recovered, color = "Recovered"))+
  geom_point(size = I(3), shape = 1,aes(y=Deaths, color = "Deaths"))+
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
                      breaks=c("Infected","Existing","Recovered","Deaths"),
                      values=c("green","red","blue","black"))+
  labs(title = "Coronavirus(2019-nCoV)",
       subtitle = paste("United States Of America",sep =" "))

rbind(
  dat %>%
    filter(Country_Region == 'Brazil',
           Last_Update > '2020-04-01') %>%
    group_by(Last_Update) %>%
    summarise_if(is.numeric,sum) %>%
    select(Last_Update,Confirmed,Deaths,Recovered,Existing) %>%
    mutate(Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
           Rate_Deaths = Deaths - lag(Deaths,default = 0),
           Country_Region = 'Brazil'),
  
  
  dat %>%
    filter(`Country_Region` == 'US' & !grepl('County',`Province_State`) &
             !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1)) &
             Last_Update > '2020-03-09') %>%
    group_by( Last_Update) %>%
    summarise_if(is.numeric,sum) %>%
    select(Last_Update,Confirmed,Deaths,Recovered,Existing) %>%
    mutate(Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
           Rate_Deaths = Deaths - lag(Deaths,default = 0),
           Country_Region = 'USA')
) %>%
  
  ggplot(aes(x = Last_Update))+
  geom_line(aes(y=Rate_Confirmed, color = "Infections_per_day"))+
  geom_line(aes(y=Rate_Deaths, color = "Deaths_per_day"))+
  geom_point(size = I(3), shape = 1, aes(y=Rate_Confirmed, color = "Infections_per_day"))+
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
                      breaks=c("Infections_per_day","Existing","Recovered","Deaths_per_day"),
                      values=c("green","red","blue","black"))+
  labs(title = "Coronavirus(2019-nCoV)",
       subtitle = paste("United States Of America",sep =" "))


# plot top 10 country's confirmed .
top_20_Country =
  dat %>% 
  filter(Last_Update == max(Last_Update)) %>%
  filter(!grepl('US',`Country_Region`) & !grepl('Other',`Country_Region`)  |
           `Country_Region` == 'US' & !grepl('County',`Province_State`) & !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1))) %>%
  group_by(`Country_Region`) %>%
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered),
            Existing = sum(Existing)) %>%
  top_n(20,Confirmed) %>%
  pull(`Country_Region`) 

dat %>% 
  filter(Country_Region %in% top_20_Country  & !grepl('County',`Province_State`) & 
           !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1)))  %>%
  filter(Last_Update > '2020-03-15') %>%
  group_by(`Country_Region`, Last_Update) %>%
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered),
            Existing = sum(Existing)) %>%
  ggplot(aes(x = Last_Update, color = Country_Region))+
  geom_line(aes(y= Confirmed)) +
  geom_point(size = I(3), shape = 1,aes(y= Confirmed))+
  geom_dl(aes(label = Country_Region,y=Confirmed), method = list(dl.trans(x = x -1,y=y+.25), "last.points", cex = 0.8)) +
  ylab(label="Count")+
  xlab(label="Date")+
  labs(title = "Coronavirus(2019-nCoV)",
       subtitle = "Top 10 Confirmed Cases ")

# plot top 10 existing cases
top_10_Country_Existing =
  dat %>% 
  filter(Last_Update == max(Last_Update)) %>%
  filter(!grepl('US',`Country_Region`) & !grepl('Other',`Country_Region`)  |
           `Country_Region` == 'US' & !grepl('County',`Province_State`) & !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1))) %>%
  group_by(`Country_Region`) %>%
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered),
            Existing = sum(Existing)) %>%
  top_n(10,Existing) %>%
  pull(`Country_Region`) 

dat %>% 
  filter(Country_Region %in% top_10_Country_Existing & !grepl('County',`Province_State`) & 
           !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1)))  %>%
  filter(Last_Update > '2020-03-15') %>%
  group_by(`Country_Region`, Last_Update) %>%
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered),
            Existing = sum(Existing)) %>%
  ggplot(aes(x = Last_Update, color = Country_Region))+
  geom_line(aes(y= Existing)) +
  geom_point(size = I(2), shape = 1,aes(y= Existing))+
  geom_dl(aes(label = Country_Region, y=Existing), method = list(dl.trans(x = x -1,y=y+.25), "last.points", cex = 0.8)) +
  ylab(label="Count")+
  xlab(label="Date")+
  labs(title = "Coronavirus(2019-nCoV)",
       subtitle = "Top 10 Existing Cases ")

### 
# Plot total case Conavirus vs. flu.

datCVirFlu <-
  dat %>% 
  group_by(`Last_Update`) %>% 
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered),
            Existing = sum(Existing)) %>%
  mutate(numDays = as.duration(lag(Last_Update,1)%--%Last_Update)) %>%
  mutate(numDays = ifelse(is.na(numDays),0,numDays/86400)) %>%
  mutate(numDays = cumsum(numDays))


datFluAlla <-
datFluAll %>%
  select(Confirmed = cSumPositive,numDays) %>%
  mutate(type = rep("Flu (A and B) ",times = nrow(datFluAll))) 
  
datCVirFlu %>% 
  select(Confirmed,numDays) %>%
  mutate(type = rep("Coronavirus",times = nrow(datCVirFlu))) %>%
  rbind(datFluAlla) %>%
  ggplot(aes(x = numDays,y=Confirmed,color=type))+
  geom_line()+
  geom_point(size = I(3), shape = 1)+
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
  labs(title = "Coronavirus(2019-nCoV)",
       subtitle = "Global")

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


######### SEIR model to actual data

## Model 1 returns a plot of the cumlative model results to match the John Hopkin's University data.
## Model 2 will take the John Hopkin's data, get a cases per day.

SEIR.model.incubation.pop.cumsum.US <- function(t, b, g, c ,
                                                population = 100000, infect = 100, exposed = 1000,
                                                country = 'US', province = 'New York' ,
                                                start_day = '2020-01-22'){
  
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
  
  out.dt[,Suspected := S*population]
  out.dt[,Infeceted := I*population]
  out.dt[,Recovered := R*population]
  out.dt[,Exposed := E*population]
  out.dt[,Population := Suspected + Infeceted + Recovered + Exposed]
  
  
  datAct <-
    dat %>% 
    filter(grepl(country,`Country_Region`) & grepl(province,`Province_State`))  %>%
    #filter(Country_Region == 'US' & Province_State == 'Michigan')  %>%
    group_by(`Province_State`) %>% 
    filter(Last_Update > start_day) %>%
    mutate(Date = ymd(Last_Update)) %>%
    arrange(Date) %>%
    mutate(numDays = as.numeric(c(diff(Date),1))) %>%
    as.data.table() %>%
    mutate(numDays = cumsum(numDays)) %>%
    select(Confirmed,Recovered,numDays) %>%
    as.data.table()
  
  datAct =  
    datAct %>%
    mutate(type = rep("Coronavirus_Data",times = nrow(datAct)),
           Suspected = rep(0,times = nrow(datAct)),
           Exposed = rep(0,times = nrow(datAct)),
           Population = rep(population,times = nrow(datAct)),
           Confirmed_sum = Confirmed,
           Recovered_sum = Recovered,
           Exposed_sum = Exposed) %>% 
    as.data.table()
  
  out.dt  =  
    out.dt %>%
    mutate(Confirmed_sum = Population - Suspected - Exposed) %>%
    mutate(type = rep('Model',times = nrow(out.dt)),
           Recovered_sum = Recovered,
           Exposed_sum = Exposed) %>%
    select(numDays = time, Suspected, Confirmed = Infeceted  ,Recovered, Exposed, Population, type, Confirmed_sum, Recovered_sum,Exposed_sum ) %>%
    rbind(datAct)
  
  print(tail(out.dt))
  
  title <- paste("Coronavirus(2019-nCoV) SEIR Model: Basic vs. Actual Data",country,province,sep=" ")
  subtit <- bquote(list(beta==.(parameters[1]),~gamma==.(round(parameters[2],3)),~eta==.(round(parameters[3],3))))
  
  res<-ggplot(out.dt,aes(x=numDays))+
    ggtitle(bquote(atop(bold(.(title)),atop(bold(.(subtit))))))+
    geom_line(size = I(1), aes(y=Confirmed_sum,colour="Confirmed"))+
    geom_line(aes(y=Recovered_sum,colour="Recovered"))+
    geom_line(aes(y=Exposed_sum,colour="Incubation"))+
    geom_line(aes(y=Confirmed,colour="Infected"))+
    ylab(label="Count")+
    xlab(label="Time (days)")+
    facet_grid(type~.)+
    theme(legend.justification=c(1,0), legend.position=c(.125,0.75))+
    theme(legend.title=element_text(size=12,face="bold"),
          legend.background = element_rect(fill='#FFFFFF',
                                           size=0.5,linetype="solid"),
          legend.text=element_text(size=10),
          legend.key=element_rect(colour="#FFFFFF",
                                  fill='#C2C2C2',
                                  size=0.25,
                                  linetype="solid"))+
    scale_colour_manual("Compartments",
                        breaks=c("Susceptible","Confirmed","Recovered","Incubation","Infected"),
                        values=c("blue","red","green","black","orange"))
  print(res)
  return(out.dt)
}

# beta is the number infection contacts per day.
# gamma is 1 over the duration of the infection.
# e is 1 over the incubation period

SEIR.model.incubation.pop.cumsum.US(100,.50,1/14,1/14,infect = 5000, exposed = 900,population = 400000,country = 'US',province = 'New York', start_day = '2020-03-10' )
SEIR.model.incubation.pop.cumsum.US(100,2.5,1/14,1/14,population = 75000,country = 'China',province = 'Hubei')
SEIR.model.incubation.pop.cumsum.US(120,.30,1/14,1/14,infect = 150,
                                    exposed = 350,population = 80000,country = 'US',province = 'Michigan', start_day = '2020-03-10' )

# fft of rate of increase
rateConf =
dat %>%
  filter(`Country_Region` == 'US' & !grepl('County',`Province_State`) &
           !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1)) &
           Last_Update > '2020-03-09') %>%
  group_by( Last_Update) %>%
  summarise_if(is.numeric,sum) %>%
  select(Last_Update,Confirmed,Deaths,Recovered,Existing) %>%
  mutate(Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
         Rate_Deaths = Deaths - lag(Deaths,default = 0),
         Country_Region = 'USA') %>%
  pull(Rate_Confirmed) 

plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
  
  # TODO: why this scaling is necessary?
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}

plot.frequency.spectrum(fft(rateConf), xlimits=c(0,50))


spectrum(fft(rateConf))

# Plot the i-th harmonic
# Xk: the frequencies computed by the FFt
#  i: which harmonic
# ts: the sampling time points
# acq.freq: the acquisition rate
plot.harmonic <- function(Xk, i, ts, acq.freq, color="red") {
  Xk.h <- rep(0,length(Xk))
  Xk.h[i+1] <- Xk[i+1] # i-th harmonic
  harmonic.trajectory <- get.trajectory(Xk.h, ts, acq.freq=acq.freq)
  points(ts, harmonic.trajectory, type="l", col=color)
}


# Chi Square Test for Recovered cases and deaths

datChiSq <-
  dat %>% 
  filter(Country_Region %in% top_20_Country & !grepl('County',`Province_State`) & 
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

notIn <- c('Princess', 'Guam', 'Samoa', 'Islands','Rico')

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

###############################
# Case increase rate
##############################
# subset to USA
notIn <- c('Princess', 'Guam', 'Samoa', 'Islands','Rico')

datRateTop15 <-
  dat %>%
  filter(Country_Region == 'US' & !grepl(paste(notIn, collapse="|"), Province_State)& !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1))) %>%
  select(Province_State,Deaths,Recovered,Confirmed,Last_Update) %>%
  group_by(Province_State) %>%
  mutate(Confirmed = zoo::rollapply(Confirmed,4,mean,align='right',fill=0),
         Deaths = zoo::rollapply(Deaths,4,mean,align='right',fill=0),
        Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
        Rate_Deaths = Deaths - lag(Deaths,default = 0),
        Accel_Confirmed = Rate_Confirmed - lag(Rate_Confirmed,default = 0),
        Accel_Deaths = Rate_Deaths - lag(Rate_Deaths,default = 0)) %>%
  #mutate(Rate_Confirmed = zoo::rollapply(Rate_Confirmed,4,mean,align='right',fill=0),
         #Accel_Confirmed = zoo::rollapply(Accel_Confirmed,4,mean,align='right',fill=0),
         #Rate_Deaths = zoo::rollapply(Rate_Deaths,4,mean,align='right',fill=0),
         #Accel_Deaths = zoo::rollapply(Accel_Deaths,4,mean,align='right',fill=0)) %>%
  filter(Last_Update == max(Last_Update)) %>%
  inner_join(select(usStatePopulation,`Geographic Area`,'2019'), by = c('Province_State' = 'Geographic Area')) %>%
  mutate(`2019` = as.numeric(gsub(",","",`2019`,fixed=TRUE))) %>%
  rename(Pop = `2019`) %>%
  ungroup() %>%
  mutate(Rate_Confirmed_per_million = (Rate_Confirmed/Pop)*1000000,
         Accel_Confirmed_per_million = (Accel_Confirmed/Pop)*1000000,
         Death_per_Conf = Confirmed/Deaths) %>%
  filter(Rate_Confirmed > 0 & Accel_Confirmed >0) %>%
  arrange(desc(Rate_Confirmed_per_million))  %>%
  top_n(15,Rate_Confirmed_per_million) %>%
  pull(Province_State)

dat %>%
  filter(Country_Region == 'US' & !grepl(paste(notIn, collapse="|"), Province_State)& !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1))) %>%
  select(Province_State,Deaths,Recovered,Confirmed,Last_Update) %>%
  filter(Province_State %in% datRateTop15) %>%
  group_by(Province_State) %>%
  mutate(Confirmed = zoo::rollapply(Confirmed,4,mean,align='right',fill=0),
         Deaths = zoo::rollapply(Deaths,4,mean,align='right',fill=0),
         Existing = Confirmed - Deaths - Recovered,
         Rate_Existing = Existing - lag(Existing,default = 0),
         Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
         Rate_Deaths = Deaths - lag(Deaths,default = 0)) %>%
  filter(Last_Update > '2020-06-01') %>%
  inner_join(select(usStatePopulation,`Geographic Area`,'2019'), by = c('Province_State' = 'Geographic Area')) %>%
  mutate(`2019` = as.numeric(gsub(",","",`2019`,fixed=TRUE))) %>%
  rename(Pop = `2019`) %>%
  ungroup() %>%
  mutate(Rate_Confirmed_per_million = (Rate_Confirmed/Pop)*1000000,
         Death_per_Conf = Confirmed/Deaths) %>%
    ggplot(aes(x = Last_Update,color = Province_State)) +
       geom_line(aes(y=Rate_Confirmed_per_million))+
       geom_point(aes(y=Rate_Confirmed_per_million)) +
       geom_dl(aes(label = Province_State, y=Rate_Confirmed_per_million), method = list(dl.trans(x = x -1,y=y+.25), "last.points", cex = 0.8)) 
  


###
# rolling 4 day average filted for rate and acceleration > 0
###

dat %>%
  filter(Country_Region == 'US' & !grepl(paste(notIn, collapse="|"), Province_State)& !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1))) %>%
  select(Province_State,Deaths,Recovered,Confirmed,Last_Update) %>%
  group_by(Province_State) %>%
  mutate(
    Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
    Rate_Deaths = Deaths - lag(Deaths,default = 0),
    Accel_Confirmed = Rate_Confirmed - lag(Rate_Confirmed,default = 0),
    Accel_Deaths = Rate_Deaths - lag(Rate_Deaths,default = 0)) %>%
  mutate(Rate_Confirmed = zoo::rollapply(Rate_Confirmed,4,mean,align='right',fill=0),
         Accel_Confirmed = zoo::rollapply(Accel_Confirmed,4,mean,align='right',fill=0),
         Rate_Deaths = zoo::rollapply(Rate_Deaths,4,mean,align='right',fill=0),
         Accel_Deaths = zoo::rollapply(Accel_Deaths,4,mean,align='right',fill=0)) %>%
  filter(Last_Update == max(Last_Update)) %>%
  inner_join(select(usStatePopulation,`Geographic Area`,'2019'), by = c('Province_State' = 'Geographic Area')) %>%
  mutate(`2019` = as.numeric(gsub(",","",`2019`,fixed=TRUE))) %>%
  rename(Pop = `2019`) %>%
  ungroup() %>%
  mutate(Rate_Confirmed_per_million = (Rate_Confirmed/Pop)*1000000,
         Accel_Confirmed_per_million = (Accel_Confirmed/Pop)*1000000,
         Death_per_Conf = Confirmed/Deaths) %>%
  filter(Rate_Confirmed > 0 & Accel_Confirmed >0) %>%
  arrange(desc(Rate_Confirmed_per_million))  
  
datAccelTop15 <-
  dat %>%
  filter(Country_Region == 'US' & !grepl(paste(notIn, collapse="|"), Province_State)& !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1))) %>%
  select(Province_State,Deaths,Recovered,Confirmed,Last_Update) %>%
  group_by(Province_State) %>%
  mutate(
    Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
    Rate_Deaths = Deaths - lag(Deaths,default = 0),
    Accel_Confirmed = Rate_Confirmed - lag(Rate_Confirmed,default = 0),
    Accel_Deaths = Rate_Deaths - lag(Rate_Deaths,default = 0)) %>%
  mutate(Rate_Confirmed = zoo::rollapply(Rate_Confirmed,4,mean,align='right',fill=0),
         Accel_Confirmed = zoo::rollapply(Accel_Confirmed,4,mean,align='right',fill=0),
         Rate_Deaths = zoo::rollapply(Rate_Deaths,4,mean,align='right',fill=0),
         Accel_Deaths = zoo::rollapply(Accel_Deaths,4,mean,align='right',fill=0)) %>%
  filter(Last_Update == max(Last_Update)) %>%
  inner_join(select(usStatePopulation,`Geographic Area`,'2019'), by = c('Province_State' = 'Geographic Area')) %>%
  mutate(`2019` = as.numeric(gsub(",","",`2019`,fixed=TRUE))) %>%
  rename(Pop = `2019`) %>%
  ungroup() %>%
  mutate(Rate_Confirmed_per_million = (Rate_Confirmed/Pop)*1000000,
         Accel_Confirmed_per_million = (Accel_Confirmed/Pop)*1000000,
         Death_per_Conf = Confirmed/Deaths) %>%
  filter(Rate_Confirmed > 0 & Accel_Confirmed >0) %>%
  arrange(desc(Accel_Confirmed_per_million))  %>%
  top_n(15,Accel_Confirmed_per_million) %>%
  pull(Province_State)

datAccelTop15

dat %>%
  filter(Country_Region == 'US' & !grepl(paste(notIn, collapse="|"), Province_State)& !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1))) %>%
  select(Province_State,Deaths,Recovered,Confirmed,Last_Update) %>%
  mutate(
    Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
    Rate_Deaths = Deaths - lag(Deaths,default = 0),
    Accel_Confirmed = Rate_Confirmed - lag(Rate_Confirmed,default = 0),
    Accel_Deaths = Rate_Deaths - lag(Rate_Deaths,default = 0)) %>%
  mutate(Rate_Confirmed = zoo::rollapply(Rate_Confirmed,4,mean,align='right',fill=0),
         Accel_Confirmed = zoo::rollapply(Accel_Confirmed,4,mean,align='right',fill=0),
         Rate_Deaths = zoo::rollapply(Rate_Deaths,4,mean,align='right',fill=0),
         Accel_Deaths = zoo::rollapply(Accel_Deaths,4,mean,align='right',fill=0)) %>%
  inner_join(select(usStatePopulation,`Geographic Area`,'2019'), by = c('Province_State' = 'Geographic Area')) %>%
  mutate(`2019` = as.numeric(gsub(",","",`2019`,fixed=TRUE))) %>%
  rename(Pop = `2019`) %>%
  ungroup() %>%
  filter(Last_Update > '2020-06-01') %>%
  ggplot(aes(x = Last_Update,color = Province_State)) +
  geom_line(aes(y=Rate_Confirmed))+
  geom_point(aes(y=Rate_Confirmed)) +
  geom_dl(aes(label = Province_State, y= Rate_Confirmed), method = list(dl.trans(x = x -1,y=y+.25), "last.points", cex = 0.8)) +
  scale_y_continuous(limits = c(1200,13000))

dat %>%
  filter(Country_Region == 'US' & !grepl(paste(notIn, collapse="|"), Province_State)& !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1))) %>%
  select(Province_State,Deaths,Recovered,Confirmed,Last_Update) %>%
  filter(Province_State %in% datAccelTop15) %>%
  mutate(
    Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
    Rate_Deaths = Deaths - lag(Deaths,default = 0),
    Accel_Confirmed = Rate_Confirmed - lag(Rate_Confirmed,default = 0),
    Accel_Deaths = Rate_Deaths - lag(Rate_Deaths,default = 0)) %>%
  mutate(Rate_Confirmed = zoo::rollapply(Rate_Confirmed,4,mean,align='right',fill=0),
         Accel_Confirmed = zoo::rollapply(Accel_Confirmed,4,mean,align='right',fill=0),
         Rate_Deaths = zoo::rollapply(Rate_Deaths,4,mean,align='right',fill=0),
         Accel_Deaths = zoo::rollapply(Accel_Deaths,4,mean,align='right',fill=0)) %>%
  inner_join(select(usStatePopulation,`Geographic Area`,'2019'), by = c('Province_State' = 'Geographic Area')) %>%
  mutate(`2019` = as.numeric(gsub(",","",`2019`,fixed=TRUE))) %>%
  rename(Pop = `2019`) %>%
  ungroup() %>%
  mutate(Rate_Confirmed_per_million = (Rate_Confirmed/Pop)*1000000,
         Accel_Confirmed_per_million = (Accel_Confirmed/Pop)*1000000,
         Death_per_Conf = Confirmed/Deaths) %>%
  filter(Last_Update > '2020-06-01') %>%
  ggplot(aes(x = Last_Update,color = Province_State)) +
  geom_line(aes(y=Rate_Confirmed_per_million))+
  geom_point(aes(y=Rate_Confirmed_per_million)) +
    #geom_line(aes(y=Accel_Confirmed_per_million))+
    #geom_point(aes(y=Accel_Confirmed_per_million)) +
  geom_dl(aes(label = Province_State, y= Rate_Confirmed_per_million), method = list(dl.trans(x = x -1,y=y+.25), "last.points", cex = 0.8)) 

dat %>%
  filter(Country_Region == 'US' & !grepl(paste(notIn, collapse="|"), Province_State)& !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1))) %>%
  select(Province_State,Deaths,Recovered,Confirmed,Last_Update) %>%
  group_by(Province_State) %>%
  mutate(Confirmed = zoo::rollapply(Confirmed,4,mean,align='right',fill=0),
         Recovered = zoo::rollapply(Recovered,4,mean,align='right',fill=0),
         Deaths = zoo::rollapply(Deaths,4,mean,align='right',fill=0)) %>%
  mutate(#Existing = Confirmed - Deaths - Recovered,
    #Rate_Existing = Existing - lag(Existing,default = 0),
    Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
    Rate_Deaths = Deaths - lag(Deaths,default = 0),
    #Accel_Existing = Rate_Existing - lag(Rate_Existing,default = 0),
    Accel_Confirmed = Rate_Confirmed - lag(Rate_Confirmed,default = 0),
    Accel_Deaths = Rate_Deaths - lag(Rate_Deaths,default = 0)) %>%
  ungroup() %>%
  filter(Last_Update == max(Last_Update) ) %>%
  arrange(Accel_Confirmed) %>%
  top_n(-15,Accel_Confirmed)

datRateBottom15 <-
  dat %>%
  filter(Country_Region == 'US' & !grepl(paste(notIn, collapse="|"), Province_State)& !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1))) %>%
  select(Province_State,Deaths,Recovered,Confirmed,Last_Update) %>%
  group_by(Province_State) %>%
  mutate(
    Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
    Rate_Deaths = Deaths - lag(Deaths,default = 0),
    Accel_Confirmed = Rate_Confirmed - lag(Rate_Confirmed,default = 0),
    Accel_Deaths = Rate_Deaths - lag(Rate_Deaths,default = 0)) %>%
  mutate(Rate_Confirmed = zoo::rollapply(Rate_Confirmed,4,mean,align='right',fill=0),
         Accel_Confirmed = zoo::rollapply(Accel_Confirmed,4,mean,align='right',fill=0),
         Rate_Deaths = zoo::rollapply(Rate_Deaths,4,mean,align='right',fill=0),
         Accel_Deaths = zoo::rollapply(Accel_Deaths,4,mean,align='right',fill=0)) %>%
  filter(Last_Update == max(Last_Update)) %>%
  inner_join(select(usStatePopulation,`Geographic Area`,'2019'), by = c('Province_State' = 'Geographic Area')) %>%
  mutate(`2019` = as.numeric(gsub(",","",`2019`,fixed=TRUE))) %>%
  rename(Pop = `2019`) %>%
  ungroup() %>%
  mutate(Rate_Confirmed_per_million = (Rate_Confirmed/Pop)*1000000,
         Accel_Confirmed_per_million = (Accel_Confirmed/Pop)*1000000,
         Death_per_Conf = Confirmed/Deaths) %>%
  #filter(Rate_Confirmed < 0 & Accel_Confirmed < 0) %>%
  arrange(Rate_Confirmed_per_million) %>%
  top_n(-15,Rate_Confirmed_per_million) %>%
  pull(Province_State)

datRateBottom15


dat %>%
  filter(Country_Region == 'US' & !grepl(paste(notIn, collapse="|"), Province_State)& !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1))) %>%
  select(Province_State,Deaths,Recovered,Confirmed,Last_Update) %>%
  filter(Province_State %in% datRateBottom15) %>%
  group_by(Province_State) %>%
  mutate(Existing = Confirmed - Deaths - Recovered,
         Rate_Existing = Existing - lag(Existing,default = 0),
         Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
         Rate_Deaths = Deaths - lag(Deaths,default = 0)) %>%
  filter(Last_Update > '2020-06-10') %>%
  filter(Rate_Confirmed < 2000 & Rate_Confirmed > -2000) %>%
  ggplot(aes(x = Last_Update,color = Province_State)) +
  geom_line(aes(y=Rate_Confirmed))+
  geom_point(aes(y=Rate_Confirmed)) +
  geom_dl(aes(label = Province_State, y = Rate_Confirmed), method = list(dl.trans(x = x -1,y=y+.25), "last.points", cex = 0.8)) 
 

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

# population of the Michigan is approximately 9996000 Probablity for 50% chance of contact

probContact <- data.table(x= seq(from = 1, to = 100, by = 1))
probContact[ , prob_conf := bday(n = max(x), pr = probMichian$total_confirmed/9996000)]
probContact[ , prob_exist := bday(n = max(x), pr = probMichian$total_existing/9996000)]
probContact <- melt(probContact, id.vars = c('x'))
probContact[value >= 0.48 & value <= 0.51,max(x)]

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




#Region Plots
  dat<-data.table()
  ##load data
  
  files<-list.files("R/2019-coronavirus-dataset-01212020-01262020/csse_covid_19_daily_reports/",full.names=TRUE)
  #print(files)
  for(i in 1:(length(files)-1)){
    data.temp<-fread(files[i],header =TRUE) 
    
    if (ncol(data.temp) == 12) {
      data.temp = select(data.temp, `Province_State`, `Country_Region`,`Admin2`,`Last_Update`, `Confirmed`, `Deaths`, `Recovered`, Lat, Long_ )
    }else{
      data.temp = data.table(Province_State = 'xx', `Country_Region` = 'xx',`Admin2`= 'xx',`Last_Update`= '1900-01-01', `Confirmed` = NA, `Deaths`= NA, `Recovered` = NA , Lat = NA, Long_ =NA)
    }
    
    #names(data.temp) <- c('Province_State', 'Country_Region','Last_Update', 'Confirmed', 'Deaths', 'Recovered' )
    
    data.temp <-
      data.temp %>%
      mutate(Last_Update = parse_date_time(Last_Update,c("mdy_HM","ymd_HMS")))%>%
      mutate(Last_Update = max(Last_Update),
             Confirmed = ifelse(is.na(Confirmed),0,Confirmed),
             Deaths = ifelse(is.na(Deaths),0,Deaths),
             Recovered = ifelse(is.na(Recovered),0,Recovered)) %>%
      mutate(Existing = Confirmed - Deaths - Recovered) %>%
      
      as.data.table()
    dat<-rbind(dat,data.temp)
    dat<-
      dat %>%
      filter(Country_Region == 'US') 
  }
  
datMidWest =
    dat %>%
    filter(Province_State == 'Illinois' |Province_State == 'Michigan' | Province_State == 'Indiana' | Province_State == 'Ohio' | Province_State == 'Wisconsin' | Province_State == 'Minnesota') %>%
    rename(c('Province_State' = 'Province_State' , 'Country_Region' = 'Country_Region','Admin2' = 'Admin2','lat'= 'Lat','lng'='Long_' )) %>%
    mutate(Existing = Confirmed - Deaths - Recovered)  

datMidWest %>% 
  group_by(`Admin2`)  %>% 
  filter(`Last_Update` == max(`Last_Update`)) %>%
  select(`Admin2`,lng,lat,Confirmed,Existing) %>%
  leaflet() %>%
  addTiles() %>%
  addCircles(lng = ~lng, lat = ~lat, weight = 1,
             radius = ~(Confirmed) * 2, 
             popup = ~`Admin2`,
             label = ~Confirmed) %>%
  addCircles(lng = ~lng, lat = ~lat, weight = 1,
             radius = ~(Existing) * 1, 
             color = 'red') %>%
  addPopups(lng=-80, lat=45, paste(sep = "<br/>", "Scroll over the circle to see the confirmed count","Click the circle to see the provice name"),
            options = popupOptions(closeButton = FALSE)) %>%
  addPopups(lng=-80, lat=47, paste(sep = "<br/>", "<b>Coronavirus(2019-nCoV</b>","Confirmed Infection Counts Illinois",max(dat$`Last Update`)),
            options = popupOptions(closeButton = FALSE))

datSouth =
  dat %>%
  filter(Province_State == 'Florida' |Province_State == 'Georgia' | Province_State == 'Alabama' | Province_State == 'Mississippi' | 
           Province_State == 'South Carolina' | Province_State == 'North Carolina'| Province_State == 'Louisiana' 
         | Province_State == 'Texas' | Province_State == 'Arkansas'| Province_State == 'Tennessee' |
           Province_State == 'Arizona' | Province_State == 'New Mexico' | Province_State == 'Oklahoma') %>%
  rename(c('Province_State' = 'Province_State' , 'Country_Region' = 'Country_Region','Admin2' = 'Admin2','lat'= 'Lat','lng'='Long_' )) %>%
  mutate(Existing = Confirmed - Deaths - Recovered)  

datSouth %>% 
  group_by(`Admin2`)  %>% 
  filter(`Last_Update` == max(`Last_Update`)) %>%
  select(`Admin2`,lng,lat,Confirmed,Existing) %>%
  leaflet() %>%
  addTiles() %>%
  addCircles(lng = ~lng, lat = ~lat, weight = 1,
             radius = ~(Confirmed) * 3, 
             popup = ~`Admin2`,
             label = ~Confirmed) %>%
  addCircles(lng = ~lng, lat = ~lat, weight = 1,
             radius = ~(Existing) * 3, 
             color = 'red') 

# United states map
dat %>%
  rename(c('Province_State' = 'Province_State' , 'Country_Region' = 'Country_Region','Admin2' = 'Admin2','lat'= 'Lat','lng'='Long_' )) %>%
  mutate(Existing = Confirmed - Deaths - Recovered) %>%
  group_by(`Admin2`)  %>% 
  filter(`Last_Update` == max(`Last_Update`)) %>%
  select(`Admin2`,lng,lat,Confirmed,Existing) %>%
  leaflet() %>%
  addTiles() %>%
  addCircles(lng = ~lng, lat = ~lat, weight = 1,
             radius = ~(Confirmed) * 0, 
             popup = ~`Admin2`,
             label = ~Confirmed) %>%
  addCircles(lng = ~lng, lat = ~lat, weight = 1,
             radius = ~(Existing) * 2, 
             color = 'red')

 
dat %>%
  filter(Province_State == 'Michigan' ) %>%
  rename(c('Province_State' = 'Province_State' , 'Country_Region' = 'Country_Region','Admin2' = 'Admin2','lat'= 'Lat','lng'='Long_' )) %>%
  mutate(Existing = Confirmed - Deaths - Recovered)  %>%
  group_by(`Admin2`)  %>% 
  filter(`Last_Update` == max(`Last_Update`)) %>%
  select(`Admin2`,lng,lat,Confirmed,Existing) %>%
  leaflet() %>%
  addTiles() %>%
  addCircles(lng = ~lng, lat = ~lat, weight = 1,
             radius = ~(Confirmed) * 2, 
             popup = ~`Admin2`,
             label = ~Confirmed) %>%
  addCircles(lng = ~lng, lat = ~lat, weight = 1,
             radius = ~(Existing) * 2, 
             color = 'red') 

#########################################
# Look at number of test vs. infections / infection rate
#########################################

# Load data to pull number of test data

files<-list.files("R/2019-coronavirus-dataset-01212020-01262020/csse_covid_19_daily_reports_us/",full.names=TRUE)
datUS <- data.table()

for(i in 1:(length(files)-1)){
  
  data.temp<-fread(files[i],header =TRUE)
  
  #data.temp = select(data.temp, `Province_State`, `Country_Region`,`Last_Update`, `Confirmed`, `Deaths`, `Recovered` )
  #names(data.temp) <- c('Province_State', 'Country_Region','Last_Update', 'Confirmed', 'Deaths', 'Recovered' )
  
  data.temp <-
    data.temp %>%
    mutate(Last_Update = parse_date_time(Last_Update,c("mdy_HM","ymd_HMS")))%>%
    mutate(Last_Update = max(Last_Update),
           Confirmed = ifelse(is.na(Confirmed),0,Confirmed),
           Deaths = ifelse(is.na(Deaths),0,Deaths),
           Recovered = ifelse(is.na(Recovered),0,Recovered)) %>%
    mutate(Country_Region = ifelse(grepl('Mainland China',Country_Region),'China',Country_Region)) %>%
    filter(!is.na(Last_Update)) %>%
    filter(!grepl('Guam',Province_State) | !grepl('Princess',Province_State) | !grepl('Island',Province_State)) %>%
    filter(Province_State != 'Recovered' ) %>%
    as.data.table()
  datUS<-rbind(datUS,data.temp)
}


# Number of test for each positive by state

datUS %>%
  filter(Province_State %in% datAccelTop15) %>%
  mutate(Existing = Confirmed - Deaths - Recovered) %>%
  group_by(Last_Update,Province_State) %>%
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered),
            Existing = sum(Existing),
            Test = sum(People_Tested,na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Last_Update = date(Last_Update)) %>%
  group_by(Last_Update,Province_State) %>%
  mutate(Rate_Existing = Existing - lag(Existing,default = 0),
         Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
         Rate_Deaths = Deaths - lag(Deaths,default = 0),
         Rate_Test = Test - lag(Test,default = 0)) %>% 
  mutate(Accel_Existing = Rate_Existing - lag(Rate_Existing,default = 0),
         Accel_Confirmed = Rate_Confirmed - lag(Rate_Confirmed,default = 0),
         Accel_Deaths = Rate_Deaths - lag(Rate_Deaths,default = 0),
         Accel_Test = Rate_Test - lag(Rate_Test,default = 0),
         Test_Confirmed = Rate_Test/Rate_Confirmed) %>%
  filter(Last_Update > '2020-05-01') %>%
  ggplot(aes(x=Last_Update, color = Province_State)) +
           geom_line(aes(y=Test_Confirmed)) + 
           geom_point(aes(y=Test_Confirmed),shape = 1) +
  geom_dl(aes(label = Province_State, y = Test_Confirmed), method = list(dl.trans(x = x -1,y=y+.25), "last.points", cex = 0.8)) +
  theme(legend.position = "none")

#  Cumlative test vs. cumlative confirmed cases
datUS %>%
  mutate(Existing = Confirmed - Deaths - Recovered) %>%
  group_by(Last_Update) %>%
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered),
            Existing = sum(Existing),
            Test = sum(People_Tested,na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Last_Update = date(Last_Update)) %>%
  group_by(Last_Update) %>%
  mutate(Rate_Existing = Existing - lag(Existing,default = 0),
         Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
         Rate_Deaths = Deaths - lag(Deaths,default = 0),
         Rate_Test = Test - lag(Test,default = 0)) %>% 
  mutate(Accel_Existing = Rate_Existing - lag(Rate_Existing,default = 0),
         Accel_Confirmed = Rate_Confirmed - lag(Rate_Confirmed,default = 0),
         Accel_Deaths = Rate_Deaths - lag(Rate_Deaths,default = 0),
         Accel_Test = Rate_Test - lag(Rate_Test,default = 0)) %>%
  filter(Last_Update > '2020-05-01') %>%
  ggplot(aes(x=Last_Update)) +
  geom_point(aes(y=Confirmed))+
  geom_point(color = 'red',aes(y=Test)) +
  xlab(label = "Date") +
  ylab(label = "Cumlative Sum (Confirmed | Test)")


notIn <- c('Princess', 'Guam', 'Samoa', 'Islands','Rico')

datUS %>%
  filter(!grepl(paste(notIn, collapse="|"),Province_State)) %>%
  mutate(Last_Update = date(Last_Update)) %>%
  group_by(Province_State) %>%
  mutate(Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
         Rate_Test = People_Tested - lag(People_Tested,default = 0)) %>%
  select(Province_State,Last_Update,Confirmed,People_Tested,Rate_Confirmed,Rate_Test) %>%
  inner_join(select(usStatePopulation,`Geographic Area`,'2019'), by = c('Province_State' = 'Geographic Area')) %>%
  mutate(`2019` = as.numeric(gsub(",","",`2019`,fixed=TRUE))) %>%
  rename(Pop = `2019`) %>%
  ungroup() %>%
  mutate(conf_per_million = (Rate_Confirmed/Pop)*1000000,
         test_per_million = (Rate_Test/Pop)*1000000) %>%
  filter(Last_Update > '2020-05-01')
###########################################################################################################


#  nomalized by state population 

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
  filter(Last_Update == Sys.Date()-1 |Last_Update == '2020-06-01' |Last_Update == '2020-05-01' |Last_Update == '2020-07-01',
         test_per_million >0) %>%
  ggplot(aes(x=conf_per_million,y=test_per_million,color=Province_State)) +
  geom_point() +
  geom_text(aes(x=conf_per_million,y=test_per_million,label=Province_State),nudge_x = 5, nudge_y = 75) +
  geom_abline(data = rateLines,aes(slope = slope,intercept = intercept),linetype = 2, color = 'grey') +
  facet_grid( .~Last_Update) +
  theme(legend.position = "none")

#####
# Plot by all states confirmed cases
#####

datUS %>%
  filter(!grepl(paste(notIn, collapse="|"),Province_State)) %>%
  mutate(Existing = Confirmed - Deaths - Recovered) %>%
  arrange(Province_State) %>%
  ggplot(aes(y=Province_State,x=Confirmed)) +
  geom_line() +
  geom_point(shape = 1)


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



