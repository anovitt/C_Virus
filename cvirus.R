library(data.table)
library(ggplot2)
library(lubridate)
library(ggmap)
library(bit64)
library(maps)
library(tmap)
library(leaflet)
library(sf)
library(RColorBrewer)
library(dplyr)
library(tidyverse)
library(deSolve)

# Load data

files<-list.files("R/2019-coronavirus-dataset-01212020-01262020/csse_covid_19_daily_reports/",full.names=TRUE)
print(files)

files[1]

dat<-data.table()
##load data
for(i in 1:(length(files)-1)){
  i <- 1
  data.temp<-fread(files[i],header =TRUE)
  data.temp <- select(data.temp, `Province/State`, `Country/Region`,     `Last Update`, `Confirmed`, `Deaths`, `Recovered` )
  dat<-rbind(dat,data.temp)
}

dat =
dat %>%
  mutate(Recovered = ifelse(is.na(Recovered), 0, Recovered),
         Deaths = ifelse(is.na(Deaths), 0, Deaths),
         Confirmed = ifelse(is.na(Confirmed), 0, Confirmed),
         `Last Update` = mdy_hm(`Last Update`)) %>%
  mutate(Existing = Confirmed - Recovered - Deaths) %>%
  group_by(`Country/Region`,`Province/State`,`Last Update`) %>%
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered),
            Existing = sum(Existing)) %>%
  mutate(Existing)
  

datConf <- fread("R/2019-coronavirus-dataset-01212020-01262020/time_series_19-covid-Confirmed.csv")

datLatLonMap <- select(datConf,c('Province/State','Country/Region','Lat','Long'))


# using the John Hopkin's table data 
#datConf <- fread("R/2019-coronavirus-dataset-01212020-01262020/time_series_19-covid-Confirmed.csv") # this file is no longer maintained
#datRec <- fread("R/2019-coronavirus-dataset-01212020-01262020/time_series_19-covid-Recovered.csv")
#datDea <- fread("R/2019-coronavirus-dataset-01212020-01262020/time_series_19-covid-Deaths.csv")

datConf <- fread("R/2019-coronavirus-dataset-01212020-01262020/time_series_covid19_confirmed_global.csv")
datRec <- fread("R/2019-coronavirus-dataset-01212020-01262020/time_series_19-covid-Recovered.csv")
datDea <- fread("R/2019-coronavirus-dataset-01212020-01262020/time_series_covid19_deaths_global.csv")

datConf <- melt(datConf, id.vars = c('Province/State','Country/Region','Lat','Long'))
datRec <- melt(datRec, id.vars = c('Province/State','Country/Region','Lat','Long'))
datDea <- melt(datDea, id.vars = c('Province/State','Country/Region','Lat','Long'))

names(datConf) <- c('Province/State','Country/Region','Lat','Long','Last Update','Confirmed')
names(datRec) <- c('Province/State','Country/Region','Lat','Long','Last Update','Recovered')
names(datDea) <- c('Province/State','Country/Region','Lat','Long','Last Update','Death')

#dat <- as.data.table(cbind(datConf,select(datRec,Recovered),select(datDea,Death)))

dat <- as.data.table(cbind(datConf, select(datDea,Death)))

dat =
dat %>%
  left_join(select(datRec,'Province/State','Country/Region','Last Update','Recovered'), 
            by = c('Province/State' = 'Province/State','Country/Region'= 'Country/Region','Last Update'='Last Update')) %>%
  mutate(Recovered = ifelse(is.na(Recovered), 0, Recovered)) %>%
  as.data.table()


#dat[, Existing := Confirmed - Recovered - Death]
#dat[, Existing := Confirmed  - Death]

datLatLonMap <- select(dat,c('Province/State','Country/Region','Lat','Long'))

dat <- select(dat,setdiff(names(dat),c('First confirmed date in country (Est.)','Lat','Long')))

datFlu <- fread("R/2019-coronavirus-dataset-01212020-01262020/flu.csv")
str(datFlu)

# set NA to 0
dat[is.na(dat)] <- 0
dat[,`Last Update`:= as.character(`Last Update`)]
str(dat)

dat %>%
  filter(`Country/Region` == 'US' & grepl('MI', `Province/State`))

# delete the duplicated entrys by date and time of entry. 

######## clean the flu data
# convert the week number to a date for ploting

datFlu[ , WeekNum := as.integer(str_sub(Week,-2,-1))]
datFlu[ , Year := paste(str_sub(Week,1,4),'1','1',sep = '-')]
datFlu[ , date := ymd( Year ) + weeks( WeekNum - 1 )]

# create two data tables, cumulative sum and cumulative sum by year.

datFluYear <-
datFlu %>%
  select(`Total A`,`Total B`,`Total # Tested`,'WeekNum','Year','date') %>%
  group_by(Year) %>%
  mutate(cSumA = cumsum(`Total A`),
         cSumB = cumsum(`Total B`),
         cSumTested = cumsum(`Total # Tested`),
         cSumPositive = cumsum(`Total A` + `Total B`)) %>%
  mutate(perPos = cSumPositive / cSumTested)

datFluAll <-
  datFlu %>%
  select(`Total A`,`Total B`,`Total # Tested`,'WeekNum','Year','date') %>%
  mutate(cSumA = cumsum(`Total A`),
         cSumB = cumsum(`Total B`),
         cSumTested = cumsum(`Total # Tested`),
         cSumPositive = cumsum(`Total A` + `Total B`)) %>%
  mutate(perPos = cSumPositive / cSumTested,
         numDays = seq(from = 1, to = nrow(datFlu)*7,by = 7))

dat =
dat %>% 
  mutate(`Last Update` = parse_date_time(`Last Update`,c("mdy_HM","mdy"))) %>%
  group_by(`Province/State`,`Country/Region`,`Last Update`) %>% 
  filter(Confirmed == max(Confirmed)) %>% 
  distinct %>%
  as.data.table()

dat %>%
  filter((grepl('China',`Country/Region`)))
# Plot Functions

plotDat <- function(country,province ){
  dat %>% 
    filter(grepl(country,`Country/Region`) & grepl(province,`Province/State`))  %>%
    group_by(`Province/State`) %>% 
    #group_by(`Province/State`, d = day(`Last Update`)) %>% 
    #filter(`Last Update` == max(`Last Update`)) %>%
    ggplot(aes(x = `Last Update`))+
    geom_line(aes(y=Confirmed, color = "Infected"))+
    geom_line(aes(y=Existing, color = "Existing"))+
    geom_line(aes(y=Recovered, color = "Recovered"))+
    geom_line(aes(y=Death, color = "Death"))+
    
    geom_point(size = I(3), shape = 1, aes(y=Confirmed, color = "Infected"))+
    geom_point(size = I(3), shape = 1,aes(y=Existing, color = "Existing"))+
    geom_point(size = I(3), shape = 1,aes(y=Recovered, color = "Recovered"))+
    geom_point(size = I(3), shape = 1,aes(y=Death, color = "Death"))+
    
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
                        breaks=c("Infected","Existing","Recovered","Death"),
                        values=c("green","red","blue","black"))+
    labs(title = "Coronavirus(2019-nCoV)",
         subtitle = paste(province,country,sep =" "))
}

plotDatContry <- function(country ){
  dat %>% 
    filter(grepl(country,`Country/Region`) & !grepl('county',`Province/State`) )  %>%
    group_by(`Province/State`) %>% 
    group_by( d= day(`Last Update`), m = month(`Last Update`),y=year(`Last Update`)) %>%
    summarise_if(is.numeric,sum) %>%
    mutate(Date = ymd(paste(y,m,d))) %>%
    
    ggplot(aes(x = `Date`))+
    geom_line(aes(y=Confirmed, color = "Infected"))+
    geom_line(aes(y=Existing, color = "Existing"))+
    geom_line(aes(y=Recovered, color = "Recovered"))+
    geom_line(aes(y=Death, color = "Death"))+
    
    geom_point(size = I(3), shape = 1, aes(y=Confirmed, color = "Infected"))+
    geom_point(size = I(3), shape = 1,aes(y=Existing, color = "Existing"))+
    geom_point(size = I(3), shape = 1,aes(y=Recovered, color = "Recovered"))+
    geom_point(size = I(3), shape = 1,aes(y=Death, color = "Death"))+
    
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
                        breaks=c("Infected","Existing","Recovered","Death"),
                        values=c("green","red","blue","black"))+
    labs(title = "Coronavirus(2019-nCoV)",
         subtitle = paste(country,sep =" "))
}

plotDatContryUSA <- function(){
dat %>%
  filter(`Country/Region` == 'US' & !grepl('County',`Province/State`) & !grepl("^[[:upper:]]+$",str_sub(`Province/State`,-2,-1))) %>%
    group_by(`Province/State` )  %>% 
    group_by( d= day(`Last Update`), m = month(`Last Update`),y=year(`Last Update`)) %>%
    summarise_if(is.numeric,sum) %>%
    mutate(Date = ymd(paste(y,m,d))) %>%
    
    ggplot(aes(x = `Date`))+
    geom_line(aes(y=Confirmed, color = "Infected"))+
    geom_line(aes(y=Existing, color = "Existing"))+
    geom_line(aes(y=Recovered, color = "Recovered"))+
    geom_line(aes(y=Death, color = "Death"))+
    geom_point(size = I(3), shape = 1, aes(y=Confirmed, color = "Infected"))+
    geom_point(size = I(3), shape = 1,aes(y=Existing, color = "Existing"))+
    geom_point(size = I(3), shape = 1,aes(y=Recovered, color = "Recovered"))+
    geom_point(size = I(3), shape = 1,aes(y=Death, color = "Death"))+
    
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
                        breaks=c("Infected","Existing","Recovered","Death"),
                        values=c("green","red","blue","black"))+
    labs(title = "Coronavirus(2019-nCoV)",
         subtitle = paste("United States Of America",sep =" "))
}

state <- 'Michigan'

plotDatContryUSAState <- function(state){
  dat %>%
    filter(`Country/Region` == 'US'& !grepl('County',`Province/State`) & grepl(state,`Province/State`) & 
             !grepl("^[[:upper:]]+$",str_sub(`Province/State`,-2,-1))) %>%
    #group_by(`Province/State`)  %>% 
    filter(`Province/State` == state) %>%
    #filter(`Last Update` == max(`Last Update`)) %>%
    #group_by( d, m = month(`Last Update`),y=year(`Last Update`)) %>%
    #summarise_if(is.numeric,sum) %>%
    #mutate(Date = ymd(paste(y,m,d))) %>%
    
    
    ggplot(aes(x = `Last Update`))+
    geom_line(aes(y=Confirmed, color = "Infected"))+
    geom_line(aes(y=Existing, color = "Existing"))+
    geom_line(aes(y=Recovered, color = "Recovered"))+
    geom_line(aes(y=Death, color = "Death"))+
    
    geom_point(size = I(3), shape = 1, aes(y=Confirmed, color = "Infected"))+
    geom_point(size = I(3), shape = 1,aes(y=Existing, color = "Existing"))+
    geom_point(size = I(3), shape = 1,aes(y=Recovered, color = "Recovered"))+
    geom_point(size = I(3), shape = 1,aes(y=Death, color = "Death"))+
    
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
                        breaks=c("Infected","Existing","Recovered","Death"),
                        values=c("green","red","blue","black"))+
    labs(title = "Coronavirus(2019-nCoV)",
         subtitle = paste("United States Of America",state,sep =" "))
}
  
 
# data is pulled at multiple times, but is a cumlative sum by province  / country
# Shanghai 
plotDat('China','Shanghai')

# South Korea
plotDat('Korea','')

# South Korea
plotDat('Iceland','')

# Hubei
plotDat('China','Hubei')

# USA
plotDatContryUSA()

plotDatContryUSAState('California')
plotDatContryUSAState('New York')
plotDatContryUSAState('Michigan')
plotDatContryUSAState('Florida')
plotDatContryUSAState('Washington')

dat %>% 
  filter(`Country/Region` == 'US' & !grepl('County',`Province/State`) &
           !grepl("^[[:upper:]]+$",str_sub(`Province/State`,-2,-1)) &
           `Province/State` == 'Washington')

#plotDat('US','Oakland MI')
#China
plotDatContry('China')

# Japan
plotDatContry('Japan')

# Italy
plotDatContry('Italy')

# Morocco
plotDatContry('Morocco')

# UK
plotDatContry('United Kingdom')

# plot of top 10 provinces 
# get the names of the 10 provinces

top_10 =
dat %>% 
  filter(grepl('China',`Country/Region`) )%>%
  filter(`Last Update` == max(`Last Update`)) %>%
  top_n(10,Confirmed) %>%
  pull(`Province/State`)

dat %>% 
  filter(grepl('China',`Country/Region`) & `Province/State` %in% top_10)  %>%
  ggplot(aes(x = `Last Update`, y= Confirmed, color = `Province/State`))+
  geom_line()+
  geom_point(size = I(3), shape = 1)+
  ylab(label="Count")+
  xlab(label="Date")+
  labs(title = "Wuhan Coronavirus(2019-nCoV)",
       subtitle = "Top 10 Confirmed Cases By Province, China")

# top 10 US states
top_10 =
  dat %>% 
  filter(`Country/Region` == 'US'& !grepl('County',`Province/State`)& !grepl('Princess',`Province/State`)& !grepl("^[[:upper:]]+$",str_sub(`Province/State`,-2,-1))) %>%
  #group_by(`Province/State`)  %>% 
  #filter(grepl('US',`Country/Region`) & !grepl('County',`Province/State`) )%>%
  filter(`Last Update` == max(`Last Update`)) %>%
  top_n(10,Confirmed) %>%
  pull(`Province/State`)

dat %>% 
  filter(grepl('US',`Country/Region`) & `Province/State` %in% top_10 & !grepl('County',`Province/State`) & !grepl('Princess',`Province/State`)& !grepl("^[[:upper:]]+$",str_sub(`Province/State`,-2,-1)))  %>%
  ggplot(aes(x = `Last Update`, y= Confirmed, color = `Province/State`))+
  geom_line()+
  geom_point(size = I(3), shape = 1)+
  ylab(label="Count")+
  xlab(label="Date")+
  labs(title = "Wuhan Coronavirus(2019-nCoV)",
       subtitle = "Top 10 Confirmed Cases By State, United States")

# plot removing Hubei province
top_13 =
  dat %>% 
  filter(grepl('China',`Country/Region`)& !grepl('Hubei',`Province/State`)) %>%
  filter(`Last Update` == max(`Last Update`)) %>%
  top_n(13,Confirmed) %>%
  pull(`Province/State`) 

dat %>% 
  filter(grepl('China',`Country/Region`) & `Province/State` %in% top_13)  %>%
  ggplot(aes(x = `Last Update`, y= Confirmed, color = `Province/State`))+
  geom_line()+
  geom_point(size = I(3), shape = 1)+
  ylab(label="Count")+
  xlab(label="Date")+
  labs(title = "Wuhan Coronavirus(2019-nCoV)",
       subtitle = "Top 12 Confirmed Cases By Province, China Less Hubei")

# Shanghai and Beijing only

dat %>% 
  filter(grepl('China',`Country/Region`) & `Province/State` %in% c('Shanghai','Beijing'))  %>%
  group_by(`Province/State`) %>% 
  ggplot(aes(x = `Last Update`, y= Confirmed, color = `Province/State`))+
  geom_line()+
  geom_point(size = I(3), shape = 1)+
  ylab(label="Count")+
  xlab(label="Date")+
  labs(title = "Wuhan Coronavirus(2019-nCoV)",
       subtitle = "Confirmed Cases Shanghai and Beijing")


# plot removing China
top_10 =
  dat %>% 
  filter(!grepl('China',`Country/Region`) & !grepl('Other',`Country/Region`)) %>%
  filter(`Last Update` == max(`Last Update`)) %>%
  #group_by(`Country/Region`,`Last Update`) %>%
  top_n(10,Confirmed) %>%
  pull(`Country/Region`) 

dat %>% 
  filter(!grepl('China',`Country/Region`) & `Country/Region` %in% top_10)  %>%
  group_by(`Country/Region`, d = day(`Last Update`)) %>% 
  filter(`Last Update` == max(`Last Update`)) %>%
  ggplot(aes(x = `Last Update`, y= Confirmed, color = `Country/Region`))+
  geom_line()+
  geom_point(size = I(3), shape = 1)+
  ylab(label="Count")+
  xlab(label="Date")+
  labs(title = "Wuhan Coronavirus(2019-nCoV)",
       subtitle = "Top 10 Confirmed Cases Outside China")



### 
# Plot total case Conavirus vs. flu.


datCVirFlu <-
  dat %>% 
  #filter(grepl('China',`Country/Region`)& grepl('Main',`Country/Region`))  %>%
  group_by(`Province/State`, d = day(`Last Update`))  %>% 
  filter(`Last Update` == max(`Last Update`)) %>%
  group_by( d, m = month(`Last Update`),y=year(`Last Update`)) %>%
  summarise_if(is.numeric,sum) %>%
  mutate(Date = ymd(paste(y,m,d))) %>%
  arrange(Date) %>%
  mutate(numDays = as.numeric(c(diff(Date),1))) %>%
  as.data.table()

datFluAlla <-
datFluAll %>%
  select(Confirmed = cSumPositive,numDays) %>%
  mutate(type = rep("Flu (A and B) ",times = nrow(datFluAll))) 
  

datCVirFlu %>%
  mutate(numDays = cumsum(numDays)) %>%
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
  labs(title = "Wuhan Coronavirus(2019-nCoV)",
       subtitle = "Mainland China")



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

provinceCoord

mapDat =
dat %>% 
  filter(grepl('China',`Country/Region`))  %>%
  group_by(`Province/State`)  %>% 
  filter(`Last Update` == max(`Last Update`)) %>%
  left_join(provinceCoord, by = c('Province/State' = 'Province')) %>%
  select(`Province/State`,lng,lat,Confirmed) %>%
  leaflet() %>%
  addTiles() %>%
  addCircles(lng = ~lng, lat = ~lat, weight = 1,
             radius = ~log(Confirmed) * 20000, 
             popup = ~`Province/State`,
             label = ~Confirmed) %>%
  addPopups(lng=121.4, lat=50, paste(sep = "<br/>", "Scroll over the circle to see the confirmed count","Click the circle to see the provice name"),
            options = popupOptions(closeButton = FALSE)) %>%
  addPopups(lng=90, lat=50, paste(sep = "<br/>", "<b>Wuhan Coronavirus(2019-nCoV</b>","Confirmed Infection Counts By Province",max(dat$`Last Update`)),
            options = popupOptions(closeButton = FALSE))


mapDat

## existing cases map
mapDat =
  dat %>% 
  filter(grepl('China',`Country/Region`))  %>%
  group_by(`Province/State`)  %>% 
  filter(`Last Update` == max(`Last Update`)) %>%
  left_join(provinceCoord, by = c('Province/State' = 'Province')) %>%
  select(`Province/State`,lng,lat,Existing) %>%
  leaflet() %>%
  addTiles() %>%
  addCircles(lng = ~lng, lat = ~lat, weight = 1,
             radius = ~log(Existing) * 20000, 
             popup = ~`Province/State`,
             label = ~Existing) %>%
  addPopups(lng=121.4, lat=50, paste(sep = "<br/>", "Scroll over the circle to see the existing count","Click the circle to see the provice name"),
            options = popupOptions(closeButton = FALSE)) %>%
  addPopups(lng=90, lat=50, paste(sep = "<br/>", "<b>Wuhan Coronavirus(2019-nCoV</b>","Existing Infection Counts By Province",max(dat$`Last Update`)),
            options = popupOptions(closeButton = FALSE))

mapDat
####
# USA maps

datMap <-
datLatLonMap %>%
  filter(`Country/Region` == 'US' & !grepl('County',`Province/State`) & !grepl('Princess',`Province/State`)& !grepl("^[[:upper:]]+$",str_sub(`Province/State`,-2,-1))) 
names(datMap) <- c('Province/State','Country/Region','lat','lng')

mapDat =
  dat %>% 
  filter(`Country/Region` == 'US' & !grepl('County',`Province/State`) & !grepl("^[[:upper:]]+$",str_sub(`Province/State`,-2,-1)))  %>%
  group_by(`Province/State`)  %>% 
  filter(`Last Update` == max(`Last Update`)) %>%
  left_join(datMap, by = c('Province/State' = 'Province/State','Country/Region' ='Country/Region')) %>%
  select(`Province/State`,lng,lat,Confirmed) %>%
  leaflet() %>%
  addTiles() %>%
  addCircles(lng = ~lng, lat = ~lat, weight = 1,
             radius = ~log(Confirmed) * 20000, 
             popup = ~`Province/State`,
             label = ~Confirmed) %>%
  addPopups(lng=-121.4, lat=50, paste(sep = "<br/>", "Scroll over the circle to see the confirmed count","Click the circle to see the provice name"),
            options = popupOptions(closeButton = FALSE)) %>%
  addPopups(lng=-90, lat=50, paste(sep = "<br/>", "<b>Wuhan Coronavirus(2019-nCoV</b>","Confirmed Infection Counts By State",max(dat$`Last Update`)),
            options = popupOptions(closeButton = FALSE))


mapDat
######### SEIR model to actual data

## Model 1 returns a plot of the cumlative model results to match the John Hopkin's University data.
## Model 2 will take the John Hopkin's data, get a cases per day.

SEIR.model.incubation.pop.cumsum <- function(t, b, g, c ,population = 10000, infect = 1){
  
  init <- c(S=1-infect/population,I=infect/population,R=0, E=200/population)
  parameters <- c(bet=b,gamm=g,eta=c)
  time <- seq(0,t,by=t/(1*length(1:t)))
  eqn <- function(time,state,parameters){
    with(as.list(c(state,parameters)),{
      dS <- -bet*S*I
      dI <- eta*E-gamm*I
      dR <- gamm*I
      dE <- bet*S*I-eta*E
      return(list(c(dS,dI,dR,dE)))})}
  out<-ode(y=init,times=time,eqn,parms=parameters)
  out.dt<-as.data.table(out)
  
 
  out.dt[,S_pop := S*population]
  out.dt[,I_pop := I*population]
  out.dt[,R_pop := R*population]
  out.dt[,E_pop := E*population]
  
datAct <-
  dat %>% 
    filter(grepl('China',`Country/Region`)& grepl('Hubei',`Province/State`))  %>%
    group_by(`Province/State`)  %>% 
    #filter(`Last Update` == max(`Last Update`)) %>%
    #group_by( d, m = month(`Last Update`),y=year(`Last Update`)) %>%
    #summarise_if(is.numeric,sum) %>%
    mutate(Date = ymd(`Last Update`)) %>%
    arrange(Date) %>%
    mutate(numDays = as.numeric(c(diff(Date),1))) %>%
    as.data.table() %>%
    mutate(numDays = cumsum(numDays)) %>%
    select(Confirmed,Recovered,numDays) %>%
    as.data.table()

datAct =  
 datAct %>%
   mutate(type = rep("Coronavirus",times = nrow(datAct)),
          Suspected = rep(0,times = nrow(datAct)),
          Expected = rep(0,times = nrow(datAct)),
          Confirmed_sum = Confirmed,
          Recovered_sum = Recovered,
          Expected_sum = Expected) %>% 
   as.data.table()

out.dt  =  
 out.dt %>%
   select(numDays = time , Suspected = S_pop,Confirmed = I_pop,Recovered = R_pop, Expected=E_pop) %>%
   mutate(type = rep('Model',times = nrow(out.dt)),
          Confirmed_sum = cumsum(Confirmed),
          Recovered_sum = cumsum(Recovered),
          Expected_sum = cumsum(Expected),
          numDays = numDays - 2) %>%
   rbind(datAct)

print(tail(out.dt))
 
   title <- bquote("SEIR Model: Basic vs. Actual Data")
   subtit <- bquote(list(beta==.(parameters[1]),~gamma==.(parameters[2]),~eta==.(parameters[3])))
   
   res<-ggplot(out.dt,aes(x=numDays))+
     ggtitle(bquote(atop(bold(.(title)),atop(bold(.(subtit))))))+
     #geom_line(aes(y=S_pop,colour="Susceptible"))+
     #geom_point(size = I(2), shape = 1, aes(y=Confirmed,colour="Confirmed"))+
     #geom_point(size = I(2), shape = 1,aes(y=Recovered,colour="Recovered"))+
     #geom_point(size = I(2), shape = 1,aes(y=Expected,colour="Incubation"))+
     geom_line(size = I(1), aes(y=Confirmed_sum,colour="Confirmed"))+
     #geom_line(aes(y=Recovered_sum,colour="Recovered"))+
     geom_line(aes(y=Expected_sum,colour="Incubation"))+
     ylab(label="Count")+
     xlab(label="Time (days)")+
     facet_grid(type~.)+
     theme(legend.justification=c(1,0), legend.position=c(1,0.75))+
     theme(legend.title=element_text(size=12,face="bold"),
           legend.background = element_rect(fill='#FFFFFF',
                                            size=0.5,linetype="solid"),
           legend.text=element_text(size=10),
           legend.key=element_rect(colour="#FFFFFF",
                                   fill='#C2C2C2',
                                   size=0.25,
                                   linetype="solid"))+
     scale_colour_manual("Compartments",
                         breaks=c("Susceptible","Confirmed","Recovered","Incubation"),
                         values=c("blue","red","green","black"))
   print(res)
   return(out.dt)
}

# beta is the number infection contacts per day.
# gamma is 1 over the duration of the infection.
# e is 1 over the incubation period

SEIR.model.incubation.pop.cumsum(55,2.5,1/7,1/10)

SEIR.model.incubation.pop.cumsum(90,2,1/7,1/10)




SEIR.model.incubation.pop <- function(t, b, g, c ,population = 10000000, infect = 270){
  
  init <- c(S=1-infect/population,I=infect/population,R=0, E=200/population)
  parameters <- c(bet=b,gamm=g,eta=c)
  time <- seq(0,t,by=t/(1*length(1:t)))
  eqn <- function(time,state,parameters){
    with(as.list(c(state,parameters)),{
      dS <- -bet*S*I
      dI <- eta*E-gamm*I
      dR <- gamm*I
      dE <- bet*S*I-eta*E
      return(list(c(dS,dI,dR,dE)))})}
  out<-ode(y=init,times=time,eqn,parms=parameters)
  out.dt<-as.data.table(out)
  
  
  out.dt[,S_pop := S*population]
  out.dt[,I_pop := I*population]
  out.dt[,R_pop := R*population]
  out.dt[,E_pop := E*population]
  
datAct = 
  dat %>% 
  filter(grepl('China',`Country/Region`))  %>%
  group_by(`Province/State`)  %>% 
  
  summarise_if(is.numeric,sum) %>%
  mutate(Date = ymd(`Last Update`)) %>%
  arrange(Date) %>%
  mutate(numDays = as.numeric(c(diff(Date),1)))%>%
  as.data.table() %>%
  mutate(Confirmed = Confirmed - lag(Confirmed, default = 0)) %>%
  mutate(Recovered = Recovered - lag(Recovered, default = 0)) %>%
  mutate(numDays = cumsum(numDays)) %>%
  select(Confirmed,Recovered,numDays) %>%
  as.data.table()
  
  datAct =  
    datAct %>%
    mutate(type = rep("Coronavirus",times = nrow(datAct)),
           Suspected = rep(0,times = nrow(datAct)),
           Expected = rep(0,times = nrow(datAct))
          ) %>% 
    as.data.table()
  
  out.dt  =  
    out.dt %>%
    select(numDays = time , Suspected = S_pop,Confirmed = I_pop,Recovered = R_pop, Expected=E_pop) %>%
    mutate(type = rep('Model',times = nrow(out.dt)),
           numDays = numDays - 0) %>%
    rbind(datAct)
  
  #print(tail(out.dt))
  
  title <- bquote("SEIR Model: Basic vs. Actual Data")
  subtit <- bquote(list(beta==.(parameters[1]),~gamma==.(parameters[2]),~eta==.(parameters[3])))
  
  res<-ggplot(out.dt,aes(x=numDays))+
    ggtitle(bquote(atop(bold(.(title)),atop(bold(.(subtit))))))+
    #geom_line(aes(y=S_pop,colour="Susceptible"))+
    #geom_point(size = I(2), shape = 1, aes(y=Confirmed,colour="Confirmed"))+
    #geom_point(size = I(2), shape = 1,aes(y=Recovered,colour="Recovered"))+
    #geom_point(size = I(2), shape = 1,aes(y=Expected,colour="Incubation"))+
    geom_line(size = I(1), aes(y=Confirmed,colour="Confirmed"))+
    #geom_line(aes(y=Recovered_sum,colour="Recovered"))+
    geom_line(aes(y=Expected,colour="Incubation"))+
    ylab(label="Count")+
    xlab(label="Time (days)")+
    facet_grid(type~.)+
    theme(legend.justification=c(1,0), legend.position=c(1,0.75))+
    theme(legend.title=element_text(size=12,face="bold"),
          legend.background = element_rect(fill='#FFFFFF',
                                           size=0.5,linetype="solid"),
          legend.text=element_text(size=10),
          legend.key=element_rect(colour="#FFFFFF",
                                  fill='#C2C2C2',
                                  size=0.25,
                                  linetype="solid"))+
    scale_colour_manual("Compartments",
                        breaks=c("Susceptible","Confirmed","Recovered","Incubation"),
                        values=c("blue","red","green","black"))
  print(res)
  return(out.dt)
}

SEIR.model.incubation.pop(25,0.5,1/7,1/10)
SEIR.model.incubation.pop(100,1.1,1/7,1/10)


#########################
# Number of people in a room to have 50% of a person confirmed

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

bday(1000,12/1000000,1)

probUsa =
dat %>% 
  filter(`Country/Region` == 'US' & !grepl('County',`Province/State`) & !grepl("^[[:upper:]]+$",str_sub(`Province/State`,-2,-1))) %>%
  mutate(Date = ymd(`Last Update`)) %>%
    filter(Date == max(Date)) %>%
  summarise(total_confirmed = sum(Confirmed),
            total_existing = sum(Existing))
# population of the USA is approximately 327.2 million

probContact <- data.table(x= seq(from = 1, to = 1500, by = 1))
probContact[ , prob_conf := bday(n = max(x), pr = probUsa$total_confirmed/327200000)]
probContact[ , prob_exist := bday(n = max(x), pr = probUsa$total_existing/327200000)]

probContact <- melt(probContact, id.vars = c('x'))

probContact[value >= 0.499 & value <= 0.501,max(x)]

ggplot(data = probContact, aes(x=x,y=value,color = variable))+
  geom_line() +
  geom_hline(yintercept = 0.5)+
  geom_vline(xintercept = c(probContact[value >= 0.499 & value <= 0.501,max(x)],probContact[value >= 0.499 & value <= 0.501,min(x)]))


probMichian =
  dat %>% 
  filter(`Country/Region` == 'US' & grepl('Michigan',`Province/State`), !grepl('County',`Province/State`) & !grepl("^[[:upper:]]+$",str_sub(`Province/State`,-2,-1))) %>%
  mutate(Date = ymd(`Last Update`)) %>%
  filter(Date == max(Date)) %>%
  summarise(total_confirmed = sum(Confirmed),
            total_existing = sum(Existing))
# population of the USA is approximately 327.2 million

probContact <- data.table(x= seq(from = 1, to = 10000, by = 1))
probContact[ , prob_conf := bday(n = max(x), pr = probMichian$total_confirmed/9996000)]
probContact[ , prob_exist := bday(n = max(x), pr = probMichian$total_existing/9996000)]

probContact <- melt(probContact, id.vars = c('x'))

probContact[value >= 0.499 & value <= 0.501,max(x)]

ggplot(data = probContact, aes(x=x,y=value,color = variable))+
  geom_line() +
  geom_hline(yintercept = 0.5)+
  geom_vline(xintercept = c(probContact[value >= 0.499 & value <= 0.501,max(x)],probContact[value >= 0.499 & value <= 0.501,min(x)]))
