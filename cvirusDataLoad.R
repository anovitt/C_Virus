# Load data
# function to load data

files<-list.files("R/2019-coronavirus-dataset-01212020-01262020/csse_covid_19_daily_reports/",full.names=TRUE)

dat<-data.table()
##load data
for(i in 1:(length(files)-1)){
  
  data.temp<-fread(files[i],header =TRUE)
  if (ncol(data.temp) >= 12) {
    data.temp = select(data.temp, `Province_State`, `Country_Region`,`Last_Update`, `Confirmed`, `Deaths`, `Recovered` )
  }else{
    data.temp = select(data.temp, `Province/State`, `Country/Region`,`Last Update`, `Confirmed`, `Deaths`, `Recovered` )
  }
  
  names(data.temp) <- c('Province_State', 'Country_Region','Last_Update', 'Confirmed', 'Deaths', 'Recovered' )
  
  data.temp <-
    data.temp %>%
    mutate(Last_Update = parse_date_time(Last_Update,c("mdy_HM","ymd_HMS")))%>%
    mutate(Last_Update = max(Last_Update),
           Confirmed = ifelse(is.na(Confirmed),0,Confirmed),
           Deaths = ifelse(is.na(Deaths),0,Deaths),
           Recovered = ifelse(is.na(Recovered),0,Recovered)) %>%
    mutate(Country_Region = ifelse(grepl('Mainland China',Country_Region),'China',Country_Region)) %>%
    
    as.data.table()
  dat<-rbind(dat,data.temp)
}

dat =
  dat %>%
  arrange(Last_Update)

dat

# State Data

datSate <- fread(files[length(files)-1],header =TRUE)



########################## USA Reports

files<-list.files("R/2019-coronavirus-dataset-01212020-01262020/csse_covid_19_daily_reports_us/",full.names=TRUE)

datUS <- data.table()

for(i in 1:(length(files)-1)){
  
  data.temp<-fread(files[i],header =TRUE)
  
  data.temp = select(data.temp, `Province_State`, `Country_Region`,`Last_Update`, `Confirmed`, `Deaths`, `Recovered` )
  names(data.temp) <- c('Province_State', 'Country_Region','Last_Update', 'Confirmed', 'Deaths', 'Recovered' )
  
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

datUS =
datUS %>%
  arrange(Last_Update)


# sub set and add sub set US daily data from after april 21.

dat<-
  rbind(
    dat %>% 
      filter(Country_Region != 'US' & Last_Update >= '2020-04-21' | Last_Update < '2020-04-21'),
    datUS %>%
      filter(!is.na(Last_Update)) %>%
      filter(!grepl('Guam',Province_State) | !grepl('Princess',Province_State) | !grepl('Island',Province_State)) %>%
      filter(Province_State != 'Recovered' )
  ) %>%
  mutate(Existing = Confirmed - Recovered - Deaths) %>%
  as.data.table()

datConf <- fread('R/2019-coronavirus-dataset-01212020-01262020/csse_covid_19_daily_reports/03-21-2020.csv')

datLatLonMap <- 
  datConf %>%
  rename(c('Province_State' = 'Province/State' , 'Country_Region' = 'Country/Region','lat'= 'Latitude','lng'='Longitude' )) %>%
  select('Province_State','Country_Region','lat','lng') 

dat =
  dat %>%
  mutate(Recovered = ifelse(is.na(Recovered), 0, Recovered),
         Deaths = ifelse(is.na(Deaths), 0, Deaths),
         Confirmed = ifelse(is.na(Confirmed), 0, Confirmed),
         Last_Update = parse_date_time(Last_Update,c("mdy_HM","ymd_HMS")))%>%
  #Last_Update = mdy_hm(Last_Update)) %>%
  mutate(Existing = Confirmed - Recovered - Deaths) %>%
  group_by(`Country_Region`,`Province_State`,`Last_Update`) %>%
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered),
            Existing = sum(Existing)) %>%
  left_join(datLatLonMap, by = c('Province_State'= 'Province_State','Country_Region'='Country_Region')) %>%
  mutate(Last_Update = date(Last_Update)) %>%
  group_by(Province_State,Country_Region,Last_Update) %>% 
  filter(Confirmed == max(Confirmed)) %>% 
  distinct %>%
  as.data.table()

datFlu <- fread("R/2019-coronavirus-dataset-01212020-01262020/flu.csv")
usStatePopulation <- fread("R/2019-coronavirus-dataset-01212020-01262020/nst-est2019-01.csv",header = TRUE)

usStatePopulation <-
  usStatePopulation %>%
  mutate(`Geographic Area` = ifelse(grepl('[[:punct:] ]+',`Geographic Area`),str_sub(`Geographic Area`,2,-1),`Geographic Area`))

Population <- 
  rbind (
    population %>% 
      filter(year == max(year)) %>%
      mutate(country = ifelse(country == "United States of America","US",country)) %>%
      add_row(country = "ChinaShanghai", year = 2013,population = 25000000) %>%
      add_row(country = "ChinaBeijing", year = 2013,population = 12000000),
    
    dat %>%
      filter(`Country_Region` == 'US' & !grepl('County',`Province_State`) & !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1))) %>%
      filter(Last_Update == max(Last_Update)) %>%
      mutate(country = paste0(Country_Region,Province_State),
             population = 1000000,
             year = 2013) %>%
      select(country,year,population)
  ) %>%
  rename(Country = country, Population = population) %>% 
  as.data.table() 

Population <-
  Population %>%
  add_row(Country = "Korea", year = 2013, Population = Population[Country  == "Republic of Korea",]$Population )

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

# Load data to pull number of test data

files<-list.files("R/2019-coronavirus-dataset-01212020-01262020/csse_covid_19_daily_reports_us/",full.names=TRUE)
datUS <- data.table()

cols <- c("Province_State", "Country_Region","Last_Update" ,"Lat" , "Long_" ,           
           "Confirmed" ,"Deaths","Recovered","Active","FIPS",                
         "Incident_Rate","People_Tested","People_Hospitalized","Case_Fatality_Ratio", "UID",                 
           "ISO3", "Testing_Rate", "Hospitalization_Rate")

for(i in 1:(length(files)-1)){
  
  data.temp<-fread(files[i],header =TRUE)
  
  names(data.temp)
  
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
  
  names(data.temp) <- cols
  
  datUS<-rbind(datUS,data.temp)
}

datUS =
  datUS %>%
  arrange(Last_Update)

# Plot Functions

plotDat <- function(country,province ){
  dat %>% 
    {if (country== 'US') filter(., Last_Update > '2020-03-09') else filter(., Last_Update > '2020-01-21')} %>% 
    filter(grepl(country,`Country_Region`) & grepl(province,`Province_State`))  %>%
    group_by(`Province_State`) %>% 
    ggplot(aes(x = `Last_Update`))+
    geom_line(aes(y=Confirmed, color = "Infected"))+
    geom_line(aes(y=Existing, color = "Existing"))+
    geom_line(aes(y=Recovered, color = "Recovered"))+
    geom_line(aes(y=Deaths, color = "Deaths"))+
    
    geom_point(size = I(3), shape = 1, aes(y=Confirmed, color = "Infected"))+
    geom_point(size = I(3), shape = 1,aes(y=Existing, color = "Existing"))+
    geom_point(size = I(3), shape = 1,aes(y=Recovered, color = "Recovered"))+
    geom_point(size = I(3), shape = 1,aes(y=Deaths, color = "Deaths"))+
    
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
         subtitle = paste(province,country,sep =" "))
}

plotDatPhaseCountry <- function(country,province ){
  
  Pop <-
    Population %>%
    #filter(Country == paste0(country,province)) %>%
    filter(Country == paste0(country)) %>%
    select(Population)
  
  print(Pop)
  
  dat %>% 
    {if (country == 'India' | country == 'Brazil' | 
         country == 'Mexico' | country == 'Canada') filter(., Last_Update > '2020-04-01') else filter(., Last_Update > '2020-01-21')} %>% 
    #filter(grepl(country,`Country_Region`) & grepl(province,`Province_State`))  %>%
    
    filter(grepl(country,`Country_Region`)) %>%
    group_by(Last_Update) %>%
    summarise_if(is.numeric,sum) %>%
    
    
    mutate('Suseptable' = Pop$Population - Existing - Deaths - Recovered) %>%
    filter(Existing >=0)%>%
    #group_by(`Province_State`) %>% 
    ggplot(aes(x = `Existing`))+
    #geom_line(aes(y=Confirmed, color = "Infected"))+
    #geom_line(aes(y=Suseptable, color = "Suseptable"))+
    
    #geom_point(size = I(3), shape = 1, aes(y=Confirmed, color = "Infected"))+
    geom_point(size = I(3), shape = 1,aes(y=Suseptable, color = "Suseptable"))+
    
    ylab(label="Suseptable")+
    xlab(label="Existing Cases")+
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
                        breaks=c("Suseptable","Existing"),
                        values=c("blue","black"))+
    labs(title = "Coronavirus(2019-nCoV)",
         subtitle = paste(province,country,sep =" "))
}

plotDatPhase <- function(country,province ){
  
  Pop <-
    Population %>%
    filter(Country == paste0(country,province)) %>%
    #filter(Country == paste0(country)) %>%
    select(Population)
  
  print(Pop)
  
  dat %>% 
    {if (country== 'Germany' | country== 'France'| country == 'India' | 
         country == 'Spain' | country == 'Italy'| country == 'Brazil' | 
         country == 'Mexico' | country == 'Canada') filter(., Last_Update > '2020-04-01') else filter(., Last_Update > '2020-01-21')} %>% 
    filter(grepl(country,`Country_Region`) & grepl(province,`Province_State`))  %>%
    
    mutate('Suseptable' = Pop$Population - Existing - Deaths - Recovered) %>%
    filter(Existing >=0)%>%
    group_by(`Province_State`) %>% 
    ggplot(aes(x = `Existing`))+
    #geom_line(aes(y=Confirmed, color = "Infected"))+
    #geom_line(aes(y=Suseptable, color = "Suseptable"))+
    
    #geom_point(size = I(3), shape = 1, aes(y=Confirmed, color = "Infected"))+
    geom_point(size = I(3), shape = 1,aes(y=Suseptable, color = "Suseptable"))+
    
    ylab(label="Suseptable")+
    xlab(label="Existing Cases")+
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
                        breaks=c("Suseptable","Existing"),
                        values=c("blue","black"))+
    labs(title = "Coronavirus(2019-nCoV)",
         subtitle = paste(province,country,sep =" "))
}

plotDatContry2 <- function(country ){
  dat %>%
    {if ( country == 'India' |  country == 'Brazil' | 
          country == 'Mexico' | country == 'Canada') filter(., Last_Update > '2020-04-01') else filter(., Last_Update > '2020-01-21')} %>%
    filter(grepl(country,`Country_Region`)) %>%
    group_by(Last_Update) %>%
    summarise_if(is.numeric,sum) %>%
    ggplot(aes(x = Last_Update))+
    geom_line(aes(y=Confirmed, color = "Infected"))+
    geom_line(aes(y=Existing, color = "Existing"))+
    geom_line(aes(y=Recovered, color = "Recovered"))+
    geom_line(aes(y=Deaths, color = "Deaths"))+
    
    geom_point(size = I(3), shape = 1, aes(y=Confirmed, color = "Infected"))+
    geom_point(size = I(3), shape = 1,aes(y=Existing, color = "Existing"))+
    geom_point(size = I(3), shape = 1,aes(y=Recovered, color = "Recovered"))+
    geom_point(size = I(3), shape = 1,aes(y=Deaths, color = "Deaths"))+
    
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
                        values=c("green","red","blue","black")) +
    labs(title = "Coronavirus(2019-nCoV)",
         subtitle = paste(country,sep =" "))
}

plotDatContry <- function(country ){
  dat %>% 
    {if (country== 'Germany' | country== 'France'| country == 'India' | country == 'Spain' | country == 'Italy') filter(., Last_Update > '2020-03-09') else filter(., Last_Update > '2020-01-21')} %>%
    filter(grepl(country,`Country_Region`) & grepl("^\\s*$",`Province_State`))  %>%
    #group_by(`Province_State`) %>% 
    group_by(Last_Update) %>%
    summarise_if(is.numeric,sum) %>%
    #mutate(Date = ymd(paste(y,m,d))) %>%
    
    ggplot(aes(x = Last_Update))+
    geom_line(aes(y=Confirmed, color = "Infected"))+
    geom_line(aes(y=Existing, color = "Existing"))+
    geom_line(aes(y=Recovered, color = "Recovered"))+
    geom_line(aes(y=Deaths, color = "Deaths"))+
    
    geom_point(size = I(3), shape = 1, aes(y=Confirmed, color = "Infected"))+
    geom_point(size = I(3), shape = 1,aes(y=Existing, color = "Existing"))+
    geom_point(size = I(3), shape = 1,aes(y=Recovered, color = "Recovered"))+
    geom_point(size = I(3), shape = 1,aes(y=Deaths, color = "Deaths"))+
    
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
                        values=c("green","red","blue","black")) +
    labs(title = "Coronavirus(2019-nCoV)",
         subtitle = paste(country,sep =" "))
}

plotDatContryUSA <- function(){
  dat %>%
    filter(`Country_Region` == 'US' & !grepl('County',`Province_State`) &
             !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1)) &
             Last_Update > '2020-03-09') %>%
    group_by( Last_Update) %>%
    summarise_if(is.numeric,sum) %>%
    
    ggplot(aes(x = Last_Update))+
    geom_line(aes(y=Confirmed, color = "Infected"))+
    geom_line(aes(y=Existing, color = "Existing"))+
    geom_line(aes(y=Recovered, color = "Recovered"))+
    geom_line(aes(y=Deaths, color = "Deaths"))+
    geom_point(size = I(3), shape = 1, aes(y=Confirmed, color = "Infected"))+
    geom_point(size = I(3), shape = 1,aes(y=Existing, color = "Existing"))+
    geom_point(size = I(3), shape = 1,aes(y=Recovered, color = "Recovered"))+
    geom_point(size = I(3), shape = 1,aes(y=Deaths, color = "Deaths"))+
    geom_hline(yintercept = seq(from = 100000, to = 35000000, by = 1000000))+
    
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
}

