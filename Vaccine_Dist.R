library(tidyverse)
library(data.table)

library(RSelenium)
library(rvest)
library(broom)

usStatePopulation <- fread("R/2019-coronavirus-dataset-01212020-01262020/nst-est2019-01.csv",header = TRUE)

usStatePopulation <-
  usStatePopulation %>%
  mutate(`Geographic Area` = ifelse(grepl('[[:punct:]]+',`Geographic Area`),str_sub(`Geographic Area`,2,-1),`Geographic Area`)) %>%
  select(`Geographic Area`,`2019`)

url <- "https://covid.cdc.gov/covid-data-tracker/#vaccinations"

# open web browser
rD <- rsDriver(browser="firefox", port=4545L, verbose=F)
remDr <- rD[["client"]]

# open web site
remDr$navigate(url)

html <- remDr$getPageSource()[[1]]

signals <- read_html(html) %>% # parse HTML
  html_nodes("table") %>% # extract table nodes with class = "tbl"
  html_table(fill=T) # have rvest turn it into a dataframe

datDist <- as.data.table(signals[[1]])


html2 <- remDr$getPageSource()[[1]]

signals2 <- read_html(html2) %>% # parse HTML
  html_nodes("table") %>% # extract table nodes with class = "tbl"
  html_table(fill=T) # have rvest turn it into a dataframe

datDist2 <- as.data.table(signals2[[1]])

datDist <- 
  datDist %>%
  select("State/Territory/Federal Entity",
         "Total Doses Administered by State where Administered",
         "Doses Administered per 100k by State where Administered")

names(datDist) <- c('State','Administered','Admin_100_k')

datDist2 <- 
  datDist2 %>%
  select("State/Territory/Federal Entity",
         "Total Doses Delivered",
         "Doses Delivered per 100K")

names(datDist2)<- c('State','Distributed','Dist_100_k')

datDist <-
datDist %>%
  left_join(datDist2,by=c("State"))

#names(datDist) <- c('State','Distributed','Dist_100_k','Administered','Admin_100_k')

str(datDist)



datDista =
  datDist %>%
    mutate(Administered = ifelse(grepl( "N/A",Administered),NA,Administered),
           Admin_100_k = ifelse(grepl("N/A",Admin_100_k),NA,Admin_100_k)) %>%
    mutate(Date = lubridate::ymd(Sys.Date()),
           Dist_100_k = as.numeric(Dist_100_k),
           Admin_100_k = as.numeric(Admin_100_k),
           Administered = as.numeric(Administered),
           Distributed = as.numeric(Distributed),
           Percent_Administered = Administered/Distributed)

datDi <- rbind(
  fread("vaccine_dist.csv") %>%
    mutate(Date = lubridate::ymd(Date)),
  datDista ) 

write.csv(datDist, file =  paste0("vaccine_dis_day",Sys.Date(),".csv"))

write.csv(datDi, file = "vaccine_dist.csv",row.names = FALSE)
write.csv(datDi, file =  paste0("vaccine_dis_",Sys.Date(),".csv"))


datDi <- 
 datDi %>%
  mutate( 
       Date = as.IDate(lubridate::ymd(Date)))

datDi %>%
  ggplot(aes(x=Distributed,y=Administered)) +
  geom_point()

lm(Administered ~ Distributed, data = datDi)


# plot by state
# total distributed and total administered
datDi %>%
  filter(Date == max(Date)) %>%
  arrange(Distributed) %>%
  ggplot(aes(y=factor(State, level = State))) +
  geom_point(aes(x=Distributed, color = "Distributed")) +
  geom_point(aes(x=Administered, color = "Administered")) +
  ylab(label = 'State') +
  xlab(label = 'Count') +
  ggtitle(label = "Covid 19 Vaccine, Total Distributed | Total Administered") +
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
                      breaks=c("Distributed","Administered"),
                      values=c("black","blue"))

# plot by day
datDi %>%
  group_by(Date) %>%
  summarise(Distributed = sum(Distributed, na.rm = TRUE),
            Administered = sum(Administered, na.rm = TRUE)) %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=Distributed, color = "Distributed")) +
  geom_line(aes(y=Administered, color = "Administered")) +
  geom_point(shape =1,aes(y=Distributed, color = "Distributed")) +
  geom_point(shape =1,aes(y=Administered, color = "Administered")) +
  ylab(label = "Doses") +
  ggtitle(label = "Total Dose USA") +
  theme(legend.justification=c(1,0), legend.position=c(.225,0.75))+
  theme(legend.title=element_text(size=12,face="bold"),
        legend.background = element_rect(fill='#FFFFFF',
                                         size=0.5,linetype="solid"),
        legend.text=element_text(size=10),
        legend.key=element_rect(colour="#FFFFFF",
                                fill='#C2C2C2',
                                size=0.25,
                                linetype="solid"))+
  scale_colour_manual("Compartments",
                      breaks=c("Distributed","Administered"),
                      values=c("black","blue"))

datDi %>%
  filter(State == "Michigan") %>%
  group_by(Date) %>%
  summarise(Distributed = sum(Distributed),
            Administered = sum(Administered)) %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=Distributed, color = "Distributed")) +
  geom_line(aes(y=Administered, color = "Administered")) +
  geom_point(shape =1,aes(y=Distributed, color = "Distributed")) +
  geom_point(shape =1,aes(y=Administered, color = "Administered")) +
  ylab(label = "Doses") +
  ggtitle(label = "Total Dose Michigan") +
  theme(legend.justification=c(1,0), legend.position=c(.225,0.75))+
  theme(legend.title=element_text(size=12,face="bold"),
        legend.background = element_rect(fill='#FFFFFF',
                                         size=0.5,linetype="solid"),
        legend.text=element_text(size=10),
        legend.key=element_rect(colour="#FFFFFF",
                                fill='#C2C2C2',
                                size=0.25,
                                linetype="solid"))+
  scale_colour_manual("Compartments",
                      breaks=c("Distributed","Administered"),
                      values=c("black","blue"))

datDi %>%
filter(State == "Michigan") %>%
  group_by(Date) %>%
  summarise(Distributed = sum(Distributed),
            Administered = sum(Administered),
            Delta_Dist_Admin = Distributed - Administered) %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=Delta_Dist_Admin, color = "Distributed")) +
  geom_point(shape =1,aes(y=Delta_Dist_Admin, color = "Distributed")) +
  ylab(label = "Doses") +
  ggtitle(label = "Total Dose Michigan") +
  theme(legend.justification=c(1,0), legend.position=c(.225,0.75))+
  theme(legend.title=element_text(size=12,face="bold"),
        legend.background = element_rect(fill='#FFFFFF',
                                         size=0.5,linetype="solid"),
        legend.text=element_text(size=10),
        legend.key=element_rect(colour="#FFFFFF",
                                fill='#C2C2C2',
                                size=0.25,
                                linetype="solid"))+
  scale_colour_manual("Compartments",
                      breaks=c("Distributed","Administered"),
                      values=c("black","blue"))
# average doses USA
datDi %>%
  group_by(Date) %>%
  summarise(Distributed = sum(Distributed),
            Administered = sum(Administered)) %>%
  mutate(Dist_Day = Distributed - lag(Distributed,default = 0),
         Admin_Day = Administered - lag(Administered,default = 0)) %>%
  summarise(avg_dist_day = mean(Dist_Day,na.rm = TRUE),
            avg_admin_day = mean(Admin_Day,na.rm = TRUE))

# average doses Michigan
datDi %>%
  filter(State == "Michigan") %>%
  group_by(Date) %>%
  summarise(Distributed = sum(Distributed),
            Administered = sum(Administered)) %>%
  mutate(Dist_Day = Distributed - lag(Distributed,default = 0),
         Admin_Day = Administered - lag(Administered,default = 0)) %>%
  summarise(avg_dist_day = mean(Dist_Day),
            avg_admin_day = mean(Admin_Day))


#Regression -> days to complete
datDi %>%
  group_by(State,Date) %>%
  summarise(Distributed = sum(Distributed),
            Administered = sum(Administered)) %>%
  ungroup() %>%
  group_by(State) %>%
  do(fitAdmin = tidy(lm(Administered ~ Date, data = .))) %>% 
  unnest(fitAdmin) %>%
  filter(term == 'Date') %>%
  left_join(select(usStatePopulation,`Geographic Area`,'2019'), by = c('State' = 'Geographic Area')) %>%
  filter(!is.na(`2019`)) %>%
  mutate(`2019` = as.numeric(gsub(",","",`2019`,fixed=TRUE))) %>%
  mutate(Days_to_admin = `2019` / estimate) %>%
  arrange(desc(Days_to_admin))

datDi %>%
  group_by(State,Date) %>%
  summarise(Distributed = sum(Distributed),
            Administered = sum(Administered)) %>%
  ungroup() %>%
  group_by(State) %>%
  do(fitAdmin = tidy(lm(Administered ~ Date, data = .))) %>% 
  unnest(fitAdmin) %>%
  filter(term == 'Date') %>%
  left_join(select(usStatePopulation,`Geographic Area`,'2019'), by = c('State' = 'Geographic Area')) %>%
  filter(!is.na(`2019`)) %>%
  mutate(`2019` = as.numeric(gsub(",","",`2019`,fixed=TRUE))) %>%
  mutate(Days_to_admin = `2019` / estimate) %>%
  arrange(desc(Days_to_admin)) %>%
  ggplot(aes(y=factor(State, level = State))) +
  #geom_point(aes(x=Distributed, color = "Administered")) +
  geom_point(aes(x=Days_to_admin, color = "Administered")) +
  ylab(label = 'State') +
  xlab(label = 'Days to Administed') +
  ggtitle(label = "Covid 19 Vaccine, Total Distributed | Total Administered") +
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
                      breaks=c("Distributed","Administered"),
                      values=c("black","blue"))
  
  
# calculate remaining to vaccinate by state
dataTotalVac =
datDi %>%
  group_by(State,Date) %>%
  summarise(Distributed = sum(Distributed),
            Administered = sum(Administered)) %>%
  ungroup() %>%
  filter(Date == max(Date)) %>%
  left_join(select(usStatePopulation,`Geographic Area`,'2019'), by = c('State' = 'Geographic Area')) %>%
  filter(!is.na(`2019`)) %>%
  mutate(`2019` = as.numeric(gsub(",","",`2019`,fixed=TRUE))) %>%
  mutate(To_Vaccinate = `2019` - Administered)

# days to vaccinate less vaccinated

datDi %>%
  group_by(State,Date) %>%
  summarise(Distributed = sum(Distributed),
            Administered = sum(Administered)) %>%
  ungroup() %>%
  group_by(State) %>%
  do(fitAdmin = tidy(lm(Administered ~ Date, data = .))) %>% 
  unnest(fitAdmin) %>%
  filter(term == 'Date') %>%
  left_join(select(usStatePopulation,`Geographic Area`,'2019'), by = c('State' = 'Geographic Area')) %>%
  filter(!is.na(`2019`)) %>%
  mutate(`2019` = as.numeric(gsub(",","",`2019`,fixed=TRUE))) %>%
  left_join(select(dataTotalVac,State,To_Vaccinate), by = c('State')) %>%
  mutate(Days_to_admin = To_Vaccinate / estimate) %>%
  arrange(desc(Days_to_admin)) %>%
  ggplot(aes(y=factor(State, level = State))) +
  #geom_point(aes(x=Distributed, color = "Administered")) +
  geom_point(aes(x=Days_to_admin, color = "To_Admin")) +
  ylab(label = 'State') +
  xlab(label = 'Days to Administered') +
  ggtitle(label = "Covid 19 Vaccine, Estimated Days To Population Vaccinated by State") +
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
                      breaks=c("Distributed","To_Admin"),
                      values=c("black","blue"))



datDi %>%
  filter(State == "Michigan") %>%
  group_by(Date) %>%
  summarise(Distributed = sum(Distributed),
            Administered = sum(Administered)) %>%
  mutate(Dist_Day = Distributed - lag(Distributed,default = 0),
         Admin_Day = Administered - lag(Administered,default = 0)) %>%
  filter(Date > '2021-01-12') %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=Dist_Day, color = "Distributed")) +
  geom_line(aes(y=Admin_Day, color = "Administered")) +
  geom_point(shape =1,aes(y=Dist_Day, color = "Distributed")) +
  geom_point(shape =1,aes(y=Admin_Day, color = "Administered")) +
  geom_hline(yintercept = 50000) +
  ylab(label = "Doses") +
  ggtitle(label = "Total Dose per day Michigan") +
  theme(legend.justification=c(1,0), legend.position=c(.225,0.75))+
  theme(legend.title=element_text(size=12,face="bold"),
        legend.background = element_rect(fill='#FFFFFF',
                                         size=0.5,linetype="solid"),
        legend.text=element_text(size=10),
        legend.key=element_rect(colour="#FFFFFF",
                                fill='#C2C2C2',
                                size=0.25,
                                linetype="solid"))+
  scale_colour_manual("Compartments",
                      breaks=c("Distributed","Administered"),
                      values=c("black","blue"))



datDi %>%
  filter(!grepl('Federated',State) ,
         !grepl('Heal',State),
         !grepl('Dept',State),
         !grepl('Pris',State),
         !grepl('Marshall',State),
         !grepl('Maria',State),
         !grepl('Republic',State),
         !grepl('Samoa',State),
         !grepl('Islands',State),
         !grepl('Guam',State)
  )%>%
  #filter(State == "Michigan") %>%
  group_by(State,Date) %>%
  summarise(Distributed = sum(Distributed),
            Administered = sum(Administered)) %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=Distributed, color = "Distributed")) +
  geom_line(aes(y=Administered, color = "Administered")) +
  #geom_point(shape =1,aes(y=Distributed, color = "Distributed")) +
  #geom_point(shape =1,aes(y=Administered, color = "Administered")) +
  ylab(label = "Doses") +
  ggtitle(label = "Total Dose per day USA") +
  facet_wrap(vars(State)) +
  theme(legend.justification=c(1,0), legend.position=c(.925,0.001))+
  theme(legend.title=element_text(size=12,face="bold"),
        legend.background = element_rect(fill='#FFFFFF',
                                         size=0.5,linetype="solid"),
        legend.text=element_text(size=10),
        legend.key=element_rect(colour="#FFFFFF",
                                fill='#C2C2C2',
                                size=0.25,
                                linetype="solid"),
        axis.text.x = element_text(angle = 90) )+
  scale_colour_manual("Compartments",
                      breaks=c("Distributed","Administered"),
                      values=c("black","blue"))


datDi %>%
  filter(!grepl('Federated',State) ,
         !grepl('Heal',State),
         !grepl('Dept',State),
         !grepl('Pris',State),
         !grepl('Marshall',State),
         !grepl('Maria',State),
         !grepl('Republic',State),
         !grepl('Samoa',State),
         !grepl('Islands',State),
         !grepl('Guam',State)
  )%>%
  #filter(State == "Michigan") %>%
  group_by(State,Date) %>%
  summarise(Distributed = sum(Dist_100_k),
            Administered = sum(Admin_100_k)) %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=Distributed, color = "Distributed")) +
  geom_line(aes(y=Administered, color = "Administered")) +
  #geom_point(shape =1,aes(y=Distributed, color = "Distributed")) +
  #geom_point(shape =1,aes(y=Administered, color = "Administered")) +
  ylab(label = "Doses") +
  ggtitle(label = "Total Doses USA Normalized") +
  facet_wrap(vars(State)) +
  theme(legend.justification=c(1,0), legend.position=c(.90,0.001))+
  theme(legend.title=element_text(size=12,face="bold"),
        legend.background = element_rect(fill='#FFFFFF',
                                         size=0.5,linetype="solid"),
        legend.text=element_text(size=10),
        legend.key=element_rect(colour="#FFFFFF",
                                fill='#C2C2C2',
                                size=0.25,
                                linetype="solid"),
        axis.text.x = element_text(angle = 90) )+
  scale_colour_manual("Compartments",
                      breaks=c("Distributed","Administered"),
                      values=c("black","blue"))

# Normalized administered and distributed by state
datDi %>%
  filter(Date == max(Date)) %>%
  filter(!grepl('Federated',State) ,
         !grepl('Heal',State),
         !grepl('Dept',State),
         !grepl('Pris',State),
         !grepl('Marshall',State),
         !grepl('Maria',State),
         !grepl('Republic',State),
         !grepl('Samoa',State),
         !grepl('Islands',State),
         !grepl('Guam',State)
  )%>%
  arrange(Admin_100_k) %>%
  ggplot(aes(y=factor(State, level = State))) +
  geom_point(aes(x=Dist_100_k, color = "Distributed" )) +
  geom_point(aes(x=Admin_100_k, color = "Administered")) +
  ylab(label = 'State') +
  xlab(label = 'Count_100_K') +
  ggtitle(label = "Normalized Total Distributed | Total Administered") +
  theme(legend.justification=c(1,0), legend.position=c(.825,0.25))+
  theme(legend.title=element_text(size=12,face="bold"),
        legend.background = element_rect(fill='#FFFFFF',
                                         size=0.5,linetype="solid"),
        legend.text=element_text(size=10),
        legend.key=element_rect(colour="#FFFFFF",
                                fill='#C2C2C2',
                                size=0.25,
                                linetype="solid"))+
  scale_colour_manual("Compartments",
                      breaks=c("Distributed","Administered"),
                      values=c("black","blue"))
  
# percent administered by state
datDi %>%
  filter(Date == max(Date)) %>%
  filter(!grepl('Federated',State) ,
         !grepl('Heal',State),
         !grepl('Dept',State),
         !grepl('Pris',State),
         !grepl('Marshall',State),
         !grepl('Islands',State),
         !grepl('Maria',State)
  )%>%
  arrange(Percent_Administered) %>%
  ggplot(aes(y=factor(State, level = State))) +
  geom_point(aes(x=Percent_Administered)) +
  #geom_point(color = 'blue',aes(x=Admin_100_k)) +
  ylab(label = 'State') +
  xlab(label = 'Percent') +
  ggtitle(label = "Percent Adminstered By State") 

# Confirmed, infection rate, and administered

notIn <- c('Princess', 'Guam', 'Samoa', 'Islands','Rico','Evacuee','Recovered')

coeff <-  100  # Value used to transform the data

# normalized confirmed, infection rate, and administered
datUS %>%
  filter(`Country_Region` == 'US' & !grepl('County',`Province_State`) &
           !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1)) &
           Last_Update > '2020-03-09') %>%
  filter(!grepl(paste(notIn, collapse="|"),Province_State)) %>%
  arrange(Province_State,Last_Update) %>%
  group_by( Province_State,Last_Update) %>%
  summarise_if(is.numeric,sum) %>%
  select(Last_Update,Confirmed,Deaths,Recovered,Existing = Active, Province_State) %>%
  left_join(select(usStatePopulation,`Geographic Area`,'2019'), by = c('Province_State' = 'Geographic Area')) %>%
  mutate(`2019` = as.numeric(gsub(",","",`2019`,fixed=TRUE))) %>%
  rename(Pop = `2019`) %>%
  mutate(Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
         Country_Region = 'USA')%>%
  mutate(Roll_Rate_Confirmed = zoo::rollmean(Rate_Confirmed,7,mean,align='right',fill=Rate_Confirmed)) %>%
  ungroup() %>%
  mutate(Normalized_Roll_Rate_Confirmed = 100000 * Roll_Rate_Confirmed/Pop,
         Normalized_Rate_Confirmed = 100000 * Rate_Confirmed/Pop,
         Normalized_Confirmed = 100000 * Confirmed/Pop) %>%
  rename("State" = "Province_State", "Date" = "Last_Update" ) %>%
  mutate(Date = as.IDate(Date)) %>%
  left_join(select(datDi,-Percent_Administered),by=c("Date","State")) %>%
  filter(Date > "2021-01-05",
         Normalized_Rate_Confirmed >0,
         Normalized_Rate_Confirmed < 750) %>%
  ggplot(aes(x = Date)) +
  geom_line(color = "black",aes(y=Normalized_Rate_Confirmed*coeff))+
  geom_line(color = 'blue',aes(y=Normalized_Confirmed))+
  geom_line(color ='red',aes(y=Admin_100_k)) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Conf / Admin_100_k",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="Rate_Conf_100_k")
  ) + 
  facet_wrap(vars(State)) +
  xlab(label="Date")+
  theme(legend.position = "none") +
  labs(title = "Coronavirus(2019-nCoV)",
       subtitle = paste("United States Of America",sep =" "))



rateLines <- data.table(slope = c(2,5,10,20,50,100,500)/20,intercept = c(0,0,0,0,0,0,0))

datUS %>%
  filter(`Country_Region` == 'US' & !grepl('County',`Province_State`) &
           !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1)) &
           Last_Update > '2021-01-01') %>%
  filter(!grepl(paste(notIn, collapse="|"),Province_State)) %>%
  arrange(Province_State,Last_Update) %>%
  group_by( Province_State,Last_Update) %>%
  summarise_if(is.numeric,sum) %>%
  select(Last_Update,Confirmed,Recovered, Province_State) %>%
  left_join(select(usStatePopulation,`Geographic Area`,'2019'), by = c('Province_State' = 'Geographic Area')) %>%
  mutate(`2019` = as.numeric(gsub(",","",`2019`,fixed=TRUE))) %>%
  rename(Pop = `2019`) %>%
  mutate(Rate_Confirmed = Confirmed - lag(Confirmed,default = 0))%>%
  #mutate(Roll_Rate_Confirmed = zoo::rollmean(Rate_Confirmed,7,mean,align='right',fill=Rate_Confirmed)) %>%
  ungroup() %>%
  mutate(
    Rate_Confirmed_100_k = 100000 * Rate_Confirmed/Pop,
    Confirmed_100_k = 100000 * Confirmed/Pop) %>%
  rename("State" = "Province_State", "Date" = "Last_Update" ) %>%
  mutate(Date = as.IDate(Date)) %>%
  left_join(select(datDi,-Percent_Administered),by=c("Date","State")) %>%
  filter(Date > "2021-01-05") %>%
  filter(Date == "2021-01-15" |Date == "2021-01-21" |
           Date == "2021-02-01" |Date == "2021-02-13" |Date == "2021-03-01" |
           Date == max(Date)) %>%
  mutate(Rate_Confirmed = Confirmed - lag(Confirmed,default = 0))%>%
  group_by(State) %>%
  mutate(Rate_Admin = Administered - lag(Administered,default = 0)) %>%
  ungroup() %>%
  mutate(Rate_Admin_100_k = Rate_Admin*10000/Pop) %>%
  filter(Date > "2021-01-14",
         Rate_Admin_100_k > 0,
         Rate_Confirmed_100_k > 0) %>%
  ggplot(aes(x=Rate_Confirmed_100_k,y=Rate_Admin_100_k,color=State)) +
  geom_point() +
  geom_text(aes(x=Rate_Confirmed_100_k,y=Rate_Admin_100_k,label=State),nudge_x = 5, nudge_y = 2) +
  #scale_y_continuous(limits = c(0,10000)) +
  geom_abline(data = rateLines,aes(slope = slope,intercept = intercept),linetype = 2, color = 'grey') +
  facet_grid( .~Date) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90))

rateLines <- data.table(slope = c(0,10,20,50,100,500)/20,intercept = c(0,0,0,0,0,0))


datUS %>%
  filter(`Country_Region` == 'US' & !grepl('County',`Province_State`) &
           !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1)) &
           Last_Update > '2021-01-01') %>%
  filter(!grepl(paste(notIn, collapse="|"),Province_State)) %>%
  arrange(Province_State,Last_Update) %>%
  group_by( Province_State,Last_Update) %>%
  summarise_if(is.numeric,sum) %>%
  select(Last_Update,Confirmed,Recovered, Province_State) %>%
  left_join(select(usStatePopulation,`Geographic Area`,'2019'), by = c('Province_State' = 'Geographic Area')) %>%
  mutate(`2019` = as.numeric(gsub(",","",`2019`,fixed=TRUE))) %>%
  rename(Pop = `2019`) %>%
  mutate(Rate_Confirmed = Confirmed - lag(Confirmed,default = 0))%>%
  #mutate(Roll_Rate_Confirmed = zoo::rollmean(Rate_Confirmed,7,mean,align='right',fill=Rate_Confirmed)) %>%
  ungroup() %>%
  mutate(
    Rate_Confirmed_100_k = 100000 * Rate_Confirmed/Pop,
    Confirmed_100_k = 100000 * Confirmed/Pop) %>%
  rename("State" = "Province_State", "Date" = "Last_Update" ) %>%
  mutate(Date = as.IDate(Date)) %>%
  left_join(select(datDi,-Percent_Administered),by=c("Date","State")) %>%
  filter(Date > "2021-01-05") %>%
  mutate(Rate_Confirmed = Confirmed - lag(Confirmed,default = 0))%>%
  group_by(State) %>%
  mutate(Rate_Admin = Administered - lag(Administered,default = 0)) %>%
  ungroup() %>%
  mutate(Rate_Admin_100_k = Rate_Admin*10000/Pop) %>%
  filter(Date >= max(Sys.Date()),
         Rate_Admin_100_k > 0,
         Rate_Confirmed_100_k > 0) %>%
  ggplot(aes(x=Rate_Confirmed_100_k,y=Rate_Admin_100_k,color=State)) +
  geom_point() +
  geom_text(aes(x=Rate_Confirmed_100_k,y=Rate_Admin_100_k,label=State),nudge_x = 0.5, nudge_y = 2,hjust = 0) +
  #scale_y_continuous(limits = c(0,120)) +
  #scale_x_continuous(limits = c(0,120)) +
  geom_abline(data = rateLines,aes(slope = slope,intercept = intercept),linetype = 2, color = 'grey') +
  #facet_grid( .~Date) +
  theme(legend.position = "none")

# load the vaccine distribution data

datModerna <- fread("R/2019-coronavirus-dataset-01212020-01262020/COVID-19_Vaccine_Distribution_Allocations_by_Jurisdiction_-_Moderna.csv")
datPhizer <- fread("R/2019-coronavirus-dataset-01212020-01262020/COVID-19_Vaccine_Distribution_Allocations_by_Jurisdiction_-_Pfizer.csv")
datJJ <- fread("R/2019-coronavirus-dataset-01212020-01262020/COVID-19_Vaccine_Distribution_Allocations_by_Jurisdiction_-_Janssen.csv")

datSupply =
  rbind(
datPhizer %>%
  filter(Jurisdiction != 'Total') %>%
  select(!starts_with('Total')) %>%
  pivot_longer(
    cols = contains('Dose'),
    names_to = 'note',
    values_to = 'doses',
    values_drop_na = TRUE
  ) %>%
  filter(doses != 'N/A') %>%
  mutate(
    Supplier = 'Phizer',
    Dose_num = ifelse(grepl('Second',note),2,1),
    Date = lubridate::mdy(`Week of Allocations`),
    doses = gsub(',','',doses),
    doses = as.numeric(doses),
    Jurisdiction = gsub('\\*','',Jurisdiction)
  ),

datModerna %>%
  filter(Jurisdiction != 'Total') %>%
  select(!starts_with('Total')) %>%
  pivot_longer(
    cols = contains('Dose'),
    names_to = 'note',
    values_to = 'doses',
    values_drop_na = TRUE
  ) %>%
  filter(doses != 'N/A') %>%
  mutate(
    Supplier = 'Moderena',
    Dose_num = ifelse(grepl('Second',note),2,1),
    Date = lubridate::mdy(`Week of Allocations`),
    doses = gsub(',','',doses),
    doses = as.numeric(doses),
    Jurisdiction = gsub('\\*','',Jurisdiction)
  ),
datJJ %>%
  filter(Jurisdiction != 'Total') %>%
  select(!starts_with('Total')) %>%
  pivot_longer(
    cols = contains('Dose'),
    names_to = 'note',
    values_to = 'doses',
    values_drop_na = TRUE
  ) %>%
  filter(doses != 'N/A') %>%
  mutate(
    Supplier = 'JJ',
    Dose_num = ifelse(grepl('Second',note),2,1),
    Date = lubridate::mdy(`Week of Allocations`),
    doses = gsub(',','',doses),
    doses = as.numeric(doses),
    Jurisdiction = gsub('\\*','',Jurisdiction)
  )

) %>%
  select(!note) %>%
  left_join(select(usStatePopulation,`Geographic Area`,'2019'), by = c('Jurisdiction' = 'Geographic Area')) %>%
  rename(Population = `2019`)

# data showing weeks to show time until vacinated based on state vacinations rates

datSupply %>%
  group_by(Dose_num,Jurisdiction) %>%
  summarise(
    weekly_avg=mean(doses)
  ) %>%
  left_join(select(usStatePopulation,`Geographic Area`,'2019'), by = c('Jurisdiction' = 'Geographic Area')) %>%
  rename(Population = `2019`) %>%
  mutate(Population = as.numeric(gsub(',','',Population)),
    Weeks_to_Doses_Available = Population/weekly_avg) %>%
  left_join(select(datDi,State,Percent_Administered),by=c('Jurisdiction' = 'State')) %>%
  mutate(weekly_admin = weekly_avg * Percent_Administered,
         Weeks_to_Vaccinated =  Population/weekly_admin)

datDiDi =
datDi %>%
  filter(Date == max(Date))


datSupply %>%
  group_by(Dose_num,Jurisdiction) %>%
  summarise(
    dayly_avg=mean(doses)
  ) %>%
  left_join(select(usStatePopulation,`Geographic Area`,'2019'), by = c('Jurisdiction' = 'Geographic Area')) %>%
  rename(Population = `2019`) %>%
  mutate(Population = as.numeric(gsub(',','',Population)),
         days_to_Doses_Available = Population/dayly_avg) %>%
  filter(!is.na(Population)) %>%
  ungroup() %>%
  left_join(select(datDiDi,State,Percent_Administered),by=c('Jurisdiction' = 'State')) %>%
  mutate(dayly_admin = dayly_avg * Percent_Administered,
         days_to_Vaccinated =  Population/dayly_admin) %>%
  filter(Dose_num == 1,
         !is.na(days_to_Vaccinated)) %>%
  arrange(days_to_Vaccinated) %>%
  ggplot(aes(y = factor(Jurisdiction, level = Jurisdiction))) +
  geom_point(aes(x = days_to_Doses_Available, color = "Distributed")) +
  geom_point(aes(x = days_to_Vaccinated, color = "Administered")) +
  ylab(label = 'State') +
  xlab(label = 'Days') +
  ggtitle(label = "Days To Distrubuted | Administered By State") +
  theme(legend.justification=c(1,0), legend.position=c(.825,0.15))+
  theme(legend.title=element_text(size=12,face="bold"),
        legend.background = element_rect(fill='#FFFFFF',
                                         size=0.5,linetype="solid"),
        legend.text=element_text(size=10),
        legend.key=element_rect(colour="#FFFFFF",
                                fill='#C2C2C2',
                                size=0.25,
                                linetype="solid")) +
  scale_colour_manual("Compartments",
                      breaks=c("Distributed","Administered"),
                      values=c("blue","black"))



datSupply %>%
  group_by(Dose_num,Jurisdiction,Supplier) %>%
  summarise(
    mean(doses)
  ) %>%
  filter(Jurisdiction == 'Michigan')

datSupply %>%
  mutate(Date = lubridate::mdy(`Week of Allocations`)) %>%
  filter(Dose_num == 1) %>%
  rename('State' = 'Jurisdiction') %>%
  arrange(Supplier,State,Date) %>%
  group_by(State, Supplier) %>%
  mutate(C_Sum_Dose = cumsum(doses)) %>%
  ggplot(aes(x=C_Sum_Dose,y=State,color=Supplier)) +
  geom_line(color = 'black') +
  geom_point()

datSupply %>%
  mutate(Date = lubridate::mdy(`Week of Allocations`)) %>%
  filter(Dose_num == 1) %>%
  rename('State' = 'Jurisdiction') %>%
  arrange(Supplier,State,Date) %>%
  group_by(State, Supplier) %>%
  mutate(C_Sum_Dose = cumsum(doses)) %>%
  ggplot(aes(x=C_Sum_Dose,y=State,color=Supplier)) +
  geom_line(color = 'black') +
  geom_point() +
  facet_grid(.~Supplier)

Pop =
usStatePopulation %>%
  rename('State' = `Geographic Area`,'Population' = '2019') %>%
  mutate(Population = as.numeric(gsub(',','',Population))) %>%
  filter(State != 'United States')%>%
  filter(State != "West",
         State != "East",
         State != "South",
         State != "North",
         State != "Midwest",
         State != "Northeast") 



datSupply %>%
  mutate(Date = lubridate::mdy(`Week of Allocations`)) %>%
  rename('State' = 'Jurisdiction') %>%
  #filter(State == "West") %>%
  filter(Dose_num == 1) %>%
  arrange(State,Date) %>%
  group_by(State, Date) %>%
  summarise(dose_sum = sum(doses)) %>%
  ungroup() %>%
  group_by(State) %>%
  mutate(C_Sum_Dose = cumsum(dose_sum)) %>%
  ungroup() %>%
  left_join(Pop, by= 'State') %>%
  filter(!is.na(Population)) %>%
  ggplot(aes(y=State)) +
  geom_line(color = 'black',aes(x=C_Sum_Dose)) +
  geom_point(aes(x=C_Sum_Dose)) +
  geom_point(data = Pop,aes(x = Population),color = 'blue') +
  xlab(label = "Count") +
  ggtitle(label = "State Population [Blue] , Cumlative Administed [Black]")

datSupply %>%
  mutate(Date = lubridate::mdy(`Week of Allocations`)) %>%
  rename('State' = 'Jurisdiction') %>%
  #filter(State == "West") %>%
  filter(Dose_num == 1) %>%
  arrange(State,Date) %>%
  group_by(State, Date) %>%
  summarise(dose_sum = sum(doses)) %>%
  ungroup() %>%
  group_by(State) %>%
  mutate(C_Sum_Dose = cumsum(dose_sum)) %>%
  ungroup() %>%
  ggplot(aes(y=State)) +
  geom_line(color = 'black',aes(x=C_Sum_Dose)) +
  geom_point(aes(x=C_Sum_Dose)) 
  
 




datSupply %>%
  mutate(Date = lubridate::mdy(`Week of Allocations`)) %>%
  rename('State' = 'Jurisdiction') %>%
  #filter(State == "West") %>%
  filter(Dose_num == 1) %>%
  arrange(State,Date) %>%
  group_by(State, Date) %>%
  summarise(dose_sum = sum(doses)) %>%
  ungroup() %>%
  group_by(State) %>%
  mutate(C_Sum_Dose = cumsum(dose_sum)) %>%
  ungroup() %>%
  left_join(Pop, by= 'State') %>%
  filter(!is.na(Population)) %>%
  mutate(Date = as.IDate(Date)) %>%
  left_join(select(datDi,State,Date,Percent_Administered), by = c('State','Date')) %>%
  mutate(C_Sum_Admin = C_Sum_Dose * Percent_Administered) %>%
  
  filter(!is.na(C_Sum_Admin)) %>%
  #filter(State == 'Michigan')
  group_by(State) %>%
  do(fitAdmin = tidy(lm(C_Sum_Admin ~ Date, data = .))) %>% 
  unnest(fitAdmin) %>%
  filter(State == 'Michigan')


inf_Day <- function(state){
  dat %>%
    filter(`Country_Region` == 'US' & !grepl('County',`Province_State`) &
             !grepl("^[[:upper:]]+$",str_sub(`Province_State`,-2,-1)) &
             Last_Update > '2020-03-09') %>%
    filter(Province_State == state) %>%
    group_by( Last_Update) %>%
    summarise_if(is.numeric,sum) %>%
    select(Last_Update,Confirmed,Deaths,Recovered,Existing) %>%
    mutate(Rate_Confirmed = Confirmed - lag(Confirmed,default = 0),
           Country_Region = 'USA',
           Province_State = state) %>%
    mutate(Roll_Rate_Confirmed = zoo::rollmean(Rate_Confirmed,7,mean,align='right',fill=Rate_Confirmed)) %>%
    
    ungroup() %>%
    mutate(Last_Update = as.IDate(Last_Update )) %>%
    left_join(select(datDi,-Percent_Administered),by=c("Last_Update" = "Date", "Province_State" = "State")) %>%
    filter(Last_Update > '2021-01-01') %>%
    
    mutate(Distributed = Distributed/1000,
           Administered = Administered/1000) %>%
    #group_by(`Province_State`) %>% 
    
    ggplot(aes(x = Last_Update))+
    #geom_line(aes(y=Rate_Confirmed, color = "Infections_per_day"))+
    geom_line(aes(y=Roll_Rate_Confirmed, color = "Rolling_Infrections_per_day"))+
    geom_line(aes(y=Distributed, color = "Distributed"))+
    geom_line(aes(y=Administered, color = "Administered"))+
    
    #geom_point(size = I(3), shape = 1, aes(y=Rate_Confirmed, color = "Infections_per_day"))+
    #geom_point(size = I(3), shape = 1,aes(y=Roll_Rate_Confirmed, color = "Rolling_Infrections_per_day"))+
    
    geom_point(size = I(1), shape = 1,aes(y=Distributed, color = "Distributed"))+
    geom_point(size = I(1), shape = 1,aes(y=Administered, color = "Administered"))+
    
    #facet_grid(.~Country_Region) +
    ylab(label="Count | Dist and Admin / 1000")+
    xlab(label="Date")+
    theme(legend.justification=c(1,0), legend.position=c(0.65,0.75))+
    theme(legend.title=element_text(size=12,face="bold"),
          legend.background = element_rect(fill='#FFFFFF',
                                           size=0.5,linetype="solid"),
          legend.text=element_text(size=10),
          legend.key=element_rect(colour="#FFFFFF",
                                  fill='#C2C2C2',
                                  size=0.25,
                                  linetype="solid"))+
    scale_colour_manual("Compartments",
                        breaks=c("Infections_per_day","Existing","Recovered","Rolling_Infrections_per_day","Distributed","Administered"),
                        values=c("green","red","blue","black","darkblue","lightblue"))+
    labs(title = "Coronavirus(2019-nCoV)",
         subtitle = paste(state,sep =" "))
  
}

inf_Day('California')
inf_Day('New Jersey')
inf_Day('New York')
inf_Day('Michigan')
inf_Day('Ohio')
inf_Day( 'Washington')
inf_Day('Illinois')
inf_Day( 'Louisiana')
inf_Day('Georgia')
inf_Day( 'Texas')
inf_Day( 'Colorado')
inf_Day( 'Florida')
inf_Day( 'Alabama')
inf_Day( 'Mississippi')
inf_Day( 'Indiana')
inf_Day( 'Wisconsin')
inf_Day( 'Arkansas')
inf_Day( 'Arizona')
inf_Day( 'North Carolina')
inf_Day( 'South Carolina')
inf_Day( 'North Dakota')
inf_Day( 'South Dakota')
inf_Day( 'Indiana')
inf_Day( 'Tennessee')
inf_Day( 'Iowa')
inf_Day( 'Nevada')
inf_Day( 'Minnesota')
inf_Day( 'Hawaii')
inf_Day( 'West Virginia')
inf_Day( 'Missouri')


