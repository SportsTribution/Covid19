library(tidyverse)
library(ggrepel)
library(rvest)
library(lubridate)
library(xkcd)


url <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"
population <- url %>%
  xml2::read_html()%>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[*]') %>%
  html_table() %>%
  bind_rows() %>%
  rename(Country=2)%>%
  mutate(Country=str_replace(Country,"\\[.*\\]",""),
         Population=as.numeric(str_replace_all(Population,",","")))

##### Cases per 1 Mio Inhabitants`,y=`Case Fatality rate (in %) ####

url1 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
confirmed_cases <- read_csv(url1)%>%
  rename(Country=2,value=(ncol(.)))%>%
  group_by(Country)%>%
  summarise(confirmed_cases=sum(value))

confirmed_cases_2days <- read_csv(url1)%>%
  rename(Country=2,value=(ncol(.)-2))%>%
  group_by(Country)%>%
  summarise(confirmed_cases_2days=sum(value))

last_date <- read_csv(url1)%>%
  colnames()%>%
  tail(1)

url2 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
covid_deaths <- read_csv(url2)%>%
  rename(Country=2,value=(ncol(.)))%>%
  group_by(Country)%>%
  summarise(covid_deaths=sum(value))

covid_deaths_2days <- read_csv(url2)%>%
  rename(Country=2,value=(ncol(.)-2))%>%
  group_by(Country)%>%
  summarise(covid_deaths_2days=sum(value))

df <- confirmed_cases %>%
  left_join(confirmed_cases_2days)%>%
  left_join(covid_deaths)%>%
  left_join(covid_deaths_2days)%>%
  mutate(Country=recode(Country,"Korea, South"="South Korea",US="United States"))%>%
  left_join(population)%>%
  filter(confirmed_cases>500 & !is.na(Population))%>%
  mutate(`Cases per 1 Mio Inhabitants`=confirmed_cases/Population*1000000)%>%
  mutate(`Case Fatality rate (in %)` = covid_deaths/confirmed_cases*100)%>%
  mutate(cases_2days=confirmed_cases_2days/Population*1000000)%>%
  mutate(cfr_2days = covid_deaths_2days/confirmed_cases_2days*100)


ggplot(df,aes(x=`Cases per 1 Mio Inhabitants`,y=`Case Fatality rate (in %)`,label=Country,colour=Country))+
  geom_point()+
  scale_x_continuous(trans='log10')+
  geom_text_repel()+
  geom_point(aes(x=cases_2days,y=cfr_2days,colour=Country))+
  labs(subtitle = paste("COVID-19 per country in relative numbers, as of",mdy(last_date)), 
       caption = "Data: https://github.com/CSSEGISandData/COVID-19, Viz: Johannes Becker")

# ##### Growth rate ####
lag_steps <- 3
url1 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
confirmed_cases <- read_csv(url1)%>%
  rename(Country=2)%>%
  # filter(Country %in% c("Italy","Korea, South","Germany","Spain","Norway","France","United Kingdom","US","Switzerland"))%>%
  filter(Country %in% c("Italy","Germany","Norway","Switzerland","US","France","Spain"))%>%
  gather(Date,Cases,-Country,-Lat,-Long,-`Province/State`)%>%
  mutate(Date=mdy(Date))%>%
  group_by(Country,Date)%>%
  summarise(Cases=sum(Cases)) %>%
  mutate(PreviousCases=dplyr::lag(Cases,lag_steps,order_by=Date))%>%
  mutate(PreviousDate=dplyr::lag(Date,lag_steps,order_by=Date))%>%
  mutate(DiffTime=as.integer(Date-PreviousDate))%>%
  filter(!is.na(PreviousCases)&Cases>=100)%>%
  mutate(`Cases Doubling Time`=log(2)/log((Cases/PreviousCases)^(1/DiffTime)))

  
ggp <- ggplot(confirmed_cases,aes(x=Date,y=`Cases Doubling Time`,color=Country)) +
  geom_smooth(method = "loess",se = FALSE, size = 2, span= 1) +
  geom_point() +
  theme_xkcd()+
  labs(subtitle = paste("Doubling time of confirmed cases as of",mdy(last_date)), 
       caption = "Data: John Hopkins University, Viz: Johannes Becker")

ggsave(paste("growth_cases",mdy(last_date),".png",ggp)



url2 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
covid_deaths <- read_csv(url2)%>%
  rename(Country=2)%>%
  filter(Country %in% c("Italy","Germany","Spain","France","US"))%>%
  gather(Date,Cases,-Country,-Lat,-Long,-`Province/State`)%>%
  mutate(Date=mdy(Date))%>%
  group_by(Country,Date)%>%
  summarise(Cases=sum(Cases)) %>%
  mutate(PreviousCases=dplyr::lag(Cases,lag_steps,order_by=Date))%>%
  mutate(PreviousDate=dplyr::lag(Date,lag_steps,order_by=Date))%>%
  mutate(DiffTime=as.integer(Date-PreviousDate))%>%
  filter(!is.na(PreviousCases)&Cases>=10)%>%
  mutate(`Death Doubling Time`=log(2)/log((Cases/PreviousCases)^(1/DiffTime)))

ggplot(covid_deaths,aes(x=Date,y=`Death Doubling Time`,color=Country)) +
  geom_smooth(method = "loess",se = FALSE, size = 2, span= 2) +
  geom_point() +
  theme_xkcd()
