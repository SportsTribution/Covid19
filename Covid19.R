library(tidyverse)
library(ggrepel)
library(rvest)


url <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"
population <- url %>%
  xml2::read_html()%>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[*]') %>%
  html_table() %>%
  bind_rows() %>%
  rename(Country=2)%>%
  mutate(Country=str_replace(Country,"\\[.*\\]",""),
         Population=as.numeric(str_replace_all(Population,",","")))


url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
confirmed_cases <- read_csv(url)%>%
  rename(Country=2,value=(ncol(.)))%>%
  group_by(Country)%>%
  summarise(confirmed_cases=sum(value))

last_date <- read_csv(url)%>%
  colnames()%>%
  tail(1)

url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
covid_deaths <- read_csv(url)%>%
  rename(Country=2,value=(ncol(.)))%>%
  group_by(Country)%>%
  summarise(covid_deaths=sum(value))

df <- confirmed_cases %>%
  left_join(covid_deaths)%>%
  mutate(Country=recode(Country,"Korea, South"="South Korea",US="United States"))%>%
  left_join(population)%>%
  filter(confirmed_cases>500 & !is.na(Population))%>%
  mutate(`Cases per 1 Mio Inhabitants`=confirmed_cases/Population*1000000)%>%
  mutate(`Case Fatality rate (in %)` = covid_deaths/confirmed_cases*100)


ggplot(df,aes(x=`Cases per 1 Mio Inhabitants`,y=`Case Fatality rate (in %)`,label=Country))+
  geom_point()+
  geom_text_repel()+
  labs(subtitle = paste("COVID-19 per country in relative numbers, as of",last_date), 
       caption = "Data: https://github.com/CSSEGISandData/COVID-19, Viz: Johannes Becker")


