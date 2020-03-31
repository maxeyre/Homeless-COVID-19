# ui.R

library(shiny)
library(dplyr)
library(tidyverse)

# read in data
confirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
data.confirmed <- read_csv(confirmed)

#data.no.US <- data.confirmed[data.confirmed$`Country/Region`!="US",]

#data.confirmed <- data.no.US

# tidy up
data.confirmed <- gather(data.confirmed, key="date", value="cases",5:ncol(data.confirmed))

data.confirmed <- data.confirmed %>%
  rename(prov_state=`Province/State`, country=`Country/Region`) %>%
  mutate(date = as.Date(date, "%m/%d/%y"))

data.confirmed <- data.confirmed %>%
  group_by(country, date) %>%
  summarise(cases=sum(cases))

data.confirmed$date = as.Date(data.confirmed$date, "%Y-%m-%d")

# get list of countries with more than 50 cases


data.confirmed$country <- as.character(data.confirmed$country)
data.confirmed$country[data.confirmed$country=="Taiwan*"] <- "Taiwan"

country.list <- c(unique(data.confirmed$country))
#country.list <- c(country.list[(1:(which(country.list=="Others"))-1)],country.list[(which(country.list=="Others")+1):length(country.list)])


list.50 <- list()

for (i in 1:length(country.list)){
  list.50[i] <- country.list[i]
}
names(list.50) <- country.list


# Define UI 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("COVID-19 Epidemic Curves"),
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    selectInput("country", "Country:",
                list.50),
    checkboxGroupInput("checkGroup", "",
                       choices = list("Cases (daily)" = "new_cases", 
                                      "Cases (total)" = "cases", 
                                      "Deaths (daily)" = "new_deaths",
                                      "Deaths (total)" = "deaths",
                                      "Recovered (daily)" ="new_recovered",
                                      "Recovered (total)" = "recovered"),
                       selected = 1),
    dateRangeInput("dateRange", "Date range",
                  start  = min(data.confirmed$date),
                  end    = (max(data.confirmed$date)-1), #max(data.confirmed$date)
                  min    = min(data.confirmed$date),
                  max    = (max(data.confirmed$date)-1)) #max(data.confirmed$date)
  ),
  
  mainPanel(
    h3("Live epidemic curves for all reporting countries"),
    h6("Data source: Johns Hopkins CSSE (live data feed)"),
    h6("Please note: date range restricted to 11/03/2020 because JH CSSE data feed updated incorrectly on 12/03/2020"),

    h3(textOutput("caption")),
    textOutput("startdate"),
    plotOutput("countryPlot"),
    h6("Made by Max Eyre"),
    h6("Any comments, questions or suggestions please contact via twitter or max.eyre@lstmed.ac.uk"),
    uiOutput("twitter"),
    uiOutput("data_source")
  )
))
