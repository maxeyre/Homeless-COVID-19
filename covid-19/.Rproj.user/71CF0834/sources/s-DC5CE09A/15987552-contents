# ui.R

library(shiny)
library(dplyr)
library(tidyverse)

# read in data
data <- "http://cowid.netlify.com/data/full_data.csv"
data <- read_csv(data)
data[is.na(data)] <- 0

data$date = as.Date(data$date, "%Y-%m-%d")

data <- data %>%
  rename(country=location)

data <- data[order(data$country),]
# get list of countries with more than 50 cases



data$country <- as.character(data$country)

country.list <- c(unique(data$country))
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
                                      "Cases (total)" = "total_cases", 
                                      "Deaths (daily)" = "new_deaths",
                                      "Deaths (total)" = "total_deaths"),
                                      #"Recovered (daily)" ="new_recovered",
                                      #"Recovered (total)" = "recovered"),
                       selected = 1),
    dateRangeInput("dateRange", "Date range",
                  start  = min(data$date),
                  end    = max(data$date), #max(data.confirmed$date)
                  min    = min(data$date),
                  max    = max(data$date)) #max(data.confirmed$date)
  ),
  
  mainPanel(
    h3("Live epidemic curves"),
    h6("Data source: Our world in data (updated every 24hrs)"),
    h6("Please note: Confirmed case data is entirely dependent on testing rates and will significantly underestimate actual number of infected individuals"),

    tabsetPanel(type = "tabs",
                tabPanel("By country",  h3(textOutput("caption")),
                         textOutput("startdate"),
                         plotOutput("countryPlot"),),
                tabPanel("Country comparisons", verbatimTextOutput("summary")),
                tabPanel("UK counties", tableOutput("table"))
    ),
    
    h6("Made by Max Eyre"),
    h6("Any comments, questions or suggestions please contact via twitter or max.eyre@lstmed.ac.uk"),
    uiOutput("twitter"),
    uiOutput("data_source")
  )
))
