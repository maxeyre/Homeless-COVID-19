# ui.R

library(shiny)
library(dplyr)
library(tidyverse)

# read in Andrew Lilley data
data <- read_csv("https://raw.githubusercontent.com/andrewlilley/tool_COVID-19/master/output_data/country_level.csv?token=ANJMHSHHDNKDPF4UV6YNJ4C6QYNTA")

if(any(colnames(data)=="X1")){
  data <- data %>%
    select(-X1)
}

data <- data %>%
  rename(country = Region, total_cases = Transmissions, total_deaths = Deaths)

data <- data[data$country!="UK",]
data$date = as.Date(data$date, "%Y-%m-%d")

UK.data <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/Data%20visualisation/UK%20data/UK_total.csv")
UK.data$date = as.Date(UK.data$date, "%d/%m/%Y")
UK.data$country <- "United Kingdom"

data <- rbind(data, UK.data)

data$country[data$country=="US"]<-"United States"
data$country[data$country=="UAE"]<-"United Arab Emirates"

data <- data[order(data$country),]

# list of countries
data$country <- as.character(data$country)
country.list <- c(unique(data$country))
list.50 <- list()
for (i in 1:length(country.list)){
  list.50[i] <- country.list[i]
}
names(list.50) <- country.list

# list of countries with >100 cases
data.100 <- data[data$total_cases>=100,]
country.list.100 <- c(unique(data.100$country))
list.100 <- list()
for (i in 1:length(country.list.100)){
  list.100[i] <- country.list.100[i]
}
names(list.100) <- country.list.100

data <- gather(data, key="type", value="number",3:ncol(data))

UK.data <- data[data$country=="United Kingdom",]

# relative dates - cases
data.100 <- data[data$type=="total_cases",]
data.100 <- data.100[data.100$number>=100,]
uni.country.100 <- c(unique(data.100$country))
data.100.out <- NULL
date.100 <- c(as.Date("2020-01-01","%Y-%m-%d"))
for (i in 1:length(uni.country.100)){
  x <- data.100[data.100$country==uni.country.100[i],]
  out <- as.Date(x$date[which(x$number>=100)],"%Y-%m-%d")
  out <- min(out)
  x$date_rel <- x$date - out
  x <- x[x$date_rel>=0,]
  data.100.out <- rbind(data.100.out, x)
}
data.100 <- data.100.out
data.100$date_rel <- as.numeric(data.100$date_rel)

# relative dates - deaths
data.deaths10 <- data[data$type=="total_deaths",]
data.deaths10 <- data.deaths10[data.deaths10$number>=10,]
uni.country.deaths10 <- c(unique(data.deaths10$country))
data.deaths10.out <- NULL
date.deaths10 <- c(as.Date("2020-01-01","%Y-%m-%d"))
for (i in 1:length(uni.country.deaths10)){
  x <- data.deaths10[data.deaths10$country==uni.country.deaths10[i],]
  out <- as.Date(x$date[which(x$number>=10)],"%Y-%m-%d")
  out <- min(out)
  x$date_rel <- x$date - out
  x <- x[x$date_rel>=0,]
  data.deaths10.out <- rbind(data.deaths10.out, x)
}
data.deaths10 <- data.deaths10.out
data.deaths10$date_rel <- as.numeric(data.deaths10$date_rel)

# read in UK county data
data.county <- "https://raw.githubusercontent.com/maxeyre/COVID-19/master/Data%20visualisation/UK%20data/england_countyUA.csv"
data.county <- read_csv(data.county)
data.county <- gather(data.county, key="date", value="cases",3:ncol(data.county))
data.county$date = as.Date(data.county$date, "%d/%m/%Y")

# get list of counties
data.county$county_UA <- as.character(data.county$county_UA)
county_LA.list <- c(unique(data.county$county_UA))
list.county <- list()
for (i in 1:length(county_LA.list)){
  list.county[i] <- county_LA.list[i]
}
names(list.county) <- county_LA.list

# read in England region data
data.region <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/Data%20visualisation/UK%20data/england_region.csv")
data.region <- gather(data.region, key="date", value="cases",3:ncol(data.region))
data.region$date = as.Date(data.region$date, "%d/%m/%Y")
data.region <- data.region %>%
  rename(region = NHSRNm)
# get list of regions
data.region$region <- as.character(data.region$region)
region.list <- c(unique(data.region$region))
list.region <- list()
for (i in 1:length(region.list)){
  list.region[i] <- region.list[i]
}
names(list.region) <- region.list

# Testing data
data.test <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/Data%20visualisation/UK%20data/UK_testing.csv")
data.test <- data.test[,1:4]
data.test <-na.omit(data.test)
data.test <- data.test %>%
  select(date, total_tested = tested)
data.test$date = as.Date(data.test$date, "%d/%m/%Y")
data.test$new_tested <- c(NA,diff(data.test$total_tested))
data.test <- gather(data.test, key="type", value="number",2:ncol(data.test))

# Define UI 
shinyUI(fluidPage(
  headerPanel("COVID-19 Data Visualisation"),
  navlistPanel(widths=c(2,9),
  # Application title
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
    "Worldwide",  
    tabPanel("By country",
             h5("Please use the menu bar on the left to navigate to different sections"),
             h3("Live epidemic curves by country"),
              h6("Data source: Collected directly from JHU CSSE sources by Andrew Lilley (updated every 24hrs)"),
              h6("Please note: Confirmed case data is entirely dependent on testing rates and will significantly underestimate actual number of infected individuals"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("country", "Country:",list.50),
                    
                    checkboxGroupInput("checkGroup", "", choices = list("Cases (daily)" = "new_cases", 
                                                  "Cases (total)" = "total_cases", 
                                                  "Deaths (daily)" = "new_deaths",
                                                  "Deaths (total)" = "total_deaths"),
                                       selected = 1),
                    dateRangeInput("dateRange", "Date range",
                               start  = min(data$date),
                               end    = max(data$date), 
                               min    = min(data$date),
                               max    = max(data$date)),
                    radioButtons("log", "y-axis scale:",
                                 choices=c('Linear'="log_no",
                                           'Log'='log_yes')),
                    radioButtons("pop_country", "Cases",
                                 choices=c('Number of cases'="pop_no",
                                           'Per 100,000 population'='pop_yes'))
                  ),
                mainPanel( 
                  h3(textOutput("caption")),
                  textOutput("startdate"),
                  plotOutput("countryPlot"),
                  h6("Made by Max Eyre"),
                  h6("Any comments, questions or suggestions please contact via twitter or max.eyre@lstmed.ac.uk"),
                  uiOutput("twitter"),
                  uiOutput("data_source"),
                  uiOutput("data_source_andrew"),
                  h6("Population data source - United Nations Population Division estimates (2019)")
                  )
                ),
    ),
  tabPanel("Country comparison",
           h5("Please use the menu bar on the left to navigate to different sections"),    
           h3("Live comparison of countries from beginning of outbreaks"),
              h5("Plotted for countries with at least 100 cases (day 0 is the first day >100 cases were reported)"),
               h6("Data source: Collected directly from JHU CSSE sources by Andrew Lilley (updated every 24hrs)"),
               h6("Please note: Confirmed case data is entirely dependent on testing rates and will significantly underestimate actual number of infected individuals"),
             sidebarLayout(
               sidebarPanel(
                 sliderInput("dateRange.100", "Number of days into outbreak (range)", 
                             min = min(data.100$date_rel), 
                             max = max(data.100$date_rel), value = c(min(data.100$date_rel), max(data.100$date_rel))),
                 radioButtons("log_compare", "y-axis scale:",
                              choices=c('Linear'="log_no",
                                        'Log'='log_yes')),
                 
                 checkboxGroupInput("checkGroup_countryCompare", "Countries", choices = list.100, selected = 1)
               ), 
               mainPanel(plotOutput("countryPlot_compare"),
                         h6("Made by Max Eyre"),
                         h6("Any comments, questions or suggestions please contact via twitter or max.eyre@lstmed.ac.uk"),
                         uiOutput("twitter_comp"),
                         uiOutput("data_source_comp"),
                         uiOutput("data_source_andrew_comp"),
                         h6("Population data source - United Nations Population Division estimates (2019)")
                )
             ),
  ),
  "United Kingdom",
  tabPanel("UK curve and overview",
           h5("Please use the menu bar on the left to navigate to different sections"),
           h3("UK Overview"),
           h6("Data source: Public Health England (updated every 24hrs)"),
           h6("Please note: Confirmed cases in the UK are now generally individuals presenting at hospitals"),
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput("checkGroup_UK", "", choices = list("Cases (daily)" = "new_cases", 
                                                                          "Cases (total)" = "total_cases"),selected = 2),
               dateRangeInput("dateRange_UK", "Date range",
                              start  = min(UK.data$date),
                              end    = max(UK.data$date), 
                              min    = min(UK.data$date),
                              max    = max(UK.data$date)),
               radioButtons("log_UK", "y-axis scale:",
                            choices=c('Linear'="log_no",
                                      'Log'='log_yes'))
             ),
             mainPanel(
               h5(textOutput("UK_newcase_update")),
               h5(textOutput("UK_totalcase_update")),
               h5(textOutput("UK_totalcase_update")),
               h3("Live epidemic curve of UK"),
               plotOutput("UKPlot"),
               h6("Made by Max Eyre"),
               h6("Any comments, questions or suggestions please contact via twitter or max.eyre@lstmed.ac.uk"),
               uiOutput("twitter_UK"),
               uiOutput("data_source_UK"),
               h6("Population data source - Office for National Statistics")
             )
           )
  ),
  tabPanel("NHS England regions",
           h5("Please use the menu bar on the left to navigate to different sections"),
           h3("Live epidemic curves by NHS England regions"),
           h6("Data source: Public Health England (updated every 24hrs)"),
           h6("Please note: Confirmed cases in the UK are now generally individuals presenting at hospitals"),
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput("checkGroup_region", "", choices = list("Cases (daily)" = "new_cases", 
                                                                          "Cases (total)" = "total_cases"),selected = 2),
               dateRangeInput("dateRange_region", "Date range",
                              start  = min(data.region$date),
                              end    = max(data.region$date), 
                              min    = min(data.region$date),
                              max    = max(data.region$date)),
               radioButtons("pop", "Cases",
                            choices=c('Number of cases'="pop_no",
                                      'Per 100,000 population'='pop_yes')),
               radioButtons("log_region", "y-axis scale:",
                            choices=c('Linear'="log_no",
                                      'Log'='log_yes'))
             ),
             mainPanel(
               plotOutput("EnglandRegionPlot"),
               h6("Made by Max Eyre"),
               h6("Any comments, questions or suggestions please contact via twitter or max.eyre@lstmed.ac.uk"),
               uiOutput("twitter2"),
               uiOutput("data_source2"),
               h6("Population data source - Office for National Statistics")
             )
           )
  ),
    tabPanel("England counties",
             h5("Please use the menu bar on the left to navigate to different sections"),
           h3("Live epidemic curves for counties of England (Unitary Areas)"),
           h6("Data source: Public Health England (updated every 24hrs)"),
           h6("Please note:"),
           h6("1. PHE data sometimes lose cases between days (hence negative new cases), hopefully this will improve with time."),
           h6("2. Confirmed case data is entirely dependent on testing rates and will significantly underestimate actual number of infected individuals"),
            sidebarLayout(
             sidebarPanel(
               selectInput("county", "County (UA):",list.county),
               checkboxGroupInput("checkGroup_county", "", choices = list("Cases (daily)" = "new_cases", 
                                                                   "Cases (total)" = "total_cases"),selected = 1),
               
               dateRangeInput("dateRange_county", "Date range",
                              start  = min(data.county$date),
                              end    = max(data.county$date), #max(data.confirmed$date)
                              min    = min(data.county$date),
                              max    = max(data.county$date))
               ),
             mainPanel(
               h5(textOutput("county_newcase_update")),
               h5(textOutput("county_totalcase_update")),
               h3(textOutput("caption_county")),
               plotOutput("englandcountyPlot"),
               h6("Made by Max Eyre"),
               h6("Any comments, questions or suggestions please contact via twitter or max.eyre@lstmed.ac.uk"),
               uiOutput("twitter3"),
               uiOutput("data_source3")
               )
           )
           ),
  tabPanel("UK Testing",
           h5("Please use the menu bar on the left to navigate to different sections"),
           h3("Live diagnostic testing rates for UK"),
           h6("Data source: Public Health England (updated every 24hrs)"),
           sidebarLayout(
             sidebarPanel(
               h4("Testing rates"),
               checkboxGroupInput("checkGroup_test", "", choices = list("Tested (daily)" = "new_tested", 
                                                                          "Tested (total)" = "total_tested"),selected = 1),
               dateRangeInput("dateRange_test", "Date range",
                              start  = min(data.test$date),
                              end    = max(data.test$date), 
                              min    = min(data.test$date),
                              max    = max(data.test$date)),
               radioButtons("log_test", "y-axis scale:",
                            choices=c('Linear'="log_no",
                                      'Log'='log_yes'))
             ),
             mainPanel(plotOutput("UKtestingPlot")
                       )
           ),
           br(),
           sidebarLayout(
             sidebarPanel(
               h4("Proportion positive"),
               checkboxGroupInput("checkGroup_test2", "", choices = list("% positive (daily)" = "new_prop_pos", 
                                                                        "% positive (total)" = "total_prop_pos"),selected = 1),
               dateRangeInput("dateRange_test2", "Date range",
                              start  = min(data.test$date),
                              end    = max(data.test$date), 
                              min    = min(data.test$date),
                              max    = max(data.test$date))
             ),
             mainPanel(plotOutput("UKtestingPlot2"),
                       h6("Made by Max Eyre"),
                       h6("Any comments, questions or suggestions please contact via twitter or max.eyre@lstmed.ac.uk"),
                       uiOutput("twitter4"),
                       uiOutput("data_source4")
             )
           )
           ),
  h5(textOutput("counter"))
  )
)
)
  


