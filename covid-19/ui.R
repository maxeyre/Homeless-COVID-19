# ui.R

# code for editing CSS directly (add id="first" in fluidRow)
# tags$style(HTML("
#       #first {
#           border: 2px groove gray;
#       }
#       #second {
#           border: 2px dashed blue;
#       }
#     ")),

library(shiny)
region.pop <- read.csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/homeless_pop.csv")
region.pop$region <- as.character(region.pop$region)
add <- c("Custom", 10000, 10000)
region.pop <- rbind(region.pop,add)

list.regions <- as.list(region.pop$region)
names(list.regions) <- region.pop$region


# Define UI 
shinyUI(fluidPage(
  navbarPage("Menu", position="fixed-top", inverse=TRUE,
               tabPanel("Model",
                        tags$head(
                          tags$style(type="text/css", "body {padding-top: 70px;}")
                        ),
                        titlePanel("CARE-PROTECT COVID-19 Modelling Tool", windowTitle = "CARE-PROTECT COVID-19 Modelling Tool"),
                        h5("Please see the About page for an explanation of modelling parameters"),
                        br(),
                        h3("1. Key parameters"),
                        h6("Please choose parameters values for the scenario you wish to model"),
                        br(),
                        h4("Population"),
                        fluidRow(
                          column(3,
                                 selectInput("region_choice", label = "Choose region", 
                                             choices = list.regions, 
                                             selected = "Custom"),
                                 ),
                          column(3,
                                 numericInput("hostel_population", label = "Hostel population", value = 10000, max= 50000)
                          ),
                          column(3,
                                 numericInput("rough_sleeping_population", label = "Rough sleeping population", value = 10000, max= 50000)
                          ),
                          column(3,
                                 sliderInput("proportion_vulnerable", label = "Proportion vulnerable", min = 0, max = 1, value = 0.5)
                          )
                        ),
                        h4("PROTECT/CARE"),
                        fluidRow(
                          column(3,
                                 sliderInput("PROTECT_incidence_fraction", label = "% reduction in incidence due to PROTECT", min = 0, max = 1, value = 0.5)
                          ),
                          column(3,
                                 sliderInput("probability_identified", label = "Prop. identified for CARE/PROTECT", min = 0, max = 1, value = 0.7)
                          ),
                          column(3,
                                 sliderInput("self_discharge_risk", label = "Risk of self discharge (from CARE and PROTECT)", min = 0, max = 1, value = 0.33)
                          ),
                          column(3,
                                 sliderInput("accept_CARE", label = "Prop. accepting CARE/PROTECT", min = 0, max = 1, value = 0.7)
                          ),
                        ),
                        h3("2. Outputs - Graphs"),
                        h6("Use the tabs below to view each graphical output (may take ~10s to load)"),
                        
                        fluidRow(
                          column(12,
                                   tabsetPanel(type = "tabs",
                                               tabPanel("Epidemic curve",  
                                                        h3("Epidemic curve"),
                                                        plotOutput("plot_EpiCurve")
                                               ),
                                               tabPanel("Statuses",
                                                        h3("Stacked graph of statuses"),
                                                        plotOutput("plot_Stacked")
                                               ),
                                               tabPanel("Healthcare use", 
                                                        h3("Healthcare use"),
                                                        plotOutput("plot_HealthcareUse")
                                               ),
                                               tabPanel("CARE-PROTECT", 
                                                        h3("CARE-PROTECT usage"),
                                                        plotOutput("plot_CAREPROTECT")
                                               ),
                                               tabPanel("Deaths", 
                                                        h3("Deaths"),
                                                        plotOutput("plot_deaths")
                                               )
                                   
                                 )
                          )
                        ),
                        actionButton("action", label = "Add/remove 'no intervention' results"),
                        
                        h3("3. Outputs - Key estimates"),
                        fluidRow(
                          column(5,
                                 h4("Epidemic overview"),
                                 tableOutput("table_epidemic")
                                 ),
                          column(5,
                                 h4("Healthcare use"),
                                 tableOutput("table_healthcare")
                          )
                        ),
                        
                        br(),
                        h3("4. Other parameters"),
                        h6("These parameters are set at our current best guess values"),
                        fluidRow(
                          column(4,
                                 h4("Interventions"),
                                 radioButtons("all_protect", "Everyone offered PROTECT",choices=c('Yes'='TRUE','No'='FALSE')),
                                 radioButtons("testing", "Testing for COVID-19",choices=c('Yes'='TRUE','No'='FALSE')),
                                 sliderInput("duration_CARE", label = "Duration of CARE (days)", min = 0, max = 30, value = 14),
                                 sliderInput("duration_PROTECT_recruitment", label = "PROTECT recruitment period (days)", min = 0, max = 60, value = 28),
                                 numericInput("rr_vulnerable", "Risk ratio for vulnerable individuals",value=9),
                                 numericInput("rr_CARE", "Risk ratio for individuals in CARE",value=0.5),
                                 radioButtons("max_protect_binary", "Does PROTECT have a maximum capacity",choices=c('No'='FALSE','Yes'='TRUE')),
                                 numericInput("max_protect_value", "Maximum capacity of PROTECT",value=NA)
                          ),
                          
                          column(4,
                                 h4("Epidemic dynamics"),
                                 sliderInput("outbreak_duration", label = "Outbreak duration (days)", min = 0, max = 200, value = 110),
                                 sliderInput("peak_day", label = "Peak day", min = 0, max = 200, value = 40),
                                 sliderInput("covid_attack_hostel", label = "Attack rate in hostels", min = 0, max = 1, value = 0.8),
                                 sliderInput("covid_attack_rough_sleepers", label = "Attack rate in rough sleepers", min = 0, max = 1, value = 0.5)
                          ),
                          
                          column(4,
                                 h4("Timings"),
                                 sliderInput("admission_day", label = "Time to hospital/ITU admission (days)", min = 0, max = 30, value = 7),
                                 sliderInput("duration_admission", label = "Time spent in hospital/ITU (days)", min = 0, max = 30, value = 12),
                                 sliderInput("time_to_results", label = "Time to results (days)", min = 0, max = 30, value = 2),
                                 sliderInput("self_discharge_day", label = "Time to self discharge (days)", min = 0, max = 30, value = 4),
                                 sliderInput("died_covid_day", label = "Time to death due to COVID-19 (days) ", min = 0, max = 90, value = 19),
                                 sliderInput("duration_covid", label = "Time to recovery (days)", min = 0, max = 90, value = 15)
                          ),
                        ),
               ),
               
               tabPanel("About",
                        tags$head(
                          tags$style(type="text/css", "body {padding-top: 70px;}")
                        ),
                         h3("Who is this model for?"),
                         h6("Add text"),
                         h3("How can it be used?"),
                         h6("Add text"),
                         h3("The CARE-PROTECT Project"),
                         uiOutput("url"), # add the link
                         h6("Add text"),
                         h3("Contact Us"),
                         h6("Add text, can add links to Github, twitter, websites etc."),
                         h3("Model structure, assumptions etc."),
                         h6("Add text")
               )
  )
)
)