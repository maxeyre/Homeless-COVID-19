# ui.R

library(shiny)

# Define UI 
shinyUI(fluidPage(
  navbarPage("Menu",
             tabPanel("Model",
                      titlePanel("CARE-PROTECT COVID-19 Modelling Tool", windowTitle = "CARE-PROTECT COVID-19 Modelling Tool"),
                      h5("Please see the About page for a definition of all modelling parameters"),
                      sidebarPanel(
                        # epidemic parameters
                        sliderInput("outbreak_duration", label = "Outbreak duration (days)", min = 0, max = 200, value = 110),
                        sliderInput("peak_day", label = "Peak day", min = 0, max = 200, value = 40),
                        sliderInput("covid_attack_hostel", label = "Attack rate in hostels", min = 0, max = 1, value = 0.8),
                        sliderInput("covid_attack_rough_sleepers", label = "Attack rate in rough sleepers", min = 0, max = 1, value = 0.5),
                        sliderInput("PROTECT_incidence_fraction", label = "PROTECT incidence reduction factor", min = 0, max = 1, value = 0.5),
                        
                        # population
                        numericInput("hostel_population", label = "Hostel population", value = 8784, max= 50000),
                        numericInput("rough_sleeping_population", label = "Rough sleeping population", value = 1136, max= 50000),
                        sliderInput("proportion_vulnerable", label = "Proportion vulnerable", min = 0, max = 1, value = 0.5),
                        
                        # interventions
                        radioButtons("all_protect", "Everyone offered PROTECT",choices=c('Yes'='TRUE','No'='FALSE')),
                        radioButtons("testing", "Testing (*add more detail*)",choices=c('Yes'='TRUE','No'='FALSE')),
                        sliderInput("probability_identified", label = "Prop. identified for CARE/PROTECT", min = 0, max = 1, value = 0.7),
                        sliderInput("duration_CARE", label = "Duration of CARE (days)", min = 0, max = 30, value = 14),
                        sliderInput("accept_CARE", label = "Prop. accepting CARE", min = 0, max = 1, value = 0.7),
                        sliderInput("duration_PROTECT_recruitment", label = "PROTECT recruitment period (days)", min = 0, max = 60, value = 28),
                        sliderInput("accept_PROTECT", label = "Prop. accepting PROTECT", min = 0, max = 1, value = 0.7),
                        sliderInput("self_discharge_risk", label = "Risk of self discharge (from hospital, CARE and PROTECT?)", min = 0, max = 1, value = 0.33),
                        
                        # timings (average)
                        sliderInput("admission_day", label = "Time to hospital/ITU admission (days)", min = 0, max = 30, value = 7),
                        sliderInput("duration_admission", label = "Time spent in hospital/ITU (days)", min = 0, max = 30, value = 12),
                        sliderInput("time_to_results", label = "Time to results (days)", min = 0, max = 30, value = 2),
                        sliderInput("self_discharge_day", label = "Time to self discharge (days)", min = 0, max = 30, value = 4),
                        sliderInput("died_covid_day", label = "Time to death due to COVID-19 (days) ", min = 0, max = 90, value = 19),
                        sliderInput("duration_covid", label = "Time to recovery (days)", min = 0, max = 90, value = 15)
                        ),
    
  mainPanel(
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
                         )
                ),
    h6("Add site visit counter")
    )
  ),
  tabPanel("Guidance",
           titlePanel("CARE-PROTECT COVID-19 Modelling Tool", windowTitle = "CARE-PROTECT COVID-19 Modelling Tool"),
           h3("Who is this model for?"),
           h6("Add text"),
           h3("How can it be used?"),
           h6("Add text")
  ),
  
  tabPanel("Model definition",
           titlePanel("CARE-PROTECT COVID-19 Modelling Tool", windowTitle = "CARE-PROTECT COVID-19 Modelling Tool"),
           h3("Model structure, assumptions etc."),
           h6("Add text")
  ),
  tabPanel("About",
           titlePanel("CARE-PROTECT COVID-19 Modelling Tool", windowTitle = "CARE-PROTECT COVID-19 Modelling Tool"),
           h3("The CARE-PROTECT Project"),
           h6("Add text"),
           h3("Contact Us"),
           h6("Add text, can add links to Github, twitter, websites etc.")
           )
)
)
)



