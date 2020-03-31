# ui.R

library(shiny)

# Define UI 
shinyUI(fluidPage(
  navbarPage("",
             tabPanel("Model",
                      headerPanel("CARE-PROTECT COVID-19 Modelling Tool"),
                      h5("Please see the About page for a definition of all modelling parameters"),
                      sidebarPanel(
                        # epidemic parameters
                        sliderInput("outbreak_duration", label = "Outbreak duration (days)", min = 0, max = 200, value = 110),
                        sliderInput("peak_day", label = "Peak day", min = 0, max = 200, value = 40),
                        sliderInput("covid_attack_hostel", label = "Attack rate in hostels (% of pop. infected)", min = 0, max = 1, value = 0.8),
                        sliderInput("covid_attack_rough_sleepers", label = "Attack rate in rough sleepers (% of pop. infected)", min = 0, max = 1, value = 0.5),
                        sliderInput("PROTECT_incidence_fraction", label = "Incidence reduction factor due to PROTECT", min = 0, max = 1, value = 0.5),
                        
                        # population
                        numericInput("hostel_population", label = "Hostel population", value = 8784, max= 50000),
                        numericInput("rough_sleeping_population", label = "Rough sleeping population", value = 1136, max= 50000),
                        sliderInput("proportion_vulnerable", label = "Proportion vulnerable", min = 0, max = 1, value = 0.5)
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
  tabPanel("About",
           headerPanel("CARE-PROTECT COVID-19 Modelling Tool"),
           h3("This Model"),
           h6("Add text"),
           h3("The CARE-PROTECT Project"),
           h6("Add text"),
           h3("Contact Us"),
           h6("Add text, can add links to Github, twitter, websites etc.")
           ),
  tabPanel("Anything else?",
           headerPanel("CARE-PROTECT COVID-19 Modelling Tool")
           )
)
)
)



