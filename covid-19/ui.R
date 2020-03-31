# ui.R

library(shiny)

# Define UI 
shinyUI(pageWithSidebar(
  headerPanel("CARE-PROTECT COVID-19 Modelling Tool"),
  sidebarPanel(
    sliderInput("total_days", label = "Epidemic length (days)", min = 0, max = 200, value = 110),
    sliderInput("peak_day", label = "Peak day", min = 0, max = 200, value = 40),
    numericInput("hostel_population", label = "Hostel population", value = 8784),
    numericInput("rough_sleeping_population", label = "Rough sleeping population", value = 1136)
    
  ),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Epidemic curve",  
                         h3("Epidemic curve"),
                         plotOutput("plot_EpiCurve"),
                         textOutput("peak_day"),
                         textOutput("hostel")),
                tabPanel("Statuses",
                         h3("Stacked graph of statuses"),
                         plotOutput("plot_Stacked")),
                tabPanel("Healthcare use", 
                         h3("Healthcare use"),
                         plotOutput("plot_HealthcareUse")),
                tabPanel("CARE-PROTECT", 
                         h3("CARE-PROTECT usage"),
                         plotOutput("plot_CAREPROTECT"))
                         )
    )
  )
)



