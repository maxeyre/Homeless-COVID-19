library(shiny)
runApp("covid-19/")

rsconnect::setAccountInfo(name='care-protect', 
                          token='SECRET', 
                          secret='SECRET')


rsconnect::deployApp('covid-19/',
                     account='care-protect')

