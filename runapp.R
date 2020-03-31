library(shiny)
runApp("covid-19/")

rsconnect::setAccountInfo(name='care-protect', 
                          token='18E557A4A9837A91E1F4D68FAD85857F', 
                          secret='f28+oywLhBEiiCq16QaQ8EhT6V5WeCUYt5hU8c47')


rsconnect::deployApp('covid-19/',
                     account='care-protect')

