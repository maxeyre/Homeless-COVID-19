# COVID-19 and homelessness in England 
### A modelling study of the COVID-19 pandemic among people experiencing homelessness and the impact of a residential intervention to isolate vulnerable people and treat people with symptoms

## Key outputs
- Report pre-print can be accessed [here](www.google.com)
- R Shiny app for policy makers currently accessible at https://care-protect.shinyapps.io/covid-19/

## Background to COVID-PROTECT and COVID-CARE
Health and housing authorities in England have established a plan to provide single room own-bathroom accommodation and care to homeless adults in England during the COVID-19 pandemic. As part of this plan, nurses or other trained staff will visit hostels, soup kitchens, day centres and other services for homeless people, and use a triage tool to determine eligibility. People with symptoms of COVID-19 will be offered ‘COVID-CARE’ accommodation, where isolation, observation, medical support and referral to hospitals are provided. People who are asymptomatic but vulnerable to severe disease will be offered ‘COVID-PROTECT’ accommodation, which is designed to reduce the risk of becoming infected. As of April 2020, the programme is in the early phases of implementation in London and is under review by other UK regions.

## About this model
This model forecasts the demand for COVID-CARE and COVID-PROTECT and their impact on deaths and hospital use. It is intended for people who are planning COVID-CARE and COVID-PROTECT or trying to understand its possible benefits. The model uses a 'discrete time Markov chain' method, which means that it assumes that individuals have a certain probibility of moving between different statuses, such as the probability of developing COVID-19, or the probability of accepting an offer to move into COVID-PROTECT accomodation. The model helps to understand what might happen under certain assumptions.

## Repository contents
- [Model](https://github.com/maxeyre/Homeless-COVID-19/tree/master/model)
  - Functions for model ([R Script](https://github.com/maxeyre/Homeless-COVID-19/blob/master/model/covid_model21.R))
  - Running model and creating plots ([R Script](https://github.com/maxeyre/Homeless-COVID-19/blob/master/model/covid_homless_model_chart.R))
  - [UK hostel population data](https://github.com/maxeyre/Homeless-COVID-19/blob/master/model/hl_hostel_beds.csv)
  - [LSHTM UK COVID-19 daily incidence predictions](https://github.com/maxeyre/Homeless-COVID-19/blob/master/model/lshtm_ld_1000.csv)
- [R Shiny code](https://github.com/maxeyre/Homeless-COVID-19/tree/master/covid-19)

## Contact Us
This model was built by a research group led by the [UCL Collaborative Centre for Inclusion Health](https://www.ucl.ac.uk/inclusion-health).
