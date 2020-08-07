# COVID-19 among people experiencing homelessness in England: a modelling study 

## Key outputs
- Report pre-print can be accessed [here](https://www.medrxiv.org/content/10.1101/2020.05.04.20079301v1.full.pdf)
- Main article [ADD LINK]
- R Shiny app for policy makers currently accessible at https://care-protect.shinyapps.io/covid-19/. Note that this application was developed at an early stage of the COVID-19 pandemic and was designed to inform planning of interventions for people experience homelessness. It does replicate the model that is reported in the main article.

## About this model
This model estimates the impact of COVID-19 on people experiencing homeless in England. It uses estimates of the impact on this population during the 'first wave' (up to 31 May 2020) to calibrate model assumptions, and then explores the potential future impact in different scenarios. The model uses a 'discrete time Markov chain' method, which means that it assumes that individuals have a certain probibility of moving between different statuses, such as the probability of developing COVID-19, or the probability of accepting an offer to move into COVID-PROTECT accomodation.

## Key results
[ADD ABSTRACT HERE ONCE PUBLISHED]

## Repository contents
- [Model](https://github.com/maxeyre/Homeless-COVID-19/tree/master/model)
  - Functions for model ([R Script](https://github.com/maxeyre/Homeless-COVID-19/blob/master/model/model_function.R))
  - Running model and creating outputs ([R Script](https://github.com/maxeyre/Homeless-COVID-19/blob/master/model/multiple_model_runs.R))
  - Model inputs / data (files with the prefix 'INPUT_')
  - Supporting data and files that have been used to create model inputs. These are not needed to run the model (as the results are already saved in the repository), and are provided for completeness and transparency (files with the prefix 'SUPPORTING_')
- [R Shiny code](https://github.com/maxeyre/Homeless-COVID-19/tree/master/covid-19), which is provided as an archive and does not replicate the most recent version of the model.

## Contact Us
This model was built by a research group led by the [UCL Collaborative Centre for Inclusion Health](https://www.ucl.ac.uk/inclusion-health).
