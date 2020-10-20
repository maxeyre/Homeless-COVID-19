# COVID-19 among people experiencing homelessness in England: a modelling study 

## Key outputs
- Article published in Lancet Respiratory Medicine [here](https://www.thelancet.com/journals/lanres/article/PIIS2213-2600(20)30396-9/fulltext)
- R Shiny app for policy makers currently accessible at https://care-protect.shinyapps.io/covid-19/. Note that this application was developed at an early stage of the COVID-19 pandemic and was designed to inform planning of interventions for people experience homelessness. It does not does not replicate the most recent version of the model reported in the article.

## About this model
This model estimates the impact of COVID-19 on people experiencing homeless in England. It uses estimates of the impact on this population during the 'first wave' (up to 31 May 2020) to calibrate model assumptions, and then explores the potential future impact in different scenarios. The model uses a 'discrete time Markov chain' method, which means that it assumes that individuals have a certain probibility of moving between different statuses, such as the probability of developing COVID-19, or the probability of accepting an offer to move into COVID-PROTECT accomodation.

## Repository contents
- [Model](https://github.com/maxeyre/Homeless-COVID-19/tree/master/model)
  - The function that runs the model once ([R Script](https://github.com/maxeyre/Homeless-COVID-19/blob/master/model/model_function.R))
  - Running model and creating outputs ([R Script](https://github.com/maxeyre/Homeless-COVID-19/blob/master/model/main_model_runs.R))
  - Model inputs / data (files with the prefix 'INPUT_')
  - Supporting data and files that have been used to create model inputs. These are not needed to run the model (as the results are already saved in the repository), and are provided for completeness and transparency (files with the prefix 'SUPPORTING_')
- [Results](https://github.com/maxeyre/Homeless-COVID-19/tree/master/results), including model outputs for 200 runs of the model in each scenario, and code that generates the tables and figures in the report.
- [R Shiny code](https://github.com/maxeyre/Homeless-COVID-19/tree/master/covid-19), which is provided as an archive and does not replicate the most recent version of the model.

## Contact Us
This model was built by a research group led by the [UCL Collaborative Centre for Inclusion Health](https://www.ucl.ac.uk/inclusion-health).

## License
This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

Please cite the published article:

Lewer D, Braithwaite I, Bullock M, Eyre MT, White PJ, Story A, Hayward A. 2020. COVID 19 among people experiencing homelessness in England: a modelling study. Lancet Respiratory Medicine. https://doi.org/10.1016/S2213-2600(20)30396-9

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg
