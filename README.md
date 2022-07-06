# Executive Summary
## Problem 
The rapid increase in political polarization in the United States over the past decade accompanies
the rise of big data analytics, and its expansion into fields outside of tech. As topics such as broadening
voter access and abolishing the electoral college become more hotly debated, the intersection between politics
and analytics presents the unique opportunity to gain insight into the perspective of the American voter.
Therefore, relying on county-level data, we aimed to analyze the relationship between election results in 2020
and various health, demographic, and socioeconomic characteristics of county residents. All of the data was
collected in 2020, enabling us to examine the impact of the COVID-19 pandemic on voting behavior as well.
Our main goal was to understand which factors have the largest influence over whether a county goes red or
blue.

## Data
Our dataset pulled data from two sources: a New York Times time series dataset which includes
cumulative counts of COVID-19 cases and deaths at the county level, and data from County Health Rankings
& Roadmaps, a program focused on collecting county-level data on a variety of health determinants. For the
latter data source, we pulled 2019 or older data from various datasets on their website, as many key variables
we wanted to study are still missing for 2020 and we assume that most county-level health determinants
stayed very similar from 2019 to 2020. Our explanatory variables span the four main categories of health
factors that the program identifies: health behaviors (e.g., smoking, sexual activity), clinical care (e.g., flu
vaccine rate), social and economic factors (e.g., unemployment rate), and physical environment (e.g., degree
of air pollution). Our primary response variable of interest was deaths per cases, which we created by
dividing county-level deaths by count-level cases.

## Analysis
Since we pulled raw data from multiple sources, our analysis began with data wrangling and
cleaning to ensure a usable dataset. Since we planned to build models and test their performance via
prediction, we made sure to split our data into separate training and test datasets before conducting any exploratory analyses. Then, using the training set, we explored our response and explanatory variables, looking
for skewness, potential multicollinearity, and possible relationships between the response and explanatory
variables. To understand how to best model the data in order to make useful election result predictions, we
drew on two key classes of machine learning methodologies: regression modeling and tree modeling. Of the
three regression models we built, had the best performance. In the tree methods category, the random forest
model had the lowest misclassification error.

## Conclusions 
Interestingly, we found that the boosted and elastic net regression both pointed to similar
types of variables as the strongest predictors of deaths per cases. Specifically, our optimal boosted model
revealed that variables related to residential segregation and unemployment emerged as the most significant
predictors, revealing that structural economic and health access inequalities were more predictive of COVID19 deaths per cases than other variables. We hope that this analysis can inform policies aimed at improving
health outcome determinants, both in the context of COVID-19 and more generally going forward.
