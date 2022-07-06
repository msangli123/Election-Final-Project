# load libraries
library(tidyverse)

# download presidential election data (source: MIT Election Lab)
election_raw = read_csv("data/raw/countypres_2000-2020.csv")

# download county health data (source: County Health Rankings)
county_health_raw = read_csv("data/raw/county_health_rankings_2020.csv")

# download COVID (source: CDC)
COVID_raw = read_csv("data/raw/covid.csv")

# download mask-data (source: NYT)
masks_raw = read_csv("data/raw/mask-use-by-county.csv")

# download education, population, unemployment/income, and poverty data (source: USDA Economic Research Service)
education_raw = read_csv("data/raw/Education.csv")
population_raw = read_csv("data/raw/PopulationEstimates.csv")
unemployment_raw = read_csv("data/raw/Unemployment.csv")
poverty_raw = read_csv("data/raw/PovertyEstimates.csv")

# download fips data
fips_raw = read_csv("data/raw/state_and_county_fips_master.csv")
states_raw = read_csv("data/raw/states.csv")

rm(list=ls())
