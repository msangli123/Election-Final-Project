# load libraries
library(lubridate)
library(tidyverse)
library(janitor)
library(moments)

# read raw data
election_raw = read_csv("data/raw/countypres_2000-2020.csv")
county_health_raw = read_csv("data/raw/county_health_rankings_2020.csv")
COVID_raw = read_csv("data/raw/covid.csv")
masks_raw = read_csv("data/raw/mask-use-by-county.csv")
education_raw = read_csv("data/raw/Education.csv")
population_raw = read_csv("data/raw/PopulationEstimates.csv")
unemployment_raw = read_csv("data/raw/Unemployment.csv")
poverty_raw = read_csv("data/raw/PovertyEstimates.csv")
fips_raw = read_csv("data/raw/state_and_county_fips_master.csv")
states_raw = read_csv("data/raw/states.csv")

# clean fips and state data
states = states_raw %>% select(-st) %>%
  rename(state = stname, state_code = stusps)
fips_clean = subset(fips_raw,!(fips %% 1000 == 0)) %>%
  mutate_all(~ gsub(" County", "", .)) %>%
  rename(county = name,
         state_code = state) %>%
  na.omit()
fips_clean$fips = sapply(fips_clean$fips, as.numeric)

# clean election data
election = election_raw %>%
  na.omit() %>%
  filter(year == 2020) %>%
  filter(mode == "TOTAL") %>%
  select(-candidate,-office,-mode,-version, -year, -state_po) %>%
  pivot_wider(names_from = party, values_from = candidatevotes) %>%
  mutate(state = str_to_title(state)) %>%
  mutate(county_name = str_to_title(county_name)) %>%
  rename(
    county = county_name,
    Democrat = DEMOCRAT,
    Libertarian = LIBERTARIAN,
    Republican = REPUBLICAN,
    Green = GREEN,
    Other = OTHER,
    total_votes = totalvotes
  ) %>%
  mutate(
    pct_dem = Democrat / total_votes,
    pct_rep = Republican / total_votes,
    pct_other = Other / total_votes,
    pct_green = Green / total_votes,
    pct_libertarian = Libertarian / total_votes
  ) %>%
  rename(fips = county_fips)
partytallies = election %>%
  select(Democrat, Other, Republican, Green, Libertarian)
election = cbind(election, leading_party = colnames(partytallies)[apply(partytallies, 1, which.max)])

# clean county health rankings data
county_health = county_health_raw %>% row_to_names(row_number = 1)
county_health = county_health[-1,]
CH_fips = county_health %>% select(fipscode)
county_health = cbind(CH_fips, county_health[, grepl("rawvalue", names(county_health))])
county_health = county_health[, apply(county_health, 2, function(x)
  ! any(is.na(x)))]
county_health = county_health[, order(colnames(county_health))]
county_health = county_health %>%
  select(-v051_rawvalue,-v069_rawvalue) %>%
  rename(
    fips = fipscode,
    poor_fair_health = v002_rawvalue,
    adult_smoking = v009_rawvalue,
    adult_obesity = v011_rawvalue,
    poor_physical_health_days = v036_rawvalue,
    poor_mental_health_days = v042_rawvalue,
    excessive_drinking = v049_rawvalue,
    pct_below_18 = v052_rawvalue,
    pct_above_65 = v053_rawvalue,
    pct_nonhispanic_black = v054_rawvalue,
    pct_native_american = v055_rawvalue,
    pct_hispanic = v056_rawvalue,
    pct_females = v057_rawvalue,
    pct_nonproficient_english = v059_rawvalue,
    diabetes_prevalence = v060_rawvalue,
    physical_inactivity = v070_rawvalue,
    pct_pacific_islander = v080_rawvalue,
    pct_asian = v081_rawvalue,
    pct_nonhispanic_white = v126_rawvalue,
    severe_housing_issues = v136_rawvalue,
    food_insecurity = v139_rawvalue,
    social_associations = v140_rawvalue,
    insufficient_sleep = v143_rawvalue,
    frequent_physical_distress = v144_rawvalue,
    frequent_mental_distress = v145_rawvalue,
    homeownership = v153_rawvalue,
    traffic_volume = v156_rawvalue
  )
county_health = subset(county_health, !(endsWith(fips, '000')))
county_health = sapply(county_health, as.numeric)

# clean COVID data
COVID = COVID_raw %>% select(
  fips = `FIPS Code`,
  total_deaths = `Total deaths`,
  covid_deaths = `COVID-19 Deaths`,
  urban_rural_desc = `Urban Rural Description`
) %>%
  mutate(pct_covid_deaths = covid_deaths/total_deaths)
COVID = unique(COVID)

# clean mask data
masks = masks_raw %>% rename(
  fips = COUNTYFP,
  never = NEVER,
  rarely = RARELY,
  sometimes = SOMETIMES,
  frequently = FREQUENTLY,
  always = ALWAYS
)
masks = sapply(masks, as.numeric)

# clean education data
education = cbind(education_raw[, 1], education_raw[, 44:47])
education = education %>%
  rename(
    fips = `FIPS Code`,
    pct_less_than_high_school = `Percent of adults with less than a high school diploma, 2015-19`,
    pct_high_school_diploma = `Percent of adults with a high school diploma only, 2015-19`,
    pct_college_associates = `Percent of adults completing some college or associate's degree, 2015-19`,
    pct_bachelors_or_higher = `Percent of adults with a bachelor's degree or higher, 2015-19`
  )
education = subset(education, !(endsWith(fips, '000')))
education = sapply(education, as.numeric)

# clean population data
population = population_raw[-c(1, 2, 3, 4),]
population = population %>% mutate_all(~ gsub("Population ", "", .)) %>%
  select(fips = FIPStxt,
         year = Attribute,
         population = Value) %>%
  filter(year == 2020) %>%
  select(-year)
population = subset(population, !(endsWith(fips, '000')))
population = sapply(population, as.numeric)

# clean unemployment data
unemployment = unemployment_raw %>%
  filter(str_detect(
    Attribute,
    "Median_Household_Income_2019|Unemployment_rate_2020"
  ))
unemployment = unemployment[-c(1, 2),]
unemployment = unemployment %>% pivot_wider(names_from = Attribute,
                                            values_from = Value) %>%
  select(
    fips = FIPS_Code,
    unemployment_rate = Unemployment_rate_2020,
    median_household_income = Median_Household_Income_2019
  )
unemployment = subset(unemployment, !(fips %% 1000 == 0))
unemployment = sapply(unemployment, as.numeric)

# clean poverty data
poverty = poverty_raw %>% pivot_wider(names_from = Attribute,
                                      values_from = Value) %>%
  mutate(log_poverty_rating = log10(POVALL_2019)) %>%
  select(fips = FIPStxt, log_poverty_rating)
poverty = subset(poverty, !(fips %% 1000 == 0))

# make each cleaned data set a dataframe
fips_clean = as.data.frame(fips_clean)
states = as.data.frame(states)
election = as.data.frame(election)
county_health = as.data.frame(county_health)
COVID = as.data.frame(COVID)
masks = as.data.frame(masks)
education = as.data.frame(education)
population = as.data.frame(population)
unemployment = as.data.frame(unemployment)
poverty = as.data.frame(poverty)


# clean each data set further by joining additional geographical information
fips_clean = inner_join(fips_clean, states, by = "state_code") %>% select(-state_code)
county_health = inner_join(county_health, fips_clean, by = "fips")
COVID = inner_join(COVID, fips_clean, by = "fips")
masks = inner_join(masks, fips_clean, by = "fips")
education = inner_join(education, fips_clean, by = "fips")
population = inner_join(population, fips_clean, by = "fips")
unemployment = inner_join(unemployment, fips_clean, by = "fips")
poverty = inner_join(poverty, fips_clean, by = "fips")

# create a master data set using inner join
master_data = inner_join(x = election,
                         y = (county_health %>% select(-county, -state)),
                         by = "fips") %>%
  inner_join((COVID %>% select(-county, -state)), by = "fips") %>%
  inner_join((masks %>% select(-county, -state)), by = "fips") %>%
  inner_join((education %>% select(-county, -state)), by = "fips") %>%
  inner_join((population %>% select(-county, -state)), by = "fips") %>%
  inner_join((unemployment %>% select(-county, -state)), by = "fips") %>%
  inner_join((poverty %>% select(-county, -state)), by = "fips") %>%
  mutate(pct_voters = total_votes / population, log_population = log10(population),
         log_traffic_volume = log10(traffic_volume))
skew_check = as.data.frame(skewness(master_data %>% select(where(is.numeric)), na.rm = TRUE))
master_data = master_data %>% select(-traffic_volume,-population,-covid_deaths,-total_deaths)

# write clean data to file
write_csv(x = master_data, file = "data/clean/master_data.csv")
write_csv(x = county_health, file = "data/clean/county_health.csv")
write_csv(x = COVID, file = "data/clean/COVID.csv")
write_csv(x = masks, file = "data/clean/masks.csv")
write_csv(x = education, file = "data/clean/education.csv")
write_csv(x = unemployment, file = "data/clean/unemployment.csv")
write_csv(x = poverty, file = "data/clean/poverty.csv")
write_csv(x = election, file = "data/clean/election.csv")

rm(list=ls())