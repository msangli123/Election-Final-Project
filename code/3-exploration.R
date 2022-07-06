# load libraries
library(kableExtra)                     # for printing tables
library(cowplot)                        # for side by side plots
library(lubridate)                      # for dealing with dates
library(maps)                           # for creating maps
library(flextable)                      # creating contingency tables
library(corrplot)                       # creating correlation matrices
library(Hmisc)
library(tidyverse)
library(tidyquant)
library(scales)

# read in the cleaned data
master_data = read_csv("data/clean/master_data.csv")
election = read_csv("data/clean/election.csv")
county_health = read_csv("data/clean/county_health.csv")
education = read_csv("data/clean/education.csv")
poverty = read_csv("data/clean/poverty.csv")
unemployment = read_csv("data/clean/unemployment.csv")
COVID = read_csv("data/clean/COVID.csv")


# create plot of # of counties for each party
p = master_data %>%
  ggplot(aes(x = leading_party, fill = leading_party)) +
  stat_count(width = 0.5) +
  labs(x = "Political Party",
       y = "Number of Counties") +
  scale_fill_manual(breaks = c("Democrat", "Republican"),
                    values = c("blue", "red")) +
  theme_bw() +
  theme(legend.position = "none")

# save the plot
ggsave(
  filename = "results/response-plot.png",
  plot = p,
  device = "png",
  width = 5,
  height = 3
)

# create plot of # of total individual votes for each party
sum_votes = cbind(as.data.frame(c(
  sum(master_data$Democrat),
  sum(master_data$Republican),
  sum(master_data$Other, na.rm = TRUE)
)),
c("Democrat", "Republican", "Other"))
colnames(sum_votes) = c("Counts", "Party")
p = sum_votes %>%
  ggplot(aes(x = Party, y = Counts, fill = Party)) +
  labs(x = "Political Party",
       y = "Number of Votes") +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = c("Democrat", "Republican", "Other")) +
  scale_y_continuous(label = comma) +
  scale_fill_manual(
    breaks = c("Democrat", "Republican", "Other"),
    values = c("blue", "red", "grey")
  ) +
  theme_bw() +
  theme(legend.position = "none")

# save the plot
ggsave(
  filename = "results/indiv-vote-plot.png",
  plot = p,
  device = "png",
  width = 5,
  height = 3
)

# examine top 10 democratic, republican, and other counties as well as civically engaged counties
master_data %>%
  select(county,
         state,
         pct_dem,
         pct_rep,
         pct_other,
         pct_voters,
         total_votes) %>%
  arrange(desc(pct_dem)) %>%
  rename(
    "County" = county,
    "State" = state,
    "% Democrat" = pct_dem,
    "% Republican" = pct_rep,
    "% Other" = pct_other,
    "% Voted of Population" = pct_voters,
    "Total Votes" = total_votes
  ) %>%
  head(10) %>%
  write_tsv(
    "results/top-10-dems-data.tsv"
  )

master_data %>%
  select(county,
         state,
         pct_dem,
         pct_rep,
         pct_other,
         pct_voters,
         total_votes) %>%
  arrange(desc(pct_rep)) %>%
  rename(
    "County" = county,
    "State" = state,
    "% Democrat" = pct_dem,
    "% Republican" = pct_rep,
    "% Other" = pct_other,
    "% Voted of Population" = pct_voters,
    "Total Votes" = total_votes
  ) %>%
  head(10) %>%
  write_tsv(
    "results/top-10-reps-data.tsv"
  )

master_data %>%
  select(county,
         state,
         pct_dem,
         pct_rep,
         pct_other,
         pct_voters,
         total_votes) %>%
  arrange(desc(pct_other)) %>%
  rename(
    "County" = county,
    "State" = state,
    "% Democrat" = pct_dem,
    "% Republican" = pct_rep,
    "% Other" = pct_other,
    "% Voted of Population" = pct_voters,
    "Total Votes" = total_votes
  ) %>%
  head(10) %>%
  write_tsv(
    "results/top-10-other-data.tsv"
  )

master_data %>%
  select(county,
         state,
         pct_dem,
         pct_rep,
         pct_other,
         pct_voters,
         total_votes) %>%
  arrange(desc(pct_voters)) %>%
  rename(
    "County" = county,
    "State" = state,
    "% Democrat" = pct_dem,
    "% Republican" = pct_rep,
    "% Other" = pct_other,
    "% Voted of Population" = pct_voters,
    "Total Votes" = total_votes
  ) %>%
  head(10) %>%
  write_tsv(
    "results/top-10-voters-data.tsv"
  )

# create a map of election parties across the U.S.
p = map_data("county") %>%
  as_tibble() %>%
  left_join(
    election %>%
      rename(
        region = state,
        subregion = county,
        `Political Party` = leading_party
      ) %>%
      mutate(region = str_to_lower(region),
             subregion = str_to_lower(subregion)),
    by = c("region", "subregion")
  ) %>%
  ggplot() +
  geom_polygon(
    data = map_data("state"),
    aes(x = long, y = lat, group = group),
    color = "black",
    fill = NA,
    size = 1,
    alpha = .3
  ) +
  geom_polygon(
    aes(
      x = long,
      y = lat,
      group = group,
      fill = `Political Party`
    ),
    color = "darkblue",
    size = .1
  ) +
  scale_fill_manual(breaks = c("Democrat", "Republican"),
                    values = c("blue", "red")) +
  theme_void()

ggsave(
  filename = "results/election-map.png",
  plot = p,
  device = "png",
  width = 7,
  height = 4
)

# create a map of health ratings across the U.S.
p = map_data("county") %>%
  as_tibble() %>%
  left_join(
    county_health %>%
      rename(
        region = state,
        subregion = county,
        `Health Rating` = poor_fair_health
      ) %>%
      mutate(region = str_to_lower(region),
             subregion = str_to_lower(subregion)),
    by = c("region", "subregion")
  ) %>%
  ggplot() +
  geom_polygon(
    data = map_data("state"),
    aes(x = long, y = lat, group = group),
    color = "black",
    fill = NA,
    size = 1,
    alpha = .3
  ) +
  geom_polygon(
    aes(
      x = long,
      y = lat,
      group = group,
      fill = `Health Rating`
    ),
    color = "darkblue",
    size = .1
  ) +
  scale_fill_gradient(low = "red", high = "green") +
  theme_void()

ggsave(
  filename = "results/health-map.png",
  plot = p,
  device = "png",
  width = 7,
  height = 4
)

# create a map of education categories across the U.S.
p = map_data("county") %>%
  as_tibble() %>%
  left_join(
    education %>%
      rename(region = state,
             subregion = county) %>%
      mutate(
        region = str_to_lower(region),
        subregion = str_to_lower(subregion),
        pct_greater_than_HSdiploma = pct_college_associates + pct_bachelors_or_higher
      ) %>%
      rename(`% of Population with an Associates Degree or More` = pct_greater_than_HSdiploma),
    by = c("region", "subregion")
  ) %>%
  ggplot() +
  geom_polygon(
    data = map_data("state"),
    aes(x = long, y = lat, group = group),
    color = "black",
    fill = NA,
    size = 1,
    alpha = .3
  ) +
  geom_polygon(
    aes(
      x = long,
      y = lat,
      group = group,
      fill = `% of Population with an Associates Degree or More`
    ),
    color = "darkblue",
    size = .1
  ) +
  scale_fill_gradient(low = "red", high = "green") +
  theme_void()

ggsave(
  filename = "results/education-map.png",
  plot = p,
  device = "png",
  width = 7,
  height = 4
)

# create a map of poverty categories across the U.S.
p = map_data("county") %>%
  as_tibble() %>%
  left_join(
    poverty %>%
      rename(
        region = state,
        subregion = county,
        `Log Poverty Rating` = log_poverty_rating
      ) %>%
      mutate(region = str_to_lower(region),
             subregion = str_to_lower(subregion)),
    by = c("region", "subregion")
  ) %>%
  ggplot() +
  geom_polygon(
    data = map_data("state"),
    aes(x = long, y = lat, group = group),
    color = "black",
    fill = NA,
    size = 1,
    alpha = .3
  ) +
  geom_polygon(
    aes(
      x = long,
      y = lat,
      group = group,
      fill = `Log Poverty Rating`
    ),
    color = "darkblue",
    size = .1
  ) +
  scale_fill_gradient(low = "green", high = "red") +
  theme_void()

ggsave(
  filename = "results/poverty-map.png",
  plot = p,
  device = "png",
  width = 7,
  height = 4
)

# create a map of unemployment values across the U.S.
p = map_data("county") %>%
  as_tibble() %>%
  left_join(
    unemployment %>%
      rename(region = state,
             subregion = county) %>%
      mutate(
        region = str_to_lower(region),
        subregion = str_to_lower(subregion),
        log_unemployment_rate = log10(unemployment_rate)
      ) %>%
      rename(`Log Unemployment Rate` = log_unemployment_rate),
    by = c("region", "subregion")
  ) %>%
  ggplot() +
  geom_polygon(
    data = map_data("state"),
    aes(x = long, y = lat, group = group),
    color = "black",
    fill = NA,
    size = 1,
    alpha = .3
  ) +
  geom_polygon(
    aes(
      x = long,
      y = lat,
      group = group,
      fill = `Log Unemployment Rate`
    ),
    color = "darkblue",
    size = .1
  ) +
  scale_fill_gradient(low = "red", high = "green") +
  theme_void()

ggsave(
  filename = "results/unemployment-map.png",
  plot = p,
  device = "png",
  width = 7,
  height = 4
)

# create a map of COVID values across the U.S.
p = map_data("county") %>%
  as_tibble() %>%
  left_join(
    COVID %>%
      rename(
        region = state,
        subregion = county,
        `% of Total Deaths that are COVID Deaths` = pct_covid_deaths
      ) %>%
      mutate(region = str_to_lower(region),
             subregion = str_to_lower(subregion)),
    by = c("region", "subregion")
  ) %>%
  ggplot() +
  geom_polygon(
    data = map_data("state"),
    aes(x = long, y = lat, group = group),
    color = "black",
    fill = NA,
    size = 1,
    alpha = .3
  ) +
  geom_polygon(
    aes(
      x = long,
      y = lat,
      group = group,
      fill = `% of Total Deaths that are COVID Deaths`
    ),
    color = "darkblue",
    size = .1
  ) +
  scale_fill_gradient(low = "green", high = "red") +
  theme_void()

ggsave(
  filename = "results/covid-map.png",
  plot = p,
  device = "png",
  width = 7,
  height = 4
)

###Explore associations (train data only)
##read in data
master_data = read_csv('data/clean/train_data.csv')
##dataset contains mixed data types - create lists of categorical vs. numerical columns
#named vector containing dtypes
dtypes = sapply(colnames(master_data), function(x) class(master_data[[x]]))
x_cats = dtypes[dtypes=='character'] %>%
  names()
x_cts = dtypes[dtypes=='numeric'] %>%
  names()
y = master_data$leading_party

##predictions
#majority class
mean(master_data$leading_party=='Democrat')
#majority class is Republican (~32.4% of observations are Democrat)

#misclassification error for naive classifier?
master_data %>% 
  group_by(leading_party) %>%
  count()
#Democrat: 232
#Republican: 484
tp = 0
tn = 484
fp = 0
fn = 232

misclass_err = (fp+fn)/(fp+fn+tp+tn)
print(misclass_err)
#naive classifier always incorrectly predicts the observations that are Democrat

##categorical variables
#create 2-way contingency table
desc_cont = proc_freq(master_data,"leading_party","urban_rural_desc")
v2_desc_cont = table(master_data$leading_party, master_data$urban_rural_desc)

#balloonplot
lpd <- master_data %>%
  # Convert to long format
  as_tibble() %>%
  group_by(leading_party, urban_rural_desc) %>%
  summarise(n())

lpd_sp <- ggplot(lpd, aes(x = leading_party, y = urban_rural_desc)) +
  geom_point(aes(size = `n()`), shape = 21, colour = "black", fill = "cornsilk") +
  scale_size_area(max_size = 20, guide = "none") +
  geom_text(aes(label = `n()`),
    vjust = 3.2,
    colour = "grey60",
    size = 4
  ) +
  labs(x = 'Leading Party',
       y = 'Urban/Rural Description',
       title = 'County Type vs. Leading Party')
#save as image
ggsave(
  filename = "results/urb-rural-lp.png",
  plot = lpd_sp,
  device = "png",
  width = 7,
  height = 9
)

#barplot
desc_bar = master_data %>%
  ggplot() +
  aes(x = urban_rural_desc, fill = leading_party) +
  geom_bar(position='fill') +
  scale_fill_manual(breaks = c("Democrat", "Republican"),
                    values = c("blue", "red"))
#save as image 
ggsave(
  filename = "results/desc-bar.png",
  plot = desc_bar,
  device = "png",
  width = 9,
  height = 9
)

#chi2 test of independence
test <- chisq.test(master_data$leading_party, master_data$urban_rural_desc)
#save results
test_results = capture.output(print(test))
writeLines(test_results, con = file("results/chi2_output.txt"))

##numerical variables
#drop numerical predictors relating to num. of Dem./Rep. votes
x_cts = x_cts[-1:-12]

#correlation matrix between explanatory variables
cor1<-rcorr(as.matrix(master_data[x_cts]))
#define flattening function
flattenCorrMatrix <- function(cormat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    corr  =(cormat)[ut]
  )
}
#flatten
corrMatrix = flattenCorrMatrix(cor1$r)
#focus on strongest correlations
corrMatrix2 = corrMatrix %>%
  filter(corr > 0.7 | corr < -0.7) 
ordered_corrDF = corrMatrix2[order(corrMatrix2$corr), ] %>%
  write_tsv(
    "results/ordered_corrDF.tsv"
  )

#boxplots between num. explanatory variables and cat. response variables
#explanatory variables span different categories (health, COVID, demographic & socioeconomic indicators)
#pick a few from each category to inspect relationship with response
#health
health_box = master_data %>%
  ggplot(aes(x=poor_fair_health, y=leading_party, fill=leading_party)) +
  geom_boxplot() +
  labs(x = "Health Score",
       y = "Leading Party") +
  scale_fill_manual(breaks = c("Democrat", "Republican"),
                    values = c("blue", "red"))
ggsave(
  filename = "results/health-box.png",
  plot = health_box,
  device = "png",
  width = 9,
  height = 9
)
#covid
covid_box = master_data %>%
  ggplot(aes(x=pct_covid_deaths, y=leading_party, fill=leading_party)) +
  geom_boxplot() +
  labs(x = "Percent of Deaths due to COVID",
       y = "Leading Party") +
  scale_fill_manual(breaks = c("Democrat", "Republican"),
                    values = c("blue", "red"))
ggsave(
  filename = "results/covid-box.png",
  plot = covid_box,
  device = "png",
  width = 9,
  height = 9
)

mask_hist = master_data %>%
  select(sometimes, always, never, leading_party) %>%
  group_by(leading_party) %>%
  summarise(mean_sometimes = mean(sometimes),
            mean_always = mean(always),
            mean_never = mean(never)) 
mask1 = mask_hist %>%
  ggplot() +
  geom_bar(aes(x=mean_sometimes,y=leading_party,fill=leading_party),stat='identity') +
  labs(x = 'Mean Proportion of Response "Sometimes"',
       y = 'Leading Party') +
  scale_fill_manual(breaks = c("Democrat", "Republican"),
                    values = c("blue", "red")) 
mask2 = mask_hist %>%
  ggplot() +
  geom_bar(aes(x=mean_always,y=leading_party,fill=leading_party),stat='identity') +
  labs(x = 'Mean Proportion of Response "Always"',
       y = 'Leading Party') +
  scale_fill_manual(breaks = c("Democrat", "Republican"),
                    values = c("blue", "red")) 
mask3 = mask_hist %>%
  ggplot() +
  geom_bar(aes(x=mean_never,y=leading_party,fill=leading_party),stat='identity') +
  labs(x = 'Mean Proportion of Response "Never"',
       y = 'Leading Party') +
  scale_fill_manual(breaks = c("Democrat", "Republican"),
                    values = c("blue", "red")) 
plot_row = plot_grid(mask1,mask2,mask3)
title = ggdraw() + 
  draw_label(
    "How often do you wear a mask?",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) + 
  theme(
    plot.margin = margin(0, 0, 0, 7))
mask_behavior = plot_grid(title, plot_row)
ggsave(
  filename = "results/mask-bar.png",
  plot = mask_behavior,
  device = "png",
  width = 20,
  height = 9
)
#demographic/socioeconomic
#a) education
edu_box = master_data %>%
  ggplot(aes(x=pct_bachelors_or_higher, y=leading_party, fill = leading_party)) +
  geom_boxplot() +
  labs(x = "% with Bach. Degree or Higher",
       y = "Leading Party") +
  scale_fill_manual(breaks = c("Democrat", "Republican"),
                    values = c("blue", "red"))
ggsave(
  filename = "results/edu-box.png",
  plot = edu_box,
  device = "png",
  width = 9,
  height = 9
)
#b) income/socieconomic status
income_box = master_data %>%
  ggplot(aes(x=median_household_income, y=leading_party, fill=leading_party)) +
  geom_boxplot() +
  labs(x = "Median Household Income",
       y = "Leading Party") +
  scale_fill_manual(breaks = c("Democrat", "Republican"),
                    values = c("blue", "red")) 
ggsave(
  filename = "results/income-box.png",
  plot = income_box,
  device = "png",
  width = 9,
  height = 9
)
unemp_box = master_data %>%
  ggplot(aes(x=unemployment_rate, y=leading_party, fill=leading_party)) +
  geom_boxplot() +
  labs(x = "Unemployment Rate (%)",
       y = "Leading Party") +
  scale_fill_manual(breaks = c("Democrat", "Republican"),
                    values = c("blue", "red"))
ggsave(
  filename = "results/unemp-box.png",
  plot = unemp_box,
  device = "png",
  width = 9,
  height = 9
)
#c) race/ethnicity
race_box1 = master_data %>%
  ggplot(aes(x=pct_nonhispanic_white, y=leading_party, fill=leading_party)) +
  geom_boxplot() +
  labs(x = "% Nonhispanic White Residents",
       y = "Leading Party") +
  scale_fill_manual(breaks = c("Democrat", "Republican"),
                    values = c("blue", "red")) 
race_box2 = master_data %>%
  ggplot(aes(x=pct_nonhispanic_black, y=leading_party, fill=leading_party)) +
  geom_boxplot()+
  labs(x = "% Nonhispanic Black Residents",
       y = "Leading Party") +
  scale_fill_manual(breaks = c("Democrat", "Republican"),
                    values = c("blue", "red")) 
race_box3 = master_data %>%
  ggplot(aes(x=pct_hispanic, y=leading_party, fill=leading_party)) +
  geom_boxplot() +
  labs(x = "% Hispanic Residents",
       y = "Leading Party") +
  scale_fill_manual(breaks = c("Democrat", "Republican"),
                    values = c("blue", "red")) 
race_box4 = master_data %>%
  ggplot(aes(x=pct_asian, y=leading_party, fill=leading_party)) +
  geom_boxplot() +
  labs(x = "% Asian Residents",
       y = "Leading Party") +
  scale_fill_manual(breaks = c("Democrat", "Republican"),
                    values = c("blue", "red")) 
race_plots = plot_grid(race_box1,race_box2,race_box3,race_box4)
ggsave(
  filename = "results/race-plots.png",
  plot = race_plots,
  device = "png",
  width = 9,
  height = 9
)
#d) age
age_box = master_data %>%
  ggplot(aes(x=pct_above_65, y=leading_party, fill=leading_party)) +
  geom_boxplot()+
  labs(x = "% Residents Above 65",
       y = "Leading Party") +
  scale_fill_manual(breaks = c("Democrat", "Republican"),
                    values = c("blue", "red")) 
ggsave(
  filename = "results/age-box.png",
  plot = age_box,
  device = "png",
  width = 9,
  height = 9
)
#e) gender
gender_box = master_data %>%
  ggplot(aes(x=pct_females, y=leading_party, fill=leading_party)) +
  geom_boxplot()+
  labs(x = "% Female Residents",
       y = "Leading Party") +
  scale_fill_manual(breaks = c("Democrat", "Republican"),
                    values = c("blue", "red"))
ggsave(
  filename = "results/gender-box.png",
  plot = gender_box,
  device = "png",
  width = 9,
  height = 9
)

rm(list=ls())
