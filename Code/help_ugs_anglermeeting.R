


library(tidyverse)
library(here)

## I pulled from two different data files here beause I didn't have the copy open of the years all put together. 

## you have to read in your own file path though.
ccfrp21 <- read_csv(here("Data", "2021-03-25_EB_EJ.csv"))
twenty19 <- read_csv(here("Data", "2019.csv"))


## filter for the 2020 anglers
## count number of fishes by trip/name/year
twentwen <- ccfrp21 %>%
  filter(name %in% c ("Eddie Gomez" , "Bill McKinney" , "Jim Webb", "Marcy Dorflinger", "Ralph Boone", "Duane Goudy", "Jeff Barriger", "Mike Booher", "Phil Ellis", "Tim Davis", "Roger Yount", "Lydon Mueller", "Ray Li")) %>%
  count(trip, name, year) 

## find the total number of fishes each angler caught in 2020
yeartot_20 <- twentwen %>%
  group_by(name, year) %>%
  summarise(yeartot = sum(n))


## find the number of trips each angler went on in 2020
numtrips_20 <- twentwen %>%
  select(trip, name) %>%
  count(name) %>%
  rename(trips = n)

## join the two into a data frame
cpue20 <- left_join(yeartot_20, numtrips_20, by = "name") %>%
  mutate(cpue = yeartot/trips)


## filter for 2019 anglers
twennine <- twenty19 %>%
  filter(name %in% c("Eddie Gomez" , "Bill McKinney" , "Jim Webb", "Marcy Dorflinger", "Ralph Boone", "Duane Goudy", "Jeff Barriger", "Mike Booher", "Phil Ellis", "Tim Davis", "Roger Yount", "Lydon Mueller", "Ray Li")) %>%
  count(trip, name, year)


## find the total number of fishes each agler caught in 2019
yeartot_19 <- twennine %>%
  group_by(name, year) %>%
  summarise(yeartot = sum(n))

## find the number of trips each angler went on in 2019
numtrips_19 <- twennine %>%
  select(trip, name) %>%
  count(name) %>%
  rename(trips = n)

## joint the two into a data frame
cpue19 <- left_join(yeartot_19, numtrips_19, by = "name") %>%
  mutate(cpue = yeartot/trips)


## join the 2019 and 2020 dataframes into one big dataframe
cpue19vs20 <- bind_rows(cpue19, cpue20) %>%
  mutate(year = as.factor(year))