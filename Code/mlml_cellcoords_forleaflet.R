

library(tidyverse)
library(here)

an_cells_raw <- read_csv(here("Data", "2021-06-23_anocells_raw.csv"))


an_cells_long1 <- an_cells_raw %>%
  select(name, lat1, lat2, lat3, lat4) %>%
  pivot_longer(cols = lat1:lat4,
               names_to = "comment",
               values_to = "latitude") %>%
  mutate(comment = case_when(
    comment == "lat1" ~ 1,
    comment == "lat2" ~ 2,
    comment == "lat3" ~ 3,
    comment == "lat4" ~ 4
  ))


an_cells_long2 <- an_cells_raw %>%
  select(name, lon1, lon2, lon3, lon4) %>%
  pivot_longer(cols = lon1:lon4,
               names_to = "comment",
               values_to = "longitude") %>%
  mutate(comment = case_when(
    comment == "lon1" ~ 1,
    comment == "lon2" ~ 2,
    comment == "lon3" ~ 3,
    comment == "lon4" ~ 4
  ))

an_cells <- left_join(an_cells_long1, an_cells_long2, by = c("name", "comment")) %>%
  select(longitude, latitude, name, comment)

write_csv(an_cells, here("Data", "AN_cellcoords.csv"))




pl_cells_raw <- read_csv(here("Data", "2021-06-23_loboscells_raw.csv"))


pl_cells_long1 <- pl_cells_raw %>%
  select(name, lat1, lat2, lat3, lat4) %>%
  pivot_longer(cols = lat1:lat4,
               names_to = "comment",
               values_to = "latitude") %>%
  mutate(comment = case_when(
    comment == "lat1" ~ 1,
    comment == "lat2" ~ 2,
    comment == "lat3" ~ 3,
    comment == "lat4" ~ 4
  ))


pl_cells_long2 <- pl_cells_raw %>%
  select(name, lon1, lon2, lon3, lon4) %>%
  pivot_longer(cols = lon1:lon4,
               names_to = "comment",
               values_to = "longitude") %>%
  mutate(comment = case_when(
    comment == "lon1" ~ 1,
    comment == "lon2" ~ 2,
    comment == "lon3" ~ 3,
    comment == "lon4" ~ 4
  ))

pl_cells <- left_join(pl_cells_long1, pl_cells_long2, by = c("name", "comment")) %>%
  select(longitude, latitude, name, comment)


write_csv(pl_cells, here("Data", "PL_cellcoords.csv"))
