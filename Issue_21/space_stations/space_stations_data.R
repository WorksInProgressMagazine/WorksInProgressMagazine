### SOURCES
library(tidyverse)
library(scales)
library(ggrepel)

# Define space station data
stations <- tibble::tibble(
  name = c(
    'Salyut-1', 'MORL', 'LORL', 'Haven-1', 'Haven-2', 'Skylab', 'Tiangong', 'ISS',
    'Gateway', 'von Braun', 'Space Base', 'Hexagonal Station', '2035 AG Station',
    'Stanford Torus', "O'Neill Cylinder", "O'Neill Cylinder (pair)"
  ),
  total_volume = c(
    214, 254.85, 1905.85, 80, 1160, 499, 726.6, 1200,
    183, 6217.85, 5921, 1274.3, 2160,
    69220470, 1636828970203, 3273657940405
  ),
  pressurised_volume = c(
    99, 254.85, 1905.85, 80, 1160, 351.6, 340, 1005,
    183, 4800, 3600, 980, 2160,
    69220470, 1604092390799, 3208184781597
  ),
  habitable_volume = c(
    90, 200, 1905.85, 45, 500, 270, 121, 388,
    125, 3600, 3600, 980, 950,
    34610235, 1227621727652, 2455243455304
  ),
  crew = c(
    3, 6, 24, 4, 12, 3, 3, 7,
    4, 80, 100, 36, 40,
    10000, 1000000, 2000000
  ),
  is_real = c(
    TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE,
    FALSE, FALSE, FALSE
  ),
  has_gravity = c(
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    FALSE, TRUE, TRUE, TRUE, TRUE,
    TRUE, TRUE, TRUE
  )
) %>%
  filter(!name %in% c("MORL", "LORL")) %>%
  mutate(
    volume_per_astronaut = habitable_volume / crew,
    category = case_when(
      is_real ~ "Real Stations",
      !is_real & !has_gravity ~ "Planned 0-g Stations",
      !is_real & has_gravity & !name %in% c("Stanford Torus", "O'Neill Cylinder", "O'Neill Cylinder (pair)") ~ "Artificial Gravity Concepts",
      name %in% c("Stanford Torus", "O'Neill Cylinder", "O'Neill Cylinder (pair)") ~ "Megastructure"
    ),
    size = log10(total_volume) * 60 * 0.8
  )

write_csv(stations, "space_stations_angadh.csv")

getwd()