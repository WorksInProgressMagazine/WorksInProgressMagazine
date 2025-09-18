library(tidyverse)

data_folder <- "" # replace with path to file

marriage_df <- read_csv(paste0(data_folder, "share-of-women-aged-1549-who-are-married-or-in-a-union.csv"))

colnames(marriage_df) <- c("Country", "Code", "Year", "Share_married_projection", "Share_married_estimate")

marriage_df <- marriage_df %>%
                select(-c("Code", "Share_married_projection")) %>%
                pivot_wider(
                    names_from = Country, 
                    values_from = Share_married_estimate
                    )

write_csv(marriage_df, paste0(data_folder, "share_married_data_wrangled.csv"))

#####

population_age_group <- read_csv(paste0(data_folder, "population-young-working-elderly.csv"))

colnames(population_age_group) <- c("Country", "Code", "Year", "Population_65plus", "Population_15_64", "Population_under15", "1", "2", "3")

population_age_group <- population_age_group %>%
                          select(-c("1","2","3", "Code"))

sk_population <- population_age_group %>%
                  filter(Country == "South Korea") %>%
                  select(-Country)

uk_population <- population_age_group %>%
  filter(Country == "United Kingdom") %>%
  select(-Country)

us_population <- population_age_group %>%
  filter(Country == "United States") %>%
  select(-Country)

swe_population <- population_age_group %>%
  filter(Country == "Sweden") %>%
  select(-Country)

write_csv(sk_population, paste0(data_folder, "sk_population_age_group.csv"))
write_csv(uk_population, paste0(data_folder, "uk_population_age_group.csv"))
write_csv(us_population, paste0(data_folder, "us_population_age_group.csv"))
write_csv(swe_population, paste0(data_folder, "swe_population_age_group.csv"))

####

motherhood_penalty_df <- read_csv(paste0(data_folder, "estimates_KR-US-SE.csv"))

motherhood_penalty_df <- motherhood_penalty_df %>%
                          filter(gender == "female") %>%
                          select(-gender) %>%
  pivot_longer(cols = c(confHigh, confLow, estimate),
               names_to = "variable", 
               values_to = "value") %>%
  mutate(name_variable = paste(name, variable, sep = " ")) %>%
  select(-name, -variable) %>%
  pivot_wider(names_from = name_variable, values_from = value) %>%
  relocate(t_es, .before = everything())

write_csv(motherhood_penalty_df, paste0(data_folder, "motherhood_penalty_cleaned.csv"))
