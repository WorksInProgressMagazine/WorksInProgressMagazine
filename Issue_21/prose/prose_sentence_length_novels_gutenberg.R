# Load required packages
library(tidyverse)
library(viridis)   

# Source: https://github.com/christofs/sentence-length/blob/main/results/Gutenberg-sample3/avgsentlens.csv
# https://dragonfly.hypotheses.org/1152

data_folder <- ""

sample3_sentences <- read_delim(paste0(data_folder, "avgsentlens_sample3.csv"), 
                                delim = ";",
                                col_names = TRUE)

# Create 20-year bins and label them
data_decades <- sample3_sentences %>%
  filter(year >= 1820 & year < 1940) %>%
  mutate(period = cut(
    year,
    breaks = seq(1820, 1940, by = 20),
    labels = c("1820–1839", "1840–1859", "1860–1879",
               "1880–1899", "1900–1919", "1920–1939"),
    right = FALSE
  ))

# Plot density curves for each 20-year period
ggplot(data_decades, aes(x = avgsentlen, color = period, fill = period)) +
  geom_density(alpha = 0.3) +
  scale_x_continuous(breaks = seq(0, 70, by = 10), limits = c(0, 70)) +
  scale_color_viridis_d(direction = -1) +  # or "magma", "inferno", "cividis"
  scale_fill_viridis_d(direction = -1) +
  labs(
    title = "Sentence Length Distribution by 20-Year Period",
    x = "Average Sentence Length",
    y = "Density",
    color = "Publication Period",
    fill = "Publication Period"
  ) +
  theme_minimal()

ggsave(paste0(data_folder, "sentence_length_density.svg"), width = 8, height = 8)

