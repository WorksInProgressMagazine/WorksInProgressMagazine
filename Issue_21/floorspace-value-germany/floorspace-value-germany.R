# Load required packages
library(sf) 
library(tidyverse)
library(viridis)   


data_folder <- "" # replace with path to file

# 1. Read the CSV file with rental data
rent_data <- read_csv(paste0(data_folder, "APPLICATIONS/DATA/OUTPUT/2024/AHS-Index-GVB2022-2024/AHS-Index-PURCH-GVB2022-2024.csv"))

# 2. Read the shapefile (hexagons)
hexagons <- st_read(paste0(data_folder, "APPLICATIONS/SHAPES/GVB/GVB2022-2024.shp"))

# 3. Join using the correct column names (postcodeid in shapefile, postcode_id in CSV)
hexagons_merged <- hexagons %>%
  left_join(rent_data, by = c("gvb_id" = "gvb_id"))

# 4. Generate 8 discrete colors from magma palette
viridis_colors <- viridis(9, direction = -1)   # reversed so dark = low values

# Assign colors to bins
bin_labels <- c("500–1000", "1000-2000", "2000-3000", "3000-4000", "4000-5000", "5000-6000", "6000-8000", "8000-10000", "10000+")
names(viridis_colors) <- bin_labels

# 5. Plot with discrete bins directly in ggplot
ggplot(hexagons_merged) +
  geom_sf(aes(fill = cut(price_qm2023,
                         breaks = c(500, 1000, 2000, 3000, 4000, 5000, 6000, 8000, 10000, Inf),
                         labels = c("500–1000", "1000-2000", "2000-3000", "3000-4000", 
                                    "4000-5000", "5000-6000", "6000-8000", "8000-10000", 
                                    "10000+")))) +
  scale_fill_manual(name = "€ per sqm", 
                    values = viridis_colors, 
                    drop = FALSE, 
                    guide = guide_legend(reverse = TRUE)) +
  labs(title = "Sales prices per sqm in Germany (2023)") +
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

ggsave("/Users/saloni/Documents/Github/private-work/wip/germany-sales-prices-sqm/germany-sales-price-sqm-map.svg", width=9,height=9)

#####
# Histogram

# 1. Define your bins and magma colors
bin_breaks <- c(500, 1000, 2000, 3000, 4000, 5000, 6000, 8000, 10000, Inf)
bin_labels <- c("500–1000", "1000-2000", "2000-3000", "3000-4000", 
                "4000-5000", "5000-6000", "6000-8000", "8000-10000", 
                "10000+")
viridis_colors <- viridis(10, direction = -1)
names(viridis_colors) <- bin_labels

# 2. Classify observations into bins
rent_data_hist <- rent_data %>%
  mutate(price_bin = cut(price_qm2023, breaks = bin_breaks, labels = bin_labels, right = FALSE))

# 3. Plot histogram with binwidth but fill determined by price_bin intervals
ggplot(rent_data_hist, aes(x = price_qm2023, fill = price_bin)) +
  geom_histogram(binwidth = 100, boundary = 0, closed = "left") +
  scale_fill_manual(name = "€ per sqm", values = viridis_colors, drop = FALSE,
                    guide = guide_legend(reverse = TRUE)) +
  scale_x_continuous(breaks = seq(0, max(rent_data$price_qm2023, na.rm = TRUE), by = 1000)) +
  labs(title = "Distribution of Sales Prices per sqm across GVBs (2023)",
       x = "Price per sqm (€)",
       y = "Number of GVBs") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "right")

ggsave("germany-sales-price-sqm-histogram.svg", width=9,height=9)


filt <- hexagons_merged %>% filter(gvb_name == "Hamburg") %>% select(c(gvb_name, price_qm2023))


