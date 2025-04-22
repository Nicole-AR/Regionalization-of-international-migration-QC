# Regionalization of International Migration in Quebec

install.packages("readxl")
install.packages("tidyr")
install.packages("dplyr") 

library(readxl)
library(tidyr)
library(dplyr)
library (readr)


read_excel("Data_Regionalization - Excel.xlsx")

ratedata <- read_excel("Data_Regionalization - Excel.xlsx")

# Transform the data to long format
colnames(ratedata)



data_long <- ratedata %>%
  pivot_longer(
    cols = starts_with("20"),  # Select all columns that start with "20" (our years)
    names_to = "year",          # Name of the new column for the years
    values_to = "value"        # Name of the new column for the values
  )

glimpse (data_long)

colnames(data_long)

install.packages("ggplot2")
library(ggplot2)

ggplot(data_long, aes(x = year, y = value, color = region, group = region)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Values by Region and Year",
    x = "Year",
    y = "Value",
    color = "Region"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

head(data_long)
levels(factor(data_long$region))

data_long$region <- factor(
  data_long$region,
  levels = c(
    setdiff(unique(data_long$region), "Ensemble du Québec"),
    "Ensemble du Québec"
  )
)


ggplot(data_long, aes(x = year, y = value, fill = region)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Values by Region and Year",
    x = "Year",
    y = "Value",
    fill = "Region"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



data_relative <- data_long %>%
  group_by(region) %>%
  mutate(
    value_rel = (value - first(value)) / abs(first(value))
  )

ggplot(data_relative, aes(x = year, y = value_rel, color = region, group = region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Growth Relative to First Year (with Sign)",
    x = "Year",
    y = "Growth (vs. 1st year)",
    color = "Region"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")



data_last_year_relative <- data_relative %>%
  group_by(region) %>%
  filter(year == max(year)) %>%
  ungroup()

ggplot(data_relative, aes(x = year, y = value_rel, color = region, group = region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Growth Relative to First Year (with Sign)",
    x = "Year",
    y = "Growth (vs. 1st year)",
    color = "Region"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_text(
    data = data_last_year_relative, 
    aes(label = round(value_rel, 2)),  # Arredonda para duas casas decimais
    vjust = -0.5,  # Ajuste para que o texto não se sobreponha ao ponto
    hjust = 1.5,   # Ajuste para deslocar o texto para a direita
    size = 3       # Tamanho do texto
  )

#Wow! Saguenay-Lac-Saint-Jean:
#This represents a growth equivalent to 77.74 times the absolute value of the initial figure.
#Alternatively: The value increased by 7674% relative to the absolute value of the starting point.

glimpse (data_relative)

# Filter the last 10 years
last_10_years <- data_relative %>%
  filter(year %in% tail(unique(year), 10))

# Create the plot using only the last 10 years
ggplot(last_10_years, aes(x = year, y = value_rel, color = region, group = region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Growth Relative to First Year (Last 10 Years)",
    x = "Year",
    y = "Growth (vs. 1st year)",
    color = "Region"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_text(
    data = data_last_year_relative %>%
      filter(year %in% tail(unique(data_relative$year), 1)),
    aes(label = scales::percent(value_rel, accuracy = 0.1)),  # Add % symbol
    vjust = -0.5,
    hjust = 1.5,
    size = 3
  )


# Filter the last 3 years
last_3_years <- data_relative %>%
  filter(year %in% tail(unique(year), 3))

# Create the plot using only the last 3 years
ggplot(last_3_years, aes(x = year, y = value_rel, color = region, group = region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Growth Relative to First Year (Last 3 Years)",
    x = "Year",
    y = "Growth (vs. 1st year)",
    color = "Region"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_text(
    data = data_last_year_relative %>%
      filter(year %in% tail(unique(data_relative$year), 1)),
    aes(label = scales::percent(value_rel, accuracy = 0.1)),  # Add % symbol
    vjust = -0.5,
    hjust = 1.5,
    size = 3
  )


### Changing the reference (not anymore the first year 2001-2002) ###

#Growth of the last 10 years

# Set the reference year (2014-2015)
reference_year <- "2014-2015"

# Extract the value of the reference year (2014-2015)
reference_value <- last_10_years %>%
  filter(year == reference_year) %>%
  select(region, value) %>%
  rename(reference_value = value)

# Join the reference value to the data and calculate value_rel based on the reference year
data_relative_with_reference <- last_10_years %>%
  left_join(reference_value, by = "region") %>%
  mutate(value_rel = (value - reference_value) / abs(reference_value))

# Create the plot with the relative growth based on 2014-2015 as the reference
ggplot(data_relative_with_reference, aes(x = year, y = value_rel, color = region, group = region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Growth Relative to 2014-2015 Year (with Sign)",
    x = "Year",
    y = "Growth (vs. 2014-2015)",
    color = "Region"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_text(
    data = data_relative_with_reference %>%
      filter(year == "2023-2024"),  # Add text only for the last year
    aes(label = scales::percent(value_rel, accuracy = 0.1)),  # Add % symbol
    vjust = -0.5,
    hjust = 1.5,
    size = 3
  )

#Growth post-COVID-19, the last 3 years


# Set the reference year (2021-2022)
reference_year <- "2021-2022"

# Extract the value of the reference year (2021-2022)
reference_value3 <- last_3_years %>%
  filter(year == reference_year) %>%
  select(region, value) %>%
  rename(reference_value3 = value)

# Join the reference value to the data and calculate value_rel based on the reference year
data_relative_with_reference3 <- last_3_years %>%
  left_join(reference_value3, by = "region") %>%
  mutate(value_rel = (value - reference_value3) / abs(reference_value3))

# Create the plot with the relative growth based on 2021-2022 as the reference
ggplot(data_relative_with_reference3, aes(x = year, y = value_rel, color = region, group = region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Growth Relative to 2021-2022 Year (with Sign)",
    x = "Year",
    y = "Growth (vs. 2021-2022)",
    color = "Region"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_text(
    data = data_relative_with_reference3 %>%
      filter(year == "2023-2024"),  # Add text only for the last year
    aes(label = scales::percent(value_rel, accuracy = 0.1)),  # Add % symbol
    vjust = -0.5,
    hjust = 1.5,
    size = 3
  )



### Let's try some maps viz :) ###

install.packages(c("ggplot2", "sf", "dplyr", "tigris"))

library(ggplot2)
library(sf)
library(dplyr)
library(tigris)

quebec_shapefile <- st_read("regio_s.shp")

names(quebec_shapefile)
head(quebec_shapefile)


read_excel("Data_Regionalization_Maps.xlsx")

Maps_ratedata <- read_excel("Data_Regionalization_Maps.xlsx")

# Transform the data to long format
colnames(Maps_ratedata)



Maps_data_long <- Maps_ratedata %>%
  pivot_longer(
    cols = starts_with("20"),  # Select all columns that start with "20" (our years)
    names_to = "year",          # Name of the new column for the years
    values_to = "value"        # Name of the new column for the values
  )

glimpse (Maps_data_long)

colnames(Maps_data_long)

quebec_shapefile <- quebec_shapefile %>%
  rename(code = RES_CO_REG)

install.packages("sf")
library(sf)   

class(quebec_shapefile) 
class(Maps_data_long)

class(quebec_shapefile$code)
class(Maps_data_long$code)

# Both character
quebec_shapefile$code <- as.character(quebec_shapefile$code)
Maps_data_long$code <- as.character(Maps_data_long$code)


library(stringr)

Maps_data_long$code <- str_pad(Maps_data_long$code, width = 2, side = "left", pad = "0")

head(Maps_data_long$code)


map_data <- quebec_shapefile %>%
  left_join(Maps_data_long, by = "code") %>%
  st_as_sf()  


head(map_data)


library(ggplot2)
library(sf)

# Complete map
ggplot(quebec_shapefile) +
  geom_sf(fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Administrative regions of Quebec")


# 
map_year <- map_data %>% filter(year == "2021-2022")

# Map

install.packages("ggplot2")
library(ggplot2)

ggplot(map_year) +
  geom_sf(aes(fill = value)) +
  scale_fill_viridis_c(
    option = "A",    
    name = "Value",  
    limits = c(min(map_year$value), max(map_year$value)),
    breaks = seq(min(map_year$value), max(map_year$value), length.out = 5),
    direction = -1) +
  labs(title = "Distribution by year (2021–2022)") +
  theme_minimal()


map_year <- map_data %>% filter(year == "2022-2023")

# Map

install.packages("ggplot2")
library(ggplot2)

ggplot(map_year) +
  geom_sf(aes(fill = value)) +
  scale_fill_viridis_c(
    option = "A",    
    name = "Value",  
    limits = c(min(map_year$value), max(map_year$value)),
    breaks = seq(min(map_year$value), max(map_year$value), length.out = 5),
    direction = -1) +
  labs(title = "Distribution by year (2022–2023)") +
  theme_minimal()


map_year <- map_data %>% filter(year == "2001-2002")

# Map

install.packages("ggplot2")
library(ggplot2)

ggplot(map_year) +
  geom_sf(aes(fill = value)) +
  scale_fill_viridis_c(
    option = "A",    
    name = "Value",  
    limits = c(min(map_year$value), max(map_year$value)),
    breaks = seq(min(map_year$value), max(map_year$value), length.out = 5),
    direction = -1) +
  labs(title = "Distribution by year (2001–2002)") +
  theme_minimal()


library(sf)


region_bbox <- st_bbox(quebec_shapefile[quebec_shapefile$code == "06", ])

#
ggplot(map_year) +
  geom_sf(aes(fill = value)) +
  scale_fill_viridis_c(option = "C", name = "Value", direction = -1, na.value = "grey50") +
  labs(title = "Distribution by year (2021–2022)") +
  theme_minimal() +
  coord_sf(xlim = c(region_bbox["xmin"], region_bbox["xmax"]),
           ylim = c(region_bbox["ymin"], region_bbox["ymax"]),
           expand = FALSE)


library(plotly)

# Crie o gráfico estático com ggplot
p <- ggplot(map_year) +
  geom_sf(aes(fill = value)) +
  scale_fill_viridis_c(option = "C", name = "Value", direction = -1, na.value = "grey50") +
  labs(title = "Distribution by year (2021–2022)") +
  theme_minimal()

#  ggplot to plotly
ggplotly(p)

# Wow, an interactive plot!  Saved in the file

htmlwidgets::saveWidget(ggplotly(p), "interactive_plot.html")
