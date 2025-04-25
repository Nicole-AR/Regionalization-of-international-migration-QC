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

head(data_long)
levels(factor(data_long$region))

data_long$region <- factor(
  data_long$region,
  levels = c(
    setdiff(unique(data_long$region), "Quebec Province"),
    "Quebec Province"
  )
)


ggplot(data_long, aes(x = year, y = value, color = region, group = region)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Net international migration rate, Quebec province and administrative regions, 2001–2024",
    x = "Year",
    y = "Value",
    color = "Region"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

####

# for color choices see: https://r-charts.com/colors/



colorsforchosenregions = c(
  "Quebec Province" = "#0000FF",
  "Montréal" = "#CD4F39",
  "Laval" = "#A0522D",
  "Capitale-Nationale" = "#2ca02c",
  "Saguenay–Lac-Saint-Jean" = "#FFC125",
  "Montérégie" = "#ff7f0e",     
  "Mauricie" = "#8B795E",
  "Lanaudière"= "#EE6AA7",
  "Chaudière-Appalaches" = "#9467bd"
)

linetypes_ = c(
  "Quebec Province" = "dashed",
  "Montréal" = "solid",
  "Laval" = "solid",
  "Capitale-Nationale" = "solid",
  "Saguenay–Lac-Saint-Jean" = "solid",
  "Montérégie" = "solid",     
  "Mauricie" = "solid",
  "Lanaudière"= "solid",
  "Chaudière-Appalaches" = "solid"
)

data_long <- data_long %>%
  mutate(region_absolute_highlight = case_when(
    region %in% names (colorsforchosenregions) ~ region,
    TRUE ~ "regionsingray"
  ))


# 

data_last_year_absolute <- data_long %>%
  group_by(region) %>%
  filter(year == max(year)) %>%
  ungroup()

### 
data_last_year_absolute <- data_last_year_absolute %>%
  mutate(region_absolute_highlight = case_when(
    region %in% names(colorsforchosenregions) ~ region,
    TRUE ~ "regionsingray"
  ))


# Gráfico adaptado com cores personalizadas e linhas destacadas
ggplot(data_long, aes(x = year, y = value, color = region_absolute_highlight, group = region)) +
  geom_line(size = 1, aes(linetype = region_absolute_highlight)) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(colorsforchosenregions, "regionsingray" = "gray80"),
    breaks = names(colorsforchosenregions)
  ) +
  scale_linetype_manual(
    values = c(linetypes_, "regionsingray" = "dashed"),
    breaks = names(colorsforchosenregions)
  ) +
  theme_minimal() +
  labs(
    title = "Net international migration rate, Quebec province and administrative regions, 2001–2024",
    x = "Year",
    y = "Value",
    color = "Region",
    linetype = "Region"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )



# Table
install.packages("gridExtra")
library(grid)
library(gridExtra)
library(dplyr)

# 
table1_data <- data_last_year_absolute %>%
  filter(region_absolute_highlight != "regionsingray") %>%
  select(region_absolute_highlight, value) %>%
  mutate(value = round(value, 2))   %>%
  arrange(desc(value))

# 
table1_plot <- tableGrob(table1_data, theme = ttheme_minimal(
  core = list(fg_params = list(col = colorsforchosenregions[table1_data$region_absolute_highlight])),
  colhead = list(fg_params = list(col = "transparent")), # Remover título das colunas
  rowhead = list(fg_params = list(col = "transparent"))  # Remover título das linhas
))

# 
grid.newpage()
grid.draw(table1_plot)



#########################################################################################################

#This graphic will probably not be used in our presentation:

ggplot(data_long, aes(x = year, y = value, fill = region)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Net international migration rate, Quebec province and administrative regions, 2001–2024",
    x = "Year",
    y = "Value",
    fill = "Region"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#########################################################################################################


####
####
####


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
    title = "Growth of the Net Immigration Rate Relative to First Year (2001)",
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


### 
data_last_year_relative <- data_last_year_relative %>%
  mutate(region_highlight = case_when(
    region %in% names(colorsforchosenregions) ~ region,
    TRUE ~ "regionsingray"
  ))

data_relative <- data_relative %>%
  mutate(region_highlight = case_when(
    region %in% names(colorsforchosenregions) ~ region,
    TRUE ~ "regionsingray"
  ))




ggplot(data_relative, aes(x = year, y = value_rel, color = region, group = region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Growth of the Net Immigration Rate Relative to First Year (2001)",
    x = "Year",
    y = "Growth (vs. 1st year)",
    color = "Region"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_text(
    data = data_last_year_relative, 
    aes(label = round(value_rel, 2)),  
    hjust = 1.5,   
    size = 3       
  )

#Wow! Saguenay-Lac-Saint-Jean:
#This represents a growth equivalent to 77.74 times the absolute value of the initial figure.
#Alternatively: The value increased by 7674% relative to the absolute value of the starting point.

install.packages("scales")
library(scales)

### 

ggplot(data_relative, aes(x = year, y = value_rel, color = region_highlight, group = region, linetype = region_highlight)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(colorsforchosenregions, "regionsingray" = "gray80"),
    breaks = names(colorsforchosenregions)
  ) +
  scale_linetype_manual(
    values = c(linetypes_, "regionsingray" = "dashed"),
    breaks = names(colorsforchosenregions)
  ) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Growth of the Net Immigration Rate Relative to First Year (2001)",
    x = "Year",
    y = "Growth (vs. 1st year)",
    color = "Region",
    linetype = "Region"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_text(
    data = data_last_year_relative %>% filter(region_highlight != "regionsingray"),
    aes(x = year, y = value_rel, label = round(value_rel, 2), color = region_highlight),
    hjust = 0.8,
    vjust = 2,
    size = 4,
    fontface = "bold"
  )




### Without the numbers (which will be presented in table)
ggplot(data_relative, aes(x = year, y = value_rel, color = region_highlight, group = region, linetype = region_highlight)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(colorsforchosenregions, "regionsingray" = "gray80"),
    breaks = names(colorsforchosenregions)
  ) +
  scale_linetype_manual(
    values = c(linetypes_, "regionsingray" = "dashed"),
    breaks = names(colorsforchosenregions)
  ) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Growth of the Net Immigration Rate Relative to First Year (2001)",
    x = "Year",
    y = "Growth (vs. 1st year)",
    color = "Region",
    linetype = "Region"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")
  



# Table
install.packages("gridExtra")
library(grid)
library(dplyr)


# 
table_data <- data_last_year_relative %>%
  filter(region_highlight != "regionsingray") %>%
  select(region_highlight, value_rel) %>%
  mutate(value_rel = round(value_rel, 2))   %>%
  arrange(desc(value_rel))

# 
table_plot <- tableGrob(table_data, theme = ttheme_minimal(
  core = list(fg_params = list(col = colorsforchosenregions[table_data$region_highlight])),
  colhead = list(fg_params = list(col = "transparent")), 
  rowhead = list(fg_params = list(col = "transparent")) 
))

# 
grid.newpage()
grid.draw(table_plot)







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
    aes (x = year, y = value_rel, label = round(value_rel, 2), color = region_highlight),
        hjust = 0.8,
        vjust = 2,
        size = 4,
        fontface = "bold"
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
    aes (x = year, y = value_rel, label = round(value_rel, 2), color = region_highlight),
        hjust = 0.8,
        vjust = 2,
        size = 4,
      fontface = "bold"
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


data_relative_with_reference <- data_relative_with_reference %>%
  mutate(region_highlight = case_when(
    region %in% names(colorsforchosenregions) ~ region,
    TRUE ~ "regionsingray"
  ))
    


ggplot(data_relative_with_reference, aes(x = year, y = value_rel, color = region_highlight, group = region, linetype=region_highlight)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(colorsforchosenregions, "regionsingray" = "gray80"),
    breaks = names(colorsforchosenregions)
  ) +
  scale_linetype_manual(
    values = c(linetypes_, "regionsingray" = "dashed"),
    breaks = names(colorsforchosenregions)
  )+
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "10 years Growth of the Net Immigration Rate - Relative to 2014-2015",
    x = "Year",
    y = "Growth (vs. 2014-2015)",
    color = "Region"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_text(
    data = data_relative_with_reference %>%
      filter(year == "2023-2024"),  # Add text only for the last year
    aes(x = year, y = value_rel, label = round(value_rel, 2), color = region_highlight,
    hjust = 0.8,
    vjust = 2,
    size = 4,
    fontface = "bold"
    ))



# Without numbers (which will be presented in tables)



ggplot(data_relative_with_reference, aes(x = year, y = value_rel, color = region_highlight, group = region, linetype=region_highlight)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(colorsforchosenregions, "regionsingray" = "gray80"),
    breaks = names(colorsforchosenregions)
  ) +
  scale_linetype_manual(
    values = c(linetypes_, "regionsingray" = "dashed"),
    breaks = names(colorsforchosenregions)
  )+
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "10 years Growth of the Net Immigration Rate - Relative to 2014-2015",
    x = "Year",
    y = "Growth (vs. 2014-2015)",
    color = "Region"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")

#Table



data10yrs_relative_with_reference <- data_relative_with_reference %>%
  group_by(region) %>%
  filter(year == max(year)) %>%
  ungroup()

table10yrs_data <- data10yrs_relative_with_reference %>%
  filter(region_highlight != "regionsingray") %>%
  select(region_highlight, value_rel) %>%
  mutate(value_rel = round(value_rel, 2))   %>%
  arrange(desc(value_rel))

# 
table10yrs_plot <- tableGrob(table10yrs_data, theme = ttheme_minimal(
  core = list(fg_params = list(col = colorsforchosenregions[table10yrs_data$region_highlight])),
  colhead = list(fg_params = list(col = "transparent")),
  rowhead = list(fg_params = list(col = "transparent"))  
))

# 
grid.newpage()
grid.draw(table10yrs_plot)



#Growth post-COVID-19, the last 3 years

# We need to set the colors to other administrative regions

colorsforchosenregions_3 = c(
  "Quebec Province" = "#0000FF",
  "Montréal" = "#CD4F39",
  "Laval" = "#A0522D",
  "Capitale-Nationale" = "#2ca02c",
  "Saguenay–Lac-Saint-Jean" = "#FFC125",
  "Montérégie" = "#ff7f0e",     
  "Mauricie" = "#8B795E",
  "Lanaudière"= "#EE6AA7",
  "Chaudière-Appalaches" = "#9467bd",
  "Estrie" = "#FFC1C1",
  "Bas-Saint-Laurent" = "#2E8B57",
  "Abitibi-Témiscamingue" = "#00E5EE",
  "Centre-du-Québec" = "#551A8B",
  "Côte-Nord" = "#EEE5DE",
  "Laurentides" = "#C0FF3E",
  "Gaspésie–Îles-de-la-Madeleine" = "#87CEFA",
  "Nord-du-Québec" = "#EEE",
  "Outaouais" = "#63b8FF"
  )

linetypes_3 = c(
  "Quebec Province" = "dashed",
  "Montréal" = "solid",
  "Laval" = "solid",
  "Capitale-Nationale" = "solid",
  "Saguenay–Lac-Saint-Jean" = "solid",
  "Montérégie" = "solid",     
  "Mauricie" = "solid",
  "Lanaudière"= "solid",
  "Chaudière-Appalaches" = "solid",
  "Estrie" = "solid",
  "Bas-Saint-Laurent" = "solid",
  "Abitibi-Témiscamingue" = "solid",
  "Centre-du-Québec" = "solid",
  "Côte-Nord" = "solid",
  "Laurentides" = "solid",
  "Gaspésie–Îles-de-la-Madeleine" = "solid",
  "Nord-du-Québec" = "solid",
  "Outaouais" = "solid"
)



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

data_relative_with_reference3 <- data_relative_with_reference3 %>%
  mutate(region_highlight = case_when(
    region %in% names(colorsforchosenregions_3) ~ region
  ))


ggplot(data_relative_with_reference3, aes(x = year, y = value_rel, color = region_highlight, group = region, linetype=region_highlight)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(colorsforchosenregions_3),
    breaks = names(colorsforchosenregions_3)
  ) +
  scale_linetype_manual(
    values = c(linetypes_3),
    breaks = names(colorsforchosenregions_3)
  )+
    geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Post-Covid 19 Growth of the Net Immigration Rate - Relative to 2021-2022",
    x = "Year",
    y = "Growth (vs. 2021-2022)",
    color = "Region"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_text(
    data = data_relative_with_reference3 %>%
      filter(year == "2023-2024"),  # Add text only for the last year
    aes(x = year, y = value_rel, label = round(value_rel, 2), color = region_highlight),
    hjust = 0.8,
    vjust = 2,
    size = 4,
    fontface = "bold"
  )


# Without numbers (which will be presented in tables)

ggplot(data_relative_with_reference3, aes(x = year, y = value_rel, color = region_highlight, group = region, linetype=region_highlight)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(colorsforchosenregions_3),
    breaks = names(colorsforchosenregions_3)
  ) +
  scale_linetype_manual(
    values = c(linetypes_3),
    breaks = names(colorsforchosenregions_3)
  )+
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Post-Covid 19 Growth of the Net Immigration Rate - Relative to 2021-2022",
    x = "Year",
    y = "Growth (vs. 2021-2022)",
    color = "Region"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")
  


#Table


data3yrs_relative_with_reference <- data_relative_with_reference %>%
  group_by(region) %>%
  filter(year == max(year)) %>%
  ungroup()

table3yrs_data <- data3yrs_relative_with_reference %>%
  select(region_highlight, value_rel) %>%
  mutate(value_rel = round(value_rel, 2))   %>%
  arrange(desc(value_rel))

# 
table3yrs_plot <- tableGrob(table3yrs_data, theme = ttheme_minimal(
  core = list(fg_params = list(col = colorsforchosenregions_3[table3yrs_data$region_highlight])),
  colhead = list(fg_params = list(col = "transparent")),
  rowhead = list(fg_params = list(col = "transparent"))  
))

# 
grid.newpage()
grid.draw(table3yrs_plot)










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
