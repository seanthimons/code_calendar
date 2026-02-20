# packages ---------------------------------------------------------------

{
  list.of.packages <- c(
    'here',
    'rio',
    'arrow',
    'tidyverse',
    'janitor',
    'esquisse',
    'skimr',
    'timetk',
    'sf', #if plotting
    'mapview', # if plotting
    'mapgl',
    'toxpiR'
  )

  new.packages <- list.of.packages[
    !(list.of.packages %in% installed.packages()[, "Package"])
  ]
  if (length(new.packages)) install.packages(new.packages)

  lapply(list.of.packages, function(pkg) {
    if (!require(pkg, character.only = TRUE)) {
      #install.packages(pkg)
      library(pkg, character.only = TRUE)
    }
  })

  rm(list.of.packages, new.packages)
}

# custom functions -------------------------------------------------------
{
  `%ni%` <- Negate(`%in%`)

  ## for printing of colnames for selection
  prettylist <- function(x) {
    paste0("'", x, "',", "\n") %>% cat()
  }

  skim_count <- skim_with(
    numeric = sfl(n = length, median = ~ median(.x, na.rm = T))
  )
}

# raw --------------------------------------------------------------------

{
  tuesdata <- tidytuesdayR::tt_load('2025-05-20')

  water_quality <- tuesdata$water_quality
  weather <- tuesdata$weather

  rm(tuesdata)
}

# Data Overview ----------------------------------------------------------

## Water Quality Data ----

### Basic Statistics ----
print("Water Quality Data - Basic Statistics:")
print(skimr::skim(water_quality))

### Missing Values and Data Types ----
print("Water Quality Data - Missing Values:")
print(sapply(water_quality, function(x) sum(is.na(x))))
print("Water Quality Data - Data Types:")
print(sapply(water_quality, class))

### Unique Value Distributions ----
print("Water Quality Data - Unique Value Distributions (first 10 columns):")
for (col in colnames(water_quality)[1:10]) {
  print(paste("Column:", col))
  print(unique(water_quality[[col]]))
}

## Weather Data ----

### Basic Statistics ----
print("Weather Data - Basic Statistics:")
print(skimr::skim(weather))

### Missing Values and Data Types ----
print("Weather Data - Missing Values:")
print(sapply(weather, function(x) sum(is.na(x))))
print("Water Quality Data - Data Types:")
print(sapply(weather, class))

### Unique Value Distributions ----
print("Weather Data - Unique Value Distributions (first 10 columns):")
for (col in colnames(weather)[1:10]) {
  print(paste("Column:", col))
  print(unique(weather[[col]]))
}

# Visualizations -----------------------------------------------------------

## Numerical Variables - Histograms ##
water_quality %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Histograms of Numerical Variables (Water Quality)")

weather %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Histograms of Numerical Variables (Weather)")

## Numerical Variables - Boxplots ##
water_quality %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(y = value)) +
  geom_boxplot(fill = "steelblue") +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Boxplots of Numerical Variables (Water Quality)")

weather %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(y = value)) +
  geom_boxplot(fill = "steelblue") +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Boxplots of Numerical Variables (Weather)")


## Categorical Variables - Bar Charts ----

### Water Quality ----
water_quality %>%
  select(where(is.character)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable, value) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = value, y = n, fill = value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Bar Charts of Categorical Variables (Water Quality)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Weather ----
weather %>%
  select(where(is.character)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable, value) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = value, y = n, fill = value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Bar Charts of Categorical Variables (Weather)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Relationships - Correlation Matrix ----

### Water Quality ###
water_quality_numeric <- water_quality %>% select(where(is.numeric))
cor_matrix_water <- cor(water_quality_numeric, use = "pairwise.complete.obs")
cor_water_df <- as.data.frame(cor_matrix_water)

print("Water Quality - Correlation Matrix")
print(cor_water_df)

### Weather ###
weather_numeric <- weather %>% select(where(is.numeric))
cor_matrix_weather <- cor(weather_numeric, use = "pairwise.complete.obs")
cor_weather_df <- as.data.frame(cor_matrix_weather)

print("Weather - Correlation Matrix")
print(cor_weather_df)

# Spatial Analysis (if applicable) ----------------------------------------

### Water Quality ----

wq_sf <- water_quality %>%
  #glimpse()
  select(
    council,
    swim_site,
    longitude,
    latitude
  ) %>%
  distinct() %>%
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326)

#sf::st_is_valid(wq_sf)

mapview(wq_sf, zcol = 'council', burst = TRUE, legend = FALSE)

# maplibre(
#   bounds = wq_sf
# ) %>%
#   add_circle_layer(
#     .,
#     id = "poi-layer",
#     source = wq_sf,
#     # circle_color = match_expr(
#     #   'category',
#     #   values = wq_sf$swim_site,
#     #   stops = randomcoloR::distinctColorPalette(k = length(unique(water_quality$swim_site)))
#     # ),
#     cluster_options = cluster_options(),
#     hover_options = list(
#       circle_radius = 12,
#       circle_color = "#ffff99"
#     )
#   )

# Quality Assessment -------------------------------------------------------

## Outlier Detection (example using IQR method) ##

outlier_threshold_upper <- function(x) {
  quantile(x, 0.75, na.rm = TRUE) + 1.5 * IQR(x, na.rm = TRUE)
}

outlier_threshold_lower <- function(x) {
  quantile(x, 0.25, na.rm = TRUE) - 1.5 * IQR(x, na.rm = TRUE)
}

### Water Quality ###
print(
  "Water Quality - Outlier Thresholds (example for first 5 numeric columns):"
)
for (col in colnames(water_quality_numeric)[1:5]) {
  upper <- outlier_threshold_upper(water_quality_numeric[[col]])
  lower <- outlier_threshold_lower(water_quality_numeric[[col]])
  print(paste(
    "Column:",
    col,
    "Upper Threshold:",
    upper,
    "Lower Threshold:",
    lower
  ))
}

### Weather ###
print("Weather - Outlier Thresholds (example for first 5 numeric columns):")
for (col in colnames(weather_numeric)[1:5]) {
  upper <- outlier_threshold_upper(weather_numeric[[col]])
  lower <- outlier_threshold_lower(weather_numeric[[col]])
  print(paste(
    "Column:",
    col,
    "Upper Threshold:",
    upper,
    "Lower Threshold:",
    lower
  ))
}

# Temporal Patterns (if applicable) -------------------------------------

## Time Series Analysis (example assuming a 'date' column exists) ##

### Water Quality ----
if ("date" %in% colnames(water_quality)) {
  water_quality %>%
    ggplot(aes(x = date, y = enterococci_cfu_100ml)) +
    geom_line() +
    labs(title = "Time Series Plot (Water Quality)")
}

### Weather ----
if ("date" %in% colnames(weather)) {
  weather %>%
    ggplot(aes(x = date, y = max_temp_C)) +
    geom_line() +
    labs(title = "Time Series Plot (Weather)")
}

# Insights & Documentation -------------------------------------------------

## Key Findings Summary ----
print(
  "Key Findings: [To be completed after reviewing the above outputs and plots]"
)

## Data Quality Issues ----
print(
  "Data Quality Issues: [To be completed after reviewing the above outputs]"
)

## Variable Relationships ----
print(
  "Variable Relationships: [To be completed after reviewing the correlation matrices and plots]"
)

## Next Steps Recommendations ----
print(
  "Next Steps: [To be completed based on the above findings, could include data cleaning, feature engineering, modeling, etc.]"
)
