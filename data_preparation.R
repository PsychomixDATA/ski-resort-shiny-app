# --------------------------------------------------
# Clean the original ski resort dataset
# --------------------------------------------------

library(dplyr)
library(readr)
library(janitor)
library(stringr)

# Read the ORIGINAL file.
# The file uses Windows-1252 encoding.
resorts_raw <- read.csv(
  "resorts.csv",
  fileEncoding = "Windows-1252",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Confirm that the import worked correctly
dim(resorts_raw)
names(resorts_raw)
head(resorts_raw[, c("ID", "Resort", "Country", "Total slopes")])

# Clean names and prepare variables
resorts_clean <- resorts_raw %>%
  janitor::clean_names() %>%
  
  mutate(
    # Clean text fields
    resort = str_squish(as.character(resort)),
    country = str_squish(as.character(country)),
    continent = str_squish(as.character(continent)),
    season = str_squish(as.character(season)),
    
    # Explicitly convert numeric variables
    id = as.integer(id),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    price = as.numeric(price),
    highest_point = as.numeric(highest_point),
    lowest_point = as.numeric(lowest_point),
    beginner_slopes = as.numeric(beginner_slopes),
    intermediate_slopes = as.numeric(intermediate_slopes),
    difficult_slopes = as.numeric(difficult_slopes),
    total_slopes = as.numeric(total_slopes),
    longest_run = as.numeric(longest_run),
    snow_cannons = as.numeric(snow_cannons),
    surface_lifts = as.numeric(surface_lifts),
    chair_lifts = as.numeric(chair_lifts),
    gondola_lifts = as.numeric(gondola_lifts),
    total_lifts = as.numeric(total_lifts),
    lift_capacity = as.numeric(lift_capacity),
    
    # Standardize Yes/No fields
    child_friendly = str_to_title(str_trim(child_friendly)),
    snowparks = str_to_title(str_trim(snowparks)),
    nightskiing = str_to_title(str_trim(nightskiing)),
    summer_skiing = str_to_title(str_trim(summer_skiing))
  ) %>%
  
  # Remove malformed rows, if any
  filter(
    !is.na(id),
    !is.na(resort),
    resort != "",
    !is.na(country),
    country != ""
  ) %>%
  
  # Remove smaller local resorts
  filter(
    total_slopes >= 40,
    total_lifts >= 5
  ) %>%
  
  # Create derived variables
  mutate(
    vertical_drop = highest_point - lowest_point,
    
    beginner_percent = if_else(
      total_slopes > 0,
      beginner_slopes / total_slopes * 100,
      NA_real_
    ),
    
    intermediate_percent = if_else(
      total_slopes > 0,
      intermediate_slopes / total_slopes * 100,
      NA_real_
    ),
    
    difficult_percent = if_else(
      total_slopes > 0,
      difficult_slopes / total_slopes * 100,
      NA_real_
    ),
    
    # Zero prices are treated as missing because a free day pass
    # is more likely to represent unavailable data.
    price_clean = na_if(price, 0),
    
    lift_capacity_clean = na_if(lift_capacity, 0),
    
    price_per_km = if_else(
      !is.na(price_clean) & total_slopes > 0,
      price_clean / total_slopes,
      NA_real_
    ),
    
    capacity_per_lift = if_else(
      total_lifts > 0 & !is.na(lift_capacity_clean),
      lift_capacity_clean / total_lifts,
      NA_real_
    )
  )

# --------------------------------------------------
# Calculate the fixed value score
# --------------------------------------------------

resorts_clean <- resorts_clean %>%
  mutate(
    terrain_score = percent_rank(total_slopes),
    vertical_score = percent_rank(vertical_drop),
    lift_score = percent_rank(lift_capacity_clean),
    longest_run_score = percent_rank(longest_run),
    affordability_score = 1 - percent_rank(price_clean),
    
    value_score = 100 * (
      0.30 * terrain_score +
        0.25 * vertical_score +
        0.15 * lift_score +
        0.10 * longest_run_score +
        0.20 * affordability_score
    ),
    
    value_score = round(value_score, 1)
  )

# --------------------------------------------------
# Validate the cleaned data
# --------------------------------------------------

# Check dimensions
dim(resorts_clean)

# Resort names should be ordinary individual names,
# not collapsed strings containing entire rows.
head(resorts_clean$resort, 20)

# Inspect key fields
resorts_clean %>%
  select(
    id,
    resort,
    country,
    continent,
    price_clean,
    total_slopes,
    total_lifts,
    vertical_drop,
    value_score
  ) %>%
  print(n = 20)

# Confirm no duplicate IDs
sum(duplicated(resorts_clean$id))

# Confirm all included resorts meet the minimum requirements
min(resorts_clean$total_slopes, na.rm = TRUE)
min(resorts_clean$total_lifts, na.rm = TRUE)

# Check variable types
str(resorts_clean)

# --------------------------------------------------
# Save a new clean CSV
# --------------------------------------------------

readr::write_excel_csv(
  resorts_clean,
  "resorts_cl.csv",
  na = ""
)
