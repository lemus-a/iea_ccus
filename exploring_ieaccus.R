library(tidyverse)
library(readxl)
library(janitor)
library(styler)
library(countrycode)


# import data from wd()
iea_ccus <- read_excel("./IEA CCUS Projects Database 2023.xlsx", sheet = 4)

# 1. Initial Data Cleaning ------------------------------------------------
names(iea_ccus)

# remove necessary columns and clean names.
iea_ccus <- iea_ccus |>
  select(!(16:29)) |>
  clean_names()

# give easier names
iea_ccus <- iea_ccus |>
  rename(
    project = project_name,
    type = project_type,
    ann = announcement,
    sus_dec = suspension_decommissioning,
    status = project_status,
    phase = project_phase,
    cap_low = announced_capacity_low_mt_co2_yr,
    cap_high = announced_capacity_high_mt_co2_yr,
    carbon_fate = fate_of_carbon,
    hub = part_of_ccus_hub
  )


# add region column. After trying out 'Continent', went with 'region' as it's more appropriate for the dataset.
iea_ccus$region <- countrycode(
  sourcevar = iea_ccus[["country"]],
  origin = "country.name",
  destination = "region"
)

iea_ccus <- iea_ccus |>
  relocate(region, .after = country)

iea_ccus |>
  distinct(region)


# a look at the dataset see no projects in Caribbean or Central Asia. Better to rename these
iea_ccus <- iea_ccus |>
  mutate(
    region = case_when(
      region == "Europe & Central Asia" ~ "Europe",
      region == "Latin America & Caribbean" ~ "Latin America",
      TRUE ~ region
    )
  )


# take a look at the region with "Asia". Seems too messy. seems best to separate into 3 separate groups: East Asia, Oceania, South East Asia. india is already in South Asia.
iea_ccus |>
  filter(str_detect(region, "Asia")) |>
  distinct(country)


iea_ccus <- iea_ccus |>
  mutate(
    region = case_when(
      country %in% c("Australia", "New Zealand", "Papua New Guinea") ~ "Oceania",
      country %in% c("Korea", "People's Republic of China", "Japan", "Chinese Taipei") ~ "East Asia",
      country %in% c("Thailand", "Indonesia", "Malaysia", "Singapore") ~ "Southeast Asia",
      TRUE ~ region
    )
  )



# got warning message that since there were many countries in same row or some non country names, region was set to NA. Need to see what to do about those NAs. I'll try two ways: find those that have multiple countries under (using text search with ",") or where region == NA.
iea_ccus |>
  filter(is.na(region))


iea_ccus |>
  filter(str_detect(country, ","))


# so it turns out that all countries where there multiple countries (separated by ",") are in Europe but those are not all the NAs... need to fix those remaining 4 by hand.

# Adding Europe to the missing European ones
iea_ccus <- iea_ccus |>
  mutate(region = case_when(
    str_detect(country, ",") == TRUE ~ "Europe",
    TRUE ~ region
  ))


# see remaining NAs in region (should only be 4). Now need to find how to address these manually.
iea_ccus |>
  filter(is.na(region))


# Libya, tweaking last four observations without country/region
iea_ccus <- iea_ccus |>
  mutate(
    region = case_when(
      country == "Lybia" ~ "Middle East & North Africa",
      country == "Unknown (Europe)" ~ "Europe",
      TRUE ~ region
    ),
    country = case_when(
      country == "Lybia" ~ "Libya",
      str_detect(country, "Unknown") == TRUE ~ as.character(NA),
      TRUE ~ country
    )
  )


# FACTOR TIME #
str(iea_ccus)

# code gets the no. of unique values per column
iea_ccus |>
  summarise_all(
    .funs = list(
      n_unique = ~ n_distinct(.)
    )
  ) |>
  view()

# check to see which could be factors
distinct(iea_ccus, carbon_fate)

names(iea_ccus)
# chose columns to make as factors
fct_cols <- c(2, 3, 5, 10, 11, 14, 15, 16)

# make factors
iea_ccus <- iea_ccus |>
  mutate(across(any_of(fct_cols), factor))



# check all factors at once
iea_ccus |>
  sapply(levels)


colSums(is.na(iea_ccus))


# 2. Look into currently operational CCS projects -------------------------
library(skimr)

skim(mtcars)

summary(iea_ccus)

distinct(iea_ccus, sector)

iea_ccus |>
  distinct(type)

iea_ccus |>
  distinct(status)


# total CCS in operation (capacity in operation)
iea_ccus |>
  filter(status == "Operational" & type != "CCU") |>
  view()

iea_ccus |>
  filter(status == "Operational" & type == "Full chain") |>
  view()




# total CCS in operation (capacity in operation) range of operations

iea_ccus |>
  filter(status == "Operational" & type != "CCU") |>
  summarize(total = sum(cap_low, na.rm = TRUE))

# low range 87.3

iea_ccus |>
  filter(status == "Operational" & type != "CCU") |>
  summarize(total = sum(cap_high, na.rm = TRUE))


# high range range 96.0



iea_ccus |>
  filter(status == "Operational" & type != "CCU") |>
  ggplot(aes(fct_infreq(region))) +
  geom_bar(aes(color = region, fill = region)) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = 1) +
  theme_minimal() +
  labs(x = "Region", y = "", title = "Operational CCS Projects") +
  theme(
    axis.text.x = element_text(
      angle = 45,
      size = 10,
      vjust = 0.5,
      hjust = 0.5
    ),
    text = element_text(size = 15),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

packageVersion("ggplot2")

packageVersion("rlang")

update.packages("rlang")

?fct_infreq


?after_stat()

# Total Storage capacity (low range)

iea_ccus |>
  distinct(project_type)

iea_ccus |>
  distinct(project_status)


iea_ccus |>
  filter(project_status %in% c("Planned", "Operational", "Under construction") & project_type %in% c("Storage", "Full Chain", "T&S")) |>
  view()


# low range
iea_ccus |>
  filter(project_status %in% c("Planned", "Operational", "Under construction") & project_type %in% c("Storage", "Full Chain", "T&S")) |>
  summarize(total = sum(announced_capacity_low_mt_co2_yr, na.rm = TRUE))


# high range
iea_ccus |>
  filter(project_status %in% c("Planned", "Operational", "Under construction") & project_type %in% c("Storage", "Full Chain", "T&S")) |>
  summarize(total = sum(announced_capacity_high_mt_co2_yr, na.rm = TRUE))








# what chatpgpt gave me...
iea_ccus |> mutate(across(where(~ is.character(.) & n_distinct(.) < 10), as.factor))


?mutate(across())
