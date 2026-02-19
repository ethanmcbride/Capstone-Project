library(readxl)
library(tidyverse)

#####
# Load the data set
anes_data <- read_csv("data/anes_timeseries_cdf_csv_20251211.csv")

# View structure of the data set
str(anes_data)

# View col_character() columns
anes_data |> 
  select(where(is.character)) |>
  glimpse()

#####
# Clean ANES data set

# Create disabled variable for time series
anes_data$disabled = case_when(
  anes_data$VCF0150 == 16 | anes_data$VCF0150 == 60 | anes_data$VCF0150 == 61 ~ 1, 
  # If individual is permanently disabled, regardless of hours worked, assign 1
  TRUE ~ 0 # Otherwise, assign 0
)

anes_data_clean <- anes_data |>
  rename(
    year = VCF0004,
    age = VCF0101,
    state = VCF0901a,
    # ex_eff = VCF0648, Calculated from VCF0609 & VCF0613
    # 1 = 0, 2 = 100, 3 = 50; re-coded values totaled and divided by number of valid responses to create index from 0-100
    voted = VCF0702,
    ed_level = VCF0140,
    income_level = VCF0114,
    govt_cares = VCF0609,
    say_in_govt = VCF0613,
    female = VCF0104,
    census_reg = VCF0112,
    attend_political_meetings = VCF0718,
    work_for_campaigns = VCF0719,
    donate_money = VCF0720,
    liberal_conservative_scale = VCF0803,
    federal_welfare_spending = VCF0894
  ) |>
  select(c(year, age, state, voted, ed_level, income_level,
           govt_cares, say_in_govt, female, census_reg, attend_political_meetings, 
           work_for_campaigns, donate_money, liberal_conservative_scale, 
           federal_welfare_spending, disabled)) |>
  filter(year >= 1990 & year < 2020)

anes_data_clean$age <- ifelse(
  anes_data_clean$age == 00, 
  NA, anes_data_clean$age) # Re-code 00 as NA for age variable

anes_data_clean$state <- ifelse(
  anes_data_clean$state == 9999, 
  NA, anes_data_clean$state) # Re-code 9999 as NA for state variable

anes_data_clean$voted <- case_when(
  anes_data_clean$voted == 1 ~ 0, # If did not vote, assign 0
  anes_data_clean$voted == 2 ~ 1, # If voted, assign 1
  TRUE ~ NA_real_ # Otherwise, assign NA
)

anes_data_clean$income_level <- ifelse(
  anes_data_clean$income_level == 0,
  NA, anes_data_clean$income_level) # Re-code 0 as NA for income level variable

anes_data_clean$govt_cares <- case_when(
  anes_data_clean$govt_cares == 1 ~ 100, # If respondent believes government cares, assign 100 
  anes_data_clean$govt_cares == 2 ~ 0, # If respondent believes government does not care, assign 0
  anes_data_clean$govt_cares == 3 ~ 50, # If respondent is unsure, assign 50
  TRUE ~ NA_real_ # Otherwise, assign NA
)

anes_data_clean$say_in_govt <- case_when(
  anes_data_clean$say_in_govt == 1 ~ 100, # If respondent believes they have a say in government, assign 100
  anes_data_clean$say_in_govt == 2 ~ 0, # If respondent believes they do not have a say in government, assign 0
  anes_data_clean$say_in_govt == 3 ~ 50, # If respondent is unsure, assign 50
  TRUE ~ NA_real_ # Otherwise, assign NA
)

anes_data_clean$female <- case_when(
  anes_data_clean$female == 1 ~ 0, # If respondent is male, assign 0
  anes_data_clean$female == 2 ~ 1, # If respondent is female, assign 1
  TRUE ~ NA_real_ # Otherwise, assign NA
)

anes_data_clean$attend_political_meetings <- case_when(
  anes_data_clean$attend_political_meetings == 1 ~ 0, # If respondent did not attend a political meetings, assign 0
  anes_data_clean$attend_political_meetings == 2 ~ 1, # If respondent attended a political meeting, assign 1
  TRUE ~ NA_real_ # Otherwise, assign NA
)

anes_data_clean$work_for_campaigns <- case_when(
  anes_data_clean$work_for_campaigns == 1 ~ 0, # If respondent did not work for a political campaign, assign 0
  anes_data_clean$work_for_campaigns == 2 ~ 1, # If respondent worked for a political campaign, assign 1
  TRUE ~ NA_real_ # Otherwise, assign NA
)

anes_data_clean$donate_money <- case_when(
  anes_data_clean$donate_money == 1 ~ 0, # If respondent did not donate money to a political campaign, assign 0
  anes_data_clean$donate_money == 2 ~ 1, # If respondent donated money to a political campaign, assign 1
  TRUE ~ NA_real_ # Otherwise, assign NA
)

anes_data_clean$liberal_conservative_scale <- ifelse(
  anes_data_clean$liberal_conservative_scale %in% c(0, 9),
  NA, anes_data_clean$liberal_conservative_scale) # Re-code 0 as NA if no thermometer

anes_data_clean$federal_welfare_spending <- case_when(
  anes_data_clean$federal_welfare_spending == 1 ~ 100, # If respondent thinks welfare should increase, assign 100
  anes_data_clean$federal_welfare_spending == 2 ~ 50, # If respondent thinks welfare should stay the same, assign 50
  anes_data_clean$federal_welfare_spending == 3 ~ 0, # If respondent thinks welfare should decrease, assign 0
  TRUE ~ NA_real_ # Otherwise, assign NA
)

#####
## Loading ANES data for 2020
anes_2020 <- read_csv("data/anes_timeseries_2020_csv_20220210.csv")
anes_2020$year <- 2020

# Disability status = V201533x
anes_2020$disabled <- ifelse(
  anes_2020$V201533x == 16 | anes_2020$V201533x == 60 | anes_2020$V201533x == 61, 1, 0)

## Cleaned ANES 2020
anes_2020_clean <- anes_2020 |>
  rename(
    age = V201507x,
    state = V203000,
    voted = V202109x,
    ed_level = V201510,
    income_level = V202468x,
    govt_cares = V202212,
    say_in_govt = V202213,
    female = V201600,
    census_reg = V203003,
    attend_political_meetings = V202014,
    work_for_campaigns = V202016,
    donate_money = V202015,
    liberal_conservative_scale = V201200,
    federal_welfare_spending = V201312
  ) |>
  select(c(year, age, voted, state, ed_level, income_level,
           govt_cares, say_in_govt, female, census_reg,
           attend_political_meetings, work_for_campaigns, donate_money,
           liberal_conservative_scale, federal_welfare_spending, disabled))

# Clean data
anes_2020_clean$age <- ifelse(
  anes_2020_clean$age == -9, 
  NA, anes_2020_clean$age) # Re-code -9 as NA for age variable

anes_2020_clean$voted <- case_when(
  anes_2020_clean$voted == 0 ~ 0, # If respondent did not vote, assign 0
  anes_2020_clean$voted == 1 ~ 1, # If respondent voted, assign 1
  TRUE ~ NA_real_ # Otherwise, assign NA
)

anes_2020_clean$ed_level <- case_when(
  anes_2020_clean$ed_level > 0 & anes_2020_clean$ed_level < 9 ~ anes_2020_clean$ed_level, # If respondent has valid education level, keep value
  TRUE ~ NA_real_ # Otherwise, assign NA
)

anes_2020_clean$income_level <- case_when(
  anes_2020_clean$income_level < 0 ~ NA, # Re-code negative values as NA for income level variable
  TRUE ~ anes_2020_clean$income_level # Otherwise, keep value
)
  
anes_2020_clean$govt_cares <- case_when(
  anes_2020_clean$govt_cares >= 0 & anes_2020_clean$govt_cares <= 2 ~ 100, # Re-code 1 and 2 as 100 for government cares variable
  anes_2020_clean$govt_cares == 3 ~ 50, # Re-code 3 as 50 for government cares variable
  anes_2020_clean$govt_cares > 3 ~ 0, # Re-code values greater than 3 as 0 for government cares variable
  TRUE ~ NA_real_ # Otherwise, assign NA
)

anes_2020_clean$say_in_govt <- case_when(
  anes_2020_clean$say_in_govt >= 0 & anes_2020_clean$say_in_govt <= 2 ~ 0, # Re-code 1 and 2 as 0 for say in gov't variable, no say
  anes_2020_clean$say_in_govt == 3 ~ 100, # Re-code 3 as 50 for say in gov't variable, unsure
  anes_2020_clean$say_in_govt > 3 ~ 100, # Re-code values greater than 3 as 100 for say in gov't variable, have a say
  TRUE ~ NA_real_ # Otherwise, assign NA
)

anes_2020_clean$gender <- case_when(
  anes_2020_clean$female == 1 ~ 0, # Re-code 1 as 0, male
  anes_2020_clean$female == 2 ~ 1, # Re-code 2 as 1, female
  TRUE ~ NA_real_ # Otherwise, assign NA
)

anes_2020_clean$attend_political_meetings <- case_when(
  anes_2020_clean$attend_political_meetings == 1 ~ 1, # Re-code 1 as 1, attended political meetings
  anes_2020_clean$attend_political_meetings == 2 ~ 0, # Re-code 2 as 0, did not attend political meetings
  TRUE ~ NA_real_ # Otherwise, assign NA
)

anes_2020_clean$work_for_campaigns <- case_when(
  anes_2020_clean$work_for_campaigns == 1 ~ 1, # Re-code 1 as 1, worked for political campaigns
  anes_2020_clean$work_for_campaigns == 2 ~ 0, # Re-code 2 as 0, did not work for political campaigns
  TRUE ~ NA_real_ # Otherwise, assign NA
)

anes_2020_clean$donate_money <- case_when(
  anes_2020_clean$donate_money == 1 ~ 1, # Re-code 1 as 1, donated money to political campaigns
  anes_2020_clean$donate_money == 2 ~ 0, # Re-code 2 as 0, did not donate money to political campaigns
  TRUE ~ NA_real_ # Otherwise, assign NA
)

anes_2020_clean$liberal_conservative_scale <- ifelse(
  anes_2020_clean$liberal_conservative_scale > 0 & anes_2020_clean$liberal_conservative_scale <= 7,
  anes_2020_clean$liberal_conservative_scale, # If it is scalable
  NA # Otherwise, assign NA
)

anes_2020_clean$federal_welfare_spending <- case_when(
  anes_2020_clean$federal_welfare_spending == 1 ~ 100, # Re-code 1 as 100, should increase
  anes_2020_clean$federal_welfare_spending == 2 ~ 0, # Re-code 2 as 0, should decrease
  anes_2020_clean$federal_welfare_spending == 3 ~ 50, # Re-code 3 as 50, should stay the same
  TRUE ~ NA_real_ # Otherwise, assign NA
)


#####
## Loading ANES data for 2024
anes_2024 <- read_csv("data/anes_timeseries_2024_csv_20250808.csv")
anes_2024$year <- 2024

# Disability status = V241574a
anes_2024$disabled <- case_when(
  anes_2024$V241574a == 1 ~ 1, # If whole disability status is 1, assign 1
  TRUE ~ 0 # Otherwise, assign 0
)

# Cleaned ANES 2024 data
anes_2024_clean <- anes_2024 |>
  rename(
    age = V241458x,
    state = V243002,
    voted = V242095x,
    ed_level = V241465x,
    income_level = V241566x,
    govt_cares = V242200,
    say_in_govt = V242201,
    female = V241550,
    census_reg = V243007,
    attend_political_meetings = V242012,
    work_for_campaigns = V242014,
    donate_money = V242013,
    liberal_conservative_scale = V241177,
    federal_welfare_spending = V241273
  ) |>
  select(c(year, age, state, voted, ed_level, income_level,
           govt_cares, say_in_govt, female, census_reg,
           attend_political_meetings, work_for_campaigns, donate_money,
           liberal_conservative_scale, federal_welfare_spending, disabled))

# Clean ANES 2024 data
anes_2024_clean$age <- ifelse(
  anes_2024_clean$age == -2, NA, # Re-code NA if nonvalid age
  anes_2024_clean$age
)

anes_2024_clean$voted <- case_when(
  anes_2024_clean$voted == 0 ~ 0, # If respondent did not vote, assign 0
  anes_2024_clean$voted == 1 ~ 1, # If respondent voted, assign 1
  TRUE ~ NA_real_ # Otherwise, assign NA
)

anes_2024_clean$ed_level <- case_when(
  anes_2024_clean$ed_level > 0 & anes_2024_clean$ed_level < 6 ~ anes_2024_clean$ed_level, # If respondent has valid education level, keep value
  TRUE ~ NA_real_ # Otherwise, assign NA
)

anes_2024_clean$income_level <- case_when(
  anes_2024_clean$income_level > 0 ~ anes_2024_clean$income_level, # If respondent has valid income level, keep value
  TRUE ~ NA_real_ # Otherwise, assign NA
)

anes_2024_clean$govt_cares <- case_when(
  anes_2024_clean$govt_cares > 0 & anes_2024_clean$govt_cares <= 2 ~ 100, # Re-code 1 and 2 as 100 for government cares variable
  anes_2024_clean$govt_cares == 3 ~ 50, # Re-code 3 as 50 for government cares variable
  anes_2024_clean$govt_cares > 3 ~ 0, # Re-code values greater than 3
  TRUE ~ NA_real_ # Otherwise, assign NA
)

anes_2024_clean$say_in_govt <- case_when(
  anes_2024_clean$say_in_govt > 0 & anes_2024_clean$say_in_govt <= 2 ~ 0, # Re-code 1 and 2 as 0 for say in gov't variable, no say
  anes_2024_clean$say_in_govt == 3 ~ 100, # Re-code 3 as 100 for say in gov't variable, unsure
  anes_2024_clean$say_in_govt > 3 ~ 100, # Re-code values greater than 3 as 100 for say in gov't variable, have a say
  TRUE ~ NA_real_ # Otherwise, assign NA
)

anes_2024_clean$female <- case_when(
  anes_2024_clean$female == 1 ~ 0, # Re-code, equal to man
  anes_2024_clean$female == 2 ~ 1, # Re-code, equal to woman
  TRUE ~ NA_real_ # Otherwise, assign NA; make a statement in study design nonbinary/other gender catergories were available but data only for 2024
)

anes_2024_clean$attend_political_meetings <- case_when(
  anes_2024_clean$attend_political_meetings == 1 ~ 1, # Re-code 1 as 1, attended political meetings
  anes_2024_clean$attend_political_meetings == 2 ~ 0, # Re-code 2 as 0, did not attend political meetings
  TRUE ~ NA_real_ # Otherwise, assign NA
)

anes_2024_clean$work_for_campaigns <- case_when(
  anes_2024_clean$work_for_campaigns == 1 ~ 1, # Re-code 1 as 1, worked for political campaigns
  anes_2024_clean$work_for_campaigns == 2 ~ 0, # Re-code 2 as 0, did not work for political campaigns
  TRUE ~ NA_real_ # Otherwise, assign NA
)

anes_2024_clean$donate_money <- case_when(
  anes_2024_clean$donate_money %in% c(1, 2) ~ 1, # Re-code 1 and 2 as 1, donated money to political campaigns
  anes_2024_clean$donate_money == 3 ~ 0, # Re-code 3 as 0, did not donate money to political campaigns
  TRUE ~ NA_real_ # Otherwise, assign NA
)

anes_2024_clean$liberal_conservative_scale <- ifelse(
  anes_2024_clean$liberal_conservative_scale > 0 & anes_2024_clean$liberal_conservative_scale <= 7,
  anes_2024_clean$liberal_conservative_scale, # If it is scalable
  NA # Otherwise, assign NA
)

anes_2024_clean$federal_welfare_spending <- case_when(
  anes_2024_clean$federal_welfare_spending == 1 ~ 100, # Re-code 1 as 100, should increase
  anes_2024_clean$federal_welfare_spending == 2 ~ 0, # Re-code 2 as 0, should decrease
  anes_2024_clean$federal_welfare_spending == 3 ~ 50, # Re-code 3 as 50, should stay the same
  TRUE ~ NA_real_ # Otherwise, assign NA
)

anes_2024_clean$state <- as.numeric(anes_2024_clean$state) # Re-code state variable as numeric

#####
all_anes <- bind_rows(anes_data_clean, anes_2020_clean, anes_2024_clean) # Combining all ANES data frames

## Creating region dummy variable

all_anes$northeast <- ifelse(
  all_anes$census_reg == 1, 1, 0)

all_anes$central <- ifelse(
  all_anes$census_reg == 2, 1, 0)

all_anes$south <- ifelse(
  all_anes$census_reg == 3, 1, 0)

all_anes$west <- ifelse(
  all_anes$census_reg == 4, 1, 0)

all_anes$external_efficacy_index <- rowMeans(all_anes[, c("govt_cares", "say_in_govt")], na.rm = TRUE)
# How ANES measures external efficacy

all_anes$other_civic_engagement <- rowSums(all_anes[, c("attend_political_meetings", "work_for_campaigns", "donate_money")], na.rm = TRUE)
# Looks at how many other forms of civic engagement respondent participates in, aside from voting; range from 0-3

all_anes$state <- case_when(
  all_anes$state == 1 ~ "AL",
  all_anes$state == 2 ~ "AK",
  all_anes$state == 4 ~ "AZ",
  all_anes$state == 5 ~ "AR",
  all_anes$state == 6 ~ "CA",
  all_anes$state == 8 ~ "CO",
  all_anes$state == 9 ~ "CT",
  all_anes$state == 10 ~ "DE",
  all_anes$state == 11 ~ "DC",
  all_anes$state == 12 ~ "FL",
  all_anes$state == 13 ~ "GA",
  all_anes$state == 15 ~ "HI",
  all_anes$state == 16 ~ "ID",
  all_anes$state == 17 ~ "IL",
  all_anes$state == 18 ~ "IN",
  all_anes$state == 19 ~ "IA",
  all_anes$state == 20 ~ "KS",
  all_anes$state == 21 ~ "KY",
  all_anes$state == 22 ~ "LA",
  all_anes$state == 23 ~ "ME",
  all_anes$state == 24 ~ "MD",
  all_anes$state == 25 ~ "MA",
  all_anes$state == 26 ~ "MI",
  all_anes$state == 27 ~ "MN",
  all_anes$state == 28 ~ "MS",
  all_anes$state == 29 ~ "MO",
  all_anes$state == 30 ~ "MT",
  all_anes$state == 31 ~ "NE",
  all_anes$state == 32 ~ "NV",
  all_anes$state == 33 ~ "NH",
  all_anes$state == 34 ~ "NJ",
  all_anes$state == 35 ~ "NM",
  all_anes$state == 36 ~ "NY",
  all_anes$state == 37 ~ "NC",
  all_anes$state == 38 ~ "ND",
  all_anes$state == 39 ~ "OH",
  all_anes$state == 40 ~ "OK",
  all_anes$state == 41 ~ "OR",
  all_anes$state == 42 ~ "PA",
  all_anes$state == 44 ~ "RI",
  all_anes$state == 45 ~ "SC",
  all_anes$state == 46 ~ "SD",
  all_anes$state == 47 ~ "TN",
  all_anes$state == 48 ~ "TX",
  all_anes$state == 49 ~ "UT",
  all_anes$state == 50 ~ "VT",
  all_anes$state == 51 ~ "VA",
  all_anes$state == 53 ~ "WA",
  all_anes$state == 54 ~ "WV",
  all_anes$state == 55 ~ "WI",
  all_anes$state == 56 ~ "WY",
)
  

grants_data <- read_xlsx("data/Taggs PAVA data.xlsx",
                         sheet = "Issued Year")

grants_data_summary <- grants_data |>
  group_by(`Funding FY`, State) |>
  summarize(total_grant = sum(`Action Amount`), 
            .groups = "drop")

joined_data <- all_anes |>
  left_join(grants_data_summary, by = c(year = "Funding FY", state = "State")) 

# Note: only years past 2008 have data regarding grants, so only a subset of the ANES data will be merged with the grants data

joined_data$after_hava <- ifelse(joined_data$year > 2002, 1, 0) # Create dummy variable for pre-HAVA years

write_csv(joined_data, "data/ANES_grants_data.csv")
