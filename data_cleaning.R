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
  filter(year >= 1980 & year < 2020)

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

anes_data$attend_political_meetings <- case_when(
  anes_data_clean$attend_political_meetings == 1 ~ 0, # If respondent did not attend a political meetings, assign 0
  anes_data_clean$attend_political_meetings == 2 ~ 1, # If respondent attended a political meeting, assign 1
  TRUE ~ NA_real_ # Otherwise, assign NA
)

anes_data$work_for_campaigns <- case_when(
  anes_data_clean$work_for_campaigns == 1 ~ 0, # If respondent did not work for a political campaign, assign 0
  anes_data_clean$work_for_campaigns == 2 ~ 1, # If respondent worked for a political campaign, assign 1
  TRUE ~ NA_real_ # Otherwise, assign NA
)

anes_data$donate_money <- case_when(
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
# Prelim analysis based on variables for entire data set
### VCF0648, external efficacy index
anes_data |>
  group_by(VCF0004) |>
  filter(VCF0648 != 999) |> ## Filters out missing values for external efficacy index
  summarize(mean_external_efficacy = mean(VCF0648, na.rm = TRUE)) |>
  print(n = Inf)

### VCF0703, 1 == not registered, 2 == registered but did not vote, 3 == voted
anes_data |>
  group_by(VCF0703, VCF0004) |> ## Groups by year of survey and voter registration/turnout status
  filter(VCF0004 >= 1980 & VCF0004 <= 2024) |> ## Filters for years 1980-2024
  summarize(count = n()) |>
  print(n = Inf) 

anes_data |>
  group_by(VCF0004) |>
  filter(VCF0150 == 16 | VCF0150 == 60 | VCF0150 == 61) |> ## Permanent disability
  summarize(count = n()) ## Shows collection was not in for 2020 & 2024


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
    gender = V241550,
    census_reg = V243007,
    attend_political_meetings = V242012,
    work_for_campaigns = V242014,
    donate_money = V242013,
    liberal_conservative_scale = V241177,
    federal_welfare_spending = V241273
  ) |>
  select(c(year, age, state, voted, ed_level, income_level,
           govt_cares, say_in_govt, gender, census_reg,
           attend_political_meetings, work_for_campaigns, donate_money,
           liberal_conservative_scale, federal_welfare_spending, disabled))

# Clean ANES 2024 data


#####
# Check disabled variable
anes_2024 |>
  group_by(disabled) |>
  summarize(count = n())

# Create disabled variable for time series
anes_data$disabled = case_when(
  anes_data$VCF0150 == 16 | anes_data$VCF0150 == 60 | anes_data$VCF0150 == 61 ~ 1, 
  # If individual is permanently disabled, regardless of hours worked, assign 1
  TRUE ~ 0 # Otherwise, assign 0
)

# Check disabled variable for time series
anes_data |>
  group_by(disabled) |>
  summarize(count = n())

## Selecting variables

# Age variable for time series = VCF0101
anes_data |>
  group_by(VCF0004) |>
  filter(VCF0101 != 00) |>
  filter(VCF0004 >= 1980 & VCF0004 <= 2016) |>
  summarize(mean_age = mean(VCF0101, na.rm = TRUE))

# Age variable for 2020 = V201507x
anes_2020 |>
  filter(V201507x != -9) |>
  summarize(mean_age = mean(V201507x, na.rm = TRUE))

# Age variable for 2024 = V241458x
anes_2024 |>
  filter(V241458x != -2) |>
  summarize(mean_age = mean(V241458x, na.rm = TRUE))


# State variable for time series = VCF0901a
anes_data |>
  group_by(VCF0901a) |>
  filter(VCF0901a != 99 & VCF0901a != 00) |>
  summarize(count = n())

### Come back to this, should I use 2020? 
### Does not include 728 participants because they were not registered to vote?
anes_2020 |>
  group_by(V201014b) |>
  summarize(count = n()) |>
  print(n = Inf)

### Come back to this, should I use 2024?
### Does not include 634 participants because they were not registered to vote?
anes_2024 |>
  group_by(V241023) |>
  summarize(count = n()) |>
  print(n = Inf)

# Voting status for time series = VCF0702
# 0, 1 == did not vote, 2 == voted
anes_data |>
  group_by(VCF0702, VCF0004) |>
  filter(VCF0004 >= 1980 & VCF0004 <= 2024) |>
  summarize(count = n()) |>
  print(n = Inf)

# Voting status for 2020 = V202068x


disability_data <- anes_data |>
  filter(VCF0150 == 16 | VCF0150 == 60 | VCF0150 == 61)
  # 16 = Permanently disabled & 20 hours or more per week
  # 60 = Permanently disabled & no occupation
  # 61 = Permanently disabled & less than 20 hours per week

disability_data |>
  group_by(VCF0004) |>
  summarize(count = n())


anes_data_with_disability_status <- anes_data |>
  mutate(disability_status = case_when(
    VCF0150 == 16 | VCF0150 == 60 | VCF0150 == 61 ~ 1, 
    # If individual is permanently disabled, regardless of hours worked, assign 1
    TRUE ~ 0
  ))

### Think if it would be helpful to split disability status
### Into different variables to observe

# Relevant columns
disability_data_w_columns <- anes_data_with_disability_status |>
  select(VCF0004, # Year of study
         disability_status,
         VCF0102, # Age group
         VCF0613, # Belief people like respondent have voice in gov't
         VCF0702, # Did respondent vote
         VCF0717, # Try to influence others' vote
         VCF0718, # Attend political meetings
         VCF0719, # Work for political campaigns
         VCF0720, # Display sticker or button in support of candidate
         VCF0721, # Donate money to party
         VCF0722, # Contact public official 
         VCF0901b, # State postal abbreviation
         VCF9013, # Society ensures equal opportunity for success
         VCF9131, # Less vs more gov't
         VCF9251, # How well does individual understand politics
         VCF9259, # How often follow political news
         VCF9250 # Belief vote makes a difference
         )

ANES <- disability_data_w_columns |>
  mutate(civic_engagement = rowSums(across(c(VCF0702, VCF0718, VCF0719, VCF0720, VCF0721, VCF0722)) == 2, 
                                    na.rm = TRUE))

test_data <- ANES |>
  filter(VCF0004 >= 2000 & VCF0004 <= 2024)

year_aov <- aov(civic_engagement ~ factor(VCF0004), data = test_data)
TukeyHSD(year_aov)

test_w_disability <- test_data |>
  filter(disability_status == 1)

year.lm <- lm(civic_engagement ~ factor(VCF0004), data = test_w_disability)
summary(year.lm)

disability.lm <- lm(civic_engagement ~ factor(VCF0004) * disability_status, data = test_data)
summary(disability.lm)

### TIME SERIES DATA DOES NOT HAVE EMPLOYMENT FACTOR FOR 2020 OR 2024 ###

# Load 2024 data series

anes_2024 <- read_csv("data/anes_timeseries_2024_csv_20250808.csv")
anes_2024$VCF004 <- 2024

anes_2024_select_variables <- anes_2024 |>
  select(V241003, # How often do you may attention to politics?
         V241005, # How interested in following campaigns?
         V241012, # Registered to vote?
         V241017, # State of residence
         V241038, # Voted in presidential election
         V241100, # Likelihood of voting
         V241239, # Government services thermometer
         V241245, # Medical insurance thermometer
         V241263x, # Social Security thermometer
         V241275x, # Welfare programs thermometer
         V241316,  # How often denied right to vote
         V241319x, # Voter ID thermometer
         V241566x, # Total household income
         V241574a, # Disability status, whole
         V241574b, # Disability status, mental
         V241574c, # Disability status, learning disability/ADHD
         V241574d, # Disability status, autism spectrum disorder
         V241574e, # Disability status, blind/visual impairment
         V241574f, # Disability status, deaf/hard of hearing
         V241574g, # Disability status, mobility-related disability
         V241574h, # Disability status, speech-related disability
         V241574i, # Disability status, other
         V241751a, # Talked for/against a political party/candidate
         V241751b, # Went to a meeting, rally, or speech (political)
         V241751c, # Wore campaign button / sticker
         V241751d, # Worked for a candidate or political party
         V241751e,  # Donated money to a candidate or political party
         V241751f, # Got into a political argument
         V241751g, # Signed or petitioned for a political/social issues
         V241751h, # Attended public meeting to discuss problem(s)
         V241751j # Posted comments on social media about political/social issues
         )

age <- anes_2024 |>
  select(V241458x)

library(readxl)  
grants_data <- read_xlsx("data/Taggs PAVA data.xlsx",
                         sheet = "Issued Year")

grants_data_summary <- grants_data |>
  group_by(`Funding FY`, State) |>
  summarize(total_grant = sum(`Action Amount`), 
            .groups = "drop")

joined_data <- anes_data_with_disability_status |>
  left_join(grants_data_summary, by = c(VCF0004 = "Funding FY", VCF0901b = "State"))
