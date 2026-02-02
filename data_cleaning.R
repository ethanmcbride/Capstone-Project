library(readxl)
library(tidyverse)

# Load the dataset
anes_data <- read_csv("data/anes_timeseries_cdf_csv_20251211.csv")

# View structure of the dataset
str(anes_data)

# View col_character() columns
anes_data |> 
  select(where(is.character)) |>
  glimpse()

disability_data <- anes_data |>
  filter(VCF0150 == 16 | VCF0150 == 60 | VCF0150 == 61)
  # 16 = Permanently disabled & 20 hours or more per week
  # 60 = Permanently disabled & no occupation
  # 61 = Permanently disabled & less than 20 hours per week

anes_data_with_disability_status <- anes_data |>
  mutate(disability_status = case_when(
    VCF0150 == 16 | VCF0150 == 60 | VCF0150 == 61 ~ 1,
    TRUE ~ 0
  ))

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
         VCF0901b, # State postal abbreviation
         VCF9013, # Society ensures equal opportunity for success
         VCF9131, # Less vs more gov't
         VCF9251, # How well does individual understand politics
         VCF9259, # How often follow political news
         VCF9250 # Belief vote makes a difference
         )

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
install.packages("ipumsr")
library(ipumsr)

ddi <- read_ipums_ddi("data/cps_00001.xml")
data <- read_ipums_micro(ddi)

library(tidyverse)

data |>
  group_by(YEAR) |>
  summarize(count = n())

data |>
  filter(YEAR == 2020) |>
  group_by(DISABWRK, VOWHYNOT) |>
  summarize(count = n())
  
