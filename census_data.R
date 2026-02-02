library(httr)
library(jsonlite)
library(readxl)
library(tidyverse)

api_key <- "0f4aeb3b180c3fccd5c6057da2a0db4e288b6d46"

census_api_key(api_key, install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

### 2024 Data from Census Voting API ###
link_2024 <- "https://api.census.gov/data/2024/cps/voting/nov?get=PEDISDRS,PEDISEAR,PEDISEYE,PEDISOUT,PEDISPHY,PEDISREM,PES1,PES2,PES3,PES4,PES5&for=state:*&key=0f4aeb3b180c3fccd5c6057da2a0db4e288b6d46"
data_2024 <- fromJSON(link_2024)

data_df_2024 <- as.data.frame(data_2024[-1, ])

colnames(data_df_2024) <- as.character(data_df_2024[1, ]) # Setting column names, scrapping set first row to column name

data_df_2024 <- sapply(data_df_2024, as.numeric) # Making all variables numeric
data_df_2024 <- as.data.frame(data_df)

data_df_2024$year <- 2024

### 2022 Data from Census Voting API ###
link_2022 <- "https://api.census.gov/data/2022/cps/voting/nov?get=PEDISDRS,PEDISEAR,PEDISEYE,PEDISOUT,PEDISPHY,PEDISREM,PES1,PES2,PES3,PES4,PES5&for=state:*&key=0f4aeb3b180c3fccd5c6057da2a0db4e288b6d46"
data_2022 <- fromJSON(link_2022)

data_df_2022 <- as.data.frame(data_2022[-1, ])

colnames(data_df_2022) <- as.character(data_df_2022[1, ]) # Setting column names, scrapping set first row to column name

data_df_2022 <- sapply(data_df_2022, as.numeric) # Making all variables numeric
data_df_2022 <- as.data.frame(data_df_2022)

data_df_2022$year <- 2022

### 2020 Data from Census Voting API ###
link_2020 <- "https://api.census.gov/data/2020/cps/voting/nov?get=PEDISDRS,PEDISEAR,PEDISEYE,PEDISOUT,PEDISPHY,PEDISREM,PES1,PES2,PES3,PES4,PES5&for=state:*&key=0f4aeb3b180c3fccd5c6057da2a0db4e288b6d46"
data_2020 <- fromJSON(link_2020)

data_df_2020 <- as.data.frame(data_2020[-1, ])

colnames(data_df_2020) <- as.character(data_df_2020[1, ]) # Setting column names, scrapping set first row to column name

data_df_2020 <- sapply(data_df_2020, as.numeric) # Making all variables numeric
data_df_2020 <- as.data.frame(data_df_2020)

data_df_2020$year <- 2020

### 2018 Data from Census Voting API ###
link_2018 <- "https://api.census.gov/data/2018/cps/voting/nov?get=PEDISDRS,PEDISEAR,PEDISEYE,PEDISOUT,PEDISPHY,PEDISREM,PES1,PES2,PES3,PES4,PES5&for=state:*&key=0f4aeb3b180c3fccd5c6057da2a0db4e288b6d46"
data_2018 <- fromJSON(link_2018)

data_df_2018 <- as.data.frame(data_2018[-1, ])

colnames(data_df_2018) <- as.character(data_df_2018[1, ]) # Setting column names, scrapping set first row to column name

data_df_2018 <- sapply(data_df_2018, as.numeric) # Making all variables numeric
data_df_2018 <- as.data.frame(data_df_2018)

data_df_2018$year <- 2018

### 2016 Data from Census Voting API ###
link_2016 <- "https://api.census.gov/data/2016/cps/voting/nov?get=PEDISDRS,PEDISEAR,PEDISEYE,PEDISOUT,PEDISPHY,PEDISREM,PES1,PES2,PES3,PES4,PES5&for=state:*&key=0f4aeb3b180c3fccd5c6057da2a0db4e288b6d46"
data_2016 <- fromJSON(link_2016)

data_df_2016 <- as.data.frame(data_2016[-1, ])

colnames(data_df_2016) <- as.character(data_df_2016[1, ]) # Setting column names, scrapping set first row to column name

data_df_2016 <- sapply(data_df_2016, as.numeric) # Making all variables numeric
data_df_2016 <- as.data.frame(data_df_2016)

data_df_2016$year <- 2016

## 2014 Data from Census Voting API ###
link_2014 <- "https://api.census.gov/data/2014/cps/voting/nov?get=PEDISDRS,PEDISEAR,PEDISEYE,PEDISOUT,PEDISPHY,PEDISREM,PES1,PES2,PES3,PES4,PES5&for=state:*&key=0f4aeb3b180c3fccd5c6057da2a0db4e288b6d46"
data_2014 <- fromJSON(link_2014)

data_df_2014 <- as.data.frame(data_2014[-1, ])

colnames(data_df_2014) <- as.character(data_df_2014[1, ]) # Setting column names, scrapping set first row to column name

data_df_2014 <- sapply(data_df_2014, as.numeric) # Making all variables numeric
data_df_2014 <- as.data.frame(data_df_2014)

data_df_2014$year <- 2014

## 2012 Data from Census Voting API ###
link_2012 <- "https://api.census.gov/data/2012/cps/voting/nov?get=PEDISDRS,PEDISEAR,PEDISEYE,PEDISOUT,PEDISPHY,PEDISREM,PES1,PES2,PES3,PES4,PES5&for=state:*&key=0f4aeb3b180c3fccd5c6057da2a0db4e288b6d46"
data_2012 <- fromJSON(link_2012)

data_df_2012 <- as.data.frame(data_2012[-1, ])

colnames(data_df_2012) <- as.character(data_df_2012[1, ]) # Setting column names, scrapping set first row to column name

data_df_2012 <- sapply(data_df_2012, as.numeric) # Making all variables numeric
data_df_2012 <- as.data.frame(data_df_2012)

data_df_2012$year <- 2012

### 2010 Data from Census Voting API ###
link_2010 <- "https://api.census.gov/data/2010/cps/voting/nov?get=PEDISDRS,PEDISEAR,PEDISEYE,PEDISOUT,PEDISPHY,PEDISREM,PES1,PES2,PES3,PES4,PES5&for=state:*&key=0f4aeb3b180c3fccd5c6057da2a0db4e288b6d46"
data_2010 <- fromJSON(link_2010)

data_df_2010 <- as.data.frame(data_2010[-1, ])

colnames(data_df_2010) <- as.character(data_df_2010[1, ]) # Setting column names, scrapping set first row to column name

data_df_2010 <- sapply(data_df_2010, as.numeric) # Making all variables numeric
data_df_2010 <- as.data.frame(data_df_2010)

data_df_2010$year <- 2010

# ### 2008 Data from Census Voting API ###
# link_2008 <- "https://api.census.gov/data/2008/cps/voting/nov?get=PEDISDRS,PEDISEAR,PEDISEYE,PEDISOUT,PEDISPHY,PEDISREM,PES1,PES2,PES3,PES4,PES5&for=state:*&key=0f4aeb3b180c3fccd5c6057da2a0db4e288b6d46"
# data_2008 <- fromJSON(link_2008)
# 
# data_df_2008 <- as.data.frame(data_2008[-1, ])
# 
# colnames(data_df_2008) <- as.character(data_df_2008[1, ]) # Setting column names, scrapping set first row to column name
# 
# data_df_2008 <- sapply(data_df_2008, as.numeric) # Making all variables numeric
# data_df_2008 <- as.data.frame(data_df_2008)
# 
# data_df_2008$year <- 2008

### Combining all years into one dataframe ###
voting_data <- bind_rows(data_df_2024, 
                         data_df_2022,
                         data_df_2020,
                         data_df_2018,
                         data_df_2016,
                         data_df_2014,
                         data_df_2012,
                         data_df_2010 )
                         # data_df_2008)

voting_data <- voting_data[,-(14:38)]

write_csv(voting_data, "data/census_voting_data_2010_2024.csv")

