library(readr)
library(tidyverse)

# -------------------- READ IN DATA ------------------------------
VotingData <- read_csv("https://raw.githubusercontent.com/khanhdo05/sta-310-final/main/data/raw/38506-0001-Data.csv", show_col_types = FALSE)
IncomeData <- read_csv("https://raw.githubusercontent.com/khanhdo05/sta-310-final/main/data/raw/20incyallnoagi.csv", show_col_types = FALSE)
UnemploymentData <- read_csv("https://raw.githubusercontent.com/khanhdo05/sta-310-final/main/data/raw/Unemployment2023.csv", show_col_types = FALSE) # this file has data from 2000 - 2023
EducationData <- read_csv("https://raw.githubusercontent.com/khanhdo05/sta-310-final/main/data/raw/Education2023.csv", show_col_types = FALSE) # this file has data from 1970 - 2023
PopulationData <- read_csv("https://raw.githubusercontent.com/khanhdo05/sta-310-final/main/data/raw/PopulationEstimates.csv", show_col_types = FALSE) # this file has data from 2020 - 2023

# ------------------- CLEAN VOTING DATA --------------------------
VotingData2020Clean <- VotingData %>%
  # only interested in the presidential selection in 2020
  filter(YEAR == 2020) %>%
  
  # remove leading zeros
  mutate(STCOFIPS10 = as.character(as.numeric(STCOFIPS10))) %>%
  
  # rename col
  rename(FIPS = STCOFIPS10) %>%
  
  # drop year and not interested in senate data
  select(-SEN_DEM_VOTES, -SEN_REP_VOTES, -SEN_DEM_RATIO, -SEN_REP_RATIO, -YEAR)

# ------------------- CLEAN INCOME DATA --------------------------
IncomeData201920Clean <- IncomeData %>%
  # remove state-level, keep only county-level
  filter(COUNTYFIPS != "000") %>%
  
  # create 4-5 characters fips code
  mutate(FIPS = as.character(
    as.numeric(STATEFIPS) * 1000 + as.numeric(COUNTYFIPS)
  )) %>%
  
  # select interested variables
  select(
    FIPS, COUNTYNAME, # County code and county name
    
    # ------------- Core Economic Variables -------------
    N1,       # total number of returns (proxy for tax-filing population size)
    A00100,   # total AGI (overall income level)
    A00200,   # salaries and wages (working income)
    A00900,   # business/self-employment income
    A01000,   # capital gains (wealth indicator)
    A01700,   # pensions and annuities (retiree income)
    A02300,   # unemployment compensation (economic distress)
    A02500,   # Social Security benefits (elderly population)
    
    # ------------- Inequality & Wealth Indicators -------------
    A26270,   # partnership/S-corp income (high-income/business owners)
    A19300,   # mortgage interest paid (homeownership)
    A19700,   # charitable contributions (affluence proxy)
    
    # ---------------------- Tax Burden ------------------------
    A10300,   # total tax liability
    A59660,   # earned income credit amount (low-income households)
    N59660,   # number of EIC returns (% low-income families)
    A11070,   # child tax credit (families with children)
    )

# --------------- CLEAN UNEMPLOYMENT DATA ----------------------
UnemploymentData2020Clean <- UnemploymentData %>%
  # remove state-level rows (FIPS ends in 000)
  filter(!grepl("000$", as.character(FIPS_Code))) %>%
  # remove the first state-level
  filter(FIPS_Code != "0") %>%
  
  # keep only 2020 attributes
  filter(grepl("_2020$", Attribute)) %>%
  
  # pivot wide so each attribute becomes a column
  pivot_wider(
    id_cols = c(FIPS_Code, State, Area_Name),
    names_from = Attribute,
    values_from = Value
  ) %>%
  
  # rename cols
  rename(FIPS = FIPS_Code) %>%
  
  # keep interested variables
  select(FIPS, Unemployment_rate_2020)

# ---------------- CLEAN EDUCATION DATA -----------------------
EducationData201923Clean <- EducationData %>%
  # remove state-level rows (FIPS ends in 000)
  filter(!grepl("000$", as.character(`FIPS Code`))) %>%
  # remove the first state-level
  filter(`FIPS Code` != "0") %>%
  
  # keep only 2020 attributes
  filter(grepl("Percent.*2019-23", Attribute)) %>%
  
  # pivot wide so each attribute becomes a column
  pivot_wider(
    id_cols = c(`FIPS Code`, State, `Area name`),
    names_from = Attribute,
    values_from = Value
  ) %>%
  
  # rename cols
  rename(FIPS = `FIPS Code`) %>%
  rename(
    PCT_NO_HS    = `Percent of adults who are not high school graduates, 2019-23`,
    PCT_HS_ONLY  = `Percent of adults who are high school graduates (or equivalent), 2019-23`,
    PCT_SOME_COL = `Percent of adults completing some college or associate degree, 2019-23`,
    PCT_BACH     = `Percent of adults with a bachelor's degree or higher, 2019-23`
  ) %>%
  
  # drop cols
  select(-State, -`Area name`)

# ---------------- CLEAN POPULATION DATA -----------------------
PopulationData2020Clean <- PopulationData %>%
  # remove state-level rows (FIPS ends in 000)
  filter(!grepl("000$", as.character(FIPStxt))) %>%
  # remove the first state-level
  filter(FIPStxt != "0") %>%
  
  # keep only 2020 attributes
  filter(Attribute %in% c("CENSUS_2020_POP", "NET_MIG_2020",
                          "BIRTHS_2020", "DEATHS_2020")) %>%
  
  # pivot wide so each attribute becomes a column
  pivot_wider(
    id_cols = c(FIPStxt, State, Area_Name),
    names_from = Attribute,
    values_from = Value
  ) %>%
  
  # rename cols
  rename(FIPS = FIPStxt) %>%
  
  # keep interested variables
  select(-State, -Area_Name) %>%
  mutate(
    # we're interested in rates, not raw counts
    R_BIRTH_2020 = BIRTHS_2020 / CENSUS_2020_POP * 1000,
    R_DEATH_2020 = DEATHS_2020 / CENSUS_2020_POP * 1000
  ) %>%
  select(-BIRTHS_2020, -DEATHS_2020)

# -------------------- WRITE TO TABLE --------------------------
write_csv(VotingData2020Clean, "./data/clean/VotingData2020.csv")
write_csv(IncomeData201920Clean, "./data/clean/IncomeData2020.csv")
write_csv(UnemploymentData2020Clean, "./data/clean/UnemploymentRate2020.csv")
write_csv(EducationData201923Clean, "./data/clean/EducationData201923.csv")
write_csv(EducationData201923Clean, "./data/clean/EducationData201923.csv")
