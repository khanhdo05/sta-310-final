library(readr)
library(tidyverse)
library(tidycensus)

# -------------------- READ IN DATA ------------------------------
VotingData <- read_csv("https://raw.githubusercontent.com/khanhdo05/sta-310-final/main/data/raw/38506-0001-Data.csv", show_col_types = FALSE)
IncomeData <- read_csv("https://raw.githubusercontent.com/khanhdo05/sta-310-final/main/data/raw/20incyallnoagi.csv", show_col_types = FALSE)
UnemploymentData <- read_csv("https://raw.githubusercontent.com/khanhdo05/sta-310-final/main/data/raw/Unemployment2023.csv", show_col_types = FALSE) # this file has data from 2000 - 2023
EducationData <- read_csv("https://raw.githubusercontent.com/khanhdo05/sta-310-final/main/data/raw/Education2023.csv", show_col_types = FALSE) # this file has data from 1970 - 2023
PopulationData <- read_csv("https://raw.githubusercontent.com/khanhdo05/sta-310-final/main/data/raw/PopulationEstimates.csv", show_col_types = FALSE) # this file has data from 2020 - 2023
census_api_key("902576123c865062163b7d7203d1aa2c49a0c58d")

# ------------------- CLEAN VOTING DATA --------------------------
VotingData2020Clean <- VotingData %>%
  # only interested in the presidential selection in 2020
  filter(YEAR == 2020) %>%
  
  # remove leading zeros
  mutate(STCOFIPS10 = as.character(as.numeric(STCOFIPS10))) %>%
  
  # rename col
  rename(FIPS = STCOFIPS10) %>%
  
  # drop year, raw counts and not interested in senate data
  select(FIPS, PRES_DEM_RATIO)

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

# ----------------- CLEAN CENSUS DATA ------------------------
# 2020 decennial census - race/ethnicity and sex at county level
DemographicData2020Clean <- get_decennial(
  geography = "county",
  variables = c(
    total_pop = "P1_001N",
    white     = "P1_003N",
    black     = "P1_004N",
    asian     = "P1_006N",
    hispanic  = "P2_002N",
    male      = "P3_002N"    # no trailing comma here
  ),
  year = 2020,
  sumfile = "pl"
) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(
    PctWhite    = white / total_pop * 100,
    PctBlack    = black / total_pop * 100,
    PctAsian    = asian / total_pop * 100,
    PctHispanic = hispanic / total_pop * 100,
    PctMale     = male / total_pop * 100
  ) %>%
  mutate(FIPS = as.character(as.numeric(GEOID))) %>%
  select(FIPS, PctWhite, PctBlack, PctAsian, PctHispanic, PctMale)

# -------------------- WRITE TO TABLE --------------------------
write_csv(VotingData2020Clean, "./data/clean/VotingData2020.csv")
write_csv(IncomeData201920Clean, "./data/clean/IncomeData2020.csv")
write_csv(UnemploymentData2020Clean, "./data/clean/UnemploymentRate2020.csv")
write_csv(EducationData201923Clean, "./data/clean/EducationData201923.csv")
write_csv(PopulationData2020Clean, "./data/clean/PopulationData2020.csv")
write_csv(DemographicData2020Clean, "./data/clean/DemographicData2020.csv")

# ------------------- JOIN ALL TABLES --------------------------
FinalData <- VotingData2020Clean %>%
  mutate(FIPS = as.character(FIPS)) %>%
  left_join(IncomeData201920Clean %>% mutate(FIPS = as.character(FIPS)), by = "FIPS") %>%
  left_join(UnemploymentData2020Clean %>% mutate(FIPS = as.character(FIPS)), by = "FIPS") %>%
  left_join(EducationData201923Clean %>% mutate(FIPS = as.character(FIPS)), by = "FIPS") %>%
  left_join(PopulationData2020Clean %>% mutate(FIPS = as.character(FIPS)), by = "FIPS") %>%
  left_join(DemographicData2020Clean %>% mutate(FIPS = as.character(FIPS)), by = "FIPS") %>%
  select(FIPS, COUNTYNAME, everything()) %>%
  # CamelCase 
  rename(
    # Identity
    CountyName            = COUNTYNAME,
    
    # Response Variable - Voting
    PresDemRatio          = PRES_DEM_RATIO,
    
    # Income / Tax Variables
    NumTaxReturns         = N1,
    TotalAgi              = A00100,
    SalariesWages         = A00200,
    BusinessNetIncome     = A00900,
    CapitalGains          = A01000,
    PensionsAnnuities     = A01700,
    UnemploymentComp      = A02300,
    SocialSecurityBenefits= A02500,
    PartnershipIncome     = A26270,
    MortgageInterest      = A19300,
    CharitableContributions= A19700,
    TotalTaxLiability     = A10300,
    EicAmount             = A59660,
    NumEicReturns         = N59660,
    ChildTaxCredit        = A11070,
    
    # Unemployment
    UnemploymentRate2020  = Unemployment_rate_2020,
    
    # Education
    PctNoHs               = PCT_NO_HS,
    PctHsOnly             = PCT_HS_ONLY,
    PctSomeCollege        = PCT_SOME_COL,
    PctBachelors          = PCT_BACH,
    
    # Population
    CensusPop2020         = CENSUS_2020_POP,
    NetMigration2020      = NET_MIG_2020,
    BirthRate2020         = R_BIRTH_2020,
    DeathRate2020         = R_DEATH_2020
  ) %>%
  
  # remove 46113 because it has been changed to Oglala Lakota County (46102)
  # remove 51515 because it has been changed to Bedford County (51019)
  filter(!(FIPS %in% c(46113, 51019))) %>%
  
  # fill out a missing county name
  mutate(CountyName = case_when(
    FIPS == "15005" ~ "Kalawao County",
    TRUE ~ CountyName
  ))
  
write_csv(FinalData, "./data/clean/FINAL_DATA.csv")
