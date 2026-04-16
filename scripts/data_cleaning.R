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
decennial_vars <- c(
  total_pop   = "P1_001N",  
  white       = "P3_002N",  
  black       = "P3_003N",  
  asian       = "P3_005N",  
  hispanic    = "P5_002N",  
  male        = "P12_002N", 
  urban_pop   = "P2_002N"  # total count of the population living in urban areas within a county   
)

DemographicData2020Clean <- get_decennial(
  geography = "county",
  variables = decennial_vars,
  year = 2020,
  sumfile = "dhc"
) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(
    PctWhite    = white / total_pop * 100,
    PctBlack    = black / total_pop * 100,
    PctAsian    = asian / total_pop * 100,
    PctHispanic = hispanic / total_pop * 100,
    PctMale     = male / total_pop * 100,
    PctUrban    = urban_pop / total_pop * 100
  ) %>%
  mutate(FIPS = as.character(as.numeric(GEOID))) %>%
  select(FIPS, PctWhite, PctBlack, PctAsian, PctHispanic, PctMale)

# 2020 acs census - age data at county level
age_vars <- c(
  MedianAge         = "S0101_C01_032E", # Median age (years)
  AgeDependency     = "S0101_C01_033E", # Age dependency ratio
  ChildDependency   = "S0101_C01_034E", # Child dependency ratio
  OldAgeDependency  = "S0101_C01_035E", # Old-age dependency ratio
  Pct65Plus         = "S0101_C02_030E", # Percent 65 years and over
  # Brackets for 18-34 (20-34 + portion of 15-19)
  Pct20_24          = "S0101_C02_006E",
  Pct25_29          = "S0101_C02_007E",
  Pct30_34          = "S0101_C02_008E"
)

AgeData2020Clean <- get_acs(
  geography = "county",
  variables = age_vars,
  year = 2020,
  survey = "acs5",
  output = "wide"
) %>%
  # Sum the brackets to create a proxy for the 18-34 age group
  mutate(Pct18_34 = Pct20_24 + Pct25_29 + Pct30_34) %>%
  mutate(FIPS = as.character(as.numeric(GEOID))) %>%
  select(FIPS, MedianAge, Pct18_34, Pct65Plus, 
         AgeDependency, OldAgeDependency, ChildDependency)

# -------------------- WRITE TO TABLE --------------------------
write_csv(VotingData2020Clean, "./data/clean/VotingData2020.csv")
write_csv(IncomeData201920Clean, "./data/clean/IncomeData2020.csv")
write_csv(UnemploymentData2020Clean, "./data/clean/UnemploymentRate2020.csv")
write_csv(EducationData201923Clean, "./data/clean/EducationData201923.csv")
write_csv(PopulationData2020Clean, "./data/clean/PopulationData2020.csv")
write_csv(DemographicData2020Clean, "./data/clean/DemographicData2020.csv")
write_csv(AgeData2020Clean, "./data/clean/AgeData2020.csv")

# ------------------- JOIN ALL TABLES --------------------------
FinalData <- VotingData2020Clean %>%
  mutate(FIPS = as.character(FIPS)) %>%
  left_join(IncomeData201920Clean %>% mutate(FIPS = as.character(FIPS)), by = "FIPS") %>%
  left_join(UnemploymentData2020Clean %>% mutate(FIPS = as.character(FIPS)), by = "FIPS") %>%
  left_join(EducationData201923Clean %>% mutate(FIPS = as.character(FIPS)), by = "FIPS") %>%
  left_join(PopulationData2020Clean %>% mutate(FIPS = as.character(FIPS)), by = "FIPS") %>%
  left_join(DemographicData2020Clean %>% mutate(FIPS = as.character(FIPS)), by = "FIPS") %>%
  left_join(AgeData2020Clean %>% mutate(FIPS = as.character(FIPS)), by = "FIPS") %>%
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

# Urban Data Add
# =================== DOWNLOAD & CLEAN 2013 RUCC ===================
library(tidyverse)
library(readxl)
download.file(
  "https://www.ers.usda.gov/media/5769/2013-rural-urban-continuum-codes.xls",
  destfile = "./data/raw/2013-rural-urban-continuum-codes.xls",
  mode = "wb"
)

IsMetro2013 <- read_xls("./data/raw/2013-rural-urban-continuum-codes.xls", sheet = 1) %>%
  mutate(
    FIPS          = as.character(as.numeric(FIPS)),
    IsMetro2013   = RUCC_2013 <= 3
  ) %>%
  select(FIPS, IsMetro2013)

# =================== DOWNLOAD & CLEAN 2020 CBSA ===================
download.file(
  "https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2023/delineation-files/list1_2023.xlsx",
  destfile = "./data/raw/list1_2023.xlsx",
  mode = "wb"
)

IsMetro2020 <- read_xlsx("./data/raw/list1_2023.xlsx", skip = 2) %>%
  mutate(
    FIPS        = as.character(as.numeric(`FIPS State Code`) * 1000 + as.numeric(`FIPS County Code`)),
    IsMetro2020 = `Metropolitan/Micropolitan Statistical Area` == "Metropolitan Statistical Area"
  ) %>%
  select(FIPS, IsMetro2020)

# =================== JOIN BOTH TO FINAL DATA ======================
FinalData <- read_csv("./data/clean/FINAL_DATA.csv") %>%
  mutate(FIPS = as.character(FIPS)) %>%
  left_join(IsMetro2013 %>% mutate(FIPS = as.character(FIPS)), by = "FIPS") %>%
  left_join(IsMetro2020 %>% mutate(FIPS = as.character(FIPS)), by = "FIPS") %>%
  # Counties absent from the 2020 CBSA file are rural
  mutate(IsMetro2020 = replace_na(IsMetro2020, FALSE))

write_csv(FinalData, "./data/clean/FINAL_DATA_WITH_URBAN.csv")

# =================== AGREEMENT ANALYSIS ===========================
agreement <- FinalData %>%
  filter(!is.na(IsMetro2013), !is.na(IsMetro2020)) %>%
  mutate(
    Match = IsMetro2013 == IsMetro2020,
    Classification = case_when(
      IsMetro2013 &  IsMetro2020 ~ "Both Metro",
      !IsMetro2013 & !IsMetro2020 ~ "Both Non-Metro",
      IsMetro2013 & !IsMetro2020 ~ "Metro in 2013 only",
      !IsMetro2013 &  IsMetro2020 ~ "Metro in 2020 only"
    )
  )

pct_match <- mean(agreement$Match) * 100

cat("====== Metro Classification Agreement ======\n")
cat(sprintf("Overall match rate: %.1f%%\n\n", pct_match))
cat("Breakdown:\n")
print(
  agreement %>%
    count(Classification) %>%
    mutate(Pct = round(n / sum(n) * 100, 1)) %>%
    arrange(desc(n))
)

# Counties that switched — useful to inspect
cat("\nCounties that changed classification between 2013 and 2020:\n")
print(
  agreement %>%
    filter(!Match) %>%
    select(FIPS, CountyName, IsMetro2013, IsMetro2020, Classification) %>%
    arrange(Classification)
)