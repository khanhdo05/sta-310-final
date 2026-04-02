library(readr)
library(tidyverse)

# -------------------- READ IN DATA ------------------------------
VotingData <- read_csv("https://raw.githubusercontent.com/khanhdo05/sta-310-final/main/data/raw/38506-0001-Data.csv", show_col_types = FALSE)



# ------------------- CLEAN VOTING DATA --------------------------

VotingData2020Clean <- VotingData %>%
  # only interested in the presidential selection in 2020
  filter(YEAR == 2020) %>%
  
  # drop year and not interested in senate and partisan data
  select(-SEN_DEM_VOTES, -SEN_REP_VOTES, -SEN_DEM_RATIO, -SEN_REP_RATIO,
         -PARTISAN_INDEX_DEM, -PARTISAN_INDEX_REP, -YEAR)
