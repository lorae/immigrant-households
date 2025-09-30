# import-data.R
#
# This script processes raw IPUMS data and saves it in a DuckDB file.
#
# Input:
# -  makes API call to IPUMS USA. Be sure to follow Part B of project set-up
#    in README.md before running - this script reads an environment variable from 
#    .Renviron
#

# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("ipumsr")
library("glue")

if (!file.exists(".Renviron")) {
  stop(".Renviron file needed for this code to run. Please refer to Part B of the README file for configuration instructions.")
} 

# Read API key from project-local .Renviron
readRenviron(".Renviron") # Force a re-read each run
api_key <- Sys.getenv("IPUMS_API_KEY")

if (api_key == "" || api_key == "your_ipums_api_key") {
  stop(".Renviron file exists, but IPUMS API key has not been added. Please refer to Part B of the README file for configuration instructions.")
}

print(paste0("IPUMS API key: ", api_key))
set_ipums_api_key(api_key)

# Set the destination directory for the IPUMS data pull
download_dir <- "data/ipums-microdata"

# ----- Step 1: Define, submit, and wait for data extract ----- #
# Browse available samples and their aliases
# get_sample_info("usa") |> print(n=200) 

# Define extract
ipums_extract <- define_extract_micro(
  description = "First data pull: immigrant households",
  collection = "usa",
  samples = c(
    # For more info see https://usa.ipums.org/usa/sampdesc.shtml
    "us1970c", # 1970 Form 1 Metro
    "us1980a", # 1980 5% state
    "us1990a", # 1990 5% state
    "us2000a", # 2000 5% 
    "us2012e", # 2008-2012, ACS 5-year
    "us2022c", # 2018-2022, ACS 5-year 
    "us2023a" # 2023 ACS (1-year)
  ),
  variables = c(
    # Household-level
    "YEAR", "SAMPLE", "SERIAL", "CBSERIAL", "HHWT",
    "CLUSTER", "STRATA", "GQ", "STATEFIP", "NUMPREC",
    "NFAMS", "NSUBFAM", "MULTGEN", #"VERSIONHIST", "HISTID" # Probably not needed, add back in if needed
    "OWNERSHP", "ROOMS", "BEDROOMS",
    # Person-level
    "PERNUM", "PERWT", "RELATE", "SEX", "AGE", "RACE", "HISPAN", 
    "YRIMMIG", "YRSUSA2", "MIGRATE5", "MIGRATE1", "MIGPLAC5", 
    "MIGPLAC1", "MOVEDIN", "BPL", "BPLD", "CITIZEN", "EDUC"
    # "REPWTP", "UNITSSTR", # Probably not needed, add back in if needed
  )
)

# Submit extract request
submitted <- submit_extract(ipums_extract)

# Poll until extract is ready
wait_for_extract(submitted) 