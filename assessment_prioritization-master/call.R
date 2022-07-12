# Order of operations for the 2022 Assessment Prioritization proces

setwd("C:/Users/Chantel.Wetzel/Documents/GitHub/assessment_prioritization")
# figure out how to use here::here

# Load up the species CSV file that will be used across functions
species_file <-  "species_names.csv"
# Set the years for the revenue summary, recreational importance
years <- 2016:2020

#============================================================================
# Commercial Revenue:
#============================================================================
# CSV files pulled from PacFIN with
# COUNCIL_CODE == "P" and !FLEET_CODE %in% c("TI", "R")
summarize_revenue(file_name = "revenue_summarized_12052021.csv", 
	species_file = species_file, 
	years = years, 
	max_exp = 0.18)

#============================================================================
# Tribal revenue:
#============================================================================
# CSV files pulled from PacFIN with
# COUNCIL_CODE == "P" and FLEET_CODE == "TI"
summarize_revenue(file_name = "tribal_revenue_12062021.csv", 
	species_file = species_file, 
	years = years, 
	max_exp = 0.18)

#============================================================================
# Recreational Importance:
#============================================================================
# Landings data pulled from RecFIN for all states and species
summarize_rec_importance(
	file_name = "CTE002-2016---2020.csv", 
	species_file = species_file, 
	years = years, 
	max_exp = 0.18) 

#============================================================================
# Fishing mortality:
# Large management quantity download from PacFIN (GMT016) 
# provided by Rob Ames. Only single species information 
# available for download as of December 2021
#============================================================================

# The nwfscSurvey package is loaded in order to pull GEMM data
# which is the official mortality source
library(nwfscSurvey)
years <- 2018:2020
species_file <-  "species_names.csv"

fish_mort <- summarize_fishing_mortality(
	manage_file = "WOC_STOCK_SUMMARY11232021.csv", 
	species_file = species_file, 
	years = years)

#============================================================================
# Function that will clean up all folders in the "model_files"
# folder to only keep the files needed for r4ss to read model results
#============================================================================

clean_model_files(loc = "model_files/")

#============================================================================
# Function to grab and summarize the newest assessment results which
# will then update the abundcancd (stock status) and the assessment
# frequency csvs from last cycle
#============================================================================

summarize_model_results(
	abundance_file = "abundance_previous_cycle.csv", 
	frequency_file = "assessment_frequency_last_cycle.csv", 
	species_file = species_file, 
	model_loc = "model_files", 
	years = 2000:2020)

#============================================================================
# Do Ecosystem Call HERE
#============================================================================

#============================================================================
# Do Future SPEX constraint mortality HERE
#============================================================================

manage_file <- "GMT008-harvest specifications-2024.csv"
fishing_mort_file <- "fishing_mortality.csv"
freq_file <- "assessment_frequency.csv"
species_file <-  "species_names.csv"
years <- c(2019, 2021)
low_f_species <- TRUE

#species_file <- "species_names_not_included.csv"
#low_f_species = FALSE

summarize_future_spex(
	manage_file = manage_file, 
	fishing_mort_file = fishing_mort_file, 
	freq_file = freq_file, 
	species_file = species_file, 
	years = years, 
	low_f_species = low_f_species
)

#============================================================================
# 
#============================================================================

frequency_file <- "frequency_partial.csv"
ecosystem_file <- "ecosystem.csv"
mortality_file <- "fishing_mortality.csv"
future_mortality_file <- "fishing_mortality.csv"
prioritization_year <- 2023

summarize_frequency(
	frequency_file = frequency_file, 
	ecosystem_file = ecosystem_file, 
	mortality_file = mortality_file, 
	future_mortality_file = future_mortality_file, 
	prioritization_year = prioritization_year, 
	max_age = 20, 
	age_exp = 0.38)

#============================================================================
# 
#============================================================================

commercial_file <- "commercial_revenue_with_gear_code.csv"
rec_file <- "recreational_importance.csv"
species_file <-  "species_names.csv"
years <- 2016:2020

summarize_const_demand(
	commercial_name = commercial_name, 
	rec_file = rec_file, 
	species_file = species_file, 
	years = years, 
	max_exp = 0.18)
