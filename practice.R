source("assessment_prioritization-master/R/summarize_revenue.R", echo = TRUE)
#source("assessment_prioritization-master/data/revenue_summarized_12052021.csv", echo = TRUE)

 summarize_revenue(
    file_name <- "assessment_prioritization-master/data/revenue_summarized_12052021.csv",
 		species_file <-  "assessment_prioritization-master/data/species_names.csv",
	  years <- 2016:2020
 )
  
 