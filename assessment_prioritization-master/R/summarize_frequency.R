#' Function to create the full assessment frequency tab. This function relies on output
#' from the summarize_model_results, ecosystem, summarize_fishing_mortality, and 
#' summarize_future_spex functions. 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#'
#' @param frequency_file A csv file created by the summarize_model_results function that 
#' inlcudes summarize the recruitment variation, mean age, year of last assessment, and the SSC
#' recommendation about the next assessment being update or a full assessment.
#' @param ecosystem_file A csv file  
#' @param mortality_file A csv file created by the summarize_fishing_mortality function with 
#' attainment comparison between recent mortality averages vs. the OFL and ACL.
#' @param future_mortality_file A csv file created by the summarize_future_spex function with 
#' attainment comparison between recent mortality averages vs. the future OFL and ACL.
#' @param prioritization_year A numerical value of the current year the assessment prioritization
#' is being conducted. This value is compared to the last year assessed values to provide species
#' rotation in the ranking based on time since the last assessment.
#' @param max_age A numerical value that is used with the age_exp to transform mean catch age 
#' by creating a meaningful spread across species (MeanAge * max_age)^age_exp). Current default
#' value is set to 20. The transformed mean catch age is combined with the "total adjustment" value where
#' the "Adjusted Transformed Mean Catch Age" is equal to the sum of these two values, unless they
#' are greater than 10, which are then capped at a maximum value of 10. The total adjustments are a
#' sum of the scored from the Recruit_Adj + Mortality_Adj + Eco_Adj
#' @param age_exp A numerical value applied as a exponential to calculate the transformed mean age
#' of the catch. Default value of 0.38.
#'
#' @author Chantel Wetzel
#' @export
#' @md
#'  
#' 
#' @examples
#' 
#' summarize_frequency(
#' 		frequency_file <- "frequency_partial.csv",
#' 		ecosystem_file <- "ecosystem.csv",
#' 		mortality_file <- "fishing_mortality.csv",
#' 		future_mortality_file <- "fishing_mortality.csv",
#' 		prioritization_year <- 2023,
#' 		max_age = 20,
#' 		age_exp = 0.38
#' )
#' 
summarize_frequency <- function(frequency_file, ecosystem_file, mortality_file, 
	future_mortality_file, prioritization_year, max_age = 20, age_exp = 0.38) {
 
	frequency <- read.csv(file.path("tables", frequency_file))
	ecosystem  <- read.csv(file.path("tables", ecosystem_file))
	mortality <- read.csv(file.path("tables", mortality_file))
	future_mortality <- read.csv(file.path("tables", future_mortality_file))

	ecosystem <- with(ecosystem, ecosystem[order(ecosystem[,"Species"]), ])
	mortality <- with(mortality, mortality[order(mortality[,"Species"]), ])
	future_mortality <- with(future_mortality, future_mortality[order(future_mortality[,"Species"]), ])
	
	df <- data.frame(
		Species = frequency$Species,
		Recruit_Var = frequency$Recruit_Var,
		MeanAge = frequency$Mean_Age,
		Trans_MeanAge = NA,
		Recruit_Adj = NA,
		Mortality_Adj = NA,
		Eco_Adj = NA,
		Total_Adj = NA,
		MeanAge_Adj = NA,
		MeanAge_Adj_Round = NA,
		Last_Assess = frequency$Last_Assess,
		Years_Since_Assess = NA,
		Beyond_Target_Freq = NA,
		Adj_Negative = NA,
		Greater_Than_10 = NA,
		Less_Than_6_Update = NA,
		Greater_Than_Target_Freq = NA,
		Constraint_2022 = NA,
		Score = NA,
		Rank = NA
	)

	df$Trans_MeanAge <- (df$MeanAge * max_age)^age_exp
	df$Years_Since_Assess <- prioritization_year - df$Last_Assess

	for(sp in 1:nrow(df)) {
		if(!is.na(df$Recruit_Var[sp])) {
			df$Recruit_Adj[sp] <-
				ifelse(df$Recruit_Var[sp] >= 1, -1, 
				ifelse(df$Recruit_Var[sp] < 1 & df$Recruit_Var[sp] >= 0.35, 0, 
				ifelse(df$Recruit_Var[sp] < 0.30, 1))) 
		} else {
			df$Recruit_Adj[sp] <-  0
		}

		df$Mortality_Adj[sp] <- 
			ifelse(mortality$Rank[sp] < 21, -1, 
			ifelse(mortality$Rank[sp] >= 21 & mortality$Rank[sp] < 41, 0, 1))

		df$Eco_Adj[sp] <- 
			ifelse(ecosystem$Rank[sp] < 21, -1,
			ifelse(ecosystem$Rank[sp] >= 21 & ecosystem$Rank[sp] < 41, 0, 1))

		df$Total_Adj[sp] <- df$Recruit_Adj[sp] + df$Mortality_Adj[sp] + df$Eco_Adj[sp]

		df$MeanAge_Adj[sp] <- 
			ifelse( df$Trans_MeanAge[sp] + df$Total_Adj[sp] > 10, 
				10, df$Trans_MeanAge[sp] + df$Total_Adj[sp])

		df$MeanAge_Adj_Round[sp] <- 
			ifelse(df$MeanAge_Adj[sp] < 4, 4, 2 * round(df$MeanAge_Adj[sp] / 2) )

		if(!is.na(df$Years_Since_Assess[sp])) {
			df$Beyond_Target_Freq[sp] <- 
				ifelse(df$Years_Since_Assess[sp] == 2, -2, 
					max(df$Years_Since_Assess[sp] - df$MeanAge_Adj_Round[sp], 0))
		} else {
			df$Beyond_Target_Freq[sp] <- 4
		}

		df$Adj_Negative[sp] <- ifelse(df$Beyond_Target_Freq[sp] == 4, -1 * df$Eco_Adj, 0)

		df$Greater_Than_10[sp] <- ifelse(df$Years_Since_Assess[sp] > 10, 1, 0)

		df$Less_Than_6_Update[sp] <- 
			ifelse(df$Years_Since_Assess[sp] < 6 & frequency$SSC_Rec[sp] == "Update", -1, 0)

		df$Greater_Than_Target_Freq[sp] <- 
			ifelse(df$MeanAge_Adj_Round[sp] <= df$Years_Since_Assess[sp], 1, 0)

		df$Constraint_2022[sp] <- 
			ifelse(future_mortality$Rank[sp] < 21, -1, 
			ifelse(future_mortality$Rank[sp] >= 21 & future_mortality$Rank[sp] < 41, 0, 1))

		df$Score[sp] <- 
			sum(df$Beyond_Target_Freq[sp], 
				df$Adj_Negative[sp], 
				df$Greater_Than_10[sp], 
				df$Less_Than_6_Update[sp],
				df$Greater_Than_Target_Freq[sp],
				df$Constraint_2022[sp],
				na.rm = TRUE)
	}

	df <- df[order(df[,"Score"], decreasing = TRUE), ]
	x <- 1
	for(i in 10:min(df$Score)) {
		ties <- which(df$Score == i)
		if(length(ties) > 0) {
			df$Rank[ties] <- x
		}
		x <- x + length(ties)
	}

	out <- with(df, df[order(df[,"Species"]), ])
	write.csv(out, file.path("tables", "assessment_frequency.csv"), row.names = FALSE)

	keep <- which(!colnames(out) %in% c("Mortality_Adj", "Eco_Adj", "Adj_Negative", "Constraint_2022"))
	df <- out[, keep]
	for(sp in 1:nrow(df)) {		

		df$Total_Adj[sp] <- df$Recruit_Adj[sp] 

		df$MeanAge_Adj[sp] <- 
			ifelse( df$Trans_MeanAge[sp] + df$Total_Adj[sp] > 10, 
				10, df$Trans_MeanAge[sp] + df$Total_Adj[sp])

		df$MeanAge_Adj_Round[sp] <- 
			ifelse(df$MeanAge_Adj[sp] < 4, 4, 2 * round(df$MeanAge_Adj[sp] / 2) )

		if(!is.na(df$Years_Since_Assess[sp])) {
			df$Beyond_Target_Freq[sp] <- 
				ifelse(df$Years_Since_Assess[sp] == 2, -2, 
					max(df$Years_Since_Assess[sp] - df$MeanAge_Adj_Round[sp], 0))
		} else {
			df$Beyond_Target_Freq[sp] <- 4
		}

		df$Greater_Than_10[sp] <- ifelse(df$Years_Since_Assess[sp] > 10, 1, 0)

		df$Less_Than_6_Update[sp] <- 
			ifelse(df$Years_Since_Assess[sp] < 6 & frequency$SSC_Rec[sp] == "Update", -1, 0)

		df$Greater_Than_Target_Freq[sp] <- 
			ifelse(df$MeanAge_Adj_Round[sp] <= df$Years_Since_Assess[sp], 1, 0)

		df$Score[sp] <- 
			sum(df$Beyond_Target_Freq[sp], 
				df$Greater_Than_10[sp], 
				df$Less_Than_6_Update[sp],
				df$Greater_Than_Target_Freq[sp],
				na.rm = TRUE)
	}	

	df <- df[order(df[,"Score"], decreasing = TRUE), ]
	x <- 1
	for(i in 10:min(df$Score)) {
		ties <- which(df$Score == i)
		if(length(ties) > 0) {
			df$Rank[ties] <- x
		}
		x <- x + length(ties)
	}

	out <- with(df, df[order(df[,"Species"]), ])
	write.csv(out, file.path("tables", "assessment_frequency_simplified.csv"), row.names = FALSE)
	
}