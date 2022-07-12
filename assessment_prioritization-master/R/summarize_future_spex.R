#' Function to compare future ACLs to existing fishing mortality averages. 
#' This function uses output from the summarize_fishing_mortality and the summarize_frequency
#' functions and hence should be only run after these two functions. The manage_file
#' is downloaded from PacFIN APEX future harvest specifications
#' table (GMT008). The draft harvest specification ACLs should correspond to 
#' the current year plus two (e.g., 2022 + 2 = 2024 ACLs). The fishing_mort_file
#' is the csv file created by the summarize_fishing_mortality function where
#' the average fishing mortality across recent years is used to compare future
#' ACLs against. The freq_file is a csv file created by the summarize_frequency function where
#' the year of the last assessment will be found to be appending in this output file.
#' 
#'
#' @param manage_file A csv file with OFLs and ACLs from the draft harvest 
#' specifications table in PacFIN APEX reporting online.
#' @param fishing_mort_file A csv file created by the summarize_fishing_mortality function.
#' @param freq_file A csv file created by the summarize_frequency function.
#' @param species_file A csv file in the data folder called "species_names.csv" to summarize across.
#'
#' @author Chantel Wetzel
#' @export
#' @md
#' 
#' @examples
#'
#' Example call including all species:
#' summarize_future_spex(
#'    manage_file <- "GMT008-harvest specifications-2024.csv",
#'    fishing_mort_file <- "fishing_mortality.csv",
#'    freq_file <- "assessment_frequency.csv",
#'    species_file <-  "species_names.csv"
#' )
#'
summarize_future_spex <- function(manage_file, fishing_mort_file, freq_file, species_file) {

	targets <- read.csv(file.path("data", manage_file)) 
	fmort_data <- read.csv(file.path("tables", fishing_mort_file))
	species <-  read.csv(paste0("data/", species_file))
	freq_data <- read.csv(file.path("tables", freq_file))

	fmort_data <- with(fmort_data, fmort_data[order(fmort_data[,"Species"]), ])
	freq_data <- with(freq_data, freq_data[order(freq_data[,"Species"]), ])
	
	mort_df <- data.frame(
		Species = species[,1], 
		Rank = NA, 
		Factor_Score = NA,
		Modifier = NA,
		Fishing_Mortality = NA,
		OFL = NA,
		ACL = NA,
		ACL_Attain_Percent = NA,
		Last_Assessed = NA 
	)
	
	for(sp in 1:nrow(species)) {

		key <- ss <- NULL
		name_list <- species[sp, species[sp,] != -99]
		for(a in 1:length(name_list)){
			key = c(key,
				grep(species[sp,a], fmort_data$Species, ignore.case = TRUE)
			)

			ss <- c(ss, 
				grep(species[sp, a], targets$STOCK_OR_COMPLEX, ignore.case = TRUE)
			)
		}

		# deal with multiple captures from complex species
		ss <- unique(ss)
		mort_df[sp, "OFL"] <- sum(targets[ss, "OFL"], na.rm = TRUE)
		mort_df[sp, "ACL"] <- sum(targets[ss, "ACL"], na.rm = TRUE) 

		mort_df[sp, "Fishing_Mortality"] <- fmort_data[key[1], "Fishing_Mortality"]
	}

	mort_df[, "ACL_Attain_Percent"] <- mort_df[, "Fishing_Mortality"] / mort_df[, "ACL"]
	mort_df[, "Last_Assessed"] <- freq_data[, "Last_Assess"]

	for(sp in 1:nrow(species)){
		score <-
			ifelse(mort_df$ACL_Attain_Percent[sp] <= 0.10, 1,
			ifelse(mort_df$ACL_Attain_Percent[sp] > 0.10 & mort_df$ACL_Attain_Percent[sp] <= 0.25, 2,
			ifelse(mort_df$ACL_Attain_Percent[sp] > 0.25 & mort_df$ACL_Attain_Percent[sp] <= 0.50, 3,
			ifelse(mort_df$ACL_Attain_Percent[sp] > 0.50 & mort_df$ACL_Attain_Percent[sp] <= 0.75, 5,
			ifelse(mort_df$ACL_Attain_Percent[sp] > 0.75 & mort_df$ACL_Attain_Percent[sp] <= 0.90, 7,
			ifelse(mort_df$ACL_Attain_Percent[sp] > 0.90 & mort_df$ACL_Attain_Percent[sp] <= 1.00, 8,
			ifelse(mort_df$ACL_Attain_Percent[sp] > 1.00 & mort_df$ACL_Attain_Percent[sp] <= 1.10, 9,
			ifelse(mort_df$ACL_Attain_Percent[sp] > 1.10, 10))))))))

		mort_df$Factor_Score[sp] <- score
	}

	mort_df <- 
		mort_df[order(mort_df[,"Factor_Score"], decreasing = TRUE), ]

	x <- 1
	for(i in 10:1) {
		ties <- which(mort_df$Factor_Score == i)
		if(length(ties) > 0) {
			mort_df$Rank[ties] <- x
		}
		x <- x + length(ties)
	}

	# Modifier scores ranging from +4 to -2 where highest attainment would equal 4
	for(sp in 1:nrow(species)){
		mort_df[sp, "Modifier"] <-
			ifelse(mort_df$Factor_Score[sp] == 10, 4, 
			ifelse(mort_df$Factor_Score[sp] == 9, 3, 
			ifelse(mort_df$Factor_Score[sp] == 8, 2, 
			ifelse(mort_df$Factor_Score[sp] == 7, 1,
			ifelse(mort_df$Factor_Score[sp] == 5, 0,
			ifelse(mort_df$Factor_Score[sp] >  4 & mort_df$Factor_Score[sp] <= 2, -1, -2))))))
	}

	mort_df <- with(mort_df, mort_df[order(mort_df[,"Species"]), ])

	write.csv(mort_df, file.path("tables", "future_spex.csv"), row.names = FALSE)
}