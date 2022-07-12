#' Function that will read output from all models in 
#' the model_loc ("model_files") folder. The unfished
#' spawning biomass, final spawning biomass, sigma R,
#' assessment year, and mean age. The function then takes 
#' these results and calculates a weighted depletion and 
#' mean age for stocks with area based assessments. These
#' results are then added to the previous cycle's stock status
#' and assessment frequency sheets in the "data" folder. The 
#' updated stock status and assessment frequency csv files are 
#' then saved to the tables folder.  
#' 
#' 
#' 
#' 
#'
#' @param abundance_file A csv file containing the existing abundance by species that the 
#' newly assessed species abundance estimates will be added to or old abundance values replaced. The
#' csv file to be read is found in the data folder.
#' @param frequency_file A csv file from the previous assessment prioritization assessment
#' frequency tab. The csv file to be read is found in the data folder.
#' @param model_loc Folder name to look for model files. The default is "model_files" in the 
#' assessment prioritization github.
#' @param years Vector of specific years to calculate the mean age of the catches by species.
#' 
#' 
#'
#' @author Chantel Wetzel
#' @export
#' @md
#' 
#' @examples
#' 
#' summarize_model_results(
#' 		abundance_file <- "abundance_previous_cycle.csv",
#' 		frequency_file <- "assessment_frequency_last_cycle.csv",
#' 		species_file <- "species_names.csv",
#' 		model_loc <- "model_files",
#' 		years <- 2000:2020
#' )
#' 
summarize_model_results <- function(abundance_file, frequency_file, species_file, model_loc = "model_files", 
	years) {
 
	abundance <- read.csv(file.path("data", abundance_file)) 
	frequency <- read.csv(file.path("data", frequency_file))
	species  <-  read.csv(file.path("data", species_file))
	
	new_models <- list.files(model_loc)
	new_results <- data.frame(
		SpeciesArea = new_models,
		Species = NA,
		AssessYear = NA,
		MeanAge = NA,
		SigmaR = NA,
		SB0 = NA,
		SBfinal = NA,
		Status = NA,
		WeightedStatus = NA,
		WeightedMeanAge = NA
	)

	for (a in 1:length(new_models)) {

		new_results[a, "Species"] <- strsplit(new_models[a], ".", fixed = TRUE)[[1]][1]
		
		model <- r4ss::SS_output(file.path(getwd(), model_loc, new_models[a]), verbose = FALSE, printstat = FALSE)
		find <- which(model$catage$Yr %in% years)
		ncols <- dim(model$catage)[2] 
		age <- 0:(ncols - 12)
		new_results[a, "MeanAge"] <- round(sum(age * apply(model$catage[find, 12:ncols], 2, sum)) / 
											sum(model$catage[find, 12:ncols]), 1)
		new_results[a, "SigmaR"] <- model$sigma_R_in
		new_results[a, "SB0"] <- model$SBzero
		new_results[a, "SBfinal"] <- model$SBzero * model$current_depletion
		new_results[a, "Status"] <- model$current_depletion
		new_results[a, "AssessYear"] <- model$endyr + 1
	}

	unique_species <- unique(new_results$Species) 
	for(b in 1:length(unique_species)) {
		group <- which(new_results$Species == unique_species[b])
		new_results[group, "WeightedStatus"] <- sum(new_results[group, "SBfinal"]) / sum(new_results[group, "SB0"])
		prop <- new_results[group, "SBfinal"] / sum(new_results[group, "SBfinal"])
		new_results[group, "WeightedMeanAge"] <- sum(prop * new_results[group, "MeanAge"])
	}

	# Thread the new values into existing files
	new_abundance <- abundance
	new_frequency <- frequency
	for(b in 1:length(unique_species)) {

		to_match <- gsub("_", " ", unique_species[b])
		key <- NULL
		for (sp in 1:ncol(species)) {
			key <- c(key, grep(to_match, species[,sp], ignore.case = TRUE) )
		}

		group <- which(new_results$Species == unique_species[b])
		new_abundance[key,"Estimate"] <- new_results[group[1], "WeightedStatus"]

		new_frequency[key, "Recruit_Var"] <- new_results[group[1], "SigmaR"]
		new_frequency[key, "Mean_Age"] <- new_results[group[1], "WeightedMeanAge"]
		new_frequency[key, "Last_Assess"] <-  new_results[group[1], "AssessYear"]
	}

	# Rank and score the stock status sheet, delete trend column, and remove NAs.
	x <- new_abundance
	x$Rank <- x$Score <- NA
	for(sp in 1:length(x$Species)) {

		if(!is.na(x$Estimate[sp])) {
		x$Score[sp] <-
			ifelse(x$Estimate[sp]  > 2.0 * x$Target[sp],  1,
			ifelse(x$Estimate[sp] <= 2.0 * x$Target[sp] & x$Estimate[sp] > 1.5 * x$Target[sp], 2,
			ifelse(x$Estimate[sp] <= 1.5 * x$Target[sp] & x$Estimate[sp] > 1.1 * x$Target[sp], 3,
			ifelse(x$Estimate[sp] <= 1.1 * x$Target[sp] & x$Estimate[sp] > 0.9 * x$Target[sp], 4,
			ifelse(x$Estimate[sp] <= 0.9 * x$Target[sp] & x$Estimate[sp] > x$MSST[sp] & x$Trend[sp] %in% c(0, 1), 5,
			ifelse(x$Estimate[sp] <= 0.9 * x$Target[sp] & x$Estimate[sp] > x$MSST[sp] & x$Trend[sp] == -1, 7,
			ifelse(x$Estimate[sp] <= x$MSST[sp] & x$Trend[sp] == 1, 8,
			ifelse(x$Estimate[sp] <= x$MSST[sp] & x$Trend[sp] == 0, 9,
			ifelse(x$Estimate[sp] <= x$MSST[sp] & x$Trend[sp] == -1 , 10)))))))))
		} else {
		x$Score[sp] <- 
			ifelse(x$PSA[sp] < 1.8, 3,
			ifelse(x$PSA[sp] >= 1.8 & x$PSA[sp] < 2, 4,
			ifelse(x$PSA[sp] >= 2.0, 6)))	
		}
	}

	x <- x[order(x[,"Score"], decreasing = TRUE), ]

	zz <- 1
	for(i in 10:1) {
		ties <- which(x$Score == i)
		if(length(ties) > 0) {
			x$Rank[ties] <- zz
		}
		zz <- zz + length(ties)
	}

	abundance_out <- with(x, x[order(x[,"Species"]), ])
	abundance_out <- abundance_out[, c("Species", "Rank", "Score", "Estimate", "Target", "MSST", "PSA", "Trend")]

	write.csv(abundance_out, file.path("tables", "stock_status.csv"), row.names = FALSE)
	write.csv(new_frequency, file.path("tables", "frequency_partial.csv"), row.names = FALSE)
	write.csv(new_results, file.path("tables", "model_results.csv"), row.names = FALSE)
}