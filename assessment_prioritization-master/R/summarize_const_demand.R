#' Calculations used for the "Const Demand" tab for assessment prioritization.
#' The values used in the constituent demand tab are primarily scored qualitatively.
#' This function will provied the state, gear, and sector (commercial vs. recreational)
#' differences across the states and coastwide which then can be qualitatively
#' used to input modifiers.
#' 
#' 
#' 
#'
#' @param commercial_file A csv file with commercial revenue and gear codes downloaded
#' from PacFIN (via login account).
#' @param rec_file A csv file with recreational importance factor calculation
#' @param species_file A csv file with species names to summarize
#' @param years Years to average the commercial revenue across.
#' @param max_exp Numeric value that will be applied to create a spread across ranked 
#' species calculated as: value ^ max_exp
#'
#' @author Chantel Wetzel
#' @export
#' @md
#' 
#' @examples
#' 
#' summarize_const_demand(
#' 		commercial_file <- "commercial_revenue_with_gear_code.csv"
#' 		rec_file <- "recreational_importance.csv"
#' 		species_file <-  "species_names.csv"
#' 		years <- 2016:2020
#' )
#'
summarize_const_demand <- function(commercial_name, rec_file, species_file, years, max_exp = 0.18) {

	com_data <- read.csv(file.path("data", commercial_file)) 
	rec_data <- read.csv(file.path("tables", rec_file)) 
	species <-  read.csv(file.path("data", species_file))

	com_data$gear <- "TWL"
	com_data$gear[!com_data$PACFIN_GROUP_GEAR_CODE == "TWL"] <- "NTWL"

	rec_data <- with(rec_data, rec_data[order(rec_data[,"Species"]), ])

	data <- data.frame(Species = species[,1], 
					   CW = NA, 
					   C = NA,
					   O = NA,
					   W = NA, 
					   TWL = NA, 
					   NTWL = NA)

	score_df <- rank_df <- data
	rec_score_df <- rec_rank_df <- data[, -c(ncol(data)-1, ncol(data))]

	denominator <- 1000
	max_value <- 10

	# Calculate commercial ranking and scores first
	for(sp in 1:nrow(species)) {

		key <- NULL
		name_list <- species[sp, species[sp,] != -99]
		for(a in 1:length(name_list)){
			key = c(key,
				grep(species[sp,a], com_data$NOMINAL_TO_ACTUAL_PACFIN_SPECIES_NAME, ignore.case = TRUE)
			)
		}
		
		sub_data <- com_data[key,]
		find <- which(sub_data$PACFIN_YEAR %in% years) 
		sub_data <- sub_data[find, ]

		tmp <- aggregate(AFI_EXVESSEL_REVENUE ~ AGENCY_CODE, sub_data, function(x) sum(x) / denominator )
		data[sp, "CW"] <- sum(tmp$AFI_EXVESSEL_REVENUE) 
		for(aa in sort(unique(tmp$AGENCY_CODE))) {
			data[sp, colnames(data) == aa] <- tmp[tmp$AGENCY_CODE == aa, "AFI_EXVESSEL_REVENUE"]
		}

		tmp <- aggregate(AFI_EXVESSEL_REVENUE ~ gear, sub_data, function(x) sum(x) / denominator)
		for(gg in sort(unique(tmp$gear))) {
			data[sp, colnames(data) == gg] <- tmp[tmp$gear == gg, "AFI_EXVESSEL_REVENUE"]
		}

		find <- is.na(data[sp,])
		data[sp, find] <- 0		
	}

	int_value <- data[, -1] ^ max_exp
	for(a in 2:ncol(score_df)){
		score_df[, a] <- int_value[, a-1] * max_value / max(int_value[, a-1])
		tmp <- score_df[order(score_df[, a], decreasing = TRUE), c(1, a)]
		tmp$tmp_rank <- 1:nrow(tmp)
		rank <- with(tmp, tmp[order(tmp[,"Species"]), ])
		rank_df[, a] <- rank$tmp_rank

		x <- 1
		for(i in sort(unique(score_df[, a]), decreasing = TRUE)) {
			ties <- which(score_df[, a] == i)
			if(length(ties) > 0) {
				rank_df[, a][ties] <- x
			}
			x <- x + length(ties)
		}
	}
    
    # Recreational importance
    rec_tmp <- rec_data[, c("Pseudo_CW", "Pseudo_CA", "Pseudo_OR", "Pseudo_WA")]
    rec_tmp[is.na(rec_tmp)] <- 0

	int_value <- rec_tmp ^ max_exp
	for(a in 2:ncol(rec_score_df)){
		rec_score_df[, a] <- int_value[, a-1] * max_value / max(int_value[, a-1])
		tmp <- rec_score_df[order(rec_score_df[, a], decreasing = TRUE), c(1, a)]
		tmp$tmp_rank <- 1:nrow(tmp)
		rank <- with(tmp, tmp[order(tmp[,"Species"]), ])
		rec_rank_df[, a] <- rank$tmp_rank

		x <- 1
		for(i in sort(unique(rec_score_df[, a]), decreasing = TRUE)) {
			ties <- which(rec_score_df[, a] == i)
			if(length(ties) > 0) {
				rec_rank_df[, a][ties] <- x
			}
			x <- x + length(ties)
		}
	}

	write.csv(score_df, file.path("tables", "const_demand_commercial_scores.csv"), row.names = FALSE)
	write.csv(rec_score_df, file.path("tables", "const_demand_recreational_scores.csv"), row.names = FALSE)
	write.csv(rank_df, file.path("tables", "const_demand_commercial_ranks.csv"), row.names = FALSE)
	write.csv(rec_rank_df, file.path("tables", "const_demand_recreational_ranks.csv"), row.names = FALSE)

}