#' Function to delete extraneous Stock Synthesis files.
#' This function only deletes extra Stock Synthesis files to reduce
#' the number of files for each assessment. Not necessary to run to
#' read or summarize model results.
#'
#' @param loc file location where folders needing cleaned up are
#'
#' @author Chantel Wetzel
#' @export
#' @md
#' 
#' @examples
#' clean_model_files(loc = "model_files/")
#'
#'
clean_model_files <- function(loc) {

	dir_list <- list.dirs(loc)[-1]
	remove <- grep("plots", dir_list)
	dir_list <- dir_list[-remove]

	files_to_keep <- c(
		"CompReport.sso",
		"control.ss_new",
		"data.ss_new",
		"forecast.ss",
		"starter.ss",
		"Forecast-Report.sso",
		"Report.sso",
		"ss.par", 
		"covar.sso",
		"wtatage.ss_new",
		"warning.sso"
	)

	for(a in 1:length(dir_list)){
		all <- list.files(dir_list[a])
		remove <- !all %in% files_to_keep
		file.remove(file.path(dir_list[a], all[remove]))
	}


}