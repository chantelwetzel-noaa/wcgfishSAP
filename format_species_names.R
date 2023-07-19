#' Function to format species names to add cryptic species names is 
#' missing and to apply the upper case for the first letter for each
#' species name.
#' 
#' @param x A factor data frame
#' @param col_names The species column name within the factor data frame
#' @returns The factor data frame with the cryptic species renamed 
#' @author Chantel Wetzel
#' @examples 
#' \dontrun{ mort_adj <- rename_cryptic_names(x = mortality) }
#' 
format_species_names <- function(x, col_name = "Species"){
  
  x[, col_name] <- tolower(x[, col_name])
  cryptic_names <- rbind(
    c("rougheye rockfish", "blackspotted rockfish", "rougheye/Blackspotted rockfish"),
    c("vermilion rockfish", "sunset rockfish", "vermilion/Sunset rockfish"),
    c("gopher rockfish", "black and yellow rockfish", "gopher/Black and Yellow rockfish"),
    c("blue rockfish", "deacon rockfish", "blue/Deacon rockfish"))
  colnames(cryptic_names) <- c("name1", "name2", "cryptic_name")
  
  for(a in 1:nrow(cryptic_names)) {
    find <- c(grep(cryptic_names[a, "name1"], x[, col_name],  ignore.case = TRUE),
              grep(cryptic_names[a, "name2"], x[, col_name],  ignore.case = TRUE))
    if (length(find) > 0 ) {
      x[unique(find), col_name] <- cryptic_names[a, "cryptic_name"]  
    }
  }
  substr(x[, col_name], 1, 1) <- toupper(substr(x[, col_name], 1, 1))
  return(x)
}

