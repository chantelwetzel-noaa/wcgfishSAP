# Always: Load packages
library(sa4ss)

# Always: Specify the directory for the document
setwd("C:/Users/Chantel.Wetzel/Documents/GitHub/assessment_prioritization/doc")

# Once: Define the title, species, and authors
#       Required author names here: https://github.com/nwfsc-assess/sa4ss/blob/master/data-raw/authors.csv
sa4ss::draft(authors = c("Chantel R. Wetzel", "Jim Hastie", "Kristin Marshall"),
  			 species = "Copper Rockfish",
  			 latin = "Sebastes caurinus",
  			 coast = "Oregon US West",
  			 type = c("sa"),
  			 create_dir = FALSE,
  			 edit = FALSE)

# Render Call:
if(file.exists("_main.Rmd")){
	file.remove("_main.Rmd")
}
# Render the pdf
bookdown::render_book("00a.Rmd", clean=FALSE, output_dir = getwd())
