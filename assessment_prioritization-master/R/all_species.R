#' Common species name in a matrix form. This approach can result in 
#' issues in identifying species. Needs to be improved in order to create
#' search groups for complex species.
#' 
#' @author Chantel Wetzel
#' @export
#' @md
#' 
#' 
all_species <- function(){
default <- getOption('warn')
options(warn = -1)

species = rbind(
	"sablefish",                                                                                                   
	"longnose skate",                                                                                                   
	c("gopher rockfish", "black and yellow rockfish", "gopher and black and yellow rockfish", "gopher/black and yellow rockfish"),
                 "cabezon",                                                                                                  
                  "cowcod",                                                                                                  
          "black rockfish",                                                                                                  
             "Pacific cod",                                                                                                  
          "brown rockfish",                                                                                                  
         "copper rockfish",                                                                                                  
            "petrale sole",                                                                                                  
              "Dover sole",                                                                                                  
               "big skate",                                                                                                  
           "bank rockfish",                                                                                                  
      "quillback rockfish",                                                                                                  
              c("treefish", "tree rockfish"),                                                                                                  
     "shortraker rockfish",                                                                                                  
          "grass rockfish",                                                                                                  
         "Pacific sanddab",                                                                                                  
      "yelloweye rockfish",                                                                                                  
                 "lingcod",                                                                                                  
      "redbanded rockfish",                                                                                                  
         "canary rockfish",                                                                                                  
     "yellowtail rockfish",                                                                                                  
     "squarespot rockfish",                                                                                                  
      "splitnose rockfish",                                                                                                  
          "widow rockfish",                                                                                                  
       "speckled rockfish",                                                                                                  
   c("Pacific spiny dogfish", "spiny dogfish", "dogfish shark"),                                    
               "sand sole",                                                                                                  
          "olive rockfish",                                                                                                  
   "shortspine thornyhead",                                                                                                  
           "kelp rockfish",                                                                                                  
         "starry rockfish",                                                                                                  
                "rex sole",                                                                                                  
           "flathead sole",                                                                                                  
         "starry flounder",                                                                                                  
   "greenstriped rockfish",                                                                                                  
            "English sole",                                                                                                  
       c("rougheye rockfish", "blackspotted rockfish", "rougheye/blackspotted rockfish", "rougheye and blackspotted rockfish"),
           "flag rockfish",                                                                                                  
          "China rockfish",                                                                                                  
             c("chilipepper", "chilipepper rockfish"),                                                                                                 
                "bocaccio",                                                                                                  
              c("rock sole", "rock sole unident", "northern rock sole", "southern rock sole"),                                                                                                  
 "California scorpionfish",                                                                                                  
    "longspine thornyhead",                                                                                                  
          c("blue rockfish", "deacon rockfish", "blue/deacon rockfish", "blue and deacon rockfish"),
     "arrowtooth flounder",                                                                                                  
          "kelp greenling",                                                                                                  
   "greenspotted rockfish",                                                                                                  
   "darkblotched rockfish",                                                                                                  
      "blackgill rockfish",                                                                                                  
         "aurora rockfish",                                                                                                  
     "Pacific ocean perch",                                                                                                  
      "sharpchin rockfish",                                                                                                  
     c("vermilion rockfish", "sunset rockfish",  "vermilion/sunset rockfish", "vermilion and sunset rockfish"),
      "honeycomb rockfish") 

options(warn = default)

return(species)

}