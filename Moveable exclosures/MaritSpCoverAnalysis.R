#### Analysing species cover data ####
#Importing data
Species_cover_setup <- read_delim("Moveable exclosures/Species.cover.setup.csv", 
                                  ";", escape_double = FALSE, trim_ws = TRUE)
Species_cover_harvest <- read_delim("Moveable exclosures/Species.cover.harvest.csv", 
                                  ";", escape_double = FALSE, trim_ws = TRUE)

