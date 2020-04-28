# This script takes in an ascii file exported from a program using a proprietary file type.
# It tidies up the data in a repeatable way.
# It does this inside a function called clean.up.ascii.
# Kathryn Busby
# mkbusby@email.arizona.edu
# Script started April 15, 2020

# Bring in libraries:

library(tidyverse)

# Start the function.

clean.up.ascii <- function(filename, notefile){
  
  # To be flexible, the user can type their filename in the variable 'filename'.
  # The file should be in the data folder.
  # The file should have been exported from Expedata using the ASCII text file type, then .txt.
  
  dir <- "~/Documents/Research/Xylocopa-CTmax/2020/Data"
  filepath <- file.path(dir, filename)
  notepath <- file.path(dir, notefile)
  data.df = read.delim(filepath, header = TRUE, stringsAsFactors = FALSE, sep=",")
  note.df = read.delim(notepath, header = FALSE, stringsAsFactors = FALSE)
  
  # note.df has a weird format. We need to search for a pattern to define where to delimit.
  # Each row of note.df should be examined as a separate string.
  # The pattern it follows is that the row number is always listed at the beginning of the
  # string. Then a batch of 2-7 integers. Then a space (or two).
  # Beyond that pattern, the subsequent text should be considered a part of the next vector.
  # Then we need to plop those vectors into a new dataframe.
  
  new.split <- strsplit(note.df, pattern = "^\d{2,8}\\s")
  
  # Next, we only want the relevant columns: CO2, Oxygen, Aux1, Aux2.
  # We also want the data to be neat and labeled in a logical way.
  # Tidyverse is good at this.
  
  tidy.df <- data.df %>%
    select(FOXTemp_C, CO2_Percent, Aux2) %>%
    filter(Aux2 >= 40) %>%
    add_column(tidy.df, Row = c(1:nrow(tidy.df)))
  
  note.df <- note.df %>%
    rename(Row=V1, Notes=V2)
  
  tidy.df <- merge(tidy.df, note.df, by="Row")
  
  return(tidy.df)
  
}

