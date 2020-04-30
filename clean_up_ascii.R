# This script takes in an ascii file exported from a program using a proprietary
# file type.
# It tidies up the data in a repeatable way.
# It does this inside a function called clean.up.ascii.
# Kathryn Busby
# mkbusby@email.arizona.edu
# Script started April 15, 2020

# Start the function.

clean.up.ascii <- function(filename, notefile){

  # Bring in libraries:
  
  library(tidyverse)
    
  # To be flexible, the user can type their filename in the variable 'filename'.
  # The file should be in the data folder.
  # The file should have been exported from Expedata using the ASCII text file
  # type, then .txt.
  
  dir <- "~/Documents/Research/Xylocopa-CTmax/2020/Data"
  filepath <- file.path(dir, filename)
  notepath <- file.path(dir, notefile)
  
  # data.df will contain the data.
  data.df = read.delim(filepath, header = TRUE,
                       stringsAsFactors = FALSE, sep=",")
  
  # note.df will contain the notes. They should be tacked on to the data later.
  # the weird formatting needs to be fixed, and the NAs removed.
  
  note.df = read.delim(notepath, header = FALSE,
                       stringsAsFactors = FALSE, sep=" ")
  
  tidy.note <- note.df %>%
    select(V1, V3:ncol(note.df)) %>%
    unite(col="Notes", -V1, remove=TRUE, sep=" ") %>%
    rename(Row = "V1")
  
  # Wherever we have the word "Ramp", we should make that into a column called
  # "Ramp" and put whatever follows it in that column.
  
  pattern <- c("\\dC")
  tidy.note$Ramp <- str_extract(tidy.note$Notes, pattern=regex(pattern,
                                ignore_case = TRUE))
  
  # Tube column is a copy of Notes
  # Extract ramp info
  # Extract everything remaining that's not a single digit.
  
  tidy.note$Tube <- tidy.note$Notes 
  tidy.note$Tube <- str_remove(tidy.note$Tube, pattern=regex(pattern,
                                ignore_case = TRUE))
  
  # Later on, we'll want to call the Baseline tube 0, to keep the tube column
  # numeric only. So here we'll find all instances of "Baseline" in tidy.note
  # and rename them 0.
  
  tidy.note$Tube[grep("Baseline",tidy.note$Tube, value = FALSE)] <- 0
  tidy.note$Tube <- str_remove_all(tidy.note$Tube, pattern=("\\D*"))
  
  # Next, we only want the relevant columns from the data df:
  # CO2, Oxygen, Aux1, Aux2.
  # We also want the data to be neat and labeled in a logical way.
  # Tidyverse is good at this.
  
  tidy.df <- data.df %>%
    select(FOXTemp_C, CO2_Percent, Aux2) %>%
    filter(Aux2 >= 40)
  
  tidy.df <- tidy.df %>%
    add_column(Row = c(1:nrow(tidy.df)))
  
  # We now want to tack tidy.note on to the end of tidy.df, but we want to line
  # them up by the values in Row. Wherever the values in Row are equal, that's
  # where they should line up. This could be done in a for-loop.
  
  i <- 1
  tidy.df$Notes <- "NA"
  tidy.df$Ramp <- "NA"
  tidy.df$Tube <- "NA"
  
  for (i in 1:nrow(tidy.note)) {
    index <- which(tidy.df$Row == tidy.note$Row[i])
    tidy.df$Notes[index] <- paste(tidy.note$Notes[i])
    tidy.df$Ramp[index] <-  paste(tidy.note$Ramp[i])
    tidy.df$Tube[index] <-  paste(tidy.note$Tube[i])
    i <- i + 1
  }
  
  # We want to populate the Tube column with data going forward from
  # the point where the last pre-existing specification was in the original data.
  # So we should copy the contents of the previous cell, go down the list, and
  # fill the subsequent empty cells with the pasted data. If the cell is not
  # empty, then we should copy the new contents and start filling going down
  # from that point.
  
  i <- 1
  Tube.holder <- "0"
  
  for (i in 1:nrow(tidy.df)) {
    if (tidy.df$Tube[i] == "NA") {
      tidy.df$Tube[i] = Tube.holder
    } else {Tube.holder = tidy.df$Tube[i]
    }
    i <- i + 1
  }
  
  # Save the resulting data in an output file.
  
  write.table(tidy.df, file =
              paste0("~/Documents/Research/Xylocopa-CTmax/2020/Data/tidydf_",filename),
              sep="\t",
              row.names=FALSE)
  
  return(tidy.df)
  
}

clean.up.ascii(filename="06-19-2019.txt", notefile="Notes_06-19-2019.txt")
