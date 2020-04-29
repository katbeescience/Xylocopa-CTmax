# Get input from clean_up_ascii script. Then make a pretty plot.
# Kathryn Busby
# mkbusby@email.arizona.edu
# Script started April 15, 2020

ggplot_tidydf <- function (filename, notefile) {

# Load pacakges.
  
  library(ggplot2)
  source("clean_up_ascii.R")
  
  # Clean up the file and get it from clean_up_ascii.
  
  tidy.df <- clean.up.ascii(filename, notefile) 

# Now make a plot.

ggplot(data=tidy.df, mapping=aes(x=Aux2)) +
  geom_line(aes(y=CO2_Percent)) +
  ylim(0,.05)

}

ggplot_tidydf(filename="06-19-2019.txt", notefile="Notes_06-19-2019.txt")
