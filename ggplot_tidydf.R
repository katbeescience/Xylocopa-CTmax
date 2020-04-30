# Get input from clean_up_ascii script. Then make a pretty plot.
# Kathryn Busby
# mkbusby@email.arizona.edu
# Script started April 15, 2020

ggplot_tidydf <- function (filename) {

# Load pacakges.
  
  library(ggplot2)
  
  # Clean up the file and get it from clean_up_ascii.
  
  dir <- "~/Documents/Research/Xylocopa-CTmax/2020"
  find.file <- file.path(dir, "Data", paste0(filename,".txt"))
  tidy.df <- read.delim(find.file)

# Now make a plot.

tidy.plot <- ggplot(data=tidy.df, mapping=aes(x=Aux2)) +
  geom_line(aes(y=CO2_Percent)) +
  ylim(0,.05) +
  labs(xlab="FoxBox Temperature (C, Aux2)", ylab="CO2 Percent",
       title=paste0("Plot of data in file: ", filename)) +
  facet_wrap(tidy.df$Tube) +
  theme_classic()

ggsave(path=file.path(dir, "Output"), filename=paste0("plot_",filename,".png"),
       plot="tidy.plot", width=12, height=12, units="in")

return(tidy.plot)

}

ggplot_tidydf(filename="tidydf_06-19-2019")
