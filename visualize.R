# This script visualizes pre-processed CTmax data.

# Load libraries.

# Bring in file.

rawdata <- read.csv(file="../Data/Rfiles/FinalColumns/2021-10-01_final.csv")

# Clean up data.

finaldata <- rawdata %>%
  select(BeeID, lifestage, CO2mean, tempmean)

# Visualize.

ggplot(data=finaldata, mapping=aes(x=tempmean, y=CO2mean,
                                   group=BeeID, color=lifestage)) +
  geom_smooth() +
  xlim(40,58) +
  ylim(0,25) +
  theme_classic()
