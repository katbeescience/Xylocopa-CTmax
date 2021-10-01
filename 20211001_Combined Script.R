# This script will turn into a function. The argument will be the segment file
# name.

# Load packages.

library(tidyverse)
library(lubridate)
library(hms)

# Bring in files.

# To start, we want a list of all the files. They are not all in the same folder.
# Automate.

# Get dates cleaned and right.

dateclean <- function(x) {
  rawsegment <- read.csv(file=x)

# Clean up data.

# Make 2 new columns: One that splits off date, and another that splits off time.

segment <- rawsegment
segment$DateTime <- mdy_hm(rawsegment$DateTime)
segment$Date <- as_date(segment$DateTime)
segment$Time <- as_hms(segment$DateTime)

# Return modified dataframe.

return(segment)
}


# Load files.

# This should come straight from the previous function later, so will lose this.
rawdatetimesegment <- segment

interpolation <- function(rawdatetimesegment) {
datetimesegment <- rawdatetimesegment

# Bring in body mass file.

rawbodymass <- read.csv(file="../Data/BodyMass.csv")
bodymass <- rawbodymass

# Format date in bodymass properly.

bodymass$Run.Date <- ymd(rawbodymass$Run.Date)
bodymass$Start.Time <- as_hms(bodymass$Start.Time)
bodymass$End.Time <- strptime(rawbodymass$End.Time, format = "%H:%M:%S")
bodymass$End.Time <- hms::as_hms(bodymass$End.Time)

# Index to parts in segment that match bodymass.

dateindex <- which(bodymass$Run.Date == datetimesegment$Date[1] &
                     bodymass$Tube == datetimesegment$Tube[1])

# Get BeeID that matches from bodymass.

BeeID <- bodymass$BeeID[dateindex]

# Make BeeID column in segment.

datetimesegment$BeeID <- BeeID

# Fix time format. It shouldn't be a character.

datetimesegment$DateTime <- strptime(rawdatetimesegment$DateTime,
                                     format = "%Y-%m-%d %H:%M:%S")
datetimesegment$Time <- as_hms(datetimesegment$DateTime)

# Get flexible formula for line from bodymass for proper bee.
# y = y1 + (x-x1)*(y2-y1)/(x2-x1)
# Here, our y is body mass (g). Our x is time. 2 is final and 1 is initial.

# As of Sept 30, this is working right, but may be reversed because clock thinks
# midnight is earlier in the day than the evening...
formula <- function(x) {
  y <- bodymass$Initial.Body.Mass..g.[dateindex] + 
    (as.numeric(x-bodymass$Start.Time[dateindex]))*
    (bodymass$Final.Body.Mass..g.[dateindex]-bodymass$Initial.Body.Mass..g.[dateindex])/
    (as.numeric(as_hms(bodymass$Time.Diff[dateindex])))
  return(y)}

i <- 1
time <- NULL

for(i in 1:length(na.omit(datetimesegment$Time))) {
  time <- datetimesegment$Time[i]
  datetimesegment$bodymass[i] <- formula(time)
  i <- i + 1
}
return(datetimesegment)
}

# Load file.

segmentfile <- datetimesegment

# Do math.

# First, calculate CO2 mL/g/min.

finalmath <- function(segmentfile){

segmentfile$CO2mLgmin <- (segmentfile$CO2_Percent*segmentfile$Flow_Read_mlmin) / 
  segmentfile$bodymass

segmentfile$CO2mean <- mean(segmentfile$CO2mLgmin)
segmentfile$tempmean <- mean(segmentfile$Aux2)

return(segmentfile)
}

# Having the list of files, we want to iterate through each one. Use a big for loop.

i <- 1
segmentfinal <- NULL
thissegment <- NULL

for (i in 1:length(filelist)) {
  
  x <- filelist[i]
  
  # # If every cell in a columns is "", delete that column
  # segment <- segment %>%
  #   discard(which(segment ==""))
  
  segment <- dateclean(x)
  rawdatetimesegment <- segment
  datetimesegment <- interpolation(rawdatetimesegment)
  CO2means <- datetimesegment
  thissegment <- finalmath(CO2means)
    
# Append to csv here
  segmentfinal <- rbind(segmentfinal, thissegment)
  
i <- i + 1
}

# Write csv

newcsv <- write.csv(segmentfinal,
                    file=paste0("../Data/Rfiles/FinalColumns/",Sys.Date(),"_final.csv"))
