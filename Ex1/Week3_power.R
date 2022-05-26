
# Big Data - Exercise 1

# Yudit Halperin - 324216589
# Ortal
# Asnat


library(ggplot2)
library(lubridate)
pdf("Week3_power.pdf")


######################################### PART ONE

# Read data from table
originalTable <- read.delim('Ex1/data/table.tsv')

# Add a DateTime column
originalTable$DateTime <- as.POSIXct(originalTable$megawatthours, tz = "EST", "%H:%M EST %m/%d/%Y")

# Order the table by date
orderedTable <- originalTable[ order(originalTable$DateTime), ]

# Set the location dimension
locationsOnce <- c("BPAT", "CISO", "CPLE", "ERCO", "FPL", "ISNE", "MISO",
                   "NYIS", "PACW", "PJM", "United.States.Lower.48..region.")
locations <- vector()
for(j in locationsOnce) {
  for(i in seq(1536)) {
    locations <- c(locations, j)
  }
}

# Set the time dimension
times <- c(orderedTable$DateTime, orderedTable$DateTime, orderedTable$DateTime,
           orderedTable$DateTime, orderedTable$DateTime, orderedTable$DateTime,
           orderedTable$DateTime, orderedTable$DateTime, orderedTable$DateTime,
           orderedTable$DateTime, orderedTable$DateTime)

# Convert to EST time zone
attr(times, "tzone") <- "EST"

# Set the net generations
netgens <- vector()
for (i in list("Net.generation", "Net.generation.1", "Net.generation.2",
               "Net.generation.3", "Net.generation.4", "Net.generation.5", 
               "Net.generation.6", "Net.generation.7", "Net.generation.8",
               "Net.generation.9", "Net.generation.10")) {
  netgens <- c(netgens, orderedTable[,i])
}

# Create data frame
df <- data.frame(location = locations,
                 time = times,
                 netgen = netgens)
row.names(df) <- NULL
sortedDf <- df[order(df$time),]

# Create data cube
dataCube <-
  tapply(df$netgen,
         df[,c("location", "time")],
         FUN = sum )

# Relevant Range (07-02-2021 to 14-02-2021)
rng <- 720:887
dataCube <- dataCube[, 720:887]

# Aggregate by day
# Find the members of each day
rollup <- cut(as.numeric(as.POSIXct(dimnames(dataCube)$time )),
              c(as.numeric(as.POSIXct("2021-02-06 23:00:00 EST")),
                as.numeric(as.POSIXct("2021-02-07 23:00:00 EST")),
                as.numeric(as.POSIXct("2021-02-08 23:00:00 EST")),
                as.numeric(as.POSIXct("2021-02-09 23:00:00 EST")),
                as.numeric(as.POSIXct("2021-02-10 23:00:00 EST")),
                as.numeric(as.POSIXct("2021-02-11 23:00:00 EST")),
                as.numeric(as.POSIXct("2021-02-12 23:00:00 EST")),
                as.numeric(as.POSIXct("2021-02-13 23:00:00 EST"))),
              dig.lab = 8)


rollup.group <- split( dimnames(dataCube)$time, rollup )
n.dayLevels <- length(rollup.group)

aggregatedCube <- lapply(
  rollup.group, function(k)
    apply( dataCube[,k], c("location"), sum, na.rm = T )
)
new.dataCube <- dataCube[,1:n.dayLevels]

# Apply the new values
for (i in seq(n.dayLevels))
  new.dataCube[,i] <- aggregatedCube[[i]]

# Rectify the time dimension name and levels - convert it to
# the name "day" and apply day values to it
dimlist <- dimnames(new.dataCube)
timecol <- which(names(dimlist) == "time")
names(dimlist)[timecol] <- "day"
reformatDate <- function(x) as.Date(x)

dimlist$day <- c("2021-02-07", "2021-02-08", "2021-02-09", "2021-02-10",
                 "2021-02-11", "2021-02-12", "2021-02-13")

dimnames(new.dataCube) <- dimlist

# Calculate the means
means <- vector()

for ( i in dimlist$day ) {
  means[[ i ]] <- mean( new.dataCube[,i] )
}

# rearrange in a new, temporary dataframe
DF <- data.frame ( Day = dimlist$day, MeanNetGeneration = means)
DF$Day <- as.Date(DF$Day)

# Display English dates
Sys.setlocale("LC_TIME", "English")

#Plot
print(ggplot(DF, aes(Day, MeanNetGeneration))
      + geom_point() 
      + geom_line()
      + ggtitle("Net Generation by Date"))



######################################### PART TWO

# Read data from table
originalTable <- read.delim('Ex1/data/table.tsv')

# Add a DateTime column
originalTable$DateTime <- as.POSIXct( originalTable$megawatthours, tz = "EST",
                                      "%H:%M EST %m/%d/%Y" )

# Order the table by date
orderedTable <- originalTable[ order(originalTable$DateTime), ]
dates <- orderedTable[, 'DateTime']

# Set the location dimension
locationsOnce <- c( "CPLE", "FPL", "ISNE","NYIS",  "PJM")
locations <- vector()
for(j in locationsOnce) {
  for(i in seq(1536)) {
    locations <- c(locations, j)
  }
}

# Set the demands
demands <- vector()
for (i in list("Demand.2", "Demand.4",  "Demand.5", "Demand.7", "Demand.9")) {
  demands <- c(demands, orderedTable[,i])
}

# Set the time dimension
times <- c(dates, dates, dates, dates, dates)
# Convert to EST time zone
attr(times, "tzone") <- "EST"

# Create data frame
df <- data.frame(location = locations,
                 time = times,
                 demand = demands)
row.names(df) <- NULL

# Create data cube
dataCube <-
  tapply(df$demand,
         df[,c("location", "time")],
         FUN = sum )

# Relevant Range (07-02-2021 to 14-02-2021)
rng <- 720:887
dataCube <- dataCube[, 720:887]

# Retrieving the required hour
slicedCube <- dataCube[,(hour(dimnames(dataCube)$time)>=10 & hour(dimnames(dataCube)$time)<=18) |
                         (hour(dimnames(dataCube)$time)>=20 & hour(dimnames(dataCube)$time)<=23) |
                         (hour(dimnames(dataCube)$time)>=0 & hour(dimnames(dataCube)$time)<=3)]

# Rollup
rollup <- cut(hour(dimnames(slicedCube)$time),
              c(-1,0,1,2,3,10,11,12,13,14,15,16,17,18,20,21,22,23),
              dig.lab = 17)


rollup.group <- split( dimnames(slicedCube)$time, rollup )
newSize <- length(rollup.group)

# Aggregate by day
aggregatedCube <- lapply(
  rollup.group, function(k)
    apply( slicedCube[,k], c("location"), sum, na.rm = T )
)

newCube  <- slicedCube[,1:newSize]
for (i in seq(newSize))
  newCube [,i] <- aggregatedCube[[i]]

demanddimlist <- dimnames(newCube)
timecol <- which(names(demanddimlist) == "time")
names(demanddimlist)[timecol] <- "hour"
demanddimlist$hour <- c("00:00", "01:00", "02:00", "03:00", "10:00", "11:00",
                        "12:00", "13:00", "14:00", "15:00", "16:00", "17:00",
                        "18:00", "20:00", "21:00", "22:00", "23:00")
dimnames(newCube) <- demanddimlist

firstRange <- newCube[,5:13]
secondRange <- newCube[,c(14,15,16,17,1,2,3,4)]

# Calculate the means
meansFirstRange <- vector()
meansSecondRange <- vector()

hourList1 <- c( "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00")
hourList2 <- c("20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00" )

for ( i in hourList1 ) {
  meansFirstRange[[ i ]] <- mean( firstRange[,i]/5 )
}

for ( i in hourList3 ) {
  meansSecondRange[[ i ]] <- mean( secondRange[,i]/5 )
}


# rearrange in new, temporary dataframes
DF1 <- data.frame ( Hour = hourList1, MeanDemand = meansFirstRange)
DF2 <- data.frame ( Hour = hourList2, MeanDemand = meansSecondRange)


DF1$Hour <- factor (dimnames(firstRange)$hour, levels = hourList1)
DF1$Hour <- as.numeric(DF1$Hour)

DF2$Hour <- factor (dimnames(secondRange)$hour, levels = hourList2)
DF2$Hour <- as.numeric(DF2$Hour)

# Plot regression lines
LM1 <- lm( MeanDemand ~ Hour, data = DF1)
a1 <- coef(LM1)[1]
b1 <- coef(LM1)[2]
print(ggplot(DF1, aes(Hour, MeanDemand), lwd=1, group=1)
      + geom_point() 
      + geom_abline(aes(intercept = a1, slope = b1), lwd=1, show.legend=TRUE )
      + scale_x_discrete(limits=hourList1) 
      + theme(legend.position = "bottom")
      + geom_line(show.legend = "TRUE")
      + ggtitle("Regression Line Describing the Power Demand at Daytime")
      + labs(y="Demand", x="Time"))

LM2 <- lm( MeanDemand ~ Hour, data = DF2)
a2 <- coef(LM2)[1]
b2 <- coef(LM2)[2]
print(ggplot(DF2, aes(Hour, MeanDemand), lwd=1, group=1)
      + geom_point() 
      + geom_abline(aes(intercept = a2, slope = b2), lwd=1, show.legend=TRUE )
      + scale_x_discrete(limits=hourList2) 
      + theme(legend.position = "bottom")
      + geom_line(show.legend = "TRUE")
      + ggtitle("Regression Line Describing the Power Demand in the Afternoon")
      + labs(y="Demand", x="Time"))


dev.off()

