
# Big Data - Exercise 1
# GitHub: https://github.com/JuditHalperin/BigData/tree/main/Ex1

# Yudit Halperin - 324216589
# Ortal Calfon (Peretz) - 315011189
# Asnat Berlin - 211825401


# Load libraries
library(ggplot2)
library(lubridate)

# Open output file
pdf("Ex1/output/Week3_power.pdf")

# Read input data
originalTable <- read.delim('Ex1/data/table.tsv')

# Add a DateTime column
originalTable$DateTime <- as.POSIXct(originalTable$megawatthours, tz = "EST", "%H:%M EST %m/%d/%Y")

# Order the table by date
orderedTable <- originalTable[ order(originalTable$DateTime), ]


## Question 1


# Set the location dimension
locationsOnce <- c("BPAT", "CISO", "CPLE", "ERCO", "FPL", "ISNE", "MISO", "NYIS", "PACW", "PJM", "United.States.Lower.48..region.")
locations <- vector()
for(j in locationsOnce) {
  for(i in seq(length(row.names(originalTable)))) {
    locations <- c(locations, j)
  }
}

# Set the time dimension
times <- rep(orderedTable$DateTime, length(locationsOnce))

# Set the net generations
netgens <- vector()
for (i in list("Net.generation", "Net.generation.1", "Net.generation.2",
               "Net.generation.3", "Net.generation.4", "Net.generation.5", 
               "Net.generation.6", "Net.generation.7", "Net.generation.8",
               "Net.generation.9", "Net.generation.10")) {
  netgens <- c(netgens, orderedTable[,i])
}

# Create data frame
df <- data.frame(location = locations, time = times, netgen = netgens)
row.names(df) <- NULL
sortedDf <- df[order(df$time),]

# Create data cube
dataCube <-
  tapply(df$netgen,
         df[,c("location", "time")],
         FUN = sum )

# EST -> PDT (-3 hrs):
for (i in c("PACW", "CISO", "BPAT")) {
  dataCube[i,] <- c(dataCube[i, seq(4, length(colnames(dataCube)))], NA, NA, NA)
}

# EST -> CDT (-1 hrs): 
for (i in c("MISO", "ERCO")) {
  dataCube[i,] <- c(dataCube[i, seq(2, length(colnames(dataCube)))], NA)
}

# Relevant Range (07-02-2021 to 14-02-2021)
rng <- 720:887
dataCube <- dataCube[, rng]

# Aggregate by day - find the members of each day
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

rollup.group <- split(dimnames(dataCube)$time, rollup)
n.dayLevels <- length(rollup.group)

aggregatedCube <- lapply(
  rollup.group, function(k)
    apply( dataCube[,k], c("location"), sum, na.rm = T )
)
new.dataCube <- dataCube[,1:n.dayLevels]

# Rectify the time dimension name and levels - convert it to the name "day" and apply day values to it
dimlist <- dimnames(new.dataCube)
timecol <- which(names(dimlist) == "time")
names(dimlist)[timecol] <- "day"
dimlist$day <- c("2021-02-07", "2021-02-08", "2021-02-09", "2021-02-10", "2021-02-11", "2021-02-12", "2021-02-13")
dimnames(new.dataCube) <- dimlist

# Apply the new values
for (i in seq(n.dayLevels))
  new.dataCube[,i] <- aggregatedCube[[i]]

# Calculate the means
means <- vector()

for ( i in dimlist$day ) {
  means[[ i ]] <- mean( new.dataCube[,i] )
}

# Rearrange in a new, temporary dataframe
DF <- data.frame ( Day = dimlist$day, MeanNetGeneration = means)
DF$Day <- as.Date(DF$Day)

# Display English dates
Sys.setlocale("LC_TIME", "English")

# Plot
print(ggplot(DF, aes(Day, MeanNetGeneration))
      + geom_point(col="blue") 
      + geom_line(col="skyblue")
      + ggtitle("Mean Daily Power Generation Across The US (7-14 Feb)")
      + theme(panel.background = element_rect(fill = "beige"))
      + labs(y="Mean Net Generation", x="Day"))


## Question 2


# Set the location dimension
locationsOnce <- c( "CPLE", "FPL", "ISNE","NYIS", "PJM")
locations <- vector()
for(j in locationsOnce) {
  for(i in seq(length(row.names(originalTable)))) {
    locations <- c(locations, j)
  }
}

# Set the demands
demands <- vector()
for (i in list("Demand.2", "Demand.4",  "Demand.5", "Demand.7", "Demand.9")) {
  demands <- c(demands, orderedTable[,i])
}

# Set the time dimension
times <- rep(orderedTable[, 'DateTime'], length(locationsOnce))

# Create data frame
df <- data.frame(location = locations, time = times, demand = demands)
row.names(df) <- NULL

# Create data cube
dataCube <- tapply(df$demand, df[,c("location", "time")], FUN = sum)

# Relevant Range (07-02-2021 to 14-02-2021)
rng <- 720:887
dataCube <- dataCube[,rng]

# Retrieving the required hour
slicedCube <- dataCube[,(hour(dimnames(dataCube)$time)>=10 & hour(dimnames(dataCube)$time)<=18) |
                         (hour(dimnames(dataCube)$time)>=20 & hour(dimnames(dataCube)$time)<=23) |
                         (hour(dimnames(dataCube)$time)>=0 & hour(dimnames(dataCube)$time)<=3)]

# Rollup
rollup <- cut(hour(dimnames(slicedCube)$time),
              c(seq(-1, 3), seq(10, 18), seq(20, 23)),
              dig.lab = 17)

rollup.group <- split(dimnames(slicedCube)$time, rollup)
newSize <- length(rollup.group)

# Aggregate by day
aggregatedCube <- lapply(
  rollup.group, function(k)
    apply( slicedCube[,k], c("location"), sum, na.rm = T )
)

newCube <- slicedCube[,1:newSize]
for (i in seq(newSize))
  newCube [,i] <- aggregatedCube[[i]]

demanddimlist <- dimnames(newCube)
timecol <- which(names(demanddimlist) == "time")
names(demanddimlist)[timecol] <- "hour"
demanddimlist$hour <- c("00:00", "01:00", "02:00", "03:00", "10:00", "11:00",
                        "12:00", "13:00", "14:00", "15:00", "16:00", "17:00",
                        "18:00", "20:00", "21:00", "22:00", "23:00")
dimnames(newCube) <- demanddimlist

# Define ranges
firstRange <- newCube[,5:13]
secondRange <- newCube[,c(14:17, 1:4)]

# Calculate the means
meansFirstRange <- vector()
meansSecondRange <- vector()

hourList1 <- c( "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00")
hourList2 <- c("20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00" )

for ( i in hourList1 ) {
  meansFirstRange[[ i ]] <- mean(firstRange[,i]/length(locationsOnce))
}

for ( i in hourList2 ) {
  meansSecondRange[[ i ]] <- mean(secondRange[,i]/length(locationsOnce))
}

# Rearrange in new, temporary dataframes
DF1 <- data.frame ( Hour = hourList1, MeanDemand = meansFirstRange)
DF2 <- data.frame ( Hour = hourList2, MeanDemand = meansSecondRange)

DF1$Hour <- factor (dimnames(firstRange)$hour, levels = hourList1)
DF1$Hour <- as.numeric(DF1$Hour)

DF2$Hour <- factor (dimnames(secondRange)$hour, levels = hourList2)
DF2$Hour <- as.numeric(DF2$Hour)

# Plot regression lines
U <- data.matrix(data.frame(ones = rep(1, length(DF1$Hour)), hours = DF1$Hour)) # 5*2
y <- matrix(DF1$MeanDemand) # 5*1
coef <- t(U) %*% U


LM1 <- lm(MeanDemand ~ Hour, data = DF1)
print(ggplot(DF1, aes(Hour, MeanDemand), lwd=1, group=1)
      + geom_point(col="blue") 
      + geom_abline(aes(intercept = coef)[1], slope = coef[2]), lwd=1, show.legend=TRUE, col="gray")
      + scale_x_discrete(limits=hourList1) 
      + theme(legend.position = "bottom", panel.background = element_rect(fill = "beige"))
      + geom_line(show.legend = "TRUE", col="skyblue")
      + ggtitle("Power Demand - Day")
      + labs(y="Demand", x="Time"))

LM2 <- lm(MeanDemand ~ Hour, data = DF2)
print(ggplot(DF2, aes(Hour, MeanDemand), lwd=1, group=1)
      + geom_point(col="blue") 
      + geom_abline(aes(intercept = coef(LM2)[1], slope = coef(LM2)[2]), lwd=1, show.legend=TRUE, col="gray")
      + scale_x_discrete(limits=hourList2) 
      + theme(legend.position = "bottom", panel.background = element_rect(fill = "beige"))
      + geom_line(show.legend = "TRUE", col="skyblue")
      + ggtitle("Power Demand - Night")
      + labs(y="Demand", x="Time"))


dev.off()

