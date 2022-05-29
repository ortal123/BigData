

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
original_table <- read.delim('Ex1/data/table.tsv')

# Add a DateTime column
original_table$DateTime <- as.POSIXct(original_table$megawatthours, tz = "EST", "%H:%M EST %m/%d/%Y")

# Order by date
ordered_table <- original_table[ order(original_table$DateTime), ]


## Question 1


# Set the location dimension
locations_list <- c("BPAT", "CISO", "CPLE", "ERCO", "FPL", "ISNE", "MISO", "NYIS", "PACW", "PJM", "United.States.Lower.48..region.")
locations <- vector()
for(j in locations_list) {
  for(i in seq(length(row.names(original_table)))) {
    locations <- c(locations, j)
  }
}

# Set the time dimension
times <- rep(ordered_table$DateTime, length(locations_list))

# Set the net generations
netgens <- vector()
for (i in list("Net.generation", "Net.generation.1", "Net.generation.2", "Net.generation.3", "Net.generation.4", "Net.generation.5", "Net.generation.6", "Net.generation.7", "Net.generation.8", "Net.generation.9", "Net.generation.10")) {
  netgens <- c(netgens, ordered_table[,i])
}

# Create data frame
df <- data.frame(location = locations, time = times, netgen = netgens)
row.names(df) <- NULL

# Create data cube
data_cube <- tapply(df$netgen, df[,c("location", "time")], FUN = sum )

# Convert time: EST -> PDT (-3 hrs):
for (i in c("PACW", "CISO", "BPAT")) {
  data_cube[i,] <- c(data_cube[i, seq(4, length(colnames(data_cube)))], NA, NA, NA)
}

# Convert time: EST -> CDT (-1 hrs): 
for (i in c("MISO", "ERCO")) {
  data_cube[i,] <- c(data_cube[i, seq(2, length(colnames(data_cube)))], NA)
}

# Relevant Range (07-02-2021 to 14-02-2021)
rng <- 720:887
data_cube <- data_cube[, rng]

# Aggregate by day - find the members of each day
rollup <- cut(as.numeric(as.POSIXct(dimnames(data_cube)$time )),
              c(as.numeric(as.POSIXct("2021-02-06 23:00:00 EST")),
                as.numeric(as.POSIXct("2021-02-07 23:00:00 EST")),
                as.numeric(as.POSIXct("2021-02-08 23:00:00 EST")),
                as.numeric(as.POSIXct("2021-02-09 23:00:00 EST")),
                as.numeric(as.POSIXct("2021-02-10 23:00:00 EST")),
                as.numeric(as.POSIXct("2021-02-11 23:00:00 EST")),
                as.numeric(as.POSIXct("2021-02-12 23:00:00 EST")),
                as.numeric(as.POSIXct("2021-02-13 23:00:00 EST"))),
              dig.lab = 8)

rollup_group <- split(dimnames(data_cube)$time, rollup)

aggregated_cube <- lapply(
  rollup_group, function(k)
    apply( data_cube[,k], c("location"), sum, na.rm = T )
)

new_data_cube <- data_cube[,1:length(rollup_group)]

dimlist <- dimnames(new_data_cube)
timecol <- which(names(dimlist) == "time")
names(dimlist)[timecol] <- "day"
dimlist$day <- c("2021-02-07", "2021-02-08", "2021-02-09", "2021-02-10", "2021-02-11", "2021-02-12", "2021-02-13")
dimnames(new_data_cube) <- dimlist

# Apply the new values
for (i in seq(length(rollup_group)))
  new_data_cube[,i] <- aggregated_cube[[i]]

# Calculate the means
means <- vector()
for ( i in dimlist$day ) {
  means[[ i ]] <- mean( new_data_cube[,i] )
}

# Rearrange in a new, temporary dataframe
DF <- data.frame(Day = dimlist$day, MeanNetGeneration = means)
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
locations_list <- c( "CPLE", "FPL", "ISNE","NYIS", "PJM")
locations <- vector()
for(j in locations_list) {
  for(i in seq(length(row.names(original_table)))) {
    locations <- c(locations, j)
  }
}

# Set the demands
demands <- vector()
for (i in list("Demand.2", "Demand.4",  "Demand.5", "Demand.7", "Demand.9")) {
  demands <- c(demands, ordered_table[,i])
}

# Set the time dimension
times <- rep(ordered_table[, 'DateTime'], length(locations_list))

# Create data frame
df <- data.frame(location = locations, time = times, demand = demands)
row.names(df) <- NULL

# Create data cube
data_cube <- tapply(df$demand, df[,c("location", "time")], FUN = sum)

# Relevant Range (07-02-2021 to 14-02-2021)
rng <- 720:887
data_cube <- data_cube[,rng]

# Retrieving the required hour
sliced_cube <- data_cube[,(hour(dimnames(data_cube)$time)>=10 & hour(dimnames(data_cube)$time)<=18) |
                         (hour(dimnames(data_cube)$time)>=20 & hour(dimnames(data_cube)$time)<=23) |
                         (hour(dimnames(data_cube)$time)>=0 & hour(dimnames(data_cube)$time)<=3)]

# Rollup
rollup <- cut(hour(dimnames(sliced_cube)$time),
              c(seq(-1, 3), seq(10, 18), seq(20, 23)),
              dig.lab = 17)

rollup_group <- split(dimnames(sliced_cube)$time, rollup)

# Aggregate by day
aggregated_cube <- lapply(
  rollup_group, function(k)
    apply( sliced_cube[,k], c("location"), sum, na.rm = T )
)

new_cube <- sliced_cube[,1:length(rollup_group)]
for (i in seq(length(rollup_group)))
  new_cube [,i] <- aggregated_cube[[i]]

demanddimlist <- dimnames(new_cube)
timecol <- which(names(demanddimlist) == "time")
names(demanddimlist)[timecol] <- "hour"
demanddimlist$hour <- c("00:00", "01:00", "02:00", "03:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "20:00", "21:00", "22:00", "23:00")
dimnames(new_cube) <- demanddimlist

# Define ranges
firstRange <- new_cube[,5:13] # 10:00-18:00
secondRange <- new_cube[,c(14:17, 1:4)] # 20:00-3:00

# Calculate the means
meansFirstRange <- vector()
meansSecondRange <- vector()

hourList1 <- c( "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00")
hourList2 <- c("20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00" )

for ( i in hourList1 ) {
  meansFirstRange[[ i ]] <- mean(firstRange[,i]/length(locations_list))
}

for ( i in hourList2 ) {
  meansSecondRange[[ i ]] <- mean(secondRange[,i]/length(locations_list))
}

# Rearrange in new, temporary dataframes
DF1 <- data.frame(Hour = hourList1, MeanDemand = meansFirstRange)
DF1$Hour <- as.numeric(factor(dimnames(firstRange)$hour, levels = hourList1))

DF2 <- data.frame(Hour = hourList2, MeanDemand = meansSecondRange)
DF2$Hour <- as.numeric(factor(dimnames(secondRange)$hour, levels = hourList2))

'Here, we implement the linear regression function instead of: lm(MeanDemand ~ Hour, data = DF1)
When %*% is matrix multiplication, t() is the transpose, and solve() is the invertible matrix (^-1).'

# Plot regression lines - first range
U1 <- data.matrix(data.frame(ones = rep(1, length(DF1$Hour)), hours = DF1$Hour)) # 5*2
y1 <- matrix(DF1$MeanDemand) # 5*1

coef1 <- solve((t(U1) %*% U1)) %*% t(U1) %*% y1

print(ggplot(DF1, aes(Hour, MeanDemand), lwd=1, group=1)
      + geom_point(col="blue") 
      + geom_abline(aes(intercept = coef1[1], slope = coef1[2]), lwd=1, show.legend=TRUE, col="gray")
      + scale_x_discrete(limits=hourList1) 
      + theme(legend.position = "bottom", panel.background = element_rect(fill = "beige"))
      + geom_line(show.legend = "TRUE", col="skyblue")
      + ggtitle("Power Demand - Day")
      + labs(y="Demand", x="Time"))

# Plot regression lines - second range
U2 <- data.matrix(data.frame(ones = rep(1, length(DF2$Hour)), hours = DF2$Hour)) # 5*2
y2 <- matrix(DF2$MeanDemand) # 5*1

coef2 <- solve((t(U2) %*% U2)) %*% t(U2) %*% y2

print(ggplot(DF2, aes(Hour, MeanDemand), lwd=1, group=1)
      + geom_point(col="blue") 
      + geom_abline(aes(intercept = coef2[1], slope = coef2[2]), lwd=1, show.legend=TRUE, col="gray")
      + scale_x_discrete(limits=hourList2) 
      + theme(legend.position = "bottom", panel.background = element_rect(fill = "beige"))
      + geom_line(show.legend = "TRUE", col="skyblue")
      + ggtitle("Power Demand - Night")
      + labs(y="Demand", x="Time"))


dev.off()



