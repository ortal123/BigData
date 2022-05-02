
# Read data:
A <- read.delim('Ex1/data/table.tsv')
A$DateTime <- as.POSIXct( A$megawatthours, tz = "EST", "%H:%M EST %m/%d/%Y" )

# Order  by time:
B <- A[ order(A$DateTime), ]

# Choose Net.generation columns:
C <- with(B, cbind(Date = as.integer(format(as.Date(A[, 'DateTime']), "%Y%m%d")), BPAT = Net.generation, CISO	= Net.generation.1, CPLE = Net.generation.2, ERCO = Net.generation.3, FPL = Net.generation.4, ISNE = Net.generation.5, MISO = Net.generation.6,
                         NYIS = Net.generation.7, PACW = Net.generation.8, PJM = Net.generation.9, US_Lower = Net.generation.10))

#C <- with(B, cbind(DateTime, Net.generation, Net.generation.1, Net.generation.2, Net.generation.3, Net.generation.4, Net.generation.5, Net.generation.6, Net.generation.7 , Net.generation.8 , Net.generation.9 , Net.generation.10))

# EDT -> PDT (-3 hrs):
for (i in c(0, 1, 8)) {
  
  C[, i + 2] <- c(C[(4 : length(C[,1])), i + 2], NA, NA, NA)
  
}

# EDT -> CDT (-1 hrs): 
for (i in c(3, 6)) {
  
  C[, i + 2] <- c(C[(2 : length(C[,1])), i + 2], NA)
  
}

# Slicing - week of 7-14 Feb:
D <- C[which( C[, 'DateTime'] < as.POSIXct("2021-02-15 00:00:00 EST") & C[, 'DateTime'] >= as.POSIXct("2021-02-07 00:00:00 EST")), ]

# Roll-up - hours to days:


strptime(D[1, "DateTime"], tz = "EST", format = "%d")

as.POSIXct(strptime(D[2, "DateTime"],tz = "EST", format = "%d"))




