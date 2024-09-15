rm(list = ls())
source('Helper_Functions.R')
val_yr <- '2023-2024'
interest_rate <- 0.04

# LOAD ALL THE REQUIRED LIBRARIES
library(openxlsx)
library(lubridate)
library(dplyr)
library(ggplot2)

# IMPORT THE RAW DATA
raw_data <- read.xlsx("Raw Data.xlsx", sheet=1)

# COPY THE DATASET AND START THE ANALYSIS:
# 1. PERFORM THE DATE CHECK - PAID DATE SHOULD BE GREATER THAN INCURRED DATE
# 2. INSERT THE FISCAL YEAR COLUMNS AND LAG COLUMN
my_dataset <- raw_data
testit::assert(is.na(match(F, my_dataset['Paid_Date'] >= my_dataset$Incurred_Date))) # A check to confirm all Paid_Dates are greater than corresponding Incurred_Date

my_dataset$Incurred_FY <- lapply(my_dataset$Incurred_Date, date_to_fy)
my_dataset$Paid_FY <- lapply(my_dataset$Paid_Date, date_to_fy)
my_dataset <- my_dataset %>% mutate(
  Lag = as.character(as.numeric(substr(Paid_FY, 1, 4)) - as.numeric(substr(Incurred_FY, 1, 4))))

# CREATE A MATRIX FOR USE AS YOUR 'TRIANGLE'
# LIST YOUR FYs AND LAGS FOR LATER USE WHEN DOING CALCULATIONS IN THE MATRIX 
(fys <- sort(unlist(unique(my_dataset$Paid_FY))))
(n <- length(fys))
(lags <- 1:n-1)

# FIRST CREATE AN EMPTY MATRIX FOR INCREMENTAL TRIANLGE
incr.tri = matrix(nrow=n, ncol=n, dimnames=list(Incurred = fys, Lag = as.character(lags)))

# NOW FILL IN THE MATRIX WITH ACTUAL AMOUNTS
for(i in fys){
  for (j in lags){
    if(as.numeric(substr(i,1,4))+j > as.numeric(substr(val_yr,1,4))){next}
    incr.tri[i, as.character(j)] <- sum(my_dataset$`Amount_$`[my_dataset$Incurred_FY == i & my_dataset$Lag == as.character(j)])
  }
}
incr.tri
testit::assert(sum(incr.tri, na.rm=T) == sum(my_dataset$`Amount_$`)) # A check to confirm the triangle numbers match the original data set

# CALCULATE CUMULATIVE SUMS ACROSS THE ROWS
cum.tri <- incr.tri
for (i in fys){cum.tri[i,] <- cumsum(incr.tri[i,])}
cum.tri

ptd <- c()
for(i in 1:n){ptd <- append(ptd, cum.tri[i,n-i+1])} #All row sums obtained from the cumulated triangle
testit::assert(rowSums(incr.tri, na.rm=T) == ptd) # A check to confirm that row sums from incr.tri match the above

# CALCULATE DEVELOPMENT FACTORS USING THE CUMULATED MATRIX
dfs <- c()
for (i in 1:(n-1)){
  dfs = c(dfs, sum(cum.tri[1:(n-i),i+1]) / sum(cum.tri[1:(n-i),i]))
}
dfs
df.plot <- plot(lags, c(dfs,1), 'l', main='Payment Development Pattern', xlab='Lag', ylab='') # A plot to visualize the trend of payments
# ggplot(data=data.frame(lags, df=c(dfs,1)), aes(x=lags, y=df)) + geom_line()

# APPLY THE DEVELOPMENT FACTORS TO HISTORICAL DATA TO PROJECT FUTURE PAYMENTS
(cum.dfs <- c(1, cumprod(rev(dfs))))
(ult <- ptd * cum.dfs)
(forecast.payments1 <- ult - ptd)
sum(forecast.payments1)
### END OF PART I


### PART II - BREAKING DOWN THE FORECAST PAYMENTS BY THE YEAR WHEN THEY ARE DUE
# FILL IN THE RIGHT-HAND SIDE OF THE CUMULATIVE TRIANGLE BY APPLYING DFs TO EACH ELEMENT OF THE MATRIX
ult.cum.tri <- cum.tri
for (i in 1:n){
  for (j in 1:n){
    if(is.na(ult.cum.tri[i,j])==F){next}
    else ult.cum.tri[i,j] <- ult.cum.tri[i,j-1] * dfs[j-1]
  }
}
ult.cum.tri

# FILL IN THE RIGHT-HAND SIDE OF THE INCREMENTAL TRIANGLE BY 'DECUMULATING' THE ABOVE
ult.incr.tri <- incr.tri
for (i in 1:n){
  for (j in 1:n){
    if(is.na(ult.incr.tri[i,j])==F){next}
    else ult.incr.tri[i,j] <- ult.cum.tri[i,j] - ult.cum.tri[i,j-1]
  }
}
ult.incr.tri

# NOW ASSIGN THE AMOUNT DUE AT EACH FUTURE FY, START BY LISTING DOWN FUTURE FYs 
future_fys <- paste0(as.numeric(unlist(strsplit(val_yr, "-"))[2]), '-', as.numeric(unlist(strsplit(val_yr, "-"))[2])+1)
for (i in 2:(n-1)){future_fys <- c(future_fys,
                                   paste0(as.numeric(unlist(strsplit(future_fys[i-1], "-"))[2]), '-', as.numeric(unlist(strsplit(future_fys[i-1], "-"))[2])+1))}
future_fys

# NOW GET THE NOMINAL CASHFLOWS BY FYs
cashflows <- c()#data.frame(row.names=future_fys)
for (i in 1:(n-2)){cashflows[i] <- sum(diag(apply(ult.incr.tri[(i+1):n,(i+1):n], 2, rev)))}
cashflows[n-1] <- ult.incr.tri[n,n]
cashflows
(forecast.payments2 <- data.frame(FY=future_fys, Nominal=cashflows))
for (i in 1:(n-1)){forecast.payments2$Discounted[i] = forecast.payments2$Nominal[i]/(1+interest_rate)**i}
testit::assert(sum(forecast.payments1) == sum(forecast.payments2$Nominal)) # A test to check if the forecast tallies with the previous calculation
forecast.payments2

#### TO CONTINUE FROM HERE.

ggplot(forecast.payments2) +
  geom_point(aes(FY,Nominal))
#  geom_bar()# +
#  geom_point(data = ds, aes(y = mean), colour = 'red', size = 3)

test = c('staories')



