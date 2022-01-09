# Load library
library(geosphere)
library(readr)

# Load data
stations <- read_csv("stations.csv")
temps <- read.csv("temps50k.csv")

# Random not random
set.seed(1234567890)

station_temps <- merge(stations, temps, by="station_number")

# Times of the day
times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00",
           "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00",
           "24:00:00")

##############################################
##---------------- Our code ----------------##
##############################################

# =================================================
# Kernel 1 [gaussian] (distance between station and point of interest)
# =================================================
dist_kernel <- function(pred_pos, data, h)
{
  positions = data.frame(data$longitude, data$latitude)
  distances <- distHaversine(pred_pos, positions)
  u = distances / h
  plot(distances, exp(-(u)^2), main="Influence of distance")
  return( exp(-(u/h)^2) )
}

# =================================================
# Kernel 2 [gaussian] (Difference in date between measurement and day of interest)
# =================================================
day_kernel <- function(pred_day, data, h)
{
  day_diff = abs(as.numeric(strftime(data$date, format = "%j")) -
                   as.numeric(strftime(pred_date, format = "%j")))
  for (i in 1:length(day_diff))
  {
    day_diff[i] = min(day_diff[i], 365-day_diff[i])
  }
  u = day_diff / h
  plot(day_diff, exp(-(u)^2), main="Influence of date")
  return( exp(-(u)^2) )
}


# =================================================
# Kernel 3 [gaussian] (Difference in time between measurement to hour of interest)
# =================================================
hour_kernel <- function(pred_time, data, h)
{
  pred_date = paste("2013-01-01", pred_time, sep=" ")
  meas_date = paste("2013-01-01", data$time, sep=" ")
  time_diff = abs(as.numeric(difftime(pred_date, meas_date, units = "hour")))
  for (i in 1:length(time_diff))
  {
    time_diff[i] = min(time_diff[i], 24-time_diff[i])
  }
  u = time_diff / h
  plot(time_diff, exp(-(u)^2), main="Influence of time")
  return( exp(-(u)^2) )
}


# =================================================
# Kernel 4 (sum/product the above kernels)
# =================================================

# Smoothing coefficients (chosen by us)
h_distance <- 100000 # 10 mil
h_date     <- 10 # 10 days
h_time     <- 2 # 2 hours

# Point to predict (Chosen by us)
latit  <- 58.4108
longit <- 15.6216

# Date to predict
pred_date <- "2012-08-04"

# Container of predicted temperatures
temp_sum  <- vector(length=length(times))
temp_prod <- vector(length=length(times))
predict_pos = c(longit, latit)

# Get data before selected date and get k_values for distances and dates.
sub_data = subset(station_temps, date < pred_date)

k_dist = dist_kernel(predict_pos, sub_data, h_distance)
k_day = day_kernel(pred_date, sub_data, h_date)

# Get k_values for each time of the day and save estimations.
for (i in 1:length(times))
{
  k_hour = hour_kernel(times[i], sub_data, h_time)
  
  total_k_sum  = k_dist + k_day + k_hour
  total_k_prod = k_dist * k_day * k_hour
  
  temp_sum[i]  <- sum(total_k_sum  %*% sub_data$air_temperature) /
    sum(total_k_sum)
  temp_prod[i] <- sum(total_k_prod %*% sub_data$air_temperature) /
    sum(total_k_prod)
}

# Plot the temperatures
plot(temp_sum,
     xaxt="n", ylim=c(min(temp_sum, temp_prod)-1, max(temp_sum, temp_prod)+1),
     xlab="Time", ylab="Temperature",
     col="red", type="o",
     main="Estimated temperatures"
)
points(temp_prod, xaxt="n",
       col="blue", type="o",
)
axis(1, at=1:length(times), labels=times)
legend(x = "bottomright", 
       legend = c("Sum of kernels","Product of kernels"),
       col = c("red", "blue"), 
       pch = "oo")

