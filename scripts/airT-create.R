#
# Create fake temperature data
#
#


# MODUL 04 task

# generate 15 min time-series data for 4 days
start_time <- as.POSIXct("2026-01-28 00:00:00")
end_time   <- as.POSIXct("2026-01-30 23:45:00")
time <- seq(from = start_time, to = end_time, by = "15 min")

# create daily theoretical temperature variation with a range of 10 degrees F
daily_cycle <- 10 * sin(2 * pi * as.numeric(difftime(time, start_time, units = "hours")) / 24)

# vary temperature around 25 F and add noise
value <- 25 + daily_cycle + rnorm(length(time), mean = 0, sd = 2.5)

# create data frame
df_temp <- data.frame( time = time, value = value)

# plot generate air temp
plot(df_temp$time, df_temp$value, type = "l", xlab = "Time", ylab = "Temperature (F)",
     main = "January Air Temperature Data")
# optional - show
lines(df_temp$time, 30+daily_cycle, col='red', lwd=0.75, lty=2)


df_temp$hour <- as.POSIXct(
  format(df_temp$time, "%Y-%m-%d %H:30:00")
  # tz = attr(df$time, "tzone")
)

# aggregate hourly
hourly_mean <- aggregate(value ~ hour, data = df_temp, FUN = mean)
# OR
hourly_mean2 <- tapply(df_temp$value, df_temp$hour, mean)


plot(df_temp$time, df_temp$value, type = "l", xlab = "Time", ylab = "Temperature (F)",
     main = "January Hourly Air Temperature (F) - Option 1", col='grey', lty=2)
lines(hourly_mean, col='red')

plot(df_temp$time, df_temp$value, type = "l", xlab = "Time", ylab = "Temperature (F)",
     main = "January Hourly Air Temperature (F) - Option 2", col='grey', lty=2)
lines(as.POSIXct(names(hourly_mean2)), hourly_mean2, col='red')


# 6 hrs
df_temp$hour6 <- as.POSIXct(
  paste(
    format(df_temp$time, "%Y-%m-%d"),
    sprintf("%02d:00:00", (as.integer(format(df_temp$time, "%H")) %/% 6) * 6)
  )
)

# aggregate to 6-hour means
mean_6hour <- aggregate(value ~ hour6, data = df_temp, FUN = mean)

# aggregate to 6-hour means
mean_6hour <- aggregate(value ~ hour6, data = df_temp, FUN = mean)







# INCLUDE MONTHLY TRENDLINE
# go out a month!

# Create 15-minute timestamps for one month (January 2024)
start_time <- as.POSIXct("2024-01-01 00:00:00")
end_time   <- as.POSIXct("2024-01-31 23:45:00")
time <- seq(from = start_time, to = end_time, by = "15 min")

# Number of observations
n <- length(time)

# Monthly warming trend (linear increase)
monthly_trend <- seq(from = 0, to = 10, length.out = n)

# Daily temperature cycle (sine wave, 24-hour period)
daily_cycle <- 5 * sin(2 * pi * as.numeric(difftime(time, start_time, units = "hours")) / 24)

# Random noise
noise <- rnorm(n, mean = 0, sd = 0.5)

# Combine components into temperature values
value <- 15 + monthly_trend + daily_cycle + noise

# Final data frame
df <- data.frame(
  time = time,
  value = value
)
