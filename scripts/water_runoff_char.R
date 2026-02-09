#
# 
#
#

# Goals
# use USGS dataRetreival
# use snotelR package
# Runoff ratio
# Wolf paper

##### USGS - dataRetreival
# read this:
# https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.html

# https://doi-usgs.github.io/dataRetrieval/
# https://waterdata.usgs.gov/blog/dataretrieval/

# install.packages("dataRetrieval")
library(dataRetrieval)

# locations
# American Fork R.
site_UTAF <- "USGS-10164500" # American Fork
site_UTDC <- "USGS-10165600" # Dry Creek
# Provo R.
site_UTPVU <- "USGS-10154200" # Upper Provo
site_UTPVJ <- "USGS-10155000" # Provo Above Jordanelle (Near Hailstone)
site_UTPVH <- "USGS-10155200" # Provo Heber (Mid Provo)
site_UTPVD <- "USGS-10155500" # Upper Above Deer Creek (Charleston)
site_UTPVL <- "USGS-10163000" # Lower Provo Before Utah Lake
# Spanish Fork R.
site_UTSFC <- "USGS-10150500" # Lower Spanish Fork (Castilla)
site_UTSFD <- "USGS-10149400" # Upper Spanish Fork (Diamond Fork)
site_UTSFC <- "USGS-10149000" # Upper Spanish Fork (Sixth Water)

# Summit Creek (Santaquin)
site_UTSC <- "USGS-10147100" # Summit Creek (2015-12-01 to Present)

# read info
site_Info <- read_waterdata_monitoring_location(site_UTSC)
parameterCd <- "00060" # discharge
# pCode	shortName
# 00060	Discharge [ft3/s]
# 00065	Gage height [ft]
# 00010	Temperature [C]
# 00045	Precipitation [in]
# 00400	pH


# Raw daily data:
flowSC <- read_waterdata_daily(monitoring_location_id = site_UTSC,
                                     parameter_code = parameterCd,
                                     time = c("1980-01-01", "2025-09-30"))
head(flowSC)
summary(flowSC)
plot(value~time, flowSC, type='l')
head(flowSC)
# See how it relates to temperature?

# calculate base flow (calendar year)
library(dplyr)
# 86,400 s/day
annual_volume <- flowSC %>%
  mutate(year = format(time, "%Y")) %>% 
  group_by(year) %>%
  summarise(
    volume_m3 = sum(value * 86400, na.rm = TRUE)
  )
plot(volume_m3~year, annual_volume)

# Or

df$year <- as.integer(format(df$time, "%Y"))
annual_volume <- aggregate(
  value ~ year,
  data = df,
  FUN = function(x) sum(x * 86400, na.rm = TRUE)
)
names(annual_volume)[2] <- "volume_m3"


# DISCOVER
# Annual discharge volume
# Baseflow separation (Lyne–Hollick and Eckhardt)
# Flow duration curves
# Flashiness
# Recession analysis (basic but informative)

# baseflow separation
# lyne-hollick
lyne_hollick <- function(Q, alpha = 0.925, passes = 3) {
  n <- length(Q)
  B <- numeric(n)
  
  for (p in 1:passes) {
    B[1] <- 0
    for (i in 2:n) {
      B[i] <- alpha * B[i - 1] + (1 + alpha) / 2 * (Q[i] - Q[i - 1])
    }
    Q <- Q - B
    Q[Q < 0] <- 0
  }
  return(Q)
}

df$baseflow <- lyne_hollick(df$Q)
df$quickflow <- df$Q - df$baseflow

# eckhardt filter
eckhardt <- function(Q, alpha = 0.98, BFImax = 0.8) {
  n <- length(Q)
  bf <- numeric(n)
  bf[1] <- Q[1]
  
  for (i in 2:n) {
    bf[i] <- ((1 - BFImax) * alpha * bf[i - 1] +
                (1 - alpha) * BFImax * Q[i]) /
      (1 - alpha * BFImax)
    
    bf[i] <- min(bf[i], Q[i])
  }
  return(bf)
}

df$baseflow <- eckhardt(df$Q)


#
# #
#
# # # #
dfsc <- flowSC
dfsc$Q <- dfsc$value
dfsc$year  <- as.integer(format(dfsc$time, "%Y"))
dfsc$month <- as.integer(format(dfsc$time, "%m"))

dfsc$WY <- ifelse(dfsc$month >= 10,
                  dfsc$year + 1,
                  dfsc$year)

dfsc <- dfsc[dfsc$WY >= 2017 & dfsc$WY <= 2025, ]

lyne_hollick <- function(Q, alpha = 0.95, passes = 3) {
  n <- length(Q)
  Qf <- Q
  
  for (p in 1:passes) {
    B <- numeric(n)
    B[1] <- 0
    
    for (i in 2:n) {
      B[i] <- alpha * B[i - 1] +
        (1 + alpha) / 2 * (Qf[i] - Qf[i - 1])
    }
    
    Qf <- Qf - B
    Qf[Qf < 0] <- 0
  }
  
  return(Qf)
}

dfsc$bf_LH <- lyne_hollick(dfsc$Q, alpha = 0.95, passes = 3)
dfsc$qf_LH <- dfsc$Q - dfsc$bf_LH

# Eckert-Hiller
eckhardt <- function(Q, alpha = 0.98, BFImax = 0.75) {
  n <- length(Q)
  bf <- numeric(n)
  bf[1] <- Q[1]
  
  for (i in 2:n) {
    bf[i] <- ((1 - BFImax) * alpha * bf[i - 1] +
                (1 - alpha) * BFImax * Q[i]) /
      (1 - alpha * BFImax)
    
    bf[i] <- min(bf[i], Q[i])
  }
  
  return(bf)
}

dfsc$bf_ECK <- eckhardt(dfsc$Q, alpha = 0.98, BFImax = 0.75)
dfsc$qf_ECK <- dfsc$Q - dfsc$bf_ECK

SECONDS_PER_DAY <- 86400

dfsc$vol_total <- dfsc$Q * SECONDS_PER_DAY
dfsc$vol_LH    <- dfsc$bf_LH * SECONDS_PER_DAY
dfsc$vol_ECK   <- dfsc$bf_ECK * SECONDS_PER_DAY

wy_summary <- aggregate(
  cbind(vol_total, vol_LH, vol_ECK) ~ WY,
  data = dfsc,
  FUN = sum,
  na.rm = TRUE
)

wy_summary$BFI_LH  <- wy_summary$vol_LH  / wy_summary$vol_total
wy_summary$BFI_ECK <- wy_summary$vol_ECK / wy_summary$vol_total

melt_months <- c(3, 4, 5, 6)
spring_summary <- aggregate(
  cbind(vol_total, vol_LH, vol_ECK) ~ WY,
  data = dfsc[dfsc$month %in% melt_months, ],
  FUN = sum,
  na.rm = TRUE
)

spring_summary$BFI_LH_spring  <- spring_summary$vol_LH  / spring_summary$vol_total
spring_summary$BFI_ECK_spring <- spring_summary$vol_ECK / spring_summary$vol_total

wy_plot <- 2022
ix <- dfsc$WY == wy_plot
plot(dfsc$time[ix], dfsc$Q[ix], type = "l",
     xlab = "Date", ylab = "Discharge (cfs)",
     main = paste("WY", wy_plot))
lines(dfsc$time[ix], dfsc$bf_LH[ix], col = "blue", lwd = 2)
lines(dfsc$time[ix], dfsc$bf_ECK[ix], col = "red",  lwd = 2)
legend("topleft",
       legend = c("Total", "LH Baseflow", "Eckhardt Baseflow"),
       col = c("black", "blue", "red"),
       lwd = 2)

plot(dfsc$Q, dfsc$bf_ECK / dfsc$Q,
     xlab = "Discharge (cfs)",
     ylab = "Baseflow Fraction",
     pch = 16, col = rgb(0,0,0,0.2))


# site stats
# discharge_stats <- readNWISstat(
#   siteNumbers = c("02319394"),
#   parameterCd = c("00060"),
#   statReportType = "annual"
# )

# StatCode	shortName
# 00001	Maximum
# 00002	Minimum
# 00003	Mean
# 00008	Median
