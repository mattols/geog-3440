#
# Runoff efficiency
#
# # # # #

library(dplyr)
library(terra)

waf <- vect("~/Downloads/af-watershed.geojson")
plot(waf)
expanse(waf) # sq meters
# 132080255

# RE = sum(Qt - Qb) /P

baseflow <- af %>% filter(WY>2014) %>% 
  group_by(month, WY) %>% 
  summarize(mcfs=mean(value, rm.na=T), sdcfs=sd(value)) %>% 
  filter(month%in%1:2) %>% 
  ungroup() %>% group_by(WY) %>% 
  summarize(winter_base = mean(mcfs), sd_base = sd(sdcfs)) %>% 
  rename(year = WY, baseflow=winter_base) %>% 
  select(year, baseflow) %>% as.data.frame() %>% 
  select(-geometry)


volume_by_year <- af %>%
  rename(discharge = value) %>% 
  mutate(calyear = as.numeric(format(time, "%Y"))) %>%  
  mutate(month = as.numeric(format(time, "%m"))) %>%  
  mutate(year = ifelse(month>=10, calyear+1, calyear)) %>% 
  filter(year > 2014) %>% 
  left_join(baseflow, by = "year") %>%       
  mutate(
    excess_q = pmax(discharge - baseflow, 0), # flow above baseflow only
    excess_volume = excess_q * 0.0283168 * 86400           # ft³/day (seconds in a day)
  ) %>%
  group_by(year) %>%
  summarise(
    annual_excess_volume = sum(excess_volume, na.rm = TRUE),
    total_volume = sum(discharge * 0.0283168 * 86400 , na.rm=TRUE)
  ) %>% as.data.frame() %>% 
  select(-geometry)

library(ggplot2)
volume_by_year %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y=annual_excess_volume)) +
  geom_line(aes(y=total_volume), col='blue') 
  

# Basic P
site_820 %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(calyear = as.numeric(format(date, "%Y"))) %>%  
  mutate(month = as.numeric(format(date, "%m"))) %>%  
  mutate(year = ifelse(month>=10, calyear+1, calyear)) %>% 
  filter(year > 2014 & year < 2026) %>%
  group_by(year) %>% 
  summarize(P = max(snow_water_equivalent)/1000 * 132080255) %>% # max swe x area
  left_join(volume_by_year, by = "year") %>%   
  rename(Q = annual_excess_volume) %>% 
  # P in cubic meters
  # Q in cf
  
  mutate(RE_ = Q / P,
         RE = total_volume / P)

# lag

# #
# # # # # #
#

# Upper Provo
wup <- vect("~/Downloads/upper-provo-watershed.geojson")
plot(wup)
expanse(wup)
# 444075286
# 162 sq miles - 419,578,073.87 m3
419578073.87/1000^3

# Trial lake snotel max (in)
dfst <- data.frame(
  years = 2015:2025,
  swe_max = c(15.4, 23.4,45.1,20.8,36.0,26.2,17.6,22.6,40.4,29.3,25.1)
)
# convert to meters and combine by area
# inches -> m
dfst$bas_swe <- dfst$swe_max * 0.0254 * expanse(wup)

df1 <- read.csv("~/Downloads/usgs-10154200-upv-stats-20152025.csv")
head(df1)



# extract year
yr <- 2025
# first day of each month
start_month <- as.Date(paste0(yr, "-", sprintf("%02d", 1:12), "-01"))
# first day of next month
start_next <- c(start_month[-1],
                as.Date(paste0(yr + 1, "-01-01")))
# number of days in each month
days_in_month <- as.integer(start_next - start_month)
# result
days_in_month
names(days_in_month) <- month.abb
days_in_month

# for all
daymonth_mat <- matrix(rep(days_in_month, 11), ncol=12, byrow = T)
colnames(daymonth_mat) <- month.abb
# hack for adjusting February dates from 2024 onward for 10yrs
# daymonth_mat[,2] = daymonth_mat[,2] + rep(c(0,rep(-1,3)),4)[1:10]
daymonth_mat[,2] = daymonth_mat[,2] + rep(c(0,1,0,0),4)[1:11]


# multiply
# convert cubic ft to meters and seconds per day = 86,400
# monthQ <- df1[2,2:13] * 0.0283168 * 86400 * days_in_month
monthQ <- df1[2,2:13] * 0.0283168 * 86400 * daymonth_mat[2,]

sum(monthQ) / dfst$bas_swe[10]
# 0.6315456
  
# all
# df1[,2:13] * 0.0283168 * 86400 * days_in_month
month_allQ <- df1[,2:13] * 0.0283168 * 86400 * daymonth_mat
month_allQ[1,12] <- month_allQ[2,12]

# annual swe
dfswe_sort <- dfst[order(dfst$years, decreasing = T),]

re = rowSums(month_allQ, na.rm=T) / dfswe_sort$bas_swe

dfswe_sort$swe_norm = (dfswe_sort$swe_max - min(dfswe_sort$swe_max)) / 
  (max(dfswe_sort$swe_max) - min(dfswe_sort$swe_max))

plot(re~dfswe_sort$years, type='l', ylim=c(0,1))
lines(swe_norm~years, dfswe_sort, col='lightblue')

#
#
# # # # # # #
# MORE EXACT

dfspv <- read_waterdata_daily(monitoring_location_id = "USGS-10154200",
                               parameter_code = "00060",
                               time = c("1980-01-01", "2025-09-30"))
head(dfspv)
summary(dfspv)
plot(value~time, dfspv, type='l')

# extract peak runoff amount and date
dfspv$year <- format(as.Date(dfspv$time), "%Y")
idx <- tapply(seq_len(nrow(dfspv)), dfspv$year,
              function(i) i[which.max(dfspv$value[i])])
tail(dfspv[unlist(idx), c("year", "time", "value")], 3)



dfspv$Q <- dfspv$value
dfspv$year  <- as.integer(format(dfspv$time, "%Y"))
dfspv$month <- as.integer(format(dfspv$time, "%m"))

dfspv$WY <- ifelse(dfspv$month >= 10,
                  dfspv$year + 1,
                  dfspv$year)

# dfspv <- dfspv[dfspv$WY >= 2015 & dfspv$WY <= 2025, ]


# Q TOTALS
# df$year <- as.integer(format(df$time, "%Y"))
dfQ <- aggregate(
  # value ~ WY,
  value ~ year,
  data = dfspv,
  FUN = function(x) sum(x * 86400, na.rm = TRUE)
)
names(dfQ) <- c("year","discharge")
# cubic meters

# download snotel
# snotel_download(site_id = 828,
#                 path = "~/data/ut-wtp/other",
#                 internal = F)

snopv <- read.csv("~/data/ut-wtp/other/snotel_828.csv")
head(snopv)

snopv$date <- as.Date(snopv$date)
snopv$cyear  <- as.integer(format(snopv$date, "%Y"))
snopv$month <- as.integer(format(snopv$date, "%m"))

snopv$year <- ifelse(snopv$month >= 10,
                     snopv$cyear + 1,
                     snopv$cyear)

plot(snow_water_equivalent~date, snopv, type='l')


# P
dfP <- aggregate(
  snow_water_equivalent ~ year,
  data = snopv,
  # FUN = function(x) max(x, na.rm = TRUE)/1000 * 444075286
  FUN = function(x) max(x, na.rm = TRUE)/1000 * 419578073.87
)
names(dfP) <- c("year","prec")
plot(prec~year, dfP, type='l')

head(dfP)
head(dfQ)

dfP %>% left_join(dfQ, by="year") %>% 
  mutate(re = discharge / prec) %>% 
  ggplot(aes(year, re)) + 
  geom_line()

# range is 

