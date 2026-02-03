

dfu <- read.csv("~/data/sundance/UVU-Sundance-20250130-20250508.csv")

# convert datetime
dfu$Date_Time <- as.POSIXct(substr(dfu$Date_Time, 1, 19), "%Y-%m-%dT%H:%M:%OS", tz="UTC")

# Convert to MT ????

plot(snow_depth_mm~Date_Time, dfu[295:2099,], type='l')
abline(v=as.POSIXct("2025-04-01"), col='red')
abline(v=as.POSIXct("2025-03-26"), col='red')
abline(v=as.POSIXct("2025-03-19"), col='red')

plot(snow_depth_mm~Date_Time, dfu[1900:2099,], type='l')


library(dplyr);library(ggplot2)

# plot daily averages in depth
dfu %>% 
  mutate(date = as.Date(Date_Time)) %>% 
  filter(date > as.Date("2025-02-22")) %>% 
  mutate(depth_cm = snow_depth_mm/10) %>% 
  group_by(date) %>% 
  summarize(air_temp_C = mean(air_temp_C),
            net_solar_Wm2 = mean(solar_radiation_wm2 * (1 - (outgoing_radiation_sw_wm2/solar_radiation_wm2))),
            snow_depth_cm = mean(depth_cm, na.rm=T)) %>%
  ggplot(aes(date, snow_depth_cm)) +
  geom_line()

# plot averages in 
dfu %>% 
  mutate(date = as.Date(Date_Time)) %>% 
  filter(date > as.Date("2025-02-22")) %>% 
  mutate(depth_cm = snow_depth_mm/10) %>% 
  filter(solar_radiation_wm2>0) %>% 
  filter(solar_radiation_wm2>outgoing_radiation_sw_wm2) %>% 
  group_by(date) %>% 
  summarize(air_temp_C = mean(air_temp_C),
            net_solar_Wm2 = mean(solar_radiation_wm2 * (1 - (outgoing_radiation_sw_wm2/solar_radiation_wm2))),
            snow_depth_cm = mean(depth_cm, na.rm=T)) %>%
  ggplot() +
  geom_line(aes(date, net_solar_Wm2), col='orange') +
  geom_line(aes(date, air_temp_C*15), col='firebrick') +
  scale_y_continuous(
    name = "Net Solar Radiation", # primary
    sec.axis = sec_axis( # secondary
      trans = ~ . / 15, 
      name = "Air Temp (C)"
    )
  ) +
  theme_classic()

# sub-hourly
dfu %>%
  filter(as.Date(Date_Time) > as.Date("2025-02-22")) %>% 
  filter(solar_radiation_wm2>outgoing_radiation_sw_wm2) %>% 
  mutate(net_solar = solar_radiation_wm2 * (1 - (outgoing_radiation_sw_wm2/solar_radiation_wm2))) %>% 
  ggplot() +
  geom_line(aes(Date_Time, net_solar), col='orange') +
  geom_line(aes(Date_Time, air_temp_C*20), col='firebrick') +
  scale_y_continuous(
    name = "Net Solar Radiation", # primary
    sec.axis = sec_axis( # secondary
      trans = ~ . / 20, 
      name = "Air Temp (C)"
    )
  ) +
  theme_classic()


# Question 3330
df_day = dfu %>% 
  mutate(date = as.Date(Date_Time)) %>% 
  filter(date > as.Date("2025-02-22")) %>% 
  # mutate(depth_cm = snow_depth_mm/10) %>%
  mutate(depth_cm = snow_depth_mm) %>%
  filter(solar_radiation_wm2>0) %>% 
  filter(solar_radiation_wm2>outgoing_radiation_sw_wm2) %>% 
  group_by(date) %>% 
  summarize(air_temp = mean(air_temp_C),
            sw_in = mean(solar_radiation_wm2),
            net_solar = mean(solar_radiation_wm2 * (1 - (outgoing_radiation_sw_wm2/solar_radiation_wm2))),
            depth = mean(depth_cm, na.rm=T),
            alb = mean((outgoing_radiation_sw_wm2/solar_radiation_wm2)))

head(df_day)
plot(alb~date, df_day, type='l')
df_day[df_day$date==as.Date("2025-04-01"),]
df_day[df_day$date==as.Date("2025-03-26"),]

# April 1
# ~200 Wm2
# depth 107.2 cm
# ~13 hrs (3600 sec per hr)
# 334 KJ/KG (Enthalpy of Fusion)

((200*3600*13)/1000 ) / 334 # convert to time, convert to KJ divide by enthalpy of fusion
# 1 day of melt = 28.02395 kg (mm)
# 1kg per cubic meter = 1000 ml (or 1 mm)
1072/28.02395

as.Date("2025-04-01") + 38.253
# "2025-05-09"
# Close! 

# more accurate
start_date = as.Date("2025-04-01")
start_depth = df_day[df_day$date==start_date, "depth"]
# need SWE
start_swe = start_depth * 0.65
ave_sol = mean(df_day$net_solar[df_day$date>=start_date], na.rm=T)

day_melt_mm = ((ave_sol*3600*13)/1000 ) / 334
# calculate days
start_swe / day_melt_mm
# 18.9 days @ 45% dens
# 23 days @ 55%
# 27.3 @ 65%
as.Date("2025-04-01") + 27


# solar rad (Apr 6)
# Total sol
183*3600*13 # 200Wm2 all day
# vs
dfu %>% 
  filter(as.Date(Date_Time) == as.Date("2025-04-06")) %>%
  filter(solar_radiation_wm2>outgoing_radiation_sw_wm2) %>% 
  summarize(tot_sw_in = sum(solar_radiation_wm2*3600),
            tot_sol = sum(solar_radiation_wm2*3600 * (1 - (outgoing_radiation_sw_wm2/solar_radiation_wm2))))

# 
# 9360000 # 200 Wm2 estimate on Apr 6
# 8517600 # 182 Wm2
# 8506872 # calculated

mean(df_day$net_solar[df_day$date>=start_date], na.rm=T)
mean(df_day$sw_in[df_day$date>=start_date], na.rm=T)
mean(df_day$alb[df_day$date>=start_date], na.rm=T)


468*(1-0.61)

468 - 285
448 - 285
343 - 285

((183*3600*13)/1000 / 334)
# days
(1072* 0.4) / ((183*3600*13)/1000 / 334)


df_day[df_day$date==as.Date("2025-03-19"), "depth"]
