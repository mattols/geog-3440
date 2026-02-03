#
# Snotel data
#
# # # # # # # #

# library(tidyverse)
# https://cran.r-project.org/web/packages/snotelr/vignettes/snotelr-vignette.html
# install.packages("snotelr")
library(snotelr)
library(ggplot2)

# download and list site information
site_meta_data <- snotel_info()
head(site_meta_data)

site_meta_data[site_meta_data$state=="UT",
                    c("site_name","start", "site_id")]

snotel_download(site_id = 820,
                path = "~/data/ut-wtp/other",
                internal = F)

site_820 <- read.csv("~/data/ut-wtp/other/snotel_820.csv")
head(site_820)

library(dplyr)

site_820 %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date >= as.Date("2020-01-01")) %>% 
  ggplot( aes(x = date, y = snow_water_equivalent)) + 
  # ylim(c(0,100)) +
  geom_line()

# march accumulation in mm
site_820 %>% 
  filter(as.Date(date) >= as.Date("2025-03-01") &
           as.Date(date) <= as.Date("2025-03-31")) %>% 
  mutate(swe_diff = c(diff(snow_water_equivalent), 0) ) %>% 
  filter(swe_diff > 0) %>% 
  summarise(swe_accum_mm = sum(swe_diff))

# 149 mm in March 2025

# PRISM?
prec_prism03 <- rast("~/data/ut-wtp/prism/prism_ppt_us_30s_202503/prism_ppt_us_30s_202503.tif")
ppc <- crop(prec_prism03, wasatch)
plot(ppc)


snopt <- vect(cbind(site_820$longitude[1], 
           site_820$latitude[1]),
     crs="epsg:4326")

plot(snopt, add=T, col='red')

buf <- buffer(snopt, 1e4) # in meters
plot(buf, add=T, border='green')

plot(crop(ppc, buf))
plot(snopt, add=T, col='red', pch=3)
extract(ppc, snopt)
# 143.421 mm

## SUMMARY
# summary - March 2025 Precip 
# comparison at Timp Divide (820)
# ERA-5 68.1 mm
# PRISM 143.421 mm
# Snotel (truth) 149 mm 

# create gridded 
grd_era <- as.polygons(crop(monthly_snow*1000, 
                            wasatch[wasatch$MapName=="Central Wasatch Range",]),
                       aggregate=F)

# PLOT COMPARISON
# ERA5
par(mfrow=c(1,3))
plot(crop(monthly_snow*1000, 
          wasatch[wasatch$MapName=="Central Wasatch Range",],
          mask=T))
plot(snopt, add=T, col='red', pch=3)
plot(wasatch[wasatch$MapName=="Central Wasatch Range",], 
     border='white', add=T)
plot(grd_era, add=T)
# PRISM
plot(crop(ppc, 
          wasatch[wasatch$MapName=="Central Wasatch Range",]))
plot(snopt, add=T, col='red', pch=3)
plot(wasatch[wasatch$MapName=="Central Wasatch Range",], 
     border='white', add=T)
plot(grd_era, add=T)
# zonal statistics of PRISM (based on ERA 5 domain)
ppc_zone <- zonal(ppc, grd_era, fun="mean", as.polygons=T)
plot(crop(ppc_zone, 
          ext(wasatch[wasatch$MapName=="Central Wasatch Range",])),
     "prism_ppt_us_30s_202503")
plot(snopt, add=T, col='red', pch=3)
plot(wasatch[wasatch$MapName=="Central Wasatch Range",], 
     border='white', add=T)
plot(grd_era, add=T)

extract(ppc_zone, snopt)
# ERA-5         PRISM - resampled
# 68.12006      108.5403

# still 40mm short

prism_res <- rasterize()


# # # # # # # #
# snow phenology
# calculate snow phenology
phenology <- snotel_phenology(site_820)

# subset data to the first decade of the century
snow_data_subset <- subset(site_820, as.Date(date) > as.Date("2000-01-01") &
                             as.Date(date) < as.Date("2010-01-01"))

# plot the snow water equivalent time series
plot(as.Date(snow_data_subset$date),
     snow_data_subset$snow_water_equivalent,
     type = "l",
     xlab = "Date",
     ylab = "SWE (mm)"
)

# plot the dates of first snow accumulation as a red dot
points(phenology$first_snow_acc,
       rep(1,nrow(phenology)),
       col = "red",
       pch = 19,
       cex = 0.5
)

head(phenology)

plot(last_snow_melt_doy~year, data=phenology)
plot(first_snow_melt_doy~year, data=phenology)
plot(first_snow_acc_doy~year, data=phenology)
plot(max_swe~year, data=phenology)
plot(max_swe_doy~year, data=phenology)


site_820 %>% 
  mutate(date = as.Date(date)) %>%
  filter(!is.na(temperature_mean)) %>% 
  ggplot(aes(date, temperature_max)) +
  geom_point()

# linear regression
site_820 %>% 
  filter(!is.na(temperature_mean)) %>%
  mutate(date = as.Date(date)) %>%
  mutate(months = format(date, "%m")) %>% 
  filter(months%in%c(1:5, 10:12)) %>% 
  mutate(year = as.numeric(format(date, "%Y"))) %>% 
  group_by(year) %>% 
  summarize(tmax = mean(temperature_max)) %>% 
  ggplot(aes(year, tmax)) +
  ggtitle("Average maximum winter recorded temperature") +
  ylab("Temperature (C°)") + xlab("") +
  geom_point() +
  geom_smooth(method="lm", formula = 'y ~ x', 
              se=FALSE, color='firebrick', linetype = 'dashed') +
  theme_classic()
  
# significant increase in max temperature?
  
  
# statistical relationships
# use broom