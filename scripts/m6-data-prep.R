#
# New mod 6
#
#

# spatial data

# join data

df_snotel %>% mutate(Date=as.Date(Date,format="%m/%d/%y")) %>%
  select(Date, contains("Snow.Water")) %>% 
  pivot_longer(cols=!Date,
               names_to = "station", values_to = "swe") %>%
  filter(swe > 0) %>%
  mutate(water_year = cut(Date, breaks =  c(as.Date(c("2020-10-01", "2021-10-01", "2022-10-01")), Inf), 
                          labels = paste0("WY_", c(2021, 2022, 2023)))) %>%
  mutate(station = gsub("\\.", " ", sub("\\.\\..*$", "", station))) %>% 
  # select only dates in april for WY 2022
  mutate(month = (format(Date, "%m"))) %>% 
  filter(month=="04") %>% 
  filter(water_year == "WY_2022") %>% 
  group_by(station) %>%
  summarise(swe_max = max(swe, na.rm=T),
            .groups = "drop") %>% 
  # left_join(df_elev, by=c("station" = "names")) %>% 
  head()


df_snotel %>% mutate(Date=as.Date(Date,format="%m/%d/%y")) %>%
  select(Date, contains("Snow.Water")) %>% 
  pivot_longer(cols=!Date,
               names_to = "station", values_to = "swe") %>%
  filter(swe > 0) %>%
  mutate(water_year = cut(Date, breaks =  c(as.Date(c("2020-10-01", "2021-10-01", "2022-10-01")), Inf), 
                          labels = paste0("WY_", c(2021, 2022, 2023)))) %>%
  mutate(station = gsub("\\.", " ", sub("\\.\\..*$", "", station))) %>% 
  # select only dates in april for WY 2022
  mutate(month = (format(Date, "%m"))) %>% 
  filter(month=="04") %>% 
  filter(water_year == "WY_2023") %>% 
  group_by(station) %>%
  summarise(swe_max = max(swe, na.rm=T),
            .groups = "drop") %>% 
  head()
  # write.csv(., "data/ut_snotel_peakSWE_WY23.csv", row.names = F)

df_snotel %>% mutate(Date=as.Date(Date,format="%m/%d/%y")) %>%
  select(Date, contains("Snow.Water")) %>% 
  pivot_longer(cols=!Date,
               names_to = "station", values_to = "swe") %>%
  filter(swe > 0) %>%
  mutate(water_year = cut(Date, breaks =  c(as.Date(c("2020-10-01", "2021-10-01", "2022-10-01")), Inf), 
                          labels = paste0("WY_", c(2021, 2022, 2023)))) %>%
  mutate(station = gsub("\\.", " ", sub("\\.\\..*$", "", station))) %>% 
  # select only dates in april for WY 2022
  mutate(month = (format(Date, "%m"))) %>% 
  filter(month=="04") %>% 
  # filter(water_year == "WY_2023") %>% 
  group_by(station, water_year) %>%
  summarise(swe_max = max(swe, na.rm=T),
            .groups = "drop") %>% 
  head()
  # write.csv(., "data/ut_snotel_peakSWE_20212023.csv", row.names = F)


# NOW join with metadata for sites
dfmeta <- read.csv("data/UTSNTL_META2.csv")
dfsno <- read.csv("data/ut_snotel_peakSWE_20212023.csv")
head(dfmeta)
dfjo <- merge(dfmeta, dfsno, by.x = "Station.Name", by.y = "station")
head(dfjo)
