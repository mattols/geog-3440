#
#
#
#

# mod 5

dfs <- read.csv('data/snotel820_timp.csv')

# add date
dfs$date<-as.Date(dfs$date)

# add density
dfs$dens<-(dfs$swe/dfs$depth)*100

head(dfs)

# # # # STATS

####
# 1. assess 
cor(dfs$depth, dfs$swe)
# 0.96
cor(dfs$depth, dfs$dens)
# -0.64
## cor(dfs$swe, dfs$dens)
## -0.43
cor(dfs[!is.na(dfs$t_ave), "depth"], 
    dfs[!is.na(dfs$t_ave), "t_max"])
# -0.20

####
# 2. Density vs temperature
dfpost <- dfs[dfs$date>as.Date("2024-01-10"),]
fit1 <- lm(dens~t_ave, dfpost)
plot(dens~t_ave, dfpost)
abline(fit1)
summary(fit1)
summary(fit1)$r.squared
# 0.73
summary(fit1)$coefficients[2,4]
# 0.00077

####
# 3. Depth vs density
d_d <- diff(dfpost$depth)
d_p <- diff(dfpost$dens)
fit2 <- lm(d_d~d_p)
plot(d_d~d_p)
abline(fit2)
#
summary(fit2)
summary(fit2)$r.squared
# 0.89
summary(fit2)$coefficients[2,4]
# 1.1e-05

# snotelR

# # # CLEANING DATA

dfm <- read.csv("data/messy_state_elev.csv")

# 4. clean data
# gsub("\\.", "\\1", names(dfm))
names(dfm) <- make.unique(gsub("\\..*$", "", names(dfm)))
dfm$Elevation = gsub("\\,","",gsub("\\{\\d\\}","",dfm$Elevation) )
dfm$Elevation.1 = gsub("\\,","",gsub("\\{\\d\\}","",dfm$Elevation.1) )
dfm$Elevation.1 = gsub("Sea [Ll]evel", 0, dfm$Elevation.1)
# elev numeric - remove comma
dfm$Elevation.1 = as.numeric(dfm$Elevation.1)
dfm$Elevation = as.numeric(dfm$Elevation)
# elev range (high - low)
dfm$e_range = dfm$Elevation - dfm$Elevation.1
head(dfm)
summary(dfm)

# 5.
hist(dfm$e_range, breaks = 20)
shapiro.test(dfm$e_range)
# not normally distributed
dfm$State[dfm$e_range > 5280*2]
# 12 total (excluding US)
# [1] "Alaska"        "Arizona"       "California"   
# [4] "Colorado"      "Hawaii"        "Idaho"        
# [7] "Montana"       "Nevada"        "Oregon"       
# [10] "Utah"          "Washington"    "Wyoming"      

# 6.

# see gds-r


plot(ChangePercent20~e_range, dfm)
abline(lm(ChangePercent20~e_range, dfm))
summary(lm(ChangePercent20~e_range, dfm))

plot(ChangePercent20~e_range, dfm2)
abline(lm(ChangePercent20~e_range, dfm2))
summary(lm(ChangePercent20~e_range, dfm2))

plot(ChangePercent10~e_range, dfm2)
abline(lm(ChangePercent10~e_range, dfm2))
summary(lm(ChangePercent10~e_range, dfm2))
