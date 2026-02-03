#
# UT DEM ideas
#
#

# Estimate the Bonneville shoreline down to great salt lake area 
# including into idaho
# surface elevation
mtn <- vect('~/ut-wtp/shp/GMBA_Inventory_v2.0_standard_basic/GMBA_Inventory_v2.0_standard_basic.shp')
wasatch <- mtn[grep("Wasatch", mtn$MapName), ]
lon_lat <- crds(centroids(terra::aggregate(wasatch)))
# 3s ~ 90 meters c(-115, -110, 40, 45) - misses lower end
dem <- elevation_3s(lon_lat[1], lon_lat[2], path=tempdir())
dem2 <- elevation_3s(lon_lat[1], lon_lat[2]-2, path=tempdir())
elv <- terra::merge(dem, dem2)
plot(crop(dem, wasatch[c(2,4),]))
plot(wasatch[c(2,4),], add=T, border='white')

# Bonneville
# highest 5100ft
# 5100/3.281 = 1554
plot(elv < 1550)
maps::map('state', c("Utah", 'Idaho', "Nevada"), add=T, col='white')
# Provo around 350ft lower
plot(elv < 1450)
# Stansbury between 1347 and 1378 meters (4420-4520 ft)
plot(elv < 1350)
maps::map('state', c("Utah", 'Idaho'), add=T, col='white')

# Current? SLC Airport is 1288 m
plot(elv < 1288)
maps::map('state', c("Utah", 'Idaho', "Nevada"), add=T, col='white')
# Utah lake elvation 1368
plot(elv < 1390) # higher than SLC

