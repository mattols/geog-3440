#
# rsat test
# search and download satellite imagery
#

# read
# https://cran.r-project.org/web/packages/rsat/vignettes/rsat1_search.html
# https://ropensci.r-universe.dev/rsat

# load
library(rsat)
# //olsonmeu
# usgs - ManofWar0920
# scihub - Sentinel2@uvu
# earthdata - matthew.olson@geog.utah.edu - Televator9

# set_credentials("rsat.package","UpnaSSG.2021", "scihub")
# In connection$username(user, credential) : Api not supported.
set_credentials("matthew.olson@geog.utah.edu","Televator9", "earthdata")

print_credentials()

# search dataset
data("ex.navarre")
toi <- as.Date("2021-01-01") + 0:15
rcd <- rsat_search(region = ex.navarre,
                   product = c("mod09ga"),
                   dates = toi)

# search different products
rcd <- rsat_search(region = ex.navarre,
                   product = c("mod09ga", "SY_2_SYN___"),
                   dates = toi)
class(rcd)

unique(sat_name(rcd))
names(rcd)[1]
unique(dates(rcd))

# subset by type 
mod.rcd <- subset(rcd, "sat", "Modis")
sn3.rcd <- subset(rcd, "sat", "Sentinel-3")
length(mod.rcd)
length(sn3.rcd)

# filter for same dates between
mod.mtch <- mod.rcd[dates(mod.rcd) %in% dates(sn3.rcd)]
sn3.mtch <- sn3.rcd[dates(sn3.rcd) %in% dates(mod.rcd)]
rcd <- c(mod.mtch, sn3.mtch)

# coerce to dataframe
rcd.df <- as.data.frame(rcd)
dim(rcd.df)
names(rcd.df)
head(rcd.df)

# preview
prview <- plot(rcd[1:12])
prview
rcd <- rcd[-9]

plot(rcd[1:6],
     region = ex.navarre,
     tm.polygon.region.border.col = "red",
     tm.polygon.region.alpha = 0,
     compass.rm = T,
     scale.bar.rm = T)




# DOWNLOAD
# https://cran.r-project.org/web/packages/rsat/vignettes/rsat2_download.html
library(sf)

ip <- st_sf(st_as_sfc(st_bbox(c(
  xmin = -111.57,
  xmax =  -111.54,
  ymin = 40.55,
  ymax = 40.57 
), crs = 4326)))
toi <- seq(as.Date("2024-02-01"),as.Date("2024-02-05"),1)

# folder locations
db.path <- file.path(tempdir(),"database")
ds.path <- file.path(tempdir(),"datasets")
dir.create(db.path)
dir.create(ds.path)

ut_test <- new_rtoi(name = "ut_test",
                     region = ip,
                     db_path = db.path,
                     rtoi_path = ds.path)

rsat_search(region = ut_test, product = c("mod09ga"), dates = toi)
# LANDSAT_8_C1

rsat_download(ut_test)
# BUG????

list.files(get_database(filomena), recursive = TRUE)

rsat_download(records(filomena), out.dir = get_database(filomena))





# # # # # #
# https://stackoverflow.com/questions/78808743/downloading-sentinel-2-with-rsat-package-in-r-produces-error-argument-espa-ord

# Source - https://stackoverflow.com/a/78850566
# Posted by Sean McKenzie, modified by community. See post 'Timeline' for change history
# Retrieved 2026-02-27, License - CC BY-SA 4.0

rsat_download2<-function(x, db_path, verbose = FALSE, parallel=FALSE, ...) {
  require(rsat)
  
  args <- list(...)
  
  if (missing(db_path)){
    db_path <- get_database(x)
    if(db_path==""){
      stop("db_path or global environment database needed for image downloading.")
    }
  }
  
  # filter records
  x<-records(x)
  dataspace <- x[get_api_name(x)%in%"dataspace"]
  usgs <- x[get_api_name(x)%in%"usgs"]
  lpdaac <- x[get_api_name(x)%in%"lpdaac"]
  
  # run download
  if(parallel){
    functions_list <- list(
      list(func = connection$getApi("lpdaac")$download_lpdaac_records,
           args = list(lpdaac_records=lpdaac,db_path=db_path,verbose=verbose,...)),
      list(func = rsat:::connection$getApi("dataspace")$dataspace_download_records,
           args = list(records=dataspace,db_path=db_path,verbose=verbose,...)),
      list(func = connection$getApi("usgs")$espa_order_and_download,
           args = list(usgs=usgs,db_path=db_path,verbose=verbose,...))
    )
    null.list <-mclapply(functions_list, function(entry) {
      do.call(entry$func, entry$args)
    }, mc.cores = 3)
  }else{
    functions_list <- list(
      list(func = rsat:::connection$getApi("usgs")$order_usgs_records,
           args = list(espa_orders=usgs,db_path=db_path,verbose=verbose,...)),
      list(func = rsat:::connection$getApi("lpdaac")$download_lpdaac_records,
           args = list(lpdaac_records=lpdaac,db_path=db_path,verbose=verbose,...)),
      list(func = rsat:::connection$getApi("dataspace")$dataspace_download_records,
           args = list(records=dataspace,db_path=db_path,verbose=verbose,...)),
      list(func = rsat:::connection$getApi("usgs")$download_espa_orders,
           args = list(espa.orders=usgs,db_path=db_path,verbose=verbose,...))
    )
    null.list <- lapply(functions_list, function(entry) {
      do.call(entry$func, entry$args)
    })
  }
}


# try again
rsat_download2(ut_test)
ut_test

list.files(get_database(ut_test), recursive = TRUE)

# vis
library(terra)
library(gdalUtilities)
hdf_file <- list.files(get_database(ut_test), 
                       recursive = TRUE, full.names = T)[1]
sds <- get_subdatasets(hdf_file)
print(sds) 
im1 <- rast()
