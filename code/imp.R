library(terra)
library(sf)
library(tigris)
library(dplyr)
require(terra)
require(ISOweek)
require(sf)
require(dplyr)
require(tidyverse)
require(tidyterra)
require(raster,quietly = T)
require(sp,quietly=T)
require(rgeos,quietly = T)
require(parallel,quietly = T)
require(stringr,quietly = T)

options(tigris_use_cache = TRUE)

dir.create("remote_sensed_data/IMP", recursive = TRUE, showWarnings = FALSE)

# Tell GDAL/terra how to access the public OSN S3 bucket
Sys.setenv(
  AWS_NO_SIGN_REQUEST = "YES",
  AWS_S3_ENDPOINT = "usgs.osn.mghpcc.org",
  AWS_HTTPS = "YES",
  AWS_VIRTUAL_HOSTING = "FALSE"
)

# Correct Annual NLCD Collection 1 Version 1 path
nlcd_imp_2020_url <- "/vsis3/hytest/nlcd/annual-nlcd-cu-c1v1/mosaic/Annual_NLCD_FctImp_2020_CU_C1V1.tif"

# Load raster from public S3
imp_2020 <- rast(nlcd_imp_2020_url)

# Orange County, Florida boundary
orange_fl <- counties(state = "FL", year = 2020, cb = TRUE) %>%
  filter(NAME == "Orange") %>%
  st_make_valid()

# Project county boundary to match raster
orange_fl_proj <- st_transform(orange_fl, crs(imp_2020))
orange_fl_vect <- vect(orange_fl_proj)

# Crop + mask
orange_imp_2020 <- imp_2020 %>%
  crop(orange_fl_vect) %>%
  mask(orange_fl_vect)

# Optional: remove weird values outside percent range
orange_imp_2020[orange_imp_2020 < 0 | orange_imp_2020 > 100] <- NA

# Save as a single local GeoTIFF
writeRaster(
  orange_imp_2020,
  "code/remote_sensed_data/orange_county_FL_NLCD_FctImp_2020.tif",
  overwrite = TRUE
)

# Check result
orange_imp_2020
plot(orange_imp_2020)
global(orange_imp_2020, "mean", na.rm = TRUE)



# create a dataset with average imp surface at a buffer radius of 100m
# add to mosq_site 

dat_complete <- mosq_site



# pull landcover at 100m radius buffer 

#Get file names for rasters (we only have one raster but this could be repurpsoed for sets of rasters)
SI_files<-list.files("./code/remote_sensed_data",pattern="\\.tif$",full.names = T)

#Get Site locations - For actual sites
sites_raw<-dat_complete%>%
  dplyr::select(site_index,sample_long_dd,sample_lat_dd)%>%
  distinct()%>%
  data.frame(Index=1:dim(.)[1])
sites<-sites_raw%>%
  st_as_sf(coords = c("sample_long_dd", "sample_lat_dd"),
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")%>%
  as_Spatial()%>%
  spTransform(CRSobj = CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))

#For each unique site go through and get LC amounts at various buffers
buff_size<-c(100) #buffer with a radius of 100m
buff_names<-c(".1km")

# Convert sites (sp) -> terra vector once 
sites_v <- terra::vect(sites)

# Load raster
r <- terra::rast(SI_files[1])

# Project sites to match raster CRS
sites_use <- terra::project(sites_v, terra::crs(r))

# Create 100 m buffers around each site
site_buffers <- terra::buffer(sites_use, width = buff_size)

# Extract mean impervious surface per buffer
imp_raw <- terra::extract(
  r,
  site_buffers,
  fun = mean,
  na.rm = TRUE
) %>%
  as.data.frame()

# Rename columns
names(imp_raw)[1] <- "site_index"
names(imp_raw)[2] <- paste0("mean_imp_", buff_names)

# Join back to site info
site_imp <- sites_raw %>%
  left_join(imp_raw, by = "site_index")


save(site_imp, file = "code/site_imp.Rdata") #Save new dataset we will add all the Remote sensed data to








