# !formatR
library(leaflet)
library(ncdf4)
library(raster)

# Load data for RasterBlock
tmp <- tempfile(fileext=".nc")
curl::curl_download("https://s3-us-west-2.amazonaws.com/mikebirdgeneau.public/ec_hrdps_000.nc",destfile = tmp)
model <- nc_open(tmp,readunlim=FALSE)
x <- as.vector(ncvar_get(model, "longitude"))-247
y <- as.vector(ncvar_get(model, "latitude"))
r <- brick(tmp,varname="LAND_surface")

extent(r) <- c(range(x),range(y))
crs(r) <- sp::CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=NAD83 +towgs84=0,0,0")
#crs(r) <- sp::CRS("+proj=longlat +units=m")

leaflet() %>% addProviderTiles(provider = "Esri.WorldPhysical") %>% addRasterTime(r,opacity = 0.5,project = TRUE)
