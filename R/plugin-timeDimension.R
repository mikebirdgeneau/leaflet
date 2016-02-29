leafletTimelineDependencies <- function() {
  list(
    htmltools::htmlDependency(
      "Leaflet-TimeDimension",
      "1.0.1",
      system.file("htmlwidgets/plugins/Leaflet.TimeDimension", package = "leaflet"),
      script = c('leaflet.timedimension.min.js'),
      stylesheet = c('leaflet.timedimension.control.min.css')
    )
  )
}

#' Add Raster with Time Dimension
#' @export
addRasterTime = function(
  map,
  x,
  colors = "Spectral",
  opacity = 1,
  attribution = NULL,
  layerId = NULL,
  group = NULL,
  project = TRUE,
  maxBytes = 4 * 1024 * 1024,
  tz=NULL
) {
  map$dependencies <- c(map$dependencies, leafletTimelineDependencies())

  stopifnot(inherits(x, "RasterBrick"))

  # Get date range from RasterBrick
  times <- as.POSIXct(as.numeric(gsub("X","",names(x))),origin = "1970-01-01",tz=tz)
  if(length(times)>1){
    dt <- mean(diff(times))
    prefix <- ifelse(attr(dt,"units") %in% c("days","weeks","months","years"),"P","PT")
    suffix <- switch(attr(dt,"units"),
                     years = "Y",
                     months = "M",
                     weeks = "W",
                     days = "D",
                     hours = "H",
                     mins = "M",
                     seconds = "S")
    per <- paste0(prefix,dt,suffix)
  } else {
    per <- "PT1H"
  }

  # Set-up Options for TimeDimension
  if (is.null(map$x$options))
    map$x$options <- list()

  map$x$options$timeDimension = TRUE
  map$x$options$timeDimensionOptions = list(timeInterval = paste0(as.Date(range(times)),collapse = "/"), period = per)
  map$x$options$timeDimensionControl = TRUE
  #timeDimension: true,
  #timeDimensionOptions: {
  #  timeInterval: "2014-09-30/2014-10-30",
  #  period: "PT1H"
  #},
  #timeDimensionControl: true,

  if (project) {
    projected <- projectRasterForLeaflet(x)
  } else {
    projected <- x
  }
  bounds <- raster::extent(raster::projectExtent(raster::projectExtent(x, crs = sp::CRS(epsg3857)), crs = sp::CRS(epsg4326)))

  if (!is.function(colors)) {
    colors <- colorNumeric(colors, domain = NULL, na.color = "#00000000", alpha = TRUE)
  }

  tileData <- raster::values(projected) %>% colors() %>% col2rgb(alpha = TRUE) %>% as.raw()
  dim(tileData) <- c(4, ncol(projected), nrow(projected))
  pngData <- png::writePNG(tileData)
  if (length(pngData) > maxBytes) {
    stop("Raster image too large; ", length(pngData), " bytes is greater than maximum ", maxBytes, " bytes")
  }
  encoded <- base64enc::base64encode(pngData)
  uri <- paste0("data:image/png;base64,", encoded)

  latlng <- list(
    list(raster::ymax(bounds), raster::xmin(bounds)),
    list(raster::ymin(bounds), raster::xmax(bounds))
  )

  invokeMethod(map, getMapData(map), "addRasterImage", uri, latlng, opacity, attribution, layerId, group) %>%
    expandLimits(c(raster::ymin(bounds), raster::ymax(bounds)), c(raster::xmin(bounds), raster::xmax(bounds)))
}

