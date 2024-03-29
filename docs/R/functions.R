#' Get data from GBIF on cane toads in Australia
#'
#' Uses gbif package to get records by year.
#'
#' @return A tibble with all info returned by gbif::occ_search, plus a column for year
GetCaneToadLocations <- function(country=NULL, stateProvince=NULL) {
  years <- seq(from=1930, to=as.integer(format(Sys.Date(), "%Y")), by=1)
  gbif_by_year <- function(year, country, stateProvince) {
    return(rgbif::occ_search(scientificName = "Rhinella marina", country=country, stateProvince=stateProvince, eventDate=year, limit=199999, hasCoordinate=TRUE)$data)
  }
  locations <- dplyr::bind_rows(sapply(years, gbif_by_year, country=country, stateProvince=stateProvince))
  locations$year <- as.integer(lubridate::year(locations$eventDate))
  return(locations)
}

#' Get map of Australia
GetAustraliaElevations <- function() {
  return(elevations <- raster::getData(name="alt", country='au'))
}

GetMaximumDistancesAndArea <- function(year, locations) {
  locations <- locations[which(locations$year<=year),]
  if(nrow(locations)==0) {
    return(c(year=year, northsouthdistancekm=0, eastwestdistancekm=0, areakm2=0, northsouthdistancemile=0, eastwestdistancemile=0, areami2=0))

  }
  northsouthdistancekm <- max(sp::spDists(x=matrix(c(min(locations$decimalLongitude),max(locations$decimalLongitude),0,0),byrow=FALSE, ncol=2), longlat=TRUE))
  eastwestdistancekm <- max(sp::spDists(x=matrix(c(0, 0, min(locations$decimalLatitude),max(locations$decimalLatitude)),byrow=FALSE, ncol=2), longlat=TRUE))
  northsouthdistancemile <- northsouthdistancekm * 0.621371
  eastwestdistancemile <- eastwestdistancekm * 0.621371
  areakm2 <- GeoRange::CHullAreaEarth(longs=locations$decimalLongitude, lats=locations$decimalLatitude)
  areami2 <- areakm2 * 0.386102
  return(c(year=year, northsouthdistancekm=northsouthdistancekm, eastwestdistancekm=eastwestdistancekm, areakm2=areakm2, northsouthdistancemile=northsouthdistancemile, eastwestdistancemile=eastwestdistancemile, areami2=areami2))
}


RenderAustraliaMap <- function(elevations) {

  elmat <- matrix(raster::extract(elevations, raster::extent(elevations), buffer = 1000), nrow = ncol(elevations), ncol = nrow(elevations))
  ambmat <- ambient_shade(elmat)
  return(elmat %>%
    sphere_shade(texture = "desert") %>%
    add_water(detect_water(elmat), color = "desert") %>%
    add_shadow(ray_shade(elmat, zscale = 3, maxsearch = 300), 0.5) %>%
    add_shadow(ambmat, 0.5) %>%
    plot_3d(elmat, zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800)))
}


#' Add locations to map
#'
AddLocations <- function(elevations, locations, my_plot) {
  plot_object <- geoviz::add_gps_to_rayshader(elevations, lat=locations$decimalLatitude, lon=locations$decimalLongitude, alt=rep(10000, length(locations$decimalLongitude)),  clamp_to_ground=TRUE, as_line=FALSE, zscale=10)
  return(plot_object)
}

#' Download a map image from the ArcGIS REST API using Will Bishop's code
#'
#' This is code taken exactly from https://raw.githubusercontent.com/wcmbishop/rayshader-demo/master/R/map-image-api.R
#' It was written by Will Bishop: https://github.com/wcmbishop
#'
#' @param bbox bounding box coordinates (list of 2 points with long/lat values)
#' @param map_type map type to download - options are World_Street_Map, World_Imagery, World_Topo_Map
#' @param file file path to save to. Default is NULL, which will create a temp file.
#' @param width image width (pixels)
#' @param height image height (pixels)
#' @param sr_bbox Spatial Reference code for bounding box
#'
#' @details This function uses the ArcGIS REST API, specifically the
#' "Execute Web Map Task" task. You can find links below to a web UI for this
#' rest endpoint and API documentation.
#'
#' Web UI: https://utility.arcgisonline.com/arcgis/rest/services/Utilities/PrintingTools/GPServer/Export%20Web%20Map%20Task/execute
#' API docs: https://developers.arcgis.com/rest/services-reference/export-web-map-task.htm
#'
#' @return file path for the downloaded .png map image
#'
#' @examples
#' bbox <- list(
#'   p1 = list(long = -122.522, lat = 37.707),
#'   p2 = list(long = -122.354, lat = 37.84)
#' )
#' image_size <- define_image_size(bbox, 600)
#' overlay_file <- get_arcgis_map_image(bbox, width = image_size$width,
#'                                      height = image_size$height)
#'
get_arcgis_map_image <- function(bbox, map_type = "World_Street_Map", file = NULL,
                          width = 400, height = 400, sr_bbox = 4326) {
  require(httr)
  require(glue)
  require(jsonlite)

  url <- parse_url("https://utility.arcgisonline.com/arcgis/rest/services/Utilities/PrintingTools/GPServer/Export%20Web%20Map%20Task/execute")

  # define JSON query parameter
  web_map_param <- list(
    baseMap = list(
      baseMapLayers = list(
        list(url = jsonlite::unbox(glue("https://services.arcgisonline.com/ArcGIS/rest/services/{map_type}/MapServer",
                                        map_type = map_type)))
      )
    ),
    exportOptions = list(
      outputSize = c(width, height)
    ),
    mapOptions = list(
      extent = list(
        spatialReference = list(wkid = jsonlite::unbox(sr_bbox)),
        xmax = jsonlite::unbox(max(bbox$p1$long, bbox$p2$long)),
        xmin = jsonlite::unbox(min(bbox$p1$long, bbox$p2$long)),
        ymax = jsonlite::unbox(max(bbox$p1$lat, bbox$p2$lat)),
        ymin = jsonlite::unbox(min(bbox$p1$lat, bbox$p2$lat))
      )
    )
  )

  res <- GET(
    url,
    query = list(
      f = "json",
      Format = "PNG32",
      Layout_Template = "MAP_ONLY",
      Web_Map_as_JSON = jsonlite::toJSON(web_map_param))
  )

  if (status_code(res) == 200) {
    body <- content(res, type = "application/json")
    message(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE))
    if (is.null(file))
      file <- tempfile("overlay_img", fileext = ".png")

    img_res <- GET(body$results[[1]]$value$url)
    img_bin <- content(img_res, "raw")
    writeBin(img_bin, file)
    message(paste("image saved to file:", file))
  } else {
    message(res)
  }
  invisible(file)
}
