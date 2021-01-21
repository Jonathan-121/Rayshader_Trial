# To install the latest version from Github:
# install.packages("devtools")
# install.packages('magick')
# install.packages('leaflet')
# devtools::install_github("tylermorganwall/rayshader")



# Starting a custom map of Brussels

library(leaflet)

# define bounding box with longitude/latitude coordinates
bbox <- list(
  p1 = list(long = 4.365, lat = 50.814),
  p2 = list(long = 4.399, lat = 50.796)
)

leaflet() %>%
  addTiles() %>%
  addRectangles(
    lng1 = bbox$p1$long, lat1 = bbox$p1$lat,
    lng2 = bbox$p2$long, lat2 = bbox$p2$lat,
    fillColor = "transparent"
  ) %>%
  fitBounds(
    lng1 = bbox$p1$long, lat1 = bbox$p1$lat,
    lng2 = bbox$p2$long, lat2 = bbox$p2$lat,
  )

#' Define image size variables from the given bounding box coordinates.
#'
#' @param bbox bounding box coordinates (list of 2 points with long/lat values)
#' @param major_dim major image dimension, in pixels. 
#'                  Default is 400 (meaning larger dimension will be 400 pixels)
#'
#' @return list with items "width", "height", and "size" (string of format "<width>,<height>")
#'
#' @examples
#' bbox <- list(
#'   p1 = list(long = -122.522, lat = 37.707),
#'   p2 = list(long = -122.354, lat = 37.84)
#' )
#' image_size <- define_image_size(bbox, 600)
#' 
define_image_size <- function(bbox, major_dim = 400) {
  # calculate aspect ration (width/height) from lat/long bounding box
  aspect_ratio <- abs((bbox$p1$long - bbox$p2$long) / (bbox$p1$lat - bbox$p2$lat))
  # define dimensions
  img_width <- ifelse(aspect_ratio > 1, major_dim, major_dim*aspect_ratio) %>% round()
  img_height <- ifelse(aspect_ratio < 1, major_dim, major_dim/aspect_ratio) %>% round()
  size_str <- paste(img_width, img_height, sep = ",")
  list(height = img_height, width = img_width, size = size_str)
}

image_size <- define_image_size(bbox, major_dim = 600)

#' Download USGS elevation data from the ArcGIS REST API.
#'
#' @param bbox bounding box coordinates (list of 2 points with long/lat values)
#' @param size image size as a string with format "<width>,<height>"
#' @param file file path to save to. Default is NULL, which will create a temp file.
#' @param sr_bbox Spatial Reference code for bounding box
#' @param sr_image Spatial Reference code for elevation image
#'
#' @details This function uses the ArcGIS REST API, specifically the
#' exportImage task. You can find links below to a web UI for this
#' rest endpoint and API documentation.
#'
#' Web UI: https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer/exportImage
#' API docs: https://developers.arcgis.com/rest/services-reference/export-image.htm
#'
#' @return file path for downloaded elevation .tif file. This can be read with
#' \code{read_elevation_file()}.
#'
#' @examples
#' bbox <- list(
#'   p1 = list(long = -122.522, lat = 37.707),
#'   p2 = list(long = -122.354, lat = 37.84)
#' )
#' image_size <- define_image_size(bbox, 600)
#' elev_file <- get_usgs_elevation_data(bbox, size = image_size$size)
#'
get_usgs_elevation_data <- function(bbox, size = "400,400", file = NULL,
                                    sr_bbox = 4326, sr_image = 4326) {
  require(httr)

  # TODO - validate inputs

  url <- parse_url("https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer/exportImage")
  res <- GET(
    url,
    query = list(
      bbox = paste(bbox$p1$long, bbox$p1$lat, bbox$p2$long, bbox$p2$lat,
                   sep = ","),
      bboxSR = sr_bbox,
      imageSR = sr_image,
      size = size,
      format = "tiff",
      pixelType = "F32",
      noDataInterpretation = "esriNoDataMatchAny",
      interpolation = "+RSP_BilinearInterpolation",
      f = "json"
    )
  )

  if (status_code(res) == 200) {
    body <- content(res, type = "application/json")
    # TODO - check that bbox values are correct
    # message(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE))

    img_res <- GET(body$href)
    img_bin <- content(img_res, "raw")
    if (is.null(file))
      file <- tempfile("elev_matrix", fileext = ".tif")
    writeBin(img_bin, file)
    message(paste("image saved to file:", file))
  } else {
    warning(res)
  }
  invisible(file)
}

# download elevation data
elev_file <- file.path("data", "brussels.tif")
get_usgs_elevation_data(bbox, size = image_size$size, file = elev_file,
                        sr_bbox = 4326, sr_image = 4326)

################
# NEED TO TROUBLESHOOT LINE 137 to 139

# proposed by r2evans on S.O: 
# chek if 
# file.exists(elev_file)
# >>> Error in file.exists(elev_file) : object 'elev_file' not found

################

#' Read an elevation image file (like a .tif) into 
#' an elevation matrix.
#'
#' @param file file path
#'
#' @return A two-dimensional matrix, where each entry in the matrix is the
#'  elevation at that point.
#'
read_elevation_file <- function(file) {
  elev_img <- raster::raster(file)
  elev_matrix <- matrix(
    raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
    nrow = ncol(elev_img), ncol = nrow(elev_img)
  )
  elev_matrix
}

# load elevation data
elev_img <- raster::raster(elev_file)
elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
  nrow = ncol(elev_img), ncol = nrow(elev_img)
)

# calculate rayshader layers
ambmat <- ambient_shade(elev_matrix, zscale = 30)
raymat <- ray_shade(elev_matrix, zscale = 30, lambert = TRUE)
watermap <- detect_water(elev_matrix)

# plot 2D
elev_matrix %>%
  sphere_shade(texture = "imhof4") %>%
  add_water(watermap, color = "imhof4") %>%
  add_shadow(raymat, max_darken = 0.5) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  plot_map()





