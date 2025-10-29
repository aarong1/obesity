
# packages
library(readr)
library(dplyr)
library(sf)

#------------------------------------------------------------
# 1) Read CSV (tab or space separated OK; set delim as needed)
#------------------------------------------------------------
# Example assumes a plain whitespace or tab delimiter shown in your paste.
# If it's comma-separated, switch to read_csv and remove delim.
library(readxl)

csv <- read_excel("data/geography-census-2021-population-weighted-centroids-for-data-zones-and-super-data-zones.xlsx")

#------------------------------------------------------------
# 2) Helper to guess CRS (29902 vs 27700) and transform to WGS84
#------------------------------------------------------------
to_wgs84 <- function(df_xy, x_col = "X", y_col = "Y") {
  stopifnot(all(c(x_col, y_col) %in% names(df_xy)))
  
  # Try Irish Grid (EPSG:29902), then BNG (EPSG:27700)
  try_crs <- function(epsg) {
    st_as_sf(df_xy, coords = c(x_col, y_col), crs = epsg) |>
      st_transform(4326)
  }
  
  cand_29902 <- try_crs(29902)  # Irish Grid TM65
  cand_27700 <- try_crs(27700)  # British National Grid
  
  # NI-ish sanity window (lon between -11 and -5; lat between 53 and 56.5)
  in_ni <- function(pts) {
    bb <- st_bbox(pts)
    bb["xmin"] >= -11 & bb["xmax"] <= -5 & bb["ymin"] >= 53 & bb["ymax"] <= 56.5
  }
  
  if (in_ni(cand_29902)) {
    cand_29902
  } else if (in_ni(cand_27700)) {
    cand_27700
  } else {
    # Fallback: default to 29902 but warn you
    warning("Neither 29902 nor 27700 produced coords within NI-ish bounds; defaulting to EPSG:29902.")
    cand_29902
  }
}

# Convert to WGS84 (auto-detect between EPSG:29902 and EPSG:27700)
csv_pts_wgs84 <- to_wgs84(csv)


# Example values you gave:
bbox_list <- list(
  north = 56.0965,
  east  = -5.139928,
  south = 53.52065,
  west  = -10.26505
)

bbox_poly <- st_as_sfc(
  st_bbox(c(
    xmin = bbox_list$west,
    ymin = bbox_list$south,
    xmax = bbox_list$east,
    ymax = bbox_list$north
  ), crs = 4326)
)

#------------------------------------------------------------
# 5) Flag points inside the bbox and filter them
#------------------------------------------------------------
inside_mat <- st_within(csv_pts_wgs84, bbox_poly, sparse = FALSE)
csv_pts_wgs84 <- csv_pts_wgs84 |>
  mutate(in_bbox = as.logical(inside_mat[, 1]))

csv_in_bbox <- csv_pts_wgs84 |>
  filter(in_bbox)

# Quick peek
csv_pts_wgs84
csv_in_bbox
