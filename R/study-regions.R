
library(ggfx)
library(ggspatial)
library(glue)
library(here)
library(sf)
library(terra)
library(tidyterra)
library(tidyverse)
library(tigris)

gpkg <- here("data", "community-centers.gpkg")

theme_set(theme_void(12))

region_labels <- c(
  "cmv" = "Central Mesa Verde",
  "nrg" = "Northern Rio Grande",
  "cib" = "Cibola"
)

# spatial data ------------------------------------------------------------

regions <- read_sf(gpkg, layer = "regions") |> st_transform(3857)

# original bbox was warped, adjusting this just for this visualization
regions[["geom"]] <- regions[["geom"]] |> 
  map(st_bbox) |> 
  map(st_as_sfc) |> 
  map(\(x){ x[[1]] }) |> 
  st_sfc(crs = 3857)

sites <- read_sf(gpkg, layer = "sites") |> 
  st_drop_geometry() |> 
  select(region, center, n_room)

four_corners <- states() |> 
  rename_with(tolower) |> 
  filter(name %in% c("Utah", "Colorado", "New Mexico", "Arizona")) |> 
  select(name) |> 
  st_transform(3857)

# total map area ----------------------------------------------------------

# want to make this equal to the aspect ratio of the reveal.js slide
# this includes adding some vertical area so the slide title does
# not overlap with the main areas in the figure

bb8 <- regions |> st_bbox()

midx <- (bb8[["xmax"]] + bb8[["xmin"]])/2
midy <- (bb8[["ymax"]] + bb8[["ymin"]])/2

dy <- (bb8[["ymax"]] - bb8[["ymin"]])

bb8[["ymin"]] <- midy - (dy/2) - (0.08 * dy)
bb8[["ymax"]] <- midy + (dy/2) + (0.12 * dy)

dx <- (1920/1080) * (bb8[["ymax"]] - bb8[["ymin"]])

bb8[["xmin"]] <- midx - (dx/2)
bb8[["xmax"]] <- midx + (dx/2)

remove(midx, midy, dx, dy)

# labels ------------------------------------------------------------------

suppressWarnings({
  
  region_labels_xy <- regions |> 
    st_cast("POINT") |> 
    slice(3, 9, 13) |> 
    st_coordinates() |> 
    as_tibble() |> 
    rename_with(tolower) |> 
    mutate(
      region = names(region_labels),
      label = unname(region_labels)
    )
  
})

y_ut <- four_corners |> 
  filter(name == "Utah") |> 
  st_bbox()

y_ut <- y_ut[["ymin"]]

y_co <- four_corners |> 
  filter(name == "Colorado") |> 
  st_bbox()

y_co <- y_co[["ymin"]]

state_labels <- tibble(
  x = c(bb8[["xmin"]] + c(3500, 3500), bb8[["xmax"]] - c(3500, 3500)), 
  y = c(y_ut + c(3500, -2500), y_co + c(3500, -2500)),
  label = c("UT", "AZ", "CO", "NM"),
  hjust = c(0, 0, 1, 1),
  vjust = c(0, 1, 0, 1)
)

remove(y_ut, y_co)

# region data -------------------------------------------------------------

area <- tibble(
  region = regions[["region"]],
  area = st_area(regions) |> units::set_units(km2) |> units::drop_units()
)

region_data <- sites |> 
  group_by(region) |> 
  summarize(
    n_centers = sum(center),
    n_farms = n() - n_centers,
    n_rooms = sum(n_room)
  ) |> 
  ungroup() |> 
  left_join(area, by = "region") |> 
  left_join(region_labels_xy, by = "region") |> 
  mutate(
    area = round(area, 1),
    y = y - 22000,
    across(
      c(area, n_centers, n_farms, n_rooms), 
      \(x){ format(x, big.mark = ",", scientific = FALSE) }
    ),
    text = glue(
      "Area: {area} sq.km
       Centers: {n_centers}
       Farms: {n_farms}
       Rooms: {n_rooms}"
    )
  ) |> 
  select(region, text, x, y)

remove(area)

# basemap -----------------------------------------------------------------

get_basemap <- function(
    x, 
    map = "World_Imagery", 
    size = c(1920,1080) * 3, 
    dpi = 300,
    imageSR = 3857
) {
  
  x <- st_bbox(x)
  
  old_ratio <- (x[["xmax"]] - x[["xmin"]]) / (x[["ymax"]] - x[["ymin"]])
  new_ratio <- (size[[1]] / size[[2]])
  
  if (!all.equal(old_ratio, new_ratio)) {
    
    msg <- paste0(
      "Extent of image (size) differs from extent of x (bbox). ",
      "Map may be warped."
    )
    
    warning(msg, call. = FALSE)
    
  }
  
  req <- httr2::request("http://services.arcgisonline.com/arcgis/rest/services")
  
  req <- httr2::req_url_path_append(req, map, "MapServer", "export")
  
  req <- httr2::req_url_query(
    req,
    bbox = paste(x, collapse = ","),
    bboxSR = st_crs(x)$epsg,
    imageSR = imageSR,
    format = "png",
    dpi = dpi,
    size = paste(size, collapse = ","),
    pixelType = "U8",
    noDataInterpretation = "esriNoDataMatchAny",
    interpolation = "+RSP_BilinearInterpolation",
    f = "image"
  )
  
  path <- tempfile(fileext = ".png")
  
  resp <- httr2::req_perform(req, path = path)
  
  png::readPNG(path, native = TRUE)
  
}

basemap <- get_basemap(bb8)

remove(get_basemap)

# map ---------------------------------------------------------------------

cover <- st_sym_difference(st_union(regions), st_as_sfc(bb8))

ggplot() +
  annotation_raster(
    basemap,
    bb8[["xmin"]], bb8[["xmax"]],
    bb8[["ymin"]], bb8[["ymax"]]
  ) +
  geom_sf(
    data = cover,
    fill = alpha("white", 0.55)
  ) +
  geom_sf(
    data = four_corners,
    fill = "transparent",
    color = "gray20",
    linewidth = 0.2
  ) +
  geom_sf(
    data = regions,
    color = "black",
    fill = alpha("white", 0.05),
    linewidth = 0.5
  ) +
  with_outer_glow(
    geom_text(
      data = region_labels_xy,
      aes(x, y, label = label),
      color = "black",
      fontface = "bold", 
      size = 16/.pt,
      hjust = c(0, 1, 0),
      vjust = 1,
      nudge_x = c(1, -1, 1) * 5000,
      nudge_y = -1
    ),
    colour = "gray90",
    expand = 2,
    sigma = 2
  ) +
  with_outer_glow(
    geom_text(
      data = region_data,
      aes(x, y, label = text),
      color = "black",
      size = 10/.pt,
      hjust = c(0, 0, 1),
      vjust = 1,
      nudge_x = c(1, 1, -1) * 5000,
      lineheight = 0.9
    ),
    colour = "gray90",
    expand = 2,
    sigma =2
  ) +
  geom_text(
    data = state_labels,
    aes(x, y, label = label),
    color = "gray20",
    size = 10/.pt,
    hjust = state_labels[["hjust"]],
    vjust = state_labels[["vjust"]]
  ) +
  coord_sf(
    crs = 3857,
    datum = 3857,
    xlim = bb8[c("xmin", "xmax")],
    ylim = bb8[c("ymin", "ymax")],
    expand = FALSE
  ) +
  annotation_scale(
    aes(location = "br"),
    pad_x = unit(2.8, "in"),
    pad_y = unit(0.35, "in"),
    text_cex = 0.8
  )

ggsave(
  filename = here("figures", "overview-map.png"),
  width = 10,
  height = 10 * (1080/1920),
  dpi = 300
)
