# ─────────────────────────────────────────────────────────────────────────────
# 1.  SET-UP — Load packages
# ─────────────────────────────────────────────────────────────────────────────
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  osmdata, tidyverse, terra, sf,
  ggspatial, rayshader
)

# ─────────────────────────────────────────────────────────────────────────────
# 2.  GET PROVINCE OUTLINE FROM OSM
# ─────────────────────────────────────────────────────────────────────────────

# Use admin level boundaries to get Northern Province (admin_level=4 for provinces)
query <- opq("Sri Lanka") |>
  add_osm_feature(key = "admin_level", value = "4") |>
  add_osm_feature(key = "name", value = "Northern Province")

osm_data <- osmdata_sf(query)

# Extract the polygon safely
if (!is.null(osm_data$osm_multipolygons) && nrow(osm_data$osm_multipolygons) > 0) {
  province_poly <- osm_data$osm_multipolygons
} else if (!is.null(osm_data$osm_polygons) && nrow(osm_data$osm_polygons) > 0) {
  province_poly <- osm_data$osm_polygons
} else {
  stop("❌ No polygon found for Northern Province.")
}

plot(st_geometry(province_poly), main = "Northern Province Boundary")

# ─────────────────────────────────────────────────────────────────────────────
# 3.  LOAD AND MASK TREE COVER RASTER
# ─────────────────────────────────────────────────────────────────────────────

tree_cover_raw <- rast('TreeCover_Northern.tif')
tree_cover_masked <- terra::mask(tree_cover_raw, vect(province_poly))
tree_cover_proj <- terra::project(tree_cover_masked, 'EPSG:4326')

# ─────────────────────────────────────────────────────────────────────────────
# 4.  DOWNSAMPLE + CONVERT TO DATAFRAME
# ─────────────────────────────────────────────────────────────────────────────

tree_cover_lowres <- terra::aggregate(tree_cover_proj, fact = 4)  # reduce resolution
tree_cover_df <- as.data.frame(tree_cover_lowres, xy = TRUE, na.rm = TRUE)
colnames(tree_cover_df)[3] <- 'percent_cover'

# ─────────────────────────────────────────────────────────────────────────────
# 5.  GGPLOT — 2D MAP
# ─────────────────────────────────────────────────────────────────────────────

cols <- rev(c("#003724", "#405f35", "#606C38", "#97A664", "#D4A373", "#D0C78D", "#F3EFDE"))
min_val <- 0
max_val <- round(max(tree_cover_df$percent_cover), 1)
pal <- colorRampPalette(cols)(8)
breaks <- seq(min_val, max_val, by = 0.1)

theme_map <- function() {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 14, hjust = 0.5),
      legend.position = c(.1, .2),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )
}

p <- ggplot(tree_cover_df) +
  geom_raster(aes(x = x, y = y, fill = percent_cover)) +
  scale_fill_gradientn(colors = pal, breaks = breaks, limits = c(min_val, max_val),
                       name = "Probability of full\ntree cover") +
  annotation_scale(location = "bl", width_hint = .25, plot_unit = "m") +
  annotation_north_arrow(location = "tr", style = north_arrow_orienteering) +
  coord_equal() +
  labs(
    title = "3D Vegetation Cover Map — Northern Province-Sri Lanka (2024)",
    caption = "Prepared by Lavanya Baskaran | Data: Google Dynamic World 2024"
  ) +
  theme_map()

ggsave("northern_tree_cover_2d.png", width = 7.5, height = 7, dpi = 600, bg = "white", plot = p)

# ─────────────────────────────────────────────────────────────────────────────
# 6.  RAYSHADER — 3D RENDER
# ─────────────────────────────────────────────────────────────────────────────

plot_gg(
  ggobj = p,
  width = 7.5, height = 7,
  windowsize = c(750, 700),
  scale = 75, shadow = FALSE,
  zoom = .68, phi = 89, theta = 0
)

# Optional: HDRI lighting
url <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/snow_field_4k.hdr"
hdri_file <- basename(url)
download.file(url, destfile = hdri_file, mode = "wb")

render_highquality(
  filename = "northern_tree_cover_3d.png",
  preview = TRUE,
  light = FALSE,
  environment_light = hdri_file,
  intensity_env = 1.75,
  interactive = FALSE,
  parallel = TRUE,
  width = 750 * 3, height = 700 * 3
)
