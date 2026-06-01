# Standalone render of the Africa baseline-WQ map (from RQ3.main.qmd), all 7 projects.
suppressMessages({
  library(data.table); library(ggplot2)
  library(sf); library(rnaturalearth); library(rnaturalearthdata); library(ggspatial)
})
options(timeout = 600)
client <- "64fa7fd74d72d707046be70b19a3963b"
grid <- function(id, cl = client)
  as.data.table(read.csv(
    sprintf("https://api.mwater.co/v3/datagrids/%s/download?client=%s&format=csv", id, cl),
    stringsAsFactors = FALSE, na.strings = c("NA", ""), check.names = TRUE))

# Pull a project's points: find lat/long columns by pattern, return lat/long/Label.
pts <- function(dt, label) {
  nm <- names(dt)
  latc <- nm[grepl("^Latitude", nm, ignore.case = TRUE)][1]
  lonc <- nm[grepl("^Longitude", nm, ignore.case = TRUE)][1]
  if (is.na(latc) || is.na(lonc)) stop(sprintf("no lat/long for %s amongst: %s", label, paste(nm, collapse=", ")))
  d <- dt[, .(Latitude = as.numeric(get(latc)), Longitude = as.numeric(get(lonc)))]
  d[, Label := label]
  d[!is.na(Latitude) & !is.na(Longitude) & abs(Latitude) <= 90 & abs(Longitude) <= 180]
}

map_data <- rbindlist(list(
  pts(grid("d59d21fdd4e5478ebbfb06897d501a33"), "LifeStraw - Kenya"),
  pts(grid("72828d483b4e4ed9ba6c9c578e3709bf"), "Asili - DRC"),
  pts(grid("9613d32ce08f46908b1180b6c8e99e58"), "Amazi Meza - Rwanda"),
  pts(grid("0689d6c952bd4db990adff37927f6448"), "DRIP - Kenya"),
  pts(grid("26cb407edb354a4382ef4f2c828a9f72"), "Helvetas - Madagascar"),
  pts(grid("9675b5ce45e849578014ef9cf09d9b43"), "Amazi Water - Burundi"),
  pts(grid("0fe1a4c959dc49d0b842550dd526c15d"), "Water Mission - Tanzania")
), use.names = TRUE)

# Drop GPS-entry errors that fall outside the East/Central Africa study region
# (a handful of points land in the Atlantic/Arctic and otherwise blow out the map).
n_before <- nrow(map_data)
study_box <- list(lon = c(20, 50), lat = c(-26, 12))
map_data <- map_data[Longitude %between% study_box$lon & Latitude %between% study_box$lat]
cat(sprintf("dropped %d out-of-region points (GPS errors)\n", n_before - nrow(map_data)))
cat("points per project:\n"); print(map_data[, .N, by = Label])

map_sf <- st_as_sf(map_data, coords = c("Longitude","Latitude"), crs = 4326)
africa <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")
cent <- suppressWarnings(st_point_on_surface(africa))
bb <- st_bbox(map_sf); pad <- 2

p <- ggplot(africa) +
  geom_sf(fill = "antiquewhite", color = "gray40") +
  geom_sf(data = map_sf, aes(color = Label), size = 2, alpha = 0.85) +
  geom_sf_text(data = cent, aes(label = name), size = 3, color = "gray20") +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(bb["xmin"]-pad, bb["xmax"]+pad), ylim = c(bb["ymin"]-pad, bb["ymax"]+pad), expand = FALSE) +
  theme_minimal(base_size = 14) +
  labs(title = "Baseline Water Quality Locations", color = "Project/Country")

out <- "/Users/ethomas/Dropbox/Claude/overleaf-6838bd41/africa_water_projects_map.pdf"
ggsave(out, plot = p, width = 10, height = 8, dpi = 300)
cat("WROTE", out, "\n")
