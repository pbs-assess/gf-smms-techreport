if (!('mssm_loaded' %in% ls())) {
  source(here::here('R', '00-load.R'))
}

sf_use_s2(FALSE)

# --- Setup coastline ---
utm_zone = 9
#bath = c(100, 200, 400, 600, 800, 1000, 1200)
bath = c(100, 200, 500, 1000)
buffer = 0
xlim_ll <- st_bbox(mssm_grid_sf)[c(1, 3)] + buffer
ylim_ll <- st_bbox(mssm_grid_sf)[c(2, 4)] + buffer

mssm_utm <- mssm_grid_sf |> st_transform(crs = 32609)

coast_utm <- gfplot:::load_coastline(xlim_ll, ylim_ll, utm_zone = utm_zone) |>
  st_as_sf(coords = c('X', 'Y'), crs = 32609) |>
  group_by(PID) |>
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") |>
  mutate(geometry = geometry * 1000) |>
  st_set_crs(32609) |>
  st_simplify(dTolerance = 200)

coast_ll <- coast_utm |> st_transform(crs = 4326)

iso_utm <- gfplot:::load_isobath(xlim_ll, ylim_ll, bath = bath,
  utm_zone = utm_zone) |>
  st_as_sf(coords = c('X', 'Y'), crs = 32609) |>
  filter(!is.na(oldPOS)) |> # I think this was some kind of duplicate causing closed loop polygon
  group_by(PID, SID) |>
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("LINESTRING") |>
  ungroup() |>
  mutate(geometry = geometry * 1000) |>
  st_set_crs(32609)

iso_ll <- iso_utm |> st_transform(crs = 4326)

inset <-
ggplot() +
  geom_sf(data = iso_ll, colour = "grey70", alpha = 0.4, linewidth = 0.3) +
  geom_sf(data = coast_ll, lwd = 0.3, fill = "grey85", colour = "grey72") +
  geom_sf(data = mssm_grid_sf, fill = 'black') +
  coord_sf(xlim = c(-128.3, -123.3), ylim = c(48.2, 51), expand = TRUE) +
  theme(axis.text = element_blank())
  #coord_sf(xlim = c(-127.9, -124), ylim = c(48.4, 51))

base_map <- function(xlim = c(-127.5, -125.45), ylim = c(48.6, 49.7)) {
  list(
    geom_sf(data = coast_ll, lwd = 0.3, fill = "grey85", colour = "grey72"),
    geom_sf(data = iso_ll, colour = "grey70", alpha = 0.4, linewidth = 0.3),
    lims(x = xlim, y = ylim)
  )
}

# ---- Compare grid cell size -----
# Make 3x3 km grid ---
# mssm_grid_3km <- pcod_dat |>
#   mk_mssm_grid(grid_spacing = 3000)

# mssm_grid_3km[[1]] |>
#   dplyr::filter(year >= 2009 & year <= 2021) |>
#   dplyr::distinct(X, Y, .keep_all = TRUE)

# mssm_grid_3km[[2]] |>
#   filter(year >= 2009 & year <= 2021) |>
#   distinct(geometry, .keep_all = TRUE) |>
#   ggplot() +
#   geom_sf(aes(fill = year)) +
#   geom_sf(data = mssm_grid_3km[[2]] |> filter(year == 2021), fill = 'pink') +
#   geom_sf(data = pcod_sf |> filter(year == 2021), shape = 21, size = 3, fill = 'white') +
#   geom_sf(data = mssm_grid_3km[[2]] |> filter(year == 2019), colour = 'purple', fill = NA) +
#   geom_sf(data = pcod_sf |> filter(year == 2019), shape = 21, size = 3, fill = 'white')

# mssm_grid_3km[[1]] |>
#   filter(year >= 2009 & year <= 2021) |>
#   dplyr::distinct(X, Y, .keep_all = TRUE) |>
# saveRDS(file.path(grid_dir, 'mssm-grid-3km_2009-2021.rds'))

# Make 2x2 km grid
# mssm_grid_2km <- pcod_dat |>
#   mk_mssm_grid(grid_spacing = 2000)

# km2 <-
#   ggplot(data = mssm_grid_2km[[2]] |> filter(year >= 2009 & year <= 2021)) +
#   base_map() +
#   geom_sf(data = pcod_sf, shape = 1, colour = 'grey50', alpha = 0.8, size = 0.1) +
#   geom_sf(alpha = 0.2) +
#   scale_fill_manual(values = grid_colours)
# km3 <-
#   ggplot(data = mssm_grid_3km[[2]] |> filter(year >= 2009 & year <= 2021)) +
#   base_map() +
#   geom_sf(data = pcod_sf, shape = 1, colour = 'grey50', alpha = 0.8, size = 0.1) +
#   geom_sf(alpha = 0.2) +
#   theme(axis.text.y = element_blank()) +
#   scale_fill_manual(values = grid_colours)
# km2 + km3

# ggsave(filename = file.path(mssm_figs, '2km-3km-grid-comp.png'), width = 6.7, height = 4.6)

# --- Look at spatial distribution of sampling ------------
# Grid from GFBioField
# sgrid <- sf::st_read(file.path(mssm_data, "SMMS-grid/SMMS_Survey_Blocks.shp"))

# gfbio_grid <- sgrid |>
#   # Select only sites off WCVI (since there is no ssid corresponding to 7 or 'MSSM WCVI')
#   filter(GROUPING_C %in% c(112, 113)) |>
#   sf::st_crop(sgrid, sf::st_bbox(c(xmin = -128, ymin = 48.5, xmax = -126, ymax = 50))) %>%
#   sf::st_set_crs('WGS84') %>%
#   mutate(area = units::set_units(sf::st_area(.), km^2))

# --- Grid used in synopsis
# grid_plot <- mssm_grid_sf |>
#   filter(year >= 2009 & year <= 2019) |>
#   distinct(geometry) |>
#   ggplot() +
#     base_map() +
#     geom_sf(data = pcod_sf, colour = 'grey50', shape = 1, alpha = 0, size = 0.1) +
#     geom_sf(aes(fill = '2009'), alpha = 0.5) +
#     scale_fill_manual(values = grid_colours) +
#     labs(fill = "Grid") +
#     theme(legend.position = c(0.8, 0.9)) +
#     theme(axis.text = element_text(size = 6),
#         legend.text = element_text(size = 6),
#         legend.title = element_blank()) +
#     guides(fill = "none")
# grid_plot

# ggsave(file.path(mssm_figs, 'grid-prediction-2009_no-points.png'), width = 3.5, height = 3.7)

# grid_plot_2009_points <- mssm_grid_sf |>
#   filter(year >= 2009 & year <= 2019) |>
#   distinct(geometry) |>
#   ggplot() +
#     base_map() +
#     geom_sf(data = pcod_sf |> filter(year < 2009), shape = 1, colour = 'grey50', alpha = 0.5, size = 0.1) +
#     geom_sf(aes(fill = '2009'), alpha = 0.5) +
#     geom_sf(data = pcod_sf |> filter(year >= 2009 & year <= 2019), shape = 1, colour = 'black', alpha = 0.5, size = 0.1) +
#     scale_fill_manual(values = grid_colours) +
#     labs(fill = "Grid") +
#     theme(legend.position = c(0.8, 0.9))
# grid_plot_2009_points + geom_sf(data = gfbio_grid, alpha = 0, colour = NA) +
#     theme(axis.text = element_text(size = 6),
#         legend.text = element_text(size = 6),
#         legend.title = element_blank()) +
#     guides(fill = "none")

# ggsave(file.path(mssm_figs, 'grid-prediction-2009.png'), width = 4, height = 2.8)
#ggsave(file.path(here::here('report', 'tech-report', 'figure'), 'grid-prediction-2009.png'),
#  width = 3.5, height = 3.7)
#system("optipng -strip all report/tech-report/figure/grid-prediction-2009.png")

# --- Overlay blocks shown/used in GFBioField
# gfbio_field_grid_plot1 <- mssm_grid_sf |>
#   filter(year >= 2009 & year <= 2019) |>
#   distinct(geometry) |>
#   ggplot() +
#   geom_sf(data = pcod_sf, shape = 1, colour = 'grey50', alpha = 0.5, size = 0.1) +
#   geom_sf(aes(fill = '2009'), alpha = 0.7) +
#   geom_sf(data = gfbio_grid, aes(fill = 'GFBioField'), alpha = 0.7) +
#   scale_fill_manual(values = grid_colours) +
#     labs(fill = "Grid") +
#     theme(legend.position = c(0.8, 0.9)) +
#     scale_x_continuous(breaks = seq(-127.4, -126.0, by = 0.4))
# gfbio_field_grid_plot1 +
#   theme(axis.text = element_text(size = 6),
#         legend.text = element_text(size = 6),
#         legend.title = element_blank())

# ggsave(file.path(mssm_figs, 'grid-prediction-gfbiofield-1.png'), width = 3.5, height = 3.7)

# --- Historical survey domain
# The grid was created as any 3x3 km grid cell, that overlapped with at least one
# sampling location.
# The overlay grid covered the bounding box of all sampling locations
df_2022 <- mssm_grid_sf |> filter(year == 2022) |> distinct(geometry)
#df_2023 <- mssm_grid_sf |> filter(year == 2023) |> distinct(geometry)
df_2009_2021 <- mssm_grid_sf |> filter(year >= 2009 & year < 2022) |> distinct(geometry)
loranA <- mssm_grid_sf |> filter(year < 1979) |> distinct(geometry)
loranC <- mssm_grid_sf |> filter(year >= 1979 & year < 1998) |> distinct(geometry)
gps <- mssm_grid_sf |> filter(year >= 1998) |> distinct(geometry)

grid_historical_plot <-
  ggplot() +
    base_map() +
    geom_sf(data = loranA, aes(fill = "1975 (Loran A)"), alpha = 1) +
    geom_sf(data = loranC,
      aes(fill = "1979 (Loran C)"), alpha = 1) +
    geom_sf(data = gps,
      aes(fill = "1998 (GPS)"), alpha = 1) +
    geom_sf(data = df_2009_2021,
      aes(fill = "2009-2021"), alpha = 1) +
    scale_fill_manual(values = grid_colours) +
    scale_colour_manual(values = grid_colours, name = NULL) +
    labs(fill = "Last year sampled") +
    theme(legend.spacing = unit(-85, "pt")) +
    theme(legend.position = c(0.84, 0.815)) +
    theme(axis.title = element_blank()) +
    theme(axis.text = element_text(size = 6),
          legend.text = element_text(size = 6),
          legend.title = element_text(size = 7),
          legend.key.size = unit(0.4, 'cm'))
grid_historical_plot

ggsave(filename = file.path(mssm_figs, 'grid-historical-nav-changes_no-points.png'),
  width = 4, height = 2.8)

# grid_historical_plot +
#   geom_point(data = pcod_dat, aes(x = longitude, y = latitude), shape = 1,
#              size = 0.5, alpha = 0.2)
# ggsave(file.path(mssm_figs, 'grid-historical-nav-changes_points.png'), width = 4, height = 3.8)

spatial_shift_plot <-
  ggplot() +
    geom_sf(data = mssm_grid_sf |>
        dplyr::filter(year >= 2009 & year <= 2021) |>
        distinct(geometry),
      aes(fill = "2009"), alpha = 0.8, colour = 'grey50') +
    geom_point(data = pcod_dat |>
      filter(year %in% c(1975, 1976, 1977, 1978, 1979, 1985, 1995, 1998, 2003, 2013, 2021, 2022)),
      aes(x = longitude, y = latitude), alpha = 1, size = 1, stroke = 0.5, shape = 21, fill = 'white') +
    scale_fill_manual(values = grid_colours) +
    facet_wrap(~ year, nrow = 3) +
    guides(fill = "none") +
    theme(legend.position = c(0.95, 0.95),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 6),
          panel.spacing.y = unit(0, "lines"))
spatial_shift_plot

ggsave(file.path(mssm_figs, 'grid-spatial-sampling-changes.png'), plot = spatial_shift_plot,
  width = 7, height = 7)

# ggsave(file.path(mssm_figs, 'grid-spatial-sampling-changes_wide.png'), plot = spatial_shift_plot,
#   width = 11, height = 7.2)

# Get trawl overlap
# ------------------------------------------------------------------------------
# The get_all_* functions get the start and end lat lon values
# mssm_dat_2024 <- gfdata::get_all_survey_sets(species = get_spp_names()$species_common_name, ssid = c(6, 7))
# saveRDS(mssm_dat_2024, "~/R_DFO/gfsynopsis/report/scratch-out/mssm-dat-2024-10.RDS")
new_dat <- readRDS("~/R_DFO/gfsynopsis/report/scratch-out/mssm-dat-2024-10.RDS") |>
  filter(species_common_name == "pacific cod") |>
  filter(grouping_desc %in% c("WCVI Shrimp Survey Area 124", "WCVI Shrimp Survey Area 125"))

# Get trawl polygons
d <- new_dat |>
  select(species_common_name, fishing_event_id, year, latitude, longitude, latitude_end, longitude_end, doorspread_m) |>
  drop_na(latitude_end)

# Create LINESTRING geometries
create_linestring <- function(lon, lat, lon_end, lat_end) {
  st_linestring(matrix(c(lon, lat, lon_end, lat_end), ncol = 2, byrow = TRUE))
}

line_geoms <- mapply(
  create_linestring,
  d$longitude, d$latitude, d$longitude_end, d$latitude_end,
  SIMPLIFY = FALSE
)
sf_lines <- st_sfc(line_geoms, crs = 4326) |>
  st_transform(crs = 32630)

# sf_polygons <- st_buffer(sf_lines, dist = 30) # Buffer to typical trawl width
sf_polygons <- st_buffer(sf_lines, dist = d$doorspread_m) # Buffer to each doorspread
d_sf <- st_sf(d, geometry = sf_polygons)

# Define a function to calculate the average proportion of overlapping area in a 5-year window
get_overlap_df <- function(dat) {
  multipolygons_by_year <- dat |>
    group_by(year) |>
    summarize(geometry = st_union(geometry)) |>
    ungroup() |>
    st_make_valid()


  slices <- split(multipolygons_by_year, multipolygons_by_year$year)
  slice_combinations <- combn(slices, 2, simplify = FALSE)

  # must calculate each intersection because of the st_intersection and geometrycollection shenanigans
  overlaps <- lapply(slice_combinations, function(pair) {
    st_intersection(pair[[1]], pair[[2]])
  })

  # Combine all overlaps into a single sf object
  overlap <- do.call(bind_rows, overlaps)
}

get_areas <- function(d_sf, start_year, end_year) {
  polygons_window <- d_sf |>
    filter(year >= start_year & year <= end_year)

  all_df <- polygons_window %>%
    mutate(area_m2 = st_area(.))

  overlap_df <- get_overlap_df(polygons_window) %>%
    mutate(area_m2 = st_area(.))

  all_area <- sum(all_df$area_m2)
  overlap_area <- sum(overlap_df$area_m2)

  summary_df <- tibble(
    start_year = start_year,
    end_year = end_year,
    total_area = all_area,
    overlap_area = overlap_area,
    prop_overlap = overlap_area / total_area
  )
}

# Apply the function in rolling 5-year windows
start_year <- min(d_sf$year)
end_year <- max(d_sf$year)
rolling_windows <- seq(start_year, end_year - 10)

prop_overlap <- purrr::map_dfr(rolling_windows, function(start) {
  message(start)
  end <- start + 9
  get_areas(d_sf, start, end)
})

ggplot(data = prop_overlap, aes(x = start_year, y = units::drop_units(prop_overlap))) +
  geom_point() +
  labs(x = "Start year of 10-year rolling window", y = "Proportion of trawl overlap")
ggsave(file.path(mssm_figs, 'sampling-overlap-10year-window.png'), width = 5, height = 4)
