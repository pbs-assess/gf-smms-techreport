library(dplyr)
library(ggplot2)
library(sf)

spp <- readRDS(here::here("data-outputs", "smms-spp.rds"))

# On DFO network:
# smms_surveys <- try({gfdata::get_all_survey_sets(spp$species_code, ssid = c(6, 7))})
# saveRDS(smms_surveys, here::here("data", "all-surveys-smms.rds"))

survey <- readRDS(here::here("data", "all-surveys-smms.rds")) |>
  filter(usability_desc == "FULLY USABLE")

survey_sf <- survey |>
  distinct(fishing_event_id, .keep_all = TRUE) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

g <- ggplot(
  survey_sf |>
    filter(!is.na(grouping_code)) |>
    mutate(label = gsub("Shrimp Survey Area ", "", grouping_desc))
  ) +
  geom_sf(aes(colour = label)) +
  geom_sf(data = pacea::bc_coast) +
  coord_sf(xlim = c(-129.5, -124), ylim = c(48.25, 52.25)) +
  ggokabeito::scale_colour_okabe_ito(order = c(1:3, 5:7)) +
  labs(colour = "Survey area") +
  gfplot::theme_pbs(base_size = 12) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.15, 0.22),
        legend.key.height = unit(1.15, 'lines')) +
  guides(colour = guide_legend(override.aes = list(size = 2)))
g
ggsave(filename = here::here("figure", "survey-area-map.png"), width = 6, height = 5.5)

# g + facet_wrap(~ year)
