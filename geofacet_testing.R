# 0. Packages ------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(purrr)
library(geofacet)


# 1. Custom functions ----------------------------------------------------

#' ggsave_all
#'
#' @param filename Filename to save under
#' @param plot GGplot variable or the last plot generated
#' @param specs Dimension
#' @param path Path to file
#' @param ... Other args
#'
#' @return A set of end-use plots at the proper resolution

ggsave_all <- function(
  filename,
  plot = ggplot2::last_plot(),
  specs = NULL,
  path = "output",
  ...
) {
  default_specs <- tibble::tribble(
    ~suffix,
    ~device,
    ~scale,
    ~width,
    ~height,
    ~units,
    ~dpi,
    "_quart_portrait",
    "png",
    1,
    (8.5 - 2) / 2,
    (11 - 2) / 2,
    "in",
    300,
    "_half_portrait",
    "png",
    1,
    8.5 - 2,
    (11 - 2) / 2,
    "in",
    300,
    "_full_portrait",
    "png",
    1,
    8.5 - 2,
    (11 - 2),
    "in",
    300,
    "_full_landscape",
    "png",
    1,
    11 - 2,
    8.5 - 2,
    "in",
    300,
    "_ppt_title_content",
    "png",
    1,
    11.5,
    4.76,
    "in",
    300,
    "_ppt_full_screen",
    "png",
    1,
    13.33,
    7.5,
    "in",
    300,
    "_ppt_two_content",
    "png",
    1,
    5.76,
    4.76,
    "in",
    300
  )

  specs <- if (is.null(specs)) {
    default_specs
  } else {
    specs
  }

  dir.create(path, showWarnings = FALSE, recursive = TRUE)

  specs %>%
    dplyr::mutate(
      filename = file.path(
        path,
        paste0(filename, suffix, ".", device)
      )
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      dpi = if (is.na(as.numeric(dpi))) {
        dpi
      } else {
        as.numeric(dpi)
      }
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(filename, device, width, height, units, dpi) %>%
    purrr::pwalk(
      ggplot2::ggsave,
      plot = plot,
      bg = "white",
      ...
    )
}


# 2. Data -------------------------------------------------------------------

epa_regions_df <- tibble(
  state_name = c(
    # Region 1 (Boston) - 6 entities
    "Connecticut",
    "Maine",
    "Massachusetts",
    "New Hampshire",
    "Rhode Island",
    "Vermont",
    # Region 2 (New York City) - 4 entities
    "New Jersey",
    "New York",
    "Puerto Rico",
    "US Virgin Islands", # Corrected name
    # Region 3 (Philadelphia) - 6 entities
    "Delaware",
    "District of Columbia",
    "Maryland",
    "Pennsylvania",
    "Virginia",
    "West Virginia",
    # Region 4 (Atlanta) - 8 entities
    "Alabama",
    "Florida",
    "Georgia",
    "Kentucky",
    "Mississippi",
    "North Carolina",
    "South Carolina",
    "Tennessee",
    # Region 5 (Chicago) - 6 entities
    "Illinois",
    "Indiana",
    "Michigan",
    "Minnesota",
    "Ohio",
    "Wisconsin",
    # Region 6 (Dallas) - 5 entities
    "Arkansas",
    "Louisiana",
    "New Mexico",
    "Oklahoma",
    "Texas",
    # Region 7 (Kansas City) - 4 entities
    "Iowa",
    "Kansas",
    "Missouri",
    "Nebraska",
    # Region 8 (Denver) - 6 entities
    "Colorado",
    "Montana",
    "North Dakota",
    "South Dakota",
    "Utah",
    "Wyoming",
    # Region 9 (San Francisco) - 7 entities
    "Arizona",
    "California",
    "Hawaii",
    "Nevada",
    "Guam",
    "American Samoa",
    "Northern Mariana Isl", #Islands must use this format....
    # Region 10 (Seattle) - 4 entities
    "Alaska",
    "Idaho",
    "Oregon",
    "Washington"
  ),
  EPA_Region = c(
    rep("Region 1 (Boston)", 6),
    rep("Region 2 (New York City)", 4),
    rep("Region 3 (Philadelphia)", 6),
    rep("Region 4 (Atlanta)", 8),
    rep("Region 5 (Chicago)", 6),
    rep("Region 6 (Dallas)", 5),
    rep("Region 7 (Kansas City)", 4),
    rep("Region 8 (Denver)", 6),
    rep("Region 9 (San Francisco)", 7),
    rep("Region 10 (Seattle)", 4)
  )
)


# 3. Plot ----------------------------------------------------------------

ggplot(epa_regions_df) +
  # Fill the background of each facet with the EPA Region color (the "tile" effect)
  geom_rect(
    aes(fill = EPA_Region),
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf
  ) +

  # Use facet_geo for the geofaceted layout with the specified grid for territories
  facet_geo(~state_name, grid = "us_states_territories_grid2", label = "name") +

  # Customize fill colors: "Paired" is a good qualitative palette
  scale_fill_brewer(palette = "Paired", name = "EPA Region") +

  # Add titles and captions
  labs(
    title = "US States and Territories by EPA Region",
    caption = "Data: EPA.gov. Map: geofacet."
  ) +
  theme_void() + # Minimalist theme
  theme(
    strip.text = element_text(size = 7, face = "bold"), # Labels for each state tile
    legend.position = "right", # Position the legend
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 14,
      margin = margin(b = 10)
    ),
    plot.caption = element_text(
      hjust = 1,
      size = 8,
      color = "gray50",
      margin = margin(t = 10)
    ),
    panel.spacing = unit(0.1, "cm"), # Reduce space between tiles for a more "tiled" look
    plot.background = element_rect(fill = "grey95", color = NA) # Overall plot background
  )


# 4. Save ----------------------------------------------------------------

ggsave_all("epa_reg_gf")
