
#Where do federal government employees (excluding military personnel and USPS workers) make up the largest share of the 18+ labor force?

# packages ----------------------------------------------------------------

{
  library(tidyverse)
  library(tidycensus)
  library(tigris)
  library(sf)
}


# load --------------------------------------------------------------------
v23 <- load_variables(2023, "acs1", cache = TRUE)

v23 %>%
  filter(name %in% c("B01003_001", "B24080_009","B24080_019"))

raw <- 
  get_acs(
    geography = "county",
    variables = c("B01003_001", "B24080_009","B24080_019"),
    year = 2023,
    survey = "acs1",
    output = "wide", 
    geometry = FALSE
  )

states <- tigris::states(year = 2023, cb = T) %>% 
  filter(STUSPS %in% state.abb) %>% 
  filter(STUSPS %in% c('OH')) %>% 
  shift_geometry()

counties <- tigris::counties(year = 2023, cb = T) %>% 
  filter(STATEFP %in% states$STATEFP) %>% 
  shift_geometry()

# transform ---------------------------------------------------------------

cleaned <- raw %>% 
  group_by(GEOID) %>% 
  summarize(.,
            percentage = sum(B24080_009E, B24080_019E) / B01003_001E
  )
  
cleaned_geo <- counties %>% 
  left_join(., cleaned, join_by(GEOID))

# plot --------------------------------------------------------------------

ggplot() +
  geom_sf(
    data = counties
  ) +a
  geom_sf(
    data = cleaned_geo,
    mapping = aes(
      fill = percentage 
    )
  ) + 
  scale_fill_viridis_b(
    name = NULL, 
    option = 'plasma',
    n.breaks = 6,
    direction = -1,
    labels = scales::label_percent()
  ) +
  labs(
    title = 'Percentage of federal employees by county',
    subtitle = 'Year: 2023, ACS 1 YR, field codes: B01003_001, B24080_009, B24080_019'
    ) +
  theme_void() +
  theme(
    legend.position = "left"
  )
