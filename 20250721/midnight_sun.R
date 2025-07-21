# Load necessary libraries
library(ggplot2)
library(dplyr)
library(ambient)
library(ggforce)
library(purrr)
library(ggnewscale) # Load the new package

# Set a seed for reproducibility
set.seed(123)

# --- 1. Define Parameters (No Changes) ---
n_rays <- 300
n_dashed_rays <- 60
thick_linewidth <- 2.0
thin_linewidth <- 1.4
bg_color <- "#00303E"
fg_color <- "#FED967"
sm_fill_color <-  "#00222C"
rays_color <- "#A59E67"
x_lim <- c(0, 1)
y_lim <- c(0, 1.2)
aspect_ratio <- (y_lim[2] - y_lim[1]) / (x_lim[2] - x_lim[1])
n_steps <- 250
step_length <- 0.005
wobble_freq <- 25
wobble_strength <- 1.0
n_circles <- 100
min_circle_alpha <- 0.60  # How transparent the most see-through circle is
max_circle_alpha <- 0.98  # How opaque the least see-through circle is
min_radius <- 0.02
max_radius <- 0.20
big_circle_data <- tibble(
  x0 = 0.3,
  y0 = 0.65,
  r = 0.15
)

# --- 2. Ray Tracing Function (No Changes) ---
trace_ray <- function(id, source_circle, n_steps, step_length, wobble_freq, wobble_strength) {
  sc_x <- source_circle$x0; sc_y <- source_circle$y0; sc_r <- source_circle$r
  angle <- runif(1, 0, 2 * pi)
  x <- numeric(n_steps); y <- numeric(n_steps)
  x[1] <- sc_x + sc_r * cos(angle); y[1] <- sc_y + sc_r * sin(angle)
  
  for (i in 2:n_steps) {
    radial_x <- x[i-1] - sc_x; radial_y <- y[i-1] - sc_y
    dist_from_center <- sqrt(radial_x^2 + radial_y^2)
    if (dist_from_center == 0) dist_from_center <- 1e-6
    radial_unit_x <- radial_x / dist_from_center; radial_unit_y <- radial_y / dist_from_center
    perp_unit_x <- -radial_unit_y; perp_unit_y <- radial_unit_x
    
    # --- THIS IS THE ONLY LINE THAT CHANGES ---
    # Instead of noise, we use a pure sin() wave based on distance.
    noise_val <- sin(dist_from_center * wobble_freq)
    
    wobble_effect <- noise_val * dist_from_center * wobble_strength
    final_vec_x <- radial_unit_x + (perp_unit_x * wobble_effect); final_vec_y <- radial_unit_y + (perp_unit_y * wobble_effect)
    final_norm <- sqrt(final_vec_x^2 + final_vec_y^2)
    if (final_norm == 0) final_norm <- 1
    x[i] <- x[i-1] + (final_vec_x / final_norm) * step_length; y[i] <- y[i-1] + (final_vec_y / final_norm) * step_length
    if(is.na(x[i]) || x[i] < x_lim[1] || x[i] > x_lim[2] || y[i] < y_lim[1] || y[i] > y_lim[2]) {
      x <- x[1:(i-1)]; y <- y[1:(i-1)]; break
    }
  }
  tibble(x = x, y = y, id = id)
}

# --- 3. Generate Data (No Changes) ---
all_rays_data <- map_df(1:n_rays, ~trace_ray(
  .x, big_circle_data, n_steps, step_length, wobble_freq, wobble_strength
)) %>%
  group_by(id) %>% filter(n() > 1) %>% ungroup() %>%
  mutate(
    ray_linetype = ifelse(id <= n_dashed_rays, "dashed", "solid"),
    ray_linewidth = ifelse(id <= n_dashed_rays, thick_linewidth, thin_linewidth)
  )

circles_data <- tibble(
  x0 = runif(n_circles, x_lim[1] - max_radius, x_lim[2] + max_radius),
  y0 = runif(n_circles, y_lim[1] - max_radius, y_lim[2] + max_radius),
  r = min_radius + (max_radius - min_radius) * rbeta(n_circles, 1, 5),
	alpha = runif(n_circles, min_circle_alpha, max_circle_alpha)
)
all_circles_data <- bind_rows(
  circles_data %>% mutate(type = "small"),
  big_circle_data %>% mutate(type = "big") 
)

# --- 4. Build the Plot (CORRECTED with ggnewscale) ---

p <- ggplot() +
  
  # Layer 1: The rays, using the first set of linewidth/linetype scales.
  geom_path(
    data = all_rays_data,
    aes(x = x, y = y, group = id, linetype = ray_linetype, linewidth = ray_linewidth),
    color = rays_color,
    alpha = 0.65
  ) +
  scale_linetype_manual(values = c("dashed" = "dashed", "solid" = "solid"), guide = "none") +
  scale_linewidth_identity(guide = "none") +
  
  # Layer 2: The dark bubble fills.
  geom_circle(
    data = circles_data,
    aes(x0 = x0, y0 = y0, r = r, alpha = alpha),
    fill = sm_fill_color,
    color = NA
  ) +
	scale_alpha_identity(guide = "none") +

  # --- RESET THE LINEWIDTH SCALE ---
  # This tells ggplot to forget the previous linewidth scale and start a new one.
  new_scale("linewidth") +

  # Layer 3: The circle outlines, using a NEW linewidth scale.
  geom_circle(
    data = circles_data,
    aes(x0 = x0, y0 = y0, r = r),
    color = fg_color,
		linewidth = 0.9
  ) +
  
  # This scale now applies ONLY to the geom_circle layer above.
  # The incorrect 'new_scale' argument is removed.
  #scale_linewidth_manual(values = c("big" = 0.8, "small" = 0.5), guide = "none") +
	
	# Layer 4: The solid sun fill.
  geom_circle(
    data = big_circle_data,
    aes(x0 = x0, y0 = y0, r = r),
    fill = fg_color,
    color = NA
  ) +
	
	# Layer 5: The solid sun border.
  geom_circle(
    data = big_circle_data,
    aes(x0 = x0, y0 = y0, r = r),
    fill = NA,
    color = bg_color,
		linewidth = 1.1
  ) +
  
  # --- Final Theming ---
  coord_fixed(ratio = 1, xlim = x_lim, ylim = y_lim, expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_color, color = NA),
    panel.background = element_rect(fill = bg_color, color = NA),
    plot.margin = margin(25, 25, 25, 25)
  )

print(p)

output_width_inches <- 10
output_height_inches <- output_width_inches * aspect_ratio

ggsave(
	here(
		'output',
		"midnight_sun.png"
	),
  plot = p,
  width = output_width_inches,
  height = output_height_inches,
  units = "in",
  dpi = 300
)
