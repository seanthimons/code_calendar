# Color Palette Generation using the "randomcoloR" package

# Install the package (if you haven't already)
# install.packages("randomcoloR")

# Load the package
library(randomcoloR)

# Specify the number of distinct colors you want
# Generate a vector of distinct colors in hexadecimal format
distinct_colors <- distinctColorPalette(k = length(unique(water_quality$swim_site)))

pie(rep(1, num_colors), col=distinct_colors)

# Print the resulting vector of hex codes
print(distinct_colors)

# Example of using the function in ggplot2
library(ggplot2)

# Create a sample dataset
df <- data.frame(
  x = 1:num_colors,
  y = runif(num_colors)
)

# Create a bar plot with the randomly selected colors
ggplot(df, aes(x = x, y = y)) +
  geom_bar(stat = "identity", fill = distinct_colors) +
  scale_x_continuous(breaks = 1:num_colors) +
  labs(title = "Bar Plot with Random Colors",
       x = "Category",
       y = "Value")
