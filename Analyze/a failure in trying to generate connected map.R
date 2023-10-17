# Load necessary libraries
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)
library(sf)
library(sp)
library(raster)

# Load state boundaries data
states <- getData('GADM', country = 'USA', level = 1)

# Extract state names
state_names <- states$NAME_1

# Create empty adjacency matrix
adj_matrix <- matrix(0, nrow = length(state_names), ncol = length(state_names))
rownames(adj_matrix) <- state_names
colnames(adj_matrix) <- state_names

# Loop through each pair of states
for (i in 1:length(state_names)) {
  for (j in 1:length(state_names)) {
    # Check if states share a border
    if (gTouches(states[i, ], states[j, ])) {
      adj_matrix[i, j] <- 1
    }
  }
}

# Create empty sf data frame for line segments
lines <- st_sf(geometry = st_sfc())

# Loop through each pair of adjacent states
for (i in 1:(length(state_names)-1)) {
  for (j in (i+1):length(state_names)) {
    # Check if states share a border
    if (gTouches(states[i, ], states[j, ])) {
      adj_matrix[i, j] <- 1
      adj_matrix[j, i] <- 1
      
      # Get border between adjacent states
      border <- states[i, ]
      
      # Add border to lines data frame
      lines <- rbind(lines, border)
    }
  }
}


# Get coordinates of line segments
lines_coords <- st_coordinates(lines)

# Plot U.S. map
map <- leaflet() %>%
  setView(lng = -98.58333, lat = 39.83333, zoom = 4) %>%
  addTiles()

# Plot connection lines
for (i in 1:nrow(lines_coords)) {
  map <- map %>%
    addPolylines(
      lng = c(lines_coords[i, "X1"], lines_coords[i, "X2"]),
      lat = c(lines_coords[i, "Y1"], lines_coords[i, "Y2"]),
      color = "blue",
      opacity = 0.6,
      weight = 2
    )
}

# Print map
map