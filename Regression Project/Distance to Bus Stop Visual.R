# Load libraries
library(tidyverse)
library(sf)
library(tigris)
library(paletteer)
library(dplyr)
library(fuzzyjoin)

# Load Census tracts for Lee County
lee_county_tracts <- tracts(state = "FL", county = "Lee", year = 2019, cb = TRUE)

# Load bus stops and tract data
bus_stops <- read.csv("C:/Users/hamidja/OneDrive - Lee County BoCC/Intern Project/bus_stop.csv")
tract_data <- read.csv("C:/Users/hamidja/OneDrive - Lee County BoCC/Intern Project/r_tract.csv")
tract_cordinates <- tract_data[, c("centlat", "centlong", "intptlat", "intptlong", "tract", "distance_from_closest_stop_feet", "poverty_prct")]
missing_tract <- read.csv("C:/Users/hamidja/OneDrive - Lee County BoCC/Intern Project/missing_tract.csv")

# Identify tracts in missing_tract that are not in tract_cordinates
missing_in_cordinates <- missing_tract %>%
  filter(!tract %in% tract_cordinates$tract)

# Combine the original data with the new missing tracts
tract_cordinates <- bind_rows(tract_cordinates, missing_tract)

new_tract_coordinates <- tract_cordinates %>%
  dplyr::select(-intptlat, -intptlong, -distance_from_closest_stop_in_miles)



# Convert tract_cordinates to sf object
tract_cordinates_sf <- st_as_sf(new_tract_coordinates, coords = c("centlong", "centlat"), crs = 4326)
#tract_cordinates_sf <- st_as_sf(tract_cordinates, coords = c("centlat", "intptlong"), crs = 4326)

# Convert bus_stops to sf object
bus_stops_sf <- st_as_sf(bus_stops, coords = c("Longitude", "Latitude"), crs = 4326)

# Check CRS of both datasets
print(st_crs(lee_county_tracts))
print(st_crs(tract_cordinates_sf))

# If they are different, transform tract_cordinates_sf to match lee_county_tracts CRS
if (st_crs(lee_county_tracts) != st_crs(tract_cordinates_sf)) {
  tract_cordinates_sf <- st_transform(tract_cordinates_sf, st_crs(lee_county_tracts))
}

tract_cordinates_sf <- tract_cordinates_sf %>%
  select(tract, distance_from_closest_stop_feet, poverty_prct)

lee_county_tracts <- lee_county_tracts %>%
  rename(tract = NAME)

lee_county_tracts$tract <- trimws(as.character(lee_county_tracts$tract))
tract_cordinates_sf$tract <- trimws(as.character(tract_cordinates_sf$tract))

#TEST
tract_cordinates_sf <- tract_cordinates_sf %>%
  mutate(geometry = st_cast(geometry, "MULTIPOINT"))

#tract_cordinates_sf <- tract_cordinates_sf %>%
#  rename(tract_id = tract)

#lee_county_tracts <- stringdist_left_join(
#  lee_county_tracts,
#  tract_cordinates_sf %>% 
#    st_drop_geometry() %>%
#    select(tract, distance_from_closest_stop_feet),
#  by = "tract",
#  max_dist = 1,
#)

#######CORRECT
# Use the existing distance variable directly in the join
lee_county_tracts <- lee_county_tracts %>%
  st_join(
    tract_cordinates_sf %>% 
      select(tract, distance_from_closest_stop_feet),
    join = function(x, y) st_intersects(st_buffer(x, dist = 10), y, sparse = TRUE)
  )
tract_cordinates_sf <- st_make_valid(tract_cordinates_sf)
lee_county_tracts <- st_make_valid(lee_county_tracts)

# Perform spatial join to combine points with the polygons
#combined_data <- st_join(tract_cordinates_sf, lee_county_tracts, join = st_within)




lee_county_tracts <- lee_county_tracts %>%
  mutate(distance_from_closest_stop_feet = case_when(
    tract.x == "502.05" ~ 2481.6000,
    tract.x == "801" ~ 65049.6000,
    tract.x == "901" ~ 95198.4000,
    tract.x == "402.06" ~ 11985.6000,
    tract.x == "602.01" ~ 950.4000,
    tract.x == "802.04" ~ 45091.2000,
    tract.x == "501.03" ~ 2270.4000,
    tract.x == "19.11" ~ 4857.600,
    TRUE ~ distance_from_closest_stop_feet  # Keep existing values for other tracts
  ))



#
#
##lee_county_tracts <- lee_county_tracts %>%
# full_join(tract_cordinates_sf %>% 
#             select(tract, distance_from_closest_stop_feet), 
#           by = "tract")

# Create the heat map using the existing distance variable
# Define breaks and colors for the gradient

# Calculate miles and remove NA values
#combined_data <- combined_data %>%
#  mutate(miles = distance_from_closest_stop_feet / 5280) %>%
#  filter(!is.na(miles)) # Remove rows with NA in miles

lee_county_tracts <- lee_county_tracts %>%
  mutate(miles = distance_from_closest_stop_feet / 5280) %>%
  filter(!is.na(miles)) # Remove rows with NA in miles

tract_cordinates_sf <- tract_cordinates_sf %>%
  mutate(miles = distance_from_closest_stop_feet / 5280) %>%
  filter(!is.na(miles)) # Remove rows with NA in miles
#lee_county_tracts <- lee_county_tracts %>%
#  group_by(tract.y, miles) %>% # Replace 'tract_id' with your actual identifier column
#  summarise(geometry = st_union(geometry), .groups = 'drop') 
  


# Define breaks and colors
breaks <- c(0, 0.25, 0.50, 0.75, 1, max(lee_county_tracts$miles, na.rm = TRUE))
colors <- c("#1e3e70", "#419c33", "#6bcf00", "#f0b514", "#be533e")

# Create the plot
ggplot(data = lee_county_tracts) +
  geom_sf(aes(fill = cut(miles, breaks = breaks, include.lowest = TRUE)), 
          color = "black", size = 0.1) +
  scale_fill_manual(values = colors,
                    name = "Distance (miles)",
                    labels = c("Within 1/4 Mile", "Within 1/2 Mile", "Within 3/4 Mile", "Within 1 Mile", "Over 1 Mile")) +
  labs(title = "Distance to Closest Bus Stop by Census Tract",
       subtitle = "Lee County, FL") +
  theme_minimal() +
  theme(
    legend.position = c(1, 0.5), # Move legend further to the right
    legend.justification = c("left", "center"), # Align legend to the left
    legend.key.size = unit(0.5, "cm"), # Reduce size of legend keys
    legend.key.width = unit(1, "cm"), # Adjust width of legend keys
    legend.box.margin = margin(10, 10, 10, 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +
  guides(fill = guide_legend(ncol = 1)) # Set legend to be a vertical list

#MERGE FOR NEXT GRAPH

lee_county_tracts <- lee_county_tracts %>%
  st_join(
    tract_cordinates_sf %>% 
      select(poverty_prct),
    join = function(x, y) st_intersects(st_buffer(x, dist = 10), y, sparse = TRUE)
  )

ggplot(data = lee_county_tracts) +
  geom_sf(aes(fill = poverty_prct), 
          color = "black", size = 0.1) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", 
                      name = "Poverty Percentage") +
  labs(title = "Poverty Percentage by Census Tract",
       subtitle = "Lee County, FL") +
  theme_minimal() +
  theme(
    legend.position = c(1, 0.5), # Move legend further to the right
    legend.justification = c("left", "center"), # Align legend to the left
    legend.key.size = unit(0.5, "cm"), # Reduce size of legend keys
    legend.key.width = unit(1, "cm"), # Adjust width of legend keys
    legend.box.margin = margin(10, 10, 10, 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +
  guides(fill = guide_colorbar(ncol = 1))
