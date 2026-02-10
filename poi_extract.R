## POI manvipulation

# Virtual environment path
.libPaths("/myriadfs/home/ucfnpje/myRlibs")


# Packages
library(sf)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(tmap)


# Import data
pois <- st_read('/myriadfs/home/ucfnpje/Scratch/enhance/data/en_wales_pois.geojson')

# Create a new column - poi_type
pois_filtered_long <- pois %>%
  select(id, lon, lat, name, amenity, tourism, shop, leisure) %>%
  pivot_longer(
    cols = c(amenity, tourism, shop, leisure),
    names_to = "key",
    values_to = "value",
    values_drop_na = TRUE
  )


# check na values
sum(is.na(pois_filtered_long$value))

# Check geometry types in your POI dataset
table(st_geometry_type(pois_filtered_long))

# to extract centroid, transform crs
pois_filtered_long <- pois_filtered_long %>% st_transform(., 27700)

# Ensure valid geometries
pois_valid <- st_make_valid(pois_filtered_long)


# Convert to centroids for non-points
# Step 1: Separate point and non-point
pois_points <- pois_valid %>% filter(st_geometry_type(.) == "POINT")
pois_lines <- pois_valid %>% filter(st_geometry_type(.) %in% c("LINESTRING", "MULTILINESTRING")) %>%
  mutate(geometry = st_centroid(geometry))


# Filter polygons only
pois_poly <- pois_valid %>%
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))

# Make valid
pois_poly <- st_make_valid(pois_poly)

# Now safely compute centroids
pois_poly <- pois_poly %>%
  mutate(geometry = st_centroid(geometry))

# Step 2: Combine back together
pois_centroided <- bind_rows(pois_points, pois_lines, pois_poly)

pois_centroided <- pois_centroided %>% st_transform(., 4326)


## 2. POI Filtering
# eliminate unimportant/unused amenities
pois <- pois_centroided %>%
  filter(!grepl('disused', value) &
           !grepl('construction', value) &
           !grepl('closed', value) &
           !grepl('parking', value) &
           !grepl('post_box', value) &
           !grepl('bus_stand', value) &
           !grepl('_bin', value) &
           !grepl('waste_basket', value) &
           !grepl('yes', value)&
           !grepl('former', value) &
           !grepl('vacant', value) &
           !grepl('waste', value)&
           !grepl('no', value)&
           !grepl('dead', value)&
           !grepl('bus_stop',value)&
           !grepl('temporary',value)&
           !grepl('letter_box', value))


# 15-minute city categorisation - essential amenities
pois <- pois %>%
  filter(
    # healthcare
    key == 'healthcare' |
      grepl('pharmacy', value) |
      grepl('dentist', value) |
      grepl('health', value) |
      grepl('care', value) |
      grepl('beauty', value) |
      grepl('hospital', value) |
      
      # education
      grepl('university', value) |
      grepl('library', value) |
      grepl('school', value) |
      grepl('arts_centre', value) |
      
      # commerce
      grepl('cloth', value) |
      grepl('store', value) |
      grepl('sport', value) |
      grepl('bank', value) |
      grepl('hair', value) |
      grepl('grocer', value) |
      grepl('grocery', value) |
      grepl('supermarket', value) |
      grepl('convenience', value) |
      grepl('mall', value) |
      grepl('department_store', value) |
      
      # entertainment
      grepl('restaurant', value) |
      grepl('bar', value) |
      grepl('pub', value) |
      grepl('club', value) |
      grepl('cinema', value) |
      grepl('theatre', value) |
      grepl('museum', value) |
      grepl('gallery', value) |
      grepl('cafe', value) |
      grepl('coffee', value) |
      grepl('bakery', value)|
      
      # living
      grepl('post_office', value) |
      grepl('worship', value) |
      
      grepl('religion', value) |
      #grepl('park', value) |
      grepl('police', value) |
      # includes fire station, bus station, bicycle station, charging station
      grepl('_station', value) |
      grepl('community_centre', value) |
      grepl('job_centre', value) |
      grepl('doityourself', value) |
      grepl('social', value) |
      
      # leisure (gardens and parks)
      (key == 'leisure' & (grepl('garden', value) | grepl('park', value)))
  )



# POI 15-minute city categorisation
pois <- pois %>%
  mutate(category = case_when(
    # Healthcare
    key == 'healthcare' | grepl('pharmacy|dentist|health|care|beauty|hospital', value) ~ 'Healthcare',
    
    # Education
    grepl('university|library|school|arts_centre', value) ~ 'Education',
    
    # Commerce
    grepl('grocer|cloth|store|sport|bank|hair|grocery|supermarket|mall|convenience|department_store', value) ~ 'Commerce',
    
    # Entertainment (including leisure: gardens and parks)
    grepl('restaurant|bar|pub|club|cinema|theatre|museum|gallery|cafe|coffee|bakery', value) ~ 'Entertainment',
    key == 'leisure' & grepl('garden|park', value) ~ 'Entertainment',
    
    # Living
    grepl('post_office|worship|religion|police|_station|community_centre|job_centre|doityourself|social', value) ~ 'Living',
    
    # Default case (if no match is found)
    TRUE ~ 'Other'
  ))

# to sf object
pois <- st_as_sf(pois, coords = c('lon', 'lat'), crs = 4326)



saveRDS(pois, '/myriadfs/home/ucfnpje/Scratch/enhance/data/en_wales_pois.rds')

