# Cycle Network Categorisation

# Virtual environment path
.libPaths("/myriadfs/home/ucfnpje/myRlibs")



# 0. Library ###################################################################
library(sf)
library(dplyr)
library(tidyverse)
library(geodist)
library(tmap)


gc()

# 1. Set-up ###################################################################

# Select your city of interest
city <- 'en_wales'


# Road data import
edges <- st_read(paste0('/myriadfs/home/ucfnpje/Scratch/enhance/data/roads/', city, '_raw_nx.gpkg'), layer = 'edges') %>% 
  st_transform(27700)
nodes <- st_read(paste0('/myriadfs/home/ucfnpje/Scratch/enhance/data/roads/', city, '_raw_nx.gpkg'), layer = 'nodes') %>% 
  st_transform(27700)




gc()

unique(edges$highway)

# Filter most useful highway types
edges_filtered <- edges %>%
  filter(highway %in% 
           c("residential",  "living_street", 'track', 'cycleway', 'path', "footway", 
             'pedestrian', "unclassified", "tertiary", "service", "tertiary_link",
             "trunk", 'trunk_link', "primary", "primary_link", "secondary", "secondary_link",
             "steps", "bridleway"))


rm(edges)
gc()




# Find unique values in the 'highway' column
print(unique(edges_filtered$highway))

# bicycle column values
print(unique(edges_filtered$bicycle))
print(unique(edges_filtered$cycleway))

# 2. Cycleway Categorisation ###################################################
## 2.0. Non-cyclable Path ######################################################
edges_filtered <- edges_filtered %>%
  filter(!bicycle %in% c('no') |
           !cycleway %in% c('no', 'none')) %>%
  distinct()


# The following link is useful to refer to: https://wiki.openstreetmap.org/wiki/Bicycle#Off-road_and_outdoor


## 2.1. Protected Cycle Lane ###################################################

#***********************
#* 2.1.1. Cycle Tracks *
#***********************

##************##
## Definition ##
##************##

# A cycle track is separated from the road by curbs, parking lots, grass or trees, 
# but is running parallel and next to the road. 
# With cycleway=track, cycle paths are mapped together with the properties of the roads 
# at the line of the road (highway=*) (mapping together).


##********##
## Method ##
##********##

# First, cycle tracks can be mapped using cycleway == 'track'

# Alternatively, cycle tracks may be mapped as a separate way and tagged as 
# But these will be covered in the 2.1.3. separated cycleways
# But cycleway=track + highway=cycleway cannot be used together as this indicates that 
# the cycleway has an adjacent cycleway=track, which it obviously does not.
# For more information, please refer to 
# https://wiki.openstreetmap.org/wiki/Tag:cycleway%3Dtrack

# cycle tracks using cycleway=track and track-related tags
cycle_tracks <- edges_filtered %>%
  filter(
    
    # With cycleway=track, cycle paths are mapped together with the properties of the roads
    (cycleway == 'track') |
      
      
      # map cycle tracks based on the description tags  
      (  
        # The description tags should be used together with
        # cycleway == 'track' but there are many cycleway tag which do not contain values
        # so, only description words in the tags are used to capture more
        grepl('cycleway:right":"track', tags) | # bicycle track only on the right
          grepl('cycleway:left":"track', tags) | #  bicycle track only on the left
          grepl('cycleway:both":"track', tags) | #  bicycle track only on both
          grepl('cycleway":"track', tags)
      ) 
      
  ) %>%
  distinct() 



#***************************
#* 2.1.2. Highway=cycleway *
#***************************

##************##
## Definition ##
##************##

# Alternatively, we can consider mapping cycle tracks as a separate way next to the road 
# Designated cycleways, by theory, should be exclusively reserved for cyclists. 
# but some are mis-tagged as they share the roads with other things so I filtered them out.


##********##
## Method ##
##********##

# Use highway=cycleway. The highway=cycleway tag indicates a separate way for the use of cyclists.
# Please refer to https://wiki.openstreetmap.org/wiki/Tag:highway%3Dcycleway

# highway=cycleway - separate OSM element 
designated_cycleways <- edges_filtered %>%
  filter(
    (highway == 'cycleway' & 
       
       # filter out shared lanes
       !(grepl('cycleway:right":"share_busway', tags) |
           grepl('cycleway:left":"share_busway', tags) |
           grepl('cycleway:both":"share_busway', tags) |
           grepl('cycleway:yes":"share_busway', tags) |
           grepl('cycleway:right":"shared_lane', tags) |
           grepl('cycleway:left":"shared_lane', tags) |
           grepl('cycleway:both":"shared_lane', tags) |
           grepl('cycleway:yes":"shared_lane', tags) |
           grepl('cycleway:right":"shared', tags) |
           grepl('cycleway:left":"shared', tags) |
           grepl('cycleway:both":"shared', tags) |
           grepl('cycleway:yes":"shared', tags)
           
           # filter out shared-, lanes
          | (cycleway %in% c('shared', 'lane'))
    )
  )) %>%
  distinct() %>% filter(!id%in%cycle_tracks$id)



#****************************
#* 2.1.3. cycleway=separate *
#****************************

##************##
## Definition ##
##************##

# Cycle tracks as a separate geometry.

##********##
## Method ##
##********##

# cycleway is mapped as a separate geometry using cycleway=separate.
# Alternatively, as mentioned in 2.1.1. cycle tracks, they may be mapped as a separate way by tagging 
# `highway=cycleway & cycleway=sidepath`, or as 
# `highway=path & path=sidepath & bicycle=designated`.
# Please refer to https://wiki.openstreetmap.org/wiki/Tag:cycleway%3Dtrack
# https://wiki.openstreetmap.org/wiki/Tag:cycleway%3Dseparate



separated_cycleways <- edges_filtered %>%
  filter(
    
    # Alternatively, they can be mapped separately as a separate line
    # in the case of separately mapped cycle paths, cycleway=separate should be added to the road way
    # https://wiki.openstreetmap.org/wiki/Tag:cycleway%3Dseparate
    (
         # cycleway=separate only indicates that there is cycle path nearby 
          (grepl('cycleway:right":"separate', tags) |
           grepl('cycleway:left":"separate', tags) |
           grepl('cycleway:both":"separate', tags) |
           grepl('cycleway":"separate', tags) |
           
           # use cycleway tag
           cycleway %in% c('separate')) |
           
            
           # bicycle=use_sidepath specifically is set on the road line to indicate
           # that cyclists are forbidden from cycling on the road itself because 
           # there is a compulsory cycle path mapped as a separate geometry. 
           # This implies that there is a separately mapped cycle path nearby.
          (
           grepl('bicycle":"use_sidepath', tags) |
           grepl('bicycle:backward":"use_sidepath', tags) |
           grepl('bicycle:forward":"use_sidepath', tags) |

           # https://wiki.openstreetmap.org/wiki/Tag:bicycle%3Duse_sidepath
           bicycle == 'use_sidepath') &
       
            
          # Filter out shared lanes
       !(grepl('cycleway:right":"share_busway', tags) |
           grepl('cycleway:left":"share_busway', tags) |
           grepl('cycleway:both":"share_busway', tags) |
           grepl('cycleway:yes":"share_busway', tags) |
           grepl('cycleway:right":"shared_lane', tags) |
           grepl('cycleway:left":"shared_lane', tags) |
           grepl('cycleway:both":"shared_lane', tags) |
           grepl('cycleway:yes":"shared_lane', tags) |
           grepl('cycleway:right":"shared', tags) |
           grepl('cycleway:left":"shared', tags) | 
           grepl('cycleway:both":"shared', tags) |
           grepl('cycleway:yes":"shared', tags))) |
      

      # The following mapping methods are indicated in the OSM link below
      # https://wiki.openstreetmap.org/wiki/Tag:cycleway%3Dtrack
      # map cycle tracks separately

      (highway == 'cycleway' & (grepl('cycleway":"sidepath', tags) | cycleway == 'sidepath')) |
      
      # or as
      (highway == 'path' & bicycle == 'designated' & 
         (grepl('path":"sidepath', tags) | path == 'sidepath'))
    
    
  ) %>% distinct() %>%
  filter(!id%in%cycle_tracks$id) %>%
  filter(!id%in%designated_cycleways$id)







#*****************************
#* 2.1.4. Data Consolidation *
#*****************************

protected_cycle_lane <- rbind(cycle_tracks, separated_cycleways, designated_cycleways)

rm(cycle_tracks, separated_cycleways, designated_cycleways)
sum(protected_cycle_lane$length)

gc()


## 2.2. Exclusive Cycle Lanes ####################################################

##************##
## Definition ##
##************##

# An exclusive cycle lane is bicycle infrastructure that is an inherent part of 
# the road, but set aside for the exclusive use of bicycles, 
# whilst being separated only by paint or other markings.
# Please refer to https://wiki.openstreetmap.org/wiki/Tag:cycleway%3Dlane, 

##********##
## Method ##
##********##

# Use cycleway=lane
# Sometimes, this can be mapped using 'cycleway":"exclusive' if the bicycle lanes are 
# strictly reserved for cyclists.


unique(edges_filtered$cycleway)


exclusive_cycle_lane <- edges_filtered %>% 
  filter(
    (highway != 'cycleway' &
       
       # Use description tags - cycle lanes
       ((
           grepl('cycleway:right":"lane', tags) |
           grepl('cycleway:left":"lane', tags) |
           grepl('cycleway:both":"lane', tags) |
           grepl('cycleway:yes":"lane', tags) |
          

           grepl('cycleway:right:lane":"exclusive', tags)|
           grepl('cycleway:left:lane":"exclusive', tags)|
           grepl('cycleway:both:lane":"exclusive', tags)
             
 
           ) &
          
          # but not shared with other motorised vehicles
          !(grepl('cycleway:right":"share_busway', tags) |
              grepl('cycleway:left":"share_busway', tags) |
              grepl('cycleway:both":"share_busway', tags) |
              grepl('cycleway:yes":"share_busway', tags) |
              grepl('cycleway:right":"shared_lane', tags) |
              grepl('cycleway:left":"shared_lane', tags) |
              grepl('cycleway:both":"shared_lane', tags) |
              grepl('cycleway:yes":"shared_lane', tags) |
              grepl('cycleway:right":"shared', tags) |
              grepl('cycleway:left":"shared', tags) | 
              grepl('cycleway:both":"shared', tags) |
              grepl('cycleway:yes":"shared', tags) |
              
              # also exclude those not exclusively reserved for cyclists
              grepl('cycleway:right:lane":"advisory', tags)|
              grepl('cycleway:left:lane":"advisory', tags)|
              grepl('cycleway:both:lane":"advisory', tags)
              
              )
        
       )) |
      
      # Use cycleway tags
      cycleway == 'lane' |
      
      # general way to detect whether a way has any designated cycle lane from 
      # the bicycle:lanes=*... tag
      # designated is explicitly a cycle lane
      # refer to this: https://wiki.openstreetmap.org/wiki/Bicycle#Off-road_and_outdoor
      grepl('"bicycle:lanes":"[^"]*designated', tags)
    
  ) %>% distinct() %>% 
  
  filter(!id %in% protected_cycle_lane$id)




gc()


## 2.3. Advisory Lane ##########################################################

#************************
#* 2.3.1. Advisory Lane *
#************************

##************##
## Definition ##
##************##

# Shared with other vehicles, yet markings and lanes are indicated,
# improving cyclists' safety.
# Yet, not exclusively reserved for cyclists


##********##
## Method ##
##********##

# Use cycleway=share_busway|shared_lane|advisory or 
# 'cycleway:right":"share_busway'|'shared'|advisory' in tags

advisory_cycle_lane <- edges_filtered %>%
  filter(
    (   
       # This is quite UK-specific cycleway
        grepl('cycleway:right":"share_busway', tags) |
        grepl('cycleway:left":"share_busway', tags) |
        grepl('cycleway:both":"share_busway', tags) |
        grepl('cycleway:yes":"share_busway', tags) |
          
        # cyclists share a lane with motor vehicles and there are markings  
        grepl('cycleway:right":"shared_lane', tags) |
        grepl('cycleway:left":"shared_lane', tags) |
        grepl('cycleway:both":"shared_lane', tags) |
        grepl('cycleway:yes":"shared_lane', tags) |
          
        grepl('cycleway:right":"shared', tags) |
        grepl('cycleway:left":"shared', tags) | 
        grepl('cycleway:both":"shared', tags) |
        grepl('cycleway:yes":"shared', tags) |
        
        # advisory cycle lanes are not exclusively reserved for cyclists
        # cars may drive over and halt on it
        # usually marked with dashed line
        
        grepl('cycleway:right:lane":"advisory', tags)|
        grepl('cycleway:left:lane":"advisory', tags)|
        grepl('cycleway:both:lane":"advisory', tags)|
        grepl('cycleway:yes:lane":"advisory', tags) |
          
        grepl('cycleway:right:lane":"pictogram', tags) |
        grepl('cycleway:left:lane":"pictogram', tags) |
        grepl('cycleway:both:lane":"pictogram', tags) |
        grepl('cycleway:yes:lane":"pictogram', tags) |
          


      # use cycleway tags
      (cycleway %in% c('share_busway', 'shared_lane',
                         'advisory', 'shared'))
  )) %>% 
  distinct() %>% 
  filter(!id%in%protected_cycle_lane$id) %>%
  filter(!id%in%exclusive_cycle_lane$id)



gc()

## 2.4. Standalone paths #######################################################

#***************************
#* 2.4.1. Standalone Paths *
#***************************

##************##
## Definition ##
##************##

# Standalone paths are bike paths or shared-use paths, typically located 
# in green settings (e.g. parks, rivers, canals).
# Caveat! pedestrianised zones, such as city centre squares, are not part of off-road 
# It is called shared streets or places with no dedicated cycling infrastructure

##********##
## Method ##
##********##

# Outside of cities and dense populated areas most cycling routes are mapped using either
# 1) highway=track when the way is also used by large motorised vehicles (agricultural, forestry, emergency vehicles...) or
# 2) highway=path when not intended for motorised vehicles (rather pedestrians, horses...).
# both imply bicycle=yes in most of the world, but its value is missing in many cases.
# refer to https://wiki.openstreetmap.org/wiki/Bicycle#Off-road_and_outdoor


# For ways designated for pedestrians, but which also allows bicycles, use
# 3) highway=footway + bicycle=yes|designated
# 4) additionally, it is also useful to categorise paths that fall within parks using spatial join

# 3) Where a path is shared-use by cyclists and pedestrians,
# If path is split, cycleway=segregated | segregated=yes. 
# If not, segregated=no.
unique(edges_filtered$cycleway)

# off-road paths
off_road <- edges_filtered %>% 
  filter(
    
    # For ways designated for pedestrians, but which also allows bicycles
    # refer to: https://wiki.openstreetmap.org/wiki/Tag:highway%3Dfootway
    ((highway %in% c('footway')) & 
       (bicycle == 'designated' | bicycle == 'yes'))  |
      
      # Ways used by pedestrians, small vehicles like bicycles, for animal riding or livestock walking.
      highway %in% c('path') |
      
      # Ways used by large motorised vehicles (agricultural, forestry, emergency vehicles...) 
      highway %in% c('track')
      
  ) %>% 
  distinct() %>%
  filter(!id %in% protected_cycle_lane$id) %>%
  filter(!id %in% exclusive_cycle_lane$id) %>%
  filter(!id %in% advisory_cycle_lane$id) 





low_traffic_shared_streets <- edges_filtered %>%
  
  # low-traffic shared foot- and bicycle ways based on highway type
  filter((highway == 'living_street' | 
    highway == 'residential') & 
     (bicycle == 'designated' | bicycle == 'yes')) %>% 
  distinct() %>%
  filter(!id %in% protected_cycle_lane$id) %>%
  filter(!id %in% exclusive_cycle_lane$id) %>%
  filter(!id %in% advisory_cycle_lane$id) %>%
  filter(!id %in% off_road$id)


# create new columns called type & protection_status
protected_cycle_lane$type <- 'Protected Cycle Lane'
exclusive_cycle_lane$type <- 'Exclusive Cycle Lane'
advisory_cycle_lane$type <- 'Advisory Cycle Lane'
off_road$type <- 'Off-road'

protected_cycle_lane$protection_status <- 'Protected Cycleway'
exclusive_cycle_lane$protection_status <- 'Unprotected Cycleway'
advisory_cycle_lane$protection_status <- 'Unprotected Cycleway'
off_road$protection_status <- 'Protected Cycleway'


# Combine all variables
all_cycleways <- rbind(protected_cycle_lane, exclusive_cycle_lane, advisory_cycle_lane, off_road)

# For routing purpose in the future as cycleways are very patchy
# Step 1: Create a `type` column in `edges_filtered`, initialising with NA or any default value
edges_final <- edges_filtered %>%
  mutate(type = 'Other Cyclable Path',
         protection_status = 'Unprotected Cycleway')

# Step 2: feed the categorised cycleways into original dataset
edges_final <- edges_final %>%
  mutate(type = ifelse(id %in% all_cycleways$id, 
                       # Assign matching type from all_cycleways
                       all_cycleways$type[match(id, all_cycleways$id)],
                       # Otherwise keep the existing type
                       type),
         
         protection_status = ifelse(id %in% all_cycleways$id, 
                                    # Assign matching protection_status from all_cycleways
                                    all_cycleways$protection_status[match(id, all_cycleways$id)],
                                    # Otherwise keep the existing type
                                    protection_status))


gc()


# create edgeID
edges_final <- edges_final %>%
  mutate(edgeID = 1:nrow(.)) %>%
  select(edgeID, everything())  # Moves edgeID to the first column


# save data
st_write(edges_final, paste0('/myriadfs/home/ucfnpje/Scratch/enhance/data/roads/', city, '_categorised_nx.gpkg'), layer = 'edges',
         delete_layer = TRUE)
st_write(nodes, paste0('/myriadfs/home/ucfnpje/Scratch/enhance/data/roads/', city, '_categorised_nx.gpkg'), layer = 'nodes', 
         delete_layer = TRUE)






