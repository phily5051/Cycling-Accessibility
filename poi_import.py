import osmnx as ox # install
import geopandas as gpd # install
from os.path import exists # install
import pyrosm as pyr # install
import networkx as nx
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# city of interest
city = 'en_wales' 


# bounding box of Amsterdam Metropolitan Region
bbox_of_interest = {
    'lisbon': {
        'bbox': {
            'west': -9.5006,
            'east': -8.4610,
            'south': 38.4091,
            'north': 39.0648
        },
        'src': '/myriadfs/home/ucfnpje/Scratch/enhance/data/osm_extracts/portugal-251126.osm.pbf'
    },
    'amsterdam': {
    'bbox': {
            'west': 4.4775,
            'east': 5.6059,
            'south': 52.1658,
            'north': 52.6929   
    },
    'src': '/myriadfs/home/ucfnpje/Scratch/enhance/data/osm_extracts/netherlands-251126.osm.pbf'
    },
    'london': {
    'bbox': {
            'west': -0.5466,
            'east': 0.3008, 
            'south': 51.2559, 
            'north': 51.7202
    },
    'src': '/myriadfs/home/ucfnpje/Scratch/enhance/data/osm_extracts/greater-london-latest.osm.pbf'
    },
    
    'en_wales': {
    'bbox': {
            'west': -6.71,
            'east': 2.1,
            'south': 49.66,
            'north': 55.92
    },
    'src':'/home/ucfnpje/Scratch/enhance/data/osm_extracts/united-kingdom-260122.osm.pbf'
    }
}


# extract poi from layer
poi_filepath = "/myriadfs/home/ucfnpje/Scratch/enhance/data/"+city+"_pois.geojson"

output_path = bbox_of_interest[city]['src']

# 2) bounding box in a list: [west, south, east, north]
bbox = [bbox_of_interest[city]['bbox']['west'],
        bbox_of_interest[city]['bbox']['south'],
        bbox_of_interest[city]['bbox']['east'],
        bbox_of_interest[city]['bbox']['north']]



# Extract POIs from polygon boundary not bounding box 
# boundary = gpd.read_file('/myriadfs/home/ucfnpje/Scratch/enhance/data/boundary/amsterdam/mra_alkmaar_boundary.geojson')
# bbox_geom = boundary['geometry'].values[0] 

# Initialise the reader
osm = pyr.OSM(output_path, bounding_box = bbox)

# Custom filter - by default pyrosm reads all elements having 'amenity', 'shop', or 'tourism' tag
custom_filter = {'amenity': True, "shop": True, "tourism":True, "leisure":True}

# Get pois
pois = osm.get_pois(custom_filter = custom_filter)
 
# save pois 
pois.to_file(poi_filepath)

print('finished!')
