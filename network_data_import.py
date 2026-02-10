# Packages
import osmnx as ox # install
import geopandas as gpd # install
from os.path import exists # install
import pyrosm as pyr # install
import networkx as nx
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# Select City
city = 'en_wales' 


# bounding box of Lisbon Metropolitan Region
bbox_of_interest = {
    'lisbon': {
        'bbox': {
            'west': -9.51,
            'east': -8.48,
            'south': 38.4,
            'north': 39.07
        },
        'src': '/home/ucfnpje/Scratch/enhance/data/osm_extracts/portugal-251126.osm.pbf'
    },
    
    'amsterdam': {
    'bbox': {
            'west': 4.47,
            'east': 5.61,
            'south': 52.16,
            'north': 52.7   
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
    'src': '/home/ucfnpje/Scratch/enhance/data/osm_extracts/greater-london-latest.osm.pbf'
    },
    
    
    'england': {
    'bbox': {
            'west': -6.71,
            'east': 2.1,
            'south': 49.66,
            'north': 55.92
    },
    'src':'/home/ucfnpje/Scratch/enhance/data/osm_extracts/england-260119.osm.pbf'
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

## Cycle Network
# extract networks from layer
net_filepath = "/myriadfs/home/ucfnpje/Scratch/enhance/data/roads/en_wales_raw_nx.gpkg"


# Extract Network - polygon or bounding box
# 1) Polygon boundary - CRS should be in EPSG:4326
#boundary = gpd.read_file('/myriadfs/home/ucfnpje/Scratch/enhance/data/boundary/wider_lisbon_boundary.geojson')
#bbox_geom = boundary['geometry'].values[0]

# 2) bounding box in a list: [west, south, east, north]
bbox = [bbox_of_interest[city]['bbox']['west'],
        bbox_of_interest[city]['bbox']['south'],
        bbox_of_interest[city]['bbox']['east'],
        bbox_of_interest[city]['bbox']['north']]


if not exists(net_filepath):
  if bbox_of_interest[city]['src']=='':
    sf_net = ox.graph_from_bbox(
      west= bbox_of_interest[city]['bbox'][1]
      ,east= bbox_of_interest[city]['bbox'][2]
      ,south= bbox_of_interest[city]['bbox'][3]
      ,north= bbox_of_interest[city]['bbox'][4]
      ,network_type='walk'
      ,simplify=True
      ,retain_all=False
      ,truncate_by_edge=True
      ,clean_periphery=True
      ,custom_filter=None
    )
    ox.save_graph_geopackage(sf_net,net_filepath)
  elif bbox_of_interest[city]['src']!='': # experimental : from a local pbf file construct the network

    print('Extracting the network from the bbox in file '+bbox_of_interest[city]['src'])

    output_path = bbox_of_interest[city]['src']


    # Initialise the reader within bounding box
    osm = pyr.OSM(output_path, bounding_box = bbox) # polygon boundary: bbox_geom 

    
    # Get all cyclable roads and the nodes
    nodes,edges = osm.get_network(network_type="cycling",nodes=True)

    # save nodes and edges
    nodes.to_file(net_filepath, layer="nodes", driver="GPKG")
    edges.to_file(net_filepath, layer="edges", driver="GPKG")


else :
  print('Raw network file exists')

print('finished!')






