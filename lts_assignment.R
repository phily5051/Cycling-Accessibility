.libPaths("/myriadfs/home/ucfnpje/myRlibs")

library(sf)
library(data.table)
library(dplyr)

## 1. Set up ####
# Read edges
edges <- st_read(
  "/myriadfs/home/ucfnpje/Scratch/enhance/data/roads/en_wales_categorised_nx.gpkg",
  layer = "edges"
) |>
  st_transform(27700)

edges<- st_read('data/categorised_nx/all_cycleways_london.gpkg', layer = 'edges') |>
  st_transform(27700)

edges <- as.data.table(edges)

# Clean maxspeed
edges[, maxspeed := as.numeric(gsub(" mph", "", maxspeed))]


## 2. Edge-level initial LTS assignment - data.table ####
edges[, lts := NA_real_]

# 2.1. No motor vehicles
edges[
  is.na(lts) &
    (motor_vehicle %in% c("no", "emergency") |
       motorcar %in% c("no", "emergency")),
  lts := 1
]

# 2.2. Low-stress highway types
edges[
  is.na(lts) &
    highway %in% c(
      "residential", "living_street", "track",
      "cycleway", "path", "footway", "pedestrian"
    ),
  lts := 1
]

# 2.3. <=2 lanes, <=20 mph (or unknown speed)
edges[
  is.na(lts) &
    (
      (!is.na(maxspeed) & maxspeed <= 20 & lanes < 3) |
        (is.na(maxspeed) & lanes < 3)
    ),
  lts := 2
]

# 2.4. Unclassified / tertiary
edges[
  is.na(lts) &
    highway %in% c("unclassified", "tertiary", "tertiary_link", "service"),
  lts := fifelse(
    (is.na(lanes) & !is.na(maxspeed) & maxspeed <= 20) |
      (!is.na(cycleway) & cycleway != "no"),
    2, 3
  )
]

# 2.5. Larger roads
edges[
  is.na(lts) &
    highway %in% c(
      "trunk", "trunk_link",
      "primary", "primary_link",
      "secondary", "secondary_link"
    ),
  lts := fifelse(
    !is.na(cycleway) & cycleway != "no", 3, 4
  )
]

# 2.6. Special cases
edges[highway == "steps", lts := 3]
edges[highway == "bridleway", lts := 2]

table(edges$lts, useNA = "ifany")


## 3. Read nodes ####
nodes <- st_read(
  "/myriadfs/home/ucfnpje/Scratch/enhance/data/roads/en_wales_categorised_nx.gpkg",
  layer = "nodes"
) |>
  st_transform(27700)

nodes<- st_read('data/categorised_nx/all_cycleways_london.gpkg', layer = 'nodes') |>
  st_transform(27700)

nodes <- as.data.table(nodes)
nodes[, `:=`(lts = NA_real_, message = NA_character_)]


## 4. Node LTS assignment - replaces the previous while loop
# 4.1. Edge -> node aggregation
edge_node_lts <- rbind(
  edges[, .(node = u, edge_lts = lts)],
  edges[, .(node = v, edge_lts = lts)]
)

# 4.2. Node LTS = max connected edge LTS
node_lts <- edge_node_lts[
  , .(lts = max(edge_lts, na.rm = TRUE)),
  by = node
]

nodes[node_lts, on = .(id = node), lts := i.lts]
nodes[!is.finite(lts), lts := NA]


## 5. Intersection adjustment - vectorised, no loops
# Stop sign: reduce LTS
nodes[
  lts <= 2 & grepl('"highway":\\s*"stop"', tags),
  `:=`(
    lts = 1,
    message = "Stop sign reduces to LTS 1"
  )
]

nodes[
  lts == 3 & grepl('"highway":\\s*"stop"', tags),
  `:=`(
    lts = 2,
    message = "Stop sign reduces to LTS 2"
  )
]

nodes[
  lts == 4 & grepl('"highway":\\s*"stop"', tags),
  `:=`(
    lts = 3,
    message = "Stop sign reduces to LTS 3"
  )
]

# Traffic signals
nodes[
  lts <= 2 & grepl('"highway":\\s*"traffic_signals"', tags),
  `:=`(
    lts = 1,
    message = "Low-stress signalised intersection"
  )
]

nodes[
  lts == 3 & grepl('"highway":\\s*"traffic_signals"', tags),
  `:=`(
    lts = 2,
    message = "Mixed-stress signalised intersection"
  )
]

nodes[
  lts == 4 & grepl('"highway":\\s*"traffic_signals"', tags),
  `:=`(
    lts = 3,
    message = "High-stress signalised intersection"
  )
]


## 6. Update edges from node LTS
edges[
  nodes, on = .(u = id), lts_u := i.lts
][
  nodes, on = .(v = id), lts_v := i.lts
][
  , lts := pmax(lts, lts_u, lts_v, na.rm = TRUE)
][
  , c("lts_u", "lts_v") := NULL
]

table(edges$lts, useNA = "ifany")


# 6. Save nodes to a GeoPackage
st_write(nodes, '/myriadfs/home/ucfnpje/Scratch/enhance/data/lts/lts_en_wales.gpkg', layer = "nodes", append = FALSE)


# 7. Rescale ordinal LTS value
edges[, lts_rescaled := 0]

edges[lts == 1, lts_rescaled := 1.00]
edges[lts == 2, lts_rescaled := 0.75]
edges[lts == 3, lts_rescaled := 0.50]
edges[lts == 4, lts_rescaled := 0.25]


# 8. Cycle infrastructure preference coefficient
## 8.1. Assign raw coefficients
edges[, cycle_coef := 1]

edges[type == "Off-road",                 cycle_coef := 3.17]
edges[type == "Protected Cycle Lane",     cycle_coef := 1.67]
edges[type == "Exclusive Cycle Lane",     cycle_coef := 1.45]
edges[type == "Advisory Cycle Lane",      cycle_coef := 1.40]

## 8.2. Min-max rescaling
coef_min <- edges[, min(cycle_coef, na.rm = TRUE)]
coef_max <- edges[, max(cycle_coef, na.rm = TRUE)]

edges[, cycle_coef := (cycle_coef - coef_min) / (coef_max - coef_min)]

# 9. Final edge weights (LTS + preference)
edges[, weights := (lts_rescaled + cycle_coef) / 2]


# 10. Save edges
st_write(
  "/myriadfs/home/ucfnpje/Scratch/enhance/data/lts/lts_en_wales.gpkg",
  layer = "edges",
  append = FALSE
)
