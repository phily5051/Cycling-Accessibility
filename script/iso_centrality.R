# Isodistance Centrality

# Virtual environment path
.libPaths("/myriadfs/home/ucfnpje/myRlibs")

# Packages
library(h3)
library(Btoolkit)
library(dplyr)
library(sf)
library(cppRouting)
library(igraph)
library(data.table)
library(purrr)
library(doParallel)
library(foreach)
library(concaveman)


# origin #####
origin <- st_read('/myriadfs/home/ucfnpje/Scratch/enhance/data/boundary/london/lsoa_london.geojson')
origin <- origin %>% st_transform(., 27700)
origin <- origin %>% st_centroid()


# destination ####
pois <- readRDS('/myriadfs/home/ucfnpje/Scratch/enhance/data/pois.rds')

# eliminate unimportant/unused amenities
pois <- pois %>%
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



# # oa-level workplace - all oas contain workplace population
# oa_london <- st_read('/myriadfs/home/ucfnpje/Scratch/enhance/data/london_oa_work.geojson') %>% st_transform(4326)
# oa_london <- st_centroid(oa_london)
# oa_london <- oa_london %>% select(OA21CD) %>% rename(id = OA21CD)


# rbind poi with oa work data
# pois <- pois %>% select(id)
# pois <- rbind(pois, oa_london)



# Graph ####
network_ <- rlist::list.load('/myriadfs/home/ucfnpje/Scratch/enhance/data/networks/lcc_raw_london_network.rds')


# # Hexagon uber h3 level 10####
# h3_res <- 10
# 
# # Create hexagons
# if(is.numeric(h3_res)){
#   #alternatively, for faster, but less accurate results use: amenities
#   h3_index <- h3::geo_to_h3(network_$graph$coords[,c(3,2)] # latitude ,longitude : in that order
#                             ,res = h3_res) |> unique()
#   
#   tbl <- table(h3_index)  |>
#     tibble::as_tibble()
#   
#   hexagons_ <- h3_to_geo_boundary_sf(tbl$h3_index)
#   
#   nrow(hexagons_)
#   
#   head(hexagons_)
#   
#   hexagons <- hexagons_ |> mutate(centroid=sf::st_centroid(geometry)) 
#   # |> as.data.table()
#   rm(hexagons_)
#   
# } 
# 
# rm(tbl, h3_res, h3_index)
# gc()
# 
# 
# 
# # Find which hexagons contain which POIs
# poi_hex_index <- st_intersects(pois, hexagons)
# 
# # Convert to data.table
# projected_pois <- data.table(
#   poi_id = rep(seq_along(poi_hex_index), lengths(poi_hex_index)),
#   h3_index = unlist(poi_hex_index)
# )
# 
# # Add geometries back
# projected_pois[, geometry := st_geometry(pois)[poi_id]]
# 
# # Count POIs per hexagon
# hex_counts <- projected_pois[, .(poi_count = .N), by = h3_index]
# 
# # Merge POI count back to projected_pois
# projected_pois <- merge(projected_pois, hex_counts, by = "h3_index", all.x = TRUE)
# 
# # Create cm_category based on poi_count
# projected_pois[, cm_category := fifelse(poi_count == 1, "cm_1",
#                                         fifelse(poi_count == 2, "cm_2",
#                                                 fifelse(poi_count == 3, "cm_3", "cm_4")))]
# 
# # Compute centre of mass (cm) based on category
# cm_results <- projected_pois[, .(
#   geometry = if (.N == 1) geometry[1] else if (.N %in% c(2, 3)) {
#     st_centroid(st_union(geometry))  # Mean centroid for 2-3 POIs
#   } else {
#     st_centroid(st_convex_hull(st_union(geometry)))  # Convex hull for 4+ POIs
#   }
# ), by = .(h3_index, cm_category)]



# Project destination points to graph nodes
nn <- Btoolkit::cppr$fnearest_nodes(network_$graph
                                    ,cm_results$geometry
                                    ,local_crs = 27700)

cm_results$id <- network_$graph$dict$ref[nn$nn.idx]

print(paste0("The number of unique centre of mass IDs is ", length(unique(cm_results$id)), " out of ", length(cm_results$id)," centre of mass IDs!"))

# Project origins to graph nodes
nn <- Btoolkit::cppr$fnearest_nodes(network_$graph
                                    ,origin$geometry
                                    ,local_crs = 27700)

origin$id <- network_$graph$dict$ref[nn$nn.idx]

print(paste0("The number of unique origin IDs is ", length(unique(origin$id)), " out of ", length(origin$id), " origin IDs!"))


#rm(pois, poi_hex_index, projected_pois, hex_counts, nn)




# Isodistance Node extraction ####
n_threads <- 24

# Step 1: Find out reachable nodes from origin points - 60-minute cycling time
smoothing_isodist <- cppRouting::get_isochrone(network_$graph
                                               ,from = origin$id
                                               ,lim = 15000)



# Step 2: Construct isodistance polygons - UCL cluster computing has GEOS lower than 3.11
# so you cannot run st_concave_hull()
# smoothing_multipoints <- parallel::mclapply(
#   seq_along(smoothing_isodist),
#   mc.cores = 1,
#   FUN = \(i) {
#     ns <- smoothing_isodist[[i]]
#     sf::st_sf(
#       origin_id = names(smoothing_isodist)[i],
#       geometry = graph$coords[match(ns, id), .(lon, lat)] |>
#         sf::st_as_sf(coords = c(1, 2), crs = 4326) |>
#         sf::st_combine() |>
#         sf::st_concave_hull(ratio = 0.4) |>
#         sf::st_geometry()
#     )
#   }
# )

# You can use concaveman package
smoothing_multipoints <- parallel::mclapply(
  seq_along(smoothing_isodist),
  mc.cores = 1,
  FUN = \(i) {
    ns <- smoothing_isodist[[i]]
    
    sf::st_sf(
      origin_id = names(smoothing_isodist)[i],
      geometry = network_$graph$coords[match(ns, id), .(lon, lat)] |>
        sf::st_as_sf(coords = c(1, 2), crs = 4326) |>
        concaveman::concaveman(concavity = 3) |>
        sf::st_make_valid() |>
        sf::st_geometry()
    )
  }
)


# Combine list of sf objects into a single sf object
smoothing_multipoints <- do.call(rbind, smoothing_multipoints)

rm(smoothing_isodist)
gc()

# Step 3: Intersect destination points with isodistance polygons
cm_results <- st_as_sf(cm_results)

# more forgiving with incorrect or messy geometries
sf::sf_use_s2(FALSE)
int <- st_intersects(smoothing_multipoints, cm_results)
sf::sf_use_s2(TRUE)

# Create named list: origin_id -> vector of destination_ids
dest_list <- setNames(
  lapply(int, function(i) cm_results$id[i]),
  smoothing_multipoints$origin_id
)

rm(origin, smoothing_multipoints, int, cm_results)
gc()



# Step 4: Extract nodes visited along the shortest path between origin and its corresponding destinations
od_pair_optimized_iso <- function(graph, dest_list, cores = 1) {
  
  # Divide into chunks
  num_origin <- length(names(dest_list))
  chunks <- floor(num_origin / cores)
  
  # **Step 2: Parallel Computation for Unique Destinations**
  registerDoParallel(cores)
  
  nodes_visited <- foreach(j = 1:cores, .combine = c, .packages = 'cppRouting') %dopar% {
    
    start_idx <- ((j - 1) * chunks) + 1
    end_idx <- ifelse(j == cores, num_origin, j * chunks)  # Last core gets remaining
    
    
    # Compute shortest paths for each chunk
    chunk_results <- lapply(start_idx:end_idx, function(i) {
      origin_id <- names(dest_list)[i]
      dest_ids <- dest_list[[origin_id]]
      
      path_result <- get_multi_paths(graph, from = origin_id, to = dest_ids)
      
      # Return nested list structure
      list(
        origin_id = origin_id,
        destination_id = dest_ids,
        nodes_visited = path_result
      )
    })
    
    return(chunk_results)
  }
  
  
  stopImplicitCluster()
  
  
  return(nodes_visited)  # Return as list of lists
}




# execution
results <- od_pair_optimized_iso(network_$graph, dest_list, cores = n_threads)

rm(network_, od_pair_optimized_iso)
gc()



# Centrality ####
# Read the edges data
edges <- st_read('/myriadfs/home/ucfnpje/Scratch/enhance/data/lts/lts_london.gpkg', layer = 'edges') %>%
  st_transform(., 27700)

# Fast graph generation when node data is present #####
edges <- edges %>% st_drop_geometry() %>% as.data.table()

# rename columns
edges <- edges %>% rename(
  from = u,
  to = v
) 

edges[, `:=`(from = as.character(from), to = as.character(to))]


edge_betweenness_v3 <- function(od_pair, edges_dt) {
  
  # Extract edges from OD pairs
  edges_tb <- rbindlist(lapply(od_pair, function(res) {
    x <- res$nodes_visited |> unlist(recursive = FALSE)  # Extract nested lists
    
    # Create edge table for each OD pair
    rbindlist(lapply(x, \(y) {
      data.table(
        from = y, 
        to = shift(y, -1, fill = NA, type = "lag"), 
        weight = 1,
        origin_id = res$origin_id
      )
    }), use.names = TRUE, fill = TRUE)
  }))
  
  # Remove rows where `to` is NA (no valid next node)
  edges_tb <- edges_tb[!is.na(to)]
  
  # Compute edge betweenness centrality
  edges_betweenness <- edges_tb[, .(betweenness = sum(weight)), by = .(from, to)]
  
  # Convert `from` and `to` in edges_dt to character
  edges_dt[, `:=`(from = as.character(from), to = as.character(to))]
  
  # Merge with edges_dt (left join)
  setkey(edges_betweenness, from, to)
  setkey(edges_dt, from, to)
  edges_visited <- edges_betweenness[edges_dt, on = c("from", "to")]
  
  # Replace NAs with 0
  edges_visited[, betweenness := fifelse(is.na(betweenness), 0, betweenness)]
  
  # Normalize betweenness centrality
  edges_visited[, betweenness_norm := betweenness / max(betweenness, na.rm = TRUE)]
  
  return(edges_visited)
}



# Multi-core processing ######
# Define chunk sizes - each chunk size tends to be much bigger if you use multicore
num_chunks <- 20
chunk_size <- ceiling(length(results) / num_chunks)
result_chunks <- split(results, ceiling(seq_along(results) / chunk_size))

rm(results)
gc()

# Initialise a list to store chunk results
centrality_list <- vector('list', length = num_chunks)

# number of cores
registerDoParallel(n_threads)


# Step 2: For each chunk, divide the data into sub-chunks for parallel processing
for (i in seq_along(result_chunks)) {
  cat("Processing chunk", i, "...\n")
  
  chunk <- result_chunks[[i]]
  
  # Further split the chunk into sub-chunks based on the number of threads
  sub_chunk_size <- ceiling(length(chunk) / n_threads)
  sub_chunks <- split(chunk, ceiling(seq_along(chunk) / sub_chunk_size))
  
  # Step 3: Use foreach to process the sub-chunks in parallel
  chunk_result <- foreach(sub_chunk = sub_chunks, .combine = rbind, .packages = 'data.table') %dopar% {
    edge_betweenness_v3(sub_chunk, edges)
  }
  
  # To save data size, we group by edgeID
  chunk_result <- chunk_result[, .(betweenness = sum(betweenness)), by = .(edgeID, from, to, length)]
  
  # Store results in the list instead of writing to disk
  centrality_list[[i]] <- chunk_result
  
  # Clean up objects to save memory
  rm(chunk, chunk_result, sub_chunks)
  gc()
}


stopImplicitCluster()
cat("All chunks processed and saved successfully!\n")

rm(edges, result_chunks)
gc()





# Combine all data.tables into one
centrality_combined <- rbindlist(centrality_list)

# Aggregate betweenness by summing its values for the same edgeID, from, to, and length
centrality_combined <- centrality_combined[, .(betweenness = sum(betweenness)), by = .(edgeID, from, to, length)]

centrality_combined <- centrality_combined[, betweenness_norm := betweenness / max(betweenness, na.rm = TRUE)]
fwrite(centrality_combined, "/myriadfs/home/ucfnpje/Scratch/enhance/data/centrality/lsoa_iso_centrality_60min_no_work_1.csv")





