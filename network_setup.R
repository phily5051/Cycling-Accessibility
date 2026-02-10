library(sf)
library(cppRouting)
library(data.table)
library(Btoolkit)

source('network_functions.R')


edges <- st_read('data/lts/lts_en_wales.gpkg', layer = 'edges') %>%
  as.data.table()


nodes <- st_read('data/lts/lts_en_wales.gpkg', layer = 'nodes') %>%
  as.data.table()


# rename columns
edges <- edges %>% rename(
  from = u,
  to = v
)


edges <- edges %>% select(edgeID, everything())




# create graphs
sf_unweighted <- make_cppr_net(edges= edges
                               ,nodes = nodes, simple = TRUE)

sf_weighted <- make_cppr_w_net(edges= edges
                               ,nodes = nodes, simple = TRUE, weighted = TRUE)


list('unweighted'=sf_unweighted
     ,'weighted' = sf_weighted) |> rlist::list.save('/myriadfs/home/ucfnpje/Scratch/enhance/data/networks/en_wales_network.rds')

