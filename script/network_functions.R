# pak::pak("ischlo/Btoolkit")
library(Btoolkit)
library(dplyr)
library(sf)
library(cppRouting)
library(igraph)
library(data.table)
## library
library(h3)


# extract largest component
get_lcc <- function(ways, graph_mode = "weak") {
  stopifnot("data.table" %in% class(ways)
            ,any("from" %in% colnames(ways), "u" %in% colnames(ways))
            ,any("to" %in% colnames(ways),"v" %in% colnames(ways))
  )
  igraph_ways <- igraph::graph_from_data_frame(ways[,.(from,to)],directed = FALSE)
  if(igraph_ways |> igraph::is_connected(mode = graph_mode)) {
    cat('Graph is connected')
    return(ways)
  }
  nodes_comp <- igraph::components(igraph_ways,mode = graph_mode)
  vert_ids <- igraph::V(igraph_ways)[nodes_comp$membership == which.max(nodes_comp$csize)]$name
  return(ways[from %in% vert_ids & to %in% vert_ids,])
}



make_cppr_net <- function(edges,nodes = NULL, simple = TRUE, directed = FALSE) {
  # Provide a edges and nodes from osmnx exported graph.
  # crs 4326 is expected
  # it's good if the data has also the variables from and to
  # designating nodes that are connected, but not essential
  edges <- edges |>
    as.data.table()
  
  try({nodes <- nodes |>
    as.data.table()})
  
  edges <- get_lcc(edges)
  
  
  # we don't have enough info on the edges to make a directed graph, so the assumption
  # is taken that you can cycle in both direction on any edge
  graph <- edges[,.(from,to,length)] |> cppRouting::makegraph(directed = directed
                                                              ,coords = nodes[,.(id,lon,lat)])  
  
  if(simple == TRUE) {
    # simplifying the graph
    graph <- graph |>
      cppRouting::cpp_simplify(rm_loop = TRUE
                               ,iterate = FALSE) # iterating may cause changes that we don't want
  }
  graph
}


  
    
make_cppr_w_net <- function(edges,nodes = NULL, simple = TRUE, directed = FALSE, weighted = FALSE) {
    # Provide a edges and nodes from osmnx exported graph.
    # crs 4326 is expected
    # it's good if the data has also the variables from and to
    # designating nodes that are connected, but not essential
    edges <- edges |>
      as.data.table()
    
    
    try({nodes <- nodes |>
      as.data.table()})
    
    edges <- get_lcc(edges)
    
    
    # prepare auxiliary weight if weighted = TRUE
    if(weighted){
      if(!'weights' %in% names(edges)) stop("edges must have a 'weights' column")
      aux_weights <- edges$length / edges$weights
    } else{
      aux_weights <- NULL
    }
    
    # we don't have enough info on the edges to make a directed graph, so the assumption
    # is taken that you can cycle in both direction on any edge
    graph <- edges[,.(from,to,length)] |> cppRouting::makegraph(directed = directed
                                                                ,coords = nodes[,.(id,lon,lat)]
                                                                ,aux = aux_weights)
    if(simple == TRUE) {
      # simplifying the graph
      graph <- graph |>
        cppRouting::cpp_simplify(rm_loop = TRUE
                                 ,iterate = FALSE) # iterating may cause changes that we don't want
    }
    graph
  }