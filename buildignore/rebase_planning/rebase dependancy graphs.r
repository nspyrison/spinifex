## Depends on:
#' @examples 
#' remotes::install_bioc("Rgraphviz")
#' remotes::install_github("kcf-jackson/funGraphs")

#Working from:
browseURL("https://github.com/kcf-jackson/funGraphs#usage-1")

run_opt1 <- F
run_opt2 <- T
if(run_opt1){
  # At the package directory
  g <- funGraphs::build_pkg_graph()
  # It's unlikely the graph-arrangement algorithm will be exactly what you want; 
  # you can move the nodes around as you see fit. Once you are done, 
  # you can click 'save', then copy the generated svg string into a file and 
  # save it as a SVG file, e.g. "test.svg". To load a SVG file, copy the svg 
  # string into the textbox provided, then click 'load'. 
  # Note that this is read-only. If you wish to continue modifying the graph, 
  # use 'update_graph_from_svg' instead:
  
  library(magrittr)
  g %>% 
    update_graph_from_svg("test.svg") %>% 
    start_app()
}

if(run_opt2){
  library(funGraphs)
  library(magrittr)
  
  # At the package directory
  g0 <- build_graph_from_dir("R/")    # Analyse directory and build 
  
  if(nrow(g0$nodes) < 20){
  # `prepare_graph` uses `igraph::layout_with_sugiyama` which works well
  # with small (<20) number of nodes
  g0 %>% 
    prepare_graph() %>%               # Add plotting parameters
    start_app()                       # Display app
  }
  
  # `prepare_graph_Rgraphviz` uses the default in `Rgraphviz` which
  # works well for larger number of nodes.
  g0 %>% 
    prepare_graph_Rgraphviz() %>%     # Add plotting parameters
    start_app()                       # Display app
}
