library(DependenciesGraphs) ## remotes::install_github("https://github.com/datastorm-open/DependenciesGraphs")
library(spinifex) # A package I'm developing

funcs <- c("play_manual_tour", "play_tour_path", "oblique_basis", "oblique_frame")
deps <- NULL
for (i in 1:length(funcs)) {
  deps <- DependenciesGraphs::funDependencies("package:spinifex", funcs[i])
  print(plot(deps))
}
