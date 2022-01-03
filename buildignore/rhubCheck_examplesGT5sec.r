## Platform:	Windows Server 2008 R2 SP1, R-oldrel, 32/64 bit
# Examples with CPU (user + system) or elapsed time > 5s
# proto_highlight 6.93   0.03    6.95
# ggtour          5.41   0.09    5.50

## Platform:	Windows Server 2022, R-devel, 64 bit
# * checking examples ... NOTE
# Examples with CPU (user + system) or elapsed time > 5s
# user system elapsed
# proto_highlight 5.27      0    5.27

## Takeaway ----
# 1) I would note that proto_basis and proto_origins take about the same time as 
# proto_point. Maybe a lighter prep .init4proto or 
# passing lists would be more performance
# 2) Lineprof is annoyingly persistent and side effect producing now.
# 3) ggtour has sizable run, and don't run only cuts 1.1 sec (implicit print).


## proto_highlight -----
## Ignoring don't runs: 2.06 sec elapsed, previously was .94 sec
## Runing don't runs: 4.83 sec.
?proto_highlight
lp_proto_highlight <- lineprof::lineprof({
  tictoc::tic("proto_highlight")
  library(spinifex)
  dat     <- scale_sd(penguins[, 1:4])
  clas    <- penguins$species
  gt_path <- save_history(dat, grand_tour(), max_bases = 5)
  
  ## d = 2 case
  ggt <- ggtour(gt_path, dat, angle = .3) +
    proto_default(aes_args = list(color = clas, shape = clas)) +
    proto_highlight(row_index = 5)
  # ## Not run: 
  # animate_plotly(ggt)
  # ## End(Not run)
  
  ## Highlight multiple observations
  ggt2 <- ggtour(gt_path, dat, angle = .3) +
    proto_default(aes_args = list(color = clas, shape = clas)) +
    proto_highlight(row_index = c( 2, 6, 19),
                    identity_args = list(color = "blue", size = 4, shape = 4))
  # ## Not run: 
  # animate_plotly(ggt2)
  # ## End(Not run)
  
  ## 1D case:
  gt_path1d <- save_history(dat, grand_tour(d = 1), max_bases = 3)
  
  ggt <- ggtour(gt_path1d, dat, angle = .3) +
    proto_default1d(aes_args = list(fill = clas, color = clas)) +
    proto_highlight1d(row_index = 7)
  # ## Not run: 
  # animate_plotly(ggt)
  # ## End(Not run)
  
  ## Highlight multiple observations, mark_initial defaults to off
  ggt2 <- ggtour(gt_path1d, dat, angle = .3) +
    proto_default1d(aes_args = list(fill = clas, color = clas)) +
    proto_highlight1d(row_index = c(2, 6, 7),
                      identity_args = list(color = "green", linetype = 1))
  # ## Not run: 
  # animate_plotly(ggt2)
  # ## End(Not run)
  tictoc::toc()
})
lineprof::shine(lp_proto_highlight)

## ggtour -----
## ignoring don't run: 3.14 sec elapsed, pi runtime
## running don't run: 4.23 sec elapsed. surprizingly, inplicit print only added 1.1sec!?
?ggtour
lp_ggtour <- lineprof::lineprof({
  tictoc::tic("ggtour")
  library(spinifex)
  dat     <- scale_sd(penguins[, 1:4])
  clas    <- penguins$species
  bas     <- basis_pca(dat)
  mv      <- manip_var_of(bas)
  mt_path <- manual_tour(bas, manip_var = mv)
  
  ## d = 2 case
  ggt <- ggtour(basis_array = mt_path, data = dat, angle = .3) +
    proto_default(aes_args = list(color = clas, shape = clas),
                  identity_args = list(size = 1.5, alpha = .8))
  # ## Not run:
  # animate_plotly(ggt)
  # ## End(Not run)
  
  ## Finer control calling individual proto_* functions
  ggt <- ggtour(basis_array = mt_path, data = dat, angle = .3) +
    proto_point(aes_args = list(color = clas, shape = clas),
                identity_args = list(size = 1.5, alpha = .8),
                row_index = which(clas == levels(clas)[1])) +
    proto_basis(position = "right",
                manip_col = "red",
                text_size = 7L) +
    proto_origin()
  # ## Not run:
  # animate_plotly(ggt)
  # ## End(Not run)
  
  ## d = 1 case
  bas1d <- basis_pca(dat, d = 1)
  mt_path1d <- manual_tour(basis = bas1d, manip_var = mv)
  
  ggt1d <- ggtour(basis_array = mt_path1d, data = dat, angle = .3) +
    proto_default1d(aes_args = list(fill= clas, color = clas))
  # ## Not run:
  # animate_plotly(ggt1d)
  # ## End(Not run)
  
  ## Single basis
  ggt <- ggtour(basis_array = bas, data = dat) +
    proto_default(aes_args = list(fill= clas, color = clas))
  # ## ggtour() returns a static ggplot2 plot
  # ggt
  # ### or as html widget with tooltips
  # animate_plotly(ggt)
  tictoc::toc()
})
lineprof::shine(lp_ggtour)
