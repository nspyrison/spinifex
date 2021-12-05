# Original case ----
## Setup -----
{
  require("ggplot2")
  require("spinifex")
  require("magrittr")
  
  tgt_sim_nm <- "EEV_p6_33_66_rep2"
  tgt_fp <- paste0("./buildignore/zExamples/", tgt_sim_nm, ".rda") 
  ## Make data plot
  load(tgt_fp, envir = globalenv())
  dat <- EEV_p6_33_66_rep2
  bas <- basis_pca(dat, d = 4)[, c(1,4)]
  clas <- attr(dat, "cluster")
  df <- tibble::tibble(dat, clas)
  source("./paper/R/9_util_funcs.r") ## previously ggproto_pca_biplot.r
  if(F)
    file.edit("./paper/R/9_util_funcs.r")
}


## Make figBiplotScoring.pdf -----

### the issue ----
if(F)
  debugonce(proto_point)
(orig <- ggtour(bas, dat) +
   proto_point(aes_args = list(shape = clas, color = clas)) +
   proto_basis() + ## removes cluster d when used....
   proto_origin() +
   labs(title = "y limits impact presence of 4th cluster"))
   #coord_fixed(xlim = c(-8, 3), ylim = c(-1.5, 6)))
   #ylim(-.5, 7)) 
   #expand_limits(x = c(-7, 3), y = c(-1.5, 5)))
orig + ylim(-.5, 5) + labs(subtitle = "WITHOUT cluster 4")# +
  ggplot2::theme_void() + theme(axis.title = ggplot2::element_text())
orig + expand_limits(x = c(-3, 7), y = c(-3, 10)) + labs(subtitle = "WITH cluster 4")
orig + theme_void()
## APPLING THEMES ALSO FIX...
orig + ylim(-.5, 5) + labs(subtitle = "WITH cluster 4") + theme_bw()
orig + ylim(-.5, 7) +  labs(subtitle = "WITH cluster 4, + theme_bw") + theme_bw()

## Coord_fixed casues?
# NOT EXCLUSIVELY.
theme_spinifex2 <- list(ggplot2::theme_void(),
                        ggplot2::coord_fixed(),
                        ggplot2::labs(x = "", y = "", color = "", shape = "", fill = "")
                
)
orig + theme_bw() + theme_spinifex2 + ylim(-.5, 5) 

## seems related to an interaction of  theme_void legend position = "bottom".
orig + theme_bw() + ylim(-.5, 5) +
  list(ggplot2::theme_void(),
       ggplot2::theme(legend.position  = "bottom"))
orig + theme_bw() + ylim(-.5, 5) +
  list(ggplot2::theme_void(),
       ggplot2::theme(legend.position  = "right"))


# Producing reprex -----
## with mtcars? -----
library(ggplot2)
external_cyl <- factor(mtcars$cyl)
g <- ggplot(mtcars, aes(disp, mpg, color = external_cyl)) +
  geom_point() +
  ylim(10, 30) +
  ggtitle("4cyl points exist, good")## cuts off 4, 4cyl points
g
g + theme(legend.position = "top")
g + theme_void()
g + list(theme_void(), theme(legend.position = "bottom"))
g + theme_spinifex() ## still contains, maybe interaction with basis map.


basis <- prcomp(mtcars)$rotation
proj <- data.frame(as.matrix(mtcars) %*% basis)

ggplot(proj, aes(PC1, PC2, color = factor(mtcars$cyl))) + 
  geom_point() + draw_basis(basis, proj[,1:2]) + 
  theme_spinifex()
  list(theme_void(), theme(legend.position = "bottom")) + ylim(-100, 22)
## can't reproduce with draw_basis...; try removeing legend position from theme_spinifex