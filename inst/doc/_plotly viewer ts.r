#devtools::install_github("nspyrison/spinifex")
install.packages("plotly", dependencies = T)

# spinifex repex
library(spinifex)
data(flea)
flea_std <- tourr::rescale(flea[,1:6])
rb <- tourr::basis_random(n = ncol(flea_std))
mtour <- manual_tour(basis = rb, manip_var = 4)
sshow <- create_slideshow(data = flea_std, tour = mtour)
as.is <- render_slideshow(slide_deck = sshow)
as.is

# dive into render_slideshow(). # render_slideshow(slide_deck = sshow)
slide_deck <- sshow
data_slides      <- slide_deck[[1]]
bases_slides     <- slide_deck[[2]]
nrow_data        <- nrow(data_slides[data_slides$slide == 1,])
nrow_data_slides <- nrow(data_slides)

lab_abbr <- abbreviate(colnames(data_slides), 3)
# Initialize circle for the axes reference frame.
angle    <- seq(0, 2 * pi, length = 360)
circ     <- data.frame(x = cos(angle), y = sin(angle))

### Graphics
(gg1 <- 
  ggplot2::ggplot() +
  ggplot2::geom_point(data_slides, size = .7,
                      mapping = ggplot2::aes(x = V1, y = V2, frame = slide) ) +
  ggplot2::scale_color_brewer(palette = "Dark2") +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::coord_fixed(ratio = 1) )

# Reference frame text and axes
(gg2 <- suppressWarnings( # suppress to ignore unused aes "frame"
  gg1 +
    ggplot2::geom_text(
      data = bases_slides, size = 4, hjust = 0, vjust = 0,
      mapping = ggplot2::aes(x = V1, y = V2, frame = slide, label = lab_abbr)
    ) +
    ggplot2::geom_segment(
      data = bases_slides, size = .3,
      mapping = ggplot2::aes(x = V1, y = V2, xend = 0, yend = 0, frame = slide)
    )
))

# Reference frame circle
(gg3 <- gg2 + ggplot2::geom_path(
  data = circ, color = "grey80", size = .3, inherit.aes = FALSE, 
  ggplot2::aes(x = x, y = y)
))
pgg1 <- plotly::ggplotly(gg3) # before rev(layers) # data disapears
gg3$layers <- rev(gg3$layers) # Reverse layers for correct overlaping.
pgg2 <- plotly::ggplotly(gg3) # after  rev(layers) # segements disapear.

pgg1
pgg2
(pgg3 <- plotly::ggplotly(gg2)) #before circ, before rev(layers) # segements disapear.

message("DEALING WITH NAs")
visdat::vis_dat(data_slides) # Well... slide 20/20 contains NAs.
na.rm_data_slides  <- sshow[[1]][complete.cases(sshow[[1]]), ]
na.rm_bases_slides <- sshow[[2]][complete.cases(sshow[[2]]), ]
na.rm_sshow <- list(na.rm_data_slides, na.rm_bases_slides)
render_slideshow(slide_deck = na.rm_sshow)
render_slideshow(slide_deck = sshow)
#doesn't seem to be related to the other display issues.
###########
# message("WINDOWS VIEWER TAB A CURRENCY ISSUE?")
# #see: https://github.com/ropensci/plotly/issues/717
# #try::
# #update Rstudio # using newest ver as of 24/10/2018.
# #update R       # using newest ver as of 24/10/2018.
# #restart R then:
# install.packages('digest',     dependencies = TRUE)
# install.packages('rlang',      dependencies = TRUE)
# install.packages('data.table', dependencies = TRUE)
# devtools::install_github("ropensci/plotly", dependancies = TRUE)
############
# message("WINDOWNS VIEWER TAB A NETWORK ISSUE?")
# # Maybe a network dependant issue, see:
# #browseURL("https://community.plot.ly/t/rstudio-viewer-shows-blank-plots-the-browser-can-show-them-though/3359")
# viewer <- getOption("viewer")
# # 'Show in new window opens Chrome@ http://localhost:30659/session/viewhtml315479c73e/index.html
# #replacing 'localhost' with '127.0.0.1' give the same issue:
# viewer("http://127.0.0.1:30659/session/viewhtml315479c73e/index.html")
############
# message("OLD CODE FROM LOCAL BACKUP 1. SHOWS SIMILAR BEHAVIOR.")
message()
# slide_deck <- sshow
# data_slides      <- slide_deck[[1]]
# bases_slides     <- slide_deck[[2]]
# nrow_data        <- nrow(data_slides[data_slides$slide == 1,])
# nrow_data_slides <- nrow(data_slides)
# 
# lab_abbr <- abbreviate(colnames(data_slides), 3)
# # Initialize circle for the axes reference frame.
# angle    <- seq(0, 2 * pi, length = 360)
# circ     <- data.frame(x = cos(angle), y = sin(angle))
# 
# gg1 <- ggplot2::ggplot(data = data_slides, ggplot2::aes(x = V1, y = V2) ) +
#   suppressWarnings( # to suppress to ignore unused aes "frame"
#     ggplot2::geom_point(size = .7,
#                         ggplot2::aes(frame = slide) 
#     )
#   ) + 
#   ggplot2::theme_void() +
#   ggplot2::theme(legend.position = "none") +
#   ggplot2::coord_fixed() + 
#   ggplot2::scale_color_brewer(palette = "Dark2")
#   
#   # axes and labels. ggplot2
#   gg2 <- suppressWarnings( # suppress to ignore unused aes "frame"
#     gg1 + ggplot2::geom_text(data = bases_slides,
#                              size = 4, hjust = 0, vjust = 0,
#                              ggplot2::aes(x = V1, y = V2, frame = slide, label = lab_abbr)
#     ) + ggplot2::geom_segment(data = bases_slides, size = .3, #color = , #TODO
#                               ggplot2::aes(x = V1, y = V2, xend = 0, yend = 0, 
#                                            frame = slide)
#     )
#   )
# 
# # axes circle. ggplot2
# gg3 <- gg2 + 
#   ggplot2::geom_path(data = circ, color = "grey80", size = .3,
#                      ggplot2::aes(x, y) )
# gg3$layers <- rev(gg3$layers)
# 
# pgg4 <- suppressMessages( plotly::ggplotly(gg3) ) 
# pgg4_ <- plotly::layout(pgg4, showlegend = F, 
#                            yaxis = list(showgrid = F, showline = F),
#                            xaxis = 
#                              list(scaleanchor = "y", scaleratio = 1, 
#                                   showgrid = F, showline = F)
# )
# pgg4_
