#see: https://github.com/ropensci/plotly/issues/717
#try::
#update Rstudio # using newest ver as of 24/10/2018.
#update R       # using newest ver as of 24/10/2018.
#restart R then:
install.packages('digest',     dependencies = TRUE)
install.packages('rlang',      dependencies = TRUE)
install.packages('data.table', dependencies = TRUE)
devtools::install_github("ropensci/plotly", dependancies = TRUE)
  #dl  24/20/2018

# spinifex repex
library(spinifex)
data(flea)
flea_std <- tourr::rescale(flea[,1:6])
rb <- tourr::basis_random(n = ncol(flea_std))
mtour <- manual_tour(basis = rb, manip_var = 4)
sshow <- create_slideshow(data = flea_std, m_tour = mtour)

# dive into render_slideshow(). # render_slideshow(slide_deck = sshow)
slide_deck = sshow

data_slides      <- slide_deck[[1]]
bases_slides     <- slide_deck[[2]]
nrow_data        <- nrow(data_slides[data_slides$slide == 1,])
nrow_data_slides <- nrow(data_slides)
visdat::vis_dat(data_slides) # Well, this is a an issue...

DS_NAs <- as.data.frame(data_slides[is.na(data_slides),])
  # 74 NAs in slide 20, and 74 NAs in slide NA.


# Initialize circle for the axes reference frame.
angle    <- seq(0, 2 * pi, length = 360)
circ     <- data.frame(x = cos(angle), y = sin(angle))
lab_abbr <- abbreviate(colnames(data_slides), 3)

### Graphics
(gg1 <- 
  ggplot2::ggplot(data_slides, ggplot2::aes(x = V1, y = V2, frame = slide) ) +
  ggplot2::geom_point(size = .7) +
  ggplot2::scale_color_brewer(palette = "Dark2") +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::coord_fixed(ratio = 1) )

# Reference frame text and axes
(gg2 <- suppressWarnings( # suppress to ignore unused aes "frame"
  gg1 +
    ggplot2::geom_text(
      data = bases_slides, size = 4, hjust = 0, vjust = 0,
      ggplot2::aes(x = V1, y = V2, label = lab_abbr, frame = slide)
    ) +
    ggplot2::geom_segment(
      data = bases_slides, size = .3,
      ggplot2::aes(x = V1, y = V2, xend = 0, yend = 0, frame = slide)
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
(pgg3 <- plotly::ggplotly(gg2)) # after  rev(layers) # segements disapear.

