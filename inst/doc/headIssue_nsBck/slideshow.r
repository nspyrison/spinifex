# Display the projected data and basis
#
# Takes the result of data_proj() and plotly (default) or gganimate
# to display and manually controlled tour.
#
# @param proj_list The output of data_proj(), list of two arrays: 
# projected data and axes by index.
# @param col COLo(u)r of the data points, expects 1 color or a color vector 
# with the same length equal to the number of rows (observations) in the data.
# @param pch Point CHaracter of the data points.
# @param gganimate Use gganimate graphics as opposed plotly. Defaults to FALSE.
# @import ggplot2
# @import ggthemes
# @import plotly
# @import gganimate
# 
# @examples
# data(flea)
# data <- spinifex::rescale01(flea[, 1:6]) # standardize flea data.
# p <- ncol(data)
# ThisBasis <- create_random_basis(p = p)
# 
# ThisProj <-
#   proj_data(
#     data = data,
#     basis = ThisBasis,
#     manip_var = 4,
#     manip_type = "radial",
#     phi_from = 0,
#     phi_to = pi,
#     n_slides = 20
#   )
#   
# pch <- flea$species # Point CHaracter
# col <- flea$species # COLor, COLour
# slideshow(ThisProj, col = col, pch = pch)
# @export


slideshow <- function(proj_list, col = "black", pch = "", gganimate = FALSE) {
  stopifnot(is.list(proj_list))
  stopifnot(length(proj_list) == 2)
  
  ### col and pch handling
  nrow_data <- sum(proj_list[[1]][4]==1)
  len_col <- length(col)
  if (!(len_col == 1 | len_col == nrow_data | 
      len_col == nrow(proj_list$proj_data) 
  ) )
    stop("length(col) expected as 1, nrow(data), or nrow(proj_data)")
  if (len_col == 1 | len_col == nrow_data)
    {col <- rep(col, nrow(proj_list$proj_data) %/% len_col)}
  
  len_pch <- length(pch)
  if (!(len_pch == 1 | len_pch == nrow_data | 
        len_pch == nrow(proj_list$proj_data) 
        ) 
      )
    stop("length(pch) expected as 1, nrow(data), or nrow(proj_data)")
  if (len_pch == 1 | len_pch == nrow_data)
    pch <- rep(pch, nrow(proj_list$proj_data) %/% len_pch)
  if (!is.character(pch) & length(unique(pch)) < 30) pch <- as.character(pch)
  
  ### initialise 
  proj_data  <- as.data.frame(proj_list$proj_data)
  proj_data$col <- col
  proj_data$pch <- pch
  proj_basis <- as.data.frame(proj_list$proj_basis)
  
  angle <- seq(0, 2 * pi, length = 150)
  circ <- as.data.frame(cbind("x" = cos(angle), "y" = sin(angle) ) )
  lab <- as.character(proj_basis$var_name)
  lab <- paste0(substr(lab, 1, 2), substr(lab, nchar(lab), nchar(lab))) 
    # first 2 char | last char of the column name.
  
  ### Display 
  #   plotly doesn't accept aspect.ratio, 
  #   plotly required frame to be in a geom_*(aes()).
  #
  # data. ggplot2
  gg1 <- ggplot2::ggplot(data = proj_data, ggplot2::aes(x = x, y = y) ) +
    suppressWarnings( # to suppress to ignore unused aes "frame"
      ggplot2::geom_point(size = .7,
                          ggplot2::aes(frame = index, color = col, shape = pch) 
      )
    ) + ggplot2::theme_void() + ggplot2::theme(legend.position = "none") 
    ggplot2::coord_fixed() + ggplot2::scale_color_brewer(palette = "Dark2") +
  
  # axes and labels. ggplot2
  gg2 <- suppressWarnings( # suppress to ignore unused aes "frame"
    gg1 + ggplot2::geom_text(data = proj_basis, #color = I(proj_basis$color), #TODO
                         phi = proj_basis$phi, size = 4, hjust = 0, vjust = 0,
                         ggplot2::aes(x = x, y = y, frame = index, label = lab)
    ) + ggplot2::geom_segment(data = proj_basis, size = .3, #color = , #TODO
                              ggplot2::aes(x = x, y = y, xend = 0, yend = 0, 
                                           frame = index)
    )
  )
  
  # axes circle. ggplot2
  gg3 <- gg2 + 
    ggplot2::geom_path(data = circ, color = "grey80", size = .3,
                       ggplot2::aes(x, y) )
  gg3$layers <- rev(gg3$layers)
  
  # plotly display. plotly
  pgg4 <- suppressMessages( plotly::ggplotly(gg3) ) 
  slideshow <- ploty::layout(pgg4, showlegend = F, 
                             yaxis = list(showgrid = F, showline = F),
                             xaxis = 
                               list(scaleanchor = "y", scaleratio = 1, 
                                    showgrid = F, showline = F)
  )
  
  stopifnot(is.list(slideshow))
  stopifnot(length(slideshow) == 8)
  return(slideshow)
}


