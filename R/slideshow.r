#' Slide show of the projected data and basis.
#'
#' Takes the result of data_proj() and uses base graphics to view each index with delay in base grahics
#'
#' @param proj_list the output of data_proj(), list of projected data and basis by index.
#' @param col color of the data, 1 color or length of the data
#' @import ggplot2
#' @import ggthemes
#' @import plotly
#' @export
#' @examples
#' 
#' flea_std <- 
#'   apply(flea[,1:6], 2, function(x) ((x-mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)))
#' data <- flea_std
#' 
#' proj1 <- proj_data(data, manip_var=3)
#' slideshow(proj1)
#' 
#' p <- ncol(data) 
#' r_basis <- create_random_basis(p = p)
#' pch <- flea$species
#' col <- flea$species
#' 
#' proj2 <-
#'   proj_data(
#'     data = data,
#'     basis = r_basis,
#'     manip_var = 3,
#'     manip_type = "radial",
#'     phi_from = 0,
#'     phi_to = pi,
#'     n_slides = 20
#'   )
#' slideshow(proj2, col = col, pch = pch)


slideshow <- function(proj_list, col = "black", pch = "") {
  stopifnot(is.list(proj_list))
  stopifnot(length(proj_list) == 2)
  
  ### COL AND PCH HANDLING
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
  
  ### INITIALIZE
  proj_data  <- as.data.frame(proj_list$proj_data)
  proj_data$col <- col
  proj_data$pch <- pch
  proj_basis <- as.data.frame(proj_list$proj_basis)
  proj_basis <- proj_basis[order(row.names(proj_basis), proj_basis[, 4]),]
  
  angle <- seq(0, 2 * pi, length = 150)
  circ <- as.data.frame(cbind("x" = cos(angle), "y" = sin(angle) ) )
  lab <- as.character(proj_basis$var_name)
  label <- paste0(substr(lab, 1, 2), substr(lab, nchar(lab), nchar(lab)))
  
  
  ### PLOTTING #frame needs to be in a geom_(aes()).
  # data
  gg1 <- ggplot2::ggplot(data = proj_data, ggplot2::aes(x = x, y = y) ) +
    suppressWarnings( # suppress to ignore unused aes "frame"
      ggplot2::geom_point(size = .7,
                          ggplot2::aes(frame = index, color = col, shape = pch) 
      )
    ) + theme_void() + 
    ggplot2::coord_fixed() + ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme(legend.position = "none") 
  #+ ggplot2::theme(aspect.ratio = 1) #ggplot a.ratio doesn't work in plotly
  
  # basis text and axes
  gg2 <- suppressWarnings( # suppress to ignore unused aes "frame"
    gg1 + ggplot2::geom_text(data = proj_basis,
                         phi = proj_basis$phi,
                         size = 4, 
                         hjust = 0, 
                         vjust = 0,
                         color = I(proj_basis$color),
                         ggplot2::aes(x = x, y = y, frame = index, 
                                      label = label)
                         ) +
      ggplot2::geom_segment(data = proj_basis,
                            size = .3,
                            color = I(proj_basis$color),
                            ggplot2::aes(x = x, y = y, xend = 0, yend = 0, 
                                         frame = index)
                            )
    )
  
  # axes circle
  gg3 <- gg2 + ggplot2::geom_path(data = circ, color = "grey80", size = .3,
                       ggplot2::aes(x, y)
                       )
  gg3$layers <- rev(gg3$layers)
  
  pgg4 <- suppressMessages( 
    plotly::ggplotly(gg3)
<<<<<<< HEAD
    ) 
  slideshow <- 
    layout(pgg4, showlegend = F, tooltip = F, hoverinfo = "none",
=======
<<<<<<< HEAD
    ) 
  slideshow <- 
    layout(pgg4, showlegend = F, #tooltip = F, hoverinfo = "none",
=======
    #, tooltip = F, colors = "Dark2", color = ~proj_data$col) #done in ggplot
    ) 
  slideshow <- 
    layout(pgg4, showlegend = F, 
>>>>>>> 08c6ad5ee37565db2ed43ba5c35426f5844bae14
>>>>>>> 539529f3c5b36e3d663a51e988819740487c9f45
           yaxis = list(showgrid = F, showline = F),
           xaxis = 
             list(scaleanchor = "y", scaleratio = 1, showgrid = F, showline = F)
         )

  stopifnot(is.list(slideshow))
  stopifnot(length(slideshow) == 8)
  return(slideshow)
}


