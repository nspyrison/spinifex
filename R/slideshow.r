#' Slide show of the projected data and basis.
#'
#' Takes the result of data_proj() and uses base graphics to view each index with delay in base grahics
#'
#' @param data [n, p] dim data to project, consisting of 
#'    only numeric variables (for coercion into matrix.)
#' @param bases the output of manual_tour(), list of projection bases by index.
#' @import ggplot2
#' @import ggthemes
#' @import plotly
#' @export
#' @examples
#' 
#' require(tourr)
#' data(flea)
#' flea_std <- rescale(flea[,1:6])
#' 
#' rb <- basis_random(n = ncol(flea_std), d=2)
#' prj <- manual_tour(rb, manip_var=4, manip_type = "radial",
#'  phi_from = 0, phi_to = pi, n_slides = 20)
#' slideshow(flea_std, prj)
#' 
slideshow <- function(data, bases, ...) {
  #stopifnot(is.list(proj_list))
  #stopifnot(length(proj_list) == 2)
  
  ### COL AND PCH HANDLING
  #nrow_data <- sum(proj_list[[1]][4]==1)
  #len_col <- length(col)
  #if (!(len_col == 1 | len_col == nrow_data | 
  #    len_col == nrow(proj_list$proj_data) 
  #) )
  #  stop("length(col) expected as 1, nrow(data), or nrow(proj_data)")
  #if (len_col == 1 | len_col == nrow_data)
  #  {col <- rep(col, nrow(proj_list$proj_data) %/% len_col)}
  
  #len_pch <- length(pch)
  #if (!(len_pch == 1 | len_pch == nrow_data | 
  #      len_pch == nrow(proj_list$proj_data) 
  #      ) 
  #    )
  #  stop("length(pch) expected as 1, nrow(data), or nrow(proj_data)")
  #if (len_pch == 1 | len_pch == nrow_data)
  #  pch <- rep(pch, nrow(proj_list$proj_data) %/% len_pch)
  #if (!is.character(pch) & length(unique(pch)) < 30) pch <- as.character(pch)

  # Check that data dimensions equal projection dimensions
  stopifnot(ncol(data) == nrow(bases[,,1]))
  
  ### INITIALIZE
  #proj_data  <- as.data.frame(proj_list$proj_data)
  #proj_data$col <- col
  #proj_data$pch <- pch
  #proj_basis <- as.data.frame(proj_list$proj_basis)
  #proj_basis <- proj_basis[order(row.names(proj_basis), proj_basis[, 4]),]
  
  # Create a full circle to bound the axes representation
  angle <- seq(0, 2 * pi, length = 150)
  circ <- tibble::tibble(x = cos(angle), y = sin(angle))
  lab <- colnames(data)
  lab_abbr <- paste0(substr(lab, 1, 2), substr(lab, nchar(lab), nchar(lab)))
  
  
  # Generate the projected data, indexed by frame
  nframes <- dim(bases)[3]
  proj_data <- NULL
  proj_bases <- NULL
  for (i in 1:nframes) {
    pd <- tibble::as_tibble(data %*% bases[,,i])
    pd$indx <- i
    proj_data <- dplyr::bind_rows(proj_data, pd)
    pb <- tibble::as_tibble(bases[,,i])
    pb$indx <- i
    pb$lab_abbr <- lab_abbr
    proj_bases <- dplyr::bind_rows(proj_bases, pb)
  }
  
  ### PLOTTING #frame needs to be in a geom_(aes()).
  # data
  #gg1 <- ggplot2::ggplot(data = proj_data, ggplot2::aes(x = x, y = y) ) +
  #  suppressWarnings( # suppress to ignore unused aes "frame"
  #    ggplot2::geom_point(size = .7,
  #                        ggplot2::aes(frame = index, color = col, shape = pch) 
  #    )
  #  ) + theme_void() + 
  #  ggplot2::coord_fixed() + ggplot2::scale_color_brewer(palette = "Dark2") +
  #  ggplot2::theme(legend.position = "none") 
  #+ ggplot2::theme(aspect.ratio = 1) #ggplot a.ratio doesn't work in plotly
  
  # basis text and axes
  gg2 <- suppressWarnings( # suppress to ignore unused aes "frame"
    #gg1 + ggplot2::geom_text(data = bases,
    ggplot() + ggplot2::geom_text(data = proj_bases,
                         #phi = proj_basis$phi,
                         size = 4, 
                         hjust = 0, 
                         vjust = 0,
                         #color = I(proj_basis$color),
                         ggplot2::aes(x = V1, y = V2, frame = indx, 
                                      label = lab_abbr)
                         ) +
      ggplot2::geom_segment(data = proj_bases,
                            size = .3,
                            #color = I(proj_basis$color),
                            ggplot2::aes(x = V1, y = V2, xend = 0, yend = 0, 
                                         frame = indx)
                            )
    )
  
  # axes circle
  gg3 <- gg2 + ggplot2::geom_path(data = circ, color = "grey80", size = .3,
                       ggplot2::aes(x, y)
                       )
  gg3$layers <- rev(gg3$layers)
  
  pgg4 <- suppressMessages( 
    plotly::ggplotly(gg3)
    #, tooltip = F, colors = "Dark2", color = ~proj_data$col) #done in ggplot
    ) 
  slideshow <- 
    layout(pgg4, showlegend = F, 
           yaxis = list(showgrid = F, showline = F),
           xaxis = 
             list(scaleanchor = "y", scaleratio = 1, showgrid = F, showline = F)
         )

  stopifnot(is.list(slideshow))
  stopifnot(length(slideshow) == 8)
  return(slideshow)
}


