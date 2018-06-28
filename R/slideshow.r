#' Slide show of the projected data and basis.
#'
#' Takes the result of data_proj() and uses base graphics to view each index with delay in base grahics
#'
#' @param proj_list the output of data_proj(), list of projected data and basis by index.
#' @param col color of the data, 1 color or length of the data
#' @import ggplot2
#' @import plotly
#' @export
#' @examples
#' 
#' data <- flea[, 1:5]
#' proj <- proj_data(data, manip_var=2)
#' slideshow(proj)
#' 
#' pal <- rainbow(length(levels(flea$species)))
#' col <- pal[as.numeric(flea$species)]
#' proj <- proj_data(data, manip_var="head")
#' slideshow(proj, col=col)

slideshow <- function(proj_list, col = "black", pch = "a") {
  stopifnot(is.list(proj_list))
  stopifnot(length(proj_list) == 2)
  if (length(col) != 1 & nrow(proj_list$proj_data) %% length(col) == 0)
    {col <- rep(col, nrow(proj_list$proj_data) / length(col))}
  stopifnot(length(col) == 1 | length(col) == nrow(proj_list$proj_data))
  if (length(pch) != 1 & nrow(proj_list$proj_data) %% length(pch) == 0)
    {pch <- rep(pch, nrow(proj_list$proj_data) / length(pch))}
  stopifnot(length(pch) == 1 | length(pch) == nrow(proj_list$proj_data))
  
  ### INITIALIZE
  proj_data <- proj_list$proj_data
  proj_data <- as.data.frame(proj_data)
  proj_basis <- proj_list$proj_basis
  proj_basis <- proj_basis[order(row.names(proj_basis), proj_basis[, 4]),]
  proj_basis <- as.data.frame(proj_basis)
  
  angle <- seq(0, 2 * pi, length = 150)
  circ <- as.data.frame(cbind("x" = cos(angle), "y" = sin(angle) ) )
  lab <- as.character(proj_basis$var_name)
  label <- paste0(substr(lab, 1, 2), substr(lab, nchar(lab), nchar(lab)))
  
  
  ### PLOTTING #frame needs to be in a geom_(aes()).
  # data
  gg1 <- ggplot2::ggplot(data = proj_data, 
                         ggplot2::aes(x = x, y = y)
                         ) +
    suppressWarnings( # suppress to ignore unused aes "frame"
      ggplot2::geom_point(size = .7,
                          ggplot2::aes(frame = index, shape = pch,color = col)
      )
    ) + 
    ggplot2::ylab("") + ggplot2::xlab("") + ggplot2::coord_fixed() 

  # basis text and axes
  gg2 <- suppressWarnings(# suppress to ignore unused aes "frame"
    gg1 +
      ggplot2::geom_text(data = proj_basis,
                         phi = proj_basis$phi,
                         color = "grey50",
                         size = 4, 
                         hjust = 0, 
                         vjust = 0,
                         ggplot2::aes(x = x, y = y, frame = index, label = label)
      ) +
      ggplot2::geom_segment(data = proj_basis,
                            color = "grey70",
                            size = .3,
                            ggplot2::aes(x = x, y = y, xend = 0, yend = 0, 
                                         frame = index)
      )
  )
  
  # axes circle
  gg3 <- gg2 +
    ggplot2::geom_path(data = circ,
                       color = "grey80",
                       size = .3,
                       ggplot2::aes(x, y)
    )

  gg3$layers <- rev(gg3$layers)
  slideshow <- plotly::ggplotly(gg3) #, yaxis())
  # layout(slideshow)
  # ,
  #   title = "fixed-ratio axes",
  #   yaxis = list(scaleanchor = "x")
  #   )
  
  
  stopifnot(is.list(slideshow))
  stopifnot(length(slideshow) == 8)
  return(slideshow)
}


