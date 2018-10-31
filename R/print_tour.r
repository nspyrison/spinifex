#' Render display of a provided tour path and data
#'
#' Takes the result of tourr::save_history and data, returns a plotly object.
#'
#' @param tour_path The result of tourr::save_history
#' @param data Optional, tour::interpolate()'s the data. p must match the 
#'   tour_path.
#' @export
#' @examples
#' data(flea)
#' flea_std <- tourr::rescale(flea[,1:6])
#' tpath <- tourr::save_history(flea_std, tourr::guided_tour(tourr::cmass))
#' 
#' print_tour(tour_path = tpath, data = flea_std)
print_tour <- function(tour_path = NULL,
                       data = NULL) {
  p <- nrow(tour_path[,, 1])
  if (!is.null(data)) n <- nrow(data)
  # Assertions
  if (!is.null(data)) stopifnot(ncol(data) == p)
  
  attr(tour_path, "class") <- NULL # remove history_array attr
  tour_path <- as.array(tour_path[,,]) # remove data attr, possibly more.
  n_slides     <- dim(tour_path)[3]
  data_slides  <- NULL
  bases_slides <- NULL
  for (i in 1:n_slides) {
    if (!is.null(data)) {
      d <- tibble::as_tibble(data %*% tour_path[,, i])
      d$slide <- i
      data_slides <- dplyr::bind_rows(data_slides, d)
    }
    b <- tibble::as_tibble(tour_path[,, i])
    b$slide <- i
    b$lab_abbr <- lab_abbr
    bases_slides <- dplyr::bind_rows(bases_slides, b)
  }
  
  for (i in 1:n_slides) {
    if (!is.null(data)) {
      d <- tibble::as_tibble(data %*% tour_path[,, i])
      d$slide <- i
      data_slides <-  dplyr::bind_rows(data_slides, d)
    }
    b <- tibble::as_tibble(tour_path[,, i])
    b$slide <- i
    b$lab_abbr <- lab_abbr
    bases_slides <- dplyr::bind_rows(bases_slides, b)
  }
  
  # Initialize circle for the axes reference frame.
  angle    <- seq(0, 2 * pi, length = 360)
  circ     <- data.frame(x = cos(angle), y = sin(angle))
  if (!is.null(data)) {lab_abbr <- abbreviate(colnames(data), 3)
  } else lab_abbr <- paste0("V", 1:nrow(tour_path))
  
  ### Graphics
  # Reference frame circle
  gg1 <- ggplot2::ggplot() + ggplot2::geom_path(
    data = circ, color = "grey80", size = .3, inherit.aes = FALSE,
    ggplot2::aes(x = x, y = y)
  )
  
  # Reference frame text and axes
  gg2 <- suppressWarnings( # suppress to ignore unused aes "frame"
    gg1 + ggplot2::geom_text(
      data = bases_slides, size = 4, hjust = 0, vjust = 0,
      ggplot2::aes(x = V1, y = V2, label = lab_abbr, frame = slide)
    ) +
    ggplot2::geom_segment(
      data = bases_slides, size = .3,
      ggplot2::aes(x = V1, y = V2, xend = 0, yend = 0, frame = slide)
    )
  ) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_fixed(ratio = 1)
  
  if (!is.null(data)) {
    # data scatterplot
    gg3 <- gg2 + suppressWarnings( # suppress to ignore unused aes "frame"
      ggplot2::geom_point(data = data_slides, size = .7,
                          ggplot2::aes(x = V1, y = V2, frame = slide) ) )
    slideshow      <- plotly::ggplotly(gg3)
  } else slideshow <- plotly::ggplotly(gg2)
  
  return(slideshow)
}