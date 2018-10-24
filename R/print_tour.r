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
  p <- nrow(tour_path[,,1])
  if (!is.null(data)) n <- nrow(data)
  # Assertions
  if (!is.null(data)) stopifnot(ncol(data) == p)
  
  tour_path    <- as.array(tour_path)
  tour_path    <- attr(tour_path, "ATT") <- NULL
  n_slides     <- dim(tour_path)[3]
  data_slides  <- NULL
  bases_slides <- NULL
  if (!is.null(data)) {lab_abbr <- abbreviate(colnames(data), 3)
  #
  #
  #
  } else lab_abbr <- paste0("V", 1:nrow(tour_path))
  for (i in 1:n_slides) { #Error in 1:n_slides : argument of length 0.
    paste(i) 
    i+1
  }
  NULL
  #
  #
  #
  # for (i in 1:n_slides) {
  #   if (!is.null(data)) {
  #     message(dim(data))
  #     message(dim(tour_path[,,i]))
  #     d <- tibble::as_tibble(data %*% tour_path[,,i])
  #     d$slide <- i
  #     data_slides <-  dplyr::bind_rows(data_slides, d)
  #   }
  #   b <- tibble::as_tibble(tour_path[,,i])
  #   b$slide <- i
  #   b$lab_abbr <- lab_abbr
  #   bases_slides <- dplyr::bind_rows(bases_slides, b)
  # }
  # 
  # bases_slides$norm <- sqrt(bases_slides[1]^2 + bases_slides[2]^2)
  # max_norm          <- max(bases_slides$norm)
  # 
  # # Initialize circle for the axes reference frame.
  # angle    <- seq(0, 2 * pi, length = 360)
  # circ     <- data.frame(x = cos(angle), y = sin(angle))
  # lab_abbr <- abbreviate(colnames(data_slides), 3)
  # 
  # ### Graphics
  # # Reference frame circle
  # gg1 <- ggplot2::ggplot2() + ggplot2::geom_path(
  #   data = circ, color = "grey80", size = .3, inherit.aes = FALSE, 
  #   ggplot2::aes(x = x, y = y)
  # ) + ggplot2::geom_path(
  #   data = circ * max_norm, color = "grey80", size = .3, inherit.aes = FALSE, 
  #   ggplot2::aes(x = x, y = y)
  # )
  # 
  # # Reference frame text and axes
  # gg2 <- suppressWarnings( # suppress to ignore unused aes "frame"
  #   gg1 + ggplot2::geom_text(
  #     data = tour_path, size = 4, hjust = 0, vjust = 0,
  #     ggplot2::aes(x = V1, y = V2, label = lab_abbr, frame = slide)
  #   ) +
  #   ggplot2::geom_segment(
  #     data = tour_path, size = .3,
  #     ggplot2::aes(x = tour_pathV1, y = V2, xend = 0, yend = 0, frame = slide)
  #   ) 
  # ) + 
  #   ggplot2::scale_color_brewer(palette = "Dark2") +
  #   ggplot2::theme_void() +
  #   ggplot2::theme(legend.position = "none") +
  #   ggplot2::coord_fixed(ratio = 1)
  # 
  # if (!is.null(data)) {
  #   # data scatterplot
  #   gg3 <- gg2 + suppressWarnings( # suppress to ignore unused aes "frame"
  #     ggplot2::geom_point(data, size = .7,
  #                         ggplot2::aes(x = V1, y = V2, frame = slide) ) )
  # 
  #   slideshow      <- plotly::ggplotly(gg3)
  # } else slideshow <- plotly::ggplotly(gg2)
  # return(slideshow)
}