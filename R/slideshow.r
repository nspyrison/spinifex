#' Create a slideshow array of the projected bases
#'
#' Takes the result of manual_tour() and interpolated the data over the 
#' the tour path of the reference frame.
#'
#' @param data [n, p] dim data to project, consisting of 
#'    only numeric variables (for coercion into matrix.)
#' @param m_tour the output of manual_tour(), list of projection bases by index.
#' @param center set the mean of the projected data to be a vector of zeros. 
#' This stops the data from wandering around the display window. Default=TRUE.
#' @param scale set the scale of the projected data to be in a standard range 
#' for all projections. Typically not useful, but occasionally we are only 
#' interested in the shape of projected data not the magnitude. Default=FALSE.
#' @export
#' @examples
#' data(flea)
#' flea_std <- tourr::rescale(flea[,1:6])
#' 
#' rb <- tourr::basis_random(n=ncol(flea_std))
#' mtour <- manual_tour(rb, manip_var = 4)
#' sshow <- create_slideshow(data = flea_std, m_tour = mtour)
create_slideshow <- function(data, m_tour, center = TRUE, scale = FALSE){
  # Assertions
  if (!is.matrix(data)) data <- as.matrix(data)
  stopifnot(is.array(m_tour))
  stopifnot(ncol(data) == nrow(m_tour[,, 1]))
  
  # Generate the projected data by slide
  n            <- nrow(data)
  p            <- ncol(data)
  n_slides     <- dim(m_tour)[3]
  manip_var    <- attributes(m_tour)$manip_var
  lab_abbr     <- abbreviate(colnames(data), 3)
  data_slides  <- NULL
  bases_slides <- NULL
  for (i in 1:n_slides) {
    d <- tibble::as_tibble(data %*% m_tour[,, i])
    d$slide <- i
    data_slides <- dplyr::bind_rows(data_slides, d)
    b <- tibble::as_tibble(m_tour[,, i])
    b$slide <- i
    b$lab_abbr <- lab_abbr
    bases_slides <- dplyr::bind_rows(bases_slides, b)
  }
  
  slide_deck <- list(data_slides, bases_slides)
  return(slide_deck)
}

#' Render a slideshow of the toured data and bases
#'
#' Takes the result of create_slideshow() and renders them as a graph object of 
#' the `disp_type`. 
#'
#' @param slide_deck The result of create_slideshow().
#' @param disp_type The graphics system to use. Defaults to 'plotly'.
#' @export
#' @examples
#' data(flea)
#' flea_std <- tourr::rescale(flea[,1:6])
#' 
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' mtour <- manual_tour(basis = rb, manip_var = 4)
#' sshow <- create_slideshow(data = flea_std, m_tour = mtour)
#' render_slideshow(slide_deck = sshow)
render_slideshow <- function(slide_deck,
                             disp_type = "plotly" # alt: "gganimate", "animate"
) {
  # Assertions
  stopifnot(disp_type %in% c("plotly", "gganimate", "animate") )
  
  data_slides       <- slide_deck[[1]]
  bases_slides      <- slide_deck[[2]]
  nrow_data         <- nrow(data_slides[data_slides$slide == 1,])
  nrow_data_slides  <- nrow(data_slides)
  
  # Initialize circle for the axes reference frame.
  angle    <- seq(0, 2 * pi, length = 360)
  circ     <- data.frame(x = cos(angle), y = sin(angle))
  lab_abbr <- abbreviate(colnames(data_slides), 3)
  
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
  
  # data scatterplot
  gg3 <- gg2 + suppressWarnings( # suppress to ignore unused aes "frame"
    ggplot2::geom_point(data = data_slides, size = .7,
                        ggplot2::aes(x = V1, y = V2, frame = slide) )
    )
  
  if (disp_type == "plotly") {
    slideshow <- plotly::ggplotly(gg3)
  } else stop("disp_types other than `plotly` not yet implemented.")
  
  return(slideshow)
}