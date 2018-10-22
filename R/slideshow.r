#' Create a slideshow array of the projected bases
#'
#' Takes the result of manual_tour() and uses base graphics to view each index with delay in base grahics
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
  stopifnot(is.matrix(as.matrix(data)))
  stopifnot(is.array(m_tour))
  stopifnot(ncol(data) == nrow(m_tour[,,1]))
  if (!is.matrix(data)) data <- as.matrix(data)
  
  p <- ncol(m_tour[,,1])
  n <- nrow(m_tour[,,1])
  
  # Generate the projected data by slide
  n_slides     <- dim(m_tour)[3]
  manip_var    <- attributes(m_tour)$manip_var
  lab_abbr     <- abbreviate(colnames(data), 3)
  data_slides  <- NULL
  bases_slides <- NULL
  for (i in 1:n_slides) {
    d <- tibble::as_tibble(data %*% m_tour[,,i])
    d$slide <- i
    data_slides <-  dplyr::bind_rows(data_slides, d)
    b <- tibble::as_tibble(m_tour[,,i])
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
#' @param data [n, p] dim data to project, consisting of 
#'    only numeric variables (for coercion into matrix.)
#' @export
#' @examples
#' data(flea)
#' flea_std <- tourr::rescale(flea[,1:6])
#' 
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' mtour <- manual_tour(basis = rb, manip_var = 4)
#' sshow <- create_slideshow(flea_std, mtour)
#' render_slideshow(slide_deck = sshow)
render_slideshow <- function(slide_deck,
                             group_by = NULL,
                             #col = NULL,
                             #pch = NULL,
                             disp_type = "plotly"# c("plotly", "gganimate", "animate")
) {
  # Assertions
  stopifnot(disp_type %in% c("plotly", "gganimate", "animate") )
  if (!is.null(group_by))
    stopifnot(length(unique(group_by)) <= ncol(slide_deck[1]) / 10)
  #if (is.null(group_by) & !is.null(col))
  #  stopifnot(length(unique(col)) <= ncol(slide_deck[1]) / 10)
  #if (is.null(group_by) & !is.null(pch))
  #  stopifnot(length(unique(pch)) <= ncol(slide_deck[1]) / 10)
  
  data_slides      <- slide_deck[[1]]
  bases_slides     <- slide_deck[[2]]
  nrow_data        <- nrow(data_slides[data_slides$slide == 1,])
  nrow_data_slides <- nrow(data_slides)
  
  # Handling group_by, col, and pch (colo(u)r and point character respectively)
  #if (!is.null(group_by) & (!is.null(col) | !is.null(pch)) )
  #  message("Non-null group_by used with non-null col or  non-null pch. Using group_by over col and pch.")
  #if (!is.null(group_by)) {
  #  col <- group_by
  #  pch <- group_by
  #}
  
  #len_col <- length(col)
  #len_pch <- length(pch)
  
  #if (!is.null(group_by) & 
   #   !(len_col == 1 | len_col == nrow_data | len_col == nrow_data_slides) )
   # stop("length(col) expected as 1, nrow(data), or nrow(data_slides)")
  #if (len_col != nrow_data_slides)
  #  col <- rep(col, nrow_data_slides / len_col)
  
  #if (!is.null(group_by) & 
  #    !(len_pch == 1 | len_pch == nrow_data | len_pch == nrow_data_slides) )
  #  stop("length(pch) expected as 1, nrow(data), or nrow(data_slides)")
  #if (len_pch != nrow_data_slides)
  #  pch <- rep(pch, nrow_data_slides / len_pch)
  #if (!is.character(pch) ) pch <- as.character(pch)
  
  ### Initialise proj_data and proj_basis
  #if (!is.null(col)) data_slides$col <- col
  #if (!is.null(pch)) data_slides$pch <- pch
  #phi                <- attributes(m_tour)$phi
  #theta              <- attributes(m_tour)$theta
  #len_phi            <- length(phi)
  #len_theta          <- length(theta)
  #data_slides$phi    <- rep(phi, nrow_data_slides / len_phi)
  #data_slides$theta  <- rep(theta, nrow_data_slides / len_theta)
  # bases_slides      <-
  #  bases_slides[order(as.vector(bases_slides[, 3]), as.vector(bases_slides[, 4])),]
    # Do we need this? why?
  
  # Initialize circle for the axes reference frame.
  angle    <- seq(0, 2 * pi, length = 360)
  circ     <- data.frame(x = cos(angle), y = sin(angle))
  lab_abbr <- abbreviate(colnames(data_slides), 3)
  
  ### Graphics #frame needs to be in a geom_(aes()) for plotly.
  gg1 <- ggplot2::ggplot(data = data_slides, ggplot2::aes(x = V1, y = V2) ) +
    suppressWarnings( # suppress to ignore unused aes "frame"
      ggplot2::geom_point(
        size = .7, ggplot2::aes(frame = slide) #, color = col, shape = pch)
      )
    ) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_fixed(ratio = 1)
    
  # basis text and axes
  gg2 <- suppressWarnings( # suppress to ignore unused aes "frame"
    gg1 + 
      ggplot2::geom_text(
        data = bases_slides, size = 4, hjust = 0, vjust = 0,
        #phi = phi, 
        ggplot2::aes(x = V1, y = V2, frame = slide, label = lab_abbr)
      ) +
      ggplot2::geom_segment(
        data = bases_slides,
        size = .3,
        #color = bases_slides$col, #I(proj_bases$col),
        ggplot2::aes(x = V1, y = V2, xend = 0, yend = 0, frame = slide)
      )
  )
  
  # axes circle
  gg3 <- gg2 + ggplot2::geom_path(
    data = circ, color = "grey80", size = .3, ggplot2::aes(x, y)
  )
  gg3$layers <- rev(gg3$layers)
  
  if (disp_type == "plotly") {
    pgg4 <- suppressMessages(
      plotly::ggplotly(gg3)
    )
    slideshow <-
      layout(
        pgg4)#, showlegend = F, yaxis = list(showgrid = F, showline = F),
        #xaxis = list(scaleanchor = "y", scaleratio = 1, showgrid = F, showline =F)
      #)
  } else stop("disp_types other than `plotly` not yet implemented.")
  
  return(slideshow)
}