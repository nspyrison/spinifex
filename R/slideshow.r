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
#' create_slideshow(data = flea_std, m_tour = mtour)
create_slideshow <- function(data, m_tour, center = TRUE, scale = FALSE) {
  # Assertions
  if (!is.matrix(data)) data <- as.matrix(data)
  stopifnot(is.matrix(data))
  stopifnot(is.array(m_tour))
  stopifnot(ncol(data) == nrow(m_tour[,,1]))
  
  n_slides   <- dim(m_tour)[3]
  
  # Generate the projected data by slide
  lab_abbr     <- abbreviate(colnames(data), 3)
  data_slides  <- NULL
  bases_slides <- NULL
  for (i in 1:n_slides) {
    d <- tibble::as_tibble(data %*% m_tour[,,i])
    d$slide <- i
    data_slides <- dplyr::bind_rows(data_slides, d)
    b <- tibble::as_tibble(m_tour[,,i])
    b$slide <- i
    b$lab_abbr <- lab_abbr
    bases_slides <- dplyr::bind_rows(bases_slides, b)
  }
  
  # Conserve tour attributes
  manip_var  <- attributes(m_tour)$manip_var
  # manip_type <- attributes(m_tour)$manip_type
  # phi        <- attributes(m_tour)$phi
  # theta      <- attributes(m_tour)$theta
  data_slides$manip_var  <- manip_var
  # data_slides$manip_type <- manip_type
  # data_slides$theta      <- theta
  # data_slides$phi        <- phi[rep(seq_len(length(phi)), each=nrow(data))]
    #error here, does rep, but falls short. of data slides, 
    ##why are there nulls after 11? look at the create on the steps..
  
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
#' render_slideshow(slide_deck = sshow, group_by = flea$species)
render_slideshow <- function(slide_deck,
                             group_by = NULL,
                             col = NULL,
                             pch = NULL,
                             disp_type = "plotly"
                             # TODO: add "gganimate", "animate"
) {
  # Assertions
  stopifnot(disp_type %in% c("plotly", "gganimate", "animate") )
  arb_n_grps_cap <- ncol(slide_deck[1]) / 10
  if (!is.null(group_by))
    stopifnot(length(unique(group_by)) <= arb_n_grps_cap)
  if (is.null(group_by) & !is.null(col))
    stopifnot(length(unique(col))      <= arb_n_grps_cap)
  if (is.null(group_by) & !is.null(pch))
    stopifnot(length(unique(pch))      <= arb_n_grps_cap)
  
  # Handling group_by, col, and pch (colo(u)r and point character respectively)
  if (!is.null(group_by) & (!is.null(col) | !is.null(pch)) )
    message("Non-null group_by used with non-null col or  non-null pch. Using group_by over col and pch.")
  if (!is.null(group_by)) {
    col <- group_by
    pch <- group_by
  }
  
  # Initialization 
  len_col          <- length(col)
  len_pch          <- length(pch)
  data_slides      <- slide_deck[[1]]
  bases_slides     <- slide_deck[[2]]
  nrow_data        <- nrow(data_slides[data_slides$slide == 1,])
  nrow_data_slides <- nrow(data_slides)
  col <- rep(col, nrow_data_slides / len_col)
  pch <- rep(pch, nrow_data_slides / len_pch)
  
  # Post-initialization assertions
  if (!is.null(group_by) & 
      !(len_col == 1 | len_col == nrow_data | len_col == nrow_data_slides) )
    stop("length(col) expected as 1, nrow(data), or nrow(data_slides)")
  if (!is.null(group_by) & 
      !(len_pch == 1 | len_pch == nrow_data | len_pch == nrow_data_slides) )
    stop("length(pch) expected as 1, nrow(data), or nrow(data_slides)")
  if (!is.character(pch)) pch <- as.character(pch)
  
  # Initialize graphics values
  data_slides$col   <- col
  data_slides$pch   <- pch
  angle    <- seq(0, 2 * pi, length = 360)
  circ     <- tibble::as_tibble(x = cos(angle), y = sin(angle))
  lab_abbr <- abbreviate(colnames(data_slides), 3)
  # bases_slides      <-
  #  bases_slides[order(as.vector(bases_slides[, 3]), as.vector(bases_slides[, 4])),]
  ## Do we need the above 2 lines? why?
  
  ### Graphics   # Plotly frame throws warning when in geom_x(aes(frame=slide))
  gg1 <- ggplot2::ggplot(data = data_slides, 
                         ggplot2::aes(
                           x = V1, y = V2, frame = slide, 
                           tt1 = manip_var, tt2 = manip_type, 
                           tt3 = theta, tt4 = phi)
  ) +
    ggplot2::geom_point(size = .7, ggplot2::aes(color = col, shape = pch) ) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme_void() + # Doesn't remove legend.
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_fixed(ratio = 1)
    
  # Reference and axes
  gg2 <- 
    gg1 + suppressWarnings( # for 'Ignoring unknown aesthetics: frame' warnings.
      ggplot2::geom_text(
        data = bases_slides, size = 4, hjust = 0, vjust = 0,
        ggplot2::aes(x = V1, y = V2, label = lab_abbr, frame = slide)
      ) + 
        ggplot2::geom_segment(
          data = bases_slides, size = .3, color = col,
          ggplot2::aes(x = V1, y = V2, xend = 0, yend = 0, frame = slide)
        )
    )
  
  # Reference frame circle
  gg3 <- gg2 + ggplot2::geom_path(
    data = circ, color = "grey80", size = .3, ggplot2::aes(x, y)
  )
  gg3$layers <- rev(gg3$layers) # Reverse layers for desired overlaping.
  
  if (disp_type == "plotly") {
    pgg4 <- suppressMessages(
      plotly::ggplotly(gg3)
    )
    slideshow <-
      layout(
        pgg4, showlegend = F, yaxis = list(showgrid = F, showline = F),
        xaxis = 
          list(scaleanchor = "y", scaleratio = 1, showgrid = F, showline = F)
      )
  } else stop("disp_types other than `plotly` not yet implemented.")
  
  return(slideshow)
}