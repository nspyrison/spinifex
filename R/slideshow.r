#' Create a slideshow array of the projected bases
#'
#' Takes the result of manual_tour() and projects the data over the interpolated
#' tour path of the reference frame.
#'
#' @param data A [n, p] dim data to project, consisting of only numeric 
#'   variables (for coercion into matrix).
#' @param tour The output of manual_tour(), a [p, d, n_slides] dim array of 
#'   the manual tour. Containing `n_slides` interpolations varying phi.
#' @return A list containing the [p, d, n_slides] dim basis slides array, and
#'   the [n, d, n_slides] dim data slides array.
#' @export
#' @examples
#' data(flea)
#' flea_std <- tourr::rescale(flea[,1:6])
#' 
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' mtour <- manual_tour(rb, manip_var = 4)
#' sshow <- create_slides(tour = mtour, data = flea_std)
create_slides <- function(tour,
                          data = NULL) { 
  # Assertions
  p <- nrow(tour[,, 1])
  if (!is.null(data)) stopifnot(ncol(data) == p)
  if (!is.null(data) & !is.matrix(data)) data <- as.matrix(data)
  stopifnot(is.array(tour))
  
  # Initialize
  n_slides     <- dim(tour)[3]
  bases_slides <- NULL
  
  if(!is.null(data)) { # IF data exsits THEN:
    data_slides <- NULL
    for (slide in 1:n_slides) {
      # make bases slides, and
      curr_slide <- tibble::as_tibble(tour[,, slide])
      curr_slide$slide <- slide
      bases_slides <- rbind(bases_slides, curr_slide)
      # make data slides
      curr_slide <- tibble::as_tibble(data %*% tour[,, slide])
      curr_slide$V1 <- curr_slide$V1 - mean(curr_slide$V1)
      curr_slide$V2 <- curr_slide$V2 - mean(curr_slide$V2)
      curr_slide$slide <- slide
      data_slides <- rbind(data_slides, curr_slide)
    }
  } else {# ELSE, (if data is NULL), just:
    # make bases slides
    for (slide in 1:n_slides) {
      curr_slide <- tibble::as_tibble(tour[,, slide])
      curr_slide$slide <- slide
      bases_slides <- rbind(bases_slides, curr_slide)
    }
  }
  
  # Set labels, abbreviated
  lab_abbr <- if(!is.null(data)) {abbreviate(colnames(data), 3)
  } else paste0("V", 1:p)
  lab_abbr <- rep(lab_abbr, n_slides)
  bases_slides$lab_abbr <- lab_abbr
  
  # Keep manip_var if it's not NULL
  if (!is.null(attributes(tour)$manip_var)) {
    manip_var <- attributes(tour)$manip_var
    attr(bases_slides, "manip_var") <- manip_var
  }
  
  slides <- if(!is.null(data)) {
    list(bases_slides = bases_slides, data_slides = data_slides)
  } else list(bases_slides = bases_slides)
  
  return(slides)
}

#' Render a slideshow of the toured data and bases
#'
#' Takes `slides`, the result of create_slides(), and renders them as a graph 
#' object of the `disp_type`. 
#'
#' @param slides The result of create_slides().
#' @param disp_type The graphics system to use. Defaults to 'plotly'.
#' @param col String of the colo(u)r to highlight the `manip_var`.
#' @param ... Optional, pass addition arguments into `plotly::animation_opts()`.
#' @return An animation in `disp_type` graphics of the interpolated data and 
#'   the corrisponding reference frame.
#' @export
#' @examples
#' data(flea)
#' flea_std <- tourr::rescale(flea[,1:6])
#' 
#' rb <- tourr::basis_random(n = ncol(flea_std))
#' mtour <- manual_tour(basis = rb, manip_var = 4)
#' sshow <- create_slides(tour = mtour, data = flea_std)
#' (pss <- render_slideshow(slides = sshow))
render_slideshow <- function(slides,
                             disp_type = "plotly", # alt: "gganimate", "animate"
                             manip_col = "blue", # string of color name
                             cat_var = NULL, # cat var to color data and pch
                             ...) {
  disp_type <- tolower(disp_type)
  # Assertions
  stopifnot(disp_type %in% c("plotly", "gganimate", "animate"))
  
  # Initiliaze
  if (length(slides) == 2)
    data_slides <- slides[[2]]
  bases_slides  <- slides[[1]]
  angle         <- seq(0, 2 * pi, length = 360)
  circ          <- data.frame(x = cos(angle), y = sin(angle))
  
  ### Graphics
  # Plot reference frame circle
  gg1 <- 
    ggplot2::ggplot() + ggplot2::geom_path(
      data = circ, color = "grey80", size = .3, inherit.aes = F,
      mapping = ggplot2::aes(x = circ$x, y = circ$y)
    ) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
  
  # Given manip_var, format reference frame accordingly
  # Initialize
  n_slides  <- length(unique(bases_slides$slide))
  p         <- nrow(bases_slides[,, 1]) / n_slides
  manip_var <- attributes(bases_slides)$manip_var
  
  # Size and color of manip_var
  if(!is.null(manip_var)) {
    col_v            <- rep("black", p) # colo(u)r vector
    col_v[manip_var] <- manip_col
    col_v            <- rep(col_v, n_slides)
    siz_v            <- rep(0.3, p)
    siz_v[manip_var] <- 1
    siz_v            <- rep(siz_v, n_slides)
  } else {
    col_v <- "black"
    siz   <- 0.3
  }
  
  # Plot refrence frame axes
  gg2 <- gg1 + suppressWarnings(ggplot2::geom_segment( # for unused aes "frame".
    data = bases_slides, size = siz_v, colour = col_v,
    mapping = ggplot2::aes(x = bases_slides$V1, y = bases_slides$V2, 
                           xend = 0, yend = 0, frame = bases_slides$slide)
  ))
  
  # Refrence frame text
  gg3 <- gg2 # + suppressWarnings(ggplot2::geom_text( # for unused aes "frame".
  # data = bases_slides, size = 4, hjust = 0, vjust = 0, colour = "black",
  # mapping = ggplot2::aes(x = bases_slides$V1, y = bases_slides$V2, 
  #                        frame = bases_slides$slide, label = lab_abbr)
  # ))
  
  # Given cat_var, set group values for pch and color of data
  if(!is.null(cat_var)) {
    if (!is.numeric(cat_var)) cat_var <- as.numeric(as.factor(cat_var))
    cat_var <- rep(cat_var, n_slides)
  } else cat_var <- 1
  
  # Plot data projection scatterplot
  gg4 <- gg3 + suppressWarnings(ggplot2::geom_point( # for unused aes "frame".
    data = data_slides, size = .7, 
    color = cat_var, shape = cat_var + 15,
    mapping = ggplot2::aes(x = data_slides$V1, y = data_slides$V2,
                           frame = data_slides$slide)
  ))
  
  # Render as disp_type
  if (disp_type == "plotly") {
    pgg4 <- plotly::ggplotly(gg4)
    pgg4 <- plotly::animation_opts(p = pgg4,
                                   frame = 200, transition = 0, redraw = FALSE)
    slideshow <- plotly::layout(
      pgg4, showlegend = F, yaxis = list(showgrid = F, showline = F),
      xaxis = list(scaleanchor = "y", scaleratio = 1, showgrid = F, showline =F)
    )
  } else stop("disp_types other than `plotly` not yet implemented.")
  
  return(slideshow)
}