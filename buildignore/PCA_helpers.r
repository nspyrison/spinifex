#### ggprto_pcaScree -----
# Example
dat <- tourr::flea[, 1:6]
ggplot() + ggprto_pcaScreeplot(data = dat)
ggplot() + ggprto_pcaScreeplot(data = dat, rescale = F) + theme_minimal()


if(F)
  data <- tourr::flea[, 1:6]; rescale <- T;
ggprto_pcaScreeplot <- function(data, rescale = T){
  if (is.matrix(data) == FALSE) data <- as.matrix(data)
  if (rescale == TRUE) data <- tourr::rescale(data)
  p <- ncol(data)
  pca_obj <- prcomp(data)
  df_pcaVar <- data.frame(
    pc_number = 1:p,
    rate_var_explained = pca_obj$sdev^2 / sum(pca_obj$sdev^2),
    cumsum_rate_var_explained = cumsum(pca_obj$sdev^2) / sum(pca_obj$sdev^2)
  )
  
  
  ## List of ggproto's that is addable to a ggplot.
  list(
    ## Individual variance bars
    geom_bar(data = df_pcaVar, stat = "identity", 
             mapping = aes(x = pc_number, y = rate_var_explained, 
                           fill = "Individual var explained")),
    ## Cummulative var line
    geom_line(data = df_pcaVar,
              mapping = aes(x = pc_number, y = cumsum_rate_var_explained,
                            color = "Cumsum of var explained")),
    geom_point(data = df_pcaVar, 
               mapping = aes(x = pc_number, y = cumsum_rate_var_explained,
                             color = "Cumsum of var explained"), shape = 4, size = 3),
    ## Themes, legends and colors 
    labs(x = "Component number", 
         y = "Variance explained", 
         colour = "", fill = ""),
    # theme(legend.position = c(0.9, 0.5)) + ## Doesn't work because bar and line?
    # scale_y_continuous(sec.axis = sec_axis(~.*.1, name = "Cumulative fraction of variance explained")) + ## if different 2nd y axis needed
    scale_fill_manual(values = "grey40"),
    scale_colour_manual(values = "blue")
  )
}

#### ggprto_pcaBiplot -----
# Example
dat <- tourr::flea[, 1:6]
ggplot() + ggprto_pcaBiplot(data = dat)


if(F)
  data <- tourr::flea[, 1:6]; x = 1; y = 2; rescale <- T; axes_position = "center"; 
ggprto_pcaBiplot <- function(data, 
                             x = 1, 
                             y = 2, 
                             rescale = T, 
                             axes_position = "center",
                             # color = NULL,
                             # shape = ,
                             size  = 3,
                             alpha = 1,
                             ...){
  if (is.matrix(data) == FALSE) data <- as.matrix(data)
  if (rescale == TRUE) data <- tourr::rescale(data)
  n           <- nrow(data)
  p           <- ncol(data)
  pca_obj     <- prcomp(data)
  pca_proj    <- 2 * (data.frame(tourr::rescale(pca_obj$x)) - .5)
  pca_pct_var <- pca_obj$sdev^2 / pca_obj(pca$sdev^2)
  
  xlab       <- paste0("PC", x, " (", round(100, pca_pct_var[x_num], 1), "% Var)")
  ylab       <- paste0("PC", y, " (", round(100, pca_pct_var[y_num], 1), "% Var)")
  angle <- seq(0, 2 * pi, length = 360)
  circ  <- set_axes_position(
    data.frame(x = cos(angle), y = sin(angle)),axes_position)
  zero  <- set_axes_position(0, axes_position)
  
  
  
  ### ggplot2
  gg <- ggplot() +
    # list()
    # data points
      geom_point(pca_x, 
                 mapping = aes(x = x, y = y), shape = shape, alpha = alpha,
                 size = size)
  } else { ## if USE_AES == TRUE then apply more aes.
    gg <- gg +
      geom_point(pca_x, 
                 mapping = aes(x = get(x_axis), y = get(y_axis),
                               color = cluster, 
                               fill  = cluster, 
                               shape = cluster), 
                 size = 3)
  }
  if (USE_AXES == TRUE) { ## then draw axes
    # axis segments
    gg <- gg +
      geom_segment(pca_rot,
                   mapping = aes(x = get(x_axis), xend = zero[, 1],
                                 y = get(y_axis), yend = zero[, 2]),
                   size = .3, colour = "red") +
      # axis label text
      geom_text(pca_rot,
                mapping = aes(x = get(x_axis),
                              y = get(y_axis),
                              label = colnames(dat_std)),
                size = 6, colour = "red", fontface = "bold",
                vjust = "outward", hjust = "outward") +
      # Cirle path
      geom_path(circ, mapping = aes(x = x, y = y),
                color = "grey80", size = .3, inherit.aes = F)
  }
  
  x_range <- max(pca_x[, 1], circ[, 1]) - min(pca_x[, 1], circ[, 1])
  y_range <- max(pca_x[, 2], circ[, 2]) - min(pca_x[, 2], circ[, 2])
  # Options 
  gg <- gg + theme_minimal() +
    scale_color_brewer(palette = pal) +
    theme(panel.grid.major = element_blank(), # no grid lines
          panel.grid.minor = element_blank(), # no grid lines
          axis.text.x  = element_blank(),     # no axis marks
          axis.text.y  = element_blank(),     # no axis marks
          axis.title.x = element_text(size = 22, face = "bold"),
          axis.title.y = element_text(size = 22, face = "bold"),
          aspect.ratio = y_range / x_range,
          legend.box.background = element_rect(),
          legend.title = element_text(size = 18, face = "bold"),
          legend.text  = element_text(size = 18, face = "bold")
    ) +
    labs(x = x_lab, y = y_lab)
  
}
