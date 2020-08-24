if(F){
  flea_std <- tourr::rescale(tourr::flea[, 1:6])
  rb <- tourr::basis_random(ncol(flea_std), 2)

  view_manip_space2(basis = rb, manip_var = 4)
}
view_manip_space2 <- function (basis,
                               manip_var,
                               manip_col = "blue",
                               tilt = pi * 5 / 12,
                               z_col = "red",
                               lab = paste0("V", 1:nrow(basis))) {
  find_angle <- function(a, b = c(1, 0))
    acos(sum(a * b) / (sqrt(sum(a * a)) * sqrt(sum(b * b))) )
  make_curve <- function(ang_st = 0,
                         ang_stop = 2 * pi) {
      angle <- seq(ang_st, ang_stop,
                   length = round(360 / (2 * pi) * abs(ang_st - ang_stop)) )
      data.frame(x = cos(angle), y = sin(angle), z = sin(angle))
    }

  ## Initialize
  p <- nrow(basis)
  m_sp   <- as.data.frame(create_manip_space(basis, manip_var))
  colnames(m_sp) <- c("x","y","z")
  m_sp_r <- m_sp %>% mutate(y = y * cos(tilt), z = z * sin(tilt))
  mv_sp   <- m_sp[manip_var, ]
  mv_sp_r <- m_sp_r[manip_var, ]

  ## Manip var asethetics
  col_v <- rep("grey80", p)
  col_v[manip_var] <- manip_col
  siz_v <- rep(0.3, p)
  siz_v[manip_var] <- 1
  ## Manip var asethetics
  circ_r <- make_curve() %>% mutate(y = y * cos(tilt), z = z * sin(tilt))
  thata_ang <- find_angle(c(mv_sp[1], mv_sp[2]), c(1, 0))
  theta_curve_r <- .5 * make_curve(rad_st = 0, ang_stop = thata_ang) %>%
    mutate(y = y * cos(tilt), z = z * sin(tilt))
  phi_m_sp <- find_angle(c(sqrt(mv_sp[1]^2 + mv_sp[2]^2), mv_sp[3]))
  phi_curve_r <- .4 * make_curve(ang_st = thata_ang, ang_stop = phi_m_sp) %>%
    mutate(y = y * cos(tilt), z = z * sin(tilt))

  ## Render
  gg <-
    ggplot2::ggplot() + ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme_void() + ggplot2::theme(legend.position = "none") +
    ggplot2::coord_fixed() +
    ggplot2::geom_path(data = circ_r,
                       mapping = ggplot2::aes(x = x, y = y),
                       color = manip_col,
                       size = 0.3,
                       inherit.aes = FALSE
    ) +
    ## projection plane:
    ggplot2::geom_segment(data = m_sp_r,
                          mapping = ggplot2::aes(
                            x = x,
                            y = y,
                            xend = 0,
                            yend = 0
                          ),
                          size = siz_v,
                          colour = col_v
    ) +
    ggplot2::geom_text(data = m_sp_r,
                       mapping = ggplot2::aes(x = x, y = y, label = labels),
                       size = 4,
                       colour = col_v,
                       vjust = "outward",
                       hjust = "outward"
    ) +
    ## Z direction
    ggplot2::geom_path(data = circ_r,
                       mapping = ggplot2::aes(x = x, y = z),
                       color = z_col,
                       size = 0.3,
                       inherit.aes = F
    ) +
    ggplot2::geom_segment(data = mv_sp_r,
                          mapping = ggplot2::aes(x = x,y = z,
                                                 xend = 0,yend = 0),
                          size = 1,
                          colour = z_col
    ) +
    ggplot2::geom_segment(data = mv_sp_r,
                          mapping = ggplot2::aes(x = x, y = z,
                                                 xend = x,yend = y),
                          size = 0.3,
                          colour = "grey80",
                          linetype = 2
    ) +
    ggplot2::geom_text(data = mv_sp_r,
                       mapping = ggplot2::aes(x = x, y = z,
                                              label = labels[manip_var]),
                       size = 4,
                       colour = z_col,
                       vjust = "outward",
                       hjust = "outward"
    ) +
    ## label angles
    geom_path(data = phi_curve_r,
              mapping = aes(x=x, y=z),
              color = z_col,
              size = 0.2
    ) +
    geom_text(data = 1.2 * phi_curve_r[ceiling(nrow(phi_curve_r)/2), ],
              mapping = ggplot2::aes(x=x, y=z, label = "phi"),
              color = z_col,
              parse = T,
              size = 4
    ) +
    geom_path(data = theta_curve_r,
              mapping = aes(x, y),
              color = manip_col,
              size = 0.2
    ) +
    geom_text(data = 1.2 * theta_curve_r[ceiling(nrow(theta_curve_r)/2), ],
              mapping = ggplot2::aes(x=x, y=y-.02, label = "theta"),
              color = manip_col,
              parse = T,
              size = 4
    )

  ## Return
  gg
}

