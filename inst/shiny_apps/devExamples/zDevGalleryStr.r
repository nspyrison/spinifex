nsLib()
library(ggplot2)

mat <- matrix(1:12, ncol=2, byrow = F)/12
colnames(mat) <- c("x", "y")
foo <- data.frame(`Manip type` = c("Horizontal", "Vertical", "Radial", "else"), 
                  `Manip var` = c("head", "tars1", "tars2", "tars2"), 
                  check.names = FALSE)
foo2 <- data.frame(Id=seq.int(nrow(foo)), foo, 
                   basis = I(replicate(nrow(foo), mat, simplify=FALSE)),
                   check.names = FALSE)
str(foo2)
### SETUP DONE
df <- foo2

dat <- tourr::rescale(flea[, 1:6])

n_bases <- nrow(df)
n <- nrow(dat)
p <- ncol(dat)
dat_colnames <- colnames(dat)
angle <- seq(0, 2 * pi, length = 360)
circ <- NULL
df$manip_var_num <- NULL
col_text <- NULL
var_lab <- NULL
type_lab <- NULL
df_gg <- NULL
for (i in 1:n_bases){
  # Circle
  this_circ <- data.frame(id = i, x = cos(angle), y = sin(angle))
  circ <- rbind(circ, this_circ)
  # Labels and color
  df$manip_var_num[i] <- which(dat_colnames == df$`Manip var`[i])
  col_chunk <- rep("grey40", p)
  col_chunk[df$`manip_var_num`[i]] <- "blue"
  col_text <- c(col_text, col_chunk)
  lab_chunk <- rep("", p)
  lab_chunk[df$`manip_var_num`[i]] <- as.character(df$`Manip var`[i])
  var_lab <- c(var_lab, lab_chunk)
  type_chunk <- rep("", p)
  type_chunk[df$`manip_var_num`[i]] <- as.character(df$`Manip type`[i])
  type_lab <- c(type_lab, type_chunk)
  # Unlist bases
  rows <- data.frame(id = rep(df$Id[i], p), 
                     df$basis[[i]])
  df_gg <- rbind(df_gg, rows)
}
dim(df_gg)

rows_needed <- (n * n_bases) - (p * n_bases)
col_text <- c(col_text, rep(NA, rows_needed))
var_lab <- c(type_lab, rep(NA, rows_needed))
type_lab <- c(type_lab, rep(NA, rows_needed))

df_gg <- rbind(df_gg, 
               data.frame(id = rep(NA, rows_needed), 
                          x = rep(NA, rows_needed), 
                          y = rep(NA, rows_needed)))

gg <-
  ggplot2::ggplot() + #data = df_gg
  ggplot2::scale_color_brewer(palette = "Dark2") +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::coord_fixed() +
  ## Cirle path
  ggplot2::geom_path(data = circ,
                     mapping = ggplot2::aes(x = x, y = y),
                     color = "grey80", size = .3, inherit.aes = F) +
  ## Basis axes line segments
  ggplot2::geom_segment(data = df_gg, col = col_text,
                        mapping = ggplot2::aes(x = x , y = y, 
                                               xend = 0, yend = 0)) +
  ## Basis variable text labels
  ggplot2::geom_text(data = df_gg, 
                     mapping = ggplot2::aes(x = 1.5 * x,
                                            y = 1.5 * y,
                                            label = var_lab),
                     size = 4, hjust = 0, vjust = 0, col = "blue") +
  ## manip_type label
  ggplot2::geom_text(data = df_gg,
                     mapping = ggplot2::aes(x = -1, y = -1, label = type_lab),
                     size = 4, hjust = 0, vjust = 0) +
  ## Facet
  ggplot2::facet_grid(rows = vars(id)) +
  ggplot2::theme(strip.background = ggplot2::element_blank(),
                 strip.text.y     = ggplot2::element_blank())
gg # goes to output$gallery_icons.


###
n <- nrow(dat)
  
df_gg_data <- NULL # Unlist basis into p rows
for (i in 1:nrow(df)){
  rows <- data.frame(id = rep(df$Id[i], n), 
                     dat %*% df$basis[[i]],
                     cat = flea[, 7])
  df_gg_data <- rbind(df_gg_data, rows)
}
dim(df_gg_data)


gg + geom_point(data = df_gg_data, mapping = aes(x, y, color = cat), size =.5) 

#gg + geom_density_2d(data = df_gg_data, bins = 4,
#                     mapping = ggplot2::aes(x = x, y = y, color = cat))



